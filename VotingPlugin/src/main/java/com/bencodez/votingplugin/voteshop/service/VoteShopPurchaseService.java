package com.bencodez.votingplugin.voteshop.service;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.HashMap;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.time.TimeCalculation;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VoteShopPurchaseEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.SharedMysqlCacheReconciler;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

import lombok.Getter;
import lombok.Setter;

/**
 * Handles vote shop validation and purchases.
 */
@Getter
@Setter
public class VoteShopPurchaseService {
	private static final int PURCHASE_LOCK_STRIPES = 256;
	private static final Object[] PURCHASE_LOCKS = createPurchaseLocks();
	private static final int COMPLETION_PENDING = 0;
	private static final int COMPLETION_RUNNING = 1;
	private static final int COMPLETION_COMPENSATING = 2;
	private static final int COMPLETION_FINISHED = 3;

	private VoteShopDefinition definition;

	private VotingPluginMain plugin;

	/**
	 * Creates the purchase service.
	 *
	 * @param plugin     the plugin
	 * @param definition the definition
	 */
	public VoteShopPurchaseService(VotingPluginMain plugin, VoteShopDefinition definition) {
		this.plugin = plugin;
		this.definition = definition;
	}

	/**
	 * Validates a purchase.
	 *
	 * @param player the player
	 * @param user   the user
	 * @param item   the item
	 * @return the result
	 */
	public VoteShopPurchaseResult validatePurchase(Player player, VotingPluginUser user, VoteShopItem item) {
		VoteShopPurchaseResult staticValidation = validateStaticPurchase(player, item);
		if (staticValidation != VoteShopPurchaseResult.SUCCESS) {
			return staticValidation;
		}
		// Shared points and limits are decided atomically by the queued reservation.
		// GUI rendering/click validation runs on Bukkit/Folia lanes and must not turn
		// an advisory precheck into a synchronous database read.
		if (usesSharedMysqlPoints()) return VoteShopPurchaseResult.SUCCESS;
		if (item.getLimit() > 0 && user.getVoteShopIdentifierLimit(item.getIdentifier()) >= item.getLimit()) {
			return VoteShopPurchaseResult.LIMIT_REACHED;
		}
		if (user.getPoints() < item.getCost()) {
			return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
		}
		return VoteShopPurchaseResult.SUCCESS;
	}

	/** Refreshes dynamic GUI validation state only when that refresh cannot block on shared MySQL. */
	public void refreshUserForPurchaseValidation(VotingPluginUser user, boolean requested) {
		if (requested && !usesSharedMysqlPoints()) user.cache();
	}

	private VoteShopPurchaseResult validateStaticPurchase(Player player, VoteShopItem item) {
		if (!definition.isEnabled()) {
			return VoteShopPurchaseResult.SHOP_DISABLED;
		}
		if (item == null) {
			return VoteShopPurchaseResult.ITEM_NOT_FOUND;
		}
		if (item.isNotBuyable()) {
			return VoteShopPurchaseResult.NOT_BUYABLE;
		}
		if (!hasPermission(player, item.getPermission())) {
			return VoteShopPurchaseResult.NO_PERMISSION;
		}
		return VoteShopPurchaseResult.SUCCESS;
	}

	/**
	 * Executes a purchase.
	 *
	 * @param player the player
	 * @param user   the user
	 * @param item   the item
	 * @return the result
	 */
	private VoteShopPurchaseResult purchaseLocal(Player player, VotingPluginUser user, VoteShopItem item) {
		if (plugin.getConfigFile().isExtraVoteShopCheck()) user.cache();
		VoteShopPurchaseResult validation = validatePurchase(player, user, item);
		if (validation != VoteShopPurchaseResult.SUCCESS) {
			return validation;
		}

		FileConfiguration shopData = plugin.getShopFile().getData();
		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("identifier", item.getIdentifierName());
		placeholders.put("points", String.valueOf(item.getCost()));
		placeholders.put("limit", String.valueOf(item.getLimit()));
		placeholders.put("shop", definition.getTitle());

		VoteShopPurchaseResult debit = debitForPurchase(user, item);
		if (debit != VoteShopPurchaseResult.SUCCESS) {
			return debit;
		}
		completePurchase(player, user, item, placeholders, shopData);
		return VoteShopPurchaseResult.SUCCESS;
	}

	/**
	 * Executes a purchase and reports its result on the Bukkit thread. Shared
	 * MySQL debits run on AdvancedCore's ordered persistence executor so earlier
	 * asynchronous user writes complete before the conditional debit.
	 *
	 * @param player the player
	 * @param user the user
	 * @param item the item
	 * @param completion completion callback
	 */
	public void purchase(Player player, VotingPluginUser user, VoteShopItem item,
			Consumer<VoteShopPurchaseResult> completion) {
		if (!usesSharedMysqlPoints()) {
			completion.accept(purchaseLocal(player, user, item));
			return;
		}
		VoteShopPurchaseResult validation = validateStaticPurchase(player, item);
		if (validation != VoteShopPurchaseResult.SUCCESS) {
			completion.accept(validation);
			return;
		}
		// Keep the loaded configuration object with the queued purchase. reloadData()
		// replaces ShopFile's FileConfiguration, so looking it up after the worker
		// or entity task runs could pair an old debit with a newly loaded reward.
		FileConfiguration shopData = plugin.getShopFile().getData();
		HashMap<String, String> placeholders = purchasePlaceholders(item);
		try {
			plugin.getTimer().execute(() -> {
				SharedPurchaseDebit debit;
				synchronized (purchaseLock(user.getUUID())) {
					// Sample the reset window beside the conditional debit. A queued
					// persistence task may otherwise cross into a new limit period.
					debit = reserveSharedMysqlPurchase(user, item,
							limitGeneration(item, System.currentTimeMillis()));
				}
				if (debit.result() != VoteShopPurchaseResult.SUCCESS) {
					plugin.getBukkitScheduler().runTask(plugin, () -> completion.accept(debit.result()), player);
					return;
				}
				completeSharedMysqlPurchase(player, user, item, placeholders, shopData, completion, debit);
			});
		} catch (RuntimeException persistenceRejected) {
			plugin.debug(persistenceRejected);
			completeFailedPurchase(player, completion);
		}
	}

	/**
	 * Compatibility entry point for integrations compiled against the synchronous
	 * API. Shared-MySQL purchases return {@link VoteShopPurchaseResult#PENDING}
	 * after static validation because their final debit result is asynchronous;
	 * use the callback overload when the final result is required.
	 *
	 * @deprecated use {@link #purchase(Player, VotingPluginUser, VoteShopItem, Consumer)}
	 */
	@Deprecated
	public VoteShopPurchaseResult purchase(Player player, VotingPluginUser user, VoteShopItem item) {
		if (!usesSharedMysqlPoints()) return purchaseLocal(player, user, item);
		VoteShopPurchaseResult validation = validateStaticPurchase(player, item);
		if (validation != VoteShopPurchaseResult.SUCCESS) return validation;
		purchase(player, user, item, ignored -> { });
		return VoteShopPurchaseResult.PENDING;
	}

	private void completeSharedMysqlPurchase(Player player, VotingPluginUser user, VoteShopItem item,
			HashMap<String, String> placeholders, FileConfiguration shopData,
			Consumer<VoteShopPurchaseResult> completion, SharedPurchaseDebit debit) {
		AtomicInteger state = new AtomicInteger(COMPLETION_PENDING);
		Runnable compensateBeforeClaim = () -> {
			if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_COMPENSATING)) return;
			compensateSharedMysqlPurchase(player, user, completion, debit);
		};
		try {
			/*
			 * The first entity callback is only a nonblocking scheduling gate. Keeping
			 * the durable row PENDING until it starts lets recovery refund a debit when
			 * the entity scheduler never accepts work. The JDBC claim then runs off the
			 * entity lane, and only a successful durable claim schedules the actual
			 * reward callback.
			 */
			CompletableFuture<EntityTaskResult> gate = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(player, ignored -> {
				if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_RUNNING)) return;
				claimSharedMysqlPurchaseAsync(debit).whenComplete((claim, failure) -> {
					if (failure != null || requiresCompensation(claim)) {
						if (state.compareAndSet(COMPLETION_RUNNING, COMPLETION_COMPENSATING)) {
							compensateSharedMysqlPurchase(player, user, completion, debit);
						}
						return;
					}
					state.compareAndSet(COMPLETION_RUNNING, COMPLETION_FINISHED);
					scheduleClaimedReward(player, user, item, placeholders, shopData, completion, debit);
				});
			}, compensateBeforeClaim);
			gate.whenComplete((result, failure) -> {
				if (failure != null || result != EntityTaskResult.SUCCESS) {
					compensateBeforeClaim.run();
				}
			});
		} catch (RuntimeException schedulingFailure) {
			compensateBeforeClaim.run();
			plugin.debug(schedulingFailure);
		}
	}

	static boolean requiresCompensation(SharedMysqlPurchaseJournal.ClaimOutcome claim) {
		// The local reward callback has not started yet, so both a rejected claim
		// and an unconfirmed claim are safe to fence and refund.
		return claim != SharedMysqlPurchaseJournal.ClaimOutcome.CLAIMED;
	}

	void scheduleClaimedReward(Player player, VotingPluginUser user, VoteShopItem item,
			HashMap<String, String> placeholders, FileConfiguration shopData,
			Consumer<VoteShopPurchaseResult> completion, SharedPurchaseDebit debit) {
		AtomicInteger state = new AtomicInteger(COMPLETION_PENDING);
		Runnable rejectBeforeStart = () -> {
			if (state.compareAndSet(COMPLETION_PENDING, COMPLETION_COMPENSATING)) {
				compensateSharedMysqlPurchase(player, user, completion, debit);
			}
		};
		try {
			CompletableFuture<EntityTaskResult> reward = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(player, ignored -> {
				if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_RUNNING)) return;
				try {
					completePurchase(player, user, item, placeholders, shopData);
					plugin.getTimer().execute(() -> settleSharedMysqlPurchase(player, completion, debit));
				} catch (RuntimeException | Error rewardFailure) {
					logClaimedRewardSchedulingFailure(debit);
					throw rewardFailure;
				} finally {
					state.set(COMPLETION_FINISHED);
				}
			}, rejectBeforeStart);
			reward.whenComplete((result, failure) -> {
				if (failure != null || result != EntityTaskResult.SUCCESS) rejectBeforeStart.run();
			});
		} catch (RuntimeException schedulingFailure) {
			rejectBeforeStart.run();
			plugin.debug(schedulingFailure);
		}
	}

	private void logClaimedRewardSchedulingFailure(SharedPurchaseDebit debit) {
		plugin.getLogger().severe("Shared MySQL vote shop purchase " + debit.purchaseId()
				+ " was claimed but its reward callback did not complete; retaining it for reconciliation");
	}

	private void compensateSharedMysqlPurchase(Player player, VotingPluginUser user,
			Consumer<VoteShopPurchaseResult> completion, SharedPurchaseDebit debit) {
		try {
			// The local state CAS proves that neither reward callback can start. Persist
			// that fence before relying on either remaining scheduler; otherwise a task
			// accepted by the persistence executor could be lost with HOOK_STARTED
			// still charged and outside automatic recovery.
			if (!debit.journal().markCompensating(debit.purchaseId())) {
				// A terminal row may have been handled by recovery already. Do not
				// enqueue another scheduler task when this invocation did not obtain
				// the durable compensation fence.
				completeFailedPurchase(player, completion);
				return;
			}
		} catch (SQLException markerFailure) {
			plugin.getLogger().severe("Unable to mark an incomplete vote shop purchase for compensation: "
					+ markerFailure.getClass().getSimpleName());
			plugin.debug(markerFailure);
			completeFailedPurchase(player, completion);
			return;
		}
		Runnable compensation = () -> {
			refundCompensatingMysqlDebit(user, debit);
			plugin.getBukkitScheduler().runTask(plugin,
					() -> completion.accept(VoteShopPurchaseResult.FAILED), player);
		};
		try {
			plugin.getTimer().execute(compensation);
		} catch (RuntimeException schedulingFailure) {
			plugin.debug(schedulingFailure);
			// The row is already COMPENSATING even though the guarded reward callback
			// was rejected. Do not leave that recoverable debit pending just because
			// the persistence executor is concurrently shutting down. Bukkit's
			// independent async scheduler also keeps JDBC off the entity lane.
			try {
				plugin.getBukkitScheduler().runTaskAsynchronously(plugin, compensation);
			} catch (RuntimeException asyncSchedulingFailure) {
				// Recovery owns the already-durable COMPENSATING row after shutdown.
				plugin.debug(asyncSchedulingFailure);
				completeFailedPurchase(player, completion);
			}
		}
	}

	private void completeFailedPurchase(Player player, Consumer<VoteShopPurchaseResult> completion) {
		try {
			plugin.getBukkitScheduler().runTask(plugin,
					() -> completion.accept(VoteShopPurchaseResult.FAILED), player);
		} catch (RuntimeException completionFailure) {
			plugin.debug(completionFailure);
		}
	}

	private void refundCompensatingMysqlDebit(VotingPluginUser user, SharedPurchaseDebit debit) {
		try {
			if (debit.journal().refundCompensatingReward(debit.purchaseId())) {
				refreshPurchaseCache(user, debit.pointsColumn(), debit.limitColumn());
			}
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to refund an incomplete vote shop purchase: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private void settleSharedMysqlPurchase(Player player, Consumer<VoteShopPurchaseResult> completion,
			SharedPurchaseDebit debit) {
		completeSharedMysqlPurchase(debit);
		plugin.getBukkitScheduler().runTask(plugin, () -> completion.accept(VoteShopPurchaseResult.SUCCESS), player);
	}

	private HashMap<String, String> purchasePlaceholders(VoteShopItem item) {
		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("identifier", item.getIdentifierName());
		placeholders.put("points", String.valueOf(item.getCost()));
		placeholders.put("limit", String.valueOf(item.getLimit()));
		placeholders.put("shop", definition.getTitle());
		return placeholders;
	}

	private void completePurchase(Player player, VotingPluginUser user, VoteShopItem item,
			HashMap<String, String> placeholders, FileConfiguration shopData) {

		plugin.getLogger().info("VoteShop: " + user.getPlayerName() + "/" + user.getUUID() + " bought "
				+ item.getIdentifier() + " for " + item.getCost());

		plugin.getRewardHandler().giveReward(user, shopData, item.getRewardsPath(),
				new RewardOptions().setPlaceholders(placeholders));

		String purchaseMessage = item.getPurchaseMessage();
		if (purchaseMessage == null || purchaseMessage.isEmpty()) {
			purchaseMessage = plugin.getConfigFile().getFormatShopPurchaseMsg();
		}
		user.sendMessage(PlaceholderUtils.replacePlaceHolder(purchaseMessage, placeholders));

		VoteShopPurchaseEvent purchaseEvent = new VoteShopPurchaseEvent(player.getUniqueId(), player.getName(), user,
				item.getIdentifier(), item.getCost());
		Bukkit.getPluginManager().callEvent(purchaseEvent);
	}

	VoteShopPurchaseResult debitForPurchase(VotingPluginUser user, VoteShopItem item) {
		synchronized (purchaseLock(user.getUUID())) {
			if (usesSharedMysqlPoints()) {
				return debitSharedMysql(user, item);
			}
			if (item.getLimit() > 0 && user.getVoteShopIdentifierLimit(item.getIdentifier()) >= item.getLimit()) {
				return VoteShopPurchaseResult.LIMIT_REACHED;
			}
			if (!user.removePoints(item.getCost(), true)) {
				return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
			}
			if (item.getLimit() > 0) {
				user.setVoteShopIdentifierLimit(item.getIdentifier(),
						user.getVoteShopIdentifierLimit(item.getIdentifier()) + 1);
			}
			return VoteShopPurchaseResult.SUCCESS;
		}
	}

	private boolean usesSharedMysqlPoints() {
		return usesSharedMysqlPoints(plugin);
	}

	private static boolean usesSharedMysqlPoints(VotingPluginMain plugin) {
		return plugin != null && UserStorage.MYSQL.equals(plugin.getStorageType())
				&& !plugin.getBungeeSettings().isPerServerPoints();
	}

	/**
	 * Resets a shared-MySQL vote-shop limit with the durable epoch marker used by
	 * reservations. Other storage modes retain the established UserManager reset.
	 */
	public static void resetSharedMysqlLimit(VotingPluginMain plugin, String limitColumn) {
		resetSharedMysqlLimit(plugin, limitColumn, UUID.randomUUID().toString());
	}

	/** Applies a named reset at most once across all backends sharing the table. */
	public static void resetSharedMysqlLimit(VotingPluginMain plugin, String limitColumn, String resetGeneration) {
		if (!usesSharedMysqlPoints(plugin)) return;
		// Shared limit writes are deliberately nonqueued. Drop read snapshots
		// without dumping them, so a backend arriving after another server's reset
		// can never replay a pre-reset absolute value.
		SharedMysqlCacheReconciler.invalidateAll(plugin, limitColumn);
		try {
			MySQL table = plugin.getMysql();
			table.checkColumn(limitColumn, DataType.INTEGER);
			SharedMysqlPurchaseJournal.forTable(table).resetLimit(limitColumn, resetGeneration);
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically reset shared MySQL vote shop limit: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		} finally {
			SharedMysqlCacheReconciler.invalidateAll(plugin, limitColumn);
		}
	}

	/** Runs bounded stale-purchase recovery from the plugin lifecycle executor. */
	public static void recoverSharedMysqlPurchases(VotingPluginMain plugin) {
		if (!usesSharedMysqlPoints(plugin)) return;
		try {
			recoverSharedMysqlPurchases(plugin, SharedMysqlPurchaseJournal.forTable(plugin.getMysql()));
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to recover pending shared MySQL vote shop purchases: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private static void recoverSharedMysqlPurchases(VotingPluginMain plugin, SharedMysqlPurchaseJournal journal)
			throws SQLException {
		for (SharedMysqlPurchaseJournal.RefundedPurchase refund : journal.recoverAndCleanup(System.currentTimeMillis())) {
			SharedMysqlCacheReconciler.invalidate(plugin, refund.uuid(), refund.pointsColumn(), refund.limitColumn());
		}
	}

	VoteShopPurchaseResult debitSharedMysql(VotingPluginUser user, VoteShopItem item) {
		// This package-visible synchronous helper has no reward lifecycle to settle
		// later. Keep its conditional debit self-contained; asynchronous purchases
		// exclusively use reserveSharedMysqlPurchase() below so they can retain a
		// durable PENDING record until the reward hook is settled or refunded.
		MySQL table = plugin.getMysql();
		String pointsColumn = user.getPointsPath();
		String limitColumn = item.getLimit() > 0 ? "VoteShopLimit" + item.getIdentifier() : null;
		if (user.isCached()) {
			user.getCache().dump();
			plugin.getUserManager().getDataManager().removeCache(UUID.fromString(user.getUUID()), null);
		}
		if (limitColumn != null) table.checkColumn(limitColumn, DataType.INTEGER);
		StringBuilder sql = new StringBuilder("UPDATE ").append(table.qi(table.getTableName())).append(" SET ")
				.append(table.qi(pointsColumn)).append(" = ").append(table.qi(pointsColumn)).append(" - ?");
		if (limitColumn != null) {
			sql.append(", ").append(table.qi(limitColumn)).append(" = COALESCE(")
					.append(table.qi(limitColumn)).append(", 0) + 1");
		}
		sql.append(" WHERE ").append(table.qi("uuid"))
				.append(table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?")
				.append(" AND ").append(table.qi(pointsColumn)).append(" >= ?");
		if (limitColumn != null) sql.append(" AND COALESCE(").append(table.qi(limitColumn)).append(", 0) < ?");

		boolean debited = false;
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql.toString())) {
			statement.setInt(1, item.getCost());
			statement.setString(2, user.getUUID());
			statement.setInt(3, item.getCost());
			if (limitColumn != null) statement.setInt(4, item.getLimit());
			debited = statement.executeUpdate() == 1;
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically debit vote shop points: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
		}
		if (debited) {
			// The conditional debit connection has been closed before NO_CACHE reads.
			refreshPurchaseCache(user, pointsColumn, limitColumn);
			return VoteShopPurchaseResult.SUCCESS;
		}
		return sharedMysqlFailure(user, item, limitColumn);
	}

	private SharedPurchaseDebit reserveSharedMysqlPurchase(VotingPluginUser user, VoteShopItem item,
			LimitGeneration limitGeneration) {
		MySQL table = plugin.getMysql();
		String pointsColumn = user.getPointsPath();
		String limitColumn = item.getLimit() > 0 ? "VoteShopLimit" + item.getIdentifier() : null;
		if (user.isCached()) {
			// dump() waits for a cache batch that has already left its queue. Removing
			// the drained cache also prevents an older absolute write from racing the
			// conditional debit on the shared database.
			user.getCache().dump();
			plugin.getUserManager().getDataManager().removeCache(UUID.fromString(user.getUUID()), null);
		}
		if (limitColumn != null) {
			table.checkColumn(limitColumn, DataType.INTEGER);
		}
		try {
			SharedMysqlPurchaseJournal journal = SharedMysqlPurchaseJournal.forTable(table);
			recoverSharedMysqlPurchases(plugin, journal);
			String purchaseId = UUID.randomUUID().toString();
			if (journal.reserve(purchaseId, user.getUUID(), pointsColumn, limitColumn, item.getCost(), item.getLimit(),
					limitGeneration.value(), limitGeneration.expiresAt(), System.currentTimeMillis())) {
				// reserve() returns only after its transaction and connection are closed;
				// NO_CACHE reads must not contend with its one-connection pool handle.
				refreshPurchaseCache(user, pointsColumn, limitColumn);
				return new SharedPurchaseDebit(VoteShopPurchaseResult.SUCCESS, journal, purchaseId, pointsColumn,
						limitColumn);
			}
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically debit vote shop points: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return new SharedPurchaseDebit(VoteShopPurchaseResult.NOT_ENOUGH_POINTS, null, null, null, null);
		}
		return new SharedPurchaseDebit(sharedMysqlFailure(user, item, limitColumn), null, null, null, null);
	}

	private SharedMysqlPurchaseJournal.ClaimOutcome claimSharedMysqlPurchase(SharedPurchaseDebit debit) {
		try {
			return debit.journal().claimReward(debit.purchaseId(), System.currentTimeMillis());
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to claim a pending vote shop purchase: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return SharedMysqlPurchaseJournal.ClaimOutcome.INDETERMINATE;
		}
	}

	private CompletableFuture<SharedMysqlPurchaseJournal.ClaimOutcome> claimSharedMysqlPurchaseAsync(
			SharedPurchaseDebit debit) {
		CompletableFuture<SharedMysqlPurchaseJournal.ClaimOutcome> result = new CompletableFuture<>();
		try {
			plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
					() -> result.complete(claimSharedMysqlPurchase(debit)));
		} catch (RuntimeException schedulingFailure) {
			result.completeExceptionally(schedulingFailure);
		}
		return result;
	}

	private void completeSharedMysqlPurchase(SharedPurchaseDebit debit) {
		try {
			debit.journal().complete(debit.purchaseId());
		} catch (SQLException failure) {
			// A HOOK_STARTED record is intentionally retained for reconciliation:
			// the arbitrary reward hook may already have side effects.
			plugin.getLogger().severe("Unable to settle a completed vote shop purchase: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private VoteShopPurchaseResult sharedMysqlFailure(VotingPluginUser user, VoteShopItem item, String limitColumn) {
		if (limitColumn != null && user.getUserData().getInt(limitColumn, UserDataFetchMode.NO_CACHE) >= item.getLimit()) {
			return VoteShopPurchaseResult.LIMIT_REACHED;
		}
		return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
	}

	private void refreshPurchaseCache(VotingPluginUser user, String pointsColumn, String limitColumn) {
		// The shared-MySQL mutation already committed. Invalidate only the fields it
		// changed; adding absolute values to the cache would turn a concurrent
		// snapshot into a dirty write that can overwrite another backend's update.
		SharedMysqlCacheReconciler.invalidate(plugin, user.getUUID(), pointsColumn, limitColumn);
	}

	private LimitGeneration limitGeneration(VoteShopItem item, long nowMillis) {
		if (item.getLimit() <= 0) return LimitGeneration.NONE;
		return limitGeneration(plugin, item.getIdentifier(), nowMillis);
	}

	/** Stable identifier shared by every backend processing the same reset period. */
	public static String currentLimitGenerationId(VotingPluginMain plugin, String identifier) {
		return limitGeneration(plugin, identifier, System.currentTimeMillis()).value();
	}

	private static LimitGeneration limitGeneration(VotingPluginMain plugin, String identifier, long nowMillis) {
		boolean daily = plugin.getShopFile().getVoteShopResetDaily(identifier);
		boolean weekly = plugin.getShopFile().getVoteShopResetWeekly(identifier);
		boolean monthly = plugin.getShopFile().getVoteShopResetMonthly(identifier);
		return limitGeneration(plugin.getTimeChecker().getTime(), nowMillis, daily, weekly, monthly,
				plugin.getOptions().getTimeWeekOffSet(), configuredTimeZone(plugin),
				plugin.getOptions().getTimeHourOffSet());
	}

	private static ZoneId configuredTimeZone(VotingPluginMain plugin) {
		String configured = plugin.getOptions().getTimeZone();
		if (configured == null || configured.isEmpty()) return ZoneId.systemDefault();
		try {
			return ZoneId.of(configured);
		} catch (RuntimeException invalidZone) {
			return ZoneId.systemDefault();
		}
	}

	static LimitGeneration limitGeneration(LocalDateTime current, long nowMillis, boolean daily, boolean weekly,
			boolean monthly, int weekOffset) {
		return limitGeneration(current, nowMillis, daily, weekly, monthly, weekOffset, ZoneId.systemDefault(), 0);
	}

	private static LimitGeneration limitGeneration(LocalDateTime current, long nowMillis, boolean daily, boolean weekly,
			boolean monthly, int weekOffset, ZoneId timeZone, int hourOffset) {
		if (!daily && !weekly && !monthly) return LimitGeneration.NONE;
		LocalDateTime next = null;
		StringBuilder generation = new StringBuilder();
		if (daily) {
			next = current.toLocalDate().plusDays(1).atStartOfDay();
			generation.append("D:").append(current.toLocalDate());
		}
		if (weekly) {
			LocalDateTime weekBoundary = current.toLocalDate().plusDays(1).atStartOfDay();
			int week = TimeCalculation.weekNumber(current, weekOffset, Locale.getDefault());
			while (TimeCalculation.weekNumber(weekBoundary, weekOffset, Locale.getDefault()) == week) {
				weekBoundary = weekBoundary.plusDays(1);
			}
			if (next == null || weekBoundary.isBefore(next)) next = weekBoundary;
			if (generation.length() > 0) generation.append('|');
			generation.append(weeklyGenerationId(current, weekOffset));
		}
		if (monthly) {
			LocalDateTime monthBoundary = current.toLocalDate().withDayOfMonth(1).plusMonths(1).atStartOfDay();
			if (next == null || monthBoundary.isBefore(next)) next = monthBoundary;
			if (generation.length() > 0) generation.append('|');
			generation.append("M:").append(current.getYear()).append('-').append(current.getMonthValue());
		}
		long expiresAt = next.minusHours(hourOffset).atZone(timeZone).toInstant().toEpochMilli();
		if (expiresAt <= nowMillis) expiresAt = nowMillis + Math.max(1L, Duration.between(current, next).toMillis());
		return new LimitGeneration(generation.toString(), expiresAt);
	}

	static String weeklyGenerationId(LocalDateTime current, int weekOffset) {
		LocalDateTime weekTime = current.plusDays(weekOffset).toLocalDate()
				.with(java.time.temporal.TemporalAdjusters.nextOrSame(java.time.DayOfWeek.MONDAY)).atStartOfDay();
		WeekFields fields = WeekFields.ISO;
		return "W:" + weekTime.get(fields.weekBasedYear()) + '-' + weekTime.get(fields.weekOfWeekBasedYear());
	}

	record SharedPurchaseDebit(VoteShopPurchaseResult result, SharedMysqlPurchaseJournal journal,
			String purchaseId, String pointsColumn, String limitColumn) {
	}

	record LimitGeneration(String value, long expiresAt) {
		private static final LimitGeneration NONE = new LimitGeneration(
				SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION, 0L);
	}

	Object purchaseLock(String uuid) {
		return PURCHASE_LOCKS[(uuid == null ? 0 : uuid.hashCode()) & (PURCHASE_LOCK_STRIPES - 1)];
	}

	private static Object[] createPurchaseLocks() {
		Object[] locks = new Object[PURCHASE_LOCK_STRIPES];
		for (int i = 0; i < locks.length; i++) {
			locks[i] = new Object();
		}
		return locks;
	}

	/**
	 * Checks permission support including inverse permissions with !.
	 *
	 * @param player     the player
	 * @param permission the permission
	 * @return true if allowed
	 */
	public boolean hasPermission(Player player, String permission) {
		if (permission == null || permission.isEmpty()) {
			return true;
		}
		if (permission.startsWith("!")) {
			String parsed = PlaceholderUtils.replacePlaceHolders(player, permission.substring(1));
			return !player.hasPermission(parsed);
		}
		String parsed = PlaceholderUtils.replacePlaceHolders(player, permission);
		return player.hasPermission(parsed);
	}

	/**
	 * Sends a failure message for a purchase result.
	 *
	 * @param player the player
	 * @param user   the user
	 * @param item   the item
	 * @param result the result
	 */
	public void sendFailureMessage(Player player, VotingPluginUser user, VoteShopItem item,
			VoteShopPurchaseResult result) {
		if (result == VoteShopPurchaseResult.SHOP_DISABLED) {
			player.sendMessage(com.bencodez.simpleapi.messages.MessageAPI.colorize("&cVote shop disabled"));
			return;
		}
		if (result == VoteShopPurchaseResult.FAILED) {
			player.sendMessage(com.bencodez.simpleapi.messages.MessageAPI.colorize(
					"&cUnable to complete this purchase; please try again."));
			return;
		}
		if (result == VoteShopPurchaseResult.LIMIT_REACHED) {
			user.sendMessage(definition.getLimitReachedMessage());
			return;
		}

		if (result == VoteShopPurchaseResult.NOT_ENOUGH_POINTS) {
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("identifier", item == null ? "" : item.getIdentifierName());
			placeholders.put("points", item == null ? "0" : String.valueOf(item.getCost()));
			placeholders.put("limit", item == null ? "-1" : String.valueOf(item.getLimit()));
			placeholders.put("shop", definition.getTitle());
			user.sendMessage(
					PlaceholderUtils.replacePlaceHolder(plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
			return;
		}

		if (result == VoteShopPurchaseResult.NOT_BUYABLE) {
			user.sendMessage(plugin.getConfigFile().getFormatShopNotPurchasable());
		}
	}
}
