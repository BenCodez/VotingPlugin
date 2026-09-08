package com.bencodez.votingplugin.voteshop.service;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.change.UserDataChangeInt;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VoteShopPurchaseEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
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
		if (item.getLimit() > 0 && user.getVoteShopIdentifierLimit(item.getIdentifier()) >= item.getLimit()) {
			return VoteShopPurchaseResult.LIMIT_REACHED;
		}
		if (user.getPoints() < item.getCost()) {
			return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
		}
		return VoteShopPurchaseResult.SUCCESS;
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
		plugin.getTimer().execute(() -> {
			SharedPurchaseDebit debit;
			synchronized (purchaseLock(user.getUUID())) {
				debit = reserveSharedMysqlPurchase(user, item);
			}
			if (debit.result() != VoteShopPurchaseResult.SUCCESS) {
				plugin.getBukkitScheduler().runTask(plugin, () -> completion.accept(debit.result()), player);
				return;
			}
			completeSharedMysqlPurchase(player, user, item, placeholders, shopData, completion, debit);
		});
	}

	private void completeSharedMysqlPurchase(Player player, VotingPluginUser user, VoteShopItem item,
			HashMap<String, String> placeholders, FileConfiguration shopData,
			Consumer<VoteShopPurchaseResult> completion, SharedPurchaseDebit debit) {
		CountDownLatch completed = new CountDownLatch(1);
		AtomicInteger state = new AtomicInteger(COMPLETION_PENDING);
		try {
			/* Claim on the persistence worker before entering the entity scheduler.
			 * A JDBC pool wait or database lock must never block a Bukkit/Folia entity
			 * lane; the scheduled callback below performs reward/UI work only. */
			SharedMysqlPurchaseJournal.ClaimOutcome claim = claimSharedMysqlPurchase(debit);
			if (claim == SharedMysqlPurchaseJournal.ClaimOutcome.NOT_CLAIMED) {
				refundSharedMysqlDebit(user, debit, false);
				return;
			}
			if (claim == SharedMysqlPurchaseJournal.ClaimOutcome.INDETERMINATE) {
				plugin.getLogger().severe("Shared MySQL vote shop purchase " + debit.purchaseId()
						+ " has an indeterminate reward claim; retaining it for reconciliation");
				return;
			}
			CompletableFuture<EntityTaskResult> scheduled = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(player, ignored -> {
				if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_RUNNING)) return;
				try {
					completePurchase(player, user, item, placeholders, shopData);
					/*
					 * The entity callback owns only reward/UI work. Queue the terminal
					 * journal update back to the persistence executor after the reward
					 * completes, so a JDBC pool wait cannot stall an entity lane.
					 */
					plugin.getTimer().execute(() -> settleSharedMysqlPurchase(player, completion, debit));
				} finally {
					state.compareAndSet(COMPLETION_RUNNING, COMPLETION_FINISHED);
					completed.countDown();
				}
			}, () -> requestCompensation(state, completed));
			scheduled.whenComplete((result, failure) -> {
				if (failure != null || result != EntityTaskResult.SUCCESS) requestCompensation(state, completed);
			});
			while (!completed.await(100, TimeUnit.MILLISECONDS)) {
				if (!plugin.isEnabled()) requestCompensation(state, completed);
			}
			if (state.get() == COMPLETION_COMPENSATING) refundSharedMysqlDebit(user, debit, true);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			if (compensationRequiredAfterInterruption(state, completed)) refundSharedMysqlDebit(user, debit, true);
		} catch (RuntimeException schedulingFailure) {
			requestCompensation(state, completed);
			if (state.get() == COMPLETION_COMPENSATING) refundSharedMysqlDebit(user, debit, true);
			plugin.debug(schedulingFailure);
		}
	}

	private void settleSharedMysqlPurchase(Player player, Consumer<VoteShopPurchaseResult> completion,
			SharedPurchaseDebit debit) {
		completeSharedMysqlPurchase(debit);
		plugin.getBukkitScheduler().runTask(plugin, () -> completion.accept(VoteShopPurchaseResult.SUCCESS), player);
	}

	private static boolean requestCompensation(AtomicInteger state, CountDownLatch completed) {
		if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_COMPENSATING)) return false;
		completed.countDown();
		return true;
	}

	/**
	 * An entity scheduler fallback can request compensation just before the
	 * persistence worker is interrupted.  The latter still owns the debit and
	 * must perform the refund even though it did not win the state transition.
	 */
	static boolean compensationRequiredAfterInterruption(AtomicInteger state, CountDownLatch completed) {
		requestCompensation(state, completed);
		return state.get() == COMPLETION_COMPENSATING;
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

	/** Runs bounded stale-purchase recovery from the plugin lifecycle executor. */
	public static void recoverSharedMysqlPurchases(VotingPluginMain plugin) {
		if (!usesSharedMysqlPoints(plugin)) return;
		try {
			SharedMysqlPurchaseJournal.forTable(plugin.getMysql()).recoverAndCleanup(System.currentTimeMillis());
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to recover pending shared MySQL vote shop purchases: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
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

	private SharedPurchaseDebit reserveSharedMysqlPurchase(VotingPluginUser user, VoteShopItem item) {
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
			journal.recoverAndCleanup(System.currentTimeMillis());
			String purchaseId = UUID.randomUUID().toString();
			if (journal.reserve(purchaseId, user.getUUID(), pointsColumn, limitColumn, item.getCost(), item.getLimit(),
					System.currentTimeMillis())) {
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

	private void refundSharedMysqlDebit(VotingPluginUser user, SharedPurchaseDebit debit,
			boolean schedulerProvesRewardCannotRun) {
		try {
			boolean refunded = schedulerProvesRewardCannotRun
					? debit.journal().refundClaimedBeforeReward(debit.purchaseId())
					: debit.journal().refundPending(debit.purchaseId());
			if (refunded) {
				// refundPending() closes its transaction handle before any NO_CACHE
				// cache refresh, including when the cache reappears concurrently.
				refreshPurchaseCache(user, debit.pointsColumn(), debit.limitColumn());
			}
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to refund an incomplete vote shop purchase: "
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
		if (!user.isCached()) return;
		user.getCache().addChange(new UserDataChangeInt(pointsColumn,
				user.getUserData().getInt(pointsColumn, UserDataFetchMode.NO_CACHE)), false);
		if (limitColumn != null) {
			user.getCache().addChange(new UserDataChangeInt(limitColumn,
					user.getUserData().getInt(limitColumn, UserDataFetchMode.NO_CACHE)), false);
		}
	}

	private record SharedPurchaseDebit(VoteShopPurchaseResult result, SharedMysqlPurchaseJournal journal,
			String purchaseId, String pointsColumn, String limitColumn) {
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
