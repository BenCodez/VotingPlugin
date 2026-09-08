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
			VoteShopPurchaseResult debit;
			synchronized (purchaseLock(user.getUUID())) {
				debit = debitSharedMysql(user, item);
			}
			if (debit != VoteShopPurchaseResult.SUCCESS) {
				plugin.getBukkitScheduler().runTask(plugin, () -> completion.accept(debit), player);
				return;
			}
			completeSharedMysqlPurchase(player, user, item, placeholders, shopData, completion);
		});
	}

	private void completeSharedMysqlPurchase(Player player, VotingPluginUser user, VoteShopItem item,
			HashMap<String, String> placeholders, FileConfiguration shopData,
			Consumer<VoteShopPurchaseResult> completion) {
		CountDownLatch completed = new CountDownLatch(1);
		AtomicInteger state = new AtomicInteger(COMPLETION_PENDING);
		try {
			CompletableFuture<EntityTaskResult> scheduled = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(player, ignored -> {
				if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_RUNNING)) return;
				try {
					completePurchase(player, user, item, placeholders, shopData);
					completion.accept(VoteShopPurchaseResult.SUCCESS);
				} finally {
					state.set(COMPLETION_FINISHED);
					completed.countDown();
				}
			}, () -> requestCompensation(state, completed));
			scheduled.whenComplete((result, failure) -> {
				if (failure != null || result != EntityTaskResult.SUCCESS) requestCompensation(state, completed);
			});
			while (!completed.await(100, TimeUnit.MILLISECONDS)) {
				if (!plugin.isEnabled()) requestCompensation(state, completed);
			}
			if (state.get() == COMPLETION_COMPENSATING) refundSharedMysqlDebit(user, item);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			if (requestCompensation(state, completed)) refundSharedMysqlDebit(user, item);
		} catch (RuntimeException schedulingFailure) {
			if (requestCompensation(state, completed)) refundSharedMysqlDebit(user, item);
			plugin.debug(schedulingFailure);
		}
	}

	private static boolean requestCompensation(AtomicInteger state, CountDownLatch completed) {
		if (!state.compareAndSet(COMPLETION_PENDING, COMPLETION_COMPENSATING)) return false;
		completed.countDown();
		return true;
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
		return plugin != null && UserStorage.MYSQL.equals(plugin.getStorageType())
				&& !plugin.getBungeeSettings().isPerServerPoints();
	}

	VoteShopPurchaseResult debitSharedMysql(VotingPluginUser user, VoteShopItem item) {
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

		StringBuilder sql = new StringBuilder("UPDATE ").append(table.qi(table.getTableName())).append(" SET ")
				.append(table.qi(pointsColumn)).append(" = ").append(table.qi(pointsColumn)).append(" - ?");
		if (limitColumn != null) {
			sql.append(", ").append(table.qi(limitColumn)).append(" = COALESCE(")
					.append(table.qi(limitColumn)).append(", 0) + 1");
		}
		sql.append(" WHERE ").append(table.qi("uuid"))
				.append(table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?")
				.append(" AND ").append(table.qi(pointsColumn)).append(" >= ?");
		if (limitColumn != null) {
			sql.append(" AND COALESCE(").append(table.qi(limitColumn)).append(", 0) < ?");
		}

		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql.toString())) {
			statement.setInt(1, item.getCost());
			statement.setString(2, user.getUUID());
			statement.setInt(3, item.getCost());
			if (limitColumn != null) statement.setInt(4, item.getLimit());
			if (statement.executeUpdate() == 1) {
				refreshPurchaseCache(user, pointsColumn, limitColumn);
				return VoteShopPurchaseResult.SUCCESS;
			}
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically debit vote shop points: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return VoteShopPurchaseResult.NOT_ENOUGH_POINTS;
		}
		// The classification performs a fresh NO_CACHE database read. It must only
		// acquire that connection after the conditional-debit handle has returned to
		// the pool, which may be configured with a single connection.
		return sharedMysqlFailure(user, item, limitColumn);
	}

	private void refundSharedMysqlDebit(VotingPluginUser user, VoteShopItem item) {
		MySQL table = plugin.getMysql();
		String pointsColumn = user.getPointsPath();
		String limitColumn = item.getLimit() > 0 ? "VoteShopLimit" + item.getIdentifier() : null;
		StringBuilder sql = new StringBuilder("UPDATE ").append(table.qi(table.getTableName())).append(" SET ")
				.append(table.qi(pointsColumn)).append(" = ").append(table.qi(pointsColumn)).append(" + ?");
		if (limitColumn != null) {
			sql.append(", ").append(table.qi(limitColumn)).append(" = GREATEST(COALESCE(")
					.append(table.qi(limitColumn)).append(", 0) - 1, 0)");
		}
		sql.append(" WHERE ").append(table.qi("uuid"))
				.append(table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql.toString())) {
			statement.setInt(1, item.getCost());
			statement.setString(2, user.getUUID());
			statement.executeUpdate();
			refreshPurchaseCache(user, pointsColumn, limitColumn);
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
