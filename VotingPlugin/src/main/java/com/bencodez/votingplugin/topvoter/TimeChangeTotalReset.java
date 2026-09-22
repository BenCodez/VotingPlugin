package com.bencodez.votingplugin.topvoter;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.sql.UserTable;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

/** Applies a period-total reset once even when its outer checkpoint must retry. */
final class TimeChangeTotalReset {
	private static final String SQLITE_JOURNAL = "VotingPlugin_TimeTotalResets";

	private TimeChangeTotalReset() { }

	static boolean reset(VotingPluginMain plugin, String column, String generation) {
		if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
			return VoteShopPurchaseService.resetMysqlLimitWithPurchaseFence(plugin, column, generation);
		}
		if (!UserStorage.SQLITE.equals(plugin.getStorageType())) return false;
		UserTable table = plugin.getSQLiteUserTable();
		try {
			return resetSqlite(table.getSqLite().getSQLConnection(), table.getName(), column, generation);
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically reset period totals: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return false;
		}
	}

	static boolean resetSqlite(Connection connection, String table, String column, String generation)
			throws SQLException {
		if (!safeIdentifier(table) || !safeIdentifier(column) || generation == null || generation.isEmpty()) {
			throw new SQLException("Invalid period-total reset input");
		}
		synchronized (connection) {
			boolean previousAutoCommit = connection.getAutoCommit();
			try {
				if (!previousAutoCommit) throw new SQLException("SQLite user connection already has an active transaction");
				connection.setAutoCommit(false);
				try (Statement schema = connection.createStatement()) {
					schema.executeUpdate("CREATE TABLE IF NOT EXISTS " + quote(SQLITE_JOURNAL)
							+ " (generation TEXT PRIMARY KEY)");
				}
				int inserted;
				try (PreparedStatement marker = connection.prepareStatement(
						"INSERT OR IGNORE INTO " + quote(SQLITE_JOURNAL) + " (generation) VALUES (?)")) {
					marker.setString(1, generation);
					inserted = marker.executeUpdate();
				}
				if (inserted == 1) {
					try (Statement wipe = connection.createStatement()) {
						wipe.executeUpdate("UPDATE " + quote(table) + " SET " + quote(column) + " = 0");
					}
				}
				connection.commit();
				return true;
			} catch (SQLException failure) {
				try {
					connection.rollback();
				} catch (SQLException rollbackFailure) {
					failure.addSuppressed(rollbackFailure);
				}
				throw failure;
			} finally {
				connection.setAutoCommit(previousAutoCommit);
			}
		}
	}

	private static boolean safeIdentifier(String value) {
		return value != null && value.matches("[A-Za-z0-9_]+");
	}

	private static String quote(String identifier) {
		return '`' + identifier + '`';
	}
}
