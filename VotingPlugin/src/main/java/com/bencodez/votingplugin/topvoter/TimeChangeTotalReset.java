package com.bencodez.votingplugin.topvoter;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.sql.UserTable;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.PeriodTotalMutationFence;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

/** Applies a period-total reset once even when its outer checkpoint must retry. */
public final class TimeChangeTotalReset {
	private static final String SQLITE_JOURNAL = "VotingPlugin_TimeTotalResets";

	private TimeChangeTotalReset() { }

	public static boolean reset(VotingPluginMain plugin, String column, String previousColumn, String generation) {
		boolean[] reset = { false };
		PeriodTotalMutationFence.withReset(() -> {
			// Drain queued absolute totals while new total mutations are excluded. The
			// reset then subtracts the boundary copy, preserving votes accepted while
			// earlier recovery phases were running.
			plugin.getUserManager().getDataManager().clearCache();
			if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
				reset[0] = VoteShopPurchaseService.resetMysqlPeriodTotal(plugin, column, previousColumn, generation);
				return;
			}
			if (!UserStorage.SQLITE.equals(plugin.getStorageType())) return;
			UserTable table = plugin.getSQLiteUserTable();
			try {
				String url = table.getSqLite().getSQLConnection().getMetaData().getURL();
				try (Connection connection = DriverManager.getConnection(url)) {
					reset[0] = resetSqlite(connection, table.getName(), column, previousColumn, generation);
				}
			} catch (SQLException failure) {
				plugin.getLogger().severe("Unable to atomically reset period totals: "
						+ failure.getClass().getSimpleName());
				plugin.debug(failure);
			}
		});
		return reset[0];
	}

	public static boolean copyBoundary(VotingPluginMain plugin, String column, String previousColumn, String generation) {
		boolean[] copied = { false };
		PeriodTotalMutationFence.withReset(() -> {
			plugin.getUserManager().getDataManager().clearCache();
			if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
				copied[0] = VoteShopPurchaseService.copyMysqlPeriodBoundary(plugin, column, previousColumn, generation);
				return;
			}
			if (!UserStorage.SQLITE.equals(plugin.getStorageType())) return;
			UserTable table = plugin.getSQLiteUserTable();
			try {
				String url = table.getSqLite().getSQLConnection().getMetaData().getURL();
				try (Connection connection = DriverManager.getConnection(url)) {
					copied[0] = copyBoundarySqlite(connection, table.getName(), column, previousColumn, generation);
				}
			} catch (SQLException failure) {
				plugin.getLogger().severe("Unable to atomically copy period totals: "
						+ failure.getClass().getSimpleName());
				plugin.debug(failure);
			}
		});
		return copied[0];
	}

	/** Resets an auxiliary integer column once for a recoverable listener effect. */
	public static boolean resetToZero(VotingPluginMain plugin, String column, String generation) {
		plugin.getUserManager().getDataManager().clearCache();
		if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
			return VoteShopPurchaseService.resetMysqlLimitWithPurchaseFence(plugin, column, generation);
		}
		if (!UserStorage.SQLITE.equals(plugin.getStorageType())) return false;
		UserTable table = plugin.getSQLiteUserTable();
		try {
			String url = table.getSqLite().getSQLConnection().getMetaData().getURL();
			try (Connection connection = DriverManager.getConnection(url)) {
				return resetSqliteToZero(connection, table.getName(), column, generation);
			}
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to atomically reset user counters: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			return false;
		}
	}

	static boolean resetSqliteToZero(Connection connection, String table, String column, String generation)
			throws SQLException {
		return resetSqliteExpression(connection, table, column, generation, "0");
	}

	static boolean copyBoundarySqlite(Connection connection, String table, String column, String previousColumn,
			String generation) throws SQLException {
		if (!safeIdentifier(column) || !safeIdentifier(previousColumn)) {
			throw new SQLException("Invalid period boundary column");
		}
		return resetSqliteExpression(connection, table, previousColumn, generation,
				"COALESCE(" + quote(column) + ", 0)");
	}

	static boolean resetSqlite(Connection connection, String table, String column, String previousColumn,
			String generation)
			throws SQLException {
		if (!safeIdentifier(table) || !safeIdentifier(column) || !safeIdentifier(previousColumn)
				|| generation == null || generation.isEmpty()) {
			throw new SQLException("Invalid period-total reset input");
		}
		return resetSqliteExpression(connection, table, column, generation, "MAX(0, COALESCE(" + quote(column)
				+ ", 0) - COALESCE(" + quote(previousColumn) + ", 0))");
	}

	private static boolean resetSqliteExpression(Connection connection, String table, String column, String generation,
			String expression) throws SQLException {
		if (!safeIdentifier(table) || !safeIdentifier(column) || generation == null || generation.isEmpty()) {
			throw new SQLException("Invalid reset input");
		}
		synchronized (connection) {
			boolean previousAutoCommit = connection.getAutoCommit();
			SQLException primaryFailure = null;
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
						wipe.executeUpdate("UPDATE " + quote(table) + " SET " + quote(column) + " = " + expression);
					}
				}
				connection.commit();
				return true;
			} catch (SQLException failure) {
				primaryFailure = failure;
				try {
					connection.rollback();
				} catch (SQLException rollbackFailure) {
					failure.addSuppressed(rollbackFailure);
				}
				throw failure;
			} finally {
				try {
					connection.setAutoCommit(previousAutoCommit);
				} catch (SQLException restoreFailure) {
					if (primaryFailure != null) primaryFailure.addSuppressed(restoreFailure);
					else throw restoreFailure;
				}
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
