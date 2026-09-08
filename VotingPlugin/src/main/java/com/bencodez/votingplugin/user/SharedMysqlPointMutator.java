package com.bencodez.votingplugin.user;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.UUID;
import java.util.function.IntFunction;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.votingplugin.VotingPluginMain;

/** Performs point writes that must remain atomic across shared MySQL servers. */
final class SharedMysqlPointMutator {
	private final VotingPluginMain plugin;

	SharedMysqlPointMutator(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	boolean applies() {
		return plugin != null && UserStorage.MYSQL.equals(plugin.getStorageType())
				&& !plugin.getBungeeSettings().isPerServerPoints();
	}

	void add(VotingPluginUser user, int amount, boolean async) {
		run(() -> update(user, amount, false), async);
	}

	void set(VotingPluginUser user, int value, boolean async) {
		run(() -> setAbsolute(user, value), async);
	}

	void cap(VotingPluginUser user, int maximum, boolean async) {
		run(() -> capAt(user, maximum), async);
	}

	boolean remove(VotingPluginUser user, int amount) {
		return update(user, -amount, true);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int amount) {
		return transfer(source, target, amount, amount);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount, int creditAmount) {
		return transfer(source, target, debitAmount, ignored -> creditAmount);
	}

	/**
	 * Transfers points while allowing the recipient hook to approve or adjust the
	 * credit after the conditional debit has succeeded. The approval callback is
	 * invoked on the persistence worker after the conditional debit. The receive
	 * event is explicitly asynchronous, so no server-thread rendezvous is needed
	 * while the transaction is open and cancellation can still roll back atomically.
	 */
	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			IntFunction<Integer> creditAmountProvider) {
		drainCache(source);
		drainCache(target);
		MySQL table = plugin.getMysql();
		String sourcePoints = source.getPointsPath();
		String targetPoints = target.getPointsPath();
		String uuidMatch = table.qi("uuid") + (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		String debit = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(sourcePoints) + " = "
				+ table.qi(sourcePoints) + " - ? WHERE " + uuidMatch + " AND " + table.qi(sourcePoints) + " >= ?";
		String credit = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(targetPoints) + " = "
				+ table.qi(targetPoints) + " + ? WHERE " + uuidMatch;
		try (Connection connection = table.getMysql().getConnectionManager().getConnection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement debitStatement = connection.prepareStatement(debit);
					PreparedStatement creditStatement = connection.prepareStatement(credit)) {
				debitStatement.setInt(1, debitAmount);
				debitStatement.setString(2, source.getUUID());
				debitStatement.setInt(3, debitAmount);
				if (debitStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
				Integer creditAmount;
				try {
					creditAmount = creditAmountProvider.apply(debitAmount);
				} catch (RuntimeException failure) {
					connection.rollback();
					logApprovalFailure(failure);
					return false;
				}
				if (creditAmount == null) {
					connection.rollback();
					return false;
				}
				creditStatement.setInt(1, creditAmount);
				creditStatement.setString(2, target.getUUID());
				if (creditStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
				connection.commit();
				return true;
			} catch (SQLException failure) {
				connection.rollback();
				throw failure;
			}
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		}
	}

	private void run(Runnable operation, boolean async) {
		if (async) {
			plugin.getTimer().execute(operation);
		} else {
			operation.run();
		}
	}

	private boolean update(VotingPluginUser user, int delta, boolean requireNonnegative) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		StringBuilder sql = new StringBuilder("UPDATE ").append(table.qi(table.getTableName())).append(" SET ")
				.append(table.qi(points)).append(" = ").append(table.qi(points)).append(" + ? WHERE ")
				.append(table.qi("uuid")).append(table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		if (requireNonnegative) {
			sql.append(" AND ").append(table.qi(points)).append(" >= ?");
		}
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql.toString())) {
			statement.setInt(1, delta);
			statement.setString(2, user.getUUID());
			if (requireNonnegative) statement.setInt(3, -delta);
			return statement.executeUpdate() == 1;
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		}
	}

	private void setAbsolute(VotingPluginUser user, int value) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String sql = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(user.getPointsPath())
				+ " = ? WHERE " + table.qi("uuid")
				+ (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setInt(1, value);
			statement.setString(2, user.getUUID());
			statement.executeUpdate();
		} catch (SQLException failure) {
			logFailure(failure);
		}
	}

	private void capAt(VotingPluginUser user, int maximum) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		String sql = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(points) + " = LEAST("
				+ table.qi(points) + ", ?) WHERE " + table.qi("uuid")
				+ (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setInt(1, maximum);
			statement.setString(2, user.getUUID());
			statement.executeUpdate();
		} catch (SQLException failure) {
			logFailure(failure);
		}
	}

	private void drainCache(VotingPluginUser user) {
		if (user.isCached()) {
			user.getCache().dump();
			plugin.getUserManager().getDataManager().removeCache(UUID.fromString(user.getUUID()), null);
		}
	}

	private void logFailure(SQLException failure) {
		plugin.getLogger().severe("Unable to update shared MySQL vote points: " + failure.getClass().getSimpleName());
		plugin.debug(failure);
	}

	private void logApprovalFailure(RuntimeException failure) {
		plugin.getLogger().severe("Unable to approve shared MySQL point transfer on the persistence worker: "
				+ failure.getClass().getSimpleName());
		plugin.debug(failure);
	}
}
