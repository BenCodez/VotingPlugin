package com.bencodez.votingplugin.user;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.IntFunction;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.votingplugin.VotingPluginMain;

/** Performs point writes that must remain atomic across shared MySQL servers. */
final class SharedMysqlPointMutator {
	private final VotingPluginMain plugin;

	SharedMysqlPointMutator(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	boolean applies() {
		return usesSharedMysqlPoints(plugin);
	}

	static boolean usesSharedMysqlPoints(VotingPluginMain plugin) {
		return plugin != null && UserStorage.MYSQL.equals(plugin.getStorageType())
				&& !plugin.getBungeeSettings().isPerServerPoints();
	}

	/**
	 * Recovers a bounded batch immediately and periodically. The executor belongs
	 * to the plugin lifecycle, so no independent task survives shutdown.
	 */
	static void scheduleTransferRecovery(VotingPluginMain plugin) {
		plugin.getTimer().execute(() -> recoverTransfers(plugin));
		plugin.getTimer().scheduleWithFixedDelay(() -> recoverTransfers(plugin), 1L, 1L, TimeUnit.MINUTES);
	}

	private static void recoverTransfers(VotingPluginMain plugin) {
		if (!usesSharedMysqlPoints(plugin)) return;
		try {
			SharedPointTransferJournal.forTable(plugin.getMysql()).recoverAndCleanup(System.currentTimeMillis());
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to recover shared MySQL point transfers: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	int add(VotingPluginUser user, int amount, boolean async) {
		if (async) {
			int predictedTotal = cachedPoints(user) + amount;
			run(() -> update(user, amount, false), true);
			// The mutation has not happened yet, so the historical asynchronous API
			// returns its predicted post-event total without blocking for storage.
			return predictedTotal;
		}
		return addAndReadCommitted(user, amount);
	}

	AddResult addCommitted(VotingPluginUser user, int amount) {
		return addAndReadCommittedResult(user, amount);
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

	boolean remove(VotingPluginUser user, int amount, boolean async) {
		if (!async) return remove(user, amount);
		boolean predictedSuccess = cachedPoints(user) >= amount;
		run(() -> update(user, -amount, true), true);
		// Preserve the historical asynchronous API contract: the caller receives
		// the cached prediction while the conditional database debit runs later.
		return predictedSuccess;
	}

	private int cachedPoints(VotingPluginUser user) {
		String path = user.getPointsPath();
		UserDataCache cache = user.getCache();
		if (cache != null) {
			synchronized (cache) {
				DataValue value = cache.getCache() == null ? null : cache.getCache().get(path);
				if (value != null) {
					if (value.isInt()) return value.getInt();
					if (value.isString()) {
						try {
							return Integer.parseInt(value.getString());
						} catch (NumberFormatException ignored) {
							// Fall through to the temporary cache/default below.
						}
					}
				}
			}
		}
		return user.getUserData().getInt(path, UserDataFetchMode.TEMP_ONLY);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int amount) {
		return transfer(source, target, amount, amount);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount, int creditAmount) {
		return transferAtomically(source, target, debitAmount, creditAmount);
	}

	/**
	 * Transfers points while allowing the recipient hook to approve or adjust the
	 * credit after the conditional debit has succeeded. The reservation transaction
	 * is committed and its connection is closed before the callback is invoked, so
	 * arbitrary listeners may safely read from the database. A durable journal then
	 * makes the refund/credit settlement idempotent.
	 */
	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			IntFunction<Integer> creditAmountProvider) {
		drainCache(source);
		drainCache(target);
		MySQL table = plugin.getMysql();
		String sourcePoints = source.getPointsPath();
		String targetPoints = target.getPointsPath();
		String transferId = UUID.randomUUID().toString();
		String owner = UUID.randomUUID().toString();
		SharedPointTransferJournal journal = null;
		try {
			journal = SharedPointTransferJournal.forTable(table);
			journal.recoverAndCleanup(System.currentTimeMillis());
			try {
				if (!journal.reserve(transferId, source.getUUID(), sourcePoints, debitAmount, target.getUUID(), debitAmount,
						System.currentTimeMillis())) return false;
			} catch (SQLException failure) {
				// If reservation commit acknowledgement was lost, release the source
				// only when the journal still proves the hook never started.
				journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				throw failure;
			}
			SharedPointTransferJournal.ClaimOutcome claim = journal.claimHookWithConfirmation(transferId, owner,
					System.currentTimeMillis());
			if (claim == SharedPointTransferJournal.ClaimOutcome.NOT_CLAIMED) {
				journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				return false;
			}
			if (claim == SharedPointTransferJournal.ClaimOutcome.INDETERMINATE) {
				logIndeterminateClaim(transferId);
				return true;
			}
			Integer creditAmount;
			try {
				creditAmount = creditAmountProvider.apply(debitAmount);
			} catch (RuntimeException failure) {
				SharedPointTransferJournal.SettlementOutcome outcome = journal.settleWithConfirmation(transferId, owner,
						source.getUUID(), sourcePoints, target.getUUID(), targetPoints, debitAmount, null);
				logApprovalFailure(failure);
				return isAcceptedSettlement(outcome);
			}
			// The approval hook may inspect or mutate the recipient and recreate its
			// cache after the initial drain. Persist and remove that cache before the
			// settlement credit so no queued pre-settlement value can overwrite it.
			drainCache(target);
			SharedPointTransferJournal.SettlementOutcome outcome = journal.settleWithConfirmation(transferId, owner,
					source.getUUID(), sourcePoints, target.getUUID(), targetPoints, debitAmount, creditAmount);
			return isAcceptedSettlement(outcome);
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		}
	}

	private boolean isAcceptedSettlement(SharedPointTransferJournal.SettlementOutcome outcome) {
		if (outcome == SharedPointTransferJournal.SettlementOutcome.INDETERMINATE) {
			// The callback has already run. Reporting a retryable failure could create
			// a second transfer after an unconfirmed credit, so retain the journal row
			// for explicit reconciliation and suppress a new debit attempt.
			plugin.getLogger().severe("Shared MySQL point transfer outcome is indeterminate; retaining journal entry for reconciliation");
			return true;
		}
		return outcome == SharedPointTransferJournal.SettlementOutcome.COMPLETED;
	}

	private void logIndeterminateClaim(String transferId) {
		plugin.getLogger().severe("Shared MySQL point transfer " + transferId
				+ " has indeterminate claim state (RESERVED or HOOK_STARTED); retaining it for explicit reconciliation");
	}

	private boolean transferAtomically(VotingPluginUser source, VotingPluginUser target,
			int debitAmount, int creditAmount) {
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

	/**
	 * Adds points and reads the resulting value through the same JDBC connection.
	 * This bypasses the wrapper's temporary user-data cache, which can remain stale
	 * even when the caller requests {@code NO_CACHE}.
	 */
	private int addAndReadCommitted(VotingPluginUser user, int amount) {
		return addAndReadCommittedResult(user, amount).total();
	}

	private AddResult addAndReadCommittedResult(VotingPluginUser user, int amount) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		String uuidMatch = table.qi("uuid") + (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		String update = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(points) + " = "
				+ table.qi(points) + " + ? WHERE " + uuidMatch;
		String read = "SELECT " + table.qi(points) + " FROM " + table.qi(table.getTableName()) + " WHERE " + uuidMatch;
		try (Connection connection = table.getMysql().getConnectionManager().getConnection();
				PreparedStatement updateStatement = connection.prepareStatement(update);
				PreparedStatement readStatement = connection.prepareStatement(read)) {
			updateStatement.setInt(1, amount);
			updateStatement.setString(2, user.getUUID());
			if (updateStatement.executeUpdate() != 1) return new AddResult(false, user.getPoints());
			readStatement.setString(1, user.getUUID());
			try (java.sql.ResultSet result = readStatement.executeQuery()) {
				return result.next() ? new AddResult(true, result.getInt(1))
						: new AddResult(false, user.getPoints());
			}
		} catch (SQLException failure) {
			logFailure(failure);
			return new AddResult(false, user.getPoints());
		}
	}

	record AddResult(boolean success, int total) {}

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
