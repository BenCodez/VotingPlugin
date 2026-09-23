package com.bencodez.votingplugin.voteshop.service;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;

/**
 * Durable shared-MySQL vote-shop debit state.
 *
 * <p>The reservation and conditional debit commit in one transaction.  A reward
 * must claim the reservation immediately before invoking the reward hook.  This
 * lets recovery refund only work which never reached that hook; a claimed row is
 * deliberately retained for reconciliation because a reward executor can have
 * arbitrary, non-idempotent side effects.</p>
 */
final class SharedMysqlPurchaseJournal {
	enum DailyStreakOutcome { APPLIED, ALREADY_UPDATED, DEFERRED }
	record DailyStreakResult(DailyStreakOutcome outcome, int streak) { }
	record RecoveredDailyStreak(UUID voteId, String uuid, int streak, boolean forceProxyRouting) { }
	record AccountingRecoveryBatch(boolean hadRows, List<UUID> pendingRewards) { }
	private static final String PENDING = "PENDING";
	private static final String HOOK_STARTED = "HOOK_STARTED";
	private static final String COMPENSATING = "COMPENSATING";
	private static final String COMPLETED = "COMPLETED";
	private static final String REFUNDED = "REFUNDED";
	static final String NO_LIMIT_RESET_GENERATION = "NONE";
	static final long PENDING_RECOVERY_AGE_MILLIS = TimeUnit.MINUTES.toMillis(5);
	static final long TERMINAL_RETENTION_MILLIS = TimeUnit.DAYS.toMillis(7);
	static final long ACCOUNTING_RETENTION_MILLIS = TimeUnit.DAYS.toMillis(30);
	private static final int RECOVERY_BATCH_SIZE = 32;
	private static final int CLEANUP_BATCH_SIZE = 100;
	/* PostgreSQL permits 63 bytes and is the tighter supported database limit. */
	private static final int MAX_IDENTIFIER_BYTES = 63;
	private static final String JOURNAL_SUFFIX = "_VoteShopPurchases";
	private static final String EPOCH_SUFFIX = "_VoteShopLimitEpochs";
	private static final String ACCOUNTING_SUFFIX = "_VoteAccounting";
	private static final String HASHED_TABLE_PREFIX = "vp_vsp_";
	private static final String HASHED_EPOCH_TABLE_PREFIX = "vp_vse_";
	private static final String HASHED_ACCOUNTING_TABLE_PREFIX = "vp_vsa_";
	private static final int HASHED_TABLE_HEX_LENGTH = 32;
	private static final int DAILY_TOTAL = 1;
	private static final int WEEKLY_TOTAL = 2;
	private static final int MONTH_TOTAL = 4;
	private static final int VOTE_PARTY_TOTAL = 8;
	private static final int DAILY_STREAK = 16;
	private static final int DAILY_STREAK_REWARD = 32;
	private static final int ACCOUNTING_DECIDED = 64;
	private static final int DAILY_STREAK_REWARD_CLAIMED = 128;
	private static final int RECOVERABLE_NON_REWARD_ACCOUNTING = DAILY_TOTAL | WEEKLY_TOTAL | MONTH_TOTAL
			| VOTE_PARTY_TOTAL | DAILY_STREAK;

	private static final ReferenceQueue<MySQL> INITIALIZED_QUEUE = new ReferenceQueue<>();
	private static final Set<IdentityWeakReference> INITIALIZED = new HashSet<>();

	private final MySQL table;
	private final String journalTable;
	private final String epochTable;
	private final String accountingTable;

	SharedMysqlPurchaseJournal(MySQL table, boolean initializeSchema) throws SQLException {
		this.table = table;
		journalTable = journalTableName(table.getTableName());
		epochTable = epochTableName(table.getTableName());
		accountingTable = accountingTableName(table.getTableName());
		if (initializeSchema) ensureSchema();
	}

	/**
	 * Keeps the historic auxiliary-table name where it is portable, while using
	 * a fixed, collision-resistant name for source tables which would exceed the
	 * PostgreSQL identifier limit.
	 */
	static String journalTableName(String sourceTable) {
		return auxiliaryTableName(sourceTable, JOURNAL_SUFFIX, HASHED_TABLE_PREFIX);
	}

	static String epochTableName(String sourceTable) {
		return auxiliaryTableName(sourceTable, EPOCH_SUFFIX, HASHED_EPOCH_TABLE_PREFIX);
	}

	static String accountingTableName(String sourceTable) {
		return auxiliaryTableName(sourceTable, ACCOUNTING_SUFFIX, HASHED_ACCOUNTING_TABLE_PREFIX);
	}

	private static String auxiliaryTableName(String sourceTable, String suffix, String hashedPrefix) {
		String legacyName = sourceTable + suffix;
		if (legacyName.getBytes(StandardCharsets.UTF_8).length <= MAX_IDENTIFIER_BYTES) return legacyName;
		return hashedPrefix + hash(sourceTable + '\0' + suffix).substring(0, HASHED_TABLE_HEX_LENGTH);
	}

	private static String hash(String value) {
		try {
			byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
			StringBuilder hex = new StringBuilder(digest.length * 2);
			for (byte valueByte : digest) {
				hex.append(Character.forDigit((valueByte >>> 4) & 0x0f, 16));
				hex.append(Character.forDigit(valueByte & 0x0f, 16));
			}
			return hex.toString();
		} catch (NoSuchAlgorithmException failure) {
			throw new IllegalStateException("SHA-256 is unavailable", failure);
		}
	}

	static SharedMysqlPurchaseJournal forTable(MySQL table) throws SQLException {
		synchronized (INITIALIZED) {
			expungeInitialized();
			for (IdentityWeakReference marker : INITIALIZED) {
				if (marker.get() == table) return new SharedMysqlPurchaseJournal(table, false);
			}
			new SharedMysqlPurchaseJournal(table, true);
			INITIALIZED.add(new IdentityWeakReference(table, INITIALIZED_QUEUE));
			return new SharedMysqlPurchaseJournal(table, false);
		}
	}

	private static void expungeInitialized() {
		IdentityWeakReference cleared;
		while ((cleared = (IdentityWeakReference) INITIALIZED_QUEUE.poll()) != null) {
			INITIALIZED.remove(cleared);
		}
		for (Iterator<IdentityWeakReference> iterator = INITIALIZED.iterator(); iterator.hasNext();) {
			if (iterator.next().get() == null) iterator.remove();
		}
	}

	/** Atomically records a pending purchase and conditionally charges it. */
	boolean reserve(String purchaseId, String uuid, String pointsColumn, String limitColumn, int cost, int limit,
			String limitGeneration, long limitGenerationExpiresAt, long now) throws SQLException {
		String insert = "INSERT INTO " + qiJournal() + " (" + qi("purchase_id") + ", " + qi("player_uuid")
				+ ", " + qi("points_column") + ", " + qi("limit_column") + ", " + qi("cost") + ", "
				+ qi("limit_value") + ", " + qi("limit_generation") + ", "
				+ qi("limit_generation_expires_at") + ", " + qi("limit_epoch") + ", " + qi("state")
				+ ", " + qi("created_at") + ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
		String points = qi(pointsColumn);
		StringBuilder debit = new StringBuilder("UPDATE ").append(qi(table.getTableName())).append(" SET ")
				.append(points).append(" = COALESCE(").append(points).append(", 0) - ?");
		if (limitColumn != null) {
			debit.append(", ").append(qi(limitColumn)).append(" = COALESCE(").append(qi(limitColumn))
					.append(", 0) + 1");
		}
		debit.append(" WHERE ").append(qi("uuid")).append(uuidCast()).append(" AND COALESCE(").append(points)
				.append(", 0) >= ?");
		if (limitColumn != null) {
			debit.append(" AND COALESCE(").append(qi(limitColumn)).append(", 0) < ?");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				Long limitEpoch = limitColumn == null ? null : lockLimitEpoch(connection, limitColumn);
				try (PreparedStatement insertStatement = connection.prepareStatement(insert);
						PreparedStatement debitStatement = connection.prepareStatement(debit.toString())) {
					insertStatement.setString(1, purchaseId);
					insertStatement.setString(2, uuid);
					insertStatement.setString(3, pointsColumn);
					insertStatement.setString(4, limitColumn);
					insertStatement.setInt(5, cost);
					if (limitColumn == null) insertStatement.setNull(6, java.sql.Types.INTEGER);
					else insertStatement.setInt(6, limit);
					if (limitGeneration == null) insertStatement.setNull(7, java.sql.Types.VARCHAR);
					else insertStatement.setString(7, limitGeneration);
					if (limitGenerationExpiresAt <= 0L) insertStatement.setNull(8, java.sql.Types.BIGINT);
					else insertStatement.setLong(8, limitGenerationExpiresAt);
					if (limitEpoch == null) insertStatement.setNull(9, java.sql.Types.BIGINT);
					else insertStatement.setLong(9, limitEpoch.longValue());
					insertStatement.setString(10, PENDING);
					insertStatement.setLong(11, now);
					insertStatement.executeUpdate();

					debitStatement.setInt(1, cost);
					debitStatement.setString(2, uuid);
					debitStatement.setInt(3, cost);
					if (limitColumn != null) debitStatement.setInt(4, limit);
					if (debitStatement.executeUpdate() != 1) {
						rollback(connection);
						return false;
					}
					return commitAndConfirm(connection, purchaseId, PENDING);
				}
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	/**
	 * Wipes a resettable limit and advances its epoch while holding the same row
	 * that reservations lock before they debit. A reservation can therefore land
	 * wholly before or wholly after the reset, never in the wiped interval.
	 */
	void resetLimit(String limitColumn, String resetGeneration) throws SQLException {
		if (!isSafeColumn(limitColumn)) throw new SQLException("Unsafe vote shop limit column");
		if (resetGeneration == null || resetGeneration.isEmpty() || resetGeneration.length() > 128) {
			throw new SQLException("Invalid vote shop reset generation");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow marker = lockLimitEpochRow(connection, limitColumn);
				if (resetGeneration.equals(marker.lastResetGeneration())) {
					rollback(connection);
					return;
				}
				long oldEpoch = marker.epoch();
				if (oldEpoch == Long.MAX_VALUE) throw new SQLException("Vote shop limit epoch overflow");
				long expectedEpoch = oldEpoch + 1L;
				try (PreparedStatement wipe = connection.prepareStatement("UPDATE " + qi(table.getTableName())
						+ " SET " + qi(limitColumn) + " = 0");
						PreparedStatement advance = connection.prepareStatement("UPDATE " + qiEpoch() + " SET "
								+ qi("epoch") + " = ?, " + qi("last_reset_generation") + " = ? WHERE "
								+ qi("limit_column") + " = ?")) {
					wipe.executeUpdate();
					advance.setLong(1, expectedEpoch);
					advance.setString(2, resetGeneration);
					advance.setString(3, limitColumn);
					if (advance.executeUpdate() != 1) throw new SQLException("Vote shop limit epoch marker missing");
				}
				try {
					connection.commit();
				} catch (SQLException ambiguousCommit) {
					closeQuietly(connection);
					EpochRow confirmed = findLimitEpoch(limitColumn);
					if (confirmed != null && resetGeneration.equals(confirmed.lastResetGeneration())) return;
					throw ambiguousCommit;
				}
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	/** Subtracts the copied boundary total once, retaining votes accepted later. */
	void resetPeriodTotal(String totalColumn, String previousColumn, String resetGeneration) throws SQLException {
		if (!isSafeColumn(totalColumn) || !isSafeColumn(previousColumn)) {
			throw new SQLException("Unsafe period total column");
		}
		if (resetGeneration == null || resetGeneration.isEmpty() || resetGeneration.length() > 128) {
			throw new SQLException("Invalid period total reset generation");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				String markerKey = "period-reset:" + totalColumn;
				EpochRow marker = lockLimitEpochRow(connection, markerKey);
				if (resetGeneration.equals(marker.lastResetGeneration())) {
					rollback(connection);
					return;
				}
				long oldEpoch = marker.epoch();
				if (oldEpoch == Long.MAX_VALUE) throw new SQLException("Period total reset epoch overflow");
				String difference = "COALESCE(" + qi(totalColumn) + ", 0) - COALESCE(" + qi(previousColumn)
						+ ", 0)";
				String bounded = "GREATEST(0, " + difference + ')';
				try (PreparedStatement reset = connection.prepareStatement("UPDATE " + qi(table.getTableName())
						+ " SET " + qi(totalColumn) + " = " + bounded);
						PreparedStatement advance = connection.prepareStatement("UPDATE " + qiEpoch() + " SET "
								+ qi("epoch") + " = ?, " + qi("last_reset_generation") + " = ? WHERE "
								+ qi("limit_column") + " = ?")) {
					reset.executeUpdate();
					advance.setLong(1, oldEpoch + 1L);
					advance.setString(2, resetGeneration);
					advance.setString(3, markerKey);
					if (advance.executeUpdate() != 1) throw new SQLException("Period total epoch marker missing");
				}
				connection.commit();
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private void commitAndConfirmRequested(Connection connection, UUID voteId, int operation) throws SQLException {
		try {
			connection.commit();
		} catch (SQLException ambiguousCommit) {
			closeQuietly(connection);
			try {
				AccountingRow row = findAccountingVote(voteId);
				if (row != null && (row.requested() & operation) == operation) return;
			} catch (SQLException confirmationFailure) {
				ambiguousCommit.addSuppressed(confirmationFailure);
			}
			throw ambiguousCommit;
		}
	}

	/**
	 * Serializes accepted-vote increments with the shared boundary marker. Every
	 * backend therefore agrees whether an increment precedes or follows the copy.
	 */
	boolean incrementPeriodTotals(UUID voteId, String uuid, String boundaryColumn, String previousColumn, List<String> columns,
			Integer maximum, boolean alreadyRequested)
			throws SQLException {
		if (voteId == null || uuid == null || uuid.isEmpty() || !isSafeColumn(boundaryColumn) || columns == null
				|| columns.isEmpty() || columns.stream().anyMatch(column -> !isSafeColumn(column))
				|| maximum != null && (maximum.intValue() < 0 || !isSafeColumn(previousColumn))) {
			throw new SQLException("Invalid period total increment");
		}
		int operation = accountingOperation(boundaryColumn);
		if (!alreadyRequested) {
			requestAccounting(voteId, uuid, operation, null,
					operation == MONTH_TOTAL && columns.size() > 1 ? columns.get(1) : null, maximum, null, null);
		}
		try {
			applyPeriodTotals(voteId, uuid, boundaryColumn, previousColumn, columns, maximum, operation);
			return true;
		} catch (SQLException deferred) {
			return false;
		}
	}

	boolean incrementPeriodTotals(UUID voteId, String uuid, String boundaryColumn, String previousColumn,
			List<String> columns, Integer maximum) throws SQLException {
		return incrementPeriodTotals(voteId, uuid, boundaryColumn, previousColumn, columns, maximum, false);
	}

	int prepareVoteAccounting(UUID voteId, String uuid, boolean countTotals, boolean countVoteParty,
			String monthColumn, Integer monthMaximum, boolean streakUsesPercentage, double streakPercentage,
			int enabledSiteCount, boolean forceProxyRouting, long acceptedAt) throws SQLException {
		int requested = (countTotals ? DAILY_TOTAL | WEEKLY_TOTAL | MONTH_TOTAL : 0)
				| (countVoteParty ? VOTE_PARTY_TOTAL : 0);
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow dailyCopyMarker = null;
				if ((requested & DAILY_TOTAL) != 0 || streakUsesPercentage) {
					dailyCopyMarker = lockLimitEpochRow(connection, "period-copy:DailyTotal");
				}
				if ((requested & MONTH_TOTAL) != 0) lockLimitEpochRow(connection, "period-copy:MonthTotal");
				if ((requested & VOTE_PARTY_TOTAL) != 0) lockLimitEpochRow(connection, "period-copy:VotePartyVotes");
				if ((requested & WEEKLY_TOTAL) != 0) lockLimitEpochRow(connection, "period-copy:WeeklyTotal");
				lockLimitEpochRow(connection, "streak-copy:DayVoteStreak");
				lockLimitEpochRow(connection, "streak-reset:DayVoteStreak");
				ensureAccountingVote(connection, voteId, uuid);
				AccountingRow row = findAndLockAccountingVote(connection, voteId);
				if (row == null || row.uuid() != null && !row.uuid().equals(uuid)) {
					throw new SQLException("Vote accounting identity does not match");
				}
				boolean accountingAlreadyDecided = (row.requested() & ACCOUNTING_DECIDED) != 0;
				boolean dailyResetPending = !accountingAlreadyDecided && streakUsesPercentage
						&& resetPending(connection, "DailyTotal", dailyCopyMarker);
				DailyStreakCandidate streak = accountingAlreadyDecided ? null
						: findDailyStreakCandidate(connection, uuid, dailyResetPending);
				boolean dailyAlreadyApplied = (row.completed() & DAILY_TOTAL) != 0;
				int projectedDailyTotal = accountingAlreadyDecided || !streakUsesPercentage ? 0
						: streak.dailyTotal() + countPendingDailyTotals(connection, voteId, uuid)
								+ (countTotals && !dailyAlreadyApplied ? 1 : 0);
				boolean percentageMet = accountingAlreadyDecided || !streakUsesPercentage || enabledSiteCount > 0
						&& (double) projectedDailyTotal / (double) enabledSiteCount * 100 > streakPercentage;
				boolean requestStreak = !accountingAlreadyDecided
						&& !sameLocalDay(streak.lastUpdate(), acceptedAt) && percentageMet;
				if (accountingAlreadyDecided) requested = row.requested();
				else if (requestStreak) requested |= DAILY_STREAK | DAILY_STREAK_REWARD;
				int persistedRequested = row.requested() | requested | ACCOUNTING_DECIDED;
				int persistedCompleted = row.completed() | ACCOUNTING_DECIDED;
				String update = "UPDATE " + qiAccounting() + " SET " + qi("player_uuid") + " = ?, "
						+ qi("requested") + " = ?, " + qi("completed") + " = ?, "
						+ qi("month_column") + " = COALESCE(?, "
						+ qi("month_column") + "), " + qi("month_maximum") + " = COALESCE(?, "
						+ qi("month_maximum") + "), " + qi("streak_value") + " = COALESCE(?, "
						+ qi("streak_value") + "), " + qi("streak_updated_at") + " = COALESCE(?, "
						+ qi("streak_updated_at") + "), " + qi("streak_force_proxy") + " = COALESCE(?, "
						+ qi("streak_force_proxy") + ") WHERE " + qi("vote_id") + " = ?";
				try (PreparedStatement statement = connection.prepareStatement(update)) {
					statement.setString(1, uuid);
					statement.setInt(2, persistedRequested);
					statement.setInt(3, persistedCompleted);
					statement.setString(4, monthColumn);
					if (monthMaximum == null) statement.setNull(5, java.sql.Types.INTEGER);
					else statement.setInt(5, monthMaximum.intValue());
					if (requestStreak) statement.setInt(6, streak.streak() + 1);
					else statement.setNull(6, java.sql.Types.INTEGER);
					if (requestStreak) statement.setLong(7, acceptedAt);
					else statement.setNull(7, java.sql.Types.BIGINT);
					if (requestStreak) statement.setInt(8, forceProxyRouting ? 1 : 0);
					else statement.setNull(8, java.sql.Types.INTEGER);
					statement.setString(9, voteId.toString());
					if (statement.executeUpdate() != 1) throw new SQLException("Vote accounting marker is missing");
				}
				commitAndConfirmRequested(connection, voteId, persistedRequested);
				return persistedRequested & ~ACCOUNTING_DECIDED;
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private int countPendingDailyTotals(Connection connection, UUID voteId, String uuid) throws SQLException {
		String select = "SELECT COUNT(*) FROM " + qiAccounting() + " WHERE " + qi("player_uuid")
				+ " = ? AND " + qi("vote_id") + " <> ? AND (" + qi("requested") + " & ?) <> 0 AND ("
				+ qi("completed") + " & ?) = 0 AND (" + qi("completed") + " & ?) <> 0";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, uuid);
			statement.setString(2, voteId.toString());
			statement.setInt(3, DAILY_TOTAL);
			statement.setInt(4, DAILY_TOTAL);
			statement.setInt(5, ACCOUNTING_DECIDED);
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? result.getInt(1) : 0;
			}
		}
	}

	private DailyStreakCandidate findDailyStreakCandidate(Connection connection, String uuid, boolean resetPending)
			throws SQLException {
		String dailyTotal = resetPending
				? "GREATEST(0, COALESCE(" + qi("DailyTotal") + ", 0) - COALESCE(" + qi("LastDailyTotal") + ", 0))"
				: "COALESCE(" + qi("DailyTotal") + ", 0)";
		String select = "SELECT " + dailyTotal + ", COALESCE(" + qi("DayVoteStreak")
				+ ", 0), " + qi("DayVoteStreakLastUpdate") + " FROM " + qi(table.getTableName()) + " WHERE "
				+ qi("uuid") + uuidCast() + " FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, uuid);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) return new DailyStreakCandidate(0, 0, null);
				return new DailyStreakCandidate(result.getInt(1), result.getInt(2), result.getString(3));
			}
		}
	}

	private void applyPeriodTotals(UUID voteId, String uuid, String boundaryColumn, String previousColumn,
			List<String> columns, Integer maximum, int operation) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow copyMarker = lockLimitEpochRow(connection, "period-copy:" + boundaryColumn);
				AccountingRow accounting = findAndLockAccountingVote(connection, voteId);
				if (accounting == null) throw new SQLException("Vote accounting marker is missing");
				int completed = accounting.completed();
				if ((completed & operation) != 0) {
					rollback(connection);
					return;
				}
				boolean resetPending = maximum != null && resetPending(connection, boundaryColumn, copyMarker);
				StringBuilder sql = new StringBuilder("UPDATE ").append(qi(table.getTableName())).append(" SET ");
				for (int index = 0; index < columns.size(); index++) {
					if (index > 0) sql.append(", ");
					String quoted = qi(columns.get(index));
					String increment = "COALESCE(" + quoted + ", 0) + 1";
					sql.append(quoted).append(" = ");
					if (maximum != null && resetPending && columns.get(index).equals(boundaryColumn)) {
						sql.append("LEAST(COALESCE(").append(qi(previousColumn)).append(", 0) + ?, ")
								.append(increment).append(')');
					} else if (maximum != null) sql.append("LEAST(?, ").append(increment).append(')');
					else sql.append(increment);
				}
				sql.append(" WHERE ").append(qi("uuid")).append(uuidCast());
				try (PreparedStatement update = connection.prepareStatement(sql.toString())) {
					int parameter = 1;
					if (maximum != null) {
						for (int ignored = 0; ignored < columns.size(); ignored++) {
							update.setInt(parameter++, maximum.intValue());
						}
					}
					update.setString(parameter, uuid);
					if (update.executeUpdate() != 1) throw new SQLException("Period total user row is missing");
				}
				markAccountingComplete(connection, voteId, completed | operation);
				commitAndConfirmAccounting(connection, voteId, operation);
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private static int accountingOperation(String boundaryColumn) throws SQLException {
		return switch (boundaryColumn) {
		case "DailyTotal" -> DAILY_TOTAL;
		case "WeeklyTotal" -> WEEKLY_TOTAL;
		case "MonthTotal" -> MONTH_TOTAL;
		case "VotePartyVotes" -> VOTE_PARTY_TOTAL;
		default -> throw new SQLException("Unsupported period total boundary");
		};
	}

	private void requestAccounting(UUID voteId, String uuid, int operation, String boundaryMarker, String monthColumn, Integer monthMaximum,
			Integer streakValue, Long streakUpdatedAt) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				if (boundaryMarker != null) lockLimitEpochRow(connection, boundaryMarker);
				ensureAccountingVote(connection, voteId, uuid);
				AccountingRow row = findAndLockAccountingVote(connection, voteId);
				if (row == null || row.uuid() != null && !row.uuid().equals(uuid)) {
					throw new SQLException("Vote accounting identity does not match");
				}
				String update = "UPDATE " + qiAccounting() + " SET " + qi("player_uuid") + " = ?, "
						+ qi("requested") + " = ?, " + qi("month_column") + " = COALESCE(?, "
						+ qi("month_column") + "), " + qi("month_maximum") + " = COALESCE(?, "
						+ qi("month_maximum") + "), " + qi("streak_value") + " = COALESCE(?, "
						+ qi("streak_value") + "), " + qi("streak_updated_at") + " = COALESCE(?, "
						+ qi("streak_updated_at") + ") WHERE " + qi("vote_id") + " = ?";
				try (PreparedStatement statement = connection.prepareStatement(update)) {
					statement.setString(1, uuid);
					statement.setInt(2, row.requested() | operation);
					statement.setString(3, monthColumn);
					if (monthMaximum == null) statement.setNull(4, java.sql.Types.INTEGER);
					else statement.setInt(4, monthMaximum.intValue());
					if (streakValue == null) statement.setNull(5, java.sql.Types.INTEGER);
					else statement.setInt(5, streakValue.intValue());
					if (streakUpdatedAt == null) statement.setNull(6, java.sql.Types.BIGINT);
					else statement.setLong(6, streakUpdatedAt.longValue());
					statement.setString(7, voteId.toString());
					if (statement.executeUpdate() != 1) throw new SQLException("Vote accounting marker is missing");
				}
				commitAndConfirmRequested(connection, voteId, operation);
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private void ensureAccountingVote(Connection connection, UUID voteId, String uuid) throws SQLException {
		String insert = table.getDbType() == DbType.POSTGRESQL
				? "INSERT INTO " + qiAccounting() + " (" + qi("vote_id") + ", " + qi("player_uuid") + ", "
						+ qi("requested") + ", " + qi("completed") + ", " + qi("created_at")
						+ ") VALUES (?, ?, 0, 0, ?) ON CONFLICT DO NOTHING"
				: "INSERT IGNORE INTO " + qiAccounting() + " (" + qi("vote_id") + ", " + qi("player_uuid") + ", "
						+ qi("requested") + ", " + qi("completed") + ", " + qi("created_at")
						+ ") VALUES (?, ?, 0, 0, ?)";
		try (PreparedStatement statement = connection.prepareStatement(insert)) {
			statement.setString(1, voteId.toString());
			statement.setString(2, uuid);
			statement.setLong(3, System.currentTimeMillis());
			statement.executeUpdate();
		}
	}

	private AccountingRow findAndLockAccountingVote(Connection connection, UUID voteId) throws SQLException {
		String select = "SELECT " + accountingColumns() + " FROM " + qiAccounting() + " WHERE "
				+ qi("vote_id") + " = ? FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, voteId.toString());
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? accountingRow(result) : null;
			}
		}
	}

	private void markAccountingComplete(Connection connection, UUID voteId, int completed) throws SQLException {
		markAccountingComplete(connection, voteId, completed, null);
	}

	private void markAccountingComplete(Connection connection, UUID voteId, int completed, Integer appliedStreak)
			throws SQLException {
		String update = "UPDATE " + qiAccounting() + " SET " + qi("completed") + " = ?, "
				+ qi("streak_applied_value") + " = COALESCE(?, " + qi("streak_applied_value") + "), "
				+ qi("created_at") + " = ? WHERE "
				+ qi("vote_id") + " = ?";
		try (PreparedStatement statement = connection.prepareStatement(update)) {
			statement.setInt(1, completed);
			if (appliedStreak == null) statement.setNull(2, java.sql.Types.INTEGER);
			else statement.setInt(2, appliedStreak.intValue());
			statement.setLong(3, System.currentTimeMillis());
			statement.setString(4, voteId.toString());
			if (statement.executeUpdate() != 1) throw new SQLException("Vote accounting marker is missing");
		}
	}

	private void commitAndConfirmAccounting(Connection connection, UUID voteId, int operation) throws SQLException {
		try {
			connection.commit();
		} catch (SQLException ambiguousCommit) {
			closeQuietly(connection);
			try {
				AccountingRow row = findAccountingVote(voteId);
				if (row != null && (row.completed() & operation) == operation) return;
			} catch (SQLException confirmationFailure) {
				ambiguousCommit.addSuppressed(confirmationFailure);
			}
			throw ambiguousCommit;
		}
	}

	private AccountingRow findAccountingVote(UUID voteId) throws SQLException {
		String select = "SELECT " + accountingColumns() + " FROM " + qiAccounting() + " WHERE "
				+ qi("vote_id") + " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, voteId.toString());
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? accountingRow(result) : null;
			}
		}
	}

	RecoveredDailyStreak claimDailyStreakReward(UUID voteId) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				AccountingRow row = findAndLockAccountingVote(connection, voteId);
				if (row == null || (row.requested() & DAILY_STREAK_REWARD) == 0
						|| (row.completed() & DAILY_STREAK) == 0
						|| (row.completed() & DAILY_STREAK_REWARD) != 0) {
					rollback(connection);
					return null;
				}
				if ((row.completed() & DAILY_STREAK_REWARD_CLAIMED) != 0) {
					throw new SQLException("Daily streak reward may already have run and requires manual reconciliation");
				}
				if (row.streakAppliedValue() == null) {
					throw new SQLException("Applied daily streak value is missing");
				}
				int streak = row.streakAppliedValue().intValue();
				markAccountingComplete(connection, voteId, row.completed() | DAILY_STREAK_REWARD_CLAIMED);
				commitAndConfirmAccounting(connection, voteId, DAILY_STREAK_REWARD_CLAIMED);
				return new RecoveredDailyStreak(voteId, row.uuid(), streak,
						row.streakForceProxy() != null && row.streakForceProxy().intValue() != 0);
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	void completeDailyStreakReward(UUID voteId) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				AccountingRow row = findAndLockAccountingVote(connection, voteId);
				if (row == null) throw new SQLException("Vote accounting marker is missing");
				if ((row.completed() & DAILY_STREAK_REWARD) != 0) {
					rollback(connection);
					return;
				}
				if ((row.completed() & DAILY_STREAK_REWARD_CLAIMED) == 0) {
					throw new SQLException("Daily streak reward was not claimed");
				}
				int completed = (row.completed() | DAILY_STREAK_REWARD) & ~DAILY_STREAK_REWARD_CLAIMED;
				markAccountingComplete(connection, voteId, completed);
				commitAndConfirmAccounting(connection, voteId, DAILY_STREAK_REWARD);
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private String accountingColumns() {
		return qi("player_uuid") + ", " + qi("requested") + ", " + qi("completed") + ", "
				+ qi("month_column") + ", " + qi("month_maximum") + ", " + qi("streak_value") + ", "
				+ qi("streak_updated_at") + ", " + qi("streak_force_proxy") + ", "
				+ qi("streak_applied_value");
	}

	private static AccountingRow accountingRow(ResultSet result) throws SQLException {
		return new AccountingRow(result.getString(1), result.getInt(2), result.getInt(3), result.getString(4),
				nullableInteger(result, 5), nullableInteger(result, 6), nullableLong(result, 7),
				nullableInteger(result, 8), nullableInteger(result, 9));
	}

	private boolean resetPending(Connection connection, String boundaryColumn, EpochRow copyMarker)
			throws SQLException {
		if (copyMarker == null) return false;
		String copyTransition = generationTransition(copyMarker.lastResetGeneration(), "time-copy:");
		if (copyTransition == null) return false;
		EpochRow resetMarker = lockLimitEpochRow(connection, "period-reset:" + boundaryColumn);
		String resetTransition = generationTransition(resetMarker.lastResetGeneration(), "time-total:");
		return copyTransition != null && !copyTransition.equals(resetTransition);
	}

	private static String generationTransition(String generation, String prefix) {
		return generation != null && generation.startsWith(prefix) ? generation.substring(prefix.length()) : null;
	}

	/** Serializes the accepted daily-streak value and timestamp with its shared boundary. */
	DailyStreakResult updateDailyStreak(UUID voteId, String uuid, int streak, long updatedAt,
			boolean alreadyRequested) throws SQLException {
		if (alreadyRequested) {
			AccountingRow row = findAccountingVote(voteId);
			if (row == null || (row.requested() & DAILY_STREAK) == 0) {
				return new DailyStreakResult(DailyStreakOutcome.ALREADY_UPDATED, 0);
			}
			if (row.streakValue() == null || row.streakUpdatedAt() == null) {
				throw new SQLException("Admitted daily streak payload is incomplete");
			}
			streak = row.streakValue().intValue();
			updatedAt = row.streakUpdatedAt().longValue();
		} else {
			requestAccounting(voteId, uuid, DAILY_STREAK | DAILY_STREAK_REWARD,
					"streak-copy:DayVoteStreak", null, null, Integer.valueOf(streak), Long.valueOf(updatedAt));
		}
		try {
			return applyDailyStreak(voteId, uuid, streak, updatedAt);
		} catch (SQLException deferred) {
			return new DailyStreakResult(DailyStreakOutcome.DEFERRED, 0);
		}
	}

	private DailyStreakResult applyDailyStreak(UUID voteId, String uuid, int streak, long updatedAt) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow copyMarker = lockLimitEpochRow(connection, "streak-copy:DayVoteStreak");
				EpochRow resetMarker = lockLimitEpochRow(connection, "streak-reset:DayVoteStreak");
				String copyTransition = generationTransition(copyMarker.lastResetGeneration(), "time-streak-copy:");
				String resetTransition = generationTransition(resetMarker.lastResetGeneration(), "time-streak-reset:");
				if (copyTransition != null && !copyTransition.equals(resetTransition)) {
					throw new SQLException("Daily streak boundary reset is still active");
				}
				AccountingRow accounting = findAndLockAccountingVote(connection, voteId);
				if (accounting == null) throw new SQLException("Vote accounting marker is missing");
				int completed = accounting.completed();
				if ((completed & DAILY_STREAK) != 0) {
					Integer appliedStreak = accounting.streakAppliedValue();
					rollback(connection);
					DailyStreakOutcome outcome = (completed & DAILY_STREAK_REWARD) == 0
							? DailyStreakOutcome.APPLIED : DailyStreakOutcome.ALREADY_UPDATED;
					if (outcome == DailyStreakOutcome.APPLIED && appliedStreak == null) {
						throw new SQLException("Applied daily streak value is missing");
					}
					return new DailyStreakResult(outcome, appliedStreak == null ? 0 : appliedStreak.intValue());
				}
				if (hasEarlierPendingDailyStreak(connection, voteId, uuid, updatedAt)) {
					throw new SQLException("An earlier daily streak is still pending");
				}
				String persistedUpdate = findDailyStreakUpdate(connection, uuid);
				if (sameLocalDay(persistedUpdate, updatedAt)) {
					int persistedStreak = findDailyStreakValue(connection, uuid);
					markAccountingComplete(connection, voteId,
							completed | DAILY_STREAK | (accounting.requested() & DAILY_STREAK_REWARD));
					commitAndConfirmAccounting(connection, voteId,
							DAILY_STREAK | (accounting.requested() & DAILY_STREAK_REWARD));
					return new DailyStreakResult(DailyStreakOutcome.ALREADY_UPDATED, persistedStreak);
				}
				String sql = "UPDATE " + qi(table.getTableName()) + " SET " + qi("DayVoteStreak") + " = COALESCE("
						+ qi("DayVoteStreak") + ", 0) + 1, " + qi("DayVoteStreakLastUpdate") + " = ? WHERE "
						+ qi("uuid") + uuidCast();
				try (PreparedStatement update = connection.prepareStatement(sql)) {
					update.setString(1, Long.toString(updatedAt));
					update.setString(2, uuid);
					if (update.executeUpdate() != 1) throw new SQLException("Daily streak user row is missing");
				}
				int persistedStreak = findDailyStreakValue(connection, uuid);
				markAccountingComplete(connection, voteId, completed | DAILY_STREAK,
						Integer.valueOf(persistedStreak));
				commitAndConfirmAccounting(connection, voteId, DAILY_STREAK);
				return new DailyStreakResult(DailyStreakOutcome.APPLIED, persistedStreak);
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private boolean hasEarlierPendingDailyStreak(Connection connection, UUID voteId, String uuid, long updatedAt)
			throws SQLException {
		int streakOperations = DAILY_STREAK | DAILY_STREAK_REWARD;
		String select = "SELECT " + qi("vote_id") + " FROM " + qiAccounting() + " WHERE "
				+ qi("player_uuid") + " = ? AND " + qi("vote_id") + " <> ? AND " + qi("streak_updated_at")
				+ " < ? AND (" + qi("requested") + " & ?) <> (" + qi("completed")
				+ " & ?) LIMIT 1 FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, uuid);
			statement.setString(2, voteId.toString());
			statement.setLong(3, updatedAt);
			statement.setInt(4, streakOperations);
			statement.setInt(5, streakOperations);
			try (ResultSet result = statement.executeQuery()) {
				return result.next();
			}
		}
	}

	private int findDailyStreakValue(Connection connection, String uuid) throws SQLException {
		String select = "SELECT COALESCE(" + qi("DayVoteStreak") + ", 0) FROM " + qi(table.getTableName())
				+ " WHERE " + qi("uuid") + uuidCast();
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, uuid);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) throw new SQLException("Daily streak user row is missing");
				return result.getInt(1);
			}
		}
	}

	private String findDailyStreakUpdate(Connection connection, String uuid) throws SQLException {
		String select = "SELECT " + qi("DayVoteStreakLastUpdate") + " FROM " + qi(table.getTableName())
				+ " WHERE " + qi("uuid") + uuidCast() + " FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, uuid);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) throw new SQLException("Daily streak user row is missing");
				return result.getString(1);
			}
		}
	}

	static boolean sameLocalDay(String persistedUpdate, long updatedAt) {
		if (persistedUpdate == null || persistedUpdate.isEmpty()) return false;
		try {
			long persisted = Long.parseLong(persistedUpdate);
			ZoneId zone = ZoneId.systemDefault();
			return Instant.ofEpochMilli(persisted).atZone(zone).toLocalDate()
					.equals(Instant.ofEpochMilli(updatedAt).atZone(zone).toLocalDate());
		} catch (NumberFormatException invalidLegacyTimestamp) {
			return false;
		}
	}

	void resetDailyStreakAtBoundary(String uuid, long boundaryUpdatedAt) throws SQLException {
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				lockLimitEpochRow(connection, "streak-copy:DayVoteStreak");
				String streak = qi("DayVoteStreak");
				String updated = qi("DayVoteStreakLastUpdate");
				String sql = "UPDATE " + qi(table.getTableName()) + " SET " + streak + " = CASE WHEN COALESCE("
						+ updated + ", '') = ? THEN 0 ELSE GREATEST(COALESCE(" + streak + ", 0), 1) END WHERE "
						+ qi("uuid") + uuidCast();
				try (PreparedStatement statement = connection.prepareStatement(sql)) {
					statement.setString(1, Long.toString(boundaryUpdatedAt));
					statement.setString(2, uuid);
					if (statement.executeUpdate() != 1) throw new SQLException("Daily streak user row is missing");
				}
				connection.commit();
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	/** Publishes that every copied daily streak has been reset for this transition. */
	void completeDailyStreakReset(String generation) throws SQLException {
		if (generation == null || !generation.startsWith("time-streak-reset:") || generation.length() > 128) {
			throw new SQLException("Invalid daily streak reset generation");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow copyMarker = lockLimitEpochRow(connection, "streak-copy:DayVoteStreak");
				EpochRow resetMarker = lockLimitEpochRow(connection, "streak-reset:DayVoteStreak");
				if (generation.equals(resetMarker.lastResetGeneration())) {
					rollback(connection);
					return;
				}
				String copiedTransition = generationTransition(copyMarker.lastResetGeneration(), "time-streak-copy:");
				String resetTransition = generationTransition(generation, "time-streak-reset:");
				if (!resetTransition.equals(copiedTransition)) {
					rollback(connection);
					return;
				}
				String update = "UPDATE " + qiEpoch() + " SET " + qi("epoch") + " = ?, "
						+ qi("last_reset_generation") + " = ? WHERE " + qi("limit_column") + " = ?";
				try (PreparedStatement statement = connection.prepareStatement(update)) {
					statement.setLong(1, resetMarker.epoch() + 1L);
					statement.setString(2, generation);
					statement.setString(3, "streak-reset:DayVoteStreak");
					if (statement.executeUpdate() != 1) throw new SQLException("Daily streak reset marker is missing");
				}
				connection.commit();
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	/** Copies the period boundary once so a phase-receipt retry cannot move it. */
	void copyPeriodBoundary(String totalColumn, String previousColumn, String generation) throws SQLException {
		if (!isSafeColumn(totalColumn) || !isSafeColumn(previousColumn)) {
			throw new SQLException("Unsafe period boundary column");
		}
		copyBoundary("period-copy:" + totalColumn, generation, accountingOperation(totalColumn),
				qi(previousColumn) + " = COALESCE(" + qi(totalColumn) + ", 0)");
	}

	/** Copies the daily-streak value and timestamp in one recoverable transaction. */
	void copyDailyStreakBoundary(String streakColumn, String previousStreakColumn, String updateColumn,
			String previousUpdateColumn, String generation) throws SQLException {
		if (!isSafeColumn(streakColumn) || !isSafeColumn(previousStreakColumn) || !isSafeColumn(updateColumn)
				|| !isSafeColumn(previousUpdateColumn)) {
			throw new SQLException("Unsafe daily streak boundary column");
		}
		copyBoundary("streak-copy:" + streakColumn, generation, DAILY_STREAK,
				qi(previousStreakColumn) + " = COALESCE(" + qi(streakColumn) + ", 0), "
						+ qi(previousUpdateColumn) + " = COALESCE(" + qi(updateColumn) + ", '')");
	}

	private void copyBoundary(String markerKey, String generation, int accountingOperation, String assignments) throws SQLException {
		if (generation == null || generation.isEmpty() || generation.length() > 128) {
			throw new SQLException("Invalid period boundary generation");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				EpochRow marker = lockLimitEpochRow(connection, markerKey);
				if (generation.equals(marker.lastResetGeneration())) {
					rollback(connection);
					return;
				}
				long oldEpoch = marker.epoch();
				if (oldEpoch == Long.MAX_VALUE) throw new SQLException("Period boundary epoch overflow");
				drainPendingAccounting(connection, accountingOperation);
				try (PreparedStatement copy = connection.prepareStatement("UPDATE " + qi(table.getTableName())
						+ " SET " + assignments);
						PreparedStatement advance = connection.prepareStatement("UPDATE " + qiEpoch() + " SET "
								+ qi("epoch") + " = ?, " + qi("last_reset_generation") + " = ? WHERE "
								+ qi("limit_column") + " = ?")) {
					copy.executeUpdate();
					advance.setLong(1, oldEpoch + 1L);
					advance.setString(2, generation);
					advance.setString(3, markerKey);
					if (advance.executeUpdate() != 1) throw new SQLException("Period boundary epoch marker missing");
				}
				connection.commit();
			} catch (SQLException failure) {
				rollback(connection);
				throw failure;
			}
		}
	}

	private void drainPendingAccounting(Connection connection, int operation) throws SQLException {
		String order = operation == DAILY_STREAK
				? " ORDER BY " + qi("streak_updated_at") + " ASC, " + qi("created_at") + " ASC" : "";
		String select = "SELECT " + qi("vote_id") + ", " + accountingColumns() + " FROM " + qiAccounting()
				+ " WHERE (" + qi("requested") + " & ?) <> 0 AND (" + qi("completed")
				+ " & ?) = 0" + order + " FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setInt(1, operation);
			statement.setInt(2, operation);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					UUID voteId = UUID.fromString(result.getString(1));
					AccountingRow row = new AccountingRow(result.getString(2), result.getInt(3), result.getInt(4),
								result.getString(5), nullableInteger(result, 6), nullableInteger(result, 7),
								nullableLong(result, 8), nullableInteger(result, 9), nullableInteger(result, 10));
					applyPendingAccounting(connection, voteId, row, operation);
				}
			}
		}
	}

	private void applyPendingAccounting(Connection connection, UUID voteId, AccountingRow row, int operation)
			throws SQLException {
		if (operation == DAILY_STREAK) {
			if (row.streakUpdatedAt() == null) throw new SQLException("Pending daily streak accounting payload is incomplete");
			if (sameLocalDay(findDailyStreakUpdate(connection, row.uuid()), row.streakUpdatedAt().longValue())) {
				markAccountingComplete(connection, voteId,
						row.completed() | DAILY_STREAK | (row.requested() & DAILY_STREAK_REWARD));
				return;
			}
			String sql = "UPDATE " + qi(table.getTableName()) + " SET " + qi("DayVoteStreak") + " = COALESCE("
					+ qi("DayVoteStreak") + ", 0) + 1, " + qi("DayVoteStreakLastUpdate") + " = ? WHERE "
					+ qi("uuid") + uuidCast();
			try (PreparedStatement update = connection.prepareStatement(sql)) {
				update.setString(1, Long.toString(row.streakUpdatedAt().longValue()));
				update.setString(2, row.uuid());
				if (update.executeUpdate() != 1) throw new SQLException("Daily streak user row is missing");
			}
			int persistedStreak = findDailyStreakValue(connection, row.uuid());
			markAccountingComplete(connection, voteId, row.completed() | operation,
					Integer.valueOf(persistedStreak));
			return;
		}
		String boundary = operation == DAILY_TOTAL ? "DailyTotal"
				: operation == WEEKLY_TOTAL ? "WeeklyTotal"
				: operation == MONTH_TOTAL ? "MonthTotal" : "VotePartyVotes";
		List<String> columns = operation == MONTH_TOTAL && row.monthColumn() != null
				? List.of(boundary, row.monthColumn()) : List.of(boundary);
		StringBuilder sql = new StringBuilder("UPDATE ").append(qi(table.getTableName())).append(" SET ");
		for (int index = 0; index < columns.size(); index++) {
			if (index > 0) sql.append(", ");
			String quoted = qi(columns.get(index));
			sql.append(quoted).append(" = ");
			if (row.monthMaximum() != null) sql.append("LEAST(?, ");
			sql.append("COALESCE(").append(quoted).append(", 0) + 1");
			if (row.monthMaximum() != null) sql.append(')');
		}
		sql.append(" WHERE ").append(qi("uuid")).append(uuidCast());
		try (PreparedStatement update = connection.prepareStatement(sql.toString())) {
			int parameter = 1;
			if (row.monthMaximum() != null) {
				for (int ignored = 0; ignored < columns.size(); ignored++) {
					update.setInt(parameter++, row.monthMaximum().intValue());
				}
			}
			update.setString(parameter, row.uuid());
			if (update.executeUpdate() != 1) throw new SQLException("Period total user row is missing");
		}
		markAccountingComplete(connection, voteId, row.completed() | operation);
	}

	/**
	 * A JDBC commit error does not prove that the database discarded the
	 * transaction. Close the possibly-broken handle before looking up the same
	 * id, which also keeps a one-connection pool from deadlocking itself.
	 */
	private boolean commitAndConfirm(Connection connection, String purchaseId, String expectedState)
			throws SQLException {
		try {
			connection.commit();
			return true;
		} catch (SQLException ambiguousCommit) {
			closeQuietly(connection);
			PurchaseRow row = find(purchaseId);
			if (row != null && expectedState.equals(row.state())) return true;
			throw ambiguousCommit;
		}
	}

	private PurchaseRow find(String purchaseId) throws SQLException {
		String select = "SELECT " + qi("state") + " FROM " + qiJournal() + " WHERE " + qi("purchase_id")
				+ " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, purchaseId);
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? new PurchaseRow(result.getString(1)) : null;
			}
		}
	}

	/** Claims a still-pending debit immediately before the external reward hook. */
	ClaimOutcome claimReward(String purchaseId, long startedAt) throws SQLException {
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ?, " + qi("hook_started_at")
				+ " = ? WHERE " + qi("purchase_id") + " = ? AND " + qi("state") + " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(update)) {
			statement.setString(1, HOOK_STARTED);
			statement.setLong(2, startedAt);
			statement.setString(3, purchaseId);
			statement.setString(4, PENDING);
			try {
				return statement.executeUpdate() == 1 ? ClaimOutcome.CLAIMED : ClaimOutcome.NOT_CLAIMED;
			} catch (SQLException ambiguousUpdate) {
				// An autocommit update can reach the database even when its acknowledgement
				// does not reach this process. Release the suspect handle before confirming
				// through a fresh connection, including with a one-connection pool.
				closeQuietly(connection);
				try {
					PurchaseRow row = find(purchaseId);
					if (row != null && HOOK_STARTED.equals(row.state())) return ClaimOutcome.CLAIMED;
					if (row != null && PENDING.equals(row.state())) return ClaimOutcome.NOT_CLAIMED;
				} catch (SQLException confirmationFailure) {
					ambiguousUpdate.addSuppressed(confirmationFailure);
				}
				return ClaimOutcome.INDETERMINATE;
			}
		}
	}

	void complete(String purchaseId) throws SQLException {
		setTerminal(purchaseId, COMPLETED, 0L);
	}

	/** Refunds only a debit whose reward hook has not started. */
	boolean refundPending(String purchaseId) throws SQLException {
		return refundPending(purchaseId, System.currentTimeMillis());
	}

	boolean refundPending(String purchaseId, long now) throws SQLException {
		return setTerminal(purchaseId, REFUNDED, now, PENDING);
	}

	RefundedPurchase refundPendingDetails(String purchaseId, long now) throws SQLException {
		return setTerminalDetails(purchaseId, REFUNDED, now, PENDING);
	}

	/**
	 * Compensates a pending or claimed purchase only when the local scheduler
	 * guard proves that its reward callback cannot run. The intermediate durable
	 * state makes a failed refund retryable after a database outage or restart.
	 */
	boolean refundUnstartedReward(String purchaseId) throws SQLException {
		SQLException lastFailure = null;
		for (int attempt = 0; attempt < 3; attempt++) {
			try {
				if (!requestUnstartedRewardRefund(purchaseId)) {
					PurchaseRow row = find(purchaseId);
					return row != null && REFUNDED.equals(row.state());
				}
				return refundCompensatingReward(purchaseId);
			} catch (SQLException failure) {
				lastFailure = failure;
			}
		}
		throw lastFailure;
	}

	/**
	 * Durably fences a rejected reward callback before another scheduler is used.
	 *
	 * <p>The caller has already won the local scheduler state race, so recovery may
	 * safely refund this row even if the persistence or Bukkit fallback scheduler
	 * is rejected or the process stops before its refund task starts.</p>
	 */
	boolean markCompensating(String purchaseId) throws SQLException {
		SQLException lastFailure = null;
		for (int attempt = 0; attempt < 3; attempt++) {
			try {
				return requestUnstartedRewardRefund(purchaseId);
			} catch (SQLException failure) {
				lastFailure = failure;
			}
		}
		throw lastFailure;
	}

	/** Durable marker used before attempting compensation, so recovery can retry it. */
	private boolean requestUnstartedRewardRefund(String purchaseId) throws SQLException {
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("purchase_id")
				+ " = ? AND " + qi("state") + " IN (?, ?, ?)";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(update)) {
			connection.setAutoCommit(false);
			statement.setString(1, COMPENSATING);
			statement.setString(2, purchaseId);
			statement.setString(3, PENDING);
			statement.setString(4, HOOK_STARTED);
			statement.setString(5, COMPENSATING);
			if (statement.executeUpdate() != 1) return false;
			return commitAndConfirm(connection, purchaseId, COMPENSATING);
		}
	}

	/** Retries the already-marked compensation without reopening the hook. */
	boolean refundCompensatingReward(String purchaseId) throws SQLException {
		return setTerminal(purchaseId, REFUNDED, System.currentTimeMillis(), COMPENSATING);
	}

	private RefundedPurchase refundCompensatingRewardDetails(String purchaseId) throws SQLException {
		return setTerminalDetails(purchaseId, REFUNDED, System.currentTimeMillis(), COMPENSATING);
	}

	private boolean setTerminal(String purchaseId, String terminalState, long now, String... refundableStates)
			throws SQLException {
		return setTerminalDetails(purchaseId, terminalState, now, refundableStates) != null;
	}

	private RefundedPurchase setTerminalDetails(String purchaseId, String terminalState, long now,
			String... refundableStates)
			throws SQLException {
		boolean refund = REFUNDED.equals(terminalState);
		String refundedUuid = null;
		String refundedPointsColumn = null;
		String refundedLimitColumn = null;
		String select = "SELECT " + qi("state") + ", " + qi("player_uuid") + ", " + qi("points_column")
				+ ", " + qi("limit_column") + ", " + qi("cost") + ", " + qi("limit_generation") + ", "
				+ qi("limit_generation_expires_at") + ", " + qi("limit_epoch") + " FROM " + qiJournal() + " WHERE "
				+ qi("purchase_id") + " = ? FOR UPDATE";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, purchaseId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next()) {
						rollback(connection);
						return null;
					}
					String state = result.getString(1);
					if (COMPLETED.equals(state) || REFUNDED.equals(state)) {
						rollback(connection);
						return terminalState.equals(state) ? new RefundedPurchase(null, null, null) : null;
					}
					if (refund && !isRefundableState(state, refundableStates)) {
						rollback(connection);
						return null;
					}
					if (!refund && !HOOK_STARTED.equals(state)) {
						rollback(connection);
						return null;
					}
					String uuid = result.getString(2);
					String pointsColumn = result.getString(3);
					String limitColumn = result.getString(4);
					refundedUuid = uuid;
					refundedPointsColumn = pointsColumn;
					refundedLimitColumn = limitColumn;
					int cost = result.getInt(5);
					String limitGeneration = result.getString(6);
					Long limitEpoch = nullableLong(result, 8);
					if (refund) {
						refund(connection, uuid, pointsColumn, limitColumn, cost,
								shouldRefundLimit(connection, limitColumn, limitGeneration, limitEpoch));
					}
				}
			}
			String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("purchase_id")
					+ " = ?";
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, terminalState);
				updateStatement.setString(2, purchaseId);
				if (updateStatement.executeUpdate() != 1) {
					rollback(connection);
					return null;
				}
			}
			if (!commitAndConfirm(connection, purchaseId, terminalState)) return null;
			return refund ? new RefundedPurchase(refundedUuid, refundedPointsColumn, refundedLimitColumn)
					: new RefundedPurchase(null, null, null);
		} catch (SQLException failure) {
			throw failure;
		}
	}

	private static boolean isRefundableState(String state, String... refundableStates) {
		for (String refundableState : refundableStates) {
			if (refundableState.equals(state)) return true;
		}
		return false;
	}

	private void refund(Connection connection, String uuid, String pointsColumn, String limitColumn, int cost,
			boolean refundLimit) throws SQLException {
		if (!isSafeColumn(pointsColumn) || (limitColumn != null && !isSafeColumn(limitColumn))) {
			throw new SQLException("Unsafe durable purchase column");
		}
		StringBuilder refund = new StringBuilder("UPDATE ").append(qi(table.getTableName())).append(" SET ")
				.append(qi(pointsColumn)).append(" = ").append(qi(pointsColumn)).append(" + ?");
		if (refundLimit) {
			refund.append(", ").append(qi(limitColumn)).append(" = GREATEST(COALESCE(").append(qi(limitColumn))
					.append(", 0) - 1, 0)");
		}
		refund.append(" WHERE ").append(qi("uuid")).append(uuidCast());
		try (PreparedStatement statement = connection.prepareStatement(refund.toString())) {
			statement.setInt(1, cost);
			statement.setString(2, uuid);
			if (statement.executeUpdate() != 1) throw new SQLException("Purchase refund player missing");
		}
	}

	private boolean shouldRefundLimit(Connection connection, String limitColumn, String generation, Long storedEpoch)
			throws SQLException {
		if (limitColumn == null) return false;
		if (storedEpoch != null) {
			EpochRow currentEpoch = findAndLockLimitEpoch(connection, limitColumn);
			return currentEpoch != null && storedEpoch.longValue() == currentEpoch.epoch();
		}
		// Legacy rows did not capture a durable epoch, so a resettable limit cannot
		// be identified safely. NONE has never reset and keeps its historic refund.
		return NO_LIMIT_RESET_GENERATION.equals(generation);
	}

	List<RefundedPurchase> recoverAndCleanup(long now) throws SQLException {
		long cutoff = now - PENDING_RECOVERY_AGE_MILLIS;
		String select = "SELECT " + qi("purchase_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " = ? AND " + qi("created_at") + " <= ? ORDER BY " + qi("created_at") + " ASC LIMIT ?";
		List<String> pending = new ArrayList<>();
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, PENDING);
			statement.setLong(2, cutoff);
			statement.setInt(3, RECOVERY_BATCH_SIZE);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) pending.add(result.getString(1));
			}
		}
		List<RefundedPurchase> refunded = new ArrayList<>();
		for (String purchaseId : pending) {
			RefundedPurchase result = refundPendingDetails(purchaseId, now);
			if (result != null) refunded.add(result);
		}
		// COMPENSATING is safe to refund: the local scheduler fence was persisted
		// before the first attempt, so the reward callback cannot run. Retry these
		// rows promptly after an outage rather than leaving them charged forever.
		for (String purchaseId : findTransferIds(COMPENSATING, RECOVERY_BATCH_SIZE)) {
			RefundedPurchase result = refundCompensatingRewardDetails(purchaseId);
			if (result != null) refunded.add(result);
		}
		cleanupTerminalRows(now - TERMINAL_RETENTION_MILLIS);
		return List.copyOf(refunded);
	}

	AccountingRecoveryBatch recoverAccounting(long now) throws SQLException {
		List<UUID> pendingRewards = new ArrayList<>();
		List<AccountingRecovery> pending = findPendingAccounting(RECOVERY_BATCH_SIZE);
		for (AccountingRecovery recovery : pending) {
			AccountingRow row = recovery.row();
			int missing = row.requested() & ~row.completed();
			if ((missing & DAILY_TOTAL) != 0) applyPeriodTotals(recovery.voteId(), row.uuid(), "DailyTotal",
						"LastDailyTotal", List.of("DailyTotal"), null, DAILY_TOTAL);
			if ((missing & WEEKLY_TOTAL) != 0) applyPeriodTotals(recovery.voteId(), row.uuid(), "WeeklyTotal",
						"LastWeeklyTotal", List.of("WeeklyTotal"), null, WEEKLY_TOTAL);
			if ((missing & MONTH_TOTAL) != 0) {
				List<String> columns = row.monthColumn() == null ? List.of("MonthTotal")
							: List.of("MonthTotal", row.monthColumn());
				applyPeriodTotals(recovery.voteId(), row.uuid(), "MonthTotal", "LastMonthTotal", columns,
							row.monthMaximum(), MONTH_TOTAL);
			}
			if ((missing & VOTE_PARTY_TOTAL) != 0) applyPeriodTotals(recovery.voteId(), row.uuid(),
						"VotePartyVotes", "LastVotePartyVotes", List.of("VotePartyVotes"), null, VOTE_PARTY_TOTAL);
			if ((missing & DAILY_STREAK) != 0) {
				if (row.streakValue() == null || row.streakUpdatedAt() == null) {
						throw new SQLException("Pending daily streak accounting payload is incomplete");
				}
				applyDailyStreak(recovery.voteId(), row.uuid(), row.streakValue().intValue(),
							row.streakUpdatedAt().longValue());
			}
			if ((row.requested() & DAILY_STREAK_REWARD) != 0
					&& (row.completed() & (DAILY_STREAK_REWARD | DAILY_STREAK_REWARD_CLAIMED)) == 0) {
				pendingRewards.add(recovery.voteId());
				break;
			}
		}
		cleanupAccounting(now - ACCOUNTING_RETENTION_MILLIS);
		return new AccountingRecoveryBatch(!pending.isEmpty(), List.copyOf(pendingRewards));
	}

	private List<AccountingRecovery> findPendingAccounting(int limit) throws SQLException {
		String requested = qi("requested");
		String completed = qi("completed");
		String select = "SELECT " + qi("vote_id") + ", " + accountingColumns() + " FROM " + qiAccounting()
				+ " WHERE " + requested + " <> " + completed + " AND (((" + requested + " & "
				+ RECOVERABLE_NON_REWARD_ACCOUNTING + ") <> (" + completed + " & "
				+ RECOVERABLE_NON_REWARD_ACCOUNTING + ")) OR ((" + requested + " & " + DAILY_STREAK_REWARD
				+ ") <> 0 AND (" + completed + " & " + DAILY_STREAK_REWARD + ") = 0 AND (" + completed
				+ " & " + DAILY_STREAK_REWARD_CLAIMED + ") = 0)) ORDER BY " + qi("created_at")
				+ " ASC LIMIT ?";
		List<AccountingRecovery> pending = new ArrayList<>();
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setInt(1, limit);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) pending.add(new AccountingRecovery(UUID.fromString(result.getString(1)),
						new AccountingRow(result.getString(2), result.getInt(3), result.getInt(4), result.getString(5),
								nullableInteger(result, 6), nullableInteger(result, 7), nullableLong(result, 8),
								nullableInteger(result, 9), nullableInteger(result, 10))));
			}
		}
		return pending;
	}

	private void cleanupAccounting(long cutoff) throws SQLException {
		String delete = "DELETE FROM " + qiAccounting() + " WHERE " + qi("requested") + " = "
				+ qi("completed") + " AND " + qi("created_at") + " < ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(delete)) {
			statement.setLong(1, cutoff);
			statement.executeUpdate();
		}
	}

	private List<String> findTransferIds(String state, int limit) throws SQLException {
		String select = "SELECT " + qi("purchase_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " = ? ORDER BY " + qi("created_at") + " ASC LIMIT ?";
		List<String> purchaseIds = new ArrayList<>();
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, state);
			statement.setInt(2, limit);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) purchaseIds.add(result.getString(1));
			}
		}
		return purchaseIds;
	}

	private void cleanupTerminalRows(long cutoff) throws SQLException {
		String select = "SELECT " + qi("purchase_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " IN (?, ?) AND " + qi("created_at") + " <= ? ORDER BY " + qi("created_at") + " ASC LIMIT ?";
		String delete = "DELETE FROM " + qiJournal() + " WHERE " + qi("purchase_id") + " = ? AND "
				+ qi("state") + " IN (?, ?) AND " + qi("created_at") + " <= ?";
		try (Connection connection = connection(); PreparedStatement selectStatement = connection.prepareStatement(select);
				PreparedStatement deleteStatement = connection.prepareStatement(delete)) {
			selectStatement.setString(1, COMPLETED);
			selectStatement.setString(2, REFUNDED);
			selectStatement.setLong(3, cutoff);
			selectStatement.setInt(4, CLEANUP_BATCH_SIZE);
			List<String> terminal = new ArrayList<>();
			try (ResultSet result = selectStatement.executeQuery()) {
				while (result.next()) terminal.add(result.getString(1));
			}
			for (String purchaseId : terminal) {
				deleteStatement.setString(1, purchaseId);
				deleteStatement.setString(2, COMPLETED);
				deleteStatement.setString(3, REFUNDED);
				deleteStatement.setLong(4, cutoff);
				deleteStatement.executeUpdate();
			}
		}
	}

	private void ensureSchema() throws SQLException {
		String create = "CREATE TABLE IF NOT EXISTS " + qiJournal() + " (" + qi("purchase_id")
				+ " VARCHAR(36) NOT NULL, " + qi("player_uuid") + " VARCHAR(37) NOT NULL, "
				+ qi("points_column") + " VARCHAR(128) NOT NULL, " + qi("limit_column") + " VARCHAR(128) NULL, "
				+ qi("cost") + " INT NOT NULL, " + qi("limit_value") + " INT NULL, " + qi("limit_generation")
				+ " VARCHAR(96) NULL, " + qi("limit_generation_expires_at") + " BIGINT NULL, " + qi("limit_epoch")
				+ " BIGINT NULL, " + qi("state")
				+ " VARCHAR(16) NOT NULL, " + qi("created_at") + " BIGINT NOT NULL, " + qi("hook_started_at")
				+ " BIGINT NULL, PRIMARY KEY (" + qi("purchase_id") + "));";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
			ensureColumn(connection, "limit_generation", "VARCHAR(96) NULL");
			ensureColumn(connection, "limit_generation_expires_at", "BIGINT NULL");
			ensureColumn(connection, "limit_epoch", "BIGINT NULL");
			ensureEpochSchema(connection);
			ensureAccountingSchema(connection);
			String index = "vp_vsp_" + Integer.toUnsignedString(journalTable.hashCode(), 36) + "_state_created";
			String createIndex = "CREATE INDEX " + (table.getDbType() == DbType.POSTGRESQL ? "IF NOT EXISTS " : "")
					+ qi(index) + " ON " + qiJournal() + " (" + qi("state") + ", " + qi("created_at") + ");";
			try (PreparedStatement indexStatement = connection.prepareStatement(createIndex)) {
				indexStatement.executeUpdate();
			} catch (SQLException failure) {
				if (failure.getErrorCode() != 1061 && !"42P07".equals(failure.getSQLState())) throw failure;
			}
		}
	}

	private void ensureAccountingSchema(Connection connection) throws SQLException {
		String create = "CREATE TABLE IF NOT EXISTS " + qiAccounting() + " (" + qi("vote_id")
				+ " VARCHAR(36) NOT NULL, " + qi("player_uuid") + " VARCHAR(37) NULL, "
				+ qi("requested") + " INT NOT NULL DEFAULT 0, " + qi("completed") + " INT NOT NULL, "
				+ qi("month_column") + " VARCHAR(128) NULL, " + qi("month_maximum") + " INT NULL, "
				+ qi("streak_value") + " INT NULL, " + qi("streak_updated_at") + " BIGINT NULL, "
				+ qi("streak_force_proxy") + " INT NULL, " + qi("streak_applied_value") + " INT NULL, "
				+ qi("created_at")
				+ " BIGINT NOT NULL, PRIMARY KEY (" + qi("vote_id") + "));";
		try (PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
		}
		ensureAccountingColumn(connection, "player_uuid", "VARCHAR(37) NULL");
		ensureAccountingColumn(connection, "requested", "INT NOT NULL DEFAULT 0");
		ensureAccountingColumn(connection, "month_column", "VARCHAR(128) NULL");
		ensureAccountingColumn(connection, "month_maximum", "INT NULL");
		ensureAccountingColumn(connection, "streak_value", "INT NULL");
		ensureAccountingColumn(connection, "streak_updated_at", "BIGINT NULL");
		ensureAccountingColumn(connection, "streak_force_proxy", "INT NULL");
		ensureAccountingColumn(connection, "streak_applied_value", "INT NULL");
		try (PreparedStatement migrate = connection.prepareStatement("UPDATE " + qiAccounting() + " SET "
				+ qi("requested") + " = " + qi("completed") + " WHERE " + qi("requested") + " = 0 AND "
				+ qi("completed") + " <> 0")) {
			migrate.executeUpdate();
		}
		String index = "vp_vsa_streak_" + hash(accountingTable).substring(0, 16);
		String createIndex = "CREATE INDEX " + (table.getDbType() == DbType.POSTGRESQL ? "IF NOT EXISTS " : "")
				+ qi(index) + " ON " + qiAccounting() + " (" + qi("player_uuid") + ", "
				+ qi("streak_updated_at") + ");";
		try (PreparedStatement statement = connection.prepareStatement(createIndex)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (failure.getErrorCode() != 1061 && !"42P07".equals(failure.getSQLState())) throw failure;
		}
	}

	private void ensureAccountingColumn(Connection connection, String column, String definition) throws SQLException {
		String alter = "ALTER TABLE " + qiAccounting() + " ADD COLUMN " + qi(column) + " " + definition;
		try (PreparedStatement statement = connection.prepareStatement(alter)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (failure.getErrorCode() != 1060 && !"42701".equals(failure.getSQLState())) throw failure;
		}
	}

	private void ensureEpochSchema(Connection connection) throws SQLException {
		String create = "CREATE TABLE IF NOT EXISTS " + qiEpoch() + " (" + qi("limit_column")
				+ " VARCHAR(128) NOT NULL, " + qi("epoch") + " BIGINT NOT NULL, "
				+ qi("last_reset_generation") + " VARCHAR(128) NULL, PRIMARY KEY ("
				+ qi("limit_column") + "));";
		try (PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
		}
		ensureEpochColumn(connection, "last_reset_generation", "VARCHAR(128) NULL");
	}

	private void ensureEpochColumn(Connection connection, String column, String definition) throws SQLException {
		String alter = "ALTER TABLE " + qiEpoch() + " ADD COLUMN " + qi(column) + " " + definition;
		try (PreparedStatement statement = connection.prepareStatement(alter)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (failure.getErrorCode() != 1060 && !"42701".equals(failure.getSQLState())) throw failure;
		}
	}

	private void ensureColumn(Connection connection, String column, String definition) throws SQLException {
		String alter = "ALTER TABLE " + qiJournal() + " ADD COLUMN " + qi(column) + " " + definition;
		try (PreparedStatement statement = connection.prepareStatement(alter)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (failure.getErrorCode() != 1060 && !"42701".equals(failure.getSQLState())) throw failure;
		}
	}

	private Connection connection() throws SQLException {
		Connection connection = table.getMysql().getConnectionManager().getConnection();
		if (connection == null) throw new SQLException("Unable to acquire shared MySQL connection");
		return connection;
	}

	private String qiJournal() { return table.qi(journalTable); }
	private String qiEpoch() { return table.qi(epochTable); }
	private String qiAccounting() { return table.qi(accountingTable); }
	private String qi(String identifier) { return table.qi(identifier); }
	private String uuidCast() { return table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?"; }

	private long lockLimitEpoch(Connection connection, String limitColumn) throws SQLException {
		return lockLimitEpochRow(connection, limitColumn).epoch();
	}

	private EpochRow lockLimitEpochRow(Connection connection, String limitColumn) throws SQLException {
		ensureLimitEpochRow(connection, limitColumn);
		EpochRow epoch = findAndLockLimitEpoch(connection, limitColumn);
		if (epoch == null) throw new SQLException("Vote shop limit epoch marker missing");
		return epoch;
	}

	private void ensureLimitEpochRow(Connection connection, String limitColumn) throws SQLException {
		String insert = table.getDbType() == DbType.POSTGRESQL
				? "INSERT INTO " + qiEpoch() + " (" + qi("limit_column") + ", " + qi("epoch")
						+ ") VALUES (?, 0) ON CONFLICT DO NOTHING"
				: "INSERT IGNORE INTO " + qiEpoch() + " (" + qi("limit_column") + ", " + qi("epoch")
						+ ") VALUES (?, 0)";
		try (PreparedStatement statement = connection.prepareStatement(insert)) {
			statement.setString(1, limitColumn);
			statement.executeUpdate();
		}
	}

	private EpochRow findAndLockLimitEpoch(Connection connection, String limitColumn) throws SQLException {
		String select = "SELECT " + qi("epoch") + ", " + qi("last_reset_generation") + " FROM " + qiEpoch()
				+ " WHERE " + qi("limit_column")
				+ " = ? FOR UPDATE";
		try (PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, limitColumn);
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? new EpochRow(result.getLong(1), result.getString(2)) : null;
			}
		}
	}

	private EpochRow findLimitEpoch(String limitColumn) throws SQLException {
		String select = "SELECT " + qi("epoch") + ", " + qi("last_reset_generation") + " FROM " + qiEpoch()
				+ " WHERE " + qi("limit_column")
				+ " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, limitColumn);
			try (ResultSet result = statement.executeQuery()) {
				return result.next() ? new EpochRow(result.getLong(1), result.getString(2)) : null;
			}
		}
	}

	private static Long nullableLong(ResultSet result, int index) throws SQLException {
		Object value = result.getObject(index);
		return value instanceof Number number ? number.longValue() : null;
	}

	private static Integer nullableInteger(ResultSet result, int index) throws SQLException {
		Object value = result.getObject(index);
		return value instanceof Number number ? number.intValue() : null;
	}

	private record EpochRow(long epoch, String lastResetGeneration) {
	}

	private record AccountingRow(String uuid, int requested, int completed, String monthColumn,
			Integer monthMaximum, Integer streakValue, Long streakUpdatedAt, Integer streakForceProxy,
			Integer streakAppliedValue) {
	}

	private record DailyStreakCandidate(int dailyTotal, int streak, String lastUpdate) { }

	private record AccountingRecovery(UUID voteId, AccountingRow row) {
	}

	private static boolean isSafeColumn(String column) {
		// Columns are passed through AbstractSqlTable.qi(), which escapes the
		// database-specific identifier delimiter. Preserve configured shop keys
		// such as "Daily Reward" in the durable journal so their debit can always
		// be recovered; reject only values that cannot be represented by its
		// bounded VARCHAR journal column or a SQL identifier.
		return column != null && !column.isEmpty() && column.length() <= 128 && column.indexOf('\0') < 0;
	}

	private static void rollback(Connection connection) {
		try {
			connection.rollback();
		} catch (SQLException ignored) {
			// Preserve the original failure; a PENDING record remains recoverable.
		}
	}

	private static void closeQuietly(Connection connection) {
		try {
			connection.close();
		} catch (SQLException ignored) {
			// The confirmation query above decides whether the durable commit landed.
		}
	}

	private record PurchaseRow(String state) {
	}

	record RefundedPurchase(String uuid, String pointsColumn, String limitColumn) {
	}

	enum ClaimOutcome {
		CLAIMED,
		NOT_CLAIMED,
		INDETERMINATE
	}

	private static final class IdentityWeakReference extends WeakReference<MySQL> {
		private final int identityHash;

		IdentityWeakReference(MySQL referent, ReferenceQueue<MySQL> queue) {
			super(referent, queue);
			identityHash = System.identityHashCode(referent);
		}

		@Override public int hashCode() { return identityHash; }

		@Override public boolean equals(Object other) {
			return this == other || other instanceof IdentityWeakReference reference && get() != null
					&& get() == reference.get();
		}
	}
}
