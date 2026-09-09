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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
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
	private static final String PENDING = "PENDING";
	private static final String HOOK_STARTED = "HOOK_STARTED";
	private static final String COMPENSATING = "COMPENSATING";
	private static final String COMPLETED = "COMPLETED";
	private static final String REFUNDED = "REFUNDED";
	static final String NO_LIMIT_RESET_GENERATION = "NONE";
	static final long PENDING_RECOVERY_AGE_MILLIS = TimeUnit.MINUTES.toMillis(5);
	static final long TERMINAL_RETENTION_MILLIS = TimeUnit.DAYS.toMillis(7);
	private static final int RECOVERY_BATCH_SIZE = 32;
	private static final int CLEANUP_BATCH_SIZE = 100;
	/* PostgreSQL permits 63 bytes and is the tighter supported database limit. */
	private static final int MAX_IDENTIFIER_BYTES = 63;
	private static final String JOURNAL_SUFFIX = "_VoteShopPurchases";
	private static final String EPOCH_SUFFIX = "_VoteShopLimitEpochs";
	private static final String HASHED_TABLE_PREFIX = "vp_vsp_";
	private static final String HASHED_EPOCH_TABLE_PREFIX = "vp_vse_";
	private static final int HASHED_TABLE_HEX_LENGTH = 32;

	private static final ReferenceQueue<MySQL> INITIALIZED_QUEUE = new ReferenceQueue<>();
	private static final Set<IdentityWeakReference> INITIALIZED = new HashSet<>();

	private final MySQL table;
	private final String journalTable;
	private final String epochTable;

	SharedMysqlPurchaseJournal(MySQL table, boolean initializeSchema) throws SQLException {
		this.table = table;
		journalTable = journalTableName(table.getTableName());
		epochTable = epochTableName(table.getTableName());
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
				.append(points).append(" = ").append(points).append(" - ?");
		if (limitColumn != null) {
			debit.append(", ").append(qi(limitColumn)).append(" = COALESCE(").append(qi(limitColumn))
					.append(", 0) + 1");
		}
		debit.append(" WHERE ").append(qi("uuid")).append(uuidCast()).append(" AND ").append(points)
				.append(" >= ?");
		if (limitColumn != null) {
			debit.append(" AND COALESCE(").append(qi(limitColumn)).append(", 0) < ?");
		}
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try {
				Long limitEpoch = tracksResetEpoch(limitColumn, limitGeneration)
						? lockLimitEpoch(connection, limitColumn) : null;
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
				try (PreparedStatement wipe = connection.prepareStatement("UPDATE " + qi(table.getTableName()) + " SET "
						+ qi(limitColumn) + " = 0");
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
		return table.getMysql().getConnectionManager().getConnection();
	}

	private String qiJournal() { return table.qi(journalTable); }
	private String qiEpoch() { return table.qi(epochTable); }
	private String qi(String identifier) { return table.qi(identifier); }
	private String uuidCast() { return table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?"; }

	private static boolean tracksResetEpoch(String limitColumn, String generation) {
		return limitColumn != null && generation != null && !NO_LIMIT_RESET_GENERATION.equals(generation);
	}

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

	private record EpochRow(long epoch, String lastResetGeneration) {
	}

	private static boolean isSafeColumn(String column) {
		return column != null && column.matches("[A-Za-z][A-Za-z0-9_-]{0,127}");
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
