package com.bencodez.votingplugin.user;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;

/**
 * Durable state for a shared-MySQL point transfer.
 *
 * <p>The journal is deliberately stored in the same database as the user row.
 * A reservation and its source debit therefore commit together. The event hook
 * is invoked only after that short transaction has released its connection;
 * settlement is a second short, idempotent transaction.</p>
 */
final class SharedPointTransferJournal {
	private static final String RESERVED = "RESERVED";
	private static final String HOOK_STARTED = "HOOK_STARTED";
	private static final String COMPLETED = "COMPLETED";
	private static final String REFUNDED = "REFUNDED";
	static final long RESERVED_RECOVERY_AGE_MILLIS = TimeUnit.MINUTES.toMillis(5);
	static final long TERMINAL_RETENTION_MILLIS = TimeUnit.DAYS.toMillis(7);
	private static final int RECOVERY_BATCH_SIZE = 32;
	private static final int CLEANUP_BATCH_SIZE = 100;
	/* PostgreSQL permits 63 bytes and is the tighter supported database limit. */
	private static final int MAX_IDENTIFIER_BYTES = 63;
	private static final String JOURNAL_SUFFIX = "_PointTransfers";
	private static final String HASHED_TABLE_PREFIX = "vp_pt_";
	private static final int HASHED_TABLE_HEX_LENGTH = 32;

	private final MySQL table;
	private final String journalTable;
	/*
	 * MySQL does not promise identity-based equals/hashCode. A regular
	 * WeakHashMap can therefore conflate two live handles that compare equal,
	 * and a value containing the handle would keep its weak key alive. Keep only
	 * identity weak references as initialization markers instead.
	 */
	private static final ReferenceQueue<MySQL> INITIALIZED_QUEUE = new ReferenceQueue<>();
	private static final Set<IdentityWeakReference> INITIALIZED = new HashSet<>();

	SharedPointTransferJournal(MySQL table) throws SQLException {
		this(table, true);
	}

	private SharedPointTransferJournal(MySQL table, boolean initializeSchema) throws SQLException {
		this.table = table;
		this.journalTable = journalTableName(table.getTableName());
		if (initializeSchema) ensureSchema();
	}

	/**
	 * Keeps the historic auxiliary-table name where it is portable, while using
	 * a fixed, collision-resistant name for source tables which would exceed the
	 * PostgreSQL identifier limit.
	 */
	static String journalTableName(String sourceTable) {
		String legacyName = sourceTable + JOURNAL_SUFFIX;
		if (legacyName.getBytes(StandardCharsets.UTF_8).length <= MAX_IDENTIFIER_BYTES) return legacyName;
		return HASHED_TABLE_PREFIX + hash(sourceTable + '\0' + JOURNAL_SUFFIX).substring(0, HASHED_TABLE_HEX_LENGTH);
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

	/** Returns a journal handle after ensuring the schema once per live MySQL table handle. */
	static SharedPointTransferJournal forTable(MySQL table) throws SQLException {
		synchronized (INITIALIZED) {
			expungeInitialized();
			for (IdentityWeakReference marker : INITIALIZED) {
				if (marker.get() == table) return new SharedPointTransferJournal(table, false);
			}
			new SharedPointTransferJournal(table, true);
			INITIALIZED.add(new IdentityWeakReference(table, INITIALIZED_QUEUE));
			return new SharedPointTransferJournal(table, false);
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

	private static final class IdentityWeakReference extends WeakReference<MySQL> {
		private final int identityHash;

		IdentityWeakReference(MySQL referent, ReferenceQueue<MySQL> queue) {
			super(referent, queue);
			identityHash = System.identityHashCode(referent);
		}

		@Override
		public int hashCode() {
			return identityHash;
		}

		@Override
		public boolean equals(Object other) {
			return this == other || other instanceof IdentityWeakReference reference && get() != null
					&& get() == reference.get();
		}
	}

	/**
	 * Inserts a reservation and conditionally debits the source in one transaction.
	 * A duplicate transfer id is treated as an idempotent retry of the same
	 * reservation, which is needed after a commit acknowledgement is lost.
	 */
	boolean reserve(String transferId, String sourceUuid, String sourcePointsColumn, int debitPoints, String targetUuid,
			int requestedCreditPoints, long now) throws SQLException {
		TransferRow existing = find(transferId);
		if (existing != null) {
			return existing.matches(sourceUuid, targetUuid, debitPoints, requestedCreditPoints)
					&& !REFUNDED.equals(existing.state);
		}

		String insert = "INSERT INTO " + qiJournal() + " (" + qi("transfer_id") + ", " + qi("source_uuid")
				+ ", " + qi("source_points_column") + ", " + qi("target_uuid") + ", " + qi("debit_points")
				+ ", " + qi("requested_credit_points") + ", " + qi("state") + ", " + qi("created_at")
				+ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
		String points = qi(sourcePointsColumn);
		String debit = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " - ? WHERE " + qi("uuid") + uuidCast() + " AND " + points + " >= ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement insertStatement = connection.prepareStatement(insert);
					PreparedStatement debitStatement = connection.prepareStatement(debit)) {
					insertStatement.setString(1, transferId);
					insertStatement.setString(2, sourceUuid);
					insertStatement.setString(3, sourcePointsColumn);
					insertStatement.setString(4, targetUuid);
					insertStatement.setInt(5, debitPoints);
					insertStatement.setInt(6, requestedCreditPoints);
					insertStatement.setString(7, RESERVED);
					insertStatement.setLong(8, now);
				insertStatement.executeUpdate();

				debitStatement.setInt(1, debitPoints);
				debitStatement.setString(2, sourceUuid);
				debitStatement.setInt(3, debitPoints);
				if (debitStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
				return commitAndConfirm(connection, transferId, RESERVED);
			} catch (SQLException failure) {
				rollback(connection);
				if (isDuplicate(failure)) {
					closeQuietly(connection);
					TransferRow duplicate = find(transferId);
					return duplicate != null && duplicate.matches(sourceUuid, targetUuid, debitPoints,
							requestedCreditPoints) && !REFUNDED.equals(duplicate.state);
				}
				throw failure;
			}
		}
	}

	/**
	 * Claims a reservation before invoking the arbitrary external event hook.
	 * A lost commit acknowledgement is retried by transfer id: a row already
	 * owned by this attempt proves that the hook may now run exactly once.
	 */
	ClaimOutcome claimHookWithConfirmation(String transferId, String owner, long startedAt) {
		for (int attempt = 0; attempt < 3; attempt++) {
			try {
				ClaimOutcome outcome = claimHookOnce(transferId, owner, startedAt);
				if (outcome != ClaimOutcome.INDETERMINATE) return outcome;
			} catch (SQLException ignored) {
				// Re-read the same journal row; do not create a second transfer id.
			}
		}
		return ClaimOutcome.INDETERMINATE;
	}

	private ClaimOutcome claimHookOnce(String transferId, String owner, long startedAt) throws SQLException {
		String select = "SELECT " + qi("state") + ", " + qi("hook_owner") + " FROM " + qiJournal()
				+ " WHERE " + qi("transfer_id") + " = ? FOR UPDATE";
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ?, " + qi("hook_owner")
				+ " = ?, " + qi("hook_started_at") + " = ? WHERE " + qi("transfer_id") + " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, transferId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next()) {
						connection.rollback();
						return ClaimOutcome.NOT_CLAIMED;
					}
					String state = result.getString(1);
					String currentOwner = result.getString(2);
					if (HOOK_STARTED.equals(state)) {
						connection.rollback();
						return owner.equals(currentOwner) ? ClaimOutcome.CLAIMED : ClaimOutcome.INDETERMINATE;
					}
					if (!RESERVED.equals(state)) {
						connection.rollback();
						return ClaimOutcome.NOT_CLAIMED;
					}
				}
			}
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, HOOK_STARTED);
				updateStatement.setString(2, owner);
				updateStatement.setLong(3, startedAt);
				updateStatement.setString(4, transferId);
				if (updateStatement.executeUpdate() != 1) {
					connection.rollback();
					return ClaimOutcome.INDETERMINATE;
				}
				commitAndConfirm(connection, transferId, HOOK_STARTED);
				return ClaimOutcome.CLAIMED;
			}
		}
	}

	enum ClaimOutcome {
		CLAIMED,
		NOT_CLAIMED,
		INDETERMINATE
	}

	/**
	 * Safely releases a reservation when the hook was never claimed. A
	 * {@code HOOK_STARTED} row is deliberately left alone because a listener may
	 * already be executing and replaying/refunding it automatically is unsafe.
	 */
	boolean refundReserved(String transferId, String sourceUuid, String sourcePointsColumn, int debitPoints)
			throws SQLException {
		if (!isSafeColumn(sourcePointsColumn)) return false;
		String select = "SELECT " + qi("state") + " FROM " + qiJournal() + " WHERE " + qi("transfer_id")
				+ " = ? FOR UPDATE";
		String points = qi(sourcePointsColumn);
		String refund = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " + ? WHERE " + qi("uuid") + uuidCast();
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("transfer_id")
				+ " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, transferId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next() || !RESERVED.equals(result.getString(1))) {
						connection.rollback();
						return false;
					}
				}
			}
			try (PreparedStatement refundStatement = connection.prepareStatement(refund)) {
				refundStatement.setInt(1, debitPoints);
				refundStatement.setString(2, sourceUuid);
				if (refundStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, REFUNDED);
				updateStatement.setString(2, transferId);
				if (updateStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			return commitAndConfirm(connection, transferId, REFUNDED);
		}
	}

	/**
	 * Refunds a claimed transfer when the second Bukkit approval task was rejected
	 * before its callback could start. The caller has the scheduler's proof that
	 * no listener ran, so it is safe to reverse the source debit.
	 */
	boolean refundHookStarted(String transferId, String sourceUuid, String sourcePointsColumn, int debitPoints)
			throws SQLException {
		if (!isSafeColumn(sourcePointsColumn)) return false;
		String select = "SELECT " + qi("state") + " FROM " + qiJournal() + " WHERE " + qi("transfer_id")
				+ " = ? FOR UPDATE";
		String points = qi(sourcePointsColumn);
		String refund = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " + ? WHERE " + qi("uuid") + uuidCast();
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("transfer_id")
				+ " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, transferId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next()) {
						connection.rollback();
						return false;
					}
					String state = result.getString(1);
					if (REFUNDED.equals(state)) {
						connection.rollback();
						return true;
					}
					if (!HOOK_STARTED.equals(state)) {
						connection.rollback();
						return false;
					}
				}
			}
			try (PreparedStatement refundStatement = connection.prepareStatement(refund)) {
				refundStatement.setInt(1, debitPoints);
				refundStatement.setString(2, sourceUuid);
				if (refundStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, REFUNDED);
				updateStatement.setString(2, transferId);
				if (updateStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			return commitAndConfirm(connection, transferId, REFUNDED);
		} catch (SQLException failure) {
			throw failure;
		}
	}

	/**
	 * Retries settlement with the same transfer id when a commit acknowledgement
	 * or its first confirmation read is lost. A terminal state proves the prior
	 * attempt's outcome; a still-HOOK_STARTED row safely retries the same point
	 * update. Exhausted attempts remain explicitly indeterminate rather than
	 * being exposed as a retryable transfer failure.
	 */
	SettlementOutcome settleWithConfirmation(String transferId, String owner, String sourceUuid,
			String sourcePointsColumn, String targetUuid, String targetPointsColumn, int debitPoints,
			Integer adjustedCreditPoints) {
		for (int attempt = 0; attempt < 3; attempt++) {
			try {
				SettlementOutcome outcome = settleOnce(transferId, owner, sourceUuid, sourcePointsColumn, targetUuid,
						targetPointsColumn, debitPoints, adjustedCreditPoints);
				if (outcome != SettlementOutcome.INDETERMINATE) return outcome;
			} catch (SQLException ignored) {
				// The next attempt re-reads the durable state using the same id.
			}
		}
		return SettlementOutcome.INDETERMINATE;
	}

	/** Retained for direct callers that only need the historic completed/not-completed boolean. */
	boolean settle(String transferId, String owner, String sourceUuid, String sourcePointsColumn, String targetUuid,
			String targetPointsColumn, int debitPoints, Integer adjustedCreditPoints) {
		return settleWithConfirmation(transferId, owner, sourceUuid, sourcePointsColumn, targetUuid,
				targetPointsColumn, debitPoints, adjustedCreditPoints) == SettlementOutcome.COMPLETED;
	}

	private SettlementOutcome settleOnce(String transferId, String owner, String sourceUuid, String sourcePointsColumn,
			String targetUuid, String targetPointsColumn, int debitPoints, Integer adjustedCreditPoints) throws SQLException {
		String select = "SELECT " + qi("state") + ", " + qi("hook_owner") + " FROM " + qiJournal()
				+ " WHERE " + qi("transfer_id") + " = ? FOR UPDATE";
		String points = qi(adjustedCreditPoints == null ? sourcePointsColumn : targetPointsColumn);
		String credit = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " + ? WHERE " + qi("uuid") + uuidCast();
		String updateJournal = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ?, "
				+ qi("adjusted_credit_points") + " = ? WHERE " + qi("transfer_id") + " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			String state;
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, transferId);
				try (ResultSet result = selectStatement.executeQuery()) {
						if (!result.next()) {
							connection.rollback();
							return SettlementOutcome.INDETERMINATE;
					}
					state = result.getString(1);
					String currentOwner = result.getString(2);
						if (COMPLETED.equals(state)) {
							connection.rollback();
							return SettlementOutcome.COMPLETED;
					}
						if (REFUNDED.equals(state)) {
							connection.rollback();
							return SettlementOutcome.REFUNDED;
					}
						if (!HOOK_STARTED.equals(state) || !owner.equals(currentOwner)) {
							connection.rollback();
							return SettlementOutcome.INDETERMINATE;
					}
				}
			}

			boolean refund = adjustedCreditPoints == null;
			try (PreparedStatement pointStatement = connection.prepareStatement(credit)) {
				pointStatement.setInt(1, refund ? debitPoints : adjustedCreditPoints);
				pointStatement.setString(2, refund ? sourceUuid : targetUuid);
				if (pointStatement.executeUpdate() != 1) {
					connection.rollback();
					return SettlementOutcome.INDETERMINATE;
				}
			}
			try (PreparedStatement journalStatement = connection.prepareStatement(updateJournal)) {
				journalStatement.setString(1, refund ? REFUNDED : COMPLETED);
				if (refund) journalStatement.setNull(2, java.sql.Types.INTEGER);
				else journalStatement.setInt(2, adjustedCreditPoints);
				journalStatement.setString(3, transferId);
				if (journalStatement.executeUpdate() != 1) {
					connection.rollback();
					return SettlementOutcome.INDETERMINATE;
				}
			}
			commitAndConfirm(connection, transferId, refund ? REFUNDED : COMPLETED);
			return refund ? SettlementOutcome.REFUNDED : SettlementOutcome.COMPLETED;
		}
	}

	enum SettlementOutcome {
		COMPLETED,
		REFUNDED,
		INDETERMINATE
	}

	/**
	 * Reclaims only old reservations that have never entered an external hook,
	 * then removes a small batch of old terminal rows. Each candidate is locked
	 * and checked again before a refund, so another server cannot compensate a
	 * transfer that it has just claimed. HOOK_STARTED rows require explicit
	 * reconciliation because an arbitrary listener may still have side effects.
	 */
	void recoverAndCleanup(long now) throws SQLException {
		long reservationCutoff = now - RESERVED_RECOVERY_AGE_MILLIS;
		for (String transferId : findExpiredTransferIds(RESERVED, "created_at", reservationCutoff, RECOVERY_BATCH_SIZE)) {
			recoverExpiredReservation(transferId, reservationCutoff);
		}
		cleanupTerminalRows(now - TERMINAL_RETENTION_MILLIS, CLEANUP_BATCH_SIZE);
	}

	private List<String> findExpiredTransferIds(String state, String timeColumn, long cutoff, int limit)
			throws SQLException {
		String sql = "SELECT " + qi("transfer_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " = ? AND " + qi(timeColumn) + " <= ? ORDER BY " + qi(timeColumn) + " ASC LIMIT ?";
		List<String> transferIds = new ArrayList<>();
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setString(1, state);
			statement.setLong(2, cutoff);
			statement.setInt(3, limit);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					transferIds.add(result.getString(1));
				}
			}
		}
		return transferIds;
	}

	private boolean recoverExpiredReservation(String transferId, long reservationCutoff) throws SQLException {
		String select = "SELECT " + qi("state") + ", " + qi("created_at") + ", " + qi("source_uuid")
				+ ", " + qi("source_points_column") + ", " + qi("debit_points") + " FROM " + qiJournal()
				+ " WHERE " + qi("transfer_id") + " = ? FOR UPDATE";
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("transfer_id")
				+ " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			String sourceUuid;
			String sourcePointsColumn;
			int debitPoints;
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, transferId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next() || !RESERVED.equals(result.getString(1)) || result.getLong(2) > reservationCutoff) {
						connection.rollback();
						return false;
					}
					sourceUuid = result.getString(3);
					sourcePointsColumn = result.getString(4);
					debitPoints = result.getInt(5);
				}
			}
			if (!isSafeColumn(sourcePointsColumn)) {
				connection.rollback();
				return false;
			}
			String points = qi(sourcePointsColumn);
			String refund = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
					+ " + ? WHERE " + qi("uuid") + uuidCast();
			try (PreparedStatement refundStatement = connection.prepareStatement(refund)) {
				refundStatement.setInt(1, debitPoints);
				refundStatement.setString(2, sourceUuid);
				if (refundStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, REFUNDED);
				updateStatement.setString(2, transferId);
				if (updateStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
			}
			return commitAndConfirm(connection, transferId, REFUNDED);
		}
	}

	private void cleanupTerminalRows(long cutoff, int limit) throws SQLException {
		String select = "SELECT " + qi("transfer_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " IN (?, ?) AND " + qi("created_at") + " <= ? ORDER BY " + qi("created_at") + " ASC LIMIT ?";
		String delete = "DELETE FROM " + qiJournal() + " WHERE " + qi("transfer_id") + " = ? AND "
				+ qi("state") + " IN (?, ?) AND " + qi("created_at") + " <= ?";
		try (Connection connection = connection(); PreparedStatement selectStatement = connection.prepareStatement(select);
				PreparedStatement deleteStatement = connection.prepareStatement(delete)) {
			selectStatement.setString(1, COMPLETED);
			selectStatement.setString(2, REFUNDED);
			selectStatement.setLong(3, cutoff);
			selectStatement.setInt(4, limit);
			List<String> transferIds = new ArrayList<>();
			try (ResultSet result = selectStatement.executeQuery()) {
				while (result.next()) {
					transferIds.add(result.getString(1));
				}
			}
			for (String transferId : transferIds) {
				deleteStatement.setString(1, transferId);
				deleteStatement.setString(2, COMPLETED);
				deleteStatement.setString(3, REFUNDED);
				deleteStatement.setLong(4, cutoff);
				deleteStatement.executeUpdate();
			}
		}
	}

	private static boolean isSafeColumn(String column) {
		return column != null && column.matches("[A-Za-z][A-Za-z0-9_]{0,127}");
	}

	private TransferRow find(String transferId) throws SQLException {
		String sql = "SELECT " + qi("source_uuid") + ", " + qi("target_uuid") + ", " + qi("debit_points")
				+ ", " + qi("requested_credit_points") + ", " + qi("state") + " FROM " + qiJournal()
				+ " WHERE " + qi("transfer_id") + " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setString(1, transferId);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) return null;
				return new TransferRow(result.getString(1), result.getString(2), result.getInt(3), result.getInt(4),
						result.getString(5));
			}
		}
	}

	private boolean commitAndConfirm(Connection connection, String transferId, String expectedState) throws SQLException {
		try {
			connection.commit();
			return true;
		} catch (SQLException ambiguousCommit) {
			// A single-connection pool cannot service the confirmation lookup while
			// this possibly-broken connection is still checked out.
			closeQuietly(connection);
			TransferRow row = find(transferId);
			if (row != null && expectedState.equals(row.state)) return true;
			throw ambiguousCommit;
		}
	}

	private void ensureSchema() throws SQLException {
		String create = "CREATE TABLE IF NOT EXISTS " + qiJournal() + " (" + qi("transfer_id")
				+ " VARCHAR(36) NOT NULL, " + qi("source_uuid") + " VARCHAR(37) NOT NULL, "
				+ qi("source_points_column") + " VARCHAR(128) NOT NULL, " + qi("target_uuid")
				+ " VARCHAR(37) NOT NULL, " + qi("debit_points") + " INT NOT NULL, "
				+ qi("requested_credit_points") + " INT NOT NULL, " + qi("adjusted_credit_points")
				+ " INT NULL, " + qi("state") + " VARCHAR(16) NOT NULL, " + qi("created_at")
				+ " BIGINT NOT NULL, " + qi("hook_started_at") + " BIGINT NULL, " + qi("hook_owner")
				+ " VARCHAR(64) NULL, PRIMARY KEY (" + qi("transfer_id")
				+ "));";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
			createIndex(connection, indexName("state_created"), new String[] { "state", "created_at" });
		}
	}

	private void createIndex(Connection connection, String indexName, String[] columns) throws SQLException {
		StringBuilder columnSql = new StringBuilder("(");
		for (int index = 0; index < columns.length; index++) {
			if (index > 0) columnSql.append(", ");
			columnSql.append(qi(columns[index]));
		}
		columnSql.append(")");
		String sql = "CREATE INDEX " + (dbType() == DbType.POSTGRESQL ? "IF NOT EXISTS " : "") + qi(indexName)
				+ " ON " + qiJournal() + " " + columnSql + ";";
		try (PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (!isDuplicateIndex(failure)) throw failure;
		}
	}

	private String indexName(String suffix) {
		return "vp_pt_" + Integer.toUnsignedString(journalTable.hashCode(), 36) + "_" + suffix;
	}

	private Connection connection() throws SQLException {
		return table.getMysql().getConnectionManager().getConnection();
	}

	private String qiJournal() {
		return table.qi(journalTable);
	}

	private String qi(String identifier) {
		return table.qi(identifier);
	}

	private String uuidCast() {
		return dbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?";
	}

	private DbType dbType() {
		return table.getDbType();
	}

	private static void rollback(Connection connection) {
		try {
			connection.rollback();
		} catch (SQLException ignored) {
			// Preserve the original failure. The journal row remains recoverable.
		}
	}

	private static void closeQuietly(Connection connection) {
		try {
			connection.close();
		} catch (SQLException ignored) {
			// The confirmation lookup below will determine whether the commit landed.
		}
	}

	private static boolean isDuplicate(SQLException failure) {
		String state = failure.getSQLState();
		return "23505".equals(state) || failure.getErrorCode() == 1062;
	}

	private static boolean isDuplicateIndex(SQLException failure) {
		return failure.getErrorCode() == 1061 || "42P07".equals(failure.getSQLState());
	}

	private record TransferRow(String sourceUuid, String targetUuid, int debitPoints, int requestedCreditPoints,
			String state) {
		boolean matches(String source, String target, int debit, int requestedCredit) {
			return sourceUuid.equals(source) && targetUuid.equals(target) && debitPoints == debit
					&& requestedCreditPoints == requestedCredit;
		}
	}
}
