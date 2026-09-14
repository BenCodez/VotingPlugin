package com.bencodez.votingplugin.user;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;

/**
 * Durable, idempotent shared-MySQL point additions for retryable reward stages.
 *
 * <p>The journal update and the point credit commit in the same transaction. A
 * retry with the same operation id can consequently prove the earlier credit
 * instead of applying the delta a second time after a lost commit acknowledgement.</p>
 */
final class SharedPointAdditionJournal {
	private static final String COMPLETED = "COMPLETED";
	private static final String ACKNOWLEDGED = "ACKNOWLEDGED";
	/* A claimed hook is never replayed automatically: arbitrary listeners may
	 * already have produced side effects before a backend stops. */
	private static final String HOOK_STARTED = "HOOK_STARTED";
	/* The receive hook may have run, but no durable credit/cancellation outcome
	 * was confirmed. This state is deliberately never replayed automatically. */
	private static final String INDETERMINATE = "INDETERMINATE";
	/* Matches the bounded durable reconciliation window used by shared transfers. */
	static final long COMPLETED_RETENTION_MILLIS = TimeUnit.DAYS.toMillis(7);
	/* A live receive hook normally settles immediately. Do not let a replacement
	 * backend preempt it; only a bounded, expired foreign claim is recoverable. */
	static final long HOOK_RECOVERY_LEASE_MILLIS = TimeUnit.MINUTES.toMillis(5);
	private static final int CLEANUP_BATCH_SIZE = 100;
	private static final int MAX_IDENTIFIER_BYTES = 63;
	private static final String JOURNAL_SUFFIX = "_PointAdditions";
	private static final String HASHED_TABLE_PREFIX = "vp_pa_";
	private static final int HASHED_TABLE_HEX_LENGTH = 32;

	private static final ReferenceQueue<MySQL> INITIALIZED_QUEUE = new ReferenceQueue<>();
	private static final Set<IdentityWeakReference> INITIALIZED = new HashSet<>();

	private final MySQL table;
	private final String journalTable;

	SharedPointAdditionJournal(MySQL table, boolean initializeSchema) throws SQLException {
		this.table = table;
		journalTable = journalTableName(table.getTableName());
		if (initializeSchema) ensureSchema();
	}

	static String journalTableName(String sourceTable) {
		String legacyName = sourceTable + JOURNAL_SUFFIX;
		if (legacyName.getBytes(StandardCharsets.UTF_8).length <= MAX_IDENTIFIER_BYTES) return legacyName;
		return HASHED_TABLE_PREFIX + hash(sourceTable + '\0' + JOURNAL_SUFFIX).substring(0, HASHED_TABLE_HEX_LENGTH);
	}

	static SharedPointAdditionJournal forTable(MySQL table) throws SQLException {
		synchronized (INITIALIZED) {
			expungeInitialized();
			for (IdentityWeakReference marker : INITIALIZED) {
				if (marker.get() == table) return new SharedPointAdditionJournal(table, false);
			}
			new SharedPointAdditionJournal(table, true);
			INITIALIZED.add(new IdentityWeakReference(table, INITIALIZED_QUEUE));
			return new SharedPointAdditionJournal(table, false);
		}
	}

	/** Applies the operation exactly once and returns the resulting durable total. */
	AdditionResult add(String operationId, String uuid, String pointsColumn, int amount, long now) throws SQLException {
		return mutate(operationId, uuid, pointsColumn, amount, now, false);
	}

	/** Applies a durable conditional debit. A retry confirms the journal row and
	 * never debits the player twice; a row with insufficient points is a definite
	 * rejection and is not recorded as a successful mutation. */
	AdditionResult subtract(String operationId, String uuid, String pointsColumn, int amount, long now) throws SQLException {
		if (amount < 0) throw new SQLException("Invalid shared point debit");
		return mutate(operationId, uuid, pointsColumn, -amount, now, true);
	}

	private AdditionResult mutate(String operationId, String uuid, String pointsColumn, int amount, long now,
			boolean requireNonnegative) throws SQLException {
		if (!isSafeColumn(pointsColumn)) throw new SQLException("Unsafe shared point column");
		AdditionRow existing = find(operationId);
		if (existing != null) return existingResult(operationId, existing, uuid, pointsColumn, amount);

		String insert = "INSERT INTO " + qiJournal() + " (" + qi("operation_id") + ", " + qi("player_uuid")
				+ ", " + qi("points_column") + ", " + qi("amount") + ", " + qi("requested_amount") + ", "
				+ qi("state") + ", " + qi("total_points") + ", " + qi("created_at") + ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
		String points = qi(pointsColumn);
		String update = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " + ? WHERE " + qi("uuid") + uuidCast()
				+ (requireNonnegative ? " AND " + points + " >= ?" : "");
		String read = "SELECT " + points + " FROM " + qi(table.getTableName()) + " WHERE " + qi("uuid") + uuidCast();
		String complete = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ?, " + qi("total_points")
				+ " = ? WHERE " + qi("operation_id") + " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement insertStatement = connection.prepareStatement(insert);
					PreparedStatement updateStatement = connection.prepareStatement(update);
					PreparedStatement readStatement = connection.prepareStatement(read);
					PreparedStatement completeStatement = connection.prepareStatement(complete)) {
				insertStatement.setString(1, operationId);
				insertStatement.setString(2, uuid);
				insertStatement.setString(3, pointsColumn);
				insertStatement.setInt(4, amount);
				insertStatement.setInt(5, amount);
				insertStatement.setString(6, COMPLETED);
				insertStatement.setNull(7, java.sql.Types.INTEGER);
				insertStatement.setLong(8, now);
				insertStatement.executeUpdate();

				updateStatement.setInt(1, amount);
				updateStatement.setString(2, uuid);
				if (requireNonnegative) updateStatement.setInt(3, -amount);
				if (updateStatement.executeUpdate() != 1) {
					rollback(connection);
					if (requireNonnegative) throw new DebitRejectedException();
					throw new SQLException("Shared point user row missing");
				}
				readStatement.setString(1, uuid);
				int total;
				try (ResultSet result = readStatement.executeQuery()) {
					if (!result.next()) {
						rollback(connection);
						throw new SQLException("Shared point user row disappeared");
					}
					total = result.getInt(1);
				}
				completeStatement.setString(1, COMPLETED);
				completeStatement.setInt(2, total);
				completeStatement.setString(3, operationId);
				if (completeStatement.executeUpdate() != 1) {
					rollback(connection);
					throw new SQLException("Shared point addition journal row missing");
				}
				commitAndConfirm(connection, operationId);
				return new AdditionResult(total);
			} catch (SQLException failure) {
				rollback(connection);
				if (isDuplicate(failure)) {
					closeQuietly(connection);
					AdditionRow duplicate = find(operationId);
					if (duplicate != null) return existingResult(operationId, duplicate, uuid, pointsColumn, amount);
				}
				throw failure;
			}
		}
	}

	static final class DebitRejectedException extends SQLException {
		private static final long serialVersionUID = 1L;

		DebitRejectedException() {
			super("Shared point debit rejected: insufficient points or missing user");
		}
	}

	/**
	 * Claims an idempotent reward operation before its arbitrary Bukkit receive
	 * hook runs. The claim is shared by all backend JVMs, unlike the caller's
	 * process-local coalescing map.
	 */
	HookClaim claimHook(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner,
			long now) throws SQLException {
		if (!isSafeColumn(pointsColumn)) throw new SQLException("Unsafe shared point column");
		AdditionRow existing = find(operationId);
		if (existing != null) {
			if (isExpiredForeignHook(existing, owner, now)
					&& markExpiredHookIndeterminate(operationId, existing, uuid, pointsColumn, requestedAmount, now)) {
				return HookClaim.reconciliationRequired();
			}
			return claimForExisting(existing, uuid, pointsColumn, requestedAmount, owner);
		}

		String insert = "INSERT INTO " + qiJournal() + " (" + qi("operation_id") + ", " + qi("player_uuid")
				+ ", " + qi("points_column") + ", " + qi("amount") + ", " + qi("requested_amount") + ", "
				+ qi("state") + ", " + qi("total_points") + ", " + qi("hook_owner") + ", "
				+ qi("created_at") + ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(insert)) {
			connection.setAutoCommit(false);
			statement.setString(1, operationId);
			statement.setString(2, uuid);
			statement.setString(3, pointsColumn);
			statement.setInt(4, requestedAmount);
			statement.setInt(5, requestedAmount);
			statement.setString(6, HOOK_STARTED);
			statement.setNull(7, java.sql.Types.INTEGER);
			statement.setString(8, owner);
			statement.setLong(9, now);
			statement.executeUpdate();
			try {
				connection.commit();
				return HookClaim.claimedByCaller();
			} catch (SQLException ambiguousCommit) {
				closeQuietly(connection);
				AdditionRow confirmed = find(operationId);
				if (confirmed != null) return claimForExisting(confirmed, uuid, pointsColumn, requestedAmount, owner);
				throw ambiguousCommit;
			}
		} catch (SQLException failure) {
			if (!isDuplicate(failure)) throw failure;
			AdditionRow duplicate = find(operationId);
			if (duplicate != null) return claimForExisting(duplicate, uuid, pointsColumn, requestedAmount, owner);
			throw failure;
		}
	}

	/** Completes a claimed hook exactly once, including a durable cancellation outcome. */
	AdditionResult settleClaim(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner,
			Integer adjustedAmount) throws SQLException {
		if (!isSafeColumn(pointsColumn)) throw new SQLException("Unsafe shared point column");
		String select = "SELECT " + qi("player_uuid") + ", " + qi("points_column") + ", " + qi("amount")
				+ ", " + qi("state") + ", " + qi("total_points") + ", " + qi("requested_amount") + ", "
				+ qi("hook_owner") + ", " + qi("created_at") + " FROM " + qiJournal() + " WHERE "
				+ qi("operation_id") + " = ? FOR UPDATE";
		String points = qi(pointsColumn);
		String updatePoints = "UPDATE " + qi(table.getTableName()) + " SET " + points + " = " + points
				+ " + ? WHERE " + qi("uuid") + uuidCast();
		String readPoints = "SELECT " + points + " FROM " + qi(table.getTableName()) + " WHERE " + qi("uuid")
				+ uuidCast();
		String complete = "UPDATE " + qiJournal() + " SET " + qi("amount") + " = ?, " + qi("state")
				+ " = ?, " + qi("total_points") + " = ? WHERE " + qi("operation_id") + " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, operationId);
				AdditionRow row;
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next()) {
						rollback(connection);
						throw new SQLException("Shared point addition claim disappeared");
					}
					Integer total = result.getObject(5) == null ? null : Integer.valueOf(result.getInt(5));
					Integer requested = result.getObject(6) == null ? null : Integer.valueOf(result.getInt(6));
					row = new AdditionRow(result.getString(1), result.getString(2), result.getInt(3), result.getString(4),
							total, requested, result.getString(7), result.getLong(8));
				}
				HookClaim resolved = claimForExisting(row, uuid, pointsColumn, requestedAmount, owner);
				if (resolved.completed()) {
					rollback(connection);
					return new AdditionResult(resolved.total());
				}
				if (!resolved.claimed()) {
					rollback(connection);
					throw new SQLException("Shared point addition claim is not owned by this operation");
				}
			}
			if (adjustedAmount != null) {
				try (PreparedStatement statement = connection.prepareStatement(updatePoints)) {
					statement.setInt(1, adjustedAmount.intValue());
					statement.setString(2, uuid);
					if (statement.executeUpdate() != 1) {
						rollback(connection);
						throw new SQLException("Shared point user row missing");
					}
				}
			}
			int total;
			try (PreparedStatement statement = connection.prepareStatement(readPoints)) {
				statement.setString(1, uuid);
				try (ResultSet result = statement.executeQuery()) {
					if (!result.next()) {
						rollback(connection);
						throw new SQLException("Shared point user row disappeared");
					}
					total = result.getInt(1);
				}
			}
			try (PreparedStatement statement = connection.prepareStatement(complete)) {
				// amount is a durable, non-null actual credit. A cancelled hook has
				// no credit, rather than a nullable/ambiguous amount.
				statement.setInt(1, adjustedAmount == null ? 0 : adjustedAmount.intValue());
				statement.setString(2, COMPLETED);
				statement.setInt(3, total);
				statement.setString(4, operationId);
				if (statement.executeUpdate() != 1) {
					rollback(connection);
					throw new SQLException("Shared point addition journal row missing");
				}
			}
			try {
				connection.commit();
				return new AdditionResult(total);
			} catch (SQLException ambiguousCommit) {
				closeQuietly(connection);
				AdditionRow confirmed = find(operationId);
				HookClaim resolved = confirmed == null ? null
						: claimForExisting(confirmed, uuid, pointsColumn, requestedAmount, owner);
				if (resolved != null && resolved.completed()) return new AdditionResult(resolved.total());
				throw ambiguousCommit;
			}
		}
	}

	private HookClaim claimForExisting(AdditionRow row, String uuid, String pointsColumn, int requestedAmount,
			String owner) throws SQLException {
		if (!row.matchesTarget(uuid, pointsColumn)
				|| row.requestedAmount != null && row.requestedAmount.intValue() != requestedAmount) {
			throw new SQLException("Mismatched shared point addition operation");
		}
		if ((COMPLETED.equals(row.state) || ACKNOWLEDGED.equals(row.state)) && row.total != null) {
			return HookClaim.completed(row.total.intValue());
		}
		if (HOOK_STARTED.equals(row.state) && owner.equals(row.hookOwner)) return HookClaim.claimedByCaller();
		if (HOOK_STARTED.equals(row.state) || INDETERMINATE.equals(row.state)) {
			return HookClaim.reconciliationRequired();
		}
		return HookClaim.inProgress();
	}

	private boolean isExpiredForeignHook(AdditionRow row, String owner, long now) {
		return HOOK_STARTED.equals(row.state) && !owner.equals(row.hookOwner)
				&& row.createdAt <= now - HOOK_RECOVERY_LEASE_MILLIS;
	}

	/**
	 * Atomically changes only an expired foreign hook claim. A current owner can
	 * settle while its lease is live; after expiry we retain the operation for
	 * manual reconciliation instead of replaying an arbitrary hook.
	 */
	private boolean markExpiredHookIndeterminate(String operationId, AdditionRow row, String uuid, String pointsColumn,
			int requestedAmount, long now) throws SQLException {
		if (!row.matchesTarget(uuid, pointsColumn)
				|| row.requestedAmount != null && row.requestedAmount.intValue() != requestedAmount) {
			throw new SQLException("Mismatched shared point addition operation");
		}
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("operation_id")
				+ " = ? AND " + qi("state") + " = ? AND " + qi("created_at") + " <= ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(update)) {
			connection.setAutoCommit(false);
			statement.setString(1, INDETERMINATE);
			statement.setString(2, operationId);
			statement.setString(3, HOOK_STARTED);
			statement.setLong(4, now - HOOK_RECOVERY_LEASE_MILLIS);
			if (statement.executeUpdate() != 1) {
				rollback(connection);
				return false;
			}
			connection.commit();
			return true;
		}
	}

	/**
	 * Records that a claimed receive hook cannot be safely retried. This is a
	 * durable operator-facing distinction from a live claim: a later retry must
	 * not wait for an owner that can no longer settle it, or invoke listeners
	 * again.
	 */
	void markIndeterminate(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner)
			throws SQLException {
		if (!isSafeColumn(pointsColumn)) throw new SQLException("Unsafe shared point column");
		String select = "SELECT " + qi("player_uuid") + ", " + qi("points_column") + ", " + qi("amount")
				+ ", " + qi("state") + ", " + qi("total_points") + ", " + qi("requested_amount") + ", "
				+ qi("hook_owner") + ", " + qi("created_at") + " FROM " + qiJournal() + " WHERE "
				+ qi("operation_id") + " = ? FOR UPDATE";
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ? WHERE " + qi("operation_id")
				+ " = ? AND " + qi("state") + " = ? AND " + qi("hook_owner") + " = ?";
		try (Connection connection = connection()) {
			connection.setAutoCommit(false);
			try (PreparedStatement selectStatement = connection.prepareStatement(select)) {
				selectStatement.setString(1, operationId);
				try (ResultSet result = selectStatement.executeQuery()) {
					if (!result.next()) {
						rollback(connection);
						throw new SQLException("Shared point addition claim disappeared");
					}
					Integer total = result.getObject(5) == null ? null : Integer.valueOf(result.getInt(5));
					Integer requested = result.getObject(6) == null ? null : Integer.valueOf(result.getInt(6));
					AdditionRow row = new AdditionRow(result.getString(1), result.getString(2), result.getInt(3),
							result.getString(4), total, requested, result.getString(7), result.getLong(8));
					if (!row.matchesTarget(uuid, pointsColumn)
							|| row.requestedAmount != null && row.requestedAmount.intValue() != requestedAmount) {
						rollback(connection);
						throw new SQLException("Mismatched shared point addition operation");
					}
					if (!HOOK_STARTED.equals(row.state)) {
						rollback(connection);
						return;
					}
				}
			}
			try (PreparedStatement updateStatement = connection.prepareStatement(update)) {
				updateStatement.setString(1, INDETERMINATE);
				updateStatement.setString(2, operationId);
				updateStatement.setString(3, HOOK_STARTED);
				updateStatement.setString(4, owner);
				if (updateStatement.executeUpdate() != 1) {
					rollback(connection);
					return;
				}
			}
			connection.commit();
		}
	}

	/**
	 * Returns a previously committed addition before a retry invokes its Bukkit
	 * receive hook. The amount deliberately remains part of {@link #add}: a
	 * listener may have adjusted it during the original invocation, so comparing
	 * it to a retry's pre-listener amount would make a completed operation look
	 * new. The immutable operation id is still bound to its player and points
	 * column before its total can be replayed.
	 */
	AdditionResult findCompleted(String operationId, String uuid, String pointsColumn) throws SQLException {
		if (!isSafeColumn(pointsColumn)) throw new SQLException("Unsafe shared point column");
		AdditionRow existing = find(operationId);
		if (existing == null) return null;
		if (!existing.matchesTarget(uuid, pointsColumn)) {
			throw new SQLException("Mismatched shared point addition operation");
		}
		if (!(COMPLETED.equals(existing.state) || ACKNOWLEDGED.equals(existing.state)) || existing.total == null) {
			throw new SQLException("Shared point addition operation is not confirmable: " + operationId);
		}
		return new AdditionResult(existing.total.intValue());
	}

	private AdditionResult existingResult(String operationId, AdditionRow row, String uuid, String pointsColumn,
			int amount) throws SQLException {
		if (!row.matches(uuid, pointsColumn, amount)) throw new SQLException("Mismatched shared point addition operation");
		if (!(COMPLETED.equals(row.state) || ACKNOWLEDGED.equals(row.state)) || row.total == null) {
			throw new SQLException("Shared point addition operation is not confirmable: " + operationId);
		}
		return new AdditionResult(row.total.intValue());
	}

	/** Marks an applied operation safe to retire after its replay checkpoint is durable. */
	void acknowledge(String operationId, long now) throws SQLException {
		String update = "UPDATE " + qiJournal() + " SET " + qi("state") + " = ?, " + qi("created_at")
				+ " = ? WHERE " + qi("operation_id") + " = ? AND " + qi("state") + " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(update)) {
			statement.setString(1, ACKNOWLEDGED);
			statement.setLong(2, now);
			statement.setString(3, operationId);
			statement.setString(4, COMPLETED);
			statement.executeUpdate();
		}
	}

	/** Removes a bounded batch of replay-acknowledged entries after the retention window. */
	void cleanupAcknowledged(long now) throws SQLException {
		long cutoff = now - COMPLETED_RETENTION_MILLIS;
		String select = "SELECT " + qi("operation_id") + " FROM " + qiJournal() + " WHERE " + qi("state")
				+ " = ? AND " + qi("created_at") + " <= ? ORDER BY " + qi("created_at") + " ASC LIMIT ?";
		String delete = "DELETE FROM " + qiJournal() + " WHERE " + qi("operation_id") + " = ? AND "
				+ qi("state") + " = ? AND " + qi("created_at") + " <= ?";
		try (Connection connection = connection(); PreparedStatement selectStatement = connection.prepareStatement(select);
				PreparedStatement deleteStatement = connection.prepareStatement(delete)) {
			selectStatement.setString(1, ACKNOWLEDGED);
			selectStatement.setLong(2, cutoff);
			selectStatement.setInt(3, CLEANUP_BATCH_SIZE);
			Set<String> operationIds = new java.util.LinkedHashSet<>();
			try (ResultSet result = selectStatement.executeQuery()) {
				while (result.next()) operationIds.add(result.getString(1));
			}
			for (String operationId : operationIds) {
				deleteStatement.setString(1, operationId);
				deleteStatement.setString(2, ACKNOWLEDGED);
				deleteStatement.setLong(3, cutoff);
				deleteStatement.executeUpdate();
			}
		}
	}

	private void commitAndConfirm(Connection connection, String operationId) throws SQLException {
		try {
			connection.commit();
		} catch (SQLException ambiguousCommit) {
			closeQuietly(connection);
			AdditionRow row = find(operationId);
			if (row != null && COMPLETED.equals(row.state) && row.total != null) return;
			throw ambiguousCommit;
		}
	}

	private AdditionRow find(String operationId) throws SQLException {
		String select = "SELECT " + qi("player_uuid") + ", " + qi("points_column") + ", " + qi("amount")
				+ ", " + qi("state") + ", " + qi("total_points") + ", " + qi("requested_amount") + ", "
				+ qi("hook_owner") + ", " + qi("created_at") + " FROM " + qiJournal() + " WHERE "
				+ qi("operation_id") + " = ?";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(select)) {
			statement.setString(1, operationId);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) return null;
				Integer total = result.getObject(5) == null ? null : Integer.valueOf(result.getInt(5));
				Integer requested = result.getObject(6) == null ? null : Integer.valueOf(result.getInt(6));
				return new AdditionRow(result.getString(1), result.getString(2), result.getInt(3), result.getString(4), total,
						requested, result.getString(7), result.getLong(8));
			}
		}
	}

	private void ensureSchema() throws SQLException {
		String create = "CREATE TABLE IF NOT EXISTS " + qiJournal() + " (" + qi("operation_id")
				+ " VARCHAR(64) NOT NULL, " + qi("player_uuid") + " VARCHAR(37) NOT NULL, "
				+ qi("points_column") + " VARCHAR(128) NOT NULL, " + qi("amount") + " INT NOT NULL, "
				+ qi("requested_amount") + " INT NULL, " + qi("state") + " VARCHAR(16) NOT NULL, "
				+ qi("total_points") + " INT NULL, " + qi("hook_owner") + " VARCHAR(64) NULL, "
				+ qi("created_at") + " BIGINT NOT NULL, PRIMARY KEY (" + qi("operation_id") + "));";
		try (Connection connection = connection(); PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
			addColumnIfMissing(connection, "requested_amount", "INT NULL");
			addColumnIfMissing(connection, "hook_owner", "VARCHAR(64) NULL");
			createIndex(connection);
		}
	}

	private void addColumnIfMissing(Connection connection, String column, String definition) throws SQLException {
		String alter = "ALTER TABLE " + qiJournal() + " ADD COLUMN " + qi(column) + " " + definition;
		try (PreparedStatement statement = connection.prepareStatement(alter)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (!isDuplicateColumn(failure)) throw failure;
		}
	}

	private void createIndex(Connection connection) throws SQLException {
		String indexName = "vp_pa_" + Integer.toUnsignedString(journalTable.hashCode(), 36) + "_state_created";
		String create = "CREATE INDEX " + (table.getDbType() == DbType.POSTGRESQL ? "IF NOT EXISTS " : "")
				+ qi(indexName) + " ON " + qiJournal() + " (" + qi("state") + ", " + qi("created_at") + ");";
		try (PreparedStatement statement = connection.prepareStatement(create)) {
			statement.executeUpdate();
		} catch (SQLException failure) {
			if (!isDuplicateIndex(failure)) throw failure;
		}
	}

	private static void expungeInitialized() {
		IdentityWeakReference cleared;
		while ((cleared = (IdentityWeakReference) INITIALIZED_QUEUE.poll()) != null) INITIALIZED.remove(cleared);
		for (Iterator<IdentityWeakReference> iterator = INITIALIZED.iterator(); iterator.hasNext();) {
			if (iterator.next().get() == null) iterator.remove();
		}
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

	private static boolean isSafeColumn(String column) {
		return column != null && column.matches("[A-Za-z][A-Za-z0-9_]{0,127}");
	}

	private Connection connection() throws SQLException {
		Connection connection = table.getMysql().getConnectionManager().getConnection();
		if (connection == null) throw new SQLException("Unable to acquire shared MySQL connection");
		return connection;
	}

	private String qiJournal() { return table.qi(journalTable); }
	private String qi(String identifier) { return table.qi(identifier); }
	private String uuidCast() { return table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?"; }

	private static void rollback(Connection connection) {
		try {
			connection.rollback();
		} catch (SQLException ignored) {
			// The follow-up lookup determines whether an ambiguous commit landed.
		}
	}

	private static void closeQuietly(Connection connection) {
		try {
			connection.close();
		} catch (SQLException ignored) {
			// Confirmation remains safe even if this broken handle cannot close cleanly.
		}
	}

	private static boolean isDuplicate(SQLException failure) {
		return "23505".equals(failure.getSQLState()) || failure.getErrorCode() == 1062;
	}

	private static boolean isDuplicateIndex(SQLException failure) {
		return failure.getErrorCode() == 1061 || "42P07".equals(failure.getSQLState());
	}

	private static boolean isDuplicateColumn(SQLException failure) {
		return failure.getErrorCode() == 1060 || "42701".equals(failure.getSQLState());
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

	record AdditionResult(int total) {}
	record HookClaim(boolean claimed, boolean completed, boolean requiresReconciliation, int total) {
		static HookClaim claimedByCaller() { return new HookClaim(true, false, false, 0); }
		static HookClaim completed(int total) { return new HookClaim(false, true, false, total); }
		static HookClaim reconciliationRequired() { return new HookClaim(false, false, true, 0); }
		static HookClaim inProgress() { return new HookClaim(false, false, false, 0); }
	}
	private record AdditionRow(String uuid, String pointsColumn, int amount, String state, Integer total,
			Integer requestedAmount, String hookOwner, long createdAt) {
		boolean matches(String expectedUuid, String expectedPointsColumn, int expectedAmount) {
			return uuid.equals(expectedUuid) && pointsColumn.equals(expectedPointsColumn) && amount == expectedAmount;
		}

		boolean matchesTarget(String expectedUuid, String expectedPointsColumn) {
			return uuid.equals(expectedUuid) && pointsColumn.equals(expectedPointsColumn);
		}
	}
}
