package com.bencodez.votingplugin.votelog;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

/**
 * Vote log table usable from BOTH proxy and backend.
 *
 * LOG ONLY table (not the offline queue).
 *
 * Columns:
 * - id (auto increment primary key)
 * - vote_id (correlation id / vote uuid; NOT UNIQUE; may repeat for other events)
 * - vote_time (LONG millis, explicit time vote happened)
 * - event (VOTE_RECEIVED default)
 * - status (IMMEDIATE/CACHED)
 * - cached_total (int, proxy only snapshot; can be 0 on backend)
 * - service
 * - player_uuid
 * - player_name
 */
public abstract class VoteLogMysqlTable {

	public enum VoteLogEvent {
		VOTE_RECEIVED
	}

	public enum VoteLogStatus {
		IMMEDIATE, CACHED
	}

	private final Object initLock = new Object();

	private final com.bencodez.simpleapi.sql.mysql.MySQL mysql;

	private final String name;

	private final boolean debug;

	private final boolean ownsConnection;

	public abstract void logSevere1(String string);

	public abstract void logInfo1(String string);

	public abstract void debug1(SQLException e);

	public VoteLogMysqlTable(String tableName, MysqlConfig config, boolean debug) {
		this.debug = debug;
		this.ownsConnection = true;

		if (config.hasTableNameSet()) {
			tableName = config.getTableName();
		}
		String tmpName = tableName;
		if (config.getTablePrefix() != null) {
			tmpName = config.getTablePrefix() + tableName;
		}
		name = tmpName;

		if (config.getPoolName().isEmpty()) {
			config.setPoolName("VotingPlugin" + "-" + tableName);
		}

		mysql = new com.bencodez.simpleapi.sql.mysql.MySQL(config.getMaxThreads()) {

			@Override
			public void debug(SQLException e) {
				if (VoteLogMysqlTable.this.debug) {
					e.printStackTrace();
				}
			}

			@Override
			public void severe(String string) {
				logSevere1(string);
			}

			@Override
			public void debug(String msg) {
				if (VoteLogMysqlTable.this.debug) {
					logInfo1("MYSQL DEBUG: " + msg);
				}
			}
		};

		if (!mysql.connect(config)) {
			// caller decides how to handle
		}

		initDatabase(config.getDatabase());
	}

	public VoteLogMysqlTable(String tableName, com.bencodez.simpleapi.sql.mysql.MySQL existingMysql, MysqlConfig config,
			boolean debug) {
		this.debug = debug;
		this.mysql = existingMysql;
		this.ownsConnection = false;

		if (config.hasTableNameSet()) {
			tableName = config.getTableName();
		}
		String tmpName = tableName;
		if (config.getTablePrefix() != null) {
			tmpName = config.getTablePrefix() + tableName;
		}
		name = tmpName;

		initDatabase(config.getDatabase());
	}

	private void initDatabase(String database) {
		if (database != null && !database.isEmpty()) {
			try {
				new Query(mysql, "USE `" + database + "`;").executeUpdateAsync();
			} catch (SQLException e) {
				logSevere1("Failed to send use database query: " + database + " Error: " + e.getMessage());
				debug1(e);
			}
		}

		createTable();
		ensureColumns();
	}

	public com.bencodez.simpleapi.sql.mysql.MySQL getMysql() {
		return mysql;
	}

	public String getName() {
		return name;
	}

	public void close() {
		shutdown();
	}

	public void shutdown() {
		if (ownsConnection) {
			mysql.disconnect();
		}
	}

	// -------------------------
	// Init / schema
	// -------------------------

	private void createTable() {
		// New schema: id auto-increment PK, vote_id NOT unique (can repeat).
		// vote_id can be NULL to allow future event types not tied to a vote correlation id.
		String sql = "CREATE TABLE IF NOT EXISTS `" + getName() + "` ("
				+ "`id` BIGINT NOT NULL AUTO_INCREMENT,"
				+ "`vote_id` VARCHAR(36) NULL,"
				+ "PRIMARY KEY (`id`)"
				+ ");";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			debug1(e);
		}
	}

	private void ensureColumns() {
		synchronized (initLock) {

			// Make sure the PK is id before other work.
			ensureIdPrimaryKey();

			// vote_id column must exist and be non-unique; ensure type/nullable
			checkColumn("vote_id", DataType.STRING);
			alterColumnType("vote_id", "VARCHAR(36) NULL");

			// vote_time is stored as millis since epoch
			checkColumn("vote_time", DataType.INTEGER);
			alterColumnType("vote_time", "BIGINT NOT NULL DEFAULT '0'");

			checkColumn("player_uuid", DataType.STRING);
			alterColumnType("player_uuid", "VARCHAR(37)");

			checkColumn("player_name", DataType.STRING);
			alterColumnType("player_name", "VARCHAR(16)");

			checkColumn("service", DataType.STRING);
			alterColumnType("service", "VARCHAR(64)");

			checkColumn("event", DataType.STRING);
			alterColumnType("event", "VARCHAR(32) NOT NULL DEFAULT '" + VoteLogEvent.VOTE_RECEIVED.name() + "'");

			checkColumn("status", DataType.STRING);
			alterColumnType("status", "VARCHAR(16)");

			checkColumn("cached_total", DataType.INTEGER);
			alterColumnType("cached_total", "INT NOT NULL DEFAULT '0'");

			// indexes (safe to attempt)
			tryCreateIndex("idx_vote_time", "(`vote_time`)");
			tryCreateIndex("idx_uuid_time", "(`player_uuid`,`vote_time`)");
			tryCreateIndex("idx_service_time", "(`service`,`vote_time`)");
			tryCreateIndex("idx_status_time", "(`status`,`vote_time`)");
			tryCreateIndex("idx_event_time", "(`event`,`vote_time`)");

			// NEW: helpful for correlation lookups / chains of events
			tryCreateIndex("idx_vote_id", "(`vote_id`)");
			tryCreateIndex("idx_vote_id_time", "(`vote_id`,`vote_time`)");
		}
	}

	/**
	 * Migration:
	 * - Old schema had vote_id as PRIMARY KEY (or even UNIQUE).
	 * - New schema wants id BIGINT AUTO_INCREMENT as PRIMARY KEY and vote_id NON-UNIQUE (may repeat).
	 *
	 * This method:
	 * - Adds `id` if missing, and makes it PRIMARY KEY.
	 * - If vote_id is currently a UNIQUE index/constraint, attempts to drop it.
	 */
	private void ensureIdPrimaryKey() {
		try {
			boolean hasId = hasColumnQuery("id");
			boolean pkIsId = hasPrimaryKeyOn("id");

			// If id exists and is already PK, still ensure no uniqueness on vote_id.
			if (hasId && pkIsId) {
				dropUniqueVoteIdIfPresent();
				return;
			}

			if (!hasId) {
				// Convert old -> new:
				// - Drop old primary (likely vote_id)
				// - Add id auto increment
				// - Make id primary
				String migrate = "ALTER TABLE `" + getName() + "` "
						+ "DROP PRIMARY KEY, "
						+ "ADD COLUMN `id` BIGINT NOT NULL AUTO_INCREMENT FIRST, "
						+ "ADD PRIMARY KEY (`id`);";
				new Query(mysql, migrate).executeUpdate();
			} else {
				// id exists but isn't PK: move primary to id
				String migrate2 = "ALTER TABLE `" + getName() + "` "
						+ "DROP PRIMARY KEY, "
						+ "ADD PRIMARY KEY (`id`);";
				new Query(mysql, migrate2).executeUpdate();
			}

			// Ensure any leftover UNIQUE on vote_id is removed
			dropUniqueVoteIdIfPresent();

		} catch (SQLException e) {
			debug1(e);
		}
	}

	private void dropUniqueVoteIdIfPresent() {
		try {
			// Drop common names you may have used
			dropIndexIfExists("uk_vote_id");
			// If you ever had vote_id as PRIMARY, that was dropped above.
			// If there is some other unique index on vote_id with a different name, try to detect and drop it:
			String idx = findUniqueIndexOnColumn("vote_id");
			if (idx != null && !idx.isEmpty() && !"PRIMARY".equalsIgnoreCase(idx)) {
				dropIndexIfExists(idx);
			}
		} catch (Exception e) {
			// don't fail startup
		}
	}

	private String findUniqueIndexOnColumn(String column) {
		String sql = "SHOW INDEX FROM `" + getName() + "` WHERE Column_name = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, column);
			try (ResultSet rs = ps.executeQuery()) {
				while (rs.next()) {
					String keyName = rs.getString("Key_name");
					int nonUnique = rs.getInt("Non_unique"); // 0 = unique
					if (keyName != null && nonUnique == 0) {
						return keyName;
					}
				}
			}
		} catch (SQLException e) {
			debug1(e);
		}
		return null;
	}

	private void dropIndexIfExists(String indexName) {
		if (indexName == null || indexName.isEmpty()) {
			return;
		}
		if (!hasIndex(indexName)) {
			return;
		}
		try {
			new Query(mysql, "DROP INDEX `" + indexName + "` ON `" + getName() + "`;").executeUpdate();
		} catch (SQLException e) {
			// If it fails, ignore (permissions/engine/etc). We just don't want startup spam.
			debug1(e);
		}
	}

	private boolean hasPrimaryKeyOn(String column) {
		String sql = "SHOW KEYS FROM `" + getName() + "` WHERE Key_name='PRIMARY';";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql);
				ResultSet rs = ps.executeQuery()) {

			while (rs.next()) {
				String col = rs.getString("Column_name");
				if (col != null && col.equalsIgnoreCase(column)) {
					return true;
				}
			}
			return false;
		} catch (SQLException e) {
			debug1(e);
			// Conservative: if we can't tell, assume it's fine to avoid loops.
			return true;
		}
	}

	private boolean hasIndex(String indexName) {
		String sql = "SHOW INDEX FROM `" + getName() + "` WHERE Key_name = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, indexName);
			try (ResultSet rs = ps.executeQuery()) {
				return rs.next();
			}
		} catch (SQLException e) {
			debug1(e);
			// If we can't determine, act conservative: assume it exists to avoid spam
			return true;
		}
	}

	private static boolean isDuplicateIndexName(SQLException e) {
		// MySQL: 1061 = Duplicate key name
		return e != null && e.getErrorCode() == 1061;
	}

	private void tryCreateIndex(String indexName, String cols) {
		if (hasIndex(indexName)) {
			return;
		}

		String sql = "CREATE INDEX `" + indexName + "` ON `" + getName() + "` " + cols + ";";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			if (isDuplicateIndexName(e)) {
				return;
			}
			debug1(e);
		}
	}

	private boolean hasColumnQuery(String column) {
		String sql = "SHOW COLUMNS FROM `" + getName() + "` LIKE ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, column);
			try (ResultSet rs = ps.executeQuery()) {
				return rs.next();
			}
		} catch (SQLException e) {
			debug1(e);
			return false;
		}
	}

	private void checkColumn(String column, DataType dataType) {
		if (!hasColumnQuery(column)) {
			String type = "text";
			if (dataType == DataType.INTEGER) {
				type = "int";
			}
			String sql = "ALTER TABLE `" + getName() + "` ADD COLUMN `" + column + "` " + type + ";";
			try {
				new Query(mysql, sql).executeUpdate();
			} catch (SQLException e) {
				debug1(e);
			}
		}
	}

	private void alterColumnType(String column, String newType) {
		try {
			new Query(mysql, "ALTER TABLE `" + getName() + "` MODIFY `" + column + "` " + newType + ";")
					.executeUpdateAsync();
		} catch (SQLException e) {
			debug1(e);
		}
	}

	private static String escape(String in) {
		if (in == null) {
			return "";
		}
		return in.replace("\\", "\\\\").replace("'", "''");
	}

	// -------------------------
	// Writes
	// -------------------------

	public String logVote(UUID voteUUID, VoteLogStatus status, String service, String playerUuid, String playerName,
			long voteTimeMillis, int proxyCachedTotal) {

		return logVote(voteUUID, VoteLogEvent.VOTE_RECEIVED, status, service, playerUuid, playerName, voteTimeMillis,
				proxyCachedTotal);
	}

	public String logVote(UUID voteUUID, VoteLogEvent event, VoteLogStatus status, String service, String playerUuid,
			String playerName, long voteTimeMillis, int proxyCachedTotal) {

		String voteId = voteUUID != null ? voteUUID.toString() : null;

		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO `").append(getName()).append("` SET ");
		// NOTE: no `id` here; AUTO_INCREMENT
		if (voteId != null) {
			sb.append("`vote_id`='").append(voteId).append("', ");
		} else {
			sb.append("`vote_id`=NULL, ");
		}
		sb.append("`vote_time`='").append(voteTimeMillis).append("', ");
		sb.append("`player_uuid`='").append(playerUuid.toString()).append("', ");
		sb.append("`player_name`='").append(escape(playerName)).append("', ");
		sb.append("`service`='").append(escape(service)).append("', ");
		sb.append("`event`='").append(event == null ? VoteLogEvent.VOTE_RECEIVED.name() : event.name()).append("', ");
		sb.append("`status`='").append(status.name()).append("', ");
		sb.append("`cached_total`='").append(proxyCachedTotal).append("';");

		try {
			new Query(mysql, sb.toString()).executeUpdateAsync();
		} catch (SQLException e) {
			debug1(e);
		}

		return voteId;
	}

	// -------------------------
	// Purge
	// -------------------------

	public void purgeOlderThanDays(int days, int batchSize) {
		if (days <= 0) {
			return;
		}
		if (batchSize <= 0) {
			batchSize = 5000;
		}

		long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);

		String sql = "DELETE FROM `" + getName() + "` WHERE `vote_time` < " + cutoff + " LIMIT " + batchSize + ";";
		try {
			new Query(mysql, sql).executeUpdateAsync();
		} catch (SQLException e) {
			debug1(e);
		}
	}

	// -------------------------
	// Reads
	// -------------------------

	/**
	 * vote_id is NOT unique. This returns the most recent row for that vote_id.
	 */
	public VoteLogEntry getByVoteId(String voteId) {
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_id=? ORDER BY vote_time DESC, id DESC LIMIT 1;";
		List<VoteLogEntry> rows = query(sql, new Object[] { voteId });
		return rows.isEmpty() ? null : rows.get(0);
	}

	public List<VoteLogEntry> getRecent(int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(sql, new Object[] {});
	}

	public List<VoteLogEntry> getRecentSinceDays(int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		if (days <= 0) {
			return getRecent(limit);
		}
		long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_time >= ? ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(sql, new Object[] { cutoff });
	}

	public List<VoteLogEntry> getByPlayer(UUID uuid, int days, int limit) {
		return getByPlayer(uuid.toString(), days, limit);
	}

	public List<VoteLogEntry> getByPlayer(String uuid, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String base = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE player_uuid=? ";
		if (days > 0) {
			long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
			base += "AND vote_time >= ? ";
			base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
			return query(base, new Object[] { uuid, cutoff });
		}
		base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(base, new Object[] { uuid });
	}

	public List<VoteLogEntry> getByPlayerName(String playerName, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String base = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE player_name=? ";
		if (days > 0) {
			long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
			base += "AND vote_time >= ? ";
			base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
			return query(base, new Object[] { playerName, cutoff });
		}
		base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(base, new Object[] { playerName });
	}

	public List<VoteLogEntry> getByService(String service, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String base = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE service=? ";
		if (days > 0) {
			long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
			base += "AND vote_time >= ? ";
			base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
			return query(base, new Object[] { service, cutoff });
		}
		base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(base, new Object[] { service });
	}

	public List<VoteLogEntry> getByStatus(VoteLogStatus status, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String base = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE status=? ";
		if (days > 0) {
			long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
			base += "AND vote_time >= ? ";
			base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
			return query(base, new Object[] { status.name(), cutoff });
		}
		base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(base, new Object[] { status.name() });
	}

	public List<VoteLogEntry> getByEvent(VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		if (event == null) {
			event = VoteLogEvent.VOTE_RECEIVED;
		}
		String base = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE event=? ";
		if (days > 0) {
			long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
			base += "AND vote_time >= ? ";
			base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
			return query(base, new Object[] { event.name(), cutoff });
		}
		base += "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(base, new Object[] { event.name() });
	}

	public VoteLogCounts getCounts(int days) {
		String sql;
		boolean useCutoff = days > 0;
		long cutoff = 0;
		if (useCutoff) {
			cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
		}

		sql = "SELECT " + "COUNT(*) AS total, " + "SUM(status='IMMEDIATE') AS immediate, "
				+ "SUM(status='CACHED') AS cached " + "FROM `" + getName() + "` "
				+ (useCutoff ? "WHERE vote_time >= ?" : "") + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (useCutoff) {
				ps.setLong(1, cutoff);
			}

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					long total = rs.getLong("total");
					long immediate = rs.getLong("immediate");
					long cached = rs.getLong("cached");
					return new VoteLogCounts(total, immediate, cached);
				}
			}
		} catch (SQLException e) {
			debug1(e);
		}

		return new VoteLogCounts(0, 0, 0);
	}

	public List<ServiceCount> getTopServices(int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = 0;
		if (useCutoff) {
			cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
		}

		String sql = "SELECT service, COUNT(*) AS votes " + "FROM `" + getName() + "` "
				+ (useCutoff ? "WHERE vote_time >= ? " : "") + "GROUP BY service ORDER BY votes DESC LIMIT " + limit
				+ ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (useCutoff) {
				ps.setLong(1, cutoff);
			}

			ResultSet rs = ps.executeQuery();
			List<ServiceCount> out = Collections.synchronizedList(new java.util.ArrayList<>());
			while (rs.next()) {
				out.add(new ServiceCount(rs.getString("service"), rs.getLong("votes")));
			}
			rs.close();
			return out;
		} catch (SQLException e) {
			debug1(e);
			return java.util.Collections.emptyList();
		}
	}

	public long getUniqueVoters(int days) {
		boolean useCutoff = days > 0;
		long cutoff = 0;
		if (useCutoff) {
			cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
		}

		String sql = "SELECT COUNT(DISTINCT player_uuid) AS uniques FROM `" + getName() + "` "
				+ (useCutoff ? "WHERE vote_time >= ?" : "") + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (useCutoff) {
				ps.setLong(1, cutoff);
			}

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					return rs.getLong("uniques");
				}
			}
		} catch (SQLException e) {
			debug1(e);
		}
		return 0;
	}

	public List<VoteLogEntry> getRecentBefore(long beforeVoteTimeMillis, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_time < ? ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(sql, new Object[] { beforeVoteTimeMillis });
	}

	public List<VoteLogEntry> getByPlayerBefore(String uuid, long beforeVoteTimeMillis, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = 0;
		if (useCutoff) {
			cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);
		}

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE player_uuid=? AND vote_time < ? "
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { uuid, beforeVoteTimeMillis, cutoff });
		}
		return query(sql, new Object[] { uuid, beforeVoteTimeMillis });
	}

	private List<VoteLogEntry> query(String sql, Object[] params) {
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			for (int i = 0; i < params.length; i++) {
				Object p = params[i];
				if (p instanceof Integer) {
					ps.setInt(i + 1, (Integer) p);
				} else if (p instanceof Long) {
					ps.setLong(i + 1, (Long) p);
				} else if (p instanceof String) {
					ps.setString(i + 1, (String) p);
				} else if (p instanceof Timestamp) {
					ps.setTimestamp(i + 1, (Timestamp) p);
				} else {
					ps.setObject(i + 1, p);
				}
			}

			ResultSet rs = ps.executeQuery();
			List<VoteLogEntry> out = Collections.synchronizedList(new java.util.ArrayList<>());

			while (rs.next()) {
				out.add(new VoteLogEntry(rs.getString("vote_id"), rs.getLong("vote_time"), rs.getString("player_uuid"),
						rs.getString("player_name"), rs.getString("service"), rs.getString("event"),
						rs.getString("status"), rs.getInt("cached_total")));
			}
			rs.close();
			return out;
		} catch (SQLException e) {
			debug1(e);
			return java.util.Collections.emptyList();
		}
	}

	public static class VoteLogEntry {
		public final String voteId;
		public final long voteTime;
		public final String playerUuid;
		public final String playerName;
		public final String service;
		public final String event;
		public final String status;
		public final int proxyCachedTotal;

		public VoteLogEntry(String voteId, long voteTime, String playerUuid, String playerName, String service,
				String event, String status, int proxyCachedTotal) {
			this.voteId = voteId;
			this.voteTime = voteTime;
			this.playerUuid = playerUuid;
			this.playerName = playerName;
			this.service = service;
			this.event = event;
			this.status = status;
			this.proxyCachedTotal = proxyCachedTotal;
		}
	}

	public static class VoteLogCounts {
		public final long total;
		public final long immediate;
		public final long cached;

		public VoteLogCounts(long total, long immediate, long cached) {
			this.total = total;
			this.immediate = immediate;
			this.cached = cached;
		}
	}

	public static class ServiceCount {
		public final String service;
		public final long votes;

		public ServiceCount(String service, long votes) {
			this.service = service;
			this.votes = votes;
		}
	}
}
