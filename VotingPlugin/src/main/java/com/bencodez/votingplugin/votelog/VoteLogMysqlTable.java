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
 * Columns: - vote_id (generated UUID string) - vote_time (LONG millis, explicit
 * time vote happened) - event (VOTE_RECEIVED default) - status
 * (IMMEDIATE/CACHED) - cached_total (int, proxy only snapshot; can be 0
 * on backend) - service - player_uuid - player_name
 */
public abstract class VoteLogMysqlTable {

	/**
	 * Future-proof event type. For now, everything is VOTE_RECEIVED.
	 */
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

	/**
	 * Create a new MySQL pool and connect (this table "owns" the connection pool).
	 */
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

	/**
	 * Reuse an existing MySQL pool (this table does NOT own the connection pool).
	 *
	 * NOTE: We still accept MysqlConfig to match VotingPlugin format and to reuse:
	 * - database name - optional table name override/prefix
	 */
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
		String sql = "CREATE TABLE IF NOT EXISTS `" + getName() + "` (" + "`vote_id` VARCHAR(36) NOT NULL,"
				+ "PRIMARY KEY (`vote_id`)" + ");";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			debug1(e);
		}
	}

	private void ensureColumns() {
		synchronized (initLock) {
			// vote_time is stored as millis since epoch
			checkColumn("vote_time", DataType.INTEGER);
			alterColumnType("vote_time", "BIGINT NOT NULL DEFAULT '0'");

			checkColumn("player_uuid", DataType.STRING);
			alterColumnType("player_uuid", "VARCHAR(37)");

			checkColumn("player_name", DataType.STRING);
			alterColumnType("player_name", "VARCHAR(16)");

			checkColumn("service", DataType.STRING);
			alterColumnType("service", "VARCHAR(64)");

			// NEW: event column (default to VOTE_RECEIVED)
			checkColumn("event", DataType.STRING);
			alterColumnType("event", "VARCHAR(32) NOT NULL DEFAULT '" + VoteLogEvent.VOTE_RECEIVED.name() + "'");

			checkColumn("status", DataType.STRING);
			alterColumnType("status", "VARCHAR(16)");

			// Only cached count field kept: global cached votes snapshot on proxy
			checkColumn("cached_total", DataType.INTEGER);
			alterColumnType("cached_total", "INT NOT NULL DEFAULT '0'");

			// indexes (safe to attempt)
			tryCreateIndex("idx_vote_time", "(`vote_time`)");
			tryCreateIndex("idx_uuid_time", "(`player_uuid`,`vote_time`)");
			tryCreateIndex("idx_service_time", "(`service`,`vote_time`)");
			tryCreateIndex("idx_status_time", "(`status`,`vote_time`)");
			tryCreateIndex("idx_event_time", "(`event`,`vote_time`)");
		}
	}

	private void tryCreateIndex(String indexName, String cols) {
		String sql = "CREATE INDEX `" + indexName + "` ON `" + getName() + "` " + cols + ";";
		try {
			new Query(mysql, sql).executeUpdateAsync();
		} catch (SQLException e) {
			// likely exists
			if (debug) {
				mysql.debug("Index create failed (likely exists) " + indexName + ": " + e.getMessage());
			}
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

	/**
	 * Insert a single vote log row (async) with explicit vote time millis.
	 *
	 * Default event: VOTE_RECEIVED
	 *
	 * @param voteTimeMillis   time vote happened (millis since epoch)
	 * @param proxyCachedTotal global cached votes snapshot on proxy (set 0 on
	 *                         backend if unknown)
	 *
	 * @return vote id
	 */
	public String logVote(UUID voteUUID, VoteLogStatus status, String service, String playerUuid, String playerName,
			long voteTimeMillis, int proxyCachedTotal) {

		return logVote(voteUUID, VoteLogEvent.VOTE_RECEIVED, status, service, playerUuid, playerName, voteTimeMillis,
				proxyCachedTotal);
	}

	/**
	 * Insert with explicit event (future-proof).
	 */
	public String logVote(UUID voteUUID, VoteLogEvent event, VoteLogStatus status, String service, String playerUuid,
			String playerName, long voteTimeMillis, int proxyCachedTotal) {

		String voteId = voteUUID.toString();

		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO `").append(getName()).append("` SET ");
		sb.append("`vote_id`='").append(voteId).append("', ");
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

	/**
	 * Purge logs older than X days (async). Batch delete.
	 *
	 * Note: uses current time millis and BIGINT vote_time.
	 */
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
	// Helpers for commands (reads)
	// -------------------------

	public VoteLogEntry getByVoteId(String voteId) {
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_id=? LIMIT 1;";
		List<VoteLogEntry> rows = query(sql, new Object[] { voteId });
		return rows.isEmpty() ? null : rows.get(0);
	}

	public List<VoteLogEntry> getRecent(int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` ORDER BY vote_time DESC LIMIT " + limit + ";";
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
				+ "FROM `" + getName() + "` WHERE vote_time >= ? ORDER BY vote_time DESC LIMIT " + limit + ";";
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
			base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
			return query(base, new Object[] { uuid, cutoff });
		}
		base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
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
			base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
			return query(base, new Object[] { playerName, cutoff });
		}
		base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
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
			base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
			return query(base, new Object[] { service, cutoff });
		}
		base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
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
			base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
			return query(base, new Object[] { status.name(), cutoff });
		}
		base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
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
			base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
			return query(base, new Object[] { event.name(), cutoff });
		}
		base += "ORDER BY vote_time DESC LIMIT " + limit + ";";
		return query(base, new Object[] { event.name() });
	}

	/**
	 * Aggregate counts over the last X days.
	 */
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

	/**
	 * Top services/sites by vote count over the last X days.
	 */
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

	/**
	 * Distinct voters in last X days.
	 */
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

	/**
	 * Cursor paging: rows older than timestamp millis.
	 */
	public List<VoteLogEntry> getRecentBefore(long beforeVoteTimeMillis, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_time < ? ORDER BY vote_time DESC LIMIT " + limit + ";";
		return query(sql, new Object[] { beforeVoteTimeMillis });
	}

	/**
	 * Cursor paging: per-player rows older than timestamp millis.
	 */
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
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { uuid, beforeVoteTimeMillis, cutoff });
		}
		return query(sql, new Object[] { uuid, beforeVoteTimeMillis });
	}

	// -------------------------
	// Internal query helper
	// -------------------------

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

	// -------------------------
	// DTOs for command layer
	// -------------------------

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
