package com.bencodez.votingplugin.votelog;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

/**
 * Vote log table usable from BOTH proxy and backend.
 *
 * LOG ONLY table (not the offline queue).
 *
 * Records the "main reward triggers" (not every command executed).
 *
 * Columns: - id (auto increment primary key) - vote_id (correlation id / vote
 * uuid; NOT UNIQUE; may repeat for other events) - vote_time (LONG millis,
 * explicit time event happened) - event (VOTE_RECEIVED default; includes
 * reward-trigger events) - context (which reward/config fired + any extra info
 * like TimeType, ex: "TopVoter:MONTH") - status (IMMEDIATE/CACHED) -
 * cached_total (int, proxy only snapshot; can be 0 on backend) - service -
 * server (server name, provided by getServerName()) - player_uuid - player_name
 *
 * NOTE: - This table is NOT meant to use AbstractSqlTable's primary key
 * caching. - INSERTs use PreparedStatements (works on MySQL/MariaDB/Postgres).
 */
public abstract class VoteLogMysqlTable extends AbstractSqlTable {

	/**
	 * Event types that can be logged in the vote log.
	 */
	public enum VoteLogEvent {
		/**
		 * A vote was received.
		 */
		VOTE_RECEIVED,

		/**
		 * A vote milestone reward was triggered.
		 */
		VOTEMILESTONE,

		/**
		 * A vote streak reward was triggered.
		 */
		VOTE_STREAK_REWARD,

		/**
		 * A top voter reward was triggered.
		 */
		TOP_VOTER_REWARD,

		/**
		 * A vote shop purchase was made.
		 */
		VOTESHOP_PURCHASE

	}

	/**
	 * Status types for vote processing.
	 */
	public enum VoteLogStatus {
		/**
		 * Vote was processed immediately.
		 */
		IMMEDIATE,
		/**
		 * Vote was cached for later processing.
		 */
		CACHED
	}

	private final boolean debugEnabled;

	/**
	 * Server name to store in the vote log for ALL writes.
	 *
	 * @return server name (may be blank)
	 */
	public abstract String getServerName();

	// ---- Constructors ----

	/**
	 * Creates a new vote log table.
	 *
	 * @param baseTableName base name for the table
	 * @param config mysql configuration
	 * @param debug whether debug logging is enabled
	 */
	public VoteLogMysqlTable(String baseTableName, MysqlConfig config, boolean debug) {
		super(baseTableName, config, debug, true);
		this.debugEnabled = debug;
		ensureColumnsAndIndexes();
	}

	/**
	 * Creates a new vote log table using an existing MySQL connection.
	 *
	 * @param baseTableName base name for the table
	 * @param existingMysql existing MySQL connection
	 * @param config mysql configuration
	 * @param debug whether debug logging is enabled
	 */
	public VoteLogMysqlTable(String baseTableName, MySQL existingMysql, MysqlConfig config, boolean debug) {
		super(resolveName(baseTableName, config), existingMysql, true);
		this.debugEnabled = debug;
		ensureColumnsAndIndexes();
	}

	private static String resolveName(String baseTableName, MysqlConfig config) {
		String resolved = baseTableName;
		if (config != null) {
			if (config.hasTableNameSet()) {
				resolved = config.getTableName();
			}
			if (config.getTablePrefix() != null) {
				resolved = config.getTablePrefix() + resolved;
			}
		}
		return resolved;
	}

	// ---- AbstractSqlTable required hooks ----

	/**
	 * We intentionally do not use a real primary key for caching in this log table.
	 *
	 * @return primary key column name
	 */
	@Override
	public String getPrimaryKeyColumn() {
		return "id";
	}

	/**
	 * Gets distinct server names from the vote log.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of distinct server names
	 */
	public List<String> getDistinctServers(int days, int limit) {
		if (limit <= 0) {
			limit = 45;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT DISTINCT server FROM " + qi(getTableName()) + " WHERE server IS NOT NULL AND server != '' "
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY server ASC LIMIT " + limit + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (useCutoff) {
				ps.setLong(1, cutoff);
			}

			try (ResultSet rs = ps.executeQuery()) {
				List<String> out = new java.util.ArrayList<>();
				while (rs.next()) {
					out.add(rs.getString("server"));
				}
				return out;
			}
		} catch (SQLException e) {
			debug(e);
			return java.util.Collections.emptyList();
		}
	}

	/**
	 * Gets vote log entries for a specific server.
	 *
	 * @param server server name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByServer(String server, int days, int limit) {
		return getByServer(server, null, days, limit);
	}

	/**
	 * Gets vote log entries for a specific server, votes only.
	 *
	 * @param server server name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByServerVotesOnly(String server, int days, int limit) {
		return getByServer(server, VoteLogEvent.VOTE_RECEIVED, days, limit);
	}

	/**
	 * Gets top servers by vote count.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of server counts
	 */
	public List<ServerCount> getTopServers(int days, int limit) {
		return getTopServers(days, limit, VoteLogEvent.VOTE_RECEIVED);
	}

	/**
	 * Gets top servers by vote count with event filtering.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @param eventFilter event type to filter by
	 * @return list of server counts
	 */
	public List<ServerCount> getTopServers(int days, int limit, VoteLogEvent eventFilter) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT server, COUNT(*) AS votes " + "FROM " + qi(getTableName()) + " WHERE 1=1 "
				+ "AND server IS NOT NULL AND server != '' " + (eventFilter != null ? "AND event=? " : "")
				+ (useCutoff ? "AND vote_time >= ? " : "") + "GROUP BY server " + "ORDER BY votes DESC " + "LIMIT "
				+ limit + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			int idx = 1;
			if (eventFilter != null) {
				ps.setString(idx++, eventFilter.name());
			}
			if (useCutoff) {
				ps.setLong(idx++, cutoff);
			}

			ResultSet rs = ps.executeQuery();
			List<ServerCount> out = Collections.synchronizedList(new java.util.ArrayList<>());
			while (rs.next()) {
				out.add(new ServerCount(rs.getString("server"), rs.getLong("votes")));
			}
			rs.close();
			return out;
		} catch (SQLException e) {
			debug(e);
			return java.util.Collections.emptyList();
		}
	}

	/**
	 * Holds server name and vote count.
	 */
	public static class ServerCount {
		/**
		 * Server name.
		 */
		public final String server;
		/**
		 * Vote count for this server.
		 */
		public final long votes;

		/**
		 * Constructs a new ServerCount.
		 *
		 * @param server server name
		 * @param votes vote count
		 */
		public ServerCount(String server, long votes) {
			this.server = server;
			this.votes = votes;
		}
	}

	/**
	 * Gets vote log entries for a specific server with event filtering.
	 *
	 * @param server server name to filter by
	 * @param event event type to filter by (null for all)
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByServer(String server, VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		if (server == null) {
			server = "";
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE server=? " + (event != null ? "AND event=? " : "")
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (event != null && useCutoff) {
			return query(sql, new Object[] { server, event.name(), cutoff });
		} else if (event != null) {
			return query(sql, new Object[] { server, event.name() });
		} else if (useCutoff) {
			return query(sql, new Object[] { server, cutoff });
		}
		return query(sql, new Object[] { server });
	}

	@Override
	public String buildCreateTableSql(DbType dbType) {
		if (dbType == DbType.POSTGRESQL) {
			return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " (" + qi("id")
					+ " BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY, " + qi("vote_id") + " VARCHAR(36) NULL"
					+ ");";
		}

		return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " (" + qi("id")
				+ " BIGINT NOT NULL AUTO_INCREMENT, " + qi("vote_id") + " VARCHAR(36) NULL, " + "PRIMARY KEY ("
				+ qi("id") + ")" + ");";
	}

	@Override
	public void debug(String messasge) {
		if (debugEnabled) {
			logInfo("MYSQL DEBUG: " + messasge);
		}
	}

	// ---- Disable AbstractSqlTable row-key caching for this log table ----

	@Override
	public List<String> getPrimaryKeysQuery() {
		return java.util.Collections.emptyList();
	}

	@Override
	public boolean containsKey(String key) {
		return false;
	}

	@Override
	public boolean containsKeyQuery(String key) {
		return false;
	}

	// ---- Schema ensure ----

	private final Object initLock = new Object();

	private void ensureColumnsAndIndexes() {
		synchronized (initLock) {
			init();

			checkColumn("vote_id", DataType.STRING);
			checkColumn("vote_time", DataType.INTEGER);
			checkColumn("player_uuid", DataType.STRING);
			checkColumn("player_name", DataType.STRING);
			checkColumn("service", DataType.STRING);
			checkColumn("server", DataType.STRING);
			checkColumn("event", DataType.STRING);
			checkColumn("context", DataType.STRING);
			checkColumn("status", DataType.STRING);
			checkColumn("cached_total", DataType.INTEGER);

			alterColumnType("vote_id", "VARCHAR(36) NULL");
			alterColumnType("vote_time", "BIGINT NOT NULL DEFAULT '0'");
			alterColumnType("player_uuid", (dbType == DbType.POSTGRESQL) ? "UUID" : "VARCHAR(37)");
			alterColumnType("player_name", "VARCHAR(16)");
			alterColumnType("service", "VARCHAR(64)");
			alterColumnType("server", "VARCHAR(64)");
			alterColumnType("event", "VARCHAR(64) NOT NULL");
			alterColumnType("context", "VARCHAR(255) NULL");
			alterColumnType("status", "VARCHAR(16)");
			alterColumnType("cached_total", "INT NOT NULL DEFAULT '0'");

			ensureDefault("vote_time", "0");
			ensureDefault("event", "'" + VoteLogEvent.VOTE_RECEIVED.name() + "'");
			ensureDefault("cached_total", "0");

			createIndexIfMissing("idx_vote_time", new String[] { "vote_time" });
			createIndexIfMissing("idx_uuid_time", new String[] { "player_uuid", "vote_time" });
			createIndexIfMissing("idx_service_time", new String[] { "service", "vote_time" });
			createIndexIfMissing("idx_server_time", new String[] { "server", "vote_time" });
			createIndexIfMissing("idx_status_time", new String[] { "status", "vote_time" });
			createIndexIfMissing("idx_event_time", new String[] { "event", "vote_time" });
			createIndexIfMissing("idx_context_time", new String[] { "context", "vote_time" });
			createIndexIfMissing("idx_event_context_time", new String[] { "event", "context", "vote_time" });

			createIndexIfMissing("idx_vote_id", new String[] { "vote_id" });
			createIndexIfMissing("idx_vote_id_time", new String[] { "vote_id", "vote_time" });
		}
	}

	private void ensureDefault(String column, String defaultSqlLiteral) {
		if (dbType != DbType.POSTGRESQL) {
			return;
		}

		String sql = "ALTER TABLE " + qi(getTableName()) + " ALTER COLUMN " + qi(column) + " SET DEFAULT "
				+ defaultSqlLiteral + ";";
		try {
			new Query(mysql, sql).executeUpdateAsync();
		} catch (SQLException e) {
			debug(e);
		}
	}

	private void createIndexIfMissing(String indexName, String[] cols) {
		if (indexName == null || indexName.isEmpty() || cols == null || cols.length == 0) {
			return;
		}

		if (hasIndex(indexName)) {
			return;
		}

		StringBuilder colsSql = new StringBuilder();
		colsSql.append("(");
		for (int i = 0; i < cols.length; i++) {
			if (i > 0) {
				colsSql.append(",");
			}
			colsSql.append(qi(cols[i]));
		}
		colsSql.append(")");

		String sql;
		if (dbType == DbType.POSTGRESQL) {
			sql = "CREATE INDEX IF NOT EXISTS " + qi(indexName) + " ON " + qi(getTableName()) + " " + colsSql + ";";
		} else {
			sql = "CREATE INDEX " + qi(indexName) + " ON " + qi(getTableName()) + " " + colsSql + ";";
		}

		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			if (isDuplicateIndexName(e)) {
				return;
			}
			debug(e);
		}
	}

	private boolean hasIndex(String indexName) {
		if (dbType == DbType.POSTGRESQL) {
			String sql = "SELECT 1 FROM pg_indexes WHERE schemaname = current_schema() AND tablename = ? AND indexname = ?;";
			try (Connection conn = mysql.getConnectionManager().getConnection();
					PreparedStatement ps = conn.prepareStatement(sql)) {
				ps.setString(1, getTableName());
				ps.setString(2, indexName);
				try (ResultSet rs = ps.executeQuery()) {
					return rs.next();
				}
			} catch (SQLException e) {
				debug(e);
				return true;
			}
		}

		String sql = "SHOW INDEX FROM " + qi(getTableName()) + " WHERE Key_name = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, indexName);
			try (ResultSet rs = ps.executeQuery()) {
				return rs.next();
			}
		} catch (SQLException e) {
			debug(e);
			return true;
		}
	}

	private static boolean isDuplicateIndexName(SQLException e) {
		return e != null && e.getErrorCode() == 1061;
	}

	// ---- Utility ----

	private static String safeStr(String s) {
		return s == null ? "" : s;
	}

	private String resolveServerName() {
		try {
			return safeStr(getServerName());
		} catch (Exception e) {
			return "";
		}
	}

	/**
	 * Appends a time type to a context string.
	 *
	 * @param baseContext base context string
	 * @param timeType time type to append
	 * @return combined context string
	 */
	public static String withTimeType(String baseContext, String timeType) {
		baseContext = safeStr(baseContext);
		timeType = safeStr(timeType);
		if (baseContext.isEmpty()) {
			return timeType.isEmpty() ? "" : timeType;
		}
		if (timeType.isEmpty()) {
			return baseContext;
		}
		return baseContext + ":" + timeType;
	}

	private static String limit(String s, int max) {
		if (s == null) {
			return "";
		}
		if (s.length() <= max) {
			return s;
		}
		return s.substring(0, max);
	}

	// ---- VoteShop helpers ----

	/**
	 * Creates a context string for a vote shop purchase.
	 *
	 * @param identifier shop item identifier
	 * @param cost cost of the item
	 * @return formatted context string
	 */
	public static String voteShopContext(String identifier, int cost) {
		identifier = safeStr(identifier);

		StringBuilder sb = new StringBuilder();
		sb.append("VShop:").append(identifier.replace(":", "_")).append(":cost=").append(cost);

		return limit(sb.toString(), 240);
	}

	// ---- INSERT ----

	private void insertLogRow(String voteId, long timeMillis, String playerUuid, String playerName, String service,
			String server, VoteLogEvent event, String context, VoteLogStatus status, int cachedTotal) {

		String sql = "INSERT INTO " + qi(getTableName())
				+ " (vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total)"
				+ " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (voteId == null || voteId.isEmpty()) {
				ps.setNull(1, Types.VARCHAR);
			} else {
				ps.setString(1, voteId);
			}

			ps.setLong(2, timeMillis);

			if (dbType == DbType.POSTGRESQL) {
				try {
					UUID u = (playerUuid == null || playerUuid.isEmpty()) ? null : UUID.fromString(playerUuid);
					if (u == null) {
						ps.setNull(3, Types.OTHER);
					} else {
						ps.setObject(3, u);
					}
				} catch (IllegalArgumentException ex) {
					ps.setNull(3, Types.OTHER);
				}
			} else {
				ps.setString(3, safeStr(playerUuid));
			}

			ps.setString(4, safeStr(playerName));
			ps.setString(5, safeStr(service));
			ps.setString(6, safeStr(server));
			ps.setString(7, (event == null ? VoteLogEvent.VOTE_RECEIVED.name() : event.name()));

			if (context == null || context.isEmpty()) {
				ps.setNull(8, Types.VARCHAR);
			} else {
				ps.setString(8, context);
			}

			ps.setString(9, (status == null ? VoteLogStatus.IMMEDIATE.name() : status.name()));
			ps.setInt(10, cachedTotal);

			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	// ---- Public logging API ----

	/**
	 * Logs a VoteMilestone reward trigger.
	 *
	 * @param voteUUID    correlation vote id if available (may be null)
	 * @param playerUuid  player uuid as string
	 * @param playerName  player name
	 * @param timeMillis  event time in millis
	 * @param milestoneId milestone id
	 * @param groupId     milestone group id
	 * @param totalType   total type name used for matching
	 * @param value       matched value
	 * @return vote id string (may be null)
	 */
	public String logVoteMilestoneReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String milestoneId, String groupId, String totalType, long value) {

		StringBuilder sb = new StringBuilder();
		sb.append("VoteMilestone:").append(safeStr(milestoneId));

		if (!safeStr(groupId).isEmpty()) {
			sb.append(" group=").append(safeStr(groupId));
		}
		if (!safeStr(totalType).isEmpty()) {
			sb.append(" total=").append(safeStr(totalType));
		}

		sb.append(" value=").append(value);

		// Keep under context column size
		String context = limit(sb.toString(), 240);

		return logEvent(voteUUID, VoteLogEvent.VOTEMILESTONE, context, playerUuid, playerName, timeMillis);
	}

	/**
	 * Logs a vote shop purchase event.
	 *
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param timeMillis event time in millis
	 * @param identifier shop item identifier
	 * @param cost cost of the item
	 */
	public void logVoteShopPurchase(String playerUuid, String playerName, long timeMillis, String identifier,
			int cost) {
		String ctx = voteShopContext(identifier, cost);
		logEvent(null, VoteLogEvent.VOTESHOP_PURCHASE, ctx, VoteLogStatus.IMMEDIATE, "", playerUuid, playerName,
				timeMillis, 0);
	}

	/**
	 * Logs a vote event.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param status processing status
	 * @param service vote service name
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param voteTimeMillis vote time in millis
	 * @param proxyCachedTotal cached total from proxy
	 * @return vote id string (may be null)
	 */
	public String logVote(UUID voteUUID, VoteLogStatus status, String service, String playerUuid, String playerName,
			long voteTimeMillis, int proxyCachedTotal) {

		return logEvent(voteUUID, VoteLogEvent.VOTE_RECEIVED, null, status, service, playerUuid, playerName,
				voteTimeMillis, proxyCachedTotal);
	}

	/**
	 * Logs a generic event.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param event event type
	 * @param context event context string
	 * @param status processing status
	 * @param service vote service name
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param timeMillis event time in millis
	 * @param proxyCachedTotal cached total from proxy
	 * @return vote id string (may be null)
	 */
	public String logEvent(UUID voteUUID, VoteLogEvent event, String context, VoteLogStatus status, String service,
			String playerUuid, String playerName, long timeMillis, int proxyCachedTotal) {

		String voteId = voteUUID != null ? voteUUID.toString() : null;

		if (event == null) {
			event = VoteLogEvent.VOTE_RECEIVED;
		}
		if (status == null) {
			status = VoteLogStatus.IMMEDIATE;
		}
		if (service == null) {
			service = "";
		}

		String server = resolveServerName();
		insertLogRow(voteId, timeMillis, playerUuid, playerName, service, server, event, context, status,
				proxyCachedTotal);

		return voteId;
	}

	/**
	 * Logs a generic event without service or status.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param event event type
	 * @param context event context string
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param timeMillis event time in millis
	 * @return vote id string (may be null)
	 */
	public String logEvent(UUID voteUUID, VoteLogEvent event, String context, String playerUuid, String playerName,
			long timeMillis) {

		String voteId = voteUUID != null ? voteUUID.toString() : null;

		if (event == null) {
			event = VoteLogEvent.VOTE_RECEIVED;
		}

		String server = resolveServerName();

		insertLogRow(voteId, timeMillis, playerUuid, playerName, "", server, event, context, null, 0);

		return voteId;
	}

	/**
	 * Logs a generic event using current time.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param event event type
	 * @param context event context string
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @return vote id string (may be null)
	 */
	public String logEventNow(UUID voteUUID, VoteLogEvent event, String context, String playerUuid, String playerName) {
		return logEvent(voteUUID, event, context, playerUuid, playerName, System.currentTimeMillis());
	}

	/**
	 * Logs a vote streak reward event.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param timeMillis event time in millis
	 * @param streakKeyWithTimeType streak key with time type suffix
	 */
	public void logVoteStreakReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String streakKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.VOTE_STREAK_REWARD, safeStr(streakKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

	/**
	 * Logs a top voter reward event.
	 *
	 * @param voteUUID correlation vote UUID (may be null)
	 * @param playerUuid player UUID as string
	 * @param playerName player name
	 * @param timeMillis event time in millis
	 * @param topVoterKeyWithTimeType top voter key with time type suffix
	 */
	public void logTopVoterReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String topVoterKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.TOP_VOTER_REWARD, safeStr(topVoterKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

	// ---- Purge ----

	/**
	 * Purges vote log entries older than the specified number of days.
	 *
	 * @param days number of days to keep
	 * @param batchSize number of rows to delete per batch
	 */
	public void purgeOlderThanDays(int days, int batchSize) {
		if (days <= 0) {
			return;
		}
		if (batchSize <= 0) {
			batchSize = 5000;
		}

		long cutoff = System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L);

		String sql;
		if (dbType == DbType.POSTGRESQL) {
			sql = "DELETE FROM " + qi(getTableName()) + " WHERE ctid IN (SELECT ctid FROM " + qi(getTableName())
					+ " WHERE (" + qi("vote_time") + ")::bigint < ? LIMIT " + batchSize + ");";
		} else {
			sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("vote_time") + " < ? LIMIT " + batchSize + ";";
		}

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setLong(1, cutoff);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	// ---- Queries ----

	/**
	 * Gets distinct service names from the vote log.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of distinct service names
	 */
	public List<String> getDistinctServices(int days, int limit) {
		if (limit <= 0) {
			limit = 45;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT DISTINCT service FROM " + qi(getTableName())
				+ " WHERE service IS NOT NULL AND service != '' " + (useCutoff ? "AND vote_time >= ? " : "")
				+ "ORDER BY service ASC LIMIT " + limit + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (useCutoff) {
				ps.setLong(1, cutoff);
			}

			try (ResultSet rs = ps.executeQuery()) {
				List<String> out = new java.util.ArrayList<>();
				while (rs.next()) {
					out.add(rs.getString("service"));
				}
				return out;
			}
		} catch (SQLException e) {
			debug(e);
			return java.util.Collections.emptyList();
		}
	}

	/**
	 * Gets recent vote log entries for all events.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getRecentAll(int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " " + (useCutoff ? "WHERE vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { cutoff });
		}
		return query(sql, new Object[] {});
	}

	/**
	 * Gets recent vote log entries filtered by event type.
	 *
	 * @param event event type to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getRecentByEvent(VoteLogEvent event, int days, int limit) {
		if (event == null) {
			return (days > 0) ? getRecentAll(days, limit) : getRecent(limit);
		}
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE event=? " + (useCutoff ? "AND vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { event.name(), cutoff });
		}
		return query(sql, new Object[] { event.name() });
	}

	/**
	 * Gets recent vote log entries with optional event filtering.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param eventFilter event type to filter by (null for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getRecent(int days, VoteLogEvent eventFilter, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE 1=1 " + (eventFilter != null ? "AND event=? " : "")
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (eventFilter != null && useCutoff) {
			return query(sql, new Object[] { eventFilter.name(), cutoff });
		} else if (eventFilter != null) {
			return query(sql, new Object[] { eventFilter.name() });
		} else if (useCutoff) {
			return query(sql, new Object[] { cutoff });
		}
		return query(sql, new Object[] {});
	}

	/**
	 * Gets the most recent vote log entry for a vote ID.
	 *
	 * @param voteId vote ID to find
	 * @return vote log entry or null if not found
	 */
	public VoteLogEntry getByVoteId(String voteId) {
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE vote_id=? ORDER BY vote_time DESC, id DESC LIMIT 1;";
		List<VoteLogEntry> rows = query(sql, new Object[] { voteId });
		return rows.isEmpty() ? null : rows.get(0);
	}

	/**
	 * Gets all vote log entries for a vote ID.
	 *
	 * @param voteId vote ID to find
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByVoteIdAll(String voteId, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE vote_id=? " + (useCutoff ? "AND vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { voteId, cutoff });
		}
		return query(sql, new Object[] { voteId });
	}

	/**
	 * Gets the most recent vote log entries.
	 *
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getRecent(int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(sql, new Object[] {});
	}

	/**
	 * Gets vote log entries for a specific service.
	 *
	 * @param service service name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByService(String service, int days, int limit) {
		return getByService(service, null, days, limit);
	}

	/**
	 * Gets vote log entries for a specific service, votes only.
	 *
	 * @param service service name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByServiceVotesOnly(String service, int days, int limit) {
		return getByService(service, VoteLogEvent.VOTE_RECEIVED, days, limit);
	}

	/**
	 * Gets vote log entries for a specific service with event filtering.
	 *
	 * @param service service name to filter by
	 * @param event event type to filter by (null for all)
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByService(String service, VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE service=? " + (event != null ? "AND event=? " : "")
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (event != null && useCutoff) {
			return query(sql, new Object[] { service, event.name(), cutoff });
		} else if (event != null) {
			return query(sql, new Object[] { service, event.name() });
		} else if (useCutoff) {
			return query(sql, new Object[] { service, cutoff });
		}
		return query(sql, new Object[] { service });
	}

	/**
	 * Gets vote log entries for a specific player.
	 *
	 * @param playerName player name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByPlayerName(String playerName, int days, int limit) {
		return getByPlayerName(playerName, null, days, limit);
	}

	/**
	 * Gets vote log entries for a specific player, votes only.
	 *
	 * @param playerName player name to filter by
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByPlayerNameVotesOnly(String playerName, int days, int limit) {
		return getByPlayerName(playerName, VoteLogEvent.VOTE_RECEIVED, days, limit);
	}

	/**
	 * Gets vote log entries for a specific player with event filtering.
	 *
	 * @param playerName player name to filter by
	 * @param event event type to filter by (null for all)
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of vote log entries
	 */
	public List<VoteLogEntry> getByPlayerName(String playerName, VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, server, event, context, status, cached_total "
				+ "FROM " + qi(getTableName()) + " WHERE player_name=? " + (event != null ? "AND event=? " : "")
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (event != null && useCutoff) {
			return query(sql, new Object[] { playerName, event.name(), cutoff });
		} else if (event != null) {
			return query(sql, new Object[] { playerName, event.name() });
		} else if (useCutoff) {
			return query(sql, new Object[] { playerName, cutoff });
		}
		return query(sql, new Object[] { playerName });
	}

	/**
	 * Gets vote counts (total, immediate, cached).
	 *
	 * @param days number of days to look back (0 for all)
	 * @return vote log counts
	 */
	public VoteLogCounts getCounts(int days) {
		return getCounts(days, VoteLogEvent.VOTE_RECEIVED);
	}

	/**
	 * Gets vote counts with event filtering.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param eventFilter event type to filter by
	 * @return vote log counts
	 */
	public VoteLogCounts getCounts(int days, VoteLogEvent eventFilter) {
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT " + "COUNT(*) AS total, "
				+ "SUM(CASE WHEN status='IMMEDIATE' THEN 1 ELSE 0 END) AS immediate, "
				+ "SUM(CASE WHEN status='CACHED' THEN 1 ELSE 0 END) AS cached " + "FROM " + qi(getTableName())
				+ " WHERE 1=1 " + (eventFilter != null ? "AND event=? " : "") + (useCutoff ? "AND vote_time >= ? " : "")
				+ ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			int idx = 1;
			if (eventFilter != null) {
				ps.setString(idx++, eventFilter.name());
			}
			if (useCutoff) {
				ps.setLong(idx++, cutoff);
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
			debug(e);
		}

		return new VoteLogCounts(0, 0, 0);
	}

	/**
	 * Gets the number of unique voters.
	 *
	 * @param days number of days to look back (0 for all)
	 * @return count of unique voters
	 */
	public long getUniqueVoters(int days) {
		return getUniqueVoters(days, VoteLogEvent.VOTE_RECEIVED);
	}

	/**
	 * Gets the number of unique voters with event filtering.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param eventFilter event type to filter by
	 * @return count of unique voters
	 */
	public long getUniqueVoters(int days, VoteLogEvent eventFilter) {
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT COUNT(DISTINCT player_uuid) AS uniques " + "FROM " + qi(getTableName()) + " WHERE 1=1 "
				+ (eventFilter != null ? "AND event=? " : "") + (useCutoff ? "AND vote_time >= ? " : "") + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			int idx = 1;
			if (eventFilter != null) {
				ps.setString(idx++, eventFilter.name());
			}
			if (useCutoff) {
				ps.setLong(idx++, cutoff);
			}

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					return rs.getLong("uniques");
				}
			}
		} catch (SQLException e) {
			debug(e);
		}
		return 0;
	}

	/**
	 * Gets top services by vote count.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @return list of service counts
	 */
	public List<ServiceCount> getTopServices(int days, int limit) {
		return getTopServices(days, limit, VoteLogEvent.VOTE_RECEIVED);
	}

	/**
	 * Gets top services by vote count with event filtering.
	 *
	 * @param days number of days to look back (0 for all)
	 * @param limit maximum number of results
	 * @param eventFilter event type to filter by
	 * @return list of service counts
	 */
	public List<ServiceCount> getTopServices(int days, int limit, VoteLogEvent eventFilter) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT service, COUNT(*) AS votes " + "FROM " + qi(getTableName()) + " WHERE 1=1 "
				+ (eventFilter != null ? "AND event=? " : "") + (useCutoff ? "AND vote_time >= ? " : "")
				+ "GROUP BY service " + "ORDER BY votes DESC " + "LIMIT " + limit + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			int idx = 1;
			if (eventFilter != null) {
				ps.setString(idx++, eventFilter.name());
			}
			if (useCutoff) {
				ps.setLong(idx++, cutoff);
			}

			ResultSet rs = ps.executeQuery();
			List<ServiceCount> out = Collections.synchronizedList(new java.util.ArrayList<>());
			while (rs.next()) {
				out.add(new ServiceCount(rs.getString("service"), rs.getLong("votes")));
			}
			rs.close();
			return out;
		} catch (SQLException e) {
			debug(e);
			return java.util.Collections.emptyList();
		}
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
						rs.getString("player_name"), rs.getString("service"), rs.getString("server"),
						rs.getString("event"), rs.getString("context"), rs.getString("status"),
						rs.getInt("cached_total")));
			}
			rs.close();
			return out;
		} catch (SQLException e) {
			debug(e);
			return java.util.Collections.emptyList();
		}
	}

	// ---- DTOs ----

	/**
	 * Represents a single vote log entry from the database.
	 */
	public static class VoteLogEntry {
		/**
		 * Vote ID (correlation id).
		 */
		public final String voteId;
		/**
		 * Vote time in milliseconds.
		 */
		public final long voteTime;
		/**
		 * Player UUID as string.
		 */
		public final String playerUuid;
		/**
		 * Player name.
		 */
		public final String playerName;
		/**
		 * Service name.
		 */
		public final String service;
		/**
		 * Server name.
		 */
		public final String server;
		/**
		 * Event type name.
		 */
		public final String event;
		/**
		 * Event context string.
		 */
		public final String context;
		/**
		 * Processing status.
		 */
		public final String status;
		/**
		 * Cached total from proxy.
		 */
		public final int proxyCachedTotal;

		/**
		 * Constructs a new VoteLogEntry.
		 *
		 * @param voteId vote ID
		 * @param voteTime vote time in milliseconds
		 * @param playerUuid player UUID as string
		 * @param playerName player name
		 * @param service service name
		 * @param server server name
		 * @param event event type name
		 * @param context event context
		 * @param status processing status
		 * @param proxyCachedTotal cached total from proxy
		 */
		public VoteLogEntry(String voteId, long voteTime, String playerUuid, String playerName, String service,
				String server, String event, String context, String status, int proxyCachedTotal) {
			this.voteId = voteId;
			this.voteTime = voteTime;
			this.playerUuid = playerUuid;
			this.playerName = playerName;
			this.service = service;
			this.server = server;
			this.event = event;
			this.context = context;
			this.status = status;
			this.proxyCachedTotal = proxyCachedTotal;
		}
	}

	/**
	 * Holds vote count statistics.
	 */
	public static class VoteLogCounts {
		/**
		 * Total vote count.
		 */
		public final long total;
		/**
		 * Immediate vote count.
		 */
		public final long immediate;
		/**
		 * Cached vote count.
		 */
		public final long cached;

		/**
		 * Constructs a new VoteLogCounts.
		 *
		 * @param total total count
		 * @param immediate immediate count
		 * @param cached cached count
		 */
		public VoteLogCounts(long total, long immediate, long cached) {
			this.total = total;
			this.immediate = immediate;
			this.cached = cached;
		}
	}

	/**
	 * Holds service name and vote count.
	 */
	public static class ServiceCount {
		/**
		 * Service name.
		 */
		public final String service;
		/**
		 * Vote count for this service.
		 */
		public final long votes;

		/**
		 * Constructs a new ServiceCount.
		 *
		 * @param service service name
		 * @param votes vote count
		 */
		public ServiceCount(String service, long votes) {
			this.service = service;
			this.votes = votes;
		}
	}
}
