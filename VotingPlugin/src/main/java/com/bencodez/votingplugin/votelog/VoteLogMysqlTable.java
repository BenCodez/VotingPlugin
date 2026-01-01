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
 * Records the "main reward triggers" (not every command executed).
 *
 * Columns: - id (auto increment primary key) - vote_id (correlation id / vote
 * uuid; NOT UNIQUE; may repeat for other events) - vote_time (LONG millis,
 * explicit time event happened) - event (VOTE_RECEIVED default; includes
 * reward-trigger events) - context (which reward/config fired + any extra info
 * like TimeType, ex: "TopVoter:MONTH") - status (IMMEDIATE/CACHED) -
 * cached_total (int, proxy only snapshot; can be 0 on backend) - service -
 * player_uuid - player_name
 */
public abstract class VoteLogMysqlTable {

	public enum VoteLogEvent {
		// Core
		VOTE_RECEIVED,

		// Reward triggers
		ALL_SITES_REWARD, ALMOST_ALL_SITES_REWARD, FIRST_VOTE_REWARD, FIRST_VOTE_TODAY_REWARD, CUMULATIVE_REWARD,
		MILESTONE_REWARD, VOTE_STREAK_REWARD, TOP_VOTER_REWARD,

		// VoteShop
		VOTESHOP_PURCHASE
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

	private void createTable() {
		String sql = "CREATE TABLE IF NOT EXISTS `" + getName() + "` (" + "`id` BIGINT NOT NULL AUTO_INCREMENT,"
				+ "`vote_id` VARCHAR(36) NULL," + "PRIMARY KEY (`id`)" + ");";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			debug1(e);
		}
	}

	private void ensureColumns() {
		synchronized (initLock) {

			ensureIdPrimaryKey();

			checkColumn("vote_id", DataType.STRING);
			alterColumnType("vote_id", "VARCHAR(36) NULL");

			checkColumn("vote_time", DataType.INTEGER);
			alterColumnType("vote_time", "BIGINT NOT NULL DEFAULT '0'");

			checkColumn("player_uuid", DataType.STRING);
			alterColumnType("player_uuid", "VARCHAR(37)");

			checkColumn("player_name", DataType.STRING);
			alterColumnType("player_name", "VARCHAR(16)");

			checkColumn("service", DataType.STRING);
			alterColumnType("service", "VARCHAR(64)");

			checkColumn("event", DataType.STRING);
			alterColumnType("event", "VARCHAR(64) NOT NULL DEFAULT '" + VoteLogEvent.VOTE_RECEIVED.name() + "'");

			checkColumn("context", DataType.STRING);
			// CHANGED: allow more space for voteshop purchase metadata
			alterColumnType("context", "VARCHAR(255) NULL");

			checkColumn("status", DataType.STRING);
			alterColumnType("status", "VARCHAR(16)");

			checkColumn("cached_total", DataType.INTEGER);
			alterColumnType("cached_total", "INT NOT NULL DEFAULT '0'");

			tryCreateIndex("idx_vote_time", "(`vote_time`)");
			tryCreateIndex("idx_uuid_time", "(`player_uuid`,`vote_time`)");
			tryCreateIndex("idx_service_time", "(`service`,`vote_time`)");
			tryCreateIndex("idx_status_time", "(`status`,`vote_time`)");
			tryCreateIndex("idx_event_time", "(`event`,`vote_time`)");
			tryCreateIndex("idx_context_time", "(`context`,`vote_time`)");
			tryCreateIndex("idx_event_context_time", "(`event`,`context`,`vote_time`)");

			tryCreateIndex("idx_vote_id", "(`vote_id`)");
			tryCreateIndex("idx_vote_id_time", "(`vote_id`,`vote_time`)");
		}
	}

	public List<String> getDistinctServices(int days, int limit) {
		if (limit <= 0) {
			limit = 45;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT DISTINCT service FROM `" + getName() + "` WHERE service IS NOT NULL AND service != '' "
				+ (useCutoff ? "AND vote_time >= ? " : "") + "ORDER BY service ASC LIMIT " + limit + ";";

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
			debug1(e);
			return java.util.Collections.emptyList();
		}
	}

	public List<VoteLogEntry> getRecentAll(int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` " + (useCutoff ? "WHERE vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { cutoff });
		}
		return query(sql, new Object[] {});
	}

	public List<VoteLogEntry> getRecentByEvent(VoteLogEvent event, int days, int limit) {
		if (event == null) {
			return (days > 0) ? getRecentAll(days, limit) : getRecent(limit);
		}
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE event=? " + (useCutoff ? "AND vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { event.name(), cutoff });
		}
		return query(sql, new Object[] { event.name() });
	}

	public List<VoteLogEntry> getRecent(int days, VoteLogEvent eventFilter, int limit) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE 1=1 " + (eventFilter != null ? "AND event=? " : "")
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

	private void ensureIdPrimaryKey() {
		try {
			boolean hasId = hasColumnQuery("id");
			boolean pkIsId = hasPrimaryKeyOn("id");

			if (hasId && pkIsId) {
				dropUniqueVoteIdIfPresent();
				return;
			}

			if (!hasId) {
				String migrate = "ALTER TABLE `" + getName() + "` " + "DROP PRIMARY KEY, "
						+ "ADD COLUMN `id` BIGINT NOT NULL AUTO_INCREMENT FIRST, " + "ADD PRIMARY KEY (`id`);";
				new Query(mysql, migrate).executeUpdate();
			} else {
				String migrate2 = "ALTER TABLE `" + getName() + "` " + "DROP PRIMARY KEY, " + "ADD PRIMARY KEY (`id`);";
				new Query(mysql, migrate2).executeUpdate();
			}

			dropUniqueVoteIdIfPresent();

		} catch (SQLException e) {
			debug1(e);
		}
	}

	private void dropUniqueVoteIdIfPresent() {
		try {
			dropIndexIfExists("uk_vote_id");
			String idx = findUniqueIndexOnColumn("vote_id");
			if (idx != null && !idx.isEmpty() && !"PRIMARY".equalsIgnoreCase(idx)) {
				dropIndexIfExists(idx);
			}
		} catch (Exception e) {
			// ignore
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
			return true;
		}
	}

	private static boolean isDuplicateIndexName(SQLException e) {
		return e != null && e.getErrorCode() == 1061; // Duplicate key name
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

	private static String safeStr(String s) {
		return s == null ? "" : s;
	}

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

	// ---- VoteShop helpers ----

	private static String limit(String s, int max) {
		if (s == null) {
			return "";
		}
		if (s.length() <= max) {
			return s;
		}
		return s.substring(0, max);
	}

	/**
	 * Context format (stable-ish, parseable): VShop:<identifier>:cost=<cost>
	 *
	 * Example: VShop:daily_reward:cost=50
	 */
	public static String voteShopContext(String identifier, int cost) {

		identifier = safeStr(identifier);

		StringBuilder sb = new StringBuilder();
		sb.append("VShop:").append(identifier.replace(":", "_")).append(":cost=").append(cost);

		return limit(sb.toString(), 240);
	}

	public void logVoteShopPurchase(String playerUuid, String playerName, long timeMillis, String identifier,
			int cost) {

		String ctx = voteShopContext(identifier, cost);

		logEvent(null, VoteLogEvent.VOTESHOP_PURCHASE, ctx, VoteLogStatus.IMMEDIATE, "", playerUuid, playerName,
				timeMillis, 0);
	}

	public String logVote(UUID voteUUID, VoteLogStatus status, String service, String playerUuid, String playerName,
			long voteTimeMillis, int proxyCachedTotal) {

		return logEvent(voteUUID, VoteLogEvent.VOTE_RECEIVED, null, status, service, playerUuid, playerName,
				voteTimeMillis, proxyCachedTotal);
	}

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

		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO `").append(getName()).append("` SET ");

		if (voteId != null) {
			sb.append("`vote_id`='").append(voteId).append("', ");
		} else {
			sb.append("`vote_id`=NULL, ");
		}

		sb.append("`vote_time`='").append(timeMillis).append("', ");
		sb.append("`player_uuid`='").append(playerUuid.toString()).append("', ");
		sb.append("`player_name`='").append(escape(playerName)).append("', ");
		sb.append("`service`='").append(escape(service)).append("', ");
		sb.append("`event`='").append(event.name()).append("', ");

		if (context != null && !context.isEmpty()) {
			sb.append("`context`='").append(escape(context)).append("', ");
		} else {
			sb.append("`context`=NULL, ");
		}

		sb.append("`status`='").append(status.name()).append("', ");
		sb.append("`cached_total`='").append(proxyCachedTotal).append("';");

		try {
			new Query(mysql, sb.toString()).executeUpdateAsync();
		} catch (SQLException e) {
			debug1(e);
		}

		return voteId;
	}

	public String logEvent(UUID voteUUID, VoteLogEvent event, String context, String playerUuid, String playerName,
			long timeMillis) {

		String voteId = voteUUID != null ? voteUUID.toString() : null;

		if (event == null) {
			event = VoteLogEvent.VOTE_RECEIVED;
		}

		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO `").append(getName()).append("` SET ");

		if (voteId != null) {
			sb.append("`vote_id`='").append(voteId).append("', ");
		} else {
			sb.append("`vote_id`=NULL, ");
		}

		sb.append("`vote_time`='").append(timeMillis).append("', ");
		sb.append("`player_uuid`='").append(playerUuid.toString()).append("', ");
		sb.append("`player_name`='").append(escape(playerName)).append("', ");

		// Not needed for these: keep them stable/default
		sb.append("`service`='', ");
		sb.append("`event`='").append(event.name()).append("', ");

		if (context != null && !context.isEmpty()) {
			sb.append("`context`='").append(escape(context)).append("', ");
		} else {
			sb.append("`context`=NULL, ");
		}

		// Not needed for these: keep them stable/default
		sb.append("`status`='', ");
		sb.append("`cached_total`='0';");

		try {
			new Query(mysql, sb.toString()).executeUpdateAsync();
		} catch (SQLException e) {
			debug1(e);
		}

		return voteId;
	}

	public String logEventNow(UUID voteUUID, VoteLogEvent event, String context, String playerUuid, String playerName) {
		return logEvent(voteUUID, event, context, playerUuid, playerName, System.currentTimeMillis());
	}

	public void logAllSitesReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String rewardKey) {
		logEvent(voteUUID, VoteLogEvent.ALL_SITES_REWARD, safeStr(rewardKey), playerUuid, playerName, timeMillis);
	}

	public void logAlmostAllSitesReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String rewardKey) {
		logEvent(voteUUID, VoteLogEvent.ALMOST_ALL_SITES_REWARD, safeStr(rewardKey), playerUuid, playerName,
				timeMillis);
	}

	public void logFirstVoteReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String rewardKey) {
		logEvent(voteUUID, VoteLogEvent.FIRST_VOTE_REWARD, safeStr(rewardKey), playerUuid, playerName, timeMillis);
	}

	public void logFirstVoteTodayReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String rewardKey) {
		logEvent(voteUUID, VoteLogEvent.FIRST_VOTE_TODAY_REWARD, safeStr(rewardKey), playerUuid, playerName,
				timeMillis);
	}

	public void logCumulativeReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String cumulativeKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.CUMULATIVE_REWARD, safeStr(cumulativeKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

	/**
	 * Milestone reward: include reset time type if applicable (ex:
	 * "Milestone50:MONTH").
	 */
	public void logMilestoneReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String milestoneKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.MILESTONE_REWARD, safeStr(milestoneKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

	public void logVoteStreakReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String streakKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.VOTE_STREAK_REWARD, safeStr(streakKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

	public void logTopVoterReward(UUID voteUUID, String playerUuid, String playerName, long timeMillis,
			String topVoterKeyWithTimeType) {
		logEvent(voteUUID, VoteLogEvent.TOP_VOTER_REWARD, safeStr(topVoterKeyWithTimeType), playerUuid, playerName,
				timeMillis);
	}

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

	public VoteLogEntry getByVoteId(String voteId) {
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_id=? ORDER BY vote_time DESC, id DESC LIMIT 1;";
		List<VoteLogEntry> rows = query(sql, new Object[] { voteId });
		return rows.isEmpty() ? null : rows.get(0);
	}

	public List<VoteLogEntry> getByVoteIdAll(String voteId, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE vote_id=? " + (useCutoff ? "AND vote_time >= ? " : "")
				+ "ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";

		if (useCutoff) {
			return query(sql, new Object[] { voteId, cutoff });
		}
		return query(sql, new Object[] { voteId });
	}

	public List<VoteLogEntry> getRecent(int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` ORDER BY vote_time DESC, id DESC LIMIT " + limit + ";";
		return query(sql, new Object[] {});
	}

	public List<VoteLogEntry> getByService(String service, int days, int limit) {
		return getByService(service, null, days, limit);
	}

	public List<VoteLogEntry> getByServiceVotesOnly(String service, int days, int limit) {
		return getByService(service, VoteLogEvent.VOTE_RECEIVED, days, limit);
	}

	public List<VoteLogEntry> getByService(String service, VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE service=? " + (event != null ? "AND event=? " : "")
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

	public List<VoteLogEntry> getByPlayerName(String playerName, int days, int limit) {
		return getByPlayerName(playerName, null, days, limit);
	}

	public List<VoteLogEntry> getByPlayerNameVotesOnly(String playerName, int days, int limit) {
		return getByPlayerName(playerName, VoteLogEvent.VOTE_RECEIVED, days, limit);
	}

	public List<VoteLogEntry> getByPlayerName(String playerName, VoteLogEvent event, int days, int limit) {
		if (limit <= 0) {
			limit = 10;
		}
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT vote_id, vote_time, player_uuid, player_name, service, event, context, status, cached_total "
				+ "FROM `" + getName() + "` WHERE player_name=? " + (event != null ? "AND event=? " : "")
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

	public VoteLogCounts getCounts(int days) {
		return getCounts(days, VoteLogEvent.VOTE_RECEIVED);
	}

	public VoteLogCounts getCounts(int days, VoteLogEvent eventFilter) {
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT " + "COUNT(*) AS total, " + "SUM(status='IMMEDIATE') AS immediate, "
				+ "SUM(status='CACHED') AS cached " + "FROM `" + getName() + "` " + "WHERE 1=1 "
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

	public long getUniqueVoters(int days) {
		return getUniqueVoters(days, VoteLogEvent.VOTE_RECEIVED);
	}

	public long getUniqueVoters(int days, VoteLogEvent eventFilter) {
		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT COUNT(DISTINCT player_uuid) AS uniques " + "FROM `" + getName() + "` " + "WHERE 1=1 "
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
			debug1(e);
		}
		return 0;
	}

	public List<ServiceCount> getTopServices(int days, int limit) {
		return getTopServices(days, limit, VoteLogEvent.VOTE_RECEIVED);
	}

	public List<ServiceCount> getTopServices(int days, int limit, VoteLogEvent eventFilter) {
		if (limit <= 0) {
			limit = 10;
		}

		boolean useCutoff = days > 0;
		long cutoff = useCutoff ? System.currentTimeMillis() - (days * 24L * 60L * 60L * 1000L) : 0;

		String sql = "SELECT service, COUNT(*) AS votes " + "FROM `" + getName() + "` " + "WHERE 1=1 "
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
			debug1(e);
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
						rs.getString("player_name"), rs.getString("service"), rs.getString("event"),
						rs.getString("context"), rs.getString("status"), rs.getInt("cached_total")));
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
		public final String context;
		public final String status;
		public final int proxyCachedTotal;

		public VoteLogEntry(String voteId, long voteTime, String playerUuid, String playerName, String service,
				String event, String context, String status, int proxyCachedTotal) {
			this.voteId = voteId;
			this.voteTime = voteTime;
			this.playerUuid = playerUuid;
			this.playerName = playerName;
			this.service = service;
			this.event = event;
			this.context = context;
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
