package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/**
 * Table for caching timed votes in the proxy.
 */
public abstract class ProxyTimedVoteCacheTable extends AbstractSqlTable {

	@Override
	public String getPrimaryKeyColumn() {
		return "id";
	}

	@Override
	public String buildCreateTableSql(DbType dbType) {
		if (dbType == DbType.POSTGRESQL) {
			return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " ("
					+ qi("id") + " BIGSERIAL PRIMARY KEY, "
					+ qi("playerName") + " VARCHAR(100), "
					+ qi("service") + " VARCHAR(100), "
					+ qi("time") + " BIGINT, "
					+ qi("voteId") + " VARCHAR(36), "
					+ qi("proxyBroadcastHandled") + " BOOLEAN NOT NULL DEFAULT FALSE, "
					+ qi("broadcastTargets") + " TEXT, "
					+ qi("broadcastForwardedServers") + " TEXT"
					+ ");";
		}

		return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " ("
				+ qi("id") + " INT AUTO_INCREMENT PRIMARY KEY,"
				+ qi("playerName") + " VARCHAR(100),"
				+ qi("service") + " VARCHAR(100),"
				+ qi("time") + " BIGINT,"
				+ qi("voteId") + " VARCHAR(36),"
				+ qi("proxyBroadcastHandled") + " TINYINT(1) NOT NULL DEFAULT 0,"
				+ qi("broadcastTargets") + " TEXT,"
				+ qi("broadcastForwardedServers") + " TEXT,"
				+ "INDEX idx_time (" + qi("time") + ")"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
	}

	@Override
	public abstract void logSevere(String msg);

	@Override
	public abstract void logInfo(String msg);

	@Override
	public abstract void debug(Throwable t);

	/**
	 * Constructor using an existing MySQL connection.
	 * @param existingMysql the existing MySQL instance
	 * @param tablePrefix the table prefix
	 * @param debug whether debug mode is enabled
	 */
	public ProxyTimedVoteCacheTable(MySQL existingMysql, String tablePrefix, boolean debug) {
		super((tablePrefix != null ? tablePrefix : "") + "votingplugin_timedvotecache",
				existingMysql,
				debug);
		ensureVoteIdColumn();
		ensureProxyBroadcastColumns();
		ensureIndexes();
	}

	/**
	 * Constructor using a MySQL configuration.
	 * @param config the MySQL configuration
	 * @param debug whether debug mode is enabled
	 */
	public ProxyTimedVoteCacheTable(MysqlConfig config, boolean debug) {
		super("votingplugin_timedvotecache", config, debug);
		ensureVoteIdColumn();
		ensureProxyBroadcastColumns();
		ensureIndexes();
	}

	private void ensureVoteIdColumn() {
		ensureColumn("voteId", "VARCHAR(36)");
	}

	private void ensureProxyBroadcastColumns() {
		ensureColumn("proxyBroadcastHandled", getDbType() == DbType.POSTGRESQL
				? "BOOLEAN NOT NULL DEFAULT FALSE"
				: "TINYINT(1) NOT NULL DEFAULT 0");
		ensureColumn("broadcastTargets", "TEXT");
		ensureColumn("broadcastForwardedServers", "TEXT");
	}

	private void ensureColumn(String column, String type) {
		String probeSql = "SELECT " + qi(column) + " FROM " + qi(getTableName()) + " WHERE 1 = 0;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(probeSql)) {
			ps.executeQuery();
			return;
		} catch (SQLException ignored) {
			// Column does not exist yet.
		}

		String alter = "ALTER TABLE " + qi(getTableName()) + " ADD COLUMN " + qi(column) + " " + type + ";";
		try (Connection conn = mysql.getConnectionManager().getConnection(); Statement st = conn.createStatement()) {
			st.executeUpdate(alter);
		} catch (SQLException e) {
			debug(e);
		}
	}

	private void ensureIndexes() {
		if (getDbType() == DbType.POSTGRESQL) {
			try {
				new Query(mysql, "CREATE INDEX IF NOT EXISTS idx_time ON " + qi(getTableName()) + " (" + qi("time")
						+ ");").executeUpdate();
			} catch (SQLException e) {
				debug(e);
			}
		}
	}

	// --- INSERT ---
	/**
	 * Inserts a timed vote.
	 * @param voteId unique vote identifier
	 * @param playerName the player name
	 * @param service the voting service
	 * @param time the vote time
	 * @param proxyBroadcastHandled whether standalone proxy forwarding was handled
	 * @param broadcastTargets encoded original broadcast targets
	 * @param broadcastForwardedServers encoded servers that received the standalone broadcast
	 * @return true when the row was inserted
	 */
	public boolean insertTimedVote(UUID voteId, String playerName, String service, long time,
			boolean proxyBroadcastHandled, String broadcastTargets, String broadcastForwardedServers) {
		String sql = "INSERT INTO " + qi(getTableName()) + " (" + qi("playerName") + ", " + qi("service") + ", "
				+ qi("time") + ", " + qi("voteId") + ", " + qi("proxyBroadcastHandled") + ", "
				+ qi("broadcastTargets") + ", " + qi("broadcastForwardedServers")
				+ ") VALUES (?, ?, ?, ?, ?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, playerName);
			ps.setString(2, service);
			ps.setLong(3, time);
			ps.setString(4, voteId == null ? null : voteId.toString());
			if (getDbType() == DbType.POSTGRESQL) {
				ps.setBoolean(5, proxyBroadcastHandled);
			} else {
				ps.setInt(5, proxyBroadcastHandled ? 1 : 0);
			}
			ps.setString(6, broadcastTargets);
			ps.setString(7, broadcastForwardedServers);
			ps.executeUpdate();
			return true;
		} catch (SQLException e) {
			debug(e);
			return false;
		}
	}

	/**
	 * Updates a queued vote's standalone broadcast delivery state.
	 *
	 * @param vote queued vote with current delivery state
	 */
	public void updateTimedVote(VoteTimeQueue vote) {
		boolean hasVoteId = vote.getVoteId() != null;
		String sql = "UPDATE " + qi(getTableName()) + " SET " + qi("proxyBroadcastHandled") + " = ?, "
				+ qi("broadcastTargets") + " = ?, " + qi("broadcastForwardedServers") + " = ? WHERE "
				+ (hasVoteId ? qi("voteId") + " = ?;"
						: qi("playerName") + " = ? AND " + qi("service") + " = ? AND " + qi("time") + " = ?;");
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			if (getDbType() == DbType.POSTGRESQL) {
				ps.setBoolean(1, vote.isProxyBroadcastHandled());
			} else {
				ps.setInt(1, vote.isProxyBroadcastHandled() ? 1 : 0);
			}
			ps.setString(2, vote.encodeBroadcastTargets());
			ps.setString(3, vote.encodeBroadcastForwardedServers());
			if (hasVoteId) {
				ps.setString(4, vote.getVoteId().toString());
			} else {
				ps.setString(4, vote.getName());
				ps.setString(5, vote.getService());
				ps.setLong(6, vote.getTime());
			}
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	// --- GET ---
	/**
	 * Gets all timed votes.
	 * @return list of all timed vote rows
	 */
	public List<TimedVoteRow> getAllVotes() {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + ";", null);
	}

	/**
	 * Gets expired votes before a given time.
	 * @param now the current time in milliseconds
	 * @return list of expired vote rows
	 */
	public List<TimedVoteRow> getExpiredVotes(long now) {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("time") + " <= ?;",
				new Object[] { now });
	}

	// --- DELETE ---
	/**
	 * Removes a vote by its ID.
	 * @param id the vote ID
	 */
	public void removeVoteById(int id) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("id") + " = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setInt(1, id);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Removes one processed queued vote by vote ID, with a legacy tuple fallback.
	 *
	 * @param vote processed queued vote
	 */
	public void removeVote(VoteTimeQueue vote) {
		boolean hasVoteId = vote.getVoteId() != null;
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE "
				+ (hasVoteId ? qi("voteId") + " = ?;"
						: qi("playerName") + " = ? AND " + qi("service") + " = ? AND " + qi("time") + " = ?;");
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			if (hasVoteId) {
				ps.setString(1, vote.getVoteId().toString());
			} else {
				ps.setString(1, vote.getName());
				ps.setString(2, vote.getService());
				ps.setLong(3, vote.getTime());
			}
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Removes expired votes before a given time.
	 * @param now the current time in milliseconds
	 */
	public void removeExpiredVotes(long now) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("time") + " <= ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setLong(1, now);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Clears all votes from the table.
	 */
	public void clearTable() {
		try {
			if (getDbType() == DbType.POSTGRESQL) {
				new Query(mysql, "TRUNCATE TABLE " + qi(getTableName()) + " RESTART IDENTITY;").executeUpdate();
			} else {
				new Query(mysql, "TRUNCATE TABLE " + qi(getTableName()) + ";").executeUpdate();
			}
		} catch (SQLException e) {
			debug(e);
		}
	}

	private List<TimedVoteRow> selectVotes(String sql, Object[] params) {
		List<TimedVoteRow> list = new ArrayList<>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					ps.setObject(i + 1, params[i]);
				}
			}

			try (ResultSet rs = ps.executeQuery()) {
				while (rs.next()) {
					list.add(new TimedVoteRow(
							rs.getInt("id"),
							rs.getString("playerName"),
							rs.getString("service"),
							rs.getLong("time"),
							parseUuid(rs.getString("voteId")),
							rs.getBoolean("proxyBroadcastHandled"),
							rs.getString("broadcastTargets"),
							rs.getString("broadcastForwardedServers")
					));
				}
			}
		} catch (SQLException e) {
			debug(e);
		}
		return list;
	}

	private UUID parseUuid(String value) {
		if (value == null || value.isEmpty()) {
			return null;
		}
		try {
			return UUID.fromString(value);
		} catch (IllegalArgumentException ignored) {
			return null;
		}
	}

	/**
	 * Represents a row in the timed vote cache table.
	 */
	public static class TimedVoteRow {
		private final int id;
		private final String playerName;
		private final String service;
		private final long time;
		private final UUID voteId;
		private final boolean proxyBroadcastHandled;
		private final String broadcastTargets;
		private final String broadcastForwardedServers;

		/**
		 * Constructor for TimedVoteRow.
		 * @param id the row ID
		 * @param playerName the player name
		 * @param service the voting service
		 * @param time the vote time
		 * @param voteId unique vote identifier
		 * @param proxyBroadcastHandled whether standalone proxy forwarding was handled
		 * @param broadcastTargets encoded original broadcast targets
		 * @param broadcastForwardedServers encoded servers that received the standalone broadcast
		 */
		public TimedVoteRow(int id, String playerName, String service, long time, UUID voteId,
				boolean proxyBroadcastHandled, String broadcastTargets, String broadcastForwardedServers) {
			this.id = id;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
			this.voteId = voteId;
			this.proxyBroadcastHandled = proxyBroadcastHandled;
			this.broadcastTargets = broadcastTargets;
			this.broadcastForwardedServers = broadcastForwardedServers;
		}

		/**
		 * Gets the row ID.
		 * @return the ID
		 */
		public int getId() {
			return id;
		}

		/**
		 * Gets the player name.
		 * @return the player name
		 */
		public String getPlayerName() {
			return playerName;
		}

		/**
		 * Gets the voting service.
		 * @return the service name
		 */
		public String getService() {
			return service;
		}

		/**
		 * Gets the vote time.
		 * @return the time in milliseconds
		 */
		public long getTime() {
			return time;
		}

		/**
		 * Gets the vote identifier.
		 *
		 * @return vote identifier or null
		 */
		public UUID getVoteId() {
			return voteId;
		}

		/**
		 * Checks whether standalone proxy forwarding was handled.
		 * @return true when forwarding was handled before queueing
		 */
		public boolean isProxyBroadcastHandled() {
			return proxyBroadcastHandled;
		}

		public String getBroadcastTargets() {
			return broadcastTargets;
		}

		/**
		 * Gets the encoded servers that received the standalone broadcast.
		 * @return encoded server set
		 */
		public String getBroadcastForwardedServers() {
			return broadcastForwardedServers;
		}
	}
}
