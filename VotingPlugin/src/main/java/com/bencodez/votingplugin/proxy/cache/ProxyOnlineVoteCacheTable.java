package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;

public abstract class ProxyOnlineVoteCacheTable {

	protected com.bencodez.simpleapi.sql.mysql.MySQL mysql;
	private final String tableName;

	public abstract void logSevere(String msg);

	public abstract void logInfo(String msg);

	public abstract void debug(Exception e);
	
	// Add this method anywhere in the class (e.g., near getTableName()).
	private void addVoteIdColumnIfMissing() {
		// If you can’t assume the configured database, just read the current one from the connection.
		final String checkSql =
				"SELECT 1 " +
				"FROM INFORMATION_SCHEMA.COLUMNS " +
				"WHERE TABLE_SCHEMA = DATABASE() " +
				"  AND TABLE_NAME = ? " +
				"  AND COLUMN_NAME = 'voteid' " +
				"LIMIT 1;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(checkSql)) {

			ps.setString(1, tableName);

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					return; // column exists
				}
			}

			// Column missing -> add it.
			// Put it after uuid for readability; adjust if you want.
			String alterSql = "ALTER TABLE `" + tableName + "` ADD COLUMN `voteid` VARCHAR(36) NULL AFTER `uuid`;";
			new Query(mysql, alterSql).executeUpdate();

			// Optional: add an index if you’ll query by voteid later
			// new Query(mysql, "ALTER TABLE `" + tableName + "` ADD INDEX idx_voteid (`voteid`);").executeUpdate();

		} catch (SQLException e) {
			// Don’t hard-fail startup for a migration; log it.
			logSevere("Failed to add voteid column to " + tableName + ": " + e.getMessage());
			debug(e);
		}
	}


	public ProxyOnlineVoteCacheTable(com.bencodez.simpleapi.sql.mysql.MySQL existingMysql, String tablePrefix,
			boolean debug) {
		this.tableName = (tablePrefix != null ? tablePrefix : "") + "votingplugin_onlinevotecache";

		this.mysql = existingMysql;

		// Create table if not exists
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36)," + "voteid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100),"
				+ "`time` BIGINT," + "realvote BOOLEAN," + "text LONGTEXT," + "INDEX idx_uuid (`uuid`),"
				+ "INDEX idx_time (`time`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
		addVoteIdColumnIfMissing();
	}

	public ProxyOnlineVoteCacheTable(MysqlConfig config, boolean debug) {
		String prefix = config.getTablePrefix() != null ? config.getTablePrefix() : "";
		this.tableName = prefix + "votingplugin_onlinevotecache";

		mysql = new com.bencodez.simpleapi.sql.mysql.MySQL(config.getMaxThreads()) {
			@Override
			public void debug(SQLException e) {
				if (debug)
					e.printStackTrace();
			}

			@Override
			public void severe(String msg) {
				logSevere(msg);
			}

			@Override
			public void debug(String msg) {
				if (debug)
					logInfo("MYSQL DEBUG: " + msg);
			}
		};

		if (!mysql.connect(config)) {
			logSevere("Failed to connect to MySQL for online vote cache!");
		}
		try {
			new Query(mysql, "USE `" + config.getDatabase() + "`;").executeUpdate();
		} catch (SQLException e) {
			logSevere("Failed to select database: " + config.getDatabase());
			debug(e);
		}

		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36)," + "voteid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100),"
				+ "`time` BIGINT," + "realvote BOOLEAN," + "text LONGTEXT," + "INDEX idx_uuid (`uuid`),"
				+ "INDEX idx_time (`time`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
		addVoteIdColumnIfMissing();
	}

	public String getTableName() {
		return tableName;
	}

	// --- INSERT ---
	public void insertOnlineVote(UUID voteId, String uuid, String playerName, String service, long time,
			boolean realvote, String text) {
		String sql = "INSERT INTO `" + tableName + "` (uuid, voteid, playerName, service, `time`, realvote, text) "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, uuid);
			ps.setString(2, voteId.toString());
			ps.setString(3, playerName);
			ps.setString(4, service);
			ps.setLong(5, time);
			ps.setBoolean(6, realvote);
			ps.setString(7, text);
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	// --- GET ---
	public List<OnlineVoteRow> getAllVotes() {
		String sql = "SELECT * FROM `" + tableName + "`;";
		return selectVotes(sql, null);
	}

	public List<OnlineVoteRow> getVotesForUUID(String uuid) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE `uuid` = ?;";
		return selectVotes(sql, new Object[] { uuid });
	}

	// --- DELETE ---
	public void removeVoteById(int id) {
		String sql = "DELETE FROM `" + tableName + "` WHERE id = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setInt(1, id);
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public void removeVotesForUUID(String uuid) {
		String sql = "DELETE FROM `" + tableName + "` WHERE uuid = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, uuid);
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public void clearTable() {
		try {
			new Query(mysql, "TRUNCATE TABLE `" + tableName + "`;").executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public void close() {
		mysql.disconnect();
	}

	private List<OnlineVoteRow> selectVotes(String sql, Object[] params) {
		List<OnlineVoteRow> list = new ArrayList<>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					ps.setObject(i + 1, params[i]);
				}
			}
			try (ResultSet rs = ps.executeQuery()) {
				while (rs.next()) {
					OnlineVoteRow v = new OnlineVoteRow(rs.getInt("id"), rs.getString("voteid"), rs.getString("uuid"),
							rs.getString("playerName"), rs.getString("service"), rs.getLong("time"),
							rs.getBoolean("realvote"), rs.getString("text"));
					list.add(v);
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return list;
	}

	public void removeOnlineVote(OfflineBungeeVote vote) {
		String sql = "DELETE FROM `" + tableName + "` WHERE `uuid` = ? AND `service` = ? AND `time` = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, vote.getUuid());
			ps.setString(2, vote.getService());
			ps.setLong(3, vote.getTime());
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public static class OnlineVoteRow {
		private final int id;
		private final String uuid;
		private final String playerName;
		private final String service;
		private final long time;
		private final boolean realvote;
		private final String text;
		private final String voteId;

		public OnlineVoteRow(int id, String voteId, String uuid, String playerName, String service, long time,
				boolean realvote, String text) {
			this.id = id;
			this.uuid = uuid;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
			this.realvote = realvote;
			this.text = text;
			this.voteId = voteId;
		}

		public int getId() {
			return id;
		}

		public String getVoteId() {
			return voteId;
		}

		public String getUuid() {
			return uuid;
		}

		public String getPlayerName() {
			return playerName;
		}

		public String getService() {
			return service;
		}

		public long getTime() {
			return time;
		}

		public boolean isRealvote() {
			return realvote;
		}

		public String getText() {
			return text;
		}
	}
}
