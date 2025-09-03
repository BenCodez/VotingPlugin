package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

public abstract class ProxyOnlineVoteCacheTable {

	protected com.bencodez.simpleapi.sql.mysql.MySQL mysql;
	private final String tableName;

	public abstract void logSevere(String msg);

	public abstract void logInfo(String msg);

	public abstract void debug(Exception e);

	public ProxyOnlineVoteCacheTable(com.bencodez.simpleapi.sql.mysql.MySQL existingMysql, String tablePrefix,
			boolean debug) {
		this.tableName = (tablePrefix != null ? tablePrefix : "") + "votingplugin_onlinevotecache";

		this.mysql = existingMysql;

		// Create table if not exists
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100)," + "`time` BIGINT,"
				+ "realvote BOOLEAN," + "text LONGTEXT," + "INDEX idx_uuid (`uuid`)," + "INDEX idx_time (`time`)"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
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
				+ "uuid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100)," + "`time` BIGINT,"
				+ "realvote BOOLEAN," + "text LONGTEXT," + "INDEX idx_uuid (`uuid`)," + "INDEX idx_time (`time`)"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public String getTableName() {
		return tableName;
	}

	// --- INSERT ---
	public void insertOnlineVote(String uuid, String playerName, String service, long time, boolean realvote,
			String text) {
		String sql = "INSERT INTO `" + tableName + "` (uuid, playerName, service, `time`, realvote, text) "
				+ "VALUES (?, ?, ?, ?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, uuid);
			ps.setString(2, playerName);
			ps.setString(3, service);
			ps.setLong(4, time);
			ps.setBoolean(5, realvote);
			ps.setString(6, text);
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
					OnlineVoteRow v = new OnlineVoteRow(rs.getInt("id"), rs.getString("uuid"),
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

	public static class OnlineVoteRow {
		private final int id;
		private final String uuid;
		private final String playerName;
		private final String service;
		private final long time;
		private final boolean realvote;
		private final String text;

		public OnlineVoteRow(int id, String uuid, String playerName, String service, long time, boolean realvote,
				String text) {
			this.id = id;
			this.uuid = uuid;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
			this.realvote = realvote;
			this.text = text;
		}

		public int getId() {
			return id;
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
