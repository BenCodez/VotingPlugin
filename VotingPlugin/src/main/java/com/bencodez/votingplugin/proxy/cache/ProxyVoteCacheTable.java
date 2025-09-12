package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

import lombok.Getter;

public abstract class ProxyVoteCacheTable {

	@Getter
	private com.bencodez.simpleapi.sql.mysql.MySQL mysql;
	private final String tableName;

	public abstract void logSevere(String msg);

	public abstract void logInfo(String msg);

	public abstract void debug(Exception e);

	public ProxyVoteCacheTable(com.bencodez.simpleapi.sql.mysql.MySQL existingMysql, String tablePrefix,
			boolean debug) {
		this.tableName = (tablePrefix != null ? tablePrefix : "") + "votingplugin_votecache";

		this.mysql = existingMysql;

		// Create table if not exists
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100)," + "`time` BIGINT,"
				+ "realVote TINYINT(1)," + "`text` TEXT," + "`server` VARCHAR(100)," + "INDEX idx_server (`server`),"
				+ "INDEX idx_uuid (`uuid`)," + "INDEX idx_time (`time`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public ProxyVoteCacheTable(MysqlConfig config, boolean debug) {
		// Always use fixed table name + optional prefix to avoid collisions with other
		// tables
		String prefix = config.getTablePrefix() != null ? config.getTablePrefix() : "";
		this.tableName = prefix + "votingplugin_votecache";

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
			logSevere("Failed to connect to MySQL for vote cache!");
		}
		try {
			new Query(mysql, "USE `" + config.getDatabase() + "`;").executeUpdate();
		} catch (SQLException e) {
			logSevere("Failed to select database: " + config.getDatabase());
			debug(e);
		}

		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36)," + "playerName VARCHAR(100)," + "service VARCHAR(100)," + "`time` BIGINT," // epoch
																												// millis
				+ "realVote TINYINT(1)," // boolean
				+ "`text` TEXT," + "`server` VARCHAR(100)," + "INDEX idx_server (`server`),"
				+ "INDEX idx_uuid (`uuid`)," + "INDEX idx_time (`time`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
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
	public void insertVote(String uuid, String playerName, String service, long time, boolean real, String text,
			String server) {
		String sql = "INSERT INTO `" + tableName + "` (uuid, playerName, service, `time`, realVote, `text`, `server`) "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, uuid);
			ps.setString(2, playerName);
			ps.setString(3, service);
			ps.setLong(4, time);
			ps.setBoolean(5, real);
			ps.setString(6, text);
			ps.setString(7, server);
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	// --- GET ---
	public List<VoteRow> getAllVotes() {
		String sql = "SELECT * FROM `" + tableName + "`;";
		return selectVotes(sql, null);
	}

	public List<VoteRow> getVotesForServer(String server) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE `server` = ?;";
		return selectVotes(sql, new Object[] { server });
	}

	public VoteRow getVoteById(int id) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE id = ?;";
		List<VoteRow> list = selectVotes(sql, new Object[] { id });
		return list.isEmpty() ? null : list.get(0);
	}

	public List<VoteRow> getVotesByUUID(String uuid) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE uuid = ?;";
		return selectVotes(sql, new Object[] { uuid });
	}

	public List<VoteRow> getVotesByPlayerName(String playerName) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE playerName = ?;";
		return selectVotes(sql, new Object[] { playerName });
	}

	public Set<String> getServers() {
		Set<String> servers = new HashSet<String>();
		String sql = "SELECT DISTINCT `server` FROM `" + tableName + "` WHERE `server` IS NOT NULL;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql);
				ResultSet rs = ps.executeQuery()) {
			while (rs.next()) {
				String s = rs.getString(1);
				if (s != null)
					servers.add(s);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return servers;
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

	public void removeVotesByServer(String server) {
		String sql = "DELETE FROM `" + tableName + "` WHERE `server` = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, server);
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

	private List<VoteRow> selectVotes(String sql, Object[] params) {
		List<VoteRow> list = new ArrayList<VoteRow>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					ps.setObject(i + 1, params[i]);
				}
			}
			try (ResultSet rs = ps.executeQuery()) {
				while (rs.next()) {
					VoteRow v = new VoteRow(rs.getInt("id"), rs.getString("uuid"), rs.getString("playerName"),
							rs.getString("service"), rs.getLong("time"), rs.getBoolean("realVote"),
							rs.getString("text"), rs.getString("server"));
					list.add(v);
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return list;
	}

	public static class VoteRow {
		private final int id;
		private final String uuid;
		private final String playerName;
		private final String service;
		private final long time;
		private final boolean realVote;
		private final String text;
		private final String server;

		public VoteRow(int id, String uuid, String playerName, String service, long time, boolean realVote, String text,
				String server) {
			this.id = id;
			this.uuid = uuid;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
			this.realVote = realVote;
			this.text = text;
			this.server = server;
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

		public boolean isRealVote() {
			return realVote;
		}

		public String getText() {
			return text;
		}

		public String getServer() {
			return server;
		}
	}
}
