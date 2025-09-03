package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

/**
 * Timed Vote Cache table (holds votes that become valid at/after expireTime).
 * Table name: <prefix>timedvotecache
 */
public abstract class ProxyTimedVoteCacheTable {

	protected com.bencodez.simpleapi.sql.mysql.MySQL mysql;
	private final String tableName;

	public abstract void logSevere(String msg);

	public abstract void logInfo(String msg);

	public abstract void debug(Exception e);

	public ProxyTimedVoteCacheTable(com.bencodez.simpleapi.sql.mysql.MySQL existingMysql, String tablePrefix,
			boolean debug) {
		this.tableName = (tablePrefix != null ? tablePrefix : "") + "votingplugin_timedvotecache";

		this.mysql = existingMysql;

		// Create table if not exists
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "playerName VARCHAR(100)," + "service VARCHAR(100)," + "time BIGINT," + "INDEX idx_time (time)"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public ProxyTimedVoteCacheTable(MysqlConfig config, boolean debug) {
		String prefix = config.getTablePrefix() != null ? config.getTablePrefix() : "";
		this.tableName = prefix + "votingplugin_timedvotecache";

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
			logSevere("Failed to connect to MySQL for timed vote cache!");
		}
		try {
			new Query(mysql, "USE `" + config.getDatabase() + "`;").executeUpdate();
		} catch (SQLException e) {
			logSevere("Failed to select database: " + config.getDatabase());
			debug(e);
		}

		// Updated table creation without uuid, server, realVote
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "playerName VARCHAR(100)," + "service VARCHAR(100)," + "time BIGINT," + "INDEX idx_time (time)"
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
	public void insertTimedVote(String playerName, String service, long time) {
		String sql = "INSERT INTO `" + tableName + "` (playerName, service, time) VALUES (?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, playerName);
			ps.setString(2, service);
			ps.setLong(3, time);
			ps.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	// --- GET ---
	public List<TimedVoteRow> getAllVotes() {
		String sql = "SELECT * FROM `" + tableName + "`;";
		return selectVotes(sql, null);
	}

	public List<TimedVoteRow> getExpiredVotes(long now) {
		String sql = "SELECT * FROM `" + tableName + "` WHERE time <= ?;";
		return selectVotes(sql, new Object[] { now });
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

	public void removeExpiredVotes(long now) {
		String sql = "DELETE FROM `" + tableName + "` WHERE time <= ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setLong(1, now);
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
					TimedVoteRow v = new TimedVoteRow(rs.getInt("id"), rs.getString("playerName"),
							rs.getString("service"), rs.getLong("time"));
					list.add(v);
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return list;
	}

	public static class TimedVoteRow {
		private final int id;
		private final String playerName;
		private final String service;
		private final long time;

		public TimedVoteRow(int id, String playerName, String service, long time) {
			this.id = id;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
		}

		public int getId() {
			return id;
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
	}
}
