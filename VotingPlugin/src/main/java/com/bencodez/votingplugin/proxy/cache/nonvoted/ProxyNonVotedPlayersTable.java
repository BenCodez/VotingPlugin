package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

import lombok.Getter;

/**
 * Table for tracking non-voted players in the proxy cache.
 */
public abstract class ProxyNonVotedPlayersTable {

	@Getter
	private final MySQL mysql;
	private final String tableName;

	/**
	 * Logs a severe message.
	 * @param msg the message to log
	 */
	public abstract void logSevere(String msg);

	/**
	 * Logs an info message.
	 * @param msg the message to log
	 */
	public abstract void logInfo(String msg);

	/**
	 * Logs debug information for an exception.
	 * @param e the exception to debug
	 */
	public abstract void debug(Exception e);

	/**
	 * Constructor using an existing MySQL connection.
	 * @param existingMysql the existing MySQL instance
	 * @param tablePrefix the table prefix
	 * @param debug whether debug mode is enabled
	 */
	public ProxyNonVotedPlayersTable(MySQL existingMysql, String tablePrefix, boolean debug) {
		this.mysql = existingMysql;
		this.tableName = (tablePrefix != null ? tablePrefix : "") + "votingplugin_nonvotedplayers";
		createTableIfNeeded();
	}

	/**
	 * Constructor using a MySQL configuration.
	 * @param config the MySQL configuration
	 * @param debug whether debug mode is enabled
	 */
	public ProxyNonVotedPlayersTable(MysqlConfig config, boolean debug) {
		String prefix = config.getTablePrefix() != null ? config.getTablePrefix() : "";
		this.tableName = prefix + "votingplugin_nonvotedplayers";

		this.mysql = new MySQL(config.getMaxThreads()) {
			@Override
			public void debug(SQLException e) {
				if (debug) {
					ProxyNonVotedPlayersTable.this.debug(e);
				}
			}

			@Override
			public void severe(String msg) {
				logSevere(msg);
			}

			@Override
			public void debug(String msg) {
				if (debug) {
					logInfo("MYSQL DEBUG: " + msg);
				}
			}
		};

		if (!mysql.connect(config)) {
			logSevere("Failed to connect to MySQL for non-voted players cache!");
		}
		try {
			new Query(mysql, "USE `" + config.getDatabase() + "`;").executeUpdate();
		} catch (SQLException e) {
			logSevere("Failed to select database: " + config.getDatabase());
			debug(e);
		}

		createTableIfNeeded();
	}

	private void createTableIfNeeded() {
		String sql = "CREATE TABLE IF NOT EXISTS `" + tableName + "` (" + "id INT AUTO_INCREMENT PRIMARY KEY,"
				+ "uuid VARCHAR(36) NOT NULL," + "playerName VARCHAR(100) NOT NULL," + "`lastTime` BIGINT NOT NULL,"
				+ "UNIQUE KEY uniq_playerName (`playerName`)," + "KEY idx_uuid (`uuid`),"
				+ "KEY idx_lastTime (`lastTime`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";

		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Gets the table name.
	 * @return the table name
	 */
	public String getTableName() {
		return tableName;
	}

	/**
	 * Upsert player record.
	 * @param uuid the player UUID
	 * @param playerName the player name
	 * @param lastTime the last time
	 */
	public void upsertPlayer(String uuid, String playerName, long lastTime) {
		String sql = "INSERT INTO `" + tableName + "` (uuid, playerName, lastTime) " + "VALUES (?, ?, ?) "
				+ "ON DUPLICATE KEY UPDATE uuid = VALUES(uuid), lastTime = VALUES(lastTime);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, uuid);
			ps.setString(2, playerName);
			ps.setLong(3, lastTime);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Get UUID by player name.
	 * @param playerName the player name
	 * @return the UUID or empty string
	 */
	public String getUuidByPlayerName(String playerName) {
		String sql = "SELECT uuid FROM `" + tableName + "` WHERE playerName = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, playerName);
			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					String uuid = rs.getString("uuid");
					return uuid != null ? uuid : "";
				}
			}
		} catch (SQLException e) {
			debug(e);
		}
		return "";
	}

	/**
	 * Remove player by name.
	 * @param playerName the player name
	 */
	public void removeByPlayerName(String playerName) {
		String sql = "DELETE FROM `" + tableName + "` WHERE playerName = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, playerName);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Gets all rows from the table.
	 * @return list of all non-voted player rows
	 */
	public List<NonVotedPlayerRow> getAllRows() {
		List<NonVotedPlayerRow> list = new ArrayList<>();
		String sql = "SELECT id, uuid, playerName, lastTime FROM `" + tableName + "`;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql);
				ResultSet rs = ps.executeQuery()) {
			while (rs.next()) {
				list.add(new NonVotedPlayerRow(rs.getInt("id"), rs.getString("uuid"), rs.getString("playerName"),
						rs.getLong("lastTime")));
			}
		} catch (SQLException e) {
			debug(e);
		}
		return list;
	}

	/**
	 * Clears all rows from the table.
	 */
	public void clearAll() {
		try {
			new Query(mysql, "TRUNCATE TABLE `" + tableName + "`;").executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	/**
	 * Represents a row in the non-voted players table.
	 */
	public static class NonVotedPlayerRow {
		private final int id;
		private final String uuid;
		private final String playerName;
		private final long lastTime;

		/**
		 * Constructor for NonVotedPlayerRow.
		 * @param id the row ID
		 * @param uuid the player UUID
		 * @param playerName the player name
		 * @param lastTime the last time
		 */
		public NonVotedPlayerRow(int id, String uuid, String playerName, long lastTime) {
			this.id = id;
			this.uuid = uuid;
			this.playerName = playerName;
			this.lastTime = lastTime;
		}

		/**
		 * Gets the row ID.
		 * @return the ID
		 */
		public int getId() {
			return id;
		}

		/**
		 * Gets the player UUID.
		 * @return the UUID
		 */
		public String getUuid() {
			return uuid;
		}

		/**
		 * Gets the player name.
		 * @return the player name
		 */
		public String getPlayerName() {
			return playerName;
		}

		/**
		 * Gets the last time.
		 * @return the last time
		 */
		public long getLastTime() {
			return lastTime;
		}
	}
}
