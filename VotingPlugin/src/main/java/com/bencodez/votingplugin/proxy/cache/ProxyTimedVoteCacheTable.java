package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

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
					+ qi("time") + " BIGINT"
					+ ");";
		}

		return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " ("
				+ qi("id") + " INT AUTO_INCREMENT PRIMARY KEY,"
				+ qi("playerName") + " VARCHAR(100),"
				+ qi("service") + " VARCHAR(100),"
				+ qi("time") + " BIGINT,"
				+ "INDEX idx_time (" + qi("time") + ")"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
	}

	@Override
	public abstract void logSevere(String msg);

	@Override
	public abstract void logInfo(String msg);

	@Override
	public abstract void debug(Throwable t);

	public ProxyTimedVoteCacheTable(MySQL existingMysql, String tablePrefix, boolean debug) {
		super((tablePrefix != null ? tablePrefix : "") + "votingplugin_timedvotecache",
				existingMysql,
				existingMysql.getConnectionManager().getDbType());
		ensureIndexes();
	}

	public ProxyTimedVoteCacheTable(MysqlConfig config, boolean debug) {
		super("votingplugin_timedvotecache", config, debug);
		ensureIndexes();
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
	public void insertTimedVote(String playerName, String service, long time) {
		String sql = "INSERT INTO " + qi(getTableName()) + " (" + qi("playerName") + ", " + qi("service") + ", "
				+ qi("time") + ") VALUES (?, ?, ?);";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, playerName);
			ps.setString(2, service);
			ps.setLong(3, time);
			ps.executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}

	// --- GET ---
	public List<TimedVoteRow> getAllVotes() {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + ";", null);
	}

	public List<TimedVoteRow> getExpiredVotes(long now) {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("time") + " <= ?;",
				new Object[] { now });
	}

	// --- DELETE ---
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
							rs.getLong("time")
					));
				}
			}
		} catch (SQLException e) {
			debug(e);
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
