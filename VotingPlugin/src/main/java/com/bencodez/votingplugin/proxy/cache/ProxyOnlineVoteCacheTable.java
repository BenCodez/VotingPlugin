package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement; // NEW
import java.util.ArrayList;
import java.util.List;
import java.util.Set; // NEW
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap; // NEW

import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;

public abstract class ProxyOnlineVoteCacheTable extends AbstractSqlTable {

	// Prevent repeated startup DDL across multiple instances/subclasses
	private static final Set<String> MIGRATED_VOTEID = ConcurrentHashMap.newKeySet();
	private static final Set<String> ENSURED_INDEXES = ConcurrentHashMap.newKeySet();

	@Override
	public String getPrimaryKeyColumn() {
		return "id";
	}

	@Override
	public String buildCreateTableSql(DbType dbType) {
		if (dbType == DbType.POSTGRESQL) {
			return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " (" + qi("id") + " BIGSERIAL PRIMARY KEY, "
					+ qi("uuid") + " " + bestUuidType() + ", " + qi("voteid") + " VARCHAR(36), " + qi("playerName")
					+ " VARCHAR(100), " + qi("service") + " VARCHAR(100), " + qi("time") + " BIGINT, " + qi("realVote")
					+ " BOOLEAN, " + qi("text") + " TEXT" + ");";
		}

		return "CREATE TABLE IF NOT EXISTS " + qi(getTableName()) + " (" + qi("id") + " INT AUTO_INCREMENT PRIMARY KEY,"
				+ qi("uuid") + " VARCHAR(37)," + qi("voteid") + " VARCHAR(36)," + qi("playerName") + " VARCHAR(100),"
				+ qi("service") + " VARCHAR(100)," + qi("time") + " BIGINT," + qi("realVote") + " TINYINT(1),"
				+ qi("text") + " TEXT," + "INDEX idx_uuid (" + qi("uuid") + ")," + "INDEX idx_time (" + qi("time") + ")"
				+ ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;";
	}

	public void removeVotesByUuid(String uuid) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(1, UUID.fromString(uuid));
			} else {
				ps.setString(1, uuid);
			}

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	@Override
	public abstract void logSevere(String msg);

	@Override
	public abstract void logInfo(String msg);

	@Override
	public abstract void debug(Throwable t);

	public ProxyOnlineVoteCacheTable(MySQL existingMysql, String tablePrefix, boolean debug) {
		super((tablePrefix != null ? tablePrefix : "") + "votingplugin_onlinevotecache", existingMysql, debug);

		// best-effort migrations (no nested connections)
		alterColumnType("uuid", bestUuidType());
		addVoteIdColumnIfMissingOnce();
		ensureIndexesOnce();
	}

	public ProxyOnlineVoteCacheTable(MysqlConfig config, boolean debug) {
		super("votingplugin_onlinevotecache", config, debug);

		alterColumnType("uuid", bestUuidType());
		addVoteIdColumnIfMissingOnce();
		ensureIndexesOnce();
	}

	public void updateVoteText(OfflineBungeeVote vote, String newText) {
		if (vote == null) {
			return;
		}

		String sql = "UPDATE " + qi(getTableName()) + " SET " + qi("text") + " = ?" + " WHERE " + qi("uuid")
				+ " = ? AND " + qi("service") + " = ? AND " + qi("time") + " = ?;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			ps.setString(1, newText);

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(2, UUID.fromString(vote.getUuid()));
			} else {
				ps.setString(2, vote.getUuid());
			}

			ps.setString(3, vote.getService());
			ps.setLong(4, vote.getTime());

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	private void ensureIndexesOnce() {
		if (getDbType() != DbType.POSTGRESQL) {
			return;
		}

		String key = getDbType() + ":" + getTableName() + ":indexes";
		if (!ENSURED_INDEXES.add(key)) {
			return;
		}

		// Use ONE connection for all index DDL to avoid pool starvation (pool=1 safe)
		try (Connection conn = mysql.getConnectionManager().getConnection(); Statement st = conn.createStatement()) {

			st.executeUpdate("CREATE INDEX IF NOT EXISTS idx_uuid ON " + qi(getTableName()) + " (" + qi("uuid") + ");");
			st.executeUpdate("CREATE INDEX IF NOT EXISTS idx_time ON " + qi(getTableName()) + " (" + qi("time") + ");");

		} catch (SQLException e) {
			debug(e);
		}
	}

	private void addVoteIdColumnIfMissingOnce() {
		String key = getDbType() + ":" + getTableName() + ":voteid";
		if (!MIGRATED_VOTEID.add(key)) {
			return;
		}
		addVoteIdColumnIfMissing();
	}

	private void addVoteIdColumnIfMissing() {
		final boolean pg = getDbType() == DbType.POSTGRESQL;

		final String schemaFilter = pg ? "table_schema = current_schema()" : "TABLE_SCHEMA = DATABASE()";

		// Lowercase compare avoids casing issues on some setups
		final String checkSql = "SELECT 1 FROM information_schema.columns WHERE " + schemaFilter
				+ " AND LOWER(table_name) = LOWER(?) AND LOWER(column_name) = LOWER(?) LIMIT 1;";

		// IMPORTANT: do check + alter on SAME connection to avoid deadlock when
		// poolSize=1
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(checkSql)) {

			ps.setString(1, getTableName());
			ps.setString(2, "voteid");

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					return; // exists
				}
			}

			String alter = "ALTER TABLE " + qi(getTableName()) + " ADD COLUMN " + qi("voteid") + " VARCHAR(36);";
			try (Statement st = conn.createStatement()) {
				st.executeUpdate(alter);
			}

		} catch (SQLException e) {
			logSevere("Failed to add voteid column to " + getTableName() + ": " + e.getMessage());
			debug(e);
		}
	}

	// --- INSERT ---

	public void insertVote(UUID voteId, String uuid, String playerName, String service, long time, boolean real,
			String text) {

		String sql = "INSERT INTO " + qi(getTableName()) + " (" + qi("uuid") + ", " + qi("voteid") + ", "
				+ qi("playerName") + ", " + qi("service") + ", " + qi("time") + ", " + qi("realVote") + ", "
				+ qi("text") + ") VALUES (?, ?, ?, ?, ?, ?, ?);";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(1, UUID.fromString(uuid));
			} else {
				ps.setString(1, uuid);
			}

			ps.setString(2, voteId != null ? voteId.toString() : null);
			ps.setString(3, playerName);
			ps.setString(4, service);
			ps.setLong(5, time);

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setBoolean(6, real);
			} else {
				ps.setInt(6, real ? 1 : 0);
			}

			ps.setString(7, text);

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	// --- GET ---

	public List<VoteRow> getAllVotes() {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + ";", null);
	}

	public List<VoteRow> getVotesByUUID(String uuid) {
		String sql = "SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ?;";
		Object param = (getDbType() == DbType.POSTGRESQL) ? UUID.fromString(uuid) : uuid;
		return selectVotes(sql, new Object[] { param });
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

	public void removeVote(OfflineBungeeVote vote) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ? AND " + qi("service")
				+ " = ? AND " + qi("time") + " = ?;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(1, UUID.fromString(vote.getUuid()));
			} else {
				ps.setString(1, vote.getUuid());
			}

			ps.setString(2, vote.getService());
			ps.setLong(3, vote.getTime());

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
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

	// --- Internals ---

	private List<VoteRow> selectVotes(String sql, Object[] params) {
		List<VoteRow> list = new ArrayList<>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					ps.setObject(i + 1, params[i]);
				}
			}

			try (ResultSet rs = ps.executeQuery()) {
				while (rs.next()) {
					list.add(new VoteRow(rs.getInt("id"), rs.getString("voteid"), rs.getString("uuid"),
							rs.getString("playerName"), rs.getString("service"), rs.getLong("time"),
							(getDbType() == DbType.POSTGRESQL ? rs.getBoolean("realVote") : rs.getInt("realVote") == 1),
							rs.getString("text")));
				}
			}
		} catch (SQLException e) {
			debug(e);
		}
		return list;
	}

	public static class VoteRow {
		private final int id;
		private final String voteId;
		private final String uuid;
		private final String playerName;
		private final String service;
		private final long time;
		private final boolean realVote;
		private final String text;

		public VoteRow(int id, String voteId, String uuid, String playerName, String service, long time,
				boolean realVote, String text) {
			this.id = id;
			this.voteId = voteId;
			this.uuid = uuid;
			this.playerName = playerName;
			this.service = service;
			this.time = time;
			this.realVote = realVote;
			this.text = text;
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

		public boolean isRealVote() {
			return realVote;
		}

		public String getText() {
			return text;
		}
	}
}
