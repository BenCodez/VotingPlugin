package com.bencodez.votingplugin.proxy.cache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;

/**
 * Proxy vote cache (per-server cached votes).
 *
 * Columns: - id (auto increment primary key) - uuid (player uuid) - voteid
 * (vote uuid, nullable) - playerName - service - time (millis) - realVote
 * (bool) - text (payload) - server
 *
 * Indexes: - server - uuid - time
 */
public abstract class ProxyVoteCacheTable extends AbstractSqlTable {

	// ---- Required hooks ----

	@Override
	public String getPrimaryKeyColumn() {
		// This table's actual PK is numeric id; AbstractSqlTable caches keys as
		// Strings.
		return "id";
	}

	@Override
	public String buildCreateTableSql(DbType dbType) {
		StringBuilder sb = new StringBuilder();

		if (dbType == DbType.POSTGRESQL) {
			sb.append("CREATE TABLE IF NOT EXISTS ").append(qi(getTableName())).append(" (").append(qi("id"))
					.append(" BIGSERIAL PRIMARY KEY, ").append(qi("uuid")).append(" ").append(bestUuidType())
					.append(", ").append(qi("voteid")).append(" VARCHAR(36), ").append(qi("playerName"))
					.append(" VARCHAR(100), ").append(qi("service")).append(" VARCHAR(100), ").append(qi("time"))
					.append(" BIGINT, ").append(qi("realVote")).append(" BOOLEAN, ").append(qi("text"))
					.append(" TEXT, ").append(qi("server")).append(" VARCHAR(100)").append(");");
		} else {
			sb.append("CREATE TABLE IF NOT EXISTS ").append(qi(getTableName())).append(" (").append(qi("id"))
					.append(" INT AUTO_INCREMENT PRIMARY KEY,").append(qi("uuid")).append(" VARCHAR(37),")
					.append(qi("voteid")).append(" VARCHAR(36),").append(qi("playerName")).append(" VARCHAR(100),")
					.append(qi("service")).append(" VARCHAR(100),").append(qi("time")).append(" BIGINT,")
					.append(qi("realVote")).append(" TINYINT(1),").append(qi("text")).append(" TEXT,")
					.append(qi("server")).append(" VARCHAR(100),").append("INDEX idx_server (").append(qi("server"))
					.append("),").append("INDEX idx_uuid (").append(qi("uuid")).append("),").append("INDEX idx_time (")
					.append(qi("time")).append(")").append(") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;");
		}

		return sb.toString();
	}

	public void updateVoteText(OfflineBungeeVote vote, String server, String newText) {
		if (vote == null || server == null) {
			return;
		}

		String sql = "UPDATE " + qi(getTableName()) + " SET " + qi("text") + " = ?" + " WHERE " + qi("uuid")
				+ " = ? AND " + qi("service") + " = ? AND " + qi("time") + " = ? AND " + qi("server") + " = ?;";

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
			ps.setString(5, server);

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

	// ---- Constructors ----

	public ProxyVoteCacheTable(MySQL existingMysql, String tablePrefix, boolean debug) {
		super((tablePrefix != null ? tablePrefix : "") + "votingplugin_votecache", existingMysql, debug);

		// best-effort migrations
		alterColumnType("uuid", bestUuidType());
		addVoteIdColumnIfMissing();
		ensureIndexes();
	}

	public ProxyVoteCacheTable(MysqlConfig config, boolean debug) {
		super("votingplugin_votecache", config, debug);

		alterColumnType("uuid", bestUuidType());
		addVoteIdColumnIfMissing();
		ensureIndexes();
	}

	// ---- Migrations / indexes ----

	private void ensureIndexes() {
		// Postgres doesn't support index defs inside CREATE TABLE the same way we do
		// for MySQL.
		// Create them best-effort; ignore duplicate errors on MySQL.
		if (getDbType() == DbType.POSTGRESQL) {
			try {
				new Query(mysql,
						"CREATE INDEX IF NOT EXISTS idx_server ON " + qi(getTableName()) + " (" + qi("server") + ");")
						.executeUpdate();
				new Query(mysql,
						"CREATE INDEX IF NOT EXISTS idx_uuid ON " + qi(getTableName()) + " (" + qi("uuid") + ");")
						.executeUpdate();
				new Query(mysql,
						"CREATE INDEX IF NOT EXISTS idx_time ON " + qi(getTableName()) + " (" + qi("time") + ");")
						.executeUpdate();
			} catch (SQLException e) {
				debug(e);
			}
		} else {
			// MySQL indexes are already in CREATE TABLE; nothing required here.
		}
	}

	private void addVoteIdColumnIfMissing() {
		// Works in both MySQL and Postgres using information_schema
		String schemaFilter;
		if (getDbType() == DbType.POSTGRESQL) {
			schemaFilter = "table_schema = current_schema()";
		} else {
			schemaFilter = "TABLE_SCHEMA = DATABASE()";
		}

		String checkSql = "SELECT 1 FROM information_schema.columns WHERE " + schemaFilter
				+ " AND table_name = ? AND column_name = ? LIMIT 1;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(checkSql)) {

			ps.setString(1, getTableName());
			ps.setString(2, getDbType() == DbType.POSTGRESQL ? "voteid" : "voteid");

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					return; // exists
				}
			}

			String alter = "ALTER TABLE " + qi(getTableName()) + " ADD COLUMN " + qi("voteid") + " VARCHAR(36);";
			new Query(mysql, alter).executeUpdate();

		} catch (SQLException e) {
			// don't hard fail startup
			logSevere("Failed to add voteid column to " + getTableName() + ": " + e.getMessage());
			debug(e);
		}
	}

	// ---- INSERT ----

	public void insertVote(UUID voteId, String uuid, String playerName, String service, long time, boolean real,
			String text, String server) {

		String sql = "INSERT INTO " + qi(getTableName()) + " (" + qi("uuid") + ", " + qi("voteid") + ", "
				+ qi("playerName") + ", " + qi("service") + ", " + qi("time") + ", " + qi("realVote") + ", "
				+ qi("text") + ", " + qi("server") + ") " + "VALUES (?, ?, ?, ?, ?, ?, ?, ?);";

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
			ps.setString(8, server);

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	// ---- GET ----

	public List<VoteRow> getAllVotes() {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + ";", null);
	}

	public List<VoteRow> getVotesForServer(String server) {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("server") + " = ?;",
				new Object[] { server });
	}

	public VoteRow getVoteById(int id) {
		List<VoteRow> list = selectVotes("SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("id") + " = ?;",
				new Object[] { id });
		return list.isEmpty() ? null : list.get(0);
	}

	public List<VoteRow> getVotesByUUID(String uuid) {
		String sql;
		Object[] params;

		if (getDbType() == DbType.POSTGRESQL) {
			sql = "SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ?;";
			params = new Object[] { UUID.fromString(uuid) };
		} else {
			sql = "SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ?;";
			params = new Object[] { uuid };
		}

		return selectVotes(sql, params);
	}

	public List<VoteRow> getVotesByPlayerName(String playerName) {
		return selectVotes("SELECT * FROM " + qi(getTableName()) + " WHERE " + qi("playerName") + " = ?;",
				new Object[] { playerName });
	}

	public Set<String> getServers() {
		Set<String> servers = new HashSet<>();
		String sql = "SELECT DISTINCT " + qi("server") + " FROM " + qi(getTableName()) + " WHERE " + qi("server")
				+ " IS NOT NULL;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql);
				ResultSet rs = ps.executeQuery()) {

			while (rs.next()) {
				String s = rs.getString(1);
				if (s != null) {
					servers.add(s);
				}
			}
		} catch (SQLException e) {
			debug(e);
		}
		return servers;
	}

	// ---- DELETE ----

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

	public void removeVotesByServerAndUUID(String server, String uuid) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("server") + " = ? AND " + qi("uuid")
				+ " = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			ps.setString(1, server);
			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(2, UUID.fromString(uuid));
			} else {
				ps.setString(2, uuid);
			}

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	public void removeVote(OfflineBungeeVote vote, String server) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("uuid") + " = ? AND " + qi("service")
				+ " = ? AND " + qi("time") + " = ? AND " + qi("server") + " = ?;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {

			if (getDbType() == DbType.POSTGRESQL) {
				ps.setObject(1, UUID.fromString(vote.getUuid()));
			} else {
				ps.setString(1, vote.getUuid());
			}

			ps.setString(2, vote.getService());
			ps.setLong(3, vote.getTime());
			ps.setString(4, server);

			ps.executeUpdate();
		} catch (SQLException | IllegalArgumentException e) {
			debug(e);
		}
	}

	public void removeVotesByServer(String server) {
		String sql = "DELETE FROM " + qi(getTableName()) + " WHERE " + qi("server") + " = ?;";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, server);
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

	// ---- Internals ----

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
					VoteRow v = new VoteRow(rs.getInt("id"), rs.getString("voteid"), rs.getString("uuid"),
							rs.getString("playerName"), rs.getString("service"), rs.getLong("time"),
							(getDbType() == DbType.POSTGRESQL ? rs.getBoolean("realVote") : rs.getInt("realVote") == 1),
							rs.getString("text"), rs.getString("server"));
					list.add(v);
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
		private final String server;

		public VoteRow(int id, String voteId, String uuid, String playerName, String service, long time,
				boolean realVote, String text, String server) {
			this.id = id;
			this.voteId = voteId;
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

		public String getServer() {
			return server;
		}
	}
}
