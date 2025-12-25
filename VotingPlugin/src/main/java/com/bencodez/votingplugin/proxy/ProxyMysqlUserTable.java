package com.bencodez.votingplugin.proxy;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

public abstract class ProxyMysqlUserTable extends AbstractSqlTable {

	// Keep this so existing callers relying on getName() still work
	private final String name;

	// For int-vs-string decode in getExactQuery (same pattern as before)
	private final List<String> intColumns = new ArrayList<>();

	// Backwards-compat wrapper (your old code expects getUuids())
	private final Set<String> uuidsView;

	// ---- Required subclass hooks ----

	@Override
	public String getPrimaryKeyColumn() {
		return "uuid";
	}

	@Override
	public String buildCreateTableSql(DbType dbType) {
		// NOTE: AbstractSqlTable.qi(...) already quotes identifiers properly for
		// DbType.
		StringBuilder sb = new StringBuilder();
		sb.append("CREATE TABLE IF NOT EXISTS ").append(qi(getTableName())).append(" (");

		// Best-case uuid type:
		// - Postgres: UUID
		// - MySQL/MariaDB: VARCHAR(37) (matches existing behavior)
		sb.append(qi("uuid")).append(" ").append(bestUuidType()).append(", ");
		sb.append("PRIMARY KEY ( ").append(qi("uuid")).append(" )");
		sb.append(");");
		return sb.toString();
	}

	// ---- Logging hooks required by AbstractSqlTable ----

	@Override
	public abstract void logSevere(String msg);

	@Override
	public abstract void logInfo(String msg);

	@Override
	public abstract void debug(Throwable t);

	// Keep these existing abstract methods for compatibility with your old
	// subclasses
	public abstract void debug(SQLException e);

	public abstract void severe(String str);

	// ---- Constructors ----

	public ProxyMysqlUserTable(String tableName, MysqlConfig config, boolean debug) {
		super(tableName, config, debug);

		this.name = super.getTableName();

		// keep uuids view
		this.uuidsView = super.getPrimaryKeys();

		// keep old "uuid column type tweak" behavior (but now uses base helper)
		alterColumnType("uuid", bestUuidType());

		// load intColumns behavior if you rely on it elsewhere
		// (no-op: it's only used if you add to it)
	}

	/**
	 * Use an already-connected pool (shared).
	 */
	public ProxyMysqlUserTable(String tableName, MySQL existingMysql, DbType dbType) {
		super(tableName, existingMysql, dbType);

		this.name = super.getTableName();
		this.uuidsView = super.getPrimaryKeys();

		alterColumnType("uuid", bestUuidType());
	}

	// ---- Backwards-compatible API ----

	public String getName() {
		return name;
	}

	/**
	 * Keep old method name and behavior.
	 */
	public Set<String> getUuids() {
		return getPrimaryKeys();
	}

	/**
	 * Keep old behavior: query uuids fresh.
	 */
	public ArrayList<String> getUuidsQuery() {
		return new ArrayList<>(getPrimaryKeysQuery());
	}

	public void loadData() {
		// old name -> new behavior
		loadBasicCaches();
	}

	public void clearCache() {
		clearCacheBasic();
	}

	public void clearCacheBasic() {
		clearCaches();
	}

	public void shutdown() {
		close();
	}

	// Keep old containsKeyQuery signature
	public boolean containsKeyQuery(String index) {
		return super.containsKeyQuery(index);
	}

	// ---- Required old utility methods (kept) ----

	public ArrayList<String> getColumnsQueury() {
		return new ArrayList<>(getColumnsQuery());
	}

	public ArrayList<Column> getRowsNameQuery() {
		ArrayList<Column> result = new ArrayList<>();
		checkColumn("PlayerName", DataType.STRING);

		String sqlStr = "SELECT " + qi("PlayerName") + " FROM " + qi(getTableName()) + ";";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sqlStr);
				ResultSet rs = ps.executeQuery()) {

			while (rs.next()) {
				result.add(new Column("PlayerName", new DataValueString(rs.getString(1))));
			}
		} catch (SQLException ignored) {
		}

		return result;
	}

	public ArrayList<String> getNamesQuery() {
		ArrayList<String> names = new ArrayList<>();
		checkColumn("PlayerName", DataType.STRING);

		ArrayList<Column> rows = getRowsNameQuery();
		if (rows != null) {
			for (Column c : rows) {
				if (c.getValue() != null && c.getValue().isString()) {
					String v = c.getValue().getString();
					if (v != null) {
						names.add(v);
					}
				}
			}
		}
		return names;
	}

	public ArrayList<Column> getRowsQuery() {
		ArrayList<Column> result = new ArrayList<>();
		String sqlStr = "SELECT " + qi("uuid") + " FROM " + qi(getTableName()) + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sqlStr);
				ResultSet rs = ps.executeQuery()) {

			while (rs.next()) {
				result.add(new Column("uuid", new DataValueString(rs.getString(1))));
			}
		} catch (SQLException e) {
			return null;
		}

		return result;
	}

	public ConcurrentHashMap<UUID, String> getRowsUUIDNameQuery() {
		ConcurrentHashMap<UUID, String> uuidNames = new ConcurrentHashMap<>();

		checkColumn("PlayerName", DataType.STRING);

		String sqlStr = "SELECT " + qi("uuid") + ", " + qi("PlayerName") + " FROM " + qi(getTableName()) + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(sqlStr);
				ResultSet rs = ps.executeQuery()) {

			while (rs.next()) {
				String uuidStr = rs.getString(1);
				String playerName = rs.getString(2);

				if (uuidStr != null && !uuidStr.isEmpty() && !"null".equalsIgnoreCase(uuidStr) && playerName != null) {
					try {
						uuidNames.put(UUID.fromString(uuidStr), playerName);
					} catch (IllegalArgumentException ignored) {
					}
				}
			}
		} catch (SQLException ignored) {
		}

		return uuidNames;
	}

	public String getUUID(String playerName) {
		checkColumn("PlayerName", DataType.STRING);

		String query = "SELECT " + qi("uuid") + " FROM " + qi(getTableName()) + " WHERE " + qi("PlayerName") + " = ?;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(query)) {

			ps.setString(1, playerName);

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					String uuid = rs.getString(1);
					if (uuid != null && !uuid.isEmpty()) {
						return uuid;
					}
				}
			}
		} catch (SQLException e) {
			debug(e);
		}

		return null;
	}

	public ArrayList<Column> getExactQuery(Column column) {
		ArrayList<Column> result = new ArrayList<>();

		String query = "SELECT * FROM " + qi(getTableName()) + " WHERE " + qi(column.getName()) + " = ?;";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(query)) {

			ps.setString(1, column.getValue().getString());

			try (ResultSet rs = ps.executeQuery()) {
				if (rs.next()) {
					for (int i = 1; i <= rs.getMetaData().getColumnCount(); i++) {
						String columnName = rs.getMetaData().getColumnLabel(i);
						Column rCol;
						if (intColumns.contains(columnName)) {
							rCol = new Column(columnName, DataType.INTEGER);
							rCol.setValue(new DataValueInt(rs.getInt(i)));
						} else {
							rCol = new Column(columnName, DataType.STRING);
							rCol.setValue(new DataValueString(rs.getString(i)));
						}
						result.add(rCol);
					}
					return result;
				}
			}
		} catch (SQLException e) {
			debug(e);
		} catch (ArrayIndexOutOfBoundsException ignored) {
		}

		for (String col : getColumns()) {
			result.add(new Column(col, DataType.STRING));
		}
		return result;
	}

	public boolean isIntColumn(String key) {
		return intColumns.contains(key);
	}

	// ---- INSERT/UPDATE/WIPE (kept similar to your existing behavior) ----

	public void insert(String index, String column, DataValue value) {
		insertQuery(index, Arrays.asList(new Column(column, value)));
	}

	public void insertQuery(String index, List<Column> cols) {
		for (Column c : cols) {
			checkColumn(c.getName(), c.getDataType());
		}

		// Keep your previous cross-db behavior:
		// - Postgres: ON CONFLICT upsert
		// - MySQL/MariaDB: INSERT IGNORE ... SET ...
		if (getDbType() == DbType.POSTGRESQL) {
			StringBuilder sb = new StringBuilder();
			sb.append("INSERT INTO ").append(qi(getTableName())).append(" (").append(qi("uuid"));

			for (Column col : cols) {
				sb.append(", ").append(qi(col.getName()));
			}
			sb.append(") VALUES (?");

			for (int i = 0; i < cols.size(); i++) {
				sb.append(", ?");
			}
			sb.append(") ON CONFLICT (").append(qi("uuid")).append(") DO UPDATE SET ");

			for (int i = 0; i < cols.size(); i++) {
				Column col = cols.get(i);
				String c = qi(col.getName());
				sb.append(c).append(" = EXCLUDED.").append(c);
				if (i != cols.size() - 1) {
					sb.append(", ");
				}
			}
			sb.append(";");

			try (Connection conn = mysql.getConnectionManager().getConnection();
					PreparedStatement ps = conn.prepareStatement(sb.toString())) {

				ps.setObject(1, UUID.fromString(index));
				int p = 2;
				for (Column col : cols) {
					ps.setString(p++, col.getValue().toString());
				}
				ps.executeUpdate();
				uuidsView.add(index);
			} catch (SQLException | IllegalArgumentException e) {
				debug(e);
			}
			return;
		}

		StringBuilder query = new StringBuilder();
		query.append("INSERT IGNORE ").append(qi(getTableName())).append(" SET ").append(qi("uuid")).append("=?, ");

		for (int i = 0; i < cols.size(); i++) {
			Column col = cols.get(i);
			query.append(qi(col.getName())).append("=?");
			query.append(i == cols.size() - 1 ? ";" : ", ");
		}

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement ps = conn.prepareStatement(query.toString())) {

			ps.setString(1, index);
			int p = 2;
			for (Column col : cols) {
				ps.setString(p++, col.getValue().toString());
			}
			ps.executeUpdate();
			uuidsView.add(index);
		} catch (SQLException e) {
			debug(e);
		}
	}

	public void update(String index, List<Column> cols) {
		for (Column col : cols) {
			checkColumn(col.getName(), col.getDataType());
		}

		// keep old behavior: if missing, insert
		if (!getUuids().contains(index) && !containsKeyQuery(index)) {
			insertQuery(index, cols);
			return;
		}

		synchronized (updateLock) {
			StringBuilder sb = new StringBuilder();
			sb.append("UPDATE ").append(qi(getTableName())).append(" SET ");

			for (int i = 0; i < cols.size(); i++) {
				Column col = cols.get(i);
				sb.append(qi(col.getName())).append("=?");
				if (i != cols.size() - 1) {
					sb.append(", ");
				}
			}

			sb.append(" WHERE ").append(qi("uuid")).append("=?;");

			try (Connection conn = mysql.getConnectionManager().getConnection();
					PreparedStatement ps = conn.prepareStatement(sb.toString())) {

				int p = 1;
				for (Column col : cols) {
					ps.setString(p++, col.getValue().toString());
				}

				if (getDbType() == DbType.POSTGRESQL) {
					ps.setObject(p, UUID.fromString(index));
				} else {
					ps.setString(p, index);
				}

				ps.executeUpdate();
			} catch (SQLException | IllegalArgumentException e) {
				debug(e);
			}
		}
	}

	public void update(String index, String column, DataValue value) {
		checkColumn(column, value.getType());

		if (!getUuids().contains(index) && !containsKeyQuery(index)) {
			insert(index, column, value);
			return;
		}

		synchronized (updateLock) {
			String sql = "UPDATE " + qi(getTableName()) + " SET " + qi(column) + "=? WHERE " + qi("uuid") + "=?;";
			try (Connection conn = mysql.getConnectionManager().getConnection();
					PreparedStatement ps = conn.prepareStatement(sql)) {

				ps.setString(1, value.toString());

				if (getDbType() == DbType.POSTGRESQL) {
					ps.setObject(2, UUID.fromString(index));
				} else {
					ps.setString(2, index);
				}

				ps.executeUpdate();
			} catch (SQLException | IllegalArgumentException e) {
				debug(e);
			}
		}
	}

	public void wipeColumnData(String columnName, DataType dataType) {
		checkColumn(columnName, dataType);

		String sql = "UPDATE " + qi(getTableName()) + " SET " + qi(columnName) + " = " + dataType.getNoValue() + ";";
		try {
			new Query(mysql, sql).executeUpdate();
		} catch (SQLException e) {
			debug(e);
		}
	}
}
