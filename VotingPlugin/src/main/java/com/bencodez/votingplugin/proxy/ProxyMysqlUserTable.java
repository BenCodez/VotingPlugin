package com.bencodez.votingplugin.proxy;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.queries.Query;

import lombok.Getter;

public abstract class ProxyMysqlUserTable {
	private List<String> columns = Collections.synchronizedList(new ArrayList<String>());

	private List<String> intColumns = new ArrayList<>();

	// private HashMap<String, ArrayList<Column>> table;

	// ConcurrentMap<String, ArrayList<Column>> table = new
	// ConcurrentHashMap<String, ArrayList<Column>>();

	@Getter
	private com.bencodez.simpleapi.sql.mysql.MySQL mysql;

	private String name;

	private Object object2 = new Object();

	private Object object3 = new Object();

	private Object object4 = new Object();

	private Set<String> uuids = Collections.synchronizedSet(new HashSet<String>());

	public abstract void logSevere(String string);

	public abstract void logInfo(String string);

	public ProxyMysqlUserTable(String tableName, MysqlConfig config, boolean debug) {

		if (config.hasTableNameSet()) {
			tableName = config.getTableName();
		}
		name = tableName;
		if (config.getTablePrefix() != null) {
			name = config.getTablePrefix() + tableName;
		}
		if (config.getPoolName().isEmpty()) {
			config.setPoolName("VotingPlugin" + "-" + tableName);
		}
		mysql = new com.bencodez.simpleapi.sql.mysql.MySQL(config.getMaxThreads()) {

			@Override
			public void debug(SQLException e) {
				if (debug) {
					e.printStackTrace();
				}
			}

			@Override
			public void severe(String string) {
				logSevere(string);
			}

			@Override
			public void debug(String msg) {
				if (debug) {
					logInfo("MYSQL DEBUG: " + msg);
				}
			}
		};

		if (!mysql.connect(config)) {

		}
		try {
			Query q = new Query(mysql, "USE `" + config.getDatabase() + "`;");
			q.executeUpdateAsync();
		} catch (SQLException e) {
			severe("Failed to send use database query: " + config.getDatabase() + " Error: " + e.getMessage());
			debug(e);
		}
		String sql = "CREATE TABLE IF NOT EXISTS " + getName() + " (";
		sql += "uuid VARCHAR(37),";
		sql += "PRIMARY KEY ( uuid )";
		sql += ");";
		Query query;
		try {
			query = new Query(mysql, sql);

			query.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}

		loadData();

		// tempoary to improve performance from old tables
		// addToQue("ALTER TABLE " + getName() + " MODIFY uuid VARCHAR(37);");
		alterColumnType("uuid", "VARCHAR(37)");
	}

	public void addColumn(String column, DataType dataType) {
		synchronized (object3) {
			String sql = "ALTER TABLE " + getName() + " ADD COLUMN `" + column + "` text" + ";";
			try {
				Query query = new Query(mysql, sql);
				query.executeUpdate();

				getColumns().add(column);
			} catch (SQLException e) {
				e.printStackTrace();
			}

		}
	}

	public void alterColumnType(String column, String newType) {
		// Keep your existing behavior (create column if needed, etc.)
		checkColumn(column, DataType.STRING);

		// Quick metadata check: if it's already OK, don't ALTER.
		try (Connection conn = mysql.getConnectionManager().getConnection()) {
			if (!columnNeedsAlter(conn, column, newType)) {
				// Already the desired type/length/default – nothing to do.
				return;
			}
		} catch (SQLException e) {
			// If the metadata check fails, be conservative and *do* the ALTER.
			mysql.debug("Failed to inspect column " + getName() + "." + column + " – running ALTER anyway");
			mysql.debug(e);
		}

		// Only reach here when we *actually* need to change the column.
		try {
			new Query(mysql, "ALTER TABLE `" + getName() + "` MODIFY `" + column + "` " + newType + ";")
					.executeUpdateAsync();
		} catch (SQLException e) {
			e.printStackTrace();
		}

		if (newType.toUpperCase().contains("INT")) {
			if (!intColumns.contains(column)) {
				intColumns.add(column);
			}
		}
	}

	private boolean columnNeedsAlter(Connection conn, String column, String newType) throws SQLException {
		String sql = "SELECT DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, COLUMN_DEFAULT " + "FROM information_schema.COLUMNS "
				+ "WHERE TABLE_SCHEMA = DATABASE() " + "AND TABLE_NAME = ? " + "AND COLUMN_NAME = ?";

		try (PreparedStatement ps = conn.prepareStatement(sql)) {
			ps.setString(1, getName()); // or tableName, whatever you use
			ps.setString(2, column);
			try (ResultSet rs = ps.executeQuery()) {
				if (!rs.next()) {
					// Column doesn't exist yet -> we definitely need to ALTER/create
					return true;
				}

				String dataType = rs.getString("DATA_TYPE"); // e.g. "varchar", "int", "mediumtext"
				Object lenObj = rs.getObject("CHARACTER_MAXIMUM_LENGTH");
				Long length = null;
				if (lenObj instanceof Number) {
					length = ((Number) lenObj).longValue(); // works for BigInteger, Long, Integer, etc.
				}
				String defaultVal = rs.getString("COLUMN_DEFAULT");

				String typeUpper = newType.toUpperCase().trim();

				// MEDIUMTEXT
				if (typeUpper.equals("MEDIUMTEXT")) {
					return !dataType.equalsIgnoreCase("mediumtext");
				}

				// VARCHAR(N)
				if (typeUpper.startsWith("VARCHAR(")) {
					int open = typeUpper.indexOf('(');
					int close = typeUpper.indexOf(')', open + 1);
					if (open != -1 && close != -1) {
						String lenStr = typeUpper.substring(open + 1, close);
						int expectedLen;
						try {
							expectedLen = Integer.parseInt(lenStr);
						} catch (NumberFormatException ex) {
							// if we can't parse, be safe and ALTER
							return true;
						}

						boolean typeMatches = dataType.equalsIgnoreCase("varchar");
						boolean lengthMatches = (length != null && length == expectedLen);
						return !(typeMatches && lengthMatches);
					}
					// malformed definition, fallback to altering
					return true;
				}

				// INT [DEFAULT '0'] style
				if (typeUpper.startsWith("INT")) {
					boolean isIntFamily = dataType.equalsIgnoreCase("int") || dataType.equalsIgnoreCase("integer")
							|| dataType.equalsIgnoreCase("mediumint") || dataType.equalsIgnoreCase("smallint")
							|| dataType.equalsIgnoreCase("tinyint") || dataType.equalsIgnoreCase("bigint");

					boolean expectDefaultZero = typeUpper.contains("DEFAULT '0'") || typeUpper.contains("DEFAULT 0");

					if (!expectDefaultZero) {
						// Only care that it's some INT-like type
						return !isIntFamily;
					} else {
						boolean defaultIsZero = defaultVal != null && defaultVal.trim().equals("0");
						return !(isIntFamily && defaultIsZero);
					}
				}

				// For any types we don't explicitly handle, be conservative and ALTER.
				return true;
			}
		}
	}

	public void checkColumn(String column, DataType dataType) {
		synchronized (object4) {
			if (!getColumns().contains(column)) {
				if (!getColumnsQueury().contains(column)) {
					addColumn(column, dataType);
				}
			}
		}
	}

	public void clearCache() {
		clearCacheBasic();
	}

	public void clearCacheBasic() {
		columns.clear();
		columns.addAll(getColumnsQueury());
		uuids.clear();
		uuids.addAll(getUuidsQuery());
	}

	public void close() {
		mysql.disconnect();
	}

	public boolean containsKeyQuery(String index) {
		String sqlStr = "SELECT uuid FROM " + getName() + ";";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */
			while (rs.next()) {
				if (rs.getString("uuid").equals(index)) {
					rs.close();
					return true;
				}
			}
			rs.close();
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
		return false;
	}

	public void copyColumnData(String columnFromName, String columnToName) {
		checkColumn(columnFromName, DataType.STRING);
		checkColumn(columnToName, DataType.STRING);
		String sql = "UPDATE `" + getName() + "` SET `" + columnToName + "` = `" + columnFromName + "`;";
		try {
			Query query = new Query(mysql, sql);
			query.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public abstract void debug(SQLException e);

	public List<String> getColumns() {
		if (columns == null || columns.size() == 0) {
			loadData();
		}
		return columns;
	}

	public ArrayList<String> getColumnsQueury() {
		ArrayList<String> columns = new ArrayList<>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement("SHOW COLUMNS FROM `" + getName() + "`;")) {
			ResultSet rs = sql.executeQuery();

			while (rs.next()) {
				String columnName = rs.getString(1);
				columns.add(columnName);

			}

			rs.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return columns;
	}

	public ArrayList<Column> getExactQuery(Column column) {
		ArrayList<Column> result = new ArrayList<>();
		String query = "SELECT * FROM " + getName() + " WHERE `" + column.getName() + "`='"
				+ column.getValue().getString() + "';";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(query)) {
			ResultSet rs = sql.executeQuery();

			/*
			 * Query sql = new Query(mysql, query); sql.setParameter(1,
			 * column.getValue().toString()); ResultSet rs = sql.executeQuery();
			 */
			if (rs.next()) {
				for (int i = 1; i <= rs.getMetaData().getColumnCount(); i++) {
					String columnName = rs.getMetaData().getColumnLabel(i);
					Column rCol = null;
					if (intColumns.contains(columnName)) {
						rCol = new Column(columnName, DataType.INTEGER);
						rCol.setValue(new DataValueInt(rs.getInt(i)));
					} else {
						rCol = new Column(columnName, DataType.STRING);
						rCol.setValue(new DataValueString(rs.getString(i)));
					}
					result.add(rCol);
				}
			}
			rs.close();
			return result;
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (ArrayIndexOutOfBoundsException e) {
		}

		for (String col : getColumns()) {
			result.add(new Column(col, DataType.STRING));
		}
		return result;
	}

	public String getName() {
		return name;
	}

	public ArrayList<String> getNamesQuery() {
		ArrayList<String> uuids = new ArrayList<>();

		checkColumn("PlayerName", DataType.STRING);
		ArrayList<Column> rows = getRowsNameQuery();
		if (rows != null) {
			for (Column c : rows) {
				if (c.getValue().isString()) {
					uuids.add(c.getValue().getString());
				}
			}
		}

		return uuids;
	}

	public ArrayList<Column> getRowsNameQuery() {
		ArrayList<Column> result = new ArrayList<>();
		String sqlStr = "SELECT PlayerName FROM " + getName() + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */

			while (rs.next()) {
				Column rCol = new Column("PlayerName", new DataValueString(rs.getString("PlayerName")));
				result.add(rCol);
			}
			sql.close();
			conn.close();
		} catch (SQLException e) {
		}

		return result;
	}

	public ArrayList<Column> getRowsQuery() {
		ArrayList<Column> result = new ArrayList<>();
		String sqlStr = "SELECT uuid FROM " + getName() + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */

			while (rs.next()) {
				Column rCol = new Column("uuid", new DataValueString(rs.getString("uuid")));
				result.add(rCol);
			}
			rs.close();
		} catch (SQLException e) {
			return null;
		}

		return result;
	}

	public ConcurrentHashMap<UUID, String> getRowsUUIDNameQuery() {
		ConcurrentHashMap<UUID, String> uuidNames = new ConcurrentHashMap<>();
		String sqlStr = "SELECT UUID, PlayerName FROM " + getName() + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */

			while (rs.next()) {
				String uuid = rs.getString("uuid");
				String playerName = rs.getString("PlayerName");
				if (uuid != null && !uuid.isEmpty() && !uuid.equals("null") && playerName != null) {
					uuidNames.put(UUID.fromString(uuid), playerName);
				}
			}
			sql.close();
			conn.close();
		} catch (SQLException e) {
		}

		return uuidNames;
	}

	public String getUUID(String playerName) {
		String query = "SELECT uuid FROM " + getName() + " WHERE " + "PlayerName" + "='" + playerName + "';";
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(query)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query sql = new Query(mysql, query); ResultSet rs = sql.executeQuery();
			 */
			if (rs.next()) {
				String uuid = rs.getString("uuid");
				if (uuid != null && !uuid.isEmpty()) {
					rs.close();
					return uuid;
				}
			}
			rs.close();
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (ArrayIndexOutOfBoundsException e) {
		}
		return null;
	}

	public Set<String> getUuids() {
		if (uuids == null || uuids.size() == 0) {
			uuids.clear();
			uuids.addAll(getUuidsQuery());
			return uuids;
		}
		return uuids;
	}

	public ArrayList<String> getUuidsQuery() {
		ArrayList<String> uuids = new ArrayList<>();

		ArrayList<Column> rows = getRowsQuery();
		for (Column c : rows) {
			if (c.getValue().isString()) {
				uuids.add(c.getValue().getString());
			}
		}

		return uuids;
	}

	public void insert(String index, String column, DataValue value) {
		insertQuery(index, Arrays.asList(new Column(column, value)));
	}

	public void insertQuery(String index, List<Column> cols) {
		String query = "INSERT IGNORE " + getName() + " ";

		query += "set uuid='" + index + "', ";

		for (int i = 0; i < cols.size(); i++) {
			Column col = cols.get(i);
			if (i == cols.size() - 1) {
				if (col.getValue().isString()) {
					query += "`" + col.getName() + "`='" + col.getValue().getString() + "';";
				} else if (col.getValue().isBoolean()) {
					query += "`" + col.getName() + "`='" + col.getValue().getBoolean() + "';";
				} else if (col.getValue().isInt()) {
					query += "`" + col.getName() + "`='" + col.getValue().getInt() + "';";
				}
			} else {
				if (col.getValue().isString()) {
					query += "`" + col.getName() + "`='" + col.getValue().getString() + "', ";
				} else if (col.getValue().isBoolean()) {
					query += "`" + col.getName() + "`='" + col.getValue().getBoolean() + "', ";
				} else if (col.getValue().isInt()) {
					query += "`" + col.getName() + "`='" + col.getValue().getInt() + "', ";
				}
			}

		}

		try {
			uuids.add(index);
			new Query(mysql, query).executeUpdateAsync();
		} catch (SQLException e) {
			e.printStackTrace();
		}

	}

	public boolean isIntColumn(String key) {
		return intColumns.contains(key);
	}

	public void loadData() {
		columns = getColumnsQueury();
	}

	public abstract void severe(String str);

	public void shutdown() {
		mysql.disconnect();
	}

	public void update(String index, List<Column> cols) {
		for (Column col : cols) {
			checkColumn(col.getName(), col.getDataType());
		}
		if (getUuids().contains(index)) {
			synchronized (object2) {

				String query = "UPDATE " + getName() + " SET ";

				for (int i = 0; i < cols.size(); i++) {
					Column col = cols.get(i);
					if (i == cols.size() - 1) {
						if (col.getValue().isString()) {
							query += "`" + col.getName() + "`='" + col.getValue().getString() + "'";
						} else if (col.getValue().isBoolean()) {
							query += "`" + col.getName() + "`='" + col.getValue().getBoolean() + "'";
						} else if (col.getValue().isInt()) {
							query += "`" + col.getName() + "`='" + col.getValue().getInt() + "'";
						}
					} else {
						if (col.getValue().isString()) {
							query += "`" + col.getName() + "`='" + col.getValue().getString() + "', ";
						} else if (col.getValue().isBoolean()) {
							query += "`" + col.getName() + "`='" + col.getValue().getBoolean() + "', ";
						} else if (col.getValue().isInt()) {
							query += "`" + col.getName() + "`='" + col.getValue().getInt() + "', ";
						}
					}
				}
				query += " WHERE `uuid`=";
				query += "'" + index + "';";

				try {
					Query q = new Query(mysql, query);
					q.executeUpdateAsync();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
		} else {
			insertQuery(index, cols);
		}
	}

	public void update(String index, String column, DataValue value) {
		checkColumn(column, value.getType());
		if (getUuids().contains(index)) {
			synchronized (object2) {
				String query = "UPDATE " + getName() + " SET ";

				if (value.isString()) {
					query += column + "='" + value.getString() + "'";
				} else if (value.isBoolean()) {
					query += column + "='" + value.getBoolean() + "'";
				} else if (value.isInt()) {
					query += column + "='" + value.getInt() + "'";
				}
				query += " WHERE `uuid`=";
				query += "'" + index + "';";

				try {
					Query q = new Query(mysql, query);
					q.executeUpdate();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}

		} else {
			insert(index, column, value);
		}

	}

	public void wipeColumnData(String columnName, DataType dataType) {
		checkColumn(columnName, dataType);
		String sql = "UPDATE " + getName() + " SET " + columnName + " = " + dataType.getNoValue() + ";";
		try {
			Query query = new Query(mysql, sql);
			query.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}

	}
}