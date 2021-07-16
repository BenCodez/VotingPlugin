package com.bencodez.votingplugin.bungee.velocity;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.bencodez.advancedcore.api.user.userstorage.mysql.api.queries.Query;
import com.bencodez.advancedcore.api.user.userstorage.sql.Column;
import com.bencodez.advancedcore.api.user.userstorage.sql.DataType;

public class BungeeMySQL {
	private List<String> columns = Collections.synchronizedList(new ArrayList<String>());

	private List<String> intColumns = new ArrayList<String>();

	// private HashMap<String, ArrayList<Column>> table;

	// ConcurrentMap<String, ArrayList<Column>> table = new
	// ConcurrentHashMap<String, ArrayList<Column>>();

	private com.bencodez.advancedcore.api.user.userstorage.mysql.api.MySQL mysql;

	private String name;

	private Object object2 = new Object();

	private Object object3 = new Object();

	private Object object4 = new Object();

	private Set<String> uuids = Collections.synchronizedSet(new HashSet<String>());

	public BungeeMySQL(VotingPluginVelocity plugin, String tableName, Config config) {
		String tablePrefix = config.getString(config.getNode("Prefix"), "");
		String hostName = config.getString(config.getNode("Host"), "");
		int port = config.getInt(config.getNode("Port"), 0);
		String user = config.getString(config.getNode("Username"), "");
		String pass = config.getString(config.getNode("Password"), "");
		String database = config.getString(config.getNode("Database"), "");
		long lifeTime = config.getLong(config.getNode("MaxLifeTime"), -1);
		int maxThreads = config.getInt(config.getNode("MaxConnections"), 1);
		String str = config.getString(config.getNode("Line"), "");
		if (maxThreads < 1) {
			maxThreads = 1;
		}
		boolean useSSL = config.getBoolean(config.getNode("UseSSL"), false);
		boolean publicKeyRetrieval = config.getBoolean(config.getNode("PublicKeyRetrieval"), false);
		name = config.getString(config.getNode("Name"), "");
		if (!name.isEmpty()) {
			name = tableName;
		}

		if (tablePrefix != null) {
			name = tablePrefix + tableName;
		}
		mysql = new com.bencodez.advancedcore.api.user.userstorage.mysql.api.MySQL(maxThreads) {

			@Override
			public void debug(SQLException e) {
				if (plugin.getConfig().getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void severe(String string) {
				plugin.getLogger().error(string);
			}
		};
		if (!mysql.connect(hostName, "" + port, user, pass, database, useSSL, lifeTime, str, publicKeyRetrieval)) {

		}
		try {
			Query q = new Query(mysql, "USE " + database + ";");
			q.executeUpdateAsync();
		} catch (SQLException e) {
			e.printStackTrace();
		}
		String sql = "CREATE TABLE IF NOT EXISTS " + getName() + " (";
		sql += "uuid VARCHAR(37),";
		sql += "PRIMARY KEY ( uuid )";
		sql += ");";
		Query query;
		try {
			query = new Query(mysql, sql);

			query.executeUpdateAsync();
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
			String sql = "ALTER TABLE " + getName() + " ADD COLUMN " + column + " text" + ";";
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
		checkColumn(column, DataType.STRING);
		if (newType.contains("INT")) {
			try {
				new Query(mysql, "UPDATE " + getName() + " SET " + column + " = '0' where trim(coalesce(" + column
						+ ", '')) = '';").executeUpdateAsync();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			;
			if (!intColumns.contains(column)) {
				intColumns.add(column);
			}
		}
		try {
			new Query(mysql, "ALTER TABLE " + getName() + " MODIFY " + column + " " + newType + ";")
					.executeUpdateAsync();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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

	public List<String> getColumns() {
		if (columns == null || columns.size() == 0) {
			loadData();
		}
		return columns;
	}

	public ArrayList<String> getColumnsQueury() {
		ArrayList<String> columns = new ArrayList<String>();
		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement("SELECT * FROM " + getName() + ";")) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, "SELECT * FROM " + getName() + ";"); ResultSet
			 * rs = query.executeQuery();
			 */

			ResultSetMetaData metadata = rs.getMetaData();
			int columnCount = 0;
			if (metadata != null) {
				columnCount = metadata.getColumnCount();

				for (int i = 1; i <= columnCount; i++) {
					String columnName = metadata.getColumnName(i);
					columns.add(columnName);
				}
				rs.close();
				return columns;
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
				+ column.getValue().toString() + "';";

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
					} else {
						rCol = new Column(columnName, DataType.STRING);
					}
					// System.out.println(i + " " +
					// rs.getMetaData().getColumnLabel(i));
					rCol.setValue(rs.getString(i));
					// System.out.println(rCol.getValue());
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
		ArrayList<String> uuids = new ArrayList<String>();

		checkColumn("PlayerName", DataType.STRING);
		ArrayList<Column> rows = getRowsNameQuery();
		if (rows != null) {
			for (Column c : rows) {
				uuids.add((String) c.getValue());
			}
		}

		return uuids;
	}

	public ArrayList<Column> getRowsNameQuery() {
		ArrayList<Column> result = new ArrayList<Column>();
		String sqlStr = "SELECT PlayerName FROM " + getName() + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */

			while (rs.next()) {
				Column rCol = new Column("PlayerName", rs.getString("PlayerName"), DataType.STRING);
				result.add(rCol);
			}
			sql.close();
			conn.close();
		} catch (SQLException e) {
		}

		return result;
	}

	public ArrayList<Column> getRowsQuery() {
		ArrayList<Column> result = new ArrayList<Column>();
		String sqlStr = "SELECT uuid FROM " + getName() + ";";

		try (Connection conn = mysql.getConnectionManager().getConnection();
				PreparedStatement sql = conn.prepareStatement(sqlStr)) {
			ResultSet rs = sql.executeQuery();
			/*
			 * Query query = new Query(mysql, sql); ResultSet rs = query.executeQuery();
			 */

			while (rs.next()) {
				Column rCol = new Column("uuid", rs.getString("uuid"), DataType.STRING);
				result.add(rCol);
			}
			rs.close();
		} catch (SQLException e) {
			return null;
		}

		return result;
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
		ArrayList<String> uuids = new ArrayList<String>();

		ArrayList<Column> rows = getRowsQuery();
		for (Column c : rows) {
			uuids.add((String) c.getValue());
		}

		return uuids;
	}

	public void insert(String index, String column, Object value, DataType dataType) {
		insertQuery(index, Arrays.asList(new Column(column, value, dataType)));
	}

	public void insertQuery(String index, List<Column> cols) {
		String query = "INSERT IGNORE " + getName() + " ";

		query += "set uuid='" + index + "', ";

		for (int i = 0; i < cols.size(); i++) {
			Column col = cols.get(i);
			if (i == cols.size() - 1) {
				query += col.getName() + "='" + col.getValue().toString() + "';";
			} else {
				query += col.getName() + "='" + col.getValue().toString() + "', ";
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

	public void update(String index, List<Column> cols, boolean queue) {
		for (Column col : cols) {
			checkColumn(col.getName(), col.getDataType());
		}
		if (getUuids().contains(index)) {
			synchronized (object2) {

				String query = "UPDATE " + getName() + " SET ";

				for (int i = 0; i < cols.size(); i++) {
					Column col = cols.get(i);
					if (i == cols.size() - 1) {
						if (col.getDataType().equals(DataType.STRING)) {
							query += col.getName() + "='" + col.getValue().toString() + "';";
						} else {
							query += col.getName() + "=" + col.getValue().toString() + ";";
						}
					} else {
						if (col.getDataType().equals(DataType.STRING)) {
							query += col.getName() + "='" + col.getValue().toString() + "', ";
						} else {
							query += col.getName() + "=" + col.getValue().toString() + ", ";
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

	public void update(String index, String column, Object value, DataType dataType) {
		checkColumn(column, dataType);
		if (getUuids().contains(index)) {
			synchronized (object2) {
				String query = "UPDATE " + getName() + " SET ";

				if (dataType == DataType.STRING) {
					query += column + "='" + value.toString() + "'";
				} else {
					query += column + "=" + value;

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
			insert(index, column, value, dataType);
		}

	}
}