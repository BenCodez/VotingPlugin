package com.Ben12345rocks.VotingPlugin.bungee;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.Ben12345rocks.AdvancedCore.UserStorage.mysql.api.queries.Query;
import com.Ben12345rocks.AdvancedCore.UserStorage.sql.Column;
import com.Ben12345rocks.AdvancedCore.UserStorage.sql.DataType;

import net.md_5.bungee.config.Configuration;

public class BungeeMySQL {
	private com.Ben12345rocks.AdvancedCore.UserStorage.mysql.api.MySQL mysql;

	private List<String> columns = Collections.synchronizedList(new ArrayList<String>());

	// private HashMap<String, ArrayList<Column>> table;

	// ConcurrentMap<String, ArrayList<Column>> table = new
	// ConcurrentHashMap<String, ArrayList<Column>>();

	private String name;

	private Set<String> uuids = Collections.synchronizedSet(new HashSet<String>());

	private Object object2 = new Object();

	private Object object3 = new Object();

	private Object object4 = new Object();

	private List<String> intColumns = new ArrayList<String>();

	public BungeeMySQL(String tableName, Configuration section) {
		String tablePrefix = section.getString("Prefix");
		String hostName = section.getString("Host");
		int port = section.getInt("Port");
		String user = section.getString("Username");
		String pass = section.getString("Password");
		String database = section.getString("Database");
		long lifeTime = section.getLong("MaxLifeTime", -1);
		int maxThreads = section.getInt("MaxConnections", 1);
		if (maxThreads < 1) {
			maxThreads = 1;
		}
		boolean useSSL = section.getBoolean("UseSSL", false);
		if (!section.getString("Name", "").isEmpty()) {
			tableName = section.getString("Name", "");
		}

		name = tableName;
		if (tablePrefix != null) {
			name = tablePrefix + tableName;
		}
		mysql = new com.Ben12345rocks.AdvancedCore.UserStorage.mysql.api.MySQL(maxThreads);
		if (!mysql.connect(hostName, "" + port, user, pass, database, useSSL, lifeTime)) {

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

	public String getUUID(String playerName) {
		String query = "SELECT uuid FROM " + getName() + " WHERE " + "PlayerName" + "='" + playerName + "';";
		try {
			Query sql = new Query(mysql, query);

			ResultSet rs = sql.executeQuery();
			rs.next();
			String uuid = rs.getString("uuid");
			if (uuid != null && !uuid.isEmpty()) {
				return uuid;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (ArrayIndexOutOfBoundsException e) {
		}
		return null;
	}

	public String getProxyVotes(String uuid, String server) {
		checkColumn("Proxy_" + server, DataType.STRING);
		String query = "SELECT Proxy_" + server + " FROM " + getName() + " WHERE " + "uuid" + "='" + uuid + "';";
		try {
			Query sql = new Query(mysql, query);

			ResultSet rs = sql.executeQuery();
			rs.next();
			String str = rs.getString("Proxy_" + server);
			if (str != null && !str.isEmpty()) {
				return str;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (ArrayIndexOutOfBoundsException e) {
		}
		return null;
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
		String sql = "SELECT uuid FROM " + getName() + ";";
		try {
			Query query = new Query(mysql, sql);

			ResultSet rs = query.executeQuery();
			while (rs.next()) {
				if (rs.getString("uuid").equals(index)) {
					return true;
				}
			}

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
		try {
			Query query = new Query(mysql, "SELECT * FROM " + getName() + ";");

			ResultSet rs = query.executeQuery();

			ResultSetMetaData metadata = rs.getMetaData();
			int columnCount = 0;
			if (metadata != null) {
				columnCount = metadata.getColumnCount();

				for (int i = 1; i <= columnCount; i++) {
					String columnName = metadata.getColumnName(i);
					columns.add(columnName);
				}
				return columns;
			}

		} catch (SQLException e) {
			e.printStackTrace();
		}
		return columns;

	}

	public ArrayList<Column> getExactQuery(Column column) {
		ArrayList<Column> result = new ArrayList<>();
		String query = "SELECT * FROM " + getName() + " WHERE `" + column.getName() + "`=?" + ";";

		try {
			Query sql = new Query(mysql, query);

			sql.setParameter(1, column.getValue().toString());

			ResultSet rs = sql.executeQuery();
			rs.next();
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
		String sql = "SELECT PlayerName FROM " + getName() + ";";

		try {
			Query query = new Query(mysql, sql);
			ResultSet rs = query.executeQuery();

			while (rs.next()) {
				Column rCol = new Column("PlayerName", rs.getString("PlayerName"), DataType.STRING);
				result.add(rCol);
			}
		} catch (SQLException e) {
		}

		return result;
	}

	public ArrayList<Column> getRowsQuery() {
		ArrayList<Column> result = new ArrayList<Column>();
		String sql = "SELECT uuid FROM " + getName() + ";";

		try {
			Query query = new Query(mysql, sql);
			ResultSet rs = query.executeQuery();

			while (rs.next()) {
				Column rCol = new Column("uuid", rs.getString("uuid"), DataType.STRING);
				result.add(rCol);
			}
		} catch (SQLException e) {
			return null;
		}

		return result;
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
		insertQuery(index, column, value, dataType);

	}

	public void insertQuery(String index, String column, Object value, DataType dataType) {
		synchronized (object2) {
			String query = "INSERT " + getName() + " ";

			query += "set uuid='" + index + "', ";
			query += column + "='" + value.toString() + "';";
			// AdvancedCorePlugin.getInstance().extraDebug(query);

			try {
				uuids.add(index);
				new Query(mysql, query).executeUpdateAsync();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}

	public boolean isIntColumn(String key) {
		return intColumns.contains(key);
	}

	public void loadData() {
		columns = getColumnsQueury();

		try {
			Connection con = mysql.getConnectionManager().getConnection();
			con.close();

		} catch (SQLException e) {
			e.printStackTrace();
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
