package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.DynamicTest;

import com.bencodez.simpleapi.sql.mysql.AbstractSqlTable;
import com.bencodez.simpleapi.sql.mysql.ConnectionManager;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.votingplugin.proxy.cache.ProxyOnlineVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyTimedVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyVoteCacheTable;

/** Direct JDBC contract tests for checked proxy-cache snapshots. */
class ProxyCacheCheckedReaderJdbcTest {
	private record Fixture(Callable<List<?>> checked, Callable<List<?>> legacy,
			Connection connection, PreparedStatement statement, ResultSet resultSet) { }

	private Fixture fixture(String lane) throws Exception {
		MySQL mysql = mock(MySQL.class);
		ConnectionManager manager = mock(ConnectionManager.class);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet result = mock(ResultSet.class);
		when(mysql.getConnectionManager()).thenReturn(manager);
		when(manager.getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(result);
		SQLException rowFailure = new SQLException("row-read-failure");
		when(result.next()).thenReturn(true).thenThrow(rowFailure);
		Object table;
		Callable<List<?>> checked;
		Callable<List<?>> legacy;
		if (lane.equals("server")) {
			ProxyVoteCacheTable value = mock(ProxyVoteCacheTable.class, CALLS_REAL_METHODS);
			when(value.getMysql()).thenReturn(mysql); when(value.getDbType()).thenReturn(DbType.MYSQL);
			when(value.getTableName()).thenReturn("proxy_votes"); table = value;
			checked = value::getAllVotesChecked; legacy = value::getAllVotes;
		} else if (lane.equals("online")) {
			ProxyOnlineVoteCacheTable value = mock(ProxyOnlineVoteCacheTable.class, CALLS_REAL_METHODS);
			when(value.getMysql()).thenReturn(mysql); when(value.getDbType()).thenReturn(DbType.MYSQL);
			when(value.getTableName()).thenReturn("proxy_online_votes"); table = value;
			checked = value::getAllVotesChecked; legacy = value::getAllVotes;
		} else {
			ProxyTimedVoteCacheTable value = mock(ProxyTimedVoteCacheTable.class, CALLS_REAL_METHODS);
			when(value.getMysql()).thenReturn(mysql); when(value.getDbType()).thenReturn(DbType.MYSQL);
			when(value.getTableName()).thenReturn("proxy_timed_votes"); table = value;
			checked = value::getAllVotesChecked; legacy = value::getAllVotes;
		}
		Field mysqlField = AbstractSqlTable.class.getDeclaredField("mysql");
        mysqlField.setAccessible(true);
        mysqlField.set(table, mysql);
        return new Fixture(checked, legacy, connection, statement, result);
	}

	@TestFactory
	Stream<DynamicTest> checkedReadersPreserveInitialQueryFailureAndCloseResources() {
		return Stream.of("server", "online", "timed").map(lane -> DynamicTest.dynamicTest(lane, () -> {
			Fixture f = fixture(lane);
			SQLException failure = new SQLException("query-failure");
			when(f.statement.executeQuery()).thenThrow(failure);
            SQLException cleanup = new SQLException("synthetic statement cleanup failure");
            doThrow(cleanup).when(f.statement).close();
			SQLException actual = assertThrows(SQLException.class, () -> f.checked.call());
			assertSame(failure, actual);
            assertArrayEquals(new Throwable[] {cleanup}, actual.getSuppressed());
			verify(f.resultSet, never()).next();
			verify(f.statement).close();
			verify(f.connection).close();
		}));
	}

	@TestFactory
	Stream<DynamicTest> checkedReadersPropagatePartialRowFailureAndCloseResources() {
		return Stream.of("server", "online", "timed").map(lane -> DynamicTest.dynamicTest(lane, () -> {
			Fixture f = fixture(lane);
			SQLException actual = assertThrows(SQLException.class, () -> f.checked.call());
			assertEquals("row-read-failure", actual.getMessage());
			verify(f.resultSet).close(); verify(f.statement).close(); verify(f.connection).close();
		}));
	}

    @TestFactory
    Stream<DynamicTest> checkedReadersMatchLegacyFieldsForSuccessfulRows() {
        return Stream.of("server", "online", "timed").map(lane -> DynamicTest.dynamicTest(lane, () -> {
            Fixture f = fixture(lane);
            when(f.resultSet.getString(anyString())).thenAnswer(call -> {
                String column = call.getArgument(0, String.class).toLowerCase(java.util.Locale.ROOT);
                return switch (column) {
                    case "voteid", "uuid" -> "00000000-0000-0000-0000-000000000071";
                    case "playername" -> "test-player";
                    case "service" -> "synthetic-service";
                    case "server" -> "backend-a";
                    default -> "";
                };
            });
            when(f.resultSet.getInt("id")).thenReturn(71);
            when(f.resultSet.getLong("time")).thenReturn(1000L);
            when(f.resultSet.next()).thenReturn(true, false);
            Object legacy = f.legacy.call().get(0);
            when(f.resultSet.next()).thenReturn(true, false);
            Object checked = f.checked.call().get(0);
            assertEquals(legacy.getClass(), checked.getClass());
            for (Field field : legacy.getClass().getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) continue;
                field.setAccessible(true);
                assertEquals(field.get(legacy), field.get(checked), field.getName());
            }
            verify(f.resultSet, times(2)).close();
            verify(f.statement, times(2)).close();
            verify(f.connection, times(2)).close();
        }));
    }

	@Test
	void checkedReaderMatchesLegacyOnEmptySuccessfulSnapshot() throws Exception {
		for (String lane : List.of("server", "online", "timed")) {
			Fixture f = fixture(lane);
			when(f.resultSet.next()).thenReturn(false);
			assertEquals(f.legacy.call().size(), f.checked.call().size());
		}
	}
}
