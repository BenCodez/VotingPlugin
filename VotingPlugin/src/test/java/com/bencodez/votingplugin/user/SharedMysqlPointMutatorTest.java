package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.ArgumentMatchers.any;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.votingplugin.VotingPluginMain;

class SharedMysqlPointMutatorTest {
	@Test
	void removeReportsARejectedConditionalDebit() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeUpdate()).thenReturn(0);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");

		assertFalse(new SharedMysqlPointMutator(plugin).remove(user, 10));
	}

	@Test
	void addUsesAtomicDatabaseArithmeticInsteadOfAnAbsoluteCachedWrite() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);

		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getMysql()).thenReturn(table);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");

		new SharedMysqlPointMutator(plugin).add(user, 10, false);

		ArgumentCaptor<String> query = ArgumentCaptor.forClass(String.class);
		verify(connection).prepareStatement(query.capture());
		assertTrue(query.getValue().contains("`Points` = `Points` + ?"));
		verify(statement).setInt(1, 10);
		verify(statement).executeUpdate();
		verify(persistence, never()).execute(any(Runnable.class));
	}

	@Test
	void capUsesLeastSoItCannotRestoreAConcurrentDebit() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");

		new SharedMysqlPointMutator(plugin).cap(user, 100, false);

		ArgumentCaptor<String> query = ArgumentCaptor.forClass(String.class);
		verify(connection).prepareStatement(query.capture());
		assertTrue(query.getValue().contains("`Points` = LEAST(`Points`, ?)"));
	}

	@Test
	void transferCreditsOnlyAfterConditionalDebitSucceeds() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(debit, credit);
		when(debit.executeUpdate()).thenReturn(1);
		when(credit.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser source = mock(VotingPluginUser.class);
		VotingPluginUser target = mock(VotingPluginUser.class);
		when(source.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
		when(source.getPointsPath()).thenReturn("Points");
		when(target.getPointsPath()).thenReturn("Points");

		assertTrue(new SharedMysqlPointMutator(plugin).transfer(source, target, 10));

		verify(debit).executeUpdate();
		verify(credit).executeUpdate();
		verify(connection).commit();
		verify(connection, times(0)).rollback();
	}
}
