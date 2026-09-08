package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VotingPluginUserPointSchedulingTest {
	@Test
	void sharedRemoveSkipsStaleCachedPointPrecheck() throws Exception {
		PointFixture fixture = pointFixture();
		doReturn(0).when(fixture.user).getPoints();
		when(fixture.statement.executeUpdate()).thenReturn(1);

		assertTrue(fixture.user.removePoints(10));
		verify(fixture.user, never()).getPoints();
		verify(fixture.statement).executeUpdate();
	}

	@Test
	void sharedRemoveConsumerRunsJdbcOnPersistenceExecutorAndReportsOnEntity() throws Exception {
		PointFixture fixture = pointFixture();
		when(fixture.statement.executeUpdate()).thenReturn(1);
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.removePoints(10, result::set);

		ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceWork.capture());
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
		persistenceWork.getValue().run();

		ArgumentCaptor<Runnable> entityWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), entityWork.capture(), eq(fixture.player));
		assertTrue(result.get() == null);
		entityWork.getValue().run();
		assertTrue(result.get());
		verify(fixture.sql.getConnectionManager()).getConnection();
	}

	@Test
	void sharedTransferReportsCompletionOnSourceEntityScheduler() throws Exception {
		PointFixture fixture = pointFixture();
		VotingPluginUser target = mock(VotingPluginUser.class);
		when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
		when(target.getPointsPath()).thenReturn("Points");
		PreparedStatement credit = mock(PreparedStatement.class);
		when(fixture.connection.prepareStatement(anyString())).thenReturn(fixture.statement, credit);
		when(fixture.statement.executeUpdate()).thenReturn(1);
		when(credit.executeUpdate()).thenReturn(1);
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(target, 10, result::set);

		ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceWork.capture());
		persistenceWork.getValue().run();
		ArgumentCaptor<Runnable> entityWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), entityWork.capture(), eq(fixture.player));
		assertTrue(result.get() == null);
		entityWork.getValue().run();
		assertTrue(result.get());
	}

	private static PointFixture pointFixture() throws Exception {
		PointFixture fixture = new PointFixture();
		fixture.plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.persistence = mock(ScheduledExecutorService.class);
		fixture.scheduler = mock(BukkitScheduler.class);
		fixture.player = mock(Player.class);
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.connection = mock(Connection.class);
		fixture.statement = mock(PreparedStatement.class);
		when(fixture.plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(fixture.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(fixture.plugin.getMysql()).thenReturn(fixture.table);
		when(fixture.plugin.getTimer()).thenReturn(fixture.persistence);
		when(fixture.plugin.getBukkitScheduler()).thenReturn(fixture.scheduler);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.connection);
		when(fixture.connection.prepareStatement(anyString())).thenReturn(fixture.statement);
		fixture.user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(fixture.user, fixture.plugin);
		doReturn("00000000-0000-0000-0000-000000000001").when(fixture.user).getUUID();
		doReturn("Points").when(fixture.user).getPointsPath();
		doReturn(fixture.player).when(fixture.user).getPlayer();
		doReturn(false).when(fixture.user).isCached();
		return fixture;
	}

	private static final class PointFixture {
		VotingPluginMain plugin;
		ScheduledExecutorService persistence;
		BukkitScheduler scheduler;
		Player player;
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		Connection connection;
		PreparedStatement statement;
		VotingPluginUser user;
	}
}
