package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.entity.Player;
import org.bukkit.Bukkit;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.simpleapi.sql.mysql.ConnectionManager;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerReceivePointsEvent;

class VotingPluginUserPointSchedulingTest {
	@Test
	void storageAwareAddStaysSynchronousOutsideSharedMysql() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
		VotingPluginUser user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(user, plugin);
		doReturn(15).when(user).addPoints(10, false);

		assertEquals(15, user.addPointsStorageAware(10));

		verify(user).addPoints(10, false);
		verify(user, never()).addPoints(10, true);
	}

	@Test
	void nonSharedTransferCreditsBeforeReportingSuccess() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
		VotingPluginUser source = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		VotingPluginUser target = mock(VotingPluginUser.class);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(source, plugin);
		doReturn(true).when(source).removePoints(10);
		AtomicReference<Boolean> result = new AtomicReference<>();

		source.transferPoints(target, 10, result::set);

		InOrder order = inOrder(source, target);
		order.verify(source).removePoints(10);
		order.verify(target).addPoints(10);
		assertEquals(Boolean.TRUE, result.get());
	}

	@Test
	void votePointAwardQueuesSharedMysqlMutationOffTheServerLane() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		doReturn(data).when(fixture.user).getUserData();
		when(data.getInt("Points", UserDataFetchMode.TEMP_ONLY)).thenReturn(10);
		when(fixture.plugin.getConfigFile().getPointsOnVote()).thenReturn(5);
		when(fixture.plugin.getConfigFile().getLimitVotePoints()).thenReturn(0);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			fixture.user.addPoints();
		}

		verify(fixture.persistence).execute(any(Runnable.class));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void sharedAddReturnsTheCommittedDatabaseBalanceInsteadOfAPredictedWrapperTotal() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		PreparedStatement read = mock(PreparedStatement.class);
		ResultSet result = mock(ResultSet.class);
		doReturn(data).when(fixture.user).getUserData();
		when(fixture.statement.executeUpdate()).thenReturn(1);
		when(data.getInt("Points", UserDataFetchMode.NO_CACHE)).thenReturn(10);
		when(fixture.connection.prepareStatement(anyString())).thenReturn(fixture.statement, read);
		when(read.executeQuery()).thenReturn(result);
		when(result.next()).thenReturn(true);
		when(result.getInt(1)).thenReturn(73);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);

			assertEquals(73, fixture.user.addPoints(5));
		}

		InOrder mutationThenRead = inOrder(fixture.statement, read);
		mutationThenRead.verify(fixture.statement).executeUpdate();
		mutationThenRead.verify(read).executeQuery();
		verify(data, never()).getInt("Points", UserDataFetchMode.NO_CACHE);
	}

	@Test
	void storageAwareAddReportsOnlyAfterCommittedSharedWrite() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		PreparedStatement read = mock(PreparedStatement.class);
		ResultSet result = mock(ResultSet.class);
		doReturn(data).when(fixture.user).getUserData();
		when(fixture.statement.executeUpdate()).thenReturn(1);
		when(fixture.connection.prepareStatement(anyString())).thenReturn(fixture.statement, read);
		when(read.executeQuery()).thenReturn(result);
		when(result.next()).thenReturn(true);
		when(result.getInt(1)).thenReturn(23);
		AtomicReference<Boolean> success = new AtomicReference<>();
		AtomicReference<Integer> total = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			fixture.user.addPointsStorageAware(5, (written, committed) -> {
				success.set(written);
				total.set(committed);
			});
		}

		assertTrue(success.get() == null, "the command callback must wait for persistence");
		ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceWork.capture());
		persistenceWork.getValue().run();
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
		completion.getValue().run();
		assertEquals(Boolean.TRUE, success.get());
		assertEquals(23, total.get());
	}

	@Test
	void sharedAsyncAddReturnsThePredictedEventAdjustedTotalWithoutJdbcOnTheCaller() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		doReturn(data).when(fixture.user).getUserData();
		when(data.getInt("Points", UserDataFetchMode.TEMP_ONLY)).thenReturn(10);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				PlayerReceivePointsEvent event = invocation.getArgument(0);
				event.setPoints(7);
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

			assertEquals(17, fixture.user.addPoints(5, true));
		}

		verify(fixture.persistence).execute(any(Runnable.class));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
		verify(data).getInt("Points", UserDataFetchMode.TEMP_ONLY);
		verify(fixture.user, never()).getPoints();
	}

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
	void sharedAsyncRemoveKeepsJdbcOffTheCallerThread() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		doReturn(data).when(fixture.user).getUserData();
		when(data.getInt("Points", UserDataFetchMode.TEMP_ONLY)).thenReturn(20);
		when(fixture.statement.executeUpdate()).thenReturn(1);

		assertTrue(fixture.user.removePoints(10, true));

		ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceWork.capture());
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
		verify(data).getInt("Points", UserDataFetchMode.TEMP_ONLY);
		verify(fixture.user, never()).getPoints();
		persistenceWork.getValue().run();
		verify(fixture.sql.getConnectionManager()).getConnection();
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
	void sharedTransferRunsRecipientApprovalOnBukkitSchedulerBeforeSettlement() throws Exception {
		TransferSchedulingFixture fixture = transferSchedulingFixture();
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				PlayerReceivePointsEvent event = invocation.getArgument(0);
				event.setPoints(4);
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

			fixture.user.transferPoints(fixture.target, 10, result::set);
			ArgumentCaptor<Runnable> firstPersistence = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(firstPersistence.capture());
			firstPersistence.getValue().run();

			ArgumentCaptor<Runnable> approval = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), approval.capture());
			assertEquals(null, result.get());
			approval.getValue().run();
			verify(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

			ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(persistence.capture());
			persistence.getAllValues().get(1).run();
		}

		assertEquals(null, result.get());
	}

	@Test
	void sharedTransferReportsCompletionOnSourceEntityScheduler() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();
		AtomicReference<Thread> eventThread = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				eventThread.set(Thread.currentThread());
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			fixture.user.transferPoints(fixture.target, 10, result::set);
			ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(persistenceWork.capture());
			persistenceWork.getValue().run();
			ArgumentCaptor<Runnable> approval = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), approval.capture());
			approval.getValue().run();
			Thread bukkitThread = eventThread.get();
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(1).run();
			ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
			assertTrue(result.get() == null);
			completion.getValue().run();
			assertEquals(bukkitThread, eventThread.get(), "the receive hook must run on Bukkit's scheduler lane");
		}
		assertTrue(result.get());
	}

	@Test
	void sharedTransferCreditsTheEventAdjustedRecipientAmountAtomically() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				PlayerReceivePointsEvent event = invocation.getArgument(0);
				event.setPoints(4);
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			fixture.user.transferPoints(fixture.target, 10, result::set);

			ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(persistenceWork.capture());
			persistenceWork.getValue().run();
			ArgumentCaptor<Runnable> approval = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), approval.capture());
			approval.getValue().run();
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(1).run();
			ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
			completion.getValue().run();
			InOrder transferOrder = inOrder(fixture.debit, pluginManager, fixture.settlementPoint);
			transferOrder.verify(fixture.debit).executeUpdate();
			transferOrder.verify(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			transferOrder.verify(fixture.settlementPoint).executeUpdate();
		}

		assertTrue(result.get());
		verify(fixture.settlementPoint).setInt(1, 4);
	}

	@Test
	void sharedTransferDoesNotFireRecipientEventWhenConditionalDebitFails() throws Exception {
		SagaFixture fixture = sagaFixture(false);
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			fixture.user.transferPoints(fixture.target, 10, result::set);

			ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(persistenceWork.capture());
			persistenceWork.getValue().run();
			ArgumentCaptor<Runnable> entityWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), entityWork.capture(), eq(fixture.player));
			entityWork.getValue().run();

			verify(pluginManager, never()).callEvent(any(PlayerReceivePointsEvent.class));
			verify(fixture.settlementPoint, never()).executeUpdate();
		}

		assertFalse(result.get());
	}

	@Test
	void cancelledSharedTransferRollsBackTheConditionalDebit() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				PlayerReceivePointsEvent event = invocation.getArgument(0);
				event.setCancelled(true);
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			fixture.user.transferPoints(fixture.target, 10, result::set);

			ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(persistenceWork.capture());
			persistenceWork.getValue().run();
			ArgumentCaptor<Runnable> approval = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), approval.capture());
			approval.getValue().run();
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(1).run();
			ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
			completion.getValue().run();

			verify(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			verify(fixture.settlementPoint).executeUpdate();
			verify(fixture.settlementPoint).setInt(1, 10);
			verify(fixture.settlementPoint).setString(2, fixture.user.getUUID());
			verify(fixture.settlement).commit();
		}

		assertTrue(Boolean.FALSE.equals(result.get()));
	}

	@Test
	void sharedTransferClosesReservationBeforeListenerDatabaseReadAndSettlesAdjustment() throws Exception {
		TransferSchedulingFixture fixture = transferSchedulingFixture();
		VotingPluginUser target = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(target, fixture.plugin);
		doReturn("00000000-0000-0000-0000-000000000002").when(target).getUUID();
		doReturn("Points").when(target).getPointsPath();
		UserDataCache recreatedCache = mock(UserDataCache.class);
		doReturn(false, true).when(target).isCached();
		doReturn(recreatedCache).when(target).getCache();
		UserData targetData = mock(UserData.class);
		doReturn(targetData).when(target).getUserData();
		doAnswer(invocation -> {
			try (Connection ignored = fixture.manager.getConnection()) {
				return 37;
			}
		}).when(targetData).getInt("Points");
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
			PlayerReceivePointsEvent event = invocation.getArgument(0);
			assertEquals(37, event.getPlayer().getPoints());
			event.setPoints(4);
			return null;
		}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

			fixture.user.transferPoints(target, 10, result::set);
			ArgumentCaptor<Runnable> persistenceWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(persistenceWork.capture());
			persistenceWork.getValue().run();
			ArgumentCaptor<Runnable> approval = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), approval.capture());
			approval.getValue().run();
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(1).run();
			ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
			completion.getValue().run();
		}

		InOrder order = org.mockito.Mockito.inOrder(fixture.reservation, fixture.claim, fixture.listenerRead,
				recreatedCache, fixture.settlement, fixture.plugin.getUserManager().getDataManager());
		order.verify(fixture.reservation).close();
		order.verify(fixture.claim).close();
		order.verify(fixture.listenerRead).close();
		order.verify(recreatedCache).dump();
		order.verify(fixture.settlement).commit();
		order.verify((UserDataManager) fixture.plugin.getUserManager().getDataManager()).removeCache(
				java.util.UUID.fromString("00000000-0000-0000-0000-000000000002"), null);
		assertEquals(Boolean.TRUE, result.get());
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
		doReturn(null).when(fixture.user).getCache();
		return fixture;
	}

	private static SagaFixture sagaFixture(boolean debitSucceeds) throws Exception {
		SagaFixture fixture = new SagaFixture();
		fixture.plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.persistence = mock(ScheduledExecutorService.class);
		fixture.scheduler = mock(BukkitScheduler.class);
		fixture.player = mock(Player.class);
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class);
		fixture.manager = mock(ConnectionManager.class);
		fixture.schema = mock(Connection.class);
		fixture.recoveryReserved = mock(Connection.class);
		fixture.cleanup = mock(Connection.class);
		fixture.lookup = mock(Connection.class);
		fixture.reservation = mock(Connection.class);
		fixture.claim = mock(Connection.class);
		fixture.settlement = mock(Connection.class);
		when(fixture.plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(fixture.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(fixture.plugin.getMysql()).thenReturn(fixture.table);
		when(fixture.plugin.getTimer()).thenReturn(fixture.persistence);
		when(fixture.plugin.getBukkitScheduler()).thenReturn(fixture.scheduler);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager()).thenReturn(fixture.manager);
		when(fixture.manager.getConnection()).thenReturn(fixture.schema, fixture.recoveryReserved, fixture.cleanup,
				fixture.lookup, fixture.reservation, fixture.claim, fixture.settlement);
		when(fixture.schema.prepareStatement(anyString())).thenReturn(mock(PreparedStatement.class));
		configureJournalMaintenance(fixture.recoveryReserved, fixture.cleanup);

		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(lookup);
		PreparedStatement insert = mock(PreparedStatement.class);
		fixture.debit = mock(PreparedStatement.class);
		when(fixture.debit.executeUpdate()).thenReturn(debitSucceeds ? 1 : 0);
		when(fixture.reservation.prepareStatement(anyString())).thenReturn(insert, fixture.debit);

		PreparedStatement claimSelect = mock(PreparedStatement.class);
		fixture.claimUpdate = mock(PreparedStatement.class);
		AtomicReference<String> owner = new AtomicReference<>();
		doAnswer(invocation -> {
			owner.set(invocation.getArgument(1));
			return null;
		}).when(fixture.claimUpdate).setString(eq(2), anyString());
		ResultSet reserved = mock(ResultSet.class);
		when(reserved.next()).thenReturn(true);
		when(reserved.getString(1)).thenReturn("RESERVED");
		when(reserved.getString(2)).thenReturn(null);
		when(claimSelect.executeQuery()).thenReturn(reserved);
		when(fixture.claimUpdate.executeUpdate()).thenReturn(1);
		when(fixture.claim.prepareStatement(anyString())).thenReturn(claimSelect, fixture.claimUpdate);

		PreparedStatement settleSelect = mock(PreparedStatement.class);
		fixture.settlementPoint = mock(PreparedStatement.class);
		PreparedStatement settleJournal = mock(PreparedStatement.class);
		ResultSet started = mock(ResultSet.class);
		when(started.next()).thenReturn(true);
		when(started.getString(1)).thenReturn("HOOK_STARTED");
		when(started.getString(2)).thenAnswer(invocation -> owner.get());
		when(settleSelect.executeQuery()).thenReturn(started);
		when(fixture.settlementPoint.executeUpdate()).thenReturn(1);
		when(settleJournal.executeUpdate()).thenReturn(1);
		when(fixture.settlement.prepareStatement(anyString())).thenReturn(settleSelect, fixture.settlementPoint,
				settleJournal);

		fixture.user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(fixture.user, fixture.plugin);
		doReturn("00000000-0000-0000-0000-000000000001").when(fixture.user).getUUID();
		doReturn("Points").when(fixture.user).getPointsPath();
		doReturn(fixture.player).when(fixture.user).getPlayer();
		doReturn(false).when(fixture.user).isCached();
		fixture.target = mock(VotingPluginUser.class);
		when(fixture.target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
		when(fixture.target.getPointsPath()).thenReturn("Points");
		return fixture;
	}

	private static TransferSchedulingFixture transferSchedulingFixture() throws Exception {
		TransferSchedulingFixture fixture = new TransferSchedulingFixture();
		fixture.plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.persistence = mock(ScheduledExecutorService.class);
		fixture.scheduler = mock(BukkitScheduler.class);
		fixture.player = mock(Player.class);
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class);
		fixture.manager = mock(ConnectionManager.class);
		fixture.schema = mock(Connection.class);
		fixture.recoveryReserved = mock(Connection.class);
		fixture.cleanup = mock(Connection.class);
		fixture.lookup = mock(Connection.class);
		fixture.reservation = mock(Connection.class);
		fixture.claim = mock(Connection.class);
		fixture.listenerRead = mock(Connection.class);
		fixture.settlement = mock(Connection.class);
		when(fixture.plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(fixture.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(fixture.plugin.getMysql()).thenReturn(fixture.table);
		when(fixture.plugin.getTimer()).thenReturn(fixture.persistence);
		when(fixture.plugin.getBukkitScheduler()).thenReturn(fixture.scheduler);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager()).thenReturn(fixture.manager);
		when(fixture.manager.getConnection()).thenReturn(fixture.schema, fixture.recoveryReserved, fixture.cleanup,
				fixture.lookup, fixture.reservation, fixture.claim, fixture.listenerRead, fixture.settlement);

		when(fixture.schema.prepareStatement(anyString())).thenReturn(mock(PreparedStatement.class));
		configureJournalMaintenance(fixture.recoveryReserved, fixture.cleanup);
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(lookup);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.reservation.prepareStatement(anyString())).thenReturn(insert, debit);
		PreparedStatement claimSelect = mock(PreparedStatement.class);
		PreparedStatement claimUpdate = mock(PreparedStatement.class);
		AtomicReference<String> owner = new AtomicReference<>();
		doAnswer(invocation -> {
			owner.set(invocation.getArgument(1));
			return null;
		}).when(claimUpdate).setString(eq(2), anyString());
		ResultSet reserved = mock(ResultSet.class);
		when(reserved.next()).thenReturn(true);
		when(reserved.getString(1)).thenReturn("RESERVED");
		when(reserved.getString(2)).thenReturn(null);
		when(claimSelect.executeQuery()).thenReturn(reserved);
		when(claimUpdate.executeUpdate()).thenReturn(1);
		when(fixture.claim.prepareStatement(anyString())).thenReturn(claimSelect, claimUpdate);
		PreparedStatement settleSelect = mock(PreparedStatement.class);
		PreparedStatement settleCredit = mock(PreparedStatement.class);
		PreparedStatement settleJournal = mock(PreparedStatement.class);
		ResultSet started = mock(ResultSet.class);
		when(started.next()).thenReturn(true);
		when(started.getString(1)).thenReturn("HOOK_STARTED");
		when(started.getString(2)).thenAnswer(invocation -> owner.get());
		when(settleSelect.executeQuery()).thenReturn(started);
		when(settleCredit.executeUpdate()).thenReturn(1);
		when(settleJournal.executeUpdate()).thenReturn(1);
		when(fixture.settlement.prepareStatement(anyString())).thenReturn(settleSelect, settleCredit, settleJournal);
		fixture.user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(fixture.user, fixture.plugin);
		doReturn("00000000-0000-0000-0000-000000000001").when(fixture.user).getUUID();
		doReturn("Points").when(fixture.user).getPointsPath();
		doReturn(fixture.player).when(fixture.user).getPlayer();
		doReturn(false).when(fixture.user).isCached();
		fixture.target = mock(VotingPluginUser.class);
		when(fixture.target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
		when(fixture.target.getPointsPath()).thenReturn("Points");
		when(fixture.target.isCached()).thenReturn(false);
		return fixture;
	}

	private static void configureJournalMaintenance(Connection reservedCandidates, Connection cleanup) throws Exception {
		PreparedStatement reservedQuery = mock(PreparedStatement.class);
		PreparedStatement cleanupQuery = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		ResultSet noRows = mock(ResultSet.class);
		ResultSet noCleanupRows = mock(ResultSet.class);
		when(noRows.next()).thenReturn(false);
		when(noCleanupRows.next()).thenReturn(false);
		when(reservedCandidates.prepareStatement(anyString())).thenReturn(reservedQuery);
		when(reservedQuery.executeQuery()).thenReturn(noRows);
		when(cleanup.prepareStatement(anyString())).thenReturn(cleanupQuery, cleanupDelete);
		when(cleanupQuery.executeQuery()).thenReturn(noCleanupRows);
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

	private static final class SagaFixture {
		VotingPluginMain plugin;
		ScheduledExecutorService persistence;
		BukkitScheduler scheduler;
		Player player;
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		ConnectionManager manager;
		Connection schema;
		Connection recoveryReserved;
		Connection cleanup;
		Connection lookup;
		Connection reservation;
		Connection claim;
		Connection settlement;
		PreparedStatement debit;
		PreparedStatement claimUpdate;
		PreparedStatement settlementPoint;
		VotingPluginUser user;
		VotingPluginUser target;
	}

	private static final class TransferSchedulingFixture {
		VotingPluginMain plugin;
		ScheduledExecutorService persistence;
		BukkitScheduler scheduler;
		Player player;
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		ConnectionManager manager;
		Connection schema;
		Connection recoveryReserved;
		Connection cleanup;
		Connection lookup;
		Connection reservation;
		Connection claim;
		Connection listenerRead;
		Connection settlement;
		VotingPluginUser user;
		VotingPluginUser target;
	}

}
