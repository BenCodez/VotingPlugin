package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.ArgumentMatchers.any;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.votingplugin.VotingPluginMain;

class SharedMysqlPointMutatorTest {
	@Test
	void pointMutationDumpHoldsTheSharedLimitResetFence() throws Exception {
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
		when(statement.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		when(user.isCached()).thenReturn(true);
		UserDataCache cache = mock(UserDataCache.class);
		when(user.getCache()).thenReturn(cache);

		java.util.concurrent.CountDownLatch dumpEntered = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch releaseDump = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch resetEntered = new java.util.concurrent.CountDownLatch(1);
		org.mockito.Mockito.doAnswer(invocation -> {
			dumpEntered.countDown();
			assertTrue(releaseDump.await(2, TimeUnit.SECONDS));
			return null;
		}).when(cache).dump();
		java.util.concurrent.ExecutorService workers = java.util.concurrent.Executors.newFixedThreadPool(2);
		try {
			java.util.concurrent.Future<Boolean> mutation = workers.submit(
					() -> new SharedMysqlPointMutator(plugin).setCommitted(user, 20));
			assertTrue(dumpEntered.await(1, TimeUnit.SECONDS));
			java.util.concurrent.Future<?> reset = workers.submit(
					() -> SharedMysqlCacheReconciler.withResetFence(resetEntered::countDown));

			assertFalse(resetEntered.await(100, TimeUnit.MILLISECONDS),
					"a limit reset must wait for an in-flight point-mutation cache dump");
			releaseDump.countDown();
			assertTrue(mutation.get(1, TimeUnit.SECONDS));
			reset.get(1, TimeUnit.SECONDS);
			assertEquals(0, resetEntered.getCount());
		} finally {
			releaseDump.countDown();
			workers.shutdownNow();
		}
	}

	@Test
	void indeterminateTransferReservationInvalidatesRecreatedSourcePoints() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(null);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getTimer()).thenReturn(persistence);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		org.mockito.Mockito.doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));

		String sourceUuid = "00000000-0000-0000-0000-000000000001";
		VotingPluginUser source = mock(VotingPluginUser.class);
		when(source.getUUID()).thenReturn(sourceUuid);
		when(source.getPointsPath()).thenReturn("Points");
		VotingPluginUser target = mock(VotingPluginUser.class);
		when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
		when(target.getPointsPath()).thenReturn("Points");
		UserDataCache recreatedCache = mock(UserDataCache.class);
		HashMap<String, DataValue> recreatedValues = new HashMap<>();
		recreatedValues.put("Points", new DataValueInt(20));
		recreatedValues.put("DailyTotal", new DataValueInt(4));
		when(recreatedCache.getCache()).thenReturn(recreatedValues);
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(UUID.fromString(sourceUuid), recreatedCache)));
		AtomicReference<PointTransferResult> result = new AtomicReference<>();

		new SharedMysqlPointMutator(plugin).transferWithBukkitApproval(source, target, 10, value -> value, result::set);
		ArgumentCaptor<Runnable> reservation = ArgumentCaptor.forClass(Runnable.class);
		verify(persistence).execute(reservation.capture());
		reservation.getValue().run();

		assertFalse(recreatedValues.containsKey("Points"));
		assertTrue(recreatedValues.containsKey("DailyTotal"));
		assertEquals(PointTransferResult.UNAVAILABLE, result.get());
	}

	@Test
	void indeterminateClaimedRefundStillInvalidatesSourcePoints() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		org.mockito.Mockito.doAnswer(invocation -> {
			invocation.<Runnable>getArgument(1).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		VotingPluginUser source = mock(VotingPluginUser.class);
		String sourceUuid = "00000000-0000-0000-0000-000000000001";
		when(source.getUUID()).thenReturn(sourceUuid);
		when(source.getPointsPath()).thenReturn("Points");
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("Points", new DataValueInt(10));
		when(cache.getCache()).thenReturn(values);
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(
						java.util.UUID.fromString(sourceUuid), cache)));
		SharedPointTransferJournal journal = mock(SharedPointTransferJournal.class);
		when(journal.refundHookStarted("transfer-1", source.getUUID(), "Points", 10))
				.thenThrow(new java.sql.SQLException("lost acknowledgement and confirmation"));
		AtomicReference<PointTransferResult> result = new AtomicReference<>();

		new SharedMysqlPointMutator(plugin).refundClaimedAfterSchedulingFailure(source, result::set, journal,
				"transfer-1", "Points", 10, new RejectedExecutionException("worker stopped"));

		assertFalse(values.containsKey("Points"));
		assertEquals(PointTransferResult.UNAVAILABLE, result.get());
	}

	@Test
	void recoveryInvalidatesOnlyRefundedColumnsAfterJdbcCompletes() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("Points", mock(DataValue.class));
		values.put("VoteShopLimitdaily", mock(DataValue.class));
		values.put("DailyTotal", mock(DataValue.class));
		when(plugin.getUserManager().getDataManager().getUserDataCache())
				.thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(
						java.util.UUID.fromString("00000000-0000-0000-0000-000000000001"), cache)));
		when(cache.getCache()).thenReturn(values);

		SharedMysqlCacheReconciler.invalidate(plugin, "00000000-0000-0000-0000-000000000001", "Points",
				"VoteShopLimitdaily");

		assertFalse(values.containsKey("Points"));
		assertFalse(values.containsKey("VoteShopLimitdaily"));
		assertTrue(values.containsKey("DailyTotal"));
	}

	@Test
	void userManagerSchedulesOneBoundedSharedTransferRecoveryPerLifecycle() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getTimer()).thenReturn(persistence);

		UserManager manager = new UserManager(plugin);
		manager.startSharedPointTransferRecovery();
		manager.startSharedPointTransferRecovery();

		verify(persistence).execute(any(Runnable.class));
		verify(persistence).scheduleWithFixedDelay(any(Runnable.class), org.mockito.ArgumentMatchers.eq(1L),
				org.mockito.ArgumentMatchers.eq(1L), org.mockito.ArgumentMatchers.eq(TimeUnit.MINUTES));
	}

	@Test
	void userManagerSchedulesRecoveryOnceWhenReloadEnablesSharedPoints() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true, false);
		when(plugin.getTimer()).thenReturn(persistence);

		UserManager manager = new UserManager(plugin);
		manager.startSharedPointTransferRecovery(); // Startup with per-server points.
		verifyNoInteractions(persistence);
		manager.startSharedPointTransferRecovery(); // Reload switches to shared points.
		manager.startSharedPointTransferRecovery(); // Later reload must not duplicate lifecycle work.

		verify(persistence, times(1)).execute(any(Runnable.class));
		verify(persistence, times(1)).scheduleWithFixedDelay(any(Runnable.class),
				org.mockito.ArgumentMatchers.eq(1L), org.mockito.ArgumentMatchers.eq(1L),
				org.mockito.ArgumentMatchers.eq(TimeUnit.MINUTES));
	}

	@Test
	void rejectedRecoverySchedulingCanRetryLater() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getTimer()).thenReturn(persistence);
		doThrow(new RejectedExecutionException("stopping")).doNothing()
				.when(persistence).execute(any(Runnable.class));

		UserManager manager = new UserManager(plugin);
		manager.startSharedPointTransferRecovery();
		manager.startSharedPointTransferRecovery();

		verify(persistence, times(2)).execute(any(Runnable.class));
		verify(persistence).scheduleWithFixedDelay(any(Runnable.class), org.mockito.ArgumentMatchers.eq(1L),
				org.mockito.ArgumentMatchers.eq(1L), org.mockito.ArgumentMatchers.eq(TimeUnit.MINUTES));
	}

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
	void pointMutationToleratesCacheRemovalDuringDatabaseWrite() throws Exception {
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
		when(statement.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		when(user.isCached()).thenReturn(false, true);

		assertTrue(new SharedMysqlPointMutator(plugin).remove(user, 10));

		verify(statement).executeUpdate();
		verify(plugin.getUserManager().getDataManager(), never()).removeCache(any(), any());
	}

	@Test
	void asynchronousRemoveDoesNotAcquireJdbcOnTheCallerThread() throws Exception {
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
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UserData data = mock(UserData.class);
		when(user.getUserData()).thenReturn(data);
		when(data.getInt("Points", UserDataFetchMode.TEMP_ONLY)).thenReturn(20);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");

		assertTrue(new SharedMysqlPointMutator(plugin).remove(user, 10, true));
		ArgumentCaptor<Runnable> work = ArgumentCaptor.forClass(Runnable.class);
		verify(persistence).execute(work.capture());
		verify(sql.getConnectionManager(), never()).getConnection();
		verify(data).getInt("Points", UserDataFetchMode.TEMP_ONLY);

		work.getValue().run();
		verify(sql.getConnectionManager()).getConnection();
		verify(statement).executeUpdate();
	}

	@Test
	void asynchronousAddUsesOnlyCachedPointsOnTheCallerThread() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UserDataCache cache = mock(UserDataCache.class);
		DataValue points = mock(DataValue.class);
		java.util.HashMap<String, DataValue> values = new java.util.HashMap<>();
		values.put("Points", points);
		when(user.getCache()).thenReturn(cache);
		when(user.isCached()).thenReturn(true);
		when(cache.getCache()).thenReturn(values);
		when(points.isInt()).thenReturn(true);
		when(points.getInt()).thenReturn(20);
		when(user.getPointsPath()).thenReturn("Points");

		assertEquals(30, new SharedMysqlPointMutator(plugin).add(user, 10, true));

		verify(cache, times(3)).getCache();
		assertEquals(30, values.get("Points").getInt());
		verify(user, never()).getPoints();
		verify(persistence).execute(any(Runnable.class));
	}

	@Test
	void asynchronousAddDoesNotFlushItsOptimisticPointsPrediction() throws Exception {
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
		when(statement.executeUpdate()).thenReturn(1);

		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		when(user.isCached()).thenReturn(true);
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("Points", new DataValueInt(20));
		values.put("DailyTotal", new DataValueInt(4));
		when(user.getCache()).thenReturn(cache);
		when(cache.getCache()).thenReturn(values);
		org.mockito.Mockito.doAnswer(invocation -> {
			assertFalse(values.containsKey("Points"), "the predicted value must not be persisted by dump");
			assertTrue(values.containsKey("DailyTotal"), "unrelated pending values must still be flushed");
			return null;
		}).when(cache).dump();

		assertEquals(30, new SharedMysqlPointMutator(plugin).add(user, 10, true));
		assertEquals(30, values.get("Points").getInt());
		ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
		verify(persistence).execute(task.capture());

		task.getValue().run();

		verify(cache).dump();
		verify(statement).executeUpdate();
	}

	@Test
	void addUsesAtomicDatabaseArithmeticInsteadOfAnAbsoluteCachedWrite() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		java.sql.ResultSet result = mock(java.sql.ResultSet.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement, read);
		when(result.next()).thenReturn(true);
		when(result.getInt(1)).thenReturn(73);
		when(read.executeQuery()).thenReturn(result);
		when(statement.executeUpdate()).thenReturn(1);

		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getMysql()).thenReturn(table);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		UserData data = mock(UserData.class);
		when(user.getUserData()).thenReturn(data);
		when(data.getInt("Points", UserDataFetchMode.NO_CACHE)).thenReturn(10);

		assertEquals(73, new SharedMysqlPointMutator(plugin).add(user, 10, false));

		ArgumentCaptor<String> query = ArgumentCaptor.forClass(String.class);
		verify(connection, times(2)).prepareStatement(query.capture());
		assertTrue(query.getAllValues().get(0).contains("`Points` = `Points` + ?"));
		assertTrue(query.getAllValues().get(1).contains("SELECT `Points`"));
		verify(statement).setInt(1, 10);
		verify(statement).executeUpdate();
		verify(read).executeQuery();
		verify(data, never()).getInt("Points", UserDataFetchMode.NO_CACHE);
		verify(persistence, never()).execute(any(Runnable.class));
	}

	@Test
	void committedAddIsNotReportedRetryableWhenFollowUpReadFails() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement update = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(update, read);
		when(update.executeUpdate()).thenReturn(1);
		when(read.executeQuery()).thenThrow(new java.sql.SQLException("connection lost after update"));
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		when(user.getPoints()).thenReturn(10);

		SharedMysqlPointMutator.AddResult result = new SharedMysqlPointMutator(plugin).addCommitted(user, 5);

		assertTrue(result.success(), "a committed update must not invite a duplicate retry");
		assertEquals(10, result.total(), "the stale total is safer than reporting a retryable failure");
		verify(update).executeUpdate();
		verify(read).executeQuery();
		org.mockito.InOrder closeBeforeFallback = inOrder(connection, user);
		closeBeforeFallback.verify(connection).close();
		closeBeforeFallback.verify(user).getPoints();
	}

	@Test
	void committedAddDefersEmptyReadFallbackUntilConnectionCloses() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement update = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		java.sql.ResultSet empty = mock(java.sql.ResultSet.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(update, read);
		when(update.executeUpdate()).thenReturn(1);
		when(read.executeQuery()).thenReturn(empty);
		when(empty.next()).thenReturn(false);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getMysql()).thenReturn(table);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		when(user.getPoints()).thenReturn(17);

		SharedMysqlPointMutator.AddResult result = new SharedMysqlPointMutator(plugin).addCommitted(user, 5);

		assertTrue(result.success());
		assertEquals(17, result.total());
		org.mockito.InOrder closeBeforeFallback = inOrder(connection, user);
		closeBeforeFallback.verify(connection).close();
		closeBeforeFallback.verify(user).getPoints();
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
	void addAndCapUsesOneAtomicPersistenceMutation() throws Exception {
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
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("Points", new DataValueInt(95));
		when(user.isCached()).thenReturn(true);
		when(user.getCache()).thenReturn(cache);
		when(cache.getCache()).thenReturn(values);
		UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(uuid, cache)));
		org.mockito.Mockito.doAnswer(invocation -> {
			assertFalse(values.containsKey("Points"), "the capped prediction must not be dumped before SQL caps it");
			return null;
		}).when(cache).dump();

		new SharedMysqlPointMutator(plugin).addAndCap(user, 10, 100, true);
		assertEquals(100, values.get("Points").getInt());

		ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
		verify(persistence).execute(task.capture());
		verify(persistence, times(1)).execute(any(Runnable.class));
		verify(sql.getConnectionManager(), never()).getConnection();
		task.getValue().run();

		ArgumentCaptor<String> query = ArgumentCaptor.forClass(String.class);
		verify(connection).prepareStatement(query.capture());
		assertTrue(query.getValue().contains("`Points` = LEAST(`Points` + ?, ?)"));
		verify(statement).setInt(1, 10);
		verify(statement).setInt(2, 100);
		verify(statement).setString(3, "00000000-0000-0000-0000-000000000001");
		verify(statement).executeUpdate();
		assertFalse(values.containsKey("Points"));
	}

	@Test
	void rejectedAddAndCapSubmissionDiscardsItsPrediction() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService persistence = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistence);
		doThrow(new RejectedExecutionException("saturated")).when(persistence).execute(any(Runnable.class));
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("Points", new DataValueInt(95));
		when(user.isCached()).thenReturn(true);
		when(user.getCache()).thenReturn(cache);
		when(cache.getCache()).thenReturn(values);
		UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(uuid, cache)));

		new SharedMysqlPointMutator(plugin).addAndCap(user, 10, 100, true);

		assertFalse(values.containsKey("Points"));
		verify(plugin.getMysql(), never()).getMysql();
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
