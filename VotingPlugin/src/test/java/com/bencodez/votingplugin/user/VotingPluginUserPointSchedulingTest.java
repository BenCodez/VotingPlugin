package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.entity.Player;
import org.bukkit.Bukkit;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.mysql.ConnectionManager;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.folialib.FoliaLib;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.folialib.impl.ServerImplementation;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerReceivePointsEvent;

class VotingPluginUserPointSchedulingTest {
	@Test
	void sharedBulkPointMutationsUseOnePersistenceSubmission() throws Exception {
		PointFixture fixture = pointFixture();
		VotingPluginUser second = mock(VotingPluginUser.class);
		java.util.List<VotingPluginUser> users = java.util.List.of(fixture.user, second);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			VotingPluginUser.addPointsStorageAware(fixture.plugin, users, 5, (user, success) -> { });
			VotingPluginUser.setPointsStorageAware(fixture.plugin, users, 42, (user, success) -> { });
			VotingPluginUser.removePointsStorageAware(fixture.plugin, users, 3, (user, success) -> { });
		}

		verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(any(Runnable.class));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void rejectedSharedBulkMutationCompletesEveryUserAsFailed() throws Exception {
		PointFixture fixture = pointFixture();
		VotingPluginUser second = mock(VotingPluginUser.class);
		Player secondPlayer = mock(Player.class);
		when(second.getPlayer()).thenReturn(secondPlayer);
		java.util.List<Boolean> results = new java.util.ArrayList<>();
		doThrow(new RejectedExecutionException()).when(fixture.persistence).execute(any(Runnable.class));
		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(1).run();
			return null;
		}).when(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), any(Player.class));

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			VotingPluginUser.addPointsStorageAware(fixture.plugin, java.util.List.of(fixture.user, second), 5,
					(user, success) -> results.add(success));
		}

		assertEquals(java.util.List.of(false, false), results);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(secondPlayer));
	}

	@Test
	void sharedBulkAddPreservesPerUserCancellationBeforePersistence() throws Exception {
		PointFixture fixture = pointFixture();
		java.util.List<Boolean> results = new java.util.ArrayList<>();
		PluginManager pluginManager = mock(PluginManager.class);
		doAnswer(invocation -> {
			invocation.<PlayerReceivePointsEvent>getArgument(0).setCancelled(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			VotingPluginUser.addPointsStorageAware(fixture.plugin, java.util.List.of(fixture.user), 5,
					(user, success) -> results.add(success));
		}

		ArgumentCaptor<Runnable> persistenceTask = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceTask.capture());
		persistenceTask.getValue().run();
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
		verify(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
	}

	@Test
	void sharedBulkPointMutationResubmitsBoundedChunks() throws Exception {
		PointFixture fixture = pointFixture();
		PluginManager pluginManager = mock(PluginManager.class);
		doAnswer(invocation -> {
			invocation.<PlayerReceivePointsEvent>getArgument(0).setCancelled(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			VotingPluginUser.addPointsStorageAware(fixture.plugin,
					java.util.Collections.nCopies(65, fixture.user), 5, (user, success) -> { });
		}

		ArgumentCaptor<Runnable> persistenceTasks = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceTasks.capture());
		persistenceTasks.getValue().run();
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(persistenceTasks.capture());
		persistenceTasks.getAllValues().get(persistenceTasks.getAllValues().size() - 1).run();
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
		verify(fixture.scheduler, org.mockito.Mockito.times(65)).runTask(eq(fixture.plugin), any(Runnable.class),
				eq(fixture.player));
	}

	@Test
	void storageAwareSetDoesNotUseJdbcOnCallerThread() throws Exception {
		PointFixture fixture = pointFixture();
		java.util.List<Boolean> results = new java.util.ArrayList<>();

		fixture.user.setPointsStorageAware(42, results::add);

		ArgumentCaptor<Runnable> persistenceTask = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceTask.capture());
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

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
	void votePointAwardCombinesSharedAdditionAndCapInOnePersistenceTask() throws Exception {
		PointFixture fixture = pointFixture();
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> values = new HashMap<>();
		values.put("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(98));
		doReturn(true).when(fixture.user).isCached();
		doReturn(cache).when(fixture.user).getCache();
		when(cache.getCache()).thenReturn(values);
		java.util.UUID userUuid = java.util.UUID.fromString("00000000-0000-0000-0000-000000000001");
		when(fixture.plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(userUuid, cache)));
		when(fixture.plugin.getConfigFile().getPointsOnVote()).thenReturn(5);
		when(fixture.plugin.getConfigFile().getLimitVotePoints()).thenReturn(100);
		PluginManager pluginManager = mock(PluginManager.class);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			fixture.user.addPoints();
		}
		assertEquals(100, values.get("Points").getInt());

		ArgumentCaptor<Runnable> persistenceTask = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistenceTask.capture());
		verify(fixture.persistence, org.mockito.Mockito.times(1)).execute(any(Runnable.class));
		persistenceTask.getValue().run();
		assertFalse(values.containsKey("Points"));

		verify(fixture.connection).prepareStatement(org.mockito.ArgumentMatchers.argThat(
				query -> query.contains("`Points` = LEAST(`Points` + ?, ?)")));
		verify(fixture.statement).setInt(1, 5);
		verify(fixture.statement).setInt(2, 100);
	}

	@Test
	void rejectedInitialSharedTransferSubmissionCompletesAsFailure() throws Exception {
		TransferSchedulingFixture fixture = transferSchedulingFixture();
		AtomicReference<Boolean> result = new AtomicReference<>();
		doThrow(new RejectedExecutionException("stopping")).when(fixture.persistence).execute(any(Runnable.class));

		fixture.user.transferPoints(fixture.target, 10, result::set);

		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
		verifyNoInteractions(fixture.manager);
		completion.getValue().run();
		assertEquals(Boolean.FALSE, result.get());
	}

	@Test
	void indeterminateSharedTransferClaimDoesNotReportSuccessBeforeApproval() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		Connection unavailable = mock(Connection.class);
		when(unavailable.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("unavailable"));
		when(fixture.manager.getConnection()).thenReturn(fixture.schema, fixture.recoveryReserved, fixture.cleanup,
				fixture.lookup, fixture.reservation, fixture.claim).thenAnswer(invocation -> unavailable);
		doThrow(new java.sql.SQLException("claim acknowledgement lost")).when(fixture.claim).commit();
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claim = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claim.capture());
		claim.getAllValues().get(1).run();

		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
		verify(fixture.entityScheduler).runAtEntityWithFallback(eq(fixture.player), any(), any(Runnable.class));
		completion.getValue().run();
		assertEquals(Boolean.FALSE, result.get());
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
	void consecutiveSharedAsyncAddsComposeThroughTheOptimisticCache() throws Exception {
		PointFixture fixture = pointFixture();
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> values = new HashMap<>();
		values.put("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(10));
		doReturn(cache).when(fixture.user).getCache();
		doReturn(true).when(fixture.user).isCached();
		when(cache.getCache()).thenReturn(values);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			assertEquals(15, fixture.user.addPointsStorageAware(5));
			assertEquals(22, fixture.user.addPointsStorageAware(7));
		}

		assertEquals(22, values.get("Points").getInt());
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(any(Runnable.class));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void storageAwareSharedAddQueuesJdbcOffTheCallingLane() throws Exception {
		PointFixture fixture = pointFixture();
		UserData data = mock(UserData.class);
		doReturn(data).when(fixture.user).getUserData();
		when(data.getInt("Points", UserDataFetchMode.TEMP_ONLY)).thenReturn(10);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);

			assertEquals(15, fixture.user.addPointsStorageAware(5));
		}

		verify(fixture.persistence).execute(any(Runnable.class));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void rejectedAsyncAddDiscardsOptimisticPointsAndKeepsCallerAlive() throws Exception {
		PointFixture fixture = pointFixture();
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> values = new HashMap<>();
		values.put("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(10));
		doReturn(true).when(fixture.user).isCached();
		doReturn(cache).when(fixture.user).getCache();
		when(cache.getCache()).thenReturn(values);
		when(fixture.plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(
						java.util.UUID.fromString("00000000-0000-0000-0000-000000000001"), cache)));
		doThrow(new RejectedExecutionException("full")).when(fixture.persistence).execute(any(Runnable.class));

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			assertEquals(10, fixture.user.addPointsStorageAware(5));
		}

		assertFalse(values.containsKey("Points"));
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void rejectedSingleUserPointCallbacksCompleteAsFailures() throws Exception {
		PointFixture fixture = pointFixture();
		AtomicReference<Boolean> addResult = new AtomicReference<>();
		AtomicReference<Boolean> removeResult = new AtomicReference<>();
		doThrow(new RejectedExecutionException("full")).when(fixture.persistence).execute(any(Runnable.class));
		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(1).run();
			return null;
		}).when(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			fixture.user.addPointsStorageAware(5, (success, ignored) -> addResult.set(success));
			fixture.user.removePoints(5, removeResult::set);
		}

		assertEquals(Boolean.FALSE, addResult.get());
		assertEquals(Boolean.FALSE, removeResult.get());
		verify(fixture.sql.getConnectionManager(), never()).getConnection();
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
	void nullSharedPointConnectionIsReportedAsASqlFailure() throws Exception {
		PointFixture fixture = pointFixture();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn((Connection) null);

		assertFalse(fixture.user.removePoints(10));
		verify(fixture.plugin.getLogger()).severe(org.mockito.ArgumentMatchers.contains("SQLException"));
	}

	@Test
	void nullSharedAddConnectionCompletesCallbackWithoutASecondLookup() throws Exception {
		PointFixture fixture = pointFixture();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn((Connection) null);
		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(1).run();
			return null;
		}).when(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));
		AtomicReference<Boolean> success = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
			fixture.user.addPointsStorageAware(5, (written, ignored) -> success.set(written));
		}
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();

		assertEquals(Boolean.FALSE, success.get());
		verify(fixture.user, never()).getPoints();
		verify(fixture.sql.getConnectionManager()).getConnection();
	}

	@Test
	void nullTransferJournalConnectionCompletesTheTransferAsFailure() throws Exception {
		TransferSchedulingFixture fixture = transferSchedulingFixture();
		when(fixture.manager.getConnection()).thenReturn((Connection) null);
		AtomicReference<Boolean> result = new AtomicReference<>();
		doAnswer(invocation -> {
			invocation.<Runnable>getArgument(1).run();
			return null;
		}).when(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();

		assertEquals(Boolean.FALSE, result.get());
		verify(fixture.plugin.getLogger()).severe(org.mockito.ArgumentMatchers.contains("SQLException"));
	}

	@Test
	void sharedAbsoluteSetUsesTheDirectMysqlMutator() throws Exception {
		PointFixture fixture = pointFixture();
		UserData userData = mock(UserData.class);
		doReturn(userData).when(fixture.user).getUserData();

		fixture.user.setPoints(42);

		verify(fixture.statement).setInt(1, 42);
		verify(fixture.statement).executeUpdate();
		verify(userData, never()).setInt(anyString(), eq(42), eq(false));
	}

	@Test
	void sharedPointMutationInvalidatesOnlyPointsFromACacheRecreatedDuringJdbc() throws Exception {
		PointFixture fixture = pointFixture();
		when(fixture.statement.executeUpdate()).thenReturn(1);
		UserDataCache recreatedCache = mock(UserDataCache.class);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> values = new HashMap<>();
		values.put("Points", mock(com.bencodez.simpleapi.sql.data.DataValue.class));
		values.put("VoteStreak", mock(com.bencodez.simpleapi.sql.data.DataValue.class));
		when(recreatedCache.getCache()).thenReturn(values);
		doReturn(false, true).when(fixture.user).isCached();
		doReturn(recreatedCache).when(fixture.user).getCache();
		java.util.UUID userUuid = java.util.UUID.fromString(fixture.user.getUUID());
		when(fixture.plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(userUuid, recreatedCache)));

		assertTrue(fixture.user.removePoints(10));

		assertFalse(values.containsKey("Points"));
		assertTrue(values.containsKey("VoteStreak"));
		verify(fixture.plugin.getUserManager().getDataManager(), never()).removeCache(any(), any());
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
				verify(fixture.claim, never()).prepareStatement(any(String.class));
				assertEquals(null, result.get());
			approval.getValue().run();
			ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(persistence.capture());
			persistence.getAllValues().get(1).run();
			@SuppressWarnings("rawtypes")
			ArgumentCaptor<java.util.function.Consumer> event = ArgumentCaptor.forClass(java.util.function.Consumer.class);
			verify(fixture.entityScheduler).runAtEntityWithFallback(eq(fixture.targetPlayer), event.capture(), any(Runnable.class));
			event.getValue().accept(null);
			verify(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));

			ArgumentCaptor<Runnable> settlement = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(settlement.capture());
			settlement.getAllValues().get(2).run();
		}

		assertEquals(null, result.get());
	}

	@Test
	void rejectedApprovalSettlementSubmissionUsesBukkitAsyncFallbackWithApprovedAmount() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			doAnswer(invocation -> {
				invocation.<PlayerReceivePointsEvent>getArgument(0).setPoints(4);
				return null;
			}).when(pluginManager).callEvent(any(PlayerReceivePointsEvent.class));
			fixture.user.transferPoints(fixture.target, 10, result::set);
			ArgumentCaptor<Runnable> reservation = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(reservation.capture());
			reservation.getValue().run();
			ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
			gate.getValue().run();
			ArgumentCaptor<Runnable> claim = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claim.capture());
			claim.getAllValues().get(1).run();
			doThrow(new RejectedExecutionException("stopping")).when(fixture.persistence).execute(any(Runnable.class));
			@SuppressWarnings("rawtypes")
			ArgumentCaptor<java.util.function.Consumer> approval = ArgumentCaptor.forClass(java.util.function.Consumer.class);
			verify(fixture.entityScheduler).runAtEntityWithFallback(eq(fixture.targetPlayer), approval.capture(), any(Runnable.class));
			approval.getValue().accept(null);

			ArgumentCaptor<Runnable> asyncSettlement = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTaskAsynchronously(eq(fixture.plugin), asyncSettlement.capture());
			verify(fixture.settlementPoint, never()).executeUpdate();
			asyncSettlement.getValue().run();
			verify(fixture.settlementPoint).setInt(1, 4);
			verify(fixture.settlementPoint).executeUpdate();
			ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
			completion.getValue().run();
		}

		assertEquals(Boolean.TRUE, result.get());
	}

	@Test
	void rejectedApprovalSettlementSchedulersRetainHookStartedForReconciliation() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> reservation = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(reservation.capture());
		reservation.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claim = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claim.capture());
		claim.getAllValues().get(1).run();
		doThrow(new RejectedExecutionException("stopping")).when(fixture.persistence).execute(any(Runnable.class));
		doThrow(new RejectedExecutionException("disabling")).when(fixture.scheduler)
				.runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
		@SuppressWarnings("rawtypes")
		ArgumentCaptor<java.util.function.Consumer> approval = ArgumentCaptor.forClass(java.util.function.Consumer.class);
		verify(fixture.entityScheduler).runAtEntityWithFallback(eq(fixture.targetPlayer), approval.capture(), any(Runnable.class));
		approval.getValue().accept(null);

		verify(fixture.scheduler).runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
		verify(fixture.settlementPoint, never()).executeUpdate();
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
		completion.getValue().run();
		assertEquals(Boolean.TRUE, result.get());
	}

	@Test
	void retiredApprovalSchedulerRefundsClaimedTransferBeforeTheHookCanRun() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		configureRejectedSagaConnections(fixture);
		when(fixture.entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claimed = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claimed.capture());
		claimed.getAllValues().get(1).run();
		ArgumentCaptor<Runnable> refunded = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(refunded.capture());
		refunded.getAllValues().get(2).run();

		verify(fixture.settlementPoint).setInt(1, 10);
		verify(fixture.settlementPoint).executeUpdate();
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler, org.mockito.Mockito.times(2)).runTask(eq(fixture.plugin), completion.capture());
		completion.getAllValues().get(1).run();
		assertEquals(Boolean.FALSE, result.get());
	}

	@Test
	void rejectedClaimedTransferCompensationUsesAsyncFallback() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		configureRejectedSagaConnections(fixture);
		when(fixture.entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claimed = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claimed.capture());
		doThrow(new RejectedExecutionException("stopping")).when(fixture.persistence).execute(any(Runnable.class));
		claimed.getAllValues().get(1).run();

		ArgumentCaptor<Runnable> asyncRefund = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTaskAsynchronously(eq(fixture.plugin), asyncRefund.capture());
		verify(fixture.settlementPoint, never()).executeUpdate();
		asyncRefund.getValue().run();
		verify(fixture.settlementPoint).setInt(1, 10);
		verify(fixture.settlementPoint).executeUpdate();
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler, org.mockito.Mockito.times(2)).runTask(eq(fixture.plugin), completion.capture());
		completion.getAllValues().get(1).run();
		assertEquals(Boolean.FALSE, result.get());
	}

	@Test
	void rejectedClaimedTransferRetainsDurableCompensationWhenBothFallbackSchedulersReject(
			@TempDir Path temporaryDirectory) throws Exception {
		SagaFixture fixture = sagaFixture(true);
		when(fixture.plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		configureRejectedSagaConnections(fixture);
		when(fixture.entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claimed = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claimed.capture());
		doThrow(new RejectedExecutionException("stopping")).when(fixture.persistence).execute(any(Runnable.class));
		doThrow(new RejectedExecutionException("disabling")).when(fixture.scheduler)
				.runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
		claimed.getAllValues().get(1).run();

		verify(fixture.compensationUpdate, never()).setString(anyInt(), anyString());
		verify(fixture.scheduler).runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
		verify(fixture.settlementPoint, never()).executeUpdate();
		assertEquals(1, new SharedPointTransferCompensationStore(temporaryDirectory).loadBatch().size());
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler, org.mockito.Mockito.times(2)).runTask(eq(fixture.plugin), completion.capture());
		completion.getAllValues().get(1).run();
		assertEquals(Boolean.FALSE, result.get());
	}

	@Test
	void failedClaimedTransferCompensationKeepsRecoveryMarkerWhenMysqlFenceAndRefundFail(
			@TempDir Path temporaryDirectory) throws Exception {
		SagaFixture fixture = sagaFixture(true);
		when(fixture.plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		configureRejectedSagaConnections(fixture);
		when(fixture.entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));

		fixture.user.transferPoints(fixture.target, 10, ignored -> { });
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		gate.getValue().run();
		ArgumentCaptor<Runnable> claimed = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(2)).execute(claimed.capture());
		claimed.getAllValues().get(1).run();
		ArgumentCaptor<Runnable> compensation = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(compensation.capture());

		Connection unavailable = mock(Connection.class);
		doThrow(new java.sql.SQLException("database unavailable")).when(unavailable).prepareStatement(anyString());
		when(fixture.manager.getConnection()).thenReturn(unavailable);
		compensation.getAllValues().get(2).run();

		assertEquals(1, new SharedPointTransferCompensationStore(temporaryDirectory).loadBatch().size());
	}

	@Test
	void failedTransferCompensationMarkerIsRetriedByRecovery(@TempDir Path temporaryDirectory) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		SharedPointTransferJournal journal = mock(SharedPointTransferJournal.class);
		when(journal.markCompensating("transfer-1"))
				.thenThrow(new java.sql.SQLException("down"))
				.thenReturn(true);
		when(journal.recoverAndCleanup(anyLong())).thenReturn(java.util.List.of());
		SharedPointTransferCompensationStore store =
				new SharedPointTransferCompensationStore(temporaryDirectory);
		store.record("transfer-1");

		SharedMysqlPointMutator.recoverTransfers(plugin, journal);
		assertEquals(java.util.List.of("transfer-1"), store.loadBatch());
		SharedMysqlPointMutator.recoverTransfers(plugin, journal);

		assertTrue(store.loadBatch().isEmpty());
		verify(journal, org.mockito.Mockito.times(2)).markCompensating("transfer-1");
		verify(journal, org.mockito.Mockito.times(2)).recoverAndCleanup(anyLong());
	}

	@Test
	void rejectedPersistenceClaimLeavesReservedTransferForOffThreadRecovery() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		AtomicReference<Boolean> result = new AtomicReference<>();

		fixture.user.transferPoints(fixture.target, 10, result::set);
		ArgumentCaptor<Runnable> persistence = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.persistence).execute(persistence.capture());
		persistence.getValue().run();

		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), gate.capture());
		doThrow(new RejectedExecutionException("full")).when(fixture.persistence).execute(any(Runnable.class));
		gate.getValue().run();

		// The gate runs on Bukkit's lane. A rejected persistence submission must
		// not synchronously acquire JDBC to refund; the durable RESERVED row is
		// recovered by the existing off-thread periodic/startup recovery.
		verifyNoInteractions(fixture.claim);
		ArgumentCaptor<Runnable> completion = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), completion.capture(), eq(fixture.player));
		completion.getValue().run();
		assertEquals(Boolean.FALSE, result.get());
	}

	@Test
	void offlineTargetApprovalFallsBackToTheOnlineSourceEntityLane() throws Exception {
		SagaFixture fixture = sagaFixture(true);
		when(fixture.target.getPlayer()).thenReturn(null);
		AtomicReference<Boolean> result = new AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			PluginManager pluginManager = mock(PluginManager.class);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);
			fixture.user.transferPoints(fixture.target, 10, result::set);
			ArgumentCaptor<Runnable> reservation = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence).execute(reservation.capture());
			reservation.getValue().run();
			runTransferApprovalGate(fixture.persistence, fixture.scheduler, fixture.plugin,
					fixture.entityScheduler, fixture.player);
		}

		verify(fixture.entityScheduler).runAtEntityWithFallback(eq(fixture.player), any(), any(Runnable.class));
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
			runTransferApprovalGate(fixture.persistence, fixture.scheduler, fixture.plugin,
					fixture.entityScheduler, fixture.targetPlayer);
			Thread bukkitThread = eventThread.get();
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(2).run();
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
			runTransferApprovalGate(fixture.persistence, fixture.scheduler, fixture.plugin,
					fixture.entityScheduler, fixture.targetPlayer);
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(2).run();
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
			runTransferApprovalGate(fixture.persistence, fixture.scheduler, fixture.plugin,
					fixture.entityScheduler, fixture.targetPlayer);
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(2).run();
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
		doReturn(fixture.targetPlayer).when(target).getPlayer();
		UserDataCache recreatedCache = mock(UserDataCache.class);
		doReturn(false, true).when(target).isCached();
		doReturn(recreatedCache).when(target).getCache();
		java.util.HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> recreatedValues = new java.util.HashMap<>();
		recreatedValues.put("Points", mock(com.bencodez.simpleapi.sql.data.DataValue.class));
		recreatedValues.put("DailyTotal", mock(com.bencodez.simpleapi.sql.data.DataValue.class));
		when(recreatedCache.getCache()).thenReturn(recreatedValues);
		when(fixture.plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(
						java.util.UUID.fromString("00000000-0000-0000-0000-000000000002"), recreatedCache)));
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
			runTransferApprovalGate(fixture.persistence, fixture.scheduler, fixture.plugin,
					fixture.entityScheduler, fixture.targetPlayer);
			ArgumentCaptor<Runnable> settlementWork = ArgumentCaptor.forClass(Runnable.class);
			verify(fixture.persistence, org.mockito.Mockito.times(3)).execute(settlementWork.capture());
			settlementWork.getAllValues().get(2).run();
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
		order.verify(recreatedCache).getCache();
		assertFalse(recreatedValues.containsKey("Points"));
		assertTrue(recreatedValues.containsKey("DailyTotal"),
				"settlement must preserve unrelated changes in a concurrently recreated cache");
		assertEquals(Boolean.TRUE, result.get());
	}

	/** Runs the gate, off-thread journal claim, and Bukkit approval callback in order. */
	private static void runTransferApprovalGate(ScheduledExecutorService persistence, BukkitScheduler scheduler,
			VotingPluginMain plugin, ServerImplementation entityScheduler, Player player) {
		ArgumentCaptor<Runnable> gate = ArgumentCaptor.forClass(Runnable.class);
		verify(scheduler).runTask(eq(plugin), gate.capture());
		gate.getValue().run();

		ArgumentCaptor<Runnable> claim = ArgumentCaptor.forClass(Runnable.class);
		verify(persistence, org.mockito.Mockito.times(2)).execute(claim.capture());
		claim.getAllValues().get(1).run();

		@SuppressWarnings("rawtypes")
		ArgumentCaptor<java.util.function.Consumer> approval = ArgumentCaptor.forClass(java.util.function.Consumer.class);
		verify(entityScheduler).runAtEntityWithFallback(eq(player), approval.capture(), any(Runnable.class));
		approval.getValue().accept(null);
	}

	private static PointFixture pointFixture() throws Exception {
		PointFixture fixture = new PointFixture();
		fixture.plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.persistence = mock(ScheduledExecutorService.class);
		fixture.scheduler = mock(BukkitScheduler.class);
		fixture.entityScheduler = configureEntityScheduler(fixture.plugin, fixture.scheduler);
		fixture.player = mock(Player.class);
		fixture.targetPlayer = mock(Player.class);
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
		fixture.entityScheduler = configureEntityScheduler(fixture.plugin, fixture.scheduler);
		fixture.player = mock(Player.class);
		fixture.targetPlayer = mock(Player.class);
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class);
		fixture.manager = mock(ConnectionManager.class);
		fixture.schema = mock(Connection.class);
		fixture.recoveryReserved = mock(Connection.class);
		fixture.cleanup = mock(Connection.class);
		fixture.lookup = mock(Connection.class);
		fixture.reservation = mock(Connection.class);
		fixture.claim = mock(Connection.class);
		fixture.compensation = mock(Connection.class);
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
		fixture.compensationUpdate = mock(PreparedStatement.class);
		when(fixture.compensationUpdate.executeUpdate()).thenReturn(1);
		when(fixture.compensation.prepareStatement(anyString())).thenReturn(fixture.compensationUpdate);

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
		when(fixture.target.getPlayer()).thenReturn(fixture.targetPlayer);
		return fixture;
	}

	private static ServerImplementation configureEntityScheduler(VotingPluginMain plugin, BukkitScheduler scheduler) {
		FoliaLib folia = mock(FoliaLib.class);
		ServerImplementation entityScheduler = mock(ServerImplementation.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SUCCESS));
		return entityScheduler;
	}

	private static TransferSchedulingFixture transferSchedulingFixture() throws Exception {
		TransferSchedulingFixture fixture = new TransferSchedulingFixture();
		fixture.plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.persistence = mock(ScheduledExecutorService.class);
		fixture.scheduler = mock(BukkitScheduler.class);
		fixture.entityScheduler = configureEntityScheduler(fixture.plugin, fixture.scheduler);
		fixture.player = mock(Player.class);
		fixture.targetPlayer = mock(Player.class);
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
		when(fixture.target.getPlayer()).thenReturn(fixture.targetPlayer);
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

	private static void configureRejectedSagaConnections(SagaFixture fixture) {
		when(fixture.manager.getConnection()).thenReturn(fixture.schema, fixture.recoveryReserved, fixture.cleanup,
				fixture.lookup, fixture.reservation, fixture.claim, fixture.compensation, fixture.settlement);
	}

	private static final class PointFixture {
		VotingPluginMain plugin;
		ScheduledExecutorService persistence;
		BukkitScheduler scheduler;
		ServerImplementation entityScheduler;
		Player player;
		Player targetPlayer;
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
		ServerImplementation entityScheduler;
		Player player;
		Player targetPlayer;
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		ConnectionManager manager;
		Connection schema;
		Connection recoveryReserved;
		Connection cleanup;
		Connection lookup;
		Connection reservation;
		Connection claim;
		Connection compensation;
		Connection settlement;
		PreparedStatement debit;
		PreparedStatement claimUpdate;
		PreparedStatement compensationUpdate;
		PreparedStatement settlementPoint;
		VotingPluginUser user;
		VotingPluginUser target;
	}

	private static final class TransferSchedulingFixture {
		VotingPluginMain plugin;
		ScheduledExecutorService persistence;
		BukkitScheduler scheduler;
		ServerImplementation entityScheduler;
		Player player;
		Player targetPlayer;
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
