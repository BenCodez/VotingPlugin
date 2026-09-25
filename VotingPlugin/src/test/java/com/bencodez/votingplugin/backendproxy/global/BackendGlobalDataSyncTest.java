package com.bencodez.votingplugin.backendproxy.global;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChecker;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.BungeeSettings;

class BackendGlobalDataSyncTest {

	@Test
	void bungeeForceUpdateStateMutationRunsOnTheBukkitScheduler() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalDataHandler globalDataHandler = mock(GlobalDataHandler.class);
		UserDataManager dataManager = mock(UserDataManager.class);
		UserManager userManager = mock(UserManager.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		AtomicReference<Runnable> asyncWrite = new AtomicReference<>();
		AtomicInteger schedulerRuns = new AtomicInteger();
		CompletableFuture<Void> cacheClear = new CompletableFuture<>();

		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(userManager.getDataManager()).thenReturn(dataManager);
		when(dataManager.clearCacheAsyncCompletion()).thenReturn(cacheClear);

		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			schedulerRuns.incrementAndGet();
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			asyncWrite.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));

		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> {
		});
		setField(sync, "globalDataHandler", globalDataHandler);

		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("ForceUpdate", new DataValueBoolean(true));
		when(globalDataHandler.getExact("lobby")).thenReturn(data);

		sync.checkGlobalData();

		verify(plugin, never()).getMysql();
		verify(plugin).getBukkitScheduler();
		verify(globalDataHandler, never()).setBoolean("lobby", "ForceUpdate", false);
		verify(plugin, never()).setUpdate(anyBoolean());
		verify(plugin, never()).update();
		verify(dataManager, never()).clearCacheAsyncCompletion();
		assertNotNull(scheduled.get());
		assertTrue(schedulerRuns.get() == 1);

		sync.checkGlobalData();
		assertTrue(schedulerRuns.get() == 1);
		verify(plugin, never()).setUpdate(anyBoolean());
		verify(plugin, never()).update();
		verify(dataManager, never()).clearCacheAsyncCompletion();

		scheduled.get().run();
		verify(dataManager).clearCacheAsyncCompletion();
		verify(plugin, never()).setUpdate(anyBoolean());
		verify(plugin, never()).update();
		org.junit.jupiter.api.Assertions.assertNull(asyncWrite.get());

		cacheClear.complete(null);
		assertNotNull(asyncWrite.get());
		verify(plugin, never()).update();
		asyncWrite.get().run();
		verify(plugin).setUpdate(true);
		verify(plugin).update();
		verify(globalDataHandler).setBoolean("lobby", "ForceUpdate", false);

		sync.checkGlobalData();
		assertTrue(schedulerRuns.get() == 2);
	}

	@Test
	void reloadCancelsAPendingForceUpdateBeforeRetry() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalDataHandler oldHandler = mock(GlobalDataHandler.class);
		GlobalDataHandler replacementHandler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		UserDataManager dataManager = mock(UserDataManager.class);
		UserManager userManager = mock(UserManager.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		AtomicReference<Runnable> asyncWrite = new AtomicReference<>();
		AtomicInteger schedulerRuns = new AtomicInteger();
		CompletableFuture<Void> cacheClear = new CompletableFuture<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(userManager.getDataManager()).thenReturn(dataManager);
		when(dataManager.clearCacheAsyncCompletion()).thenReturn(cacheClear);
		when(oldHandler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			schedulerRuns.incrementAndGet();
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			asyncWrite.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendGlobalDataSync oldSync = new BackendGlobalDataSync(plugin, ignored -> { });
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(oldSync, "globalDataHandler", oldHandler);
		setField(oldSync, "ownsGlobalMysql", true);
		setField(replacement, "globalDataHandler", replacementHandler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("ForceUpdate", new DataValueBoolean(true));
		when(oldHandler.getExact("lobby")).thenReturn(data);
		when(replacementHandler.getExact("lobby")).thenReturn(data);

		oldSync.checkGlobalData();
		scheduled.get().run();
		CompletableFuture<Void> close = CompletableFuture.runAsync(oldSync::close);
		close.get(1, TimeUnit.SECONDS);
		replacement.checkGlobalData();
		org.junit.jupiter.api.Assertions.assertEquals(2, schedulerRuns.get());
		verify(plugin, never()).update();
		verify(oldHandler, never()).setBoolean("lobby", "ForceUpdate", false);
		verify(mysql).close();

		scheduled.get().run();
		cacheClear.complete(null);
		assertNotNull(asyncWrite.get());
		asyncWrite.get().run();
		verify(replacementHandler).setBoolean("lobby", "ForceUpdate", false);
	}

	@Test
	void closeDrainsAnExecutingForceUpdateBeforeRetiringItsHandler() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		UserDataManager dataManager = mock(UserDataManager.class);
		UserManager userManager = mock(UserManager.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		AtomicReference<Runnable> asyncWrite = new AtomicReference<>();
		CountDownLatch updateStarted = new CountDownLatch(1);
		CountDownLatch finishUpdate = new CountDownLatch(1);
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(userManager.getDataManager()).thenReturn(dataManager);
		when(dataManager.clearCacheAsyncCompletion()).thenReturn(CompletableFuture.completedFuture(null));
		when(handler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			asyncWrite.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			updateStarted.countDown();
			assertTrue(finishUpdate.await(2, TimeUnit.SECONDS));
			return null;
		}).when(plugin).update();
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", true);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("ForceUpdate", new DataValueBoolean(true));
		when(handler.getExact("lobby")).thenReturn(data);

		sync.checkGlobalData();
		scheduled.get().run();
		CompletableFuture<Void> update = CompletableFuture.runAsync(asyncWrite.get());
		assertTrue(updateStarted.await(1, TimeUnit.SECONDS));
		CompletableFuture<Void> close = CompletableFuture.runAsync(sync::close);
		Thread.sleep(50L);
		assertFalse(close.isDone());
		verify(mysql, never()).close();

		finishUpdate.countDown();
		update.get(1, TimeUnit.SECONDS);
		close.get(1, TimeUnit.SECONDS);
		verify(handler).setBoolean("lobby", "ForceUpdate", false);
		verify(mysql).close();
	}

	@Test
	void bungeeDayChangeRunsOnTheTimeCheckerExecutor() {
		assertTimeChangeRunsOnTheTimeCheckerExecutor(TimeType.DAY);
	}

	@Test
	void bungeeWeekChangeRunsOnTheTimeCheckerExecutor() {
		assertTimeChangeRunsOnTheTimeCheckerExecutor(TimeType.WEEK);
	}

	@Test
	void bungeeMonthChangeRunsOnTheTimeCheckerExecutor() {
		assertTimeChangeRunsOnTheTimeCheckerExecutor(TimeType.MONTH);
	}

	private void assertTimeChangeRunsOnTheTimeCheckerExecutor(TimeType type) {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService timeCheckerExecutor = mock(ScheduledExecutorService.class);
		GlobalDataHandler globalDataHandler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();

		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(timeCheckerExecutor);

		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(timeCheckerExecutor).execute(any(Runnable.class));

		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", globalDataHandler);

		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(type.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(type, data));

		verify(plugin, never()).getBukkitScheduler();
		verify(timeChecker, never()).forceChanged(any(), any(Boolean.class), any(Boolean.class), any(Boolean.class));
		assertNotNull(scheduled.get());
		scheduled.get().run();
		verify(timeChecker).forceChanged(type, false, true, true);
		verify(globalDataHandler).setBoolean("lobby", type.toString(), false);
		verify(globalDataHandler).setData(eq("lobby"), any());
	}

	@Test
	void rejectedTimeChangeSubmissionClearsProcessingState() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doThrow(new RejectedExecutionException()).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		org.junit.jupiter.api.Assertions.assertFalse(sync.checkGlobalDataTime(TimeType.DAY, data));

		verify(handler).setBoolean("lobby", "Processing", true);
		verify(handler).setData(eq("lobby"), org.mockito.ArgumentMatchers.argThat(update ->
				!update.containsKey("FinishedProcessing") && !update.get("Processing").getBoolean()));
	}

	@Test
	void closeDefersOwnedMysqlRetirementUntilQueuedTimeChangeFinishes() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", true);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, data));
		CompletableFuture<Void> close = CompletableFuture.runAsync(sync::close);
		try {
			Thread.sleep(50L);
		} catch (InterruptedException failure) {
			Thread.currentThread().interrupt();
			throw new AssertionError(failure);
		}
		org.junit.jupiter.api.Assertions.assertFalse(close.isDone());
		verify(mysql, never()).close();

		assertNotNull(scheduled.get());
		scheduled.get().run();
		close.join();

		verify(handler).setBoolean("lobby", TimeType.DAY.toString(), false);
		verify(mysql).close();
	}

	@Test
	void replacementReceivesCompletionFromAnAdmittedOldTransition() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> oldMessages =
				new CopyOnWriteArrayList<>();
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> replacementMessages =
				new CopyOnWriteArrayList<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, oldMessages::add);
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, replacementMessages::add);
		setField(sync, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, data));
		sync.handoffCompletionSender(replacement);
		scheduled.get().run();

		assertTrue(oldMessages.isEmpty());
		org.junit.jupiter.api.Assertions.assertEquals(1, replacementMessages.size());
		org.junit.jupiter.api.Assertions.assertEquals("TimeChangeFinished",
				replacementMessages.get(0).getSubChannel());
	}

	@Test
	void completionHandoffDoesNotWaitForTransportIo() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		CountDownLatch sendStarted = new CountDownLatch(1);
		CountDownLatch finishSend = new CountDownLatch(1);
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> replacementMessages =
				new CopyOnWriteArrayList<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> {
			sendStarted.countDown();
			try {
				assertTrue(finishSend.await(2, TimeUnit.SECONDS));
			} catch (InterruptedException failure) {
				Thread.currentThread().interrupt();
				throw new AssertionError(failure);
			}
			throw new RejectedExecutionException("old transport retired");
		});
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, replacementMessages::add);
		setField(sync, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, data));
		CompletableFuture<Void> processing = CompletableFuture.runAsync(scheduled.get());
		assertTrue(sendStarted.await(1, TimeUnit.SECONDS));

		CompletableFuture<Void> handoff = CompletableFuture.runAsync(
				() -> sync.handoffCompletionSender(replacement));
		try {
			handoff.get(250, TimeUnit.MILLISECONDS);
		} finally {
			finishSend.countDown();
			processing.get(1, TimeUnit.SECONDS);
		}
		org.junit.jupiter.api.Assertions.assertEquals(1, replacementMessages.size());
	}

	@Test
	void admittedCompletionFollowsMultipleRuntimeHandoffs() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> firstMessages =
				new CopyOnWriteArrayList<>();
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> middleMessages =
				new CopyOnWriteArrayList<>();
		CopyOnWriteArrayList<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> latestMessages =
				new CopyOnWriteArrayList<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync first = new BackendGlobalDataSync(plugin, firstMessages::add);
		BackendGlobalDataSync middle = new BackendGlobalDataSync(plugin, middleMessages::add);
		BackendGlobalDataSync latest = new BackendGlobalDataSync(plugin, latestMessages::add);
		setField(first, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(first.checkGlobalDataTime(TimeType.DAY, data));
		first.handoffCompletionSender(middle);
		middle.handoffCompletionSender(latest);
		scheduled.get().run();

		assertTrue(firstMessages.isEmpty());
		assertTrue(middleMessages.isEmpty());
		org.junit.jupiter.api.Assertions.assertEquals(1, latestMessages.size());
	}

	@Test
	void replacementDoesNotReadmitAnActiveTimeChange() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler oldHandler = mock(GlobalDataHandler.class);
		GlobalDataHandler replacementHandler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync oldSync = new BackendGlobalDataSync(plugin, ignored -> { });
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(oldSync, "globalDataHandler", oldHandler);
		setField(replacement, "globalDataHandler", replacementHandler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(oldSync.checkGlobalDataTime(TimeType.DAY, data));
		assertFalse(replacement.checkGlobalDataTime(TimeType.DAY, data));

		verify(executor).execute(any(Runnable.class));
		verify(replacementHandler, never()).setBoolean("lobby", "Processing", true);
		assertNotNull(scheduled.get());
		scheduled.get().run();
		verify(timeChecker).forceChanged(TimeType.DAY, false, true, true);
	}

	@Test
	void replacementWaitsForThePredecessorsEntirePeriodBatch() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler oldHandler = mock(GlobalDataHandler.class);
		GlobalDataHandler replacementHandler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync oldSync = new BackendGlobalDataSync(plugin, ignored -> { });
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(oldSync, "globalDataHandler", oldHandler);
		setField(replacement, "globalDataHandler", replacementHandler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> day = new HashMap<>();
		day.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		day.put(TimeType.DAY.toString(), new DataValueBoolean(true));
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> week = new HashMap<>();
		week.put("LastUpdated", day.get("LastUpdated"));
		week.put(TimeType.WEEK.toString(), new DataValueBoolean(true));

		assertTrue(oldSync.checkGlobalDataTime(TimeType.DAY, day));
		assertFalse(replacement.checkGlobalDataTime(TimeType.WEEK, week));
		scheduled.get().run();
		assertTrue(replacement.checkGlobalDataTime(TimeType.WEEK, week));
		scheduled.get().run();

		verify(timeChecker).forceChanged(TimeType.DAY, false, true, true);
		verify(timeChecker).forceChanged(TimeType.WEEK, false, true, true);
	}

	@Test
	void aFailedPeriodDoesNotLetAnotherPeriodPublishBatchCompletion() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		CopyOnWriteArrayList<Runnable> scheduled = new CopyOnWriteArrayList<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.add(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		org.mockito.Mockito.doThrow(new IllegalStateException("day failed"))
				.when(timeChecker).forceChanged(TimeType.DAY, false, true, true);
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));
		data.put(TimeType.WEEK.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, data));
		assertTrue(sync.checkGlobalDataTime(TimeType.WEEK, data));
		org.junit.jupiter.api.Assertions.assertEquals(2, scheduled.size());
		scheduled.get(0).run();
		scheduled.get(1).run();

		verify(handler).setData(eq("lobby"), org.mockito.ArgumentMatchers.argThat(update ->
				!update.containsKey("FinishedProcessing") && !update.get("Processing").getBoolean()));
	}

	@Test
	void aNewPeriodWaitsUntilThePreviousBatchFinalizationIsPersisted() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		CountDownLatch finalizationStarted = new CountDownLatch(1);
		CountDownLatch finishFinalization = new CountDownLatch(1);
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			finalizationStarted.countDown();
			assertTrue(finishFinalization.await(2, TimeUnit.SECONDS));
			return null;
		}).when(handler).setData(eq("lobby"), any());
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> day = new HashMap<>();
		day.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		day.put(TimeType.DAY.toString(), new DataValueBoolean(true));
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> week = new HashMap<>();
		week.put("LastUpdated", day.get("LastUpdated"));
		week.put(TimeType.WEEK.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, day));
		CompletableFuture<Void> dayCompletion = CompletableFuture.runAsync(scheduled.get());
		assertTrue(finalizationStarted.await(1, TimeUnit.SECONDS));
		assertFalse(sync.checkGlobalDataTime(TimeType.WEEK, week));

		finishFinalization.countDown();
		dayCompletion.get(1, TimeUnit.SECONDS);
		assertTrue(sync.checkGlobalDataTime(TimeType.WEEK, week));
		scheduled.get().run();
	}

	@Test
	void closeDrainsBorrowedMysqlTransitionWithoutClosingItsConnection() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", false);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		assertTrue(sync.checkGlobalDataTime(TimeType.DAY, data));
		CompletableFuture<Void> close = CompletableFuture.runAsync(sync::close);
		Thread.sleep(50L);
		org.junit.jupiter.api.Assertions.assertFalse(close.isDone());
		scheduled.get().run();
		close.get(1, TimeUnit.SECONDS);

		verify(mysql, never()).close();
	}

	@Test
	void timedOutBorrowedTransitionTransfersAdmissionWithoutDuplicatingItsPeriod() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler oldHandler = mock(GlobalDataHandler.class);
		GlobalDataHandler replacementHandler = mock(GlobalDataHandler.class);
		CopyOnWriteArrayList<Runnable> scheduled = new CopyOnWriteArrayList<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.add(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync oldSync = new BackendGlobalDataSync(plugin, ignored -> { });
		BackendGlobalDataSync replacement = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(oldSync, "globalDataHandler", oldHandler);
		setField(oldSync, "ownsGlobalMysql", false);
		setField(replacement, "globalDataHandler", replacementHandler);
		oldSync.handoffCompletionSender(replacement);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> day = new HashMap<>();
		day.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		day.put(TimeType.DAY.toString(), new DataValueBoolean(true));
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> week = new HashMap<>();
		week.put("LastUpdated", day.get("LastUpdated"));
		week.put(TimeType.WEEK.toString(), new DataValueBoolean(true));

		assertTrue(oldSync.checkGlobalDataTime(TimeType.DAY, day));
		oldSync.close(1, TimeUnit.MILLISECONDS);
		assertFalse(replacement.checkGlobalDataTime(TimeType.DAY, day));
		assertTrue(replacement.checkGlobalDataTime(TimeType.WEEK, week));
		org.junit.jupiter.api.Assertions.assertEquals(2, scheduled.size());

		scheduled.get(1).run();
		verify(replacementHandler, never()).setData(eq("lobby"), any());
		scheduled.get(0).run();
		verify(oldHandler).setData(eq("lobby"), org.mockito.ArgumentMatchers.argThat(update ->
				update.containsKey("FinishedProcessing") && !update.get("Processing").getBoolean()));
	}

	@Test
	void slowProcessingWriteDoesNotHoldLifecycleLockDuringClose() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		CountDownLatch writeStarted = new CountDownLatch(1);
		CountDownLatch releaseWrite = new CountDownLatch(1);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTimer()).thenReturn(executor);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			writeStarted.countDown();
			assertTrue(releaseWrite.await(2, TimeUnit.SECONDS));
			return null;
		}).when(handler).setBoolean("lobby", "Processing", true);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", true);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));

		CompletableFuture<Boolean> admission = CompletableFuture.supplyAsync(
				() -> sync.checkGlobalDataTime(TimeType.DAY, data));
		assertTrue(writeStarted.await(1, TimeUnit.SECONDS));
		CompletableFuture<Void> close = CompletableFuture.runAsync(sync::close);
		Thread.sleep(50L);
		org.junit.jupiter.api.Assertions.assertFalse(close.isDone());
		verify(mysql, never()).close();

		releaseWrite.countDown();
		assertTrue(admission.get(1, TimeUnit.SECONDS));
		assertNotNull(scheduled.get());
		scheduled.get().run();
		close.get(1, TimeUnit.SECONDS);
		verify(mysql).close();
	}

	@Test
	void blockedAdmissionForcesOwnedMysqlClosedWithinTheConfiguredGrace() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungeeSettings = mock(BungeeSettings.class);
		TimeChecker timeChecker = mock(TimeChecker.class);
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		CountDownLatch writeStarted = new CountDownLatch(1);
		CountDownLatch releaseWrite = new CountDownLatch(1);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(plugin.getBungeeSettings()).thenReturn(bungeeSettings);
		when(bungeeSettings.getServer()).thenReturn("lobby");
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("global-data-close-test"));
		when(timeChecker.getTimer()).thenReturn(executor);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		org.mockito.Mockito.doAnswer(invocation -> {
			writeStarted.countDown();
			assertTrue(releaseWrite.await(2, TimeUnit.SECONDS));
			return null;
		}).when(handler).setBoolean("lobby", "Processing", true);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(0));
			return null;
		}).when(executor).execute(any(Runnable.class));
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", true);
		HashMap<String, com.bencodez.simpleapi.sql.data.DataValue> data = new HashMap<>();
		data.put("LastUpdated", new DataValueString(
				"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
		data.put(TimeType.DAY.toString(), new DataValueBoolean(true));
		CompletableFuture<Boolean> admission = CompletableFuture.supplyAsync(
				() -> sync.checkGlobalDataTime(TimeType.DAY, data));
		assertTrue(writeStarted.await(1, TimeUnit.SECONDS));

		sync.close(50L, TimeUnit.MILLISECONDS);
		verify(mysql).close();

		releaseWrite.countDown();
		assertTrue(admission.get(1, TimeUnit.SECONDS));
		assertNotNull(scheduled.get());
		scheduled.get().run();
		verify(mysql, org.mockito.Mockito.times(1)).close();
	}

	@Test
	void closingBorrowedMainMysqlLeavesItsOwnerRunning() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", false);

		sync.close();

		verify(mysql, never()).close();
	}

	@Test
	void closingDedicatedGlobalMysqlClosesItsOwnedConnection() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		GlobalDataHandler handler = mock(GlobalDataHandler.class);
		GlobalMySQL mysql = mock(GlobalMySQL.class);
		when(handler.getGlobalMysql()).thenReturn(mysql);
		BackendGlobalDataSync sync = new BackendGlobalDataSync(plugin, ignored -> { });
		setField(sync, "globalDataHandler", handler);
		setField(sync, "ownsGlobalMysql", true);

		sync.close();

		verify(mysql).close();
	}

	private static void setField(Object target, String fieldName, Object value) {
		try {
			Field field = BackendGlobalDataSync.class.getDeclaredField(fieldName);
			field.setAccessible(true);
			field.set(target, value);
		} catch (ReflectiveOperationException failure) {
			throw new AssertionError(failure);
		}
	}
}
