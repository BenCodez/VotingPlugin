package com.bencodez.votingplugin.backendproxy.global;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
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
		sync.close();
		verify(mysql, never()).close();

		assertNotNull(scheduled.get());
		scheduled.get().run();

		verify(handler).setBoolean("lobby", TimeType.DAY.toString(), false);
		verify(mysql).close();
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
