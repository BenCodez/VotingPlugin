package com.bencodez.votingplugin.tests.backgroundtask;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import org.bukkit.Bukkit;
import org.bukkit.Server;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask;

public class VotingPluginBackgroundTaskTest {
	@Test
	public void updateDoesNotHoldPluginMonitorWhileRefreshWaits() throws Exception {
		assertFalse(java.lang.reflect.Modifier.isSynchronized(
				VotingPluginMain.class.getMethod("update").getModifiers()));
	}

	@Test
	public void requestedStateCanBeDelegated() {
		VotingPluginBackgroundTask task = new VotingPluginBackgroundTask(mock(VotingPluginMain.class));
		assertFalse(task.isRequested());
		task.setRequested(true);
		assertTrue(task.isRequested());
		task.setRequested(false);
		assertFalse(task.isRequested());
	}
	@Test
	public void runRequestsPlatformSnapshotBeforeStartingRefresh() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		when(plugin.isEnabled()).thenReturn(true);
		when(plugin.getConfigFile()).thenReturn(config);
		when(config.isUpdateWithPlayersOnlineOnly()).thenReturn(true);
		java.util.concurrent.atomic.AtomicReference<java.util.function.Consumer<java.util.Map<java.util.UUID, Boolean>>> callback =
				new java.util.concurrent.atomic.AtomicReference<>();
		org.mockito.Mockito.doAnswer(call -> { callback.set(call.getArgument(0)); return null; })
				.when(plugin).captureOnlineTopVoterIgnore(any(), any());
		VotingPluginBackgroundTask task = new VotingPluginBackgroundTask(plugin);
		task.setRequested(true);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getServer).thenReturn(mock(Server.class));
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
			task.run();

			verify(plugin).captureOnlineTopVoterIgnore(any(), any());
			assertTrue(task.isRunning());
			callback.get().accept(java.util.Map.of());
			assertFalse(task.isRunning());
			assertTrue(task.isRequested(), "online-only skip must preserve the pending request");
		}
	}

	@Test
	public void refreshUsesUserStorageWorkerInsteadOfVoteExecutor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Config config = mock(Config.class);
		when(plugin.isEnabled()).thenReturn(true);
		when(plugin.getConfigFile()).thenReturn(config);
		java.util.concurrent.atomic.AtomicReference<java.util.function.Consumer<java.util.Map<java.util.UUID, Boolean>>> callback =
				new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(call -> { callback.set(call.getArgument(0)); return null; })
				.when(plugin).captureOnlineTopVoterIgnore(any(), any());
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(storage);
		VotingPluginBackgroundTask task = new VotingPluginBackgroundTask(plugin);
		task.setRequested(true);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getServer).thenReturn(mock(Server.class));
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
			task.run();
			callback.get().accept(java.util.Map.of(java.util.UUID.randomUUID(), Boolean.FALSE));
		}

		verify(storage).execute(any(Runnable.class));
		verify(plugin.getVoteTimer(), never()).execute(any(Runnable.class));
	}

	@Test
	public void retiredFoliaPlayerStillCompletesOnlineSnapshot() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation implementation =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(implementation);
		when(implementation.runAtEntityWithFallback(eq(player), any(), any(Runnable.class)))
				.thenReturn(java.util.concurrent.CompletableFuture.completedFuture(
						com.bencodez.simpleapi.folialib.enums.EntityTaskResult.SCHEDULER_RETIRED));
		doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
				.when(scheduler).runTask(eq(plugin), any(Runnable.class));
		java.util.concurrent.atomic.AtomicReference<java.util.Map<java.util.UUID, Boolean>> result =
				new java.util.concurrent.atomic.AtomicReference<>();

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getOnlinePlayers).thenReturn(java.util.List.of(player));
			plugin.captureOnlineTopVoterIgnore(result::set);
		}

		assertEquals(java.util.Map.of(), result.get());
		verify(scheduler, times(2)).runTask(eq(plugin), any(Runnable.class));
	}

	@Test
	public void cancelledInitialSnapshotDoesNotBlockStorageWorkerForever() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		when(plugin.isEnabled()).thenReturn(true);
		when(plugin.getConfigFile()).thenReturn(config);
		java.lang.reflect.Constructor<VotingPluginBackgroundTask> constructor =
				VotingPluginBackgroundTask.class.getDeclaredConstructor(VotingPluginMain.class, long.class);
		constructor.setAccessible(true);
		VotingPluginBackgroundTask task = constructor.newInstance(plugin, 10L);
		task.setRequested(true);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getServer).thenReturn(null);
			long started = System.nanoTime();
			task.run();
			assertTrue(java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started) < 1_000);
		}

		assertFalse(task.isRunning());
		assertTrue(task.isRequested());
		verify(plugin).captureOnlineTopVoterIgnore(any(), any());
	}

	@Test
	public void snapshotAdmissionFailureKeepsRefreshPending() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Config config = mock(Config.class);
		when(plugin.isEnabled()).thenReturn(true);
		when(plugin.getConfigFile()).thenReturn(config);
		doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
				.when(plugin).captureOnlineTopVoterIgnore(any(), any());
		VotingPluginBackgroundTask task = new VotingPluginBackgroundTask(plugin);
		task.setRequested(true);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getServer).thenReturn(null);
			task.run();
		}

		assertFalse(task.isRunning());
		assertTrue(task.isRequested());
		verify(plugin.getUserManager().getDataManager().getTimer(), never()).execute(any(Runnable.class));
	}

}
