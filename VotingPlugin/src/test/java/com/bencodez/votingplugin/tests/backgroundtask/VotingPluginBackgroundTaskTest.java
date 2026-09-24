package com.bencodez.votingplugin.tests.backgroundtask;

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
				.when(plugin).captureOnlineTopVoterIgnore(any());
		VotingPluginBackgroundTask task = new VotingPluginBackgroundTask(plugin);
		task.setRequested(true);

		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::getServer).thenReturn(mock(Server.class));
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
			task.run();

			verify(plugin).captureOnlineTopVoterIgnore(any());
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
				.when(plugin).captureOnlineTopVoterIgnore(any());
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

}
