package com.bencodez.votingplugin.tests.backgroundtask;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask;

public class VotingPluginBackgroundTaskTest {

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

		task.run();

		verify(plugin).captureOnlineTopVoterIgnore(any());
		assertTrue(task.isRunning());
		callback.get().accept(java.util.Map.of());
		assertFalse(task.isRunning());
		assertTrue(task.isRequested(), "online-only skip must preserve the pending request");
	}

}
