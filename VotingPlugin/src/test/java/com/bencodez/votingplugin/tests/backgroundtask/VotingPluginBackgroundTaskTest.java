package com.bencodez.votingplugin.tests.backgroundtask;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
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
}
