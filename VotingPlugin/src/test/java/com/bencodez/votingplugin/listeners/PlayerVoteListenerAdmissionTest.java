package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

class PlayerVoteListenerAdmissionTest {
	@Test
	void primaryThreadEventDefersAccountingToTheVoteExecutor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ScheduledExecutorService voteExecutor = mock(ScheduledExecutorService.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "player", "site", false);
		when(plugin.getVoteTimer()).thenReturn(voteExecutor);

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
			new PlayerVoteListener(plugin).onplayerVote(event);
		}

		verify(voteExecutor).submit(any(Runnable.class));
		assertFalse(event.isAccountingAdmissionFailed());
	}

	@Test
	void rejectedDeferralSurfacesAccountingFailure() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ScheduledExecutorService voteExecutor = mock(ScheduledExecutorService.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "player", "site", false);
		when(plugin.getVoteTimer()).thenReturn(voteExecutor);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("PlayerVoteListenerAdmissionTest"));
		doThrow(new RejectedExecutionException("full")).when(voteExecutor).submit(any(Runnable.class));

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class)) {
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
			new PlayerVoteListener(plugin).onplayerVote(event);
		}

		assertTrue(event.isAccountingAdmissionFailed());
	}
}
