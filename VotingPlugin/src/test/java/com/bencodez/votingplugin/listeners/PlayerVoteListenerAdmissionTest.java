package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

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

	@Test
	void postAdmissionFailureIsVisibleToDurableProducers() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "player", "site", false);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("PlayerVoteListenerAdmissionTest"));

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class);
				MockedStatic<SharedVoteProcessor> processor = org.mockito.Mockito.mockStatic(SharedVoteProcessor.class)) {
			bukkit.when(Bukkit::isPrimaryThread).thenReturn(false);
			processor.when(() -> SharedVoteProcessor.process(any()))
					.thenThrow(new IllegalStateException("storage failed"));
			new PlayerVoteListener(plugin).onplayerVote(event);
		}

		assertTrue(event.isProcessingFailed());
	}

	@Test
	void capturedPlatformStateAvoidsWorkerPlayerReads() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "player", "site", false);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UserManager users = mock(UserManager.class);
		UUID uuid = UUID.randomUUID();
		when(plugin.getVotingPluginUserManager()).thenReturn(users);
		when(users.getVotingPluginUser(uuid, "Player")).thenReturn(user);
		when(user.getJavaUUID()).thenReturn(uuid);
		PlayerVoteListener.PlatformVoteState state = new PlayerVoteListener.PlatformVoteState(true, mock(Player.class),
				uuid, "Player", true, true, true);
		PlayerVoteListener.BukkitOperations operations = new PlayerVoteListener.BukkitOperations(plugin, event, state);

		assertTrue(operations.userOnline(user));
		assertTrue(operations.userVanished(user));
		assertTrue(operations.bypassWaitPermission(user));
		assertTrue(operations.resolveUser("player") == user);
		verify(user, never()).isOnline();
		verify(user, never()).isVanished();
		verify(user, never()).hasPermission(any());
	}

	@Test
	void capturedPlatformStateSchedulesBroadcastForThePlayerOwner() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player owner = mock(Player.class);
		UUID uuid = UUID.randomUUID();
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		PlayerVoteListener.PlatformVoteState state = new PlayerVoteListener.PlatformVoteState(true, owner, uuid,
				"Player", true, false, false);
		PlayerVoteListener.BukkitOperations operations = new PlayerVoteListener.BukkitOperations(plugin,
				new PlayerVoteEvent(null, "player", "site", false), state);

		operations.broadcast(uuid, "Player", "Site", true);

		verify(scheduler).runTask(org.mockito.ArgumentMatchers.same(plugin), any(Runnable.class),
				org.mockito.ArgumentMatchers.same(owner));
	}
}
