package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VotingPluginHooksReplayCompletionTest {
	@Test
	void backgroundUpdateWaitsForConfirmedReplayAndStorageWorker() {
		Fixture fixture = new Fixture();
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.hooks.backgroundUpdate(fixture.player);

		verify(fixture.user, never()).checkOfflineRewards();
		assertTrue(fixture.worker.isEmpty());
		replay.complete(null);
		verify(fixture.user, never()).checkOfflineRewards();
		assertEquals(1, fixture.worker.size(), "completion on any thread must schedule the storage continuation");
		fixture.worker.remove().run();
		verify(fixture.user).checkOfflineRewards();
	}

	@Test
	void failedReplayCannotRunGenericOfflineRewards() {
		Fixture fixture = new Fixture();
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.hooks.backgroundUpdate(fixture.player);
		replay.completeExceptionally(new IllegalStateException("reward delivery failed"));

		verify(fixture.user, never()).checkOfflineRewards();
		assertTrue(fixture.worker.isEmpty());
	}

	@Test
	void disabledVoteRewardsPreserveGenericOfflineRewardContinuation() {
		Fixture fixture = new Fixture();
		when(fixture.plugin.getOptions().isProcessRewards()).thenReturn(false);

		fixture.hooks.backgroundUpdate(fixture.player);

		verify(fixture.user, never()).checkOfflineRewards();
		assertEquals(1, fixture.worker.size());
		fixture.worker.remove().run();
		verify(fixture.user).checkOfflineRewards();
	}

	private static final class Fixture {
		final VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		final Player player = mock(Player.class);
		final VotingPluginUser user;
		final VotingPluginHooks hooks = spy(new VotingPluginHooks());
		final Queue<Runnable> worker = new ArrayDeque<>();

		Fixture() {
			AdvancedCoreUser base = mock(AdvancedCoreUser.class);
			when(base.getUserData()).thenReturn(mock(com.bencodez.advancedcore.api.user.UserData.class));
			when(base.getUUID()).thenReturn(UUID.randomUUID().toString());
			when(base.getPlayerName()).thenReturn("Player");
			user = spy(new VotingPluginUser(plugin, base));
			doNothing().when(user).checkOfflineRewards();
			doReturn(plugin).when(hooks).getMainClass();
			when(plugin.getVotingPluginUserManager().getVotingPluginUser(player)).thenReturn(user);
			java.util.concurrent.ScheduledExecutorService storageTimer =
					plugin.getUserManager().getDataManager().getTimer();
			doAnswer(call -> {
				worker.add(call.getArgument(0, Runnable.class));
				return null;
			}).when(storageTimer).execute(any(Runnable.class));
		}
	}
}
