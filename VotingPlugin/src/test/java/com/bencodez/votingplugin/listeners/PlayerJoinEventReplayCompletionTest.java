package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
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
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.user.VotingPluginUser;

class PlayerJoinEventReplayCompletionTest {
	@Test
	void storedUserLoginReportsPresenceWhileWaitingForReplayAndLoginRewards() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.login();

		verify(fixture.user).offVoteAndThen(any(Player.class), any(Runnable.class));
		fixture.verifyNoRewardFollowUp();
		assertEquals(1, fixture.worker.size(), "presence must not await replay completion");
		assertTrue(fixture.presence.isOnline(fixture.uuid));
		fixture.runWorker();
		fixture.verifyProxyPresence();
		fixture.verifyNoRewardFollowUp();
		replay.complete(null);
		fixture.verifyNoRewardFollowUp();
		assertEquals(1, fixture.worker.size(), "replay completion must hand login rewards back to storage");
		fixture.runWorker();
		verify(fixture.user).loginRewardsAsync();
		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
		assertTrue(fixture.worker.isEmpty(), "reward admission is not reward completion");
		fixture.rewards.complete(null);
		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
		assertEquals(1, fixture.worker.size(), "reward completion must return placeholders to storage");
		fixture.runWorker();

		fixture.verifyFollowUpOrder();
	}

	@Test
	void failedReplaySuppressesRewardFollowUpButStillReportsProxyPresence() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.login();
		replay.completeExceptionally(new IllegalStateException("replay commit failed"));

		fixture.verifyNoRewardFollowUp();
		assertEquals(1, fixture.worker.size());
		fixture.runWorker();
		fixture.verifyProxyPresence();
		fixture.verifyNoRewardFollowUp();
		assertTrue(fixture.worker.isEmpty());
	}

	@Test
	void failedLoginRewardsCannotRefreshPlaceholdersOrSuppressPresence() {
		Fixture fixture = new Fixture(false);
		fixture.login();
		fixture.runWorker();
		fixture.runWorker();

		fixture.rewards.completeExceptionally(new IllegalStateException("reward action failed"));

		fixture.verifyProxyPresence();
		verify(fixture.user).loginRewardsAsync();
		verify(fixture.user, never()).loginRewards();
		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
		assertTrue(fixture.worker.isEmpty());
	}

	@Test
	void noDataLoginSkipsReplayButStillSchedulesRewardsAndPlaceholdersOnStorage() {
		Fixture fixture = new Fixture(false);
		fixture.login();

		verify(fixture.user, never()).offVoteAsync(any());
		verify(fixture.user, never()).offVoteAndThen(any(), any(Runnable.class));
		fixture.verifyNoRewardFollowUp();
		assertEquals(2, fixture.worker.size());
		assertTrue(fixture.presence.isOnline(fixture.uuid));
		fixture.finishLogin();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void disabledOfflineRewardsStillAllowTheWorkerScheduledLoginContinuation() {
		Fixture fixture = new Fixture(true);
		when(fixture.plugin.getOptions().isProcessRewards()).thenReturn(false);
		fixture.login();

		fixture.verifyNoRewardFollowUp();
		assertEquals(2, fixture.worker.size());
		fixture.finishLogin();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void absentPlayerStillAllowsTheWorkerScheduledLoginContinuation() {
		Fixture fixture = new Fixture(true);
		when(fixture.event.getPlayer()).thenReturn(null);
		when(fixture.plugin.getOptions().isProcessRewards()).thenReturn(true);
		fixture.login();

		fixture.verifyNoRewardFollowUp();
		assertEquals(2, fixture.worker.size());
		fixture.finishLogin();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void completedNoOpLoginRewardsStillRefreshPlaceholdersOnlyOnStorage() {
		Fixture fixture = new Fixture(false);
		doAnswer(call -> {
			assertTrue(fixture.onStorageWorker);
			return CompletableFuture.completedFuture(null);
		}).when(fixture.user).loginRewardsAsync();
		fixture.login();
		fixture.runWorker();
		fixture.runWorker();

		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
		assertEquals(1, fixture.worker.size());
		fixture.runWorker();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void disabledProxyModeSkipsTheIndependentPresenceReport() {
		Fixture fixture = new Fixture(false);
		when(fixture.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(false);
		fixture.login();
		fixture.finishLogin();

		verify(fixture.plugin.getBackendProxyHandler(), never()).playerOnline("Player", fixture.uuid.toString());
		verify(fixture.plugin.getPlaceholders()).onUpdate(fixture.user, true);
	}

	@Test
	void asynchronousLoginCapturesOperatorStateOnlyOnTheEntityOwner() {
		Fixture fixture = new Fixture(false);
		when(fixture.plugin.isYmlError()).thenReturn(true);
		when(fixture.player.isOp()).thenReturn(true);
		doNothing().when(fixture.user).sendMessage(any(String.class));
		Queue<Runnable> entityTasks = new ArrayDeque<>();
		// The legacy adapter is an entity-aware scheduler boundary too.
		when(fixture.plugin.getBukkitScheduler().getFoliaLib()).thenReturn(null);
		com.bencodez.simpleapi.scheduler.BukkitScheduler ownerScheduler = fixture.plugin.getBukkitScheduler();
		doAnswer(call -> {
			entityTasks.add(call.getArgument(1, Runnable.class));
			return null;
		}).when(ownerScheduler).runTask(
				org.mockito.ArgumentMatchers.eq(fixture.plugin), any(Runnable.class),
				org.mockito.ArgumentMatchers.eq(fixture.player));

		fixture.login();

		verify(fixture.player, never()).isOp();
		verify(fixture.user, never()).sendMessage(any(String.class));
		assertEquals(1, entityTasks.size());
		entityTasks.remove().run();
		verify(fixture.player).isOp();
		verify(fixture.user).sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
	}

	@Test
	void quitBeforeQueuedPresenceAndReplayCompletionCannotPublishAStaleLogin() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);
		fixture.login();
		fixture.presence.playerOffline(fixture.uuid, fixture.player);
		replay.complete(null);
		fixture.runWorker();
		fixture.runWorker();

		fixture.verifyNoRewardFollowUp();
		verify(fixture.plugin.getBackendProxyHandler(), never()).playerOnline("Player", fixture.uuid.toString());
	}

	@Test
	void replacementBeforeQueuedPresenceAndReplayCompletionCannotReceiveEarlierFollowUp() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);
		fixture.login();
		fixture.presence.playerOnline(fixture.uuid, mock(Player.class));
		replay.complete(null);
		fixture.runWorker();
		fixture.runWorker();

		fixture.verifyNoRewardFollowUp();
		verify(fixture.plugin.getBackendProxyHandler(), never()).playerOnline("Player", fixture.uuid.toString());
	}

	@Test
	void quitWhileLoginRewardsArePendingCannotRefreshPlaceholders() {
		Fixture fixture = new Fixture(false);
		fixture.login();
		fixture.runWorker();
		fixture.runWorker();
		fixture.presence.playerOffline(fixture.uuid, fixture.player);
		fixture.rewards.complete(null);
		fixture.runWorker();

		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
	}

	@Test
	void replacementAfterRewardCompletionBeforeQueuedRefreshPreservesReplacementPlaceholders() {
		Fixture fixture = new Fixture(false);
		fixture.login();
		fixture.runWorker();
		fixture.runWorker();
		fixture.rewards.complete(null);
		fixture.presence.playerOnline(fixture.uuid, mock(Player.class));
		fixture.runWorker();

		verify(fixture.plugin.getPlaceholders(), never()).onUpdate(fixture.user, true);
	}

	/** Use the real continuation helper with controlled replay completion and worker admission. */
	private static final class Fixture {
		final VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		final Player player = mock(Player.class);
		final UUID uuid = UUID.randomUUID();
		final VotingPluginUser user;
		final AdvancedCoreLoginEvent event = mock(AdvancedCoreLoginEvent.class);
		final PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		final Queue<Runnable> worker = new ArrayDeque<>();
		final CompletableFuture<Void> rewards = new CompletableFuture<>();
		boolean onStorageWorker;

		Fixture(boolean hasData) {
			AdvancedCoreUser base = mock(AdvancedCoreUser.class);
			when(base.getUserData()).thenReturn(mock(com.bencodez.advancedcore.api.user.UserData.class));
			when(base.getUUID()).thenReturn(uuid.toString());
			when(base.getPlayerName()).thenReturn("Player");
			user = spy(new VotingPluginUser(plugin, base));
			doReturn(uuid).when(user).getJavaUUID();
			doReturn(uuid.toString()).when(user).getUUID();
			doReturn("Player").when(user).getPlayerName();
			doAnswer(call -> {
				assertTrue(onStorageWorker, "login reward planning must stay on storage");
				return rewards;
			}).when(user).loginRewardsAsync();
			var placeholders = plugin.getPlaceholders();
			var proxyHandler = plugin.getBackendProxyHandler();
			doAnswer(call -> {
				assertTrue(onStorageWorker, "placeholder refresh reads storage");
				return null;
			}).when(placeholders).onUpdate(user, true);
			doAnswer(call -> {
				assertTrue(onStorageWorker, "proxy presence reads the storage-backed player name");
				return null;
			}).when(proxyHandler).playerOnline("Player", uuid.toString());
			when(plugin.isMySQLOkay()).thenReturn(true);
			when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
			when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid.toString())).thenReturn(user);
			when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
			when(event.getUser()).thenReturn(base);
			when(event.getUuid()).thenReturn(uuid.toString());
			when(event.getPlayer()).thenReturn(player);
			when(event.isUserInStorage()).thenReturn(hasData);
			java.util.concurrent.ScheduledExecutorService storageTimer =
					plugin.getUserManager().getDataManager().getTimer();
			doAnswer(call -> {
				worker.add(call.getArgument(0, Runnable.class));
				return null;
			}).when(storageTimer).execute(any(Runnable.class));
		}

		void login() {
			new PlayerJoinEvent(plugin).onPlayerLogin(event);
		}

		void runWorker() {
			onStorageWorker = true;
			try {
				worker.remove().run();
			} finally {
				onStorageWorker = false;
			}
		}

		void finishLogin() {
			runWorker();
			runWorker();
			rewards.complete(null);
			runWorker();
			assertTrue(worker.isEmpty());
		}

		void verifyNoRewardFollowUp() {
			verify(user, never()).loginRewards();
			verify(user, never()).loginRewardsAsync();
			verify(plugin.getPlaceholders(), never()).onUpdate(user, true);
		}

		void verifyProxyPresence() {
			verify(plugin.getBackendProxyHandler()).playerOnline("Player", uuid.toString());
		}

		void verifyFollowUpOrder() {
			verify(user, never()).loginRewards();
			InOrder ordered = inOrder(user, plugin.getPlaceholders(), plugin.getBackendProxyHandler());
			ordered.verify(plugin.getBackendProxyHandler()).playerOnline("Player", uuid.toString());
			ordered.verify(user).loginRewardsAsync();
			ordered.verify(plugin.getPlaceholders()).onUpdate(user, true);
		}
	}
}
