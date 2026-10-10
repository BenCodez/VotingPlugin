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
	void storedUserLoginWaitsForReplaySuccessAndTheStorageWorker() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.login();

		verify(fixture.user).offVoteAndThen(any(Player.class), any(Runnable.class));
		fixture.verifyNoFollowUp();
		assertTrue(fixture.worker.isEmpty(), "an admitted replay is not a completed login");
		assertTrue(fixture.presence.isOnline(fixture.uuid), "presence must be published before replay");
		replay.complete(null);
		fixture.verifyNoFollowUp();
		assertEquals(1, fixture.worker.size(), "completion must hand dependent work back to storage");
		fixture.worker.remove().run();

		fixture.verifyFollowUpOrder();
	}

	@Test
	void failedReplaySuppressesLoginRewardsPlaceholdersAndProxyNotification() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);

		fixture.login();
		replay.completeExceptionally(new IllegalStateException("replay commit failed"));

		fixture.verifyNoFollowUp();
		assertTrue(fixture.worker.isEmpty());
	}

	@Test
	void noDataLoginSkipsReplayButStillSchedulesFollowUpOnStorage() {
		Fixture fixture = new Fixture(false);

		fixture.login();

		verify(fixture.user, never()).offVoteAsync(any());
		verify(fixture.user, never()).offVoteAndThen(any(), any(Runnable.class));
		fixture.verifyNoFollowUp();
		assertEquals(1, fixture.worker.size());
		assertTrue(fixture.presence.isOnline(fixture.uuid));
		fixture.worker.remove().run();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void disabledOfflineRewardsStillAllowTheWorkerScheduledLoginContinuation() {
		Fixture fixture = new Fixture(true);
		when(fixture.plugin.getOptions().isProcessRewards()).thenReturn(false);

		fixture.login();

		fixture.verifyNoFollowUp();
		assertEquals(1, fixture.worker.size());
		fixture.worker.remove().run();
		fixture.verifyFollowUpOrder();
	}

	@Test
	void absentPlayerStillAllowsTheWorkerScheduledLoginContinuation() {
		Fixture fixture = new Fixture(true);
		when(fixture.event.getPlayer()).thenReturn(null);
		when(fixture.plugin.getOptions().isProcessRewards()).thenReturn(true);

		fixture.login();

		fixture.verifyNoFollowUp();
		assertEquals(1, fixture.worker.size());
		fixture.worker.remove().run();
		fixture.verifyFollowUpOrder();
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
	void quitWhileReplayIsPendingCannotPublishAStaleLogin() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);
		fixture.login();
		fixture.presence.playerOffline(fixture.uuid, fixture.player);
		replay.complete(null);
		fixture.worker.remove().run();
		fixture.verifyNoFollowUp();
	}

	@Test
	void replacementLoginCannotReceiveTheEarlierOwnersFollowUp() {
		Fixture fixture = new Fixture(true);
		CompletableFuture<Void> replay = new CompletableFuture<>();
		doReturn(replay).when(fixture.user).offVoteAsync(fixture.player);
		fixture.login();
		fixture.presence.playerOnline(fixture.uuid, mock(Player.class));
		replay.complete(null);
		fixture.worker.remove().run();
		fixture.verifyNoFollowUp();
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

		Fixture(boolean hasData) {
			AdvancedCoreUser base = mock(AdvancedCoreUser.class);
			when(base.getUserData()).thenReturn(mock(com.bencodez.advancedcore.api.user.UserData.class));
			when(base.getUUID()).thenReturn(uuid.toString());
			when(base.getPlayerName()).thenReturn("Player");
			user = spy(new VotingPluginUser(plugin, base));
			doReturn(uuid).when(user).getJavaUUID();
			doReturn(uuid.toString()).when(user).getUUID();
			doReturn("Player").when(user).getPlayerName();
			doNothing().when(user).loginRewards();
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

		void verifyNoFollowUp() {
			verify(user, never()).loginRewards();
			verify(plugin.getPlaceholders(), never()).onUpdate(user, true);
			verify(plugin.getBackendProxyHandler(), never()).playerOnline("Player", uuid.toString());
		}

		void verifyFollowUpOrder() {
			InOrder ordered = inOrder(user, plugin.getPlaceholders(), plugin.getBackendProxyHandler());
			ordered.verify(user).loginRewards();
			ordered.verify(plugin.getPlaceholders()).onUpdate(user, true);
			ordered.verify(plugin.getBackendProxyHandler()).playerOnline("Player", uuid.toString());
		}
	}
}
