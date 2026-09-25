package com.bencodez.votingplugin.tests.reminders;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.folialib.FoliaLib;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.folialib.impl.ServerImplementation;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderConditions;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderDefinition;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderType;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderOptions;
import com.bencodez.votingplugin.votereminding.store.VoteReminderCooldownStore;

@ExtendWith(MockitoExtension.class)
public class VoteRemindersManagerTest {
	@Test
	void shutdownDrainsClaimRollbackBeforeLateEntityCallback() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		AtomicInteger rollbacks = new AtomicInteger();
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		Method track = VoteRemindersManager.class.getDeclaredMethod("trackClaimRollback", Runnable.class);
		track.setAccessible(true);
		Runnable rollback = (Runnable) track.invoke(manager, (Runnable) rollbacks::incrementAndGet);

		manager.shutdown();
		rollback.run();

		org.junit.jupiter.api.Assertions.assertEquals(1, rollbacks.get());
	}

	@Test
	void shutdownCancelsDelayedReminderEvaluations() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		Method schedule = VoteRemindersManager.class.getDeclaredMethod("scheduleDelayedEvaluation",
				UUID.class, String.class, Map.class, long.class);
		schedule.setAccessible(true);
		schedule.invoke(manager, UUID.randomUUID(), "login", Collections.emptyMap(), 60_000L);

		manager.shutdown();

		Field schedulerField = VoteRemindersManager.class.getDeclaredField("scheduler");
		schedulerField.setAccessible(true);
		ScheduledExecutorService executor = (ScheduledExecutorService) schedulerField.get(manager);
		assertTrue(executor.isTerminated());
		verify(plugin, never()).getBukkitScheduler();
	}

	@Test
	void reminderDeliveryRevalidatesItsPlayerOwnerOnTheEntityScheduler() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player player = mock(Player.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		UUID uuid = UUID.randomUUID();
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		AtomicInteger deliveries = new AtomicInteger();
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		presence.playerOnline(uuid, player);
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			Field options = VoteRemindersManager.class.getDeclaredField("options");
			options.setAccessible(true);
			options.set(manager, new VoteReminderOptions(true, true, ParsedDuration.parse(""), 0,
					ParsedDuration.parse(""), ParsedDuration.parse(""), new VoteReminderConditions(), ""));
			Method schedule = VoteRemindersManager.class.getDeclaredMethod("scheduleIfStillOnline",
					UUID.class, Runnable.class);
			schedule.setAccessible(true);

			assertTrue((boolean) schedule.invoke(manager, uuid, (Runnable) deliveries::incrementAndGet));
			presence.playerOffline(uuid, player);
			scheduled.get().run();
			org.junit.jupiter.api.Assertions.assertEquals(0, deliveries.get());

			presence.playerOnline(uuid, player);
			when(player.isOnline()).thenReturn(false);
			assertTrue((boolean) schedule.invoke(manager, uuid, (Runnable) deliveries::incrementAndGet));
			scheduled.get().run();
			org.junit.jupiter.api.Assertions.assertEquals(0, deliveries.get());
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void reminderDeliveryLeavesTheEntitySchedulerBeforeStartingRewardWork() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player player = mock(Player.class);
		AdvancedCoreConfigOptions configOptions = mock(AdvancedCoreConfigOptions.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		UUID uuid = UUID.randomUUID();
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		AtomicReference<String> deliveryThread = new AtomicReference<>();
		CountDownLatch delivered = new CountDownLatch(1);
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getOptions()).thenReturn(configOptions);
		when(player.isOnline()).thenReturn(true);
		org.mockito.Mockito.doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		presence.playerOnline(uuid, player);
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			Field options = VoteRemindersManager.class.getDeclaredField("options");
			options.setAccessible(true);
			options.set(manager, new VoteReminderOptions(true, true, ParsedDuration.parse(""), 0,
					ParsedDuration.parse(""), ParsedDuration.parse(""), new VoteReminderConditions(), ""));
			Method schedule = VoteRemindersManager.class.getDeclaredMethod("scheduleIfStillOnline",
					UUID.class, Runnable.class);
			schedule.setAccessible(true);

			assertTrue((boolean) schedule.invoke(manager, uuid, (Runnable) () -> {
				deliveryThread.set(Thread.currentThread().getName());
				delivered.countDown();
			}));
			scheduled.get().run();

			assertTrue(delivered.await(1, TimeUnit.SECONDS));
			assertTrue(deliveryThread.get().startsWith("VotingPlugin-VoteReminders-"));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void reminderDeliveryReleasesReservationsWhenTheEntitySchedulerRetires() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		FoliaLib folia = mock(FoliaLib.class);
		ServerImplementation implementation = mock(ServerImplementation.class);
		Player player = mock(Player.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		UUID uuid = UUID.randomUUID();
		AtomicInteger deliveries = new AtomicInteger();
		AtomicInteger unavailable = new AtomicInteger();
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(implementation);
		when(implementation.runAtEntityWithFallback(eq(player), any(), any(Runnable.class)))
				.thenAnswer(invocation -> {
					invocation.getArgument(2, Runnable.class).run();
					return CompletableFuture.completedFuture(EntityTaskResult.ENTITY_RETIRED);
				});
		org.mockito.Mockito.doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		presence.playerOnline(uuid, player);
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			Method schedule = VoteRemindersManager.class.getDeclaredMethod("scheduleIfStillOnline",
					UUID.class, Runnable.class, Runnable.class);
			schedule.setAccessible(true);

			assertTrue((boolean) schedule.invoke(manager, uuid, (Runnable) deliveries::incrementAndGet,
					(Runnable) unavailable::incrementAndGet));

			org.junit.jupiter.api.Assertions.assertEquals(0, deliveries.get());
			org.junit.jupiter.api.Assertions.assertEquals(1, unavailable.get());
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void loginRewardCarriesCapturedOnlineStateIntoWorkerExecution() throws Exception {
		Method method = VoteRemindersManager.class.getDeclaredMethod("onlineRewardBuilder",
				org.bukkit.configuration.ConfigurationSection.class, String.class);
		method.setAccessible(true);
		RewardBuilder builder = (RewardBuilder) method.invoke(null, null, "VoteReminders.Login.Rewards");

		assertTrue(builder.getRewardOptions().isOnlineSet());
		assertTrue(builder.getRewardOptions().isOnline());
		assertFalse(builder.getRewardOptions().isGiveOffline());
	}

	@Test
	void reminderWorkerPreservesTheCapturedPlayerName() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UUID uuid = UUID.randomUUID();
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(uuid, "MchtTester")).thenReturn(user);
		Method method = VoteRemindersManager.class.getDeclaredMethod("snapshotUser",
				VotingPluginMain.class, UUID.class, String.class);
		method.setAccessible(true);

		assertTrue(method.invoke(null, plugin, uuid, "MchtTester") == user);
		verify(manager).getVotingPluginUser(uuid, "MchtTester");
		verify(manager, never()).getVotingPluginUser(uuid, false);
	}

	@Test
	public void cooldownWrapper_tryAcquireGlobal_usesStoreWhenNonZero() {
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID uuid = UUID.randomUUID();

		ParsedDuration global = ParsedDuration.parse("10s");
		VoteRemindersManager.VoteReminderCooldowns cd = new VoteRemindersManager.VoteReminderCooldowns(store, global);

		when(store.tryClaimGlobal(eq(uuid), eq(1000L), eq(10_000L))).thenReturn(true);

		assertTrue(cd.tryAcquireGlobal(uuid, 1000L));
		verify(store).tryClaimGlobal(uuid, 1000L, 10_000L);
	}

	@Test
	public void cooldownWrapper_canFireReminder_usesMaxOfCooldownAndInterval() {
	    VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
	    UUID uuid = UUID.randomUUID();

	    // last fire was at t=1000 (non-zero, so cooldown applies)
	    when(store.getPerReminderMap(uuid)).thenReturn(Collections.singletonMap("Basic", 1000L));

	    VoteRemindersManager.VoteReminderCooldowns cd =
	            new VoteRemindersManager.VoteReminderCooldowns(store, ParsedDuration.parse(""));

	    // req = max(5s, 30s) = 30s
	    assertFalse(cd.canFireReminder(uuid, "Basic", 10_000L,
	            ParsedDuration.parse("5s"), ParsedDuration.parse("30s"))); // 9s since last -> should be blocked

	    assertTrue(cd.canFireReminder(uuid, "Basic", 50_000L,
	            ParsedDuration.parse("5s"), ParsedDuration.parse("30s"))); // 49s since last -> allowed
	}


	@Test
	public void cooldownWrapper_markFired_delegatesToStore() {
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID uuid = UUID.randomUUID();

		VoteRemindersManager.VoteReminderCooldowns cd = new VoteRemindersManager.VoteReminderCooldowns(store,
				ParsedDuration.parse(""));

		cd.markFired(uuid, "Basic", 123L);
		verify(store).setPerReminderLast(uuid, "Basic", 123L);
	}

	@Test
	public void cooldownWrapper_releasesUndeliveredGlobalClaim() {
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID uuid = UUID.randomUUID();
		VoteRemindersManager.VoteReminderCooldowns cd = new VoteRemindersManager.VoteReminderCooldowns(store,
				ParsedDuration.parse("10s"));

		cd.releaseGlobal(uuid, 123L);

		verify(store).releaseGlobalClaim(uuid, 123L);
	}

	@Test
	public void reminderPreference_loadsLegacyDisabledPlayersAndPersistsToggles() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID disabled = UUID.randomUUID();
		UUID enabled = UUID.randomUUID();

		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Arrays.asList(disabled.toString()));

		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			assertFalse(manager.isRemindersEnabled(disabled));
			assertTrue(manager.isRemindersEnabled(enabled));

			assertTrue(manager.toggleReminders(disabled));
			assertFalse(manager.toggleReminders(enabled));

			verify(serverData, times(2)).saveDisabledReminders(any());
			verify(serverData).saveDisabledReminders(argThat(uuids -> uuids.size() == 1 && uuids.contains(enabled)));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void fireAttempt_rechecksReminderPreferenceAfterDelay() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UUID uuid = UUID.randomUUID();

		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		when(user.getJavaUUID()).thenReturn(uuid);

		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			assertFalse(manager.toggleReminders(uuid));

			VoteReminderDefinition definition = new VoteReminderDefinition("Delayed", VoteReminderType.LOGIN, 0,
					ParsedDuration.parse(""), ParsedDuration.parse(""), "", new VoteReminderConditions(),
					ParsedDuration.parse(""));
			Method attemptFireNow = VoteRemindersManager.class.getDeclaredMethod("attemptFireNow",
					VotingPluginUser.class, Player.class, VoteReminderDefinition.class, Map.class);
			attemptFireNow.setAccessible(true);

			assertFalse((boolean) attemptFireNow.invoke(manager, user, null, definition, null));
			verify(user, never()).shouldBeReminded();
		} finally {
			manager.shutdown();
		}
	}
	@Test
	public void snapshotPermissionPreservesNegatedSitePermission() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ServerData serverData = mock(ServerData.class);
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getDisabledReminders()).thenReturn(Collections.emptyList());
		Player player = mock(Player.class);
		when(player.hasPermission("example.hidden")).thenReturn(false);
		VoteRemindersManager manager = new VoteRemindersManager(plugin, store);
		try {
			Method method = VoteRemindersManager.class.getDeclaredMethod("hasPlatformPermission", Player.class, String.class);
			method.setAccessible(true);
			assertTrue((boolean) method.invoke(manager, player, "!example.hidden"));
			when(player.hasPermission("example.hidden")).thenReturn(true);
			assertFalse((boolean) method.invoke(manager, player, "!example.hidden"));
		} finally {
			manager.shutdown();
		}
	}

}
