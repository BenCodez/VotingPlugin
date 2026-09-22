package com.bencodez.votingplugin.placeholders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.entity.Player;
import org.bukkit.Bukkit;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.placeholder.CalculatingPlaceholder;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class PlaceHoldersWorkerSafetyTest {
	@Test
	void workerSafeUpdateUsesCapturedPresenceWithoutBukkitScheduling() {
		Fixture fixture = new Fixture();
		AtomicInteger requests = new AtomicInteger();
		PlaceHolder<VotingPluginUser> placeholder = fixture.cachedPlaceholder("Points", requests);
		fixture.publish(placeholder);

		try (var bukkit = mockStatic(Bukkit.class)) {
			fixture.placeholders.onUserDataChange(fixture.advancedUser, "Points");
			assertEquals(0, requests.get(), "offline online-only users must not be refreshed");

			fixture.presence.playerOnline(fixture.player);
			fixture.placeholders.onUserDataChange(fixture.advancedUser, "Points");
			assertEquals(1, requests.get());
			assertEquals("value", placeholder.getCache().get("points").get(fixture.uuid));
			verify(fixture.scheduler, never()).runTask(eq(fixture.plugin), any(Runnable.class));
			verify(fixture.advancedUser, never()).isOnline();

			fixture.presence.playerOffline(fixture.uuid);
			fixture.placeholders.onLogout(fixture.uuid);
			assertFalse(placeholder.getCache().get("points").containsKey(fixture.uuid));
			fixture.placeholders.onUserDataChange(fixture.advancedUser, "Points");
			assertEquals(1, requests.get());
			bukkit.verifyNoInteractions();
		}
	}

	@Test
	void playerDependentUpdateSchedulesOnlyItsEntityOwnedWork() {
		Fixture fixture = new Fixture();
		fixture.presence.playerOnline(fixture.player);
		AtomicInteger requests = new AtomicInteger();
		PlaceHolder<VotingPluginUser> placeholder = fixture.cachedPlaceholder("LastVotes", requests);
		fixture.placeholders.getPlaceholders().add(fixture.placeholders.platformOwned(placeholder));
		fixture.placeholders.publishUserDataChangePlaceholders();

		fixture.placeholders.onUserDataChange(fixture.advancedUser, "LastVotes");

		assertEquals(0, requests.get());
		ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), task.capture(), eq(fixture.player));
		task.getValue().run();
		assertEquals(1, requests.get());
		verify(fixture.advancedUser, never()).isOnline();
	}

	@Test
	void backgroundWarmupSchedulesPlayerDependentWorkOnTheEntityOwner() {
		Fixture fixture = new Fixture();
		fixture.presence.playerOnline(fixture.player);
		AtomicInteger requests = new AtomicInteger();
		PlaceHolder<VotingPluginUser> placeholder = fixture.cachedPlaceholder("LastVotes", requests);
		fixture.placeholders.getPlaceholders().add(fixture.placeholders.platformOwned(placeholder));
		fixture.placeholders.publishUserDataChangePlaceholders();

		fixture.placeholders.onUpdate(fixture.votingUser, true);

		assertEquals(0, requests.get());
		ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), task.capture(), eq(fixture.player));
		task.getValue().run();
		assertEquals(1, requests.get());
	}

	@Test
	void offlineAllCacheUsesTheWorkerSafePlatformFallback() {
		Fixture fixture = new Fixture(PlaceholderCacheLevel.AUTOALL);
		when(fixture.votingUser.getSitesNotVotedOnWithoutOnlinePermissions()).thenReturn(4);
		when(fixture.votingUser.getTotalNumberOfSitesWithoutOnlinePermissions()).thenReturn(6);
		AtomicInteger liveRequests = new AtomicInteger();
		PlaceHolder<VotingPluginUser> available = fixture.cachedPlaceholder("SitesAvailable", "LastVotes", liveRequests);
		PlaceHolder<VotingPluginUser> total = fixture.cachedPlaceholder("SitesAvailableTotal", "LastVotes", liveRequests);
		fixture.placeholders.getPlaceholders().add(
				fixture.placeholders.platformOwnedWithOfflineWorkerFallback(available));
		fixture.placeholders.getPlaceholders().add(
				fixture.placeholders.platformOwnedWithOfflineWorkerFallback(total));
		fixture.placeholders.publishUserDataChangePlaceholders();

		fixture.placeholders.onUserDataChange(fixture.advancedUser, "LastVotes");

		assertEquals("4", available.getCache().get("sitesavailable").get(fixture.uuid));
		assertEquals("6", total.getCache().get("sitesavailabletotal").get(fixture.uuid));
		assertEquals(0, liveRequests.get(), "offline fallback must not resolve live player permissions");
		verify(fixture.scheduler, never()).runTask(eq(fixture.plugin), any(Runnable.class), any(Player.class));
	}

	@Test
	void playerDependentUpdateMovesToTheReplacementEntityOwner() {
		Fixture fixture = new Fixture();
		fixture.presence.playerOnline(fixture.player);
		AtomicInteger requests = new AtomicInteger();
		PlaceHolder<VotingPluginUser> placeholder = fixture.cachedPlaceholder("LastVotes", requests);
		fixture.placeholders.getPlaceholders().add(fixture.placeholders.platformOwned(placeholder));
		fixture.placeholders.publishUserDataChangePlaceholders();

		fixture.placeholders.onUserDataChange(fixture.advancedUser, "LastVotes");
		ArgumentCaptor<Runnable> tasks = ArgumentCaptor.forClass(Runnable.class);
		verify(fixture.scheduler).runTask(eq(fixture.plugin), tasks.capture(), eq(fixture.player));

		Player replacement = mock(Player.class);
		when(replacement.getUniqueId()).thenReturn(fixture.uuid);
		fixture.presence.playerOffline(fixture.uuid);
		fixture.presence.playerOnline(replacement);
		tasks.getValue().run();
		assertEquals(0, requests.get(), "the stale owner task must not touch the replacement player");

		verify(fixture.scheduler).runTask(eq(fixture.plugin), tasks.capture(), eq(replacement));
		tasks.getAllValues().get(1).run();
		assertEquals(1, requests.get());
		verify(fixture.scheduler, times(2)).runTask(eq(fixture.plugin), any(Runnable.class), any(Player.class));
	}

	@Test
	void concurrentCalculatingUpdatesKeepBothCachesCoherent() throws Exception {
		Fixture fixture = new Fixture();
		fixture.presence.playerOnline(fixture.player);
		AtomicInteger sequence = new AtomicInteger();
		CalculatingPlaceholder<VotingPluginUser> placeholder = new CalculatingPlaceholder<>("Next_site") {
			@Override public String placeholderRequest(VotingPluginUser user, String identifier) {
				return getCacheData().get(user.getJavaUUID());
			}
			@Override public String placeholderDataRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(sequence.incrementAndGet());
			}
		};
		placeholder.updateDataKey("LastVotes").setUseCache(true, "next_site");
		fixture.publish(placeholder);

		ExecutorService workers = Executors.newFixedThreadPool(8);
		try {
			List<Callable<Void>> updates = new ArrayList<>();
			for (int i = 0; i < 100; i++) {
				boolean bulkUpdate = i % 2 == 0;
				updates.add(() -> {
					if (bulkUpdate) fixture.placeholders.onUpdate(fixture.votingUser, true);
					else fixture.placeholders.onUserDataChange(fixture.advancedUser, "LastVotes");
					return null;
				});
			}
			workers.invokeAll(updates).forEach(result -> {
				try { result.get(); }
				catch (Exception failure) { throw new AssertionError(failure); }
			});
		} finally {
			workers.shutdownNow();
			assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
		}
		assertEquals(placeholder.getCacheData().get(fixture.uuid),
				placeholder.getCache().get("next_site").get(fixture.uuid));
		assertEquals(1, placeholder.getCache().size());
	}

	private static final class Fixture {
		final UUID uuid = UUID.randomUUID();
		final VotingPluginMain plugin = mock(VotingPluginMain.class);
		final Config config = mock(Config.class);
		final UserManager userManager = mock(UserManager.class);
		final VotingPluginUser votingUser = mock(VotingPluginUser.class);
		final AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		final BukkitScheduler scheduler = mock(BukkitScheduler.class);
		final PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		final Player player = mock(Player.class);
		final PlaceHolders placeholders;

		Fixture() {
			this(PlaceholderCacheLevel.AUTO);
		}

		Fixture(PlaceholderCacheLevel level) {
			when(plugin.getConfigFile()).thenReturn(config);
			when(config.getPlaceholderCacheLevel()).thenReturn(level);
			when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
			when(userManager.getVotingPluginUser(advancedUser)).thenReturn(votingUser);
			when(votingUser.isCached()).thenReturn(true);
			when(votingUser.getJavaUUID()).thenReturn(uuid);
			when(votingUser.getUUID()).thenReturn(uuid.toString());
			when(advancedUser.getJavaUUID()).thenReturn(uuid);
			doThrow(new AssertionError("worker callback accessed live Bukkit online state"))
					.when(advancedUser).isOnline();
			when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
			when(plugin.getBukkitScheduler()).thenReturn(scheduler);
			when(plugin.isEnabled()).thenReturn(true);
			when(player.getUniqueId()).thenReturn(uuid);
			placeholders = new PlaceHolders(plugin);
		}

		PlaceHolder<VotingPluginUser> cachedPlaceholder(String key, AtomicInteger requests) {
			return cachedPlaceholder(key, key, requests);
		}

		PlaceHolder<VotingPluginUser> cachedPlaceholder(String identifier, String key, AtomicInteger requests) {
			PlaceHolder<VotingPluginUser> placeholder = new PlaceHolder<>(identifier) {
				@Override public String placeholderRequest(VotingPluginUser user, String identifier) {
					requests.incrementAndGet();
					return "value";
				}
			};
			placeholder.updateDataKey(key).setUseCache(true, identifier.toLowerCase());
			return placeholder;
		}

		void publish(PlaceHolder<VotingPluginUser> placeholder) {
			placeholders.getPlaceholders().add(placeholder);
			placeholders.publishUserDataChangePlaceholders();
		}
	}
}
