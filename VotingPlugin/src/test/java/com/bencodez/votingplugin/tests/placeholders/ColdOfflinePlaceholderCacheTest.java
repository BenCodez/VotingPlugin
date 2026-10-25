package com.bencodez.votingplugin.tests.placeholders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.placeholder.CalculatingPlaceholder;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

class ColdOfflinePlaceholderCacheTest {
    private VotingPluginMain previousPlugin;

    @BeforeEach void rememberPlugin() { previousPlugin = VotingPluginMain.plugin; }
    @AfterEach void restorePlugin() { VotingPluginMain.plugin = previousPlugin; }

    @Test void autoAllPopulatesConfiguredMonthlyTotal() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.AUTOALL, "total_monthly", "42");
    }

    @Test void specificAllPopulatesConfiguredMonthlyTotal() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.SPECIFICALL, "total_monthly", "42");
    }

    @Test void autoAllPopulatesConfiguredPoints() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.AUTOALL, "points", "17");
    }

    @Test void specificAllPopulatesConfiguredPoints() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.SPECIFICALL, "points", "17");
    }

    @Test void autoAllAdmitsAndPopulatesUnconfiguredMonthlyTotal() {
        assertAutomaticAdmission("total_monthly", "42");
    }

    @Test void autoAllAdmitsAndPopulatesUnconfiguredPoints() {
        assertAutomaticAdmission("points", "17");
    }

    @Test void onlineOnlyModesLeaveColdOfflineEntriesEmpty() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("...", f.request("points"));
            f.drain();
            assertEquals("...", f.request("points"));
            f.drain();
            verify(f.user, never()).getPoints();
            assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        }
    }

    @Test void configuredColdOnlineEntriesStillPopulateInEveryCachingMode() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC,
                PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.presence.playerOnline(f.uuid, f.player);
            assertEquals("...", f.request("points"));
            f.drain();
            assertEquals("17", f.request("points"));
            assertTrue(f.pending.isEmpty());
        }
    }

    @Test void warmOfflineEntriesStillRefreshInBothAllPlayerModes() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.placeholder("points").getCache().get("points").put(f.uuid, "old");
            f.placeholders.schedulePlaceholderCheck(f.user);
            f.drain();
            assertEquals("17", f.request("points"));
            assertTrue(f.pending.isEmpty());
        }
    }

    @Test void noneModeKeepsDirectUncachedEvaluation() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        assertEquals("17", f.request("points"));
        assertEquals("42", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
        assertFalse(f.placeholder("points").isUsesCache());
    }

    @Test void explicitNoCacheAndProcessSuffixesKeepTheirContracts() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("17", f.request("points_nocache"));
            assertEquals("17", f.request("points_process"));
            assertTrue(f.pending.isEmpty());
            assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        }
    }

    @Test void specificAllDoesNotAutomaticallyAdmitAnUnconfiguredIdentifier() {
        Fixture f = new Fixture(PlaceholderCacheLevel.SPECIFICALL);
        assertEquals("..", f.request("points"));
        assertTrue(f.pending.isEmpty());
        assertFalse(f.placeholder("points").isUsesCache());
        verify(f.user, never()).getPoints();
    }

    @Test void unknownIdentifiersDoNotScheduleOrReadUserData() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("Not a valid placeholder", f.request("unknown_test_placeholder"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, never()).getPoints();
        }
    }

    @Test void admittedOfflinePopulationCanRunOnAnActualWorkerWithoutBukkitReads() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points", "total_monthly");
        assertEquals("...", f.request("points"));
        try (var worker = Executors.newSingleThreadExecutor()) {
            worker.submit(() -> {
                try (var bukkit = mockStatic(Bukkit.class)) {
                    f.drain();
                    bukkit.verifyNoInteractions();
                }
            }).get(5, TimeUnit.SECONDS);
        }
        assertEquals("17", f.request("points"));
        assertEquals("42", f.request("total_monthly"));
        verify(f.user, never()).isOnline();
        verify(f.user, never()).getPlayer();
    }

    @Test void realCalculatingPlaceholderPopulatesDataAndValueTogether() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "next_example");
            assertEquals("...", f.request("next_example"));
            f.drain();
            CalculatingPlaceholder<?> calculating = (CalculatingPlaceholder<?>) f.placeholder("next_example");
            assertEquals("1234", calculating.getCacheData().get(f.uuid));
            assertEquals("wait", calculating.getCache().get("next_example").get(f.uuid));
            assertEquals("wait", f.request("next_example"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, times(1)).getTime(f.site);
            verify(f.user, never()).voteCommandNextInfo(f.site);
        }
    }

    @Test void coldPlatformPlaceholdersUseOnlyTheExplicitOfflineFallback() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "sitesavailable", "sitesavailabletotal");
            when(f.user.getSitesNotVotedOnWithoutOnlinePermissions()).thenReturn(4);
            when(f.user.getTotalNumberOfSitesWithoutOnlinePermissions()).thenReturn(6);
            assertEquals("...", f.request("sitesavailable"));
            try (var bukkit = mockStatic(Bukkit.class)) {
                f.drain();
                bukkit.verifyNoInteractions();
            }
            assertEquals("4", f.request("sitesavailable"));
            assertEquals("6", f.request("sitesavailabletotal"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, never()).getSitesNotVotedOn();
            verify(f.user, never()).getTotalNumberOfSites();
            verify(f.scheduler, never()).runTask(eq(f.plugin), any(Runnable.class), any(Player.class));
        }
    }

    @Test void aPlatformPlaceholderWithoutOfflineFallbackIsNotEvaluatedOnTheWorker() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL);
        PlaceHolder<VotingPluginUser> liveOnly = new PlaceHolder<>("liveonly") {
            @Override public String placeholderRequest(VotingPluginUser user, String identifier) {
                throw new AssertionError("live-only producer evaluated without entity ownership");
            }
        };
        f.placeholders.getPlaceholders().add(PlaceHoldersAccess.platformOwned(f.placeholders, liveOnly));
        PlaceHoldersAccess.publishUserDataChangePlaceholders(f.placeholders);
        assertEquals("..", f.request("liveonly"));
        f.drain();
        assertFalse(liveOnly.getCache().get("liveonly").containsKey(f.uuid));
        verify(f.scheduler, never()).runTask(eq(f.plugin), any(Runnable.class), any(Player.class));
    }

    @Test void coldPlatformWorkMovesToReplacementOwnerBeforePublishing() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "sitesavailable");
        when(f.user.canVoteSite(f.site)).thenReturn(true);
        when(f.site.getPermissionToView()).thenReturn("");
        f.presence.playerOnline(f.uuid, f.player);
        assertEquals("...", f.request("sitesavailable"));
        f.drain();
        ArgumentCaptor<Runnable> first = ArgumentCaptor.forClass(Runnable.class);
        verify(f.scheduler).runTask(eq(f.plugin), first.capture(), eq(f.player));
        assertFalse(f.placeholder("sitesavailable").getCache().get("sitesavailable").containsKey(f.uuid));
        Player replacement = mock(Player.class);
        f.presence.playerOffline(f.uuid, f.player);
        f.placeholders.onLogout(f.uuid);
        f.presence.playerOnline(f.uuid, replacement);
        first.getValue().run();
        assertFalse(f.placeholder("sitesavailable").getCache().get("sitesavailable").containsKey(f.uuid));
        ArgumentCaptor<Runnable> second = ArgumentCaptor.forClass(Runnable.class);
        verify(f.scheduler).runTask(eq(f.plugin), second.capture(), eq(replacement));
        second.getValue().run();
        assertEquals("1", f.request("sitesavailable"));
        verify(f.user, never()).getSitesNotVotedOn();
    }

    @Test void queuedColdWorkHonorsTheReloadedOnlineOnlyMode() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        assertEquals("...", f.request("points"));
        when(f.plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTO);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            f.placeholders.reload();
        }
        f.drain();
        verify(f.user, never()).getPoints();
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
    }

    @Test void logoutBeforeAdmittedWorkPreservesEachModesOfflinePolicy() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC,
                PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.presence.playerOnline(f.uuid, f.player);
            assertEquals("...", f.request("points"));
            f.presence.playerOffline(f.uuid, f.player);
            f.placeholders.onLogout(f.uuid);
            f.drain();
            if (level.onlineOnly()) {
                assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
                verify(f.user, never()).getPoints();
            } else {
                assertEquals("17", f.request("points"));
                assertTrue(f.pending.isEmpty());
            }
        }
    }

    @Test void autoAllNonLoginCompletionDoesNotReadColdStorage() {
        assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel.AUTOALL);
    }

    @Test void specificAllNonLoginCompletionDoesNotReadColdStorage() {
        assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel.SPECIFICALL);
    }

    private static void assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel level) {
        Fixture f = new Fixture(level, "points", "total_monthly");
        doThrow(new AssertionError("platform completion read cold points storage")).when(f.user).getPoints();
        doThrow(new AssertionError("platform completion read cold total storage"))
                .when(f.user).getTotal(TopVoter.Monthly);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        BukkitCompletionScheduler.run(f.plugin, null, () -> f.placeholders.onUpdate(f.user, false));
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        assertFalse(f.placeholder("total_monthly").getCache().get("total_monthly").containsKey(f.uuid));
        assertTrue(f.pending.isEmpty());
        verify(f.user, never()).getPoints();
        verify(f.user, never()).getTotal(TopVoter.Monthly);
    }

    private static void assertConfiguredColdEntry(PlaceholderCacheLevel level, String identifier, String value) {
        Fixture f = new Fixture(level, identifier);
        assertEquals("...", f.request(identifier));
        assertEquals(1, f.pending.size());
        f.drain();
        assertEquals(value, f.request(identifier));
        assertEquals(value, f.request(identifier));
        assertTrue(f.pending.isEmpty(), "successful first population must stop empty rescheduling");
        if (identifier.equals("points")) verify(f.user, times(1)).getPoints();
        else verify(f.user, times(1)).getTotal(TopVoter.Monthly);
    }

    private static void assertAutomaticAdmission(String identifier, String value) {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL);
        assertEquals("..", f.request(identifier));
        assertEquals(1, f.pending.size());
        f.drain();
        assertEquals(value, f.request(identifier));
        assertEquals(value, f.request(identifier));
        assertTrue(f.pending.isEmpty());
        verify(f.plugin.getServerData(), times(1)).addAutoCachedPlaceholder(identifier);
    }

    private static final class Fixture {
        final UUID uuid = UUID.fromString("f2a01ff9-0787-4c22-a693-3e2c80354c65");
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final OfflinePlayer offline = mock(OfflinePlayer.class);
        final Player player = mock(Player.class);
        final VoteSite site = mock(VoteSite.class);
        final BukkitScheduler scheduler = mock(BukkitScheduler.class);
        final PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
        final ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        final Deque<Runnable> pending = new ArrayDeque<>();
        final PlaceHolders placeholders;

        Fixture(PlaceholderCacheLevel level, String... configured) {
            when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(level);
            when(plugin.getConfigFile().getCachedPlaceholders()).thenReturn(new ArrayList<>(List.of(configured)));
            when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
            when(plugin.getTimer()).thenReturn(timer);
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            when(site.getKey()).thenReturn("example");
            when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(List.of(site)));
            when(plugin.isEnabled()).thenReturn(true);
            doAnswer(call -> { pending.addLast(call.getArgument(0)); return null; })
                    .when(timer).execute(any(Runnable.class));
            when(offline.getUniqueId()).thenReturn(uuid);
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(offline)).thenReturn(user);
            when(user.getJavaUUID()).thenReturn(uuid);
            when(user.getUUID()).thenReturn(uuid.toString());
            when(user.getTotal(TopVoter.Monthly)).thenReturn(42);
            when(user.getPoints()).thenReturn(17);
            when(user.getTime(site)).thenReturn(1234L);
            when(user.voteCommandNextInfo(site, 1234L)).thenReturn("wait");
            doThrow(new AssertionError("worker read live player state")).when(user).isOnline();
            doThrow(new AssertionError("worker requested live player")).when(user).getPlayer();
            placeholders = new PlaceHolders(plugin);
            VotingPluginMain.plugin = plugin;
            placeholders.load();
        }

        String request(String identifier) {
            try (var bukkit = mockStatic(Bukkit.class)) {
                bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
                return placeholders.getPlaceHolder(offline, identifier, false);
            }
        }

        void drain() {
            int budget = 20;
            while (!pending.isEmpty()) {
                assertTrue(budget-- > 0, "callback queue must terminate");
                pending.removeFirst().run();
            }
        }

        PlaceHolder<VotingPluginUser> placeholder(String identifier) {
            return placeholders.getPlaceholders().stream().filter(p -> p.matches(identifier)).findFirst().orElseThrow();
        }
    }
}
