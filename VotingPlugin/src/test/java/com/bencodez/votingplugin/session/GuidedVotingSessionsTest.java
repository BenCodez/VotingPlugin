package com.bencodez.votingplugin.session;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.votesites.VoteSite;

class GuidedVotingSessionsTest {
    @Test void sameMillisecondFreshLocalVoteRestoresTheInitiallyCoolingSite() throws Exception {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            var sessionsField = GuidedVotingSessions.class.getDeclaredField("sessions"); sessionsField.setAccessible(true);
            var active = (java.util.Map<?, ?>) sessionsField.get(f.sessions);
            long started = ((com.bencodez.votingplugin.core.session.GuidedVoteSession) active.get(f.uuid)).started();
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false); f.lastVotes.put(f.site, started);
            var event = f.event(f.uuid, started, UUID.randomUUID());
            event.setLiveLocalSessionDelivery(true); event.setBackendObservationOrder(System.nanoTime());
            f.sessions.credited(event); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
            f.sessions.credited(event); f.sessions.command(f.player, "check");
            f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void historicalLocalVoteDoesNotUseItsRecentDeliveryOrderToConfirm() {
        try (var f = new Fixture()) {
            long before = System.nanoTime();
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var old = f.event(f.uuid, 1, UUID.randomUUID()); old.setBackendObservationOrder(System.nanoTime());
            f.sessions.credited(old);
            var preSession = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID());
            preSession.setLiveLocalSessionDelivery(true); preSession.setBackendObservationOrder(before);
            f.sessions.credited(preSession);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void hundredSiteRequestLoadsOneLastVoteSnapshotOnTheWorker() {
        try (var f = new Fixture()) {
            var sites = new java.util.ArrayList<VoteSite>();
            for (int i = 0; i < 100; i++) {
                var site = mock(VoteSite.class);
                when(site.getKey()).thenReturn("site" + i); when(site.isEnabled()).thenReturn(true);
                when(site.getPermissionToView()).thenReturn(""); when(site.getDisplayName()).thenReturn("Site " + i);
                when(site.getVoteURL(true)).thenReturn("https://example.org/" + i);
                f.lastVotes.put(site, (long) i);
                when(f.user.canVoteSite(site, (long) i)).thenReturn(true);
                sites.add(site);
            }
            when(f.plugin.getVoteSiteManager().getVoteSites()).thenReturn(sites);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            verify(f.user, never()).getLastVotes();
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.user, times(1)).getLastVotes();
            for (int i = 0; i < 100; i++) verify(f.user).canVoteSite(sites.get(i), (long) i);
            verify(f.user, never()).getTime(any()); verify(f.user, never()).canVoteSite(any());
            verify(f.player).sendMessage("Voting session: 0/100 votes received.");
        }
    }
    @Test void snapshotCooldownOverloadUsesTheExistingDurationDailyAndOffsetDecisions() throws Exception {
        try (var f = new Fixture()) {
            var field = VotingPluginUser.class.getDeclaredField("plugin"); field.setAccessible(true); field.set(f.user, f.plugin);
            doCallRealMethod().when(f.user).canVoteSite(any());
            doCallRealMethod().when(f.user).canVoteSite(any(), anyLong());
            when(f.plugin.getTimeChecker().getTime()).thenReturn(java.time.LocalDateTime.of(2026, 10, 15, 12, 0));
            long timestamp = java.time.LocalDateTime.of(2026, 10, 15, 10, 0)
                    .atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli();
            when(f.user.getTime(f.site)).thenReturn(timestamp);
            when(f.site.getVoteDelay()).thenReturn(com.bencodez.simpleapi.time.ParsedDuration.parse("1h"));
            assertTrue(f.user.canVoteSite(f.site, timestamp)); assertEquals(f.user.canVoteSite(f.site), f.user.canVoteSite(f.site, timestamp));
            when(f.plugin.getOptions().getTimeHourOffSet()).thenReturn(2);
            assertFalse(f.user.canVoteSite(f.site, timestamp)); assertEquals(f.user.canVoteSite(f.site), f.user.canVoteSite(f.site, timestamp));
            when(f.plugin.getOptions().getTimeHourOffSet()).thenReturn(0);
            when(f.site.isVoteDelayDaily()).thenReturn(true); when(f.site.getVoteDelayDailyHour()).thenReturn(11);
            assertTrue(f.user.canVoteSite(f.site, timestamp)); assertEquals(f.user.canVoteSite(f.site), f.user.canVoteSite(f.site, timestamp));
            when(f.site.getVoteDelayDailyHour()).thenReturn(9);
            assertFalse(f.user.canVoteSite(f.site, timestamp)); assertEquals(f.user.canVoteSite(f.site), f.user.canVoteSite(f.site, timestamp));
            clearInvocations(f.user);
            assertTrue(f.user.canVoteSite(f.site, 0)); verify(f.user, never()).getTime(any()); verify(f.user, never()).getLastVotes();
        }
    }
    @Test void liveProxyVoteUsesBackendOrderWhenProxyClockIsBehindOrAhead() {
        for (long remoteTime : new long[] { 1, Long.MAX_VALUE }) try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            f.worker.remove().run(); f.entity.remove().run();
            var event = f.event(f.uuid, remoteTime, UUID.randomUUID());
            event.setBungee(true); event.setProxyQueueClassificationKnown(true);
            event.setBackendObservationOrder(System.nanoTime());
            f.sessions.credited(event); f.sessions.credited(event);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void queuedUnknownOrPreviouslyObservedProxyDeliveryCannotConfirmFreshSession() {
        try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            long oldOrder = System.nanoTime();
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var old = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID());
            old.setBungee(true); old.setProxyQueueClassificationKnown(true); old.setBackendObservationOrder(oldOrder);
            f.sessions.credited(old);
            var queued = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID());
            queued.setBungee(true); queued.setProxyQueueClassificationKnown(true); queued.setQueuedProxyVote(true);
            queued.setBackendObservationOrder(System.nanoTime()); f.sessions.credited(queued);
            var unknown = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID()); unknown.setBungee(true);
            unknown.setBackendObservationOrder(System.nanoTime()); f.sessions.credited(unknown);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void earlyLiveProxyReceiptRestoresSiteAfterCooldownSampleWithoutClockComparison() {
        try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false); f.lastVotes.put(f.site, 1L);
            var event = f.event(f.uuid, 1, UUID.randomUUID()); event.setBungee(true);
            event.setProxyQueueClassificationKnown(true); event.setBackendObservationOrder(System.nanoTime());
            f.sessions.credited(event); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void storageRunsOnWorkerAndOnlyEntityCallbackRenders() {
        try (var fixture = new Fixture()) {
            fixture.sessions.command(fixture.player, "");
            fixture.entity.remove().run();
            verify(fixture.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
            fixture.worker.remove().run();
            verify(fixture.user).canVoteSite(eq(fixture.site), anyLong());
            verify(fixture.player, never()).sendMessage(startsWith("Voting session:"));
            fixture.entity.remove().run();
            verify(fixture.player).sendMessage("Voting session: 0/1 votes received.");
            verify(fixture.user, never()).addTotal();
            verify(fixture.user, never()).playerVote(any(), anyBoolean(), anyBoolean());
        }
    }
    @Test void unrelatedPlayerAndOldCachedVotesDoNotConfirmButNewDuplicateCountsOnce() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            long time = System.currentTimeMillis() + 1000;
            UUID id = UUID.randomUUID();
            f.sessions.credited(f.event(UUID.randomUUID(), time, id));
            f.sessions.credited(f.event(f.uuid, 1, id));
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 0/1 votes received.");
            var real = f.event(f.uuid, time, id);
            f.sessions.credited(real); f.sessions.credited(real);
            // Closing chat needs no mutation. No entity work is triggered by votes.
            assertTrue(f.entity.isEmpty());
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void reloadAndNewerRequestFenceDelayedUiAndRetiredFallbackDoesNotTouchPlayer() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run();
            f.sessions.clear(); f.entity.remove().run();
            verify(f.player, never()).sendMessage(startsWith("Voting session:"));
            f.sessions.command(f.player, ""); f.entity.remove().run();
            f.sessions.command(f.player, "check"); f.entity.remove().run();
            assertEquals(1, f.worker.size());
            f.worker.remove().run();
            assertEquals(1, f.entity.size());
            f.retiredFallback.remove().run();
            verify(f.player, never()).sendMessage(startsWith("Voting session:"));
        }
    }
    @Test void permissionRevocationAfterWorkerSuppressesNameAndUrl() {
        try (var f = new Fixture()) {
            when(f.site.getPermissionToView()).thenReturn("site.a");
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run();
            when(f.player.hasPermission("site.a")).thenReturn(false);
            f.entity.remove().run();
            verify(f.player).sendMessage("> Unavailable site — UNAVAILABLE");
            var components = org.mockito.ArgumentCaptor.forClass(net.md_5.bungee.api.chat.BaseComponent.class);
            verify(f.player.spigot(), atLeastOnce()).sendMessage(components.capture());
            assertTrue(components.getAllValues().stream().noneMatch(c -> c.getClickEvent().getAction()
                    == net.md_5.bungee.api.chat.ClickEvent.Action.OPEN_URL));
            // Navigation components remain; no OPEN_URL component is sent.
            verify(f.player, never()).sendMessage(contains("Site A"));
        }
    }
    @Test void receiptAfterEligibilitySampleIsVisibleOnReopen() {
        try (var f = new Fixture()) {
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false);
            f.lastVotes.put(f.site, System.currentTimeMillis() + 1000);
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            f.sessions.credited(f.event(f.uuid, System.currentTimeMillis() + 1000, UUID.randomUUID()));
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void reloadFencesQueuedFailureNotification() {
        try (var f = new Fixture()) {
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenThrow(new IllegalStateException("fixture"));
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run();
            f.sessions.clear(); f.entity.remove().run();
            verify(f.player, never()).sendMessage(startsWith("Could not check voting status"));
        }
    }
    @Test void checksAreCoalescedAndInvalidatedWorkDoesNotReadStorage() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            for (int i = 0; i < 100; i++) { f.sessions.command(f.player, "check"); f.entity.remove().run(); }
            assertEquals(1, f.worker.size());
            f.sessions.clear(); f.worker.remove().run();
            verify(f.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
        }
    }
    @Test void siteReloadBeforeSamplingUsesCurrentCooldownIdentity() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            VoteSite replacement = mock(VoteSite.class);
            when(replacement.getKey()).thenReturn("a"); when(replacement.getDisplayName()).thenReturn("Site A");
            when(replacement.getPermissionToView()).thenReturn(""); when(replacement.isEnabled()).thenReturn(true);
            when(f.plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of(replacement));
            when(f.user.canVoteSite(eq(replacement), anyLong())).thenReturn(false);
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.user, never()).canVoteSite(eq(f.site), anyLong());
            verify(f.user).canVoteSite(eq(replacement), anyLong());
            verify(f.player).sendMessage("Voting session: 0/0 votes received.");
        }
    }
    @Test void siteReloadAfterSamplingSuppressesPotentiallyStaleLink() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run();
            when(f.plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of());
            f.entity.remove().run();
            verify(f.player).sendMessage("> Unavailable site — UNAVAILABLE");
        }
    }
    @Test void resolvedStorageUuidConfirmsOfflineIdentityIncludingEarlyDelivery() {
        try (var f = new Fixture()) {
            UUID storage = UUID.randomUUID(); when(f.user.getJavaUUID()).thenReturn(storage);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            long time = System.currentTimeMillis() + 1000;
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false); f.lastVotes.put(f.site, time);
            f.sessions.credited(f.event(UUID.randomUUID(), time, UUID.randomUUID()));
            f.sessions.credited(f.event(storage, time, UUID.randomUUID()));
            f.sessions.credited(f.event(UUID.randomUUID(), time, UUID.randomUUID()));
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
            f.sessions.credited(f.event(storage, time + 1, UUID.randomUUID()));
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void initialEntityHandoffIsFencedAcrossReload() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, "restart");
            f.sessions.clear(); f.entity.remove().run();
            assertTrue(f.worker.isEmpty());
            verify(f.player, never()).sendMessage(anyString());
        }
    }
    @Test void disabledDoesNotStartStorageOrSession() {
        try (var f = new Fixture()) {
            f.config.set("GuidedVotingSession.Enabled", false);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            assertTrue(f.worker.isEmpty());
            verify(f.player).sendMessage("Guided voting sessions are disabled by this server.");
        }
    }
    @Test void reloadAdmitsFreshRequestWhileOldWorkerCannotReleaseItsLock() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            f.sessions.clear();
            f.sessions.command(f.player, ""); f.entity.remove().run();
            assertEquals(2, f.worker.size());
            f.worker.remove().run();
            verify(f.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
            f.sessions.command(f.player, "check"); f.entity.remove().run();
            assertEquals(1, f.worker.size());
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void repeatedReloadDoesNotRemoveTheGlobalOutstandingWorkBound() {
        try (var f = new Fixture()) {
            for (int i = 0; i < 100; i++) {
                f.sessions.clear(); f.sessions.command(f.player, ""); f.entity.remove().run();
            }
            assertEquals(64, f.worker.size());
            while (!f.worker.isEmpty()) f.worker.remove().run();
            verify(f.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void earlyConfirmedReceiptSurvivesSiteDisappearingBeforeFirstSample() {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            f.sessions.credited(f.event(f.uuid, System.currentTimeMillis() + 1000, UUID.randomUUID()));
            when(f.plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of());
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("> Unavailable site — UNAVAILABLE");
            when(f.plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of(f.site));
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void configuredForceLinksAndStoredPlayerPlaceholderProduceTheEstablishedDestination() throws Exception {
        try (var f = new Fixture()) {
            var pluginField = VoteSite.class.getDeclaredField("plugin"); pluginField.setAccessible(true); pluginField.set(f.site, f.plugin);
            doCallRealMethod().when(f.site).setVoteURL(anyString());
            doCallRealMethod().when(f.site).getVoteURL(anyBoolean());
            when(f.plugin.getConfigFile().isFormatCommandsVoteForceLinks()).thenReturn(true);
            when(f.user.getPlayerName()).thenReturn("Stored_Name");
            f.site.setVoteURL("www.example.org/vote?username=%PLAYER%");
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var components = org.mockito.ArgumentCaptor.forClass(net.md_5.bungee.api.chat.BaseComponent.class);
            verify(f.player.spigot(), atLeastOnce()).sendMessage(components.capture());
            assertTrue(components.getAllValues().stream().anyMatch(c -> c.getClickEvent().getAction()
                    == net.md_5.bungee.api.chat.ClickEvent.Action.OPEN_URL
                    && c.getClickEvent().getValue().equals("http://www.example.org/vote?username=Stored_Name")));
            verify(f.site, never()).getVoteURL(false);
        }
    }
    @Test void formattedLinksRenderPlayerPlaceholderBeforeHttpValidation() {
        try (var f = new Fixture()) {
            when(f.site.getVoteURL(true)).thenReturn("[Text=\"Vote for %player%\",url=\"https://example.org/vote/%player%\"]");
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var components = org.mockito.ArgumentCaptor.forClass(net.md_5.bungee.api.chat.BaseComponent.class);
            verify(f.player.spigot(), atLeastOnce()).sendMessage(components.capture());
            assertTrue(components.getAllValues().stream().anyMatch(c -> c.getClickEvent().getAction()
                    == net.md_5.bungee.api.chat.ClickEvent.Action.OPEN_URL
                    && c.getClickEvent().getValue().equals("https://example.org/vote/Alice")));
        }
    }
    private static class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final Player player = mock(Player.class);
        final UUID uuid = UUID.randomUUID();
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final VoteSite site = mock(VoteSite.class);
        final YamlConfiguration config = new YamlConfiguration();
        final java.util.HashMap<VoteSite, Long> lastVotes = new java.util.HashMap<>();
        final Queue<Runnable> worker = new ArrayDeque<>(), entity = new ArrayDeque<>(), retiredFallback = new ArrayDeque<>();
        final MockedStatic<BukkitCompletionScheduler> scheduler = mockStatic(BukkitCompletionScheduler.class);
        final GuidedVotingSessions sessions = new GuidedVotingSessions(plugin);
        Fixture() {
            config.set("GuidedVotingSession.Enabled", true);
            when(plugin.getConfigFile().getData()).thenReturn(config);
            when(player.isOnline()).thenReturn(true);
            when(player.spigot()).thenReturn(mock(Player.Spigot.class));
            when(player.getUniqueId()).thenReturn(uuid);
            when(player.getName()).thenReturn("Alice");
            when(player.hasPermission(anyString())).thenReturn(true);
            when(site.getKey()).thenReturn("a"); when(site.getDisplayName()).thenReturn("Site A");
            when(site.getPermissionToView()).thenReturn(""); when(site.isEnabled()).thenReturn(true);
            when(site.getVoteURL(true)).thenReturn("https://example.org/a");
            when(plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of(site));
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, "Alice")).thenReturn(user);
            when(user.canVoteSite(eq(site), anyLong())).thenReturn(true);
            when(user.getLastVotes()).thenReturn(lastVotes);
            when(user.getJavaUUID()).thenReturn(uuid);
            when(user.getPlayerName()).thenReturn("Alice");
            ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
            when(plugin.getTimer()).thenReturn(timer);
            doAnswer(i -> { worker.add(i.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), eq(player), any(Runnable.class), any(Runnable.class), any(Runnable.class)))
                    .thenAnswer(i -> {
                        Runnable task = i.getArgument(2);
                        entity.add(task);
                        retiredFallback.add(i.getArgument(3));
                        return null;
                    });
        }
        PlayerPostVoteEvent event(UUID playerId, long time, UUID id) {
            var e = new PlayerPostVoteEvent(site, user, true, false, time, true, "service", playerId, "Alice", id);
            return e;
        }
        @Override public void close() { scheduler.close(); }
    }
}
