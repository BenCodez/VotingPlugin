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
    private static void expireCurrent(Fixture f) throws Exception {
        var field = GuidedVotingSessions.class.getDeclaredField("sessions"); field.setAccessible(true);
        var session = ((java.util.Map<?, ?>) field.get(f.sessions)).get(f.uuid);
        var started = com.bencodez.votingplugin.core.session.GuidedVoteSession.class.getDeclaredField("started"); started.setAccessible(true);
        started.setLong(session, System.currentTimeMillis() - 60_001L);
    }
    @Test void offlineProxyGuideCapturesCanonicalStorageUuidOnOwnerBeforeWorkerLookup() {
        try (var f = new Fixture(); var resolver = mockStatic(com.bencodez.advancedcore.api.player.UuidLookup.class)) {
            UUID storageId = UUID.randomUUID(), changedId = UUID.randomUUID();
            var lookup = mock(com.bencodez.advancedcore.api.player.UuidLookup.class);
            resolver.when(com.bencodez.advancedcore.api.player.UuidLookup::getInstance).thenReturn(lookup);
            when(lookup.getCachedUUID("Alice")).thenReturn(storageId.toString());
            when(f.plugin.getOptions().isOnlineMode()).thenReturn(false);
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            when(f.plugin.getVotingPluginUserManager().getVotingPluginUser(storageId,"Alice")).thenReturn(f.user);
            when(f.user.getJavaUUID()).thenReturn(storageId);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            verify(f.plugin.getVotingPluginUserManager(),never()).getVotingPluginUser(any(UUID.class),anyString());
            when(lookup.getCachedUUID("Alice")).thenReturn(changedId.toString());
            f.worker.remove().run(); f.entity.remove().run();
            verify(f.plugin.getVotingPluginUserManager()).getVotingPluginUser(storageId,"Alice");
            verify(f.plugin.getVotingPluginUserManager(),never()).getVotingPluginUser(f.uuid,"Alice");
            verify(f.player).sendMessage("Voting session: 0/1 votes received.");
            when(lookup.getCachedUUID("Alice")).thenReturn(storageId.toString());
            f.lastVotes.put(f.site,1234L); when(f.user.canVoteSite(f.site,1234L)).thenReturn(false);
            f.sessions.command(f.player,"check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.user).canVoteSite(f.site,1234L);
            verify(f.player).sendMessage(contains("UNAVAILABLE"));
            verify(lookup,times(2)).getCachedUUID("Alice");
        }
    }
    @Test void expiredQueuedReadAllowsRestartWithoutReleasingAnotherRequestsAdmission() throws Exception {
        try (var f = new Fixture()) {
            f.config.set("GuidedVotingSession.TimeoutMinutes", 1);
            f.sessions.command(f.player, ""); f.entity.remove().run(); expireCurrent(f);
            f.sessions.command(f.player, "restart"); f.entity.remove().run(); assertEquals(2, f.worker.size());
            f.worker.remove().run(); verify(f.user, never()).getLastVotes();
            f.sessions.command(f.player, "check"); f.entity.remove().run();
            verify(f.player).sendMessage(startsWith("Voting status is already")); assertEquals(1, f.worker.size());
            f.worker.remove().run(); f.entity.remove().run(); verify(f.player).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void expiredOwnerCallbackCannotRenderOrConfirmItsOldGuide() throws Exception {
        try (var f = new Fixture()) {
            f.config.set("GuidedVotingSession.TimeoutMinutes", 1);
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); expireCurrent(f);
            f.entity.remove().run(); verify(f.player, never()).sendMessage(startsWith("Voting session:"));
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(1)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void sameMillisecondFreshLocalVoteRestoresTheInitiallyCoolingSite() throws Exception {
        try (var f = new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run();
            var sessionsField = GuidedVotingSessions.class.getDeclaredField("sessions"); sessionsField.setAccessible(true);
            var active = (java.util.Map<?, ?>) sessionsField.get(f.sessions);
            long started = ((com.bencodez.votingplugin.core.session.GuidedVoteSession) active.get(f.uuid)).started();
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false); f.lastVotes.put(f.site, started);
            var event = f.event(f.uuid, started, UUID.randomUUID());
            event.setLiveLocalSessionDelivery(true); event.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next());
            f.sessions.credited(event); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 1/1 votes received.");
            f.sessions.credited(event); f.sessions.command(f.player, "check");
            f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 1/1 votes received.");
        }
    }
    @Test void unknownRecoveredLocalIngressCannotConfirmEvenWithFreshProcessingTimestamp() {
        try(var f=new Fixture()) {
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var replay=f.event(f.uuid,Long.MAX_VALUE,UUID.randomUUID()); replay.setUnconfirmedLocalSessionDelivery(true);
            f.sessions.credited(replay); f.sessions.credited(replay);
            f.sessions.command(f.player,"check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player,times(2)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void historicalLocalVoteDoesNotUseItsRecentDeliveryOrderToConfirm() {
        try (var f = new Fixture()) {
            long before = com.bencodez.votingplugin.core.session.VoteObservationSequence.next();
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var old = f.event(f.uuid, 1, UUID.randomUUID()); old.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next());
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
    @Test void proxyEventCannotProveFreshnessWhenProxyClockIsBehindOrAhead() {
        for (long remoteTime : new long[] { 1, Long.MAX_VALUE }) try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            f.worker.remove().run(); f.entity.remove().run();
            var event = f.event(f.uuid, remoteTime, UUID.randomUUID());
            event.setBungee(true); event.setProxyQueueClassificationKnown(true);
            event.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next());
            f.sessions.credited(event); f.sessions.credited(event);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void queuedUnknownOrPreviouslyObservedProxyDeliveryCannotConfirmFreshSession() {
        try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            long oldOrder = com.bencodez.votingplugin.core.session.VoteObservationSequence.next();
            f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            var old = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID());
            old.setBungee(true); old.setProxyQueueClassificationKnown(true); old.setBackendObservationOrder(oldOrder);
            f.sessions.credited(old);
            var queued = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID());
            queued.setBungee(true); queued.setProxyQueueClassificationKnown(true); queued.setQueuedProxyVote(true);
            queued.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next()); f.sessions.credited(queued);
            var unknown = f.event(f.uuid, Long.MAX_VALUE, UUID.randomUUID()); unknown.setBungee(true);
            unknown.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next()); f.sessions.credited(unknown);
            f.sessions.command(f.player, "check"); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player, times(2)).sendMessage("Voting session: 0/1 votes received.");
        }
    }
    @Test void earlyProxyEventCannotRestoreInitiallyCoolingSiteWithoutCrossNodeOrder() {
        try (var f = new Fixture()) {
            when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            f.sessions.command(f.player, ""); f.entity.remove().run();
            when(f.user.canVoteSite(eq(f.site), anyLong())).thenReturn(false); f.lastVotes.put(f.site, 1L);
            var event = f.event(f.uuid, 1, UUID.randomUUID()); event.setBungee(true);
            event.setProxyQueueClassificationKnown(true); event.setBackendObservationOrder(com.bencodez.votingplugin.core.session.VoteObservationSequence.next());
            f.sessions.credited(event); f.worker.remove().run(); f.entity.remove().run();
            verify(f.player).sendMessage("Voting session: 0/0 votes received.");
            verify(f.player).sendMessage("No eligible, visible voting sites. Try /vote session restart after your cooldowns end.");
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
    @Test void durableMultiProxyForwardingCannotConfirmANewGuideButStillProcessesTheVote() throws Exception {
        for (boolean allServers : new boolean[] { false, true }) {
            for (boolean validationKnown : new boolean[] { false, true }) {
                for (long remoteTime : new long[] { 1L, Long.MAX_VALUE }) try (var f = new Fixture()) {
                    var backend = acceptedBackend(f);
                    var proxy = receiverProxy(allServers);
                    UUID occurrence = UUID.randomUUID();
                    var outbox = new com.bencodez.votingplugin.timequeue.VoteTimeQueue(occurrence, "Alice", "service",
                            remoteTime, false, java.util.Set.of(), java.util.Set.of(), "", false, f.uuid.toString());
                    outbox.setRealVote(true); outbox.setMultiProxyOrigin("Primary");
                    if (validationKnown) outbox.setDelayValidated(true);
                    var producer = com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredMethod(
                            "multiProxyVoteEnvelope", com.bencodez.votingplugin.timequeue.VoteTimeQueue.class);
                    producer.setAccessible(true);
                    var pending = (com.bencodez.simpleapi.servercomm.codec.JsonEnvelope) producer.invoke(proxy, outbox);
                    // The durable vote exists before the guide; its forwarding wakeup runs afterwards.
                    startGuide(f);
                    var handler = mock(com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler.class, CALLS_REAL_METHODS);
                    doAnswer(i -> {
                        proxy.receive(i.getArgument(0), i.getArgument(1), i.getArgument(2), i.getArgument(3),
                                i.getArgument(4), i.getArgument(5), i.getArgument(6), i.getArgument(7),
                                i.getArgument(8), i.getArgument(9), i.getArgument(10));
                        return null;
                    }).when(handler).triggerVote(anyString(), anyString(), anyBoolean(), anyBoolean(), anyLong(),
                            any(com.bencodez.votingplugin.proxy.VoteTotalsSnapshot.class), anyString(), any(UUID.class),
                            anyString(), anyBoolean(), anyBoolean());
                    var handle = com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler.class.getDeclaredMethod(
                            "handleEnvelope", com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.class);
                    handle.setAccessible(true);
                    long beforeReceive = System.currentTimeMillis();
                    handle.invoke(handler, pending);
                    long afterReceive = System.currentTimeMillis();
                    var delivered = proxy.getLastVoteEnvelope(); assertNotNull(delivered);
                    var vote = com.bencodez.votingplugin.proxy.VotingPluginWire.readVote(delivered);
                    assertEquals(occurrence, vote.voteId);
                    assertTrue(vote.time >= beforeReceive && vote.time <= afterReceive,
                            "normal backend cooldown time remains local to the receiving proxy");
                    assertTrue(vote.queuedDeliveryKnown); assertFalse(vote.queuedDelivery);
                    assertEquals(validationKnown, vote.delayValidationKnown);
                    assertEquals(validationKnown, vote.delayValidated);
                    assertEquals("Primary", delivered.getFields().get(com.bencodez.votingplugin.proxy.VotingPluginWire.K_MULTI_PROXY_ORIGIN));
                    backend.handleOrderedVote(delivered, result -> assertEquals(
                            com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome.COMPLETE, result));
                    verify(f.user).canVoteSite(f.site); // Freshness exclusion must not bypass normal delay enforcement.
                    verify(f.user).setTime(f.site, vote.time);
                    verify(f.user).playerVote(eq(f.site), anyBoolean(), eq(true));
                    verify(f.user).addTotal(); verify(f.user).addTotalDaily(); verify(f.user).addTotalWeekly(); verify(f.user).addPoints();
                    var post = org.mockito.ArgumentCaptor.forClass(org.bukkit.event.Event.class);
                    verify(f.plugin.getServer().getPluginManager(), times(2)).callEvent(post.capture());
                    var credited = (PlayerPostVoteEvent) post.getAllValues().get(1);
                    assertEquals(occurrence, credited.getVoteUUID()); assertEquals(vote.time, credited.getVoteTime());
                    assertTrue(credited.isUnconfirmedProxySessionDelivery()); assertFalse(credited.isQueuedProxyVote());
                    checkGuide(f, 0);
                }
            }
        }
    }

    @Test void durableDeliveryRetriesCannotConfirmAGuideOpenedAfterInitialAdmission(@org.junit.jupiter.api.io.TempDir java.nio.file.Path root) throws Exception {
        for (String mode : new String[] {"negotiation", "rejection", "restart"}) try(var f=new Fixture()) {
            var backend=acceptedBackend(f);var proxy=receiverProxy(false);proxy.setMethod(com.bencodez.votingplugin.proxy.BungeeMethod.HTTP);
            var type=Class.forName("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox");var constructor=type.getDeclaredConstructor(java.nio.file.Path.class);constructor.setAccessible(true);
            var file=root.resolve(mode+".dat");var outboxField=com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredField("reliableVoteDeliveryOutbox");outboxField.setAccessible(true);outboxField.set(proxy,constructor.newInstance(file));
            var legacyField=com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredField("legacyVoteDeliveryServers");legacyField.setAccessible(true);
            ((java.util.Set<?>)legacyField.get(proxy)).clear();
            var reliableField=com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredField("reliableVoteDeliveryServers");reliableField.setAccessible(true);
            @SuppressWarnings("unchecked") var reliable=(java.util.Set<String>)reliableField.get(proxy);
            if(mode.equals("rejection")) { reliable.add("server1");proxy.setStableHttpDeliveryResult(false); }
            UUID occurrence=UUID.randomUUID();proxy.vote("Alice","service",true,false,1L,null,f.uuid.toString(),occurrence);
            if(mode.equals("rejection")) assertEquals("false",proxy.getLastVoteEnvelope().getFields().get(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH));
            if(mode.equals("restart")) outboxField.set(proxy,constructor.newInstance(file));
            startGuide(f);reliable.add("server1");proxy.setStableHttpDeliveryResult(true);
            var retry=com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredMethod("retryReliableVoteDeliveries");retry.setAccessible(true);retry.invoke(proxy);
            var delivered=proxy.getLastVoteEnvelope();assertNotNull(delivered);assertEquals("false",delivered.getFields().get(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH));
            assertEquals(occurrence,com.bencodez.votingplugin.proxy.VotingPluginWire.readVote(delivered).voteId);
            backend.handleOrderedVote(delivered,result->assertEquals(com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome.COMPLETE,result));
            verify(f.user).playerVote(eq(f.site),anyBoolean(),eq(true));verify(f.user).addPoints();checkGuide(f,0);
        }
    }

    @Test void actualLegacyMultiProxyCallbackNeverMintsFreshGuideProvenance() throws Exception {
        for (boolean allServers : new boolean[] { false, true }) try (var f = new Fixture()) {
            var backend=acceptedBackend(f);var proxy=receiverProxy(allServers);
            // Build the real receiver callback without opening a transport.
            when(proxy.getConfig().getMultiProxySupport()).thenReturn(false);proxy.loadMultiProxySupport();
            when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
            var handlerField=com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredField("multiProxyHandler");
            handlerField.setAccessible(true);var handler=handlerField.get(proxy);
            var live=com.bencodez.votingplugin.proxy.VotingPluginWire.vote("Alice",f.uuid.toString(),"service",1L,
                    true,true,"",UUID.randomUUID(),false,false,1,1);
            var legacy=com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder(live.getSubChannel()).schema(live.getSchema());
            for(var entry:live.getFields().entrySet()) if(!entry.getKey().equals(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH)) legacy.put(entry.getKey(),entry.getValue());
            startGuide(f);
            var receive=com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler.class.getDeclaredMethod("handleEnvelope",com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.class);
            receive.setAccessible(true);receive.invoke(handler,legacy.build());
            var delivered=proxy.getLastVoteEnvelope();assertNotNull(delivered);
            assertEquals("false",delivered.getFields().get(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH));
            backend.handleOrderedVote(delivered, result->assertEquals(com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome.COMPLETE,result));
            verify(f.user).playerVote(eq(f.site),anyBoolean(),eq(true));verify(f.user).addPoints();checkGuide(f,0);
        }
    }

    @Test void directProxyIngressBeforeGuideCannotConfirmAfterTransportDelayEvenWithOlderPositiveHint() throws Exception {
        for (String hint : new String[] {"false", "true", "missing"}) try (var f = new Fixture()) {
            var backend = acceptedBackend(f); var proxy = receiverProxy(false); UUID occurrence = UUID.randomUUID();
            proxy.vote("Alice", "service", true, false, Long.MAX_VALUE, null, f.uuid.toString(), occurrence);
            var initial = proxy.getLastVoteEnvelope(); assertNotNull(initial);
            var transport = com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder(initial.getSubChannel()).schema(initial.getSchema());
            for (var field : initial.getFields().entrySet()) if (!field.getKey().equals(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH)) transport.put(field.getKey(), field.getValue());
            if (!hint.equals("missing")) transport.put(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH, hint);
            // The admitted transport has not reached this backend when the guide opens.
            startGuide(f);
            backend.handleOrderedVote(transport.build(), result -> assertEquals(
                    com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome.COMPLETE, result));
            verify(f.user).setTime(f.site, Long.MAX_VALUE); verify(f.user).playerVote(eq(f.site), anyBoolean(), eq(true));
            verify(f.user).addTotal(); verify(f.user).addPoints();
            var events = org.mockito.ArgumentCaptor.forClass(org.bukkit.event.Event.class);
            verify(f.plugin.getServer().getPluginManager(), times(2)).callEvent(events.capture());
            var credited = (PlayerPostVoteEvent) events.getAllValues().get(1);
            assertEquals(occurrence, credited.getVoteUUID()); assertTrue(credited.isUnconfirmedProxySessionDelivery());
            assertFalse(credited.isQueuedProxyVote()); checkGuide(f, 0);
        }
    }

    @Test void crossNodeReceiptsRemainUnconfirmedWhileLocalProductionReceiptsConfirmTheGuide() throws Exception {
        for (long remoteTime : new long[] { 1L, Long.MAX_VALUE }) try (var f = new Fixture()) {
            var backend = acceptedBackend(f); var proxy = receiverProxy(false); UUID occurrence = UUID.randomUUID();
            startGuide(f);
            proxy.vote("Alice", "service", true, false, remoteTime, null, f.uuid.toString(), occurrence);
            var delivered = proxy.getLastVoteEnvelope(); assertNotNull(delivered);
            assertFalse(delivered.getFields().containsKey(com.bencodez.votingplugin.proxy.VotingPluginWire.K_MULTI_PROXY_ORIGIN));
            backend.handleOrderedVote(delivered, ignored -> { });
            verify(f.user).setTime(f.site, remoteTime); verify(f.user).addPoints();
            checkGuide(f, 0);
        }
        try (var f = new Fixture()) {
            acceptedBackend(f); startGuide(f);
            var event = new com.bencodez.votingplugin.events.PlayerVoteEvent(f.site, "Alice", "service", true);
            event.setVotingPluginUser(f.user);
            f.plugin.getServer().getPluginManager().callEvent(event);
            verify(f.user).setTime(f.site); verify(f.user).playerVote(f.site, true, false);
            verify(f.user).addTotal(); verify(f.user).addPoints(); checkGuide(f, 1);
        }
    }

    @Test void cachedAndReceiverTimedReplayRemainUnconfirmedWithoutChangingQueuePolicy() throws Exception {
        for (boolean validationKnown : new boolean[] { false, true }) {
            for (boolean online : new boolean[] { false, true }) try (var f = new Fixture()) {
                var backend = acceptedBackend(f); var proxy = receiverProxy(false); UUID occurrence = UUID.randomUUID();
                var cached = new com.bencodez.votingplugin.proxy.OfflineBungeeVote(occurrence, "Alice", f.uuid.toString(),
                        "service", 1L, true, "");
                if (validationKnown) cached.setDelayValidated(true);
                var emitter = com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredMethod("cachedVoteEnvelope",
                        com.bencodez.votingplugin.proxy.OfflineBungeeVote.class, boolean.class, boolean.class, int.class, int.class);
                emitter.setAccessible(true); startGuide(f);
                var delivered = (com.bencodez.simpleapi.servercomm.codec.JsonEnvelope) emitter.invoke(proxy, cached, online, false, 1, 1);
                var vote = com.bencodez.votingplugin.proxy.VotingPluginWire.readVote(delivered);
                assertEquals(validationKnown, vote.queuedDelivery); assertTrue(vote.queuedDeliveryKnown);
                assertEquals(occurrence, vote.voteId); assertEquals(1L, vote.time);
                backend.handleOrderedVote(delivered, ignored -> { });
                verify(f.user).playerVote(eq(f.site), anyBoolean(), eq(true)); verify(f.user).addPoints(); checkGuide(f, 0);
            }
        }
        try (var f = new Fixture()) {
            var backend = acceptedBackend(f); var proxy = receiverProxy(false); UUID occurrence = UUID.randomUUID();
            var queued = new com.bencodez.votingplugin.timequeue.VoteTimeQueue(occurrence, "Alice", "service", 1L,
                    false, java.util.Set.of(), java.util.Set.of(), "", false, f.uuid.toString());
            queued.setRealVote(true); queued.setMultiProxyOrigin("Primary");
            startGuide(f); proxy.replay(queued);
            var delivered = proxy.getLastVoteEnvelope(); assertNotNull(delivered);
            assertEquals("Primary", delivered.getFields().get(com.bencodez.votingplugin.proxy.VotingPluginWire.K_MULTI_PROXY_ORIGIN));
            backend.handleOrderedVote(delivered, ignored -> { });
            verify(f.user).setTime(f.site, 1L); verify(f.user).addPoints(); checkGuide(f, 0);
        }
    }

    @Test void forwardedVoteStillObeysBackendWaitUntilVoteDelayRejection() throws Exception {
        try (var f = new Fixture()) {
            var backend = acceptedBackend(f); var proxy = receiverProxy(false); startGuide(f);
            when(f.user.canVoteSite(f.site)).thenReturn(false);
            proxy.receive("Alice", "service", true, false, 1L, null, f.uuid.toString(), UUID.randomUUID(), "Primary", true, true);
            backend.handleOrderedVote(proxy.getLastVoteEnvelope(), ignored -> { });
            verify(f.user, never()).playerVote(any(), anyBoolean(), anyBoolean());
            verify(f.user, never()).addPoints(); checkGuide(f, 0);
        }
    }

    @Test void olderProxyWithoutPositiveFreshnessMetadataRewardsNormallyButCannotConfirmGuide() throws Exception {
        try (var f = new Fixture()) {
            var backend = acceptedBackend(f); startGuide(f);
            var fresh = com.bencodez.votingplugin.proxy.VotingPluginWire.vote("Alice", f.uuid.toString(), "service", 1L,
                    true, true, "", UUID.randomUUID(), false, false, 1, 1);
            var legacy = com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder(fresh.getSubChannel()).schema(fresh.getSchema());
            for (var field : fresh.getFields().entrySet()) if (!field.getKey().equals(com.bencodez.votingplugin.proxy.VotingPluginWire.K_SESSION_DELIVERY_FRESH)) legacy.put(field.getKey(), field.getValue());
            backend.handleOrderedVote(legacy.build(), ignored -> { });
            verify(f.user).playerVote(eq(f.site), anyBoolean(), eq(true)); verify(f.user).addPoints(); checkGuide(f, 0);
        }
    }

    private static void startGuide(Fixture f) {
        f.sessions.command(f.player, ""); f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
    }
    private static void checkGuide(Fixture f, int received) {
        clearInvocations(f.player); f.sessions.command(f.player, "check");
        f.entity.remove().run(); f.worker.remove().run(); f.entity.remove().run();
        verify(f.player).sendMessage("Voting session: " + received + "/1 votes received.");
    }
    private static com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter acceptedBackend(Fixture f) throws Exception {
        when(f.plugin.isEnabled()).thenReturn(true); when(f.plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        when(f.plugin.getConfigFile().isAddTotals()).thenReturn(true);
        when(f.plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(f.plugin.getUserManager().getProperName("Alice")).thenReturn("Alice");
        var validation = mock(com.bencodez.advancedcore.api.user.validation.UserValidationResult.class);
        when(validation.isValid()).thenReturn(true); when(validation.getNormalizedName()).thenReturn("Alice");
        when(validation.getSource()).thenReturn(com.bencodez.advancedcore.api.user.validation.ValidationSource.STORAGE);
        when(f.plugin.getUserManager().getValidationService().validate("Alice", false)).thenReturn(validation);
        when(f.user.isOnline()).thenReturn(true); when(f.user.getUUID()).thenReturn(f.uuid.toString());
        when(f.site.getServiceSite()).thenReturn("service"); when(f.site.isWaitUntilVoteDelay()).thenReturn(true);
        when(f.user.canVoteSite(f.site)).thenReturn(true);
        when(f.plugin.getVoteSiteManager().getVoteSite("service", true)).thenReturn(f.site);
        var pluginField = VotingPluginUser.class.getDeclaredField("plugin"); pluginField.setAccessible(true); pluginField.set(f.user, f.plugin);
        doCallRealMethod().when(f.user).bungeeVotePluginMessaging(anyString(), anyLong(), any(), anyBoolean(), anyBoolean(),
                anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), any(), anyBoolean(), anyBoolean());
        var listener = new com.bencodez.votingplugin.listeners.PlayerVoteListener(f.plugin);
        var pluginManager = f.plugin.getServer().getPluginManager();
        doAnswer(i -> {
            var event = i.getArgument(0);
            if (event instanceof com.bencodez.votingplugin.events.PlayerVoteEvent input) listener.onplayerVote(input);
            else if (event instanceof PlayerPostVoteEvent post) f.sessions.credited(post);
            return null;
        }).when(pluginManager).callEvent(any(org.bukkit.event.Event.class));
        var cache = mock(com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache.class);
        when(cache.reserveWithOutcome(any())).thenReturn(com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache.Reservation.RESERVED);
        when(cache.complete(any())).thenReturn(true);
        return new com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter(f.plugin,
                mock(com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager.class),
                mock(com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync.class),
                mock(com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync.class), cache);
    }
    private static ReceiverProxy receiverProxy(boolean allServers) throws Exception {
        var proxy = spy(new ReceiverProxy());
        var cache = mock(com.bencodez.votingplugin.proxy.cache.VoteCacheHandler.class);
        when(cache.markMultiProxyVoteCompletedDurably(any())).thenReturn(true);
        when(cache.getTimeChangeQueue()).thenReturn(new java.util.concurrent.ConcurrentLinkedQueue<>());
        when(cache.addOnlineVoteDurably(anyString(), any())).thenReturn(true);
        when(cache.addServerVoteDurably(anyString(), any())).thenReturn(true);
        when(cache.updateOnlineVote(anyString(), any())).thenReturn(true);
        when(cache.updateServerVote(anyString(), any())).thenReturn(true);
        when(cache.updateTimeVote(any())).thenReturn(true);
        doReturn(cache).when(proxy).getVoteCacheHandler(); doNothing().when(proxy).addVoteParty();
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
        when(proxy.getConfig().getPrimaryServer()).thenReturn(false);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getSendVotesToAllServers()).thenReturn(allServers);
        proxy.setMethod(com.bencodez.votingplugin.proxy.BungeeMethod.PLUGINMESSAGING);
        proxy.setGlobalMessageProxyHandlerForTest(new com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler() {
            @Override public void sendMessage(String server, int delay, com.bencodez.simpleapi.servercomm.codec.JsonEnvelope envelope) { }
        });
        var legacyField = com.bencodez.votingplugin.proxy.VotingPluginProxy.class.getDeclaredField("legacyVoteDeliveryServers");
        legacyField.setAccessible(true);
        @SuppressWarnings("unchecked") var legacy = (java.util.Set<String>) legacyField.get(proxy);
        legacy.add("server1"); legacy.add("server2");
        return proxy;
    }
    private static final class ReceiverProxy extends com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl {
        void receive(String player, String service, boolean real, boolean timeQueue, long time,
                com.bencodez.votingplugin.proxy.VoteTotalsSnapshot totals, String uuid, UUID id, String origin,
                boolean validated, boolean known) {
            receiveMultiProxyVote(player, service, real, timeQueue, time, totals, uuid, id, origin, validated, known);
        }
        void replay(com.bencodez.votingplugin.timequeue.VoteTimeQueue vote) { replayQueuedVote(vote, null, true); }
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
            when(plugin.getOptions().isOnlineMode()).thenReturn(true);
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
