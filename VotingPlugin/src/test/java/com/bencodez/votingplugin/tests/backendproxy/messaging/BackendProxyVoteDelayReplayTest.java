package com.bencodez.votingplugin.tests.backendproxy.messaging;

import com.bencodez.votingplugin.tests.backendproxy.cache.ProcessedVoteCacheAccess;

import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.user.validation.UserValidationResult;
import com.bencodez.advancedcore.api.user.validation.ValidationSource;
import com.bencodez.advancedcore.api.user.validation.ValidationStatus;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

/** Exercises wire decoding, durable deduplication, event metadata and the real listener together. */
class BackendProxyVoteDelayReplayTest {
    private static final UUID PLAYER_ID = UUID.fromString("d4463458-69f3-4c2f-b8c1-d57d187da516");
    private static final UUID FIRST_VOTE_ID = UUID.fromString("00000000-0000-4000-8000-000000000001");
    private static final UUID SECOND_VOTE_ID = UUID.fromString("00000000-0000-4000-8000-000000000002");
    private static final String SERVICE = "known.example";
    private static final long RECORDED_TIME = 1_788_201_600_000L;

    private final AtomicInteger fixtureSequence = new AtomicInteger();

    @TempDir
    Path directory;

    @TestFactory
    Stream<DynamicTest> modernLiveVoteCannotReuseUpstreamTimestampToBypassBackendDelay() {
        return routes(targeted -> {
            Fixture fixture = fixture();
            JsonEnvelope live = vote(FIRST_VOTE_ID, RECORDED_TIME, targeted, false, true);

            fixture.deliver(live);
            fixture.deliver(live);

            fixture.assertEffects(0);
            verify(fixture.site, times(1)).giveWaitUntilVoteDelayRewards(fixture.user, true, true);
            assertEquals(RECORDED_TIME, fixture.lastVote.get());
        });
    }

    @TestFactory
    Stream<DynamicTest> identifiedQueuedVoteExecutesOnceDespiteNewerUpstreamLastVotesAndRestart() {
        return routes(targeted -> {
            Fixture fixture = fixture();
            UUID voteId = FIRST_VOTE_ID;
            JsonEnvelope queued = vote(voteId, RECORDED_TIME - 60_000L, targeted, true, true);

            fixture.deliver(queued);
            fixture.deliver(queued);
            fixture.restartReceiver();
            fixture.deliver(queued);

            fixture.assertEffects(1);
            fixture.assertOccurrence(0, voteId, RECORDED_TIME - 60_000L);
            verify(fixture.site, never()).giveWaitUntilVoteDelayRewards(any(), anyBoolean(), anyBoolean());
            assertTrue(ProcessedVoteCacheAccess.create(fixture.receipts).hasDurableReceipt(voteId));
        });
    }

    @TestFactory
    Stream<DynamicTest> sameTimestampWithDifferentQueuedIdsRepresentsDistinctAcceptedOccurrences() {
        return routes(targeted -> {
            Fixture fixture = fixture();
            UUID first = FIRST_VOTE_ID;
            UUID second = SECOND_VOTE_ID;
            JsonEnvelope firstVote = vote(first, RECORDED_TIME, targeted, true, true);
            JsonEnvelope secondVote = vote(second, RECORDED_TIME, targeted, true, true);

            fixture.deliver(firstVote);
            fixture.deliver(secondVote);
            fixture.deliver(firstVote);
            fixture.restartReceiver();
            fixture.deliver(secondVote);

            fixture.assertEffects(2);
            fixture.assertOccurrence(0, first, RECORDED_TIME);
            fixture.assertOccurrence(1, second, RECORDED_TIME);
        });
    }

    @TestFactory
    Stream<DynamicTest> changedTimestampDoesNotTurnACompletedIdIntoAnotherVote() {
        return routes(targeted -> {
            Fixture fixture = fixture();
            UUID voteId = FIRST_VOTE_ID;
            fixture.deliver(vote(voteId, RECORDED_TIME, targeted, true, true));
            fixture.deliver(vote(voteId, RECORDED_TIME + 60_000L, targeted, true, true));
            fixture.restartReceiver();
            fixture.deliver(vote(voteId, RECORDED_TIME - 60_000L, targeted, true, true));

            fixture.assertEffects(1);
            fixture.assertOccurrence(0, voteId, RECORDED_TIME);
            assertEquals(RECORDED_TIME, fixture.lastVote.get());
        });
    }

    @TestFactory
    Stream<DynamicTest> outOfOrderQueuedVotesKeepTheirIdsAndDoNotReplayEffects() {
        return routes(targeted -> {
            Fixture fixture = fixture();
            UUID older = FIRST_VOTE_ID;
            UUID newer = SECOND_VOTE_ID;
            JsonEnvelope olderVote = vote(older, RECORDED_TIME - 60_000L, targeted, true, true);
            JsonEnvelope newerVote = vote(newer, RECORDED_TIME + 60_000L, targeted, true, true);

            fixture.deliver(newerVote);
            fixture.deliver(olderVote);
            fixture.deliver(newerVote);
            fixture.restartReceiver();
            fixture.deliver(olderVote);

            fixture.assertEffects(2);
            fixture.assertOccurrence(0, newer, RECORDED_TIME + 60_000L);
            fixture.assertOccurrence(1, older, RECORDED_TIME - 60_000L);
        });
    }

    @Test
    void legacyUnclassifiedBacklogKeepsTimestampFallbackAndDurableIdentity() throws Exception {
        Fixture fixture = fixture();
        UUID voteId = FIRST_VOTE_ID;
        JsonEnvelope legacy = VotingPluginWire.legacyUnclassifiedDelivery(
                vote(voteId, RECORDED_TIME, true, false, false));

        fixture.deliver(legacy);
        fixture.restartReceiver();
        fixture.deliver(legacy);

        fixture.assertEffects(1);
        fixture.assertOccurrence(0, voteId, RECORDED_TIME);
        verify(fixture.site, never()).giveWaitUntilVoteDelayRewards(any(), anyBoolean(), anyBoolean());
    }

    @Test
    void missingQueuedIdentityStillChecksDelayWhileLegacyMissingIdentityRemainsCompatible() throws Exception {
        Fixture fixture = fixture();
        // Missing IDs retain legacy delivery semantics; do not request reliable acknowledgement.
        JsonEnvelope current = VotingPluginWire.queuedDelivery(VotingPluginWire.voteOnline("Player",
                PLAYER_ID.toString(), SERVICE, RECORDED_TIME, true, true, "", null, false, false, 1, 1, true));
        fixture.deliver(current);
        fixture.assertEffects(0);
        verify(fixture.site).giveWaitUntilVoteDelayRewards(fixture.user, true, true);

        JsonEnvelope legacy = VotingPluginWire.legacyUnclassifiedDelivery(VotingPluginWire.voteOnline("Player",
                PLAYER_ID.toString(), SERVICE, RECORDED_TIME, true, true, "", null, false, false, 1, 1));
        fixture.deliver(legacy);
        fixture.assertEffects(1);
    }

    private Fixture fixture() throws Exception {
        return new Fixture(directory.resolve("fixture-" + fixtureSequence.incrementAndGet() + ".receipts"));
    }

    private Stream<DynamicTest> routes(RouteTest test) {
        return Stream.of(false, true).map(targeted -> DynamicTest.dynamicTest(
                targeted ? "VoteOnline selected backend" : "Vote all backends", () -> test.run(targeted)));
    }

    @FunctionalInterface
    private interface RouteTest {
        void run(boolean targeted) throws Exception;
    }

    private static JsonEnvelope vote(UUID voteId, long time, boolean targeted, boolean queued, boolean validated) {
        JsonEnvelope envelope;
        if (validated) {
            envelope = targeted
                    ? VotingPluginWire.voteOnline("Player", PLAYER_ID.toString(), SERVICE, time,
                            true, true, "", voteId, false, false, 1, 1, true)
                    : VotingPluginWire.vote("Player", PLAYER_ID.toString(), SERVICE, time,
                            true, true, "", voteId, false, false, 1, 1, true);
        } else {
            envelope = targeted
                    ? VotingPluginWire.voteOnline("Player", PLAYER_ID.toString(), SERVICE, time,
                            true, true, "", voteId, false, false, 1, 1)
                    : VotingPluginWire.vote("Player", PLAYER_ID.toString(), SERVICE, time,
                            true, true, "", voteId, false, false, 1, 1);
        }
        if (queued) envelope = VotingPluginWire.queuedDelivery(envelope);
        return VotingPluginWire.requestVoteDeliveryAcknowledgement(envelope);
    }

    private static final class Fixture {
        private final Path receipts;
        private final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        private final VotingPluginUser user = mock(VotingPluginUser.class);
        private final VoteSite site = mock(VoteSite.class);
        private final AtomicLong lastVote = new AtomicLong(RECORDED_TIME);
        private final AtomicBoolean canVote = new AtomicBoolean(false);
        private final List<PlayerPostVoteEvent> completed = new ArrayList<>();
        private BackendProxyMessageRouter router;

        private Fixture(Path receipts) throws Exception {
            this.receipts = receipts;
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getOptions().isProcessRewards()).thenReturn(true);
            when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
            when(plugin.getConfigFile().isAddTotals()).thenReturn(true);
            when(plugin.getUserManager().getProperName("Player")).thenReturn("Player");
            when(plugin.getUserManager().getValidationService().validate("Player", false)).thenReturn(
                    new UserValidationResult(ValidationStatus.VALID, "Player", ValidationSource.STORAGE, "ok", false));
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_ID, "Player")).thenReturn(user);
            when(plugin.getVoteSiteManager().getVoteSite(SERVICE, true)).thenReturn(site);
            when(site.isEnabled()).thenReturn(true);
            when(site.isWaitUntilVoteDelay()).thenReturn(true);
            when(site.getServiceSite()).thenReturn(SERVICE);
            when(site.getKey()).thenReturn("Example");
            when(user.isOnline()).thenReturn(true);
            when(user.getPlayerName()).thenReturn("Player");
            when(user.getUUID()).thenReturn(PLAYER_ID.toString());
            when(user.getJavaUUID()).thenReturn(PLAYER_ID);
            when(user.getTime(site)).thenAnswer(ignored -> lastVote.get());
            when(user.canVoteSite(site)).thenAnswer(ignored -> canVote.get());
            doAnswer(call -> {
                lastVote.set(call.getArgument(1));
                canVote.set(false);
                return null;
            }).when(user).setTime(eq(site), anyLong());

            // Run the cancellation-aware production event adapter used by the router.
            var pluginField = VotingPluginUser.class.getDeclaredField("plugin");
            pluginField.setAccessible(true);
            pluginField.set(user, plugin);
            doCallRealMethod().when(user).bungeeVotePluginMessagingEvent(any(), anyLong(), any(VoteTotalsSnapshot.class),
                    anyBoolean(), anyBoolean(), anyBoolean(), anyInt(), anyBoolean(), anyBoolean(),
                    anyBoolean(), any(), anyBoolean(), anyBoolean(), anyLong());
            when(plugin.getDateVoteMilestones()).thenReturn(
                    new com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones(plugin));
            PlayerVoteListener listener = new PlayerVoteListener(plugin);
            var eventManager = plugin.getServer().getPluginManager();
            doAnswer(call -> {
                Object event = call.getArgument(0);
                if (event instanceof PlayerVoteEvent vote) listener.onplayerVote(vote);
                if (event instanceof PlayerPostVoteEvent post) completed.add(post);
                return null;
            }).when(eventManager).callEvent(any());
            restartReceiver();
        }

        private void restartReceiver() {
            router = new BackendProxyMessageRouter(plugin, mock(BackendPresenceManager.class),
                    mock(BackendGlobalDataSync.class), mock(BackendVotePartySync.class),
                    ProcessedVoteCacheAccess.create(receipts));
        }

        private void deliver(JsonEnvelope envelope) {
            AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
            router.handleOrderedVote(envelope, outcome::set);
            assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
        }

        private void assertOccurrence(int index, UUID voteId, long time) {
            PlayerPostVoteEvent post = completed.get(index);
            assertEquals(voteId, post.getVoteUUID());
            assertEquals(time, post.getVoteTime());
            verify(plugin.getVoteStreakHandler()).processVote(user, time, voteId);
        }

        private void assertEffects(int count) {
            assertEquals(count, completed.size());
            verify(user, times(count)).setTime(eq(site), anyLong());
            verify(user, times(count)).playerVote(site, true, true);
            verify(user, times(count)).addTotal();
            verify(user, times(count)).addTotalDaily();
            verify(user, times(count)).addTotalWeekly();
            verify(user, times(count)).addPoints();
            verify(user, never()).addPoints(anyInt());
            verify(user, times(count)).checkDayVoteStreak(true);
            verify(plugin.getVoteParty(), times(count)).vote(user, true, true, true);
            verify(plugin.getCoolDownCheck(), times(count)).vote(user, site);
            verify(plugin.getVoteStreakHandler(), times(count)).processVote(eq(user), anyLong(), any(UUID.class));
            verify(plugin.getVoteMilestonesManager(), times(count))
                    .handleVote(eq(user), any(VoteTotalsSnapshot.class), eq(true), any(UUID.class), any());
            verify(user, never()).addOfflineVote(any());
        }
    }
}
