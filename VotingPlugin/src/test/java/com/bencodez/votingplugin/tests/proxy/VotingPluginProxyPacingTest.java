package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteJournal;
import com.bencodez.votingplugin.tests.proxy.PluginMessageDispatcherAccess;
import com.bencodez.votingplugin.tests.proxy.ReliableVoteDeliveryOutboxAccess;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;

class VotingPluginProxyPacingTest {
    private static final String BACKEND = "backend-a";
    private static final long INTERVAL = TimeUnit.MILLISECONDS.toNanos(20);

    @Test
    void immediateBroadcastAndScheduledControlsShareTheActualHandoffBudget(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        assertTrue(proxy.sendPluginMessageImmediately(BACKEND, control("first")));
        proxy.sendPluginMessageServer(BACKEND, 1, control("scheduled"));
        assertFalse(proxy.sendProxyBroadcastImmediately(BACKEND, control("too-soon")));
        proxy.time.advance(TimeUnit.MILLISECONDS.toNanos(19));
        assertEquals(1, proxy.handoffs.size());
        proxy.time.advance(TimeUnit.MILLISECONDS.toNanos(81));
        assertEquals(List.of("first", "scheduled"), proxy.handoffs.stream()
                .map(handoff -> handoff.envelope().getFields().get("value")).toList());
        assertPaced(proxy.handoffs);
    }

    @Test
    void schedulerStallCannotReleaseSeveralOverdueControlsAtOnce(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        for (int i = 0; i < 3; i++) proxy.sendPluginMessageServer(BACKEND, 1, control("same"));
        proxy.time.stall(TimeUnit.SECONDS.toNanos(5));
        assertEquals(1, proxy.handoffs.size());
        proxy.time.advance(INTERVAL - 1);
        assertEquals(1, proxy.handoffs.size());
        proxy.time.advance(1);
        assertEquals(2, proxy.handoffs.size());
        proxy.time.advance(INTERVAL);
        assertEquals(3, proxy.handoffs.size());
        assertPaced(proxy.handoffs);
    }

    @Test
    void rejectedLegacyCacheHandoffRetainsTheOriginalVoteAcrossRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().add(BACKEND);
        OfflineBungeeVote cached = cached(1);
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, cached));
        proxy.deliveryAccepted = false;
        proxy.checkCachedVotes(BACKEND);
        assertFalse(cached.isRewardDelivered());
        assertEquals(List.of(cached), proxy.cache.getVotes(BACKEND));
        assertTrue(proxy.handoffs.isEmpty());
        assertEquals(1, proxy.transportAttempts);
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.legacy().add(BACKEND);
        restarted.cache.load();
        assertEquals(cached.getVoteId(), restarted.cache.getVotes(BACKEND).get(0).getVoteId());
        restarted.checkCachedVotes(BACKEND);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, restarted.handoffs.size());
        assertEquals(cached.getVoteId().toString(), restarted.handoffs.get(0).envelope()
                .getFields().get(VotingPluginWire.K_VOTE_ID));
        assertTrue(restarted.cache.getVotes(BACKEND).isEmpty());
    }

    @Test
    void serverReplayBatchExcludesMalformedAndAlreadyDeliveredRows(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().add(BACKEND);
        OfflineBungeeVote malformed = cached(800);
        OfflineBungeeVote delivered = cached(801);
        delivered.setRewardDelivered(true);
        OfflineBungeeVote fresh = cached(802);
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, malformed));
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, delivered));
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, fresh));
        malformed.setCachedVoteBatch(2, 1);
        assertFalse(malformed.isCachedVoteBatchValid());

        proxy.checkCachedVotes(BACKEND);
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));

        assertEquals(1, proxy.handoffs.size());
        assertSingleVoteBatch(proxy.handoffs.get(0).envelope(), VotingPluginWire.SUB_VOTE, fresh.getVoteId());
        assertEquals(List.of(malformed), proxy.cache.getVotes(BACKEND));
        assertFalse(malformed.isRewardDelivered());
    }

    @Test
    void offlinePlayerDoesNotConsumeFirstEligibleBatchPosition(@TempDir Path directory) throws Exception {
        verifyRoutingEligibilityBeforeBatchAssignment(directory, false);
    }

    @Test
    void playerOnAnotherBackendDoesNotConsumeFirstEligibleBatchPosition(@TempDir Path directory) throws Exception {
        verifyRoutingEligibilityBeforeBatchAssignment(directory, true);
    }

    private static void verifyRoutingEligibilityBeforeBatchAssignment(Path directory, boolean otherBackend) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().add(BACKEND);
        when(proxy.getConfig().getWaitForUserOnline()).thenReturn(true);
        OfflineBungeeVote waiting = new OfflineBungeeVote(new UUID(3, 1), "test-player-a",
                new UUID(4, 1).toString(), "synthetic-service", 1L, true, "");
        OfflineBungeeVote ready = new OfflineBungeeVote(new UUID(3, 2), "test-player-b",
                new UUID(4, 2).toString(), "synthetic-service", 2L, true, "");
        if (otherBackend) proxy.playerRoutes.put(waiting.getPlayerName(), "backend-b");
        else proxy.offlinePlayers.add(waiting.getPlayerName());
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, waiting));
        assertTrue(proxy.cache.addServerVoteDurably(BACKEND, ready));
        proxy.checkCachedVotes(BACKEND);
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.size());
        assertSingleVoteBatch(proxy.handoffs.get(0).envelope(), VotingPluginWire.SUB_VOTE, ready.getVoteId());
        assertFalse(waiting.hasCachedVoteBatch(), "ineligible row received a permanent batch position");
        assertEquals(List.of(waiting), proxy.cache.getVotes(BACKEND));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.legacy().add(BACKEND);
        when(restarted.getConfig().getWaitForUserOnline()).thenReturn(true);
        restarted.cache.load();
        restarted.checkCachedVotes(BACKEND);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, restarted.handoffs.size());
        assertSingleVoteBatch(restarted.handoffs.get(0).envelope(), VotingPluginWire.SUB_VOTE, waiting.getVoteId());
        assertTrue(restarted.cache.getVotes(BACKEND).isEmpty());
    }

    @Test
    void onlineReplayBatchExcludesMalformedAndAlreadyDeliveredRows(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().add(BACKEND);
        OfflineBungeeVote malformed = cached(803);
        String player = malformed.getUuid();
        OfflineBungeeVote delivered = new OfflineBungeeVote(new UUID(2, 804), "Synthetic", player,
                "synthetic-service", 1L, true, "");
        delivered.setRewardDelivered(true);
        OfflineBungeeVote fresh = new OfflineBungeeVote(new UUID(2, 805), "Synthetic", player,
                "synthetic-service", 1L, true, "");
        assertTrue(proxy.cache.addOnlineVoteDurably(player, malformed));
        assertTrue(proxy.cache.addOnlineVoteDurably(player, delivered));
        assertTrue(proxy.cache.addOnlineVoteDurably(player, fresh));
        malformed.setCachedVoteBatch(2, 1);
        assertFalse(malformed.isCachedVoteBatchValid());

        proxy.checkOnlineVotes("Synthetic", player, BACKEND);
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));

        assertEquals(1, proxy.handoffs.size());
        assertSingleVoteBatch(proxy.handoffs.get(0).envelope(), VotingPluginWire.SUB_VOTE_ONLINE, fresh.getVoteId());
        assertEquals(List.of(malformed), proxy.cache.getOnlineVotes(player));
        assertFalse(malformed.isRewardDelivered());
    }

    private static void assertSingleVoteBatch(JsonEnvelope envelope, String subChannel, UUID voteId) {
        assertEquals(subChannel, envelope.getSubChannel());
        assertEquals(voteId.toString(), envelope.getFields().get(VotingPluginWire.K_VOTE_ID));
        assertEquals("1", envelope.getFields().get(VotingPluginWire.K_NUM));
        assertEquals("1", envelope.getFields().get(VotingPluginWire.K_NUMBER_OF_VOTES));
    }

    @Test
    void reentrantCompletionAndReleaseAcknowledgementsCannotReenterTransport(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        UUID id = new UUID(2, 2);
        proxy.onHandoff = envelope -> {
            if (VotingPluginWire.SUB_VOTE.equals(envelope.getSubChannel())) {
                invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                        VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE));
            } else if (VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(envelope.getSubChannel())) {
                invoke(proxy, "handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class,
                        VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(BACKEND, id,
                                VotingPluginWire.SUB_VOTE));
            }
        };
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(2)));
        assertEquals(1, proxy.handoffs.size());
        assertTrue(proxy.outbox.find(BACKEND, id, VotingPluginWire.SUB_VOTE).orElseThrow().awaitingReceiptRelease());
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(VotingPluginWire.SUB_VOTE, VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE),
                proxy.handoffs.stream().map(handoff -> handoff.envelope().getSubChannel()).toList());
        assertEquals(1, proxy.maxTransportDepth);
        assertEquals(0, proxy.outbox.size());
        assertEquals(0, new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).size());
        assertPaced(proxy.handoffs);
    }

    @Test
    void fullReliableWindowAllowsReceiptsAndReleasesCapacityAfterDurableCompletion(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i <= PluginMessageDispatcherAccess.MAX_IN_FLIGHT; i++)
            assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(100 + i)));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT + 1, proxy.outbox.size());

        UUID completed = new UUID(2, 100);
        invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, completed, VotingPluginWire.SUB_VOTE));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT + 1, rewardHandoffs(proxy).size());
        assertTrue(proxy.handoffs.stream().anyMatch(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                .equals(handoff.envelope().getSubChannel())));
        assertTrue(proxy.outbox.find(BACKEND, completed, VotingPluginWire.SUB_VOTE).orElseThrow().awaitingReceiptRelease());
        assertPaced(proxy.handoffs);
    }

    @Test
    void outstandingReceiptsDoNotConsumeTheRewardWindow(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_IN_FLIGHT + 1; i++) {
            assertTrue(proxy.outbox.offer(BACKEND, vote(200 + i)));
            assertTrue(proxy.outbox.acknowledgeCompletion(BACKEND, new UUID(2, 200 + i), VotingPluginWire.SUB_VOTE));
        }
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(300)));
        proxy.retry();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertTrue(rewardHandoffs(proxy).stream().anyMatch(handoff -> new UUID(2, 300).toString()
                .equals(handoff.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))));
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT + 1, proxy.handoffs.stream()
                .filter(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                        .equals(handoff.envelope().getSubChannel())).count());
        assertPaced(proxy.handoffs);
    }

    @Test
    void durableLegacyRetirementReleasesTheFullWindowBeforeReliableDeliveryReturns(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_IN_FLIGHT; i++)
            assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(600 + i)));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());
        assertEquals(PluginMessageDispatcherAccess.MAX_IN_FLIGHT, proxy.outbox.size());

        proxy.reliable().remove(BACKEND);
        proxy.legacy().add(BACKEND);
        proxy.retry();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(0, proxy.outbox.size());
        assertEquals(0, new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).size());
        assertEquals(2 * PluginMessageDispatcherAccess.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());

        proxy.legacy().remove(BACKEND);
        proxy.reliable().add(BACKEND);
        int before = proxy.handoffs.size();
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(700)));
        assertEquals(before + 1, proxy.handoffs.size(), "durably retired attempts still occupy the reward window");
        assertEquals(new UUID(2, 700).toString(), proxy.handoffs.get(before).envelope()
                .getFields().get(VotingPluginWire.K_VOTE_ID));
        assertPaced(proxy.handoffs);
    }

    @Test
    void durableLegacyRetirementReleasesAReceiptAttemptBeforeTheSameVoteIsReoffered(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        UUID id = new UUID(2, 701);
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(701)));
        invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE));
        proxy.time.advance(2 * INTERVAL);
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                .equals(handoff.envelope().getSubChannel())).count());

        proxy.reliable().remove(BACKEND);
        proxy.legacy().add(BACKEND);
        proxy.retry();
        assertEquals(0, proxy.outbox.size());
        assertEquals(0, new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).size());

        proxy.legacy().remove(BACKEND);
        proxy.reliable().add(BACKEND);
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(701)));
        invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(2, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                .equals(handoff.envelope().getSubChannel())).count(), "retired receipt kept its old retry deadline");
        assertEquals(2, rewardHandoffs(proxy).size());
        assertPaced(proxy.handoffs);
    }

    @Test
    void staleScheduledCallbacksCannotSendAfterRuntimeShutdown(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.sendPluginMessageServer(BACKEND, 1, control("pending"));
        List<Runnable> stale = proxy.time.callbacks();
        assertFalse(stale.isEmpty());
        proxy.completeRuntimeReplacementShutdown();
        stale.forEach(Runnable::run); // Simulate callbacks already selected before cancellation.
        proxy.time.advance(TimeUnit.SECONDS.toNanos(30));
        assertTrue(proxy.handoffs.isEmpty());
        assertFalse(proxy.sendPluginMessageImmediately(BACKEND, control("closed")));
        assertThrows(RejectedExecutionException.class,
                () -> proxy.sendPluginMessageServer(BACKEND, 0, control("closed")));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(proxy.handoffs.isEmpty());
    }

    @Test
    void replacementWaitsForAdmittedControlsAndThenFencesNewAdmission(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.sendPluginMessageServer(BACKEND, 1, control("pending replacement"));
        IllegalStateException pending = assertThrows(IllegalStateException.class, proxy::prepareForRuntimeReplacement);
        assertTrue(pending.getMessage().contains("Queued plugin-message controls"));
        assertTrue(proxy.handoffs.isEmpty());

        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.size());
        assertDoesNotThrow(proxy::prepareForRuntimeReplacement);
        assertFalse((Boolean) field(proxy, "pluginMessageStopped"), "preparation stopped the active transport");
        assertThrows(RejectedExecutionException.class,
                () -> proxy.sendPluginMessageServer(BACKEND, 1, control("late replacement")));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.size());
    }

    @Test
    void unavailableBootstrapMetadataDoesNotBlockRuntimeReplacement(@TempDir Path directory) throws Exception {
        for (BungeeMethod method : List.of(BungeeMethod.PLUGINMESSAGING, BungeeMethod.REDIS)) {
            CapturingProxy proxy = new CapturingProxy(directory.resolve(method.name()));
            proxy.setMethod(method);
            proxy.carrier = false;
            proxy.sendServerNameMessage();
            proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
            assertTrue(proxy.transportAttempts > 0);
            assertTrue(proxy.handoffs.isEmpty());
            assertDoesNotThrow(proxy::prepareForRuntimeReplacement);
            proxy.completeRuntimeReplacementShutdown();
            proxy.carrier = true;
            proxy.time.advance(TimeUnit.SECONDS.toNanos(30));
            assertTrue(proxy.handoffs.isEmpty(), "old metadata callback survived replacement");
        }
    }

    @Test
    void effectBearingControlsStillFenceReplacementWithoutCarrier(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.carrier = false;
        proxy.sendServerNameMessage();
        proxy.sendPluginMessageServer(BACKEND, 1, VotingPluginWire.votePartyBungee());
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertThrows(IllegalStateException.class, proxy::prepareForRuntimeReplacement);
        proxy.carrier = true;
        proxy.time.advance(TimeUnit.SECONDS.toNanos(3));
        assertDoesNotThrow(proxy::prepareForRuntimeReplacement);
    }

    @Test
    void queuedPluginVotePartySurvivesSchedulerCancellationAndShutdown(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.persistPartyState = true;
        when(proxy.getConfig().getVotePartyEnabled()).thenReturn(true);
        when(proxy.getConfig().getVotePartySendToAllServers()).thenReturn(true);
        when(proxy.getConfig().getVotePartyBroadcast()).thenReturn("");
        when(proxy.getConfig().getVotePartyBungeeCommands()).thenReturn(List.of());
        proxy.setVotePartyVotes(1);
        proxy.setCurrentVotePartyVotesRequired(1);
        proxy.checkVoteParty();
        assertEquals(0, proxy.storage.getVotePartyCurrentVotes());
        assertTrue(proxy.handoffs.isEmpty(), "party handoff must still be paced");
        proxy.time.executor.shutdownNow();
        proxy.completeRuntimeReplacementShutdown();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.persistPartyState = true;
        restarted.cache.load();
        restarted.setVotePartyVotes(restarted.storage.getVotePartyCurrentVotes());
        restarted.setCurrentVotePartyVotesRequired(1);
        assertEquals(0, restarted.getVotePartyVotes(), "the committed threshold must not be regenerated");
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertEquals(2, restarted.handoffs.stream().filter(h ->
                VotingPluginWire.votePartyBungee().getSubChannel().equals(h.envelope().getSubChannel())).count(),
                "graceful shutdown lost committed backend party rewards");
        restarted.closeDispatcher();
    }

    private static CapturingProxy durablePartyProxy(Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.persistPartyState = true;
        when(proxy.getConfig().getVotePartyEnabled()).thenReturn(true);
        when(proxy.getConfig().getVotePartySendToAllServers()).thenReturn(true);
        when(proxy.getConfig().getVotePartyBroadcast()).thenReturn("");
        when(proxy.getConfig().getVotePartyBungeeCommands()).thenReturn(List.of());
        proxy.setVotePartyVotes(1);
        proxy.setCurrentVotePartyVotesRequired(1);
        return proxy;
    }

    @Test
    void partyDeliveryPreservesExactConfiguredBackendName(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        String configured = " Backend-a ";
        proxy.availableTargets.clear();
        proxy.availableTargets.add(configured);
        proxy.unavailableBackends.add("backend-a");
        proxy.checkVoteParty();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertEquals(1, proxy.handoffs.size(), "exact online backend was replaced with an unavailable alias");
        assertEquals(configured, proxy.handoffs.getFirst().server());
        assertTrue(proxy.cache.getPluginMessageVotePartyRewards().deliveries().isEmpty());
        proxy.closeDispatcher();
    }

    @Test
    void rejectedPartyHandoffsKeepOriginalIdsAndRetryWithoutHttpOwnership(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        proxy.deliveryAccepted = false;
        proxy.checkVoteParty();
        var original = proxy.cache.getPluginMessageVotePartyRewards();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertEquals(original, proxy.cache.getPluginMessageVotePartyRewards());
        assertTrue(proxy.storage.getPendingVotePartyRewardServers().isEmpty());
        proxy.deliveryAccepted = true;
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(2, proxy.handoffs.size());
        assertTrue(proxy.cache.getPluginMessageVotePartyRewards().deliveries().isEmpty());
        assertEquals(0, proxy.getVotePartyVotes());
        assertPaced(proxy.handoffs);
        proxy.closeDispatcher();
    }

    @Test
    void partyFencePersistenceFailureDoesNotAttemptTransport(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        proxy.checkVoteParty();
        var original = proxy.cache.getPluginMessageVotePartyRewards();
        proxy.failDurablePartySave = true;
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertEquals(0, proxy.transportAttempts);
        assertEquals(original, proxy.cache.getPluginMessageVotePartyRewards());
        proxy.failDurablePartySave = false;
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(2, proxy.handoffs.size());
        proxy.closeDispatcher();
    }

    @Test
    void acceptedPartyHandoffWithFailedCompletionSaveNeverRepeats(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        proxy.availableTargets.clear();
        proxy.availableTargets.add(BACKEND);
        proxy.onHandoff = envelope -> proxy.failDurablePartySave = true;
        proxy.checkVoteParty();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(12));
        assertEquals(1, proxy.handoffs.size());
        assertEquals(com.bencodez.votingplugin.proxy.cache.PendingPluginMessageVotePartyRewards.State.HANDOFF_FENCED,
                proxy.cache.getPluginMessageVotePartyRewards().deliveries().getFirst().state());
        proxy.failDurablePartySave = false;
        invoke(proxy, "retryReliableVoteDeliveries", String.class, null);
        assertTrue(proxy.cache.getPluginMessageVotePartyRewards().deliveries().isEmpty());
        assertEquals(1, proxy.handoffs.size());
        proxy.closeDispatcher();
    }

    @Test
    void ambiguousPartyHandoffStaysFencedAcrossRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        proxy.availableTargets.clear();
        proxy.availableTargets.add(BACKEND);
        proxy.onHandoff = envelope -> { throw new IllegalStateException("synthetic ambiguous transport outcome"); };
        proxy.checkVoteParty();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(1, proxy.handoffs.size());
        var fenced = proxy.cache.getPluginMessageVotePartyRewards();
        proxy.completeRuntimeReplacementShutdown();
        CapturingProxy restarted = durablePartyProxy(directory);
        restarted.cache.load();
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertTrue(restarted.handoffs.isEmpty());
        assertEquals(fenced, restarted.cache.getPluginMessageVotePartyRewards());
        restarted.closeDispatcher();
    }

    @Test
    void partialPartyRestartReplaysOnlyUnattemptedBackend(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = durablePartyProxy(directory);
        proxy.checkVoteParty();
        proxy.unavailableBackends.add("backend-b");
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        assertEquals(1, proxy.handoffs.size());
        var pending = proxy.cache.getPluginMessageVotePartyRewards();
        assertEquals("backend-b", pending.deliveries().getFirst().server());
        proxy.time.executor.shutdownNow();
        proxy.completeRuntimeReplacementShutdown();
        CapturingProxy restarted = durablePartyProxy(directory);
        restarted.cache.load();
        assertEquals(pending, restarted.cache.getPluginMessageVotePartyRewards());
        restarted.setMethod(BungeeMethod.REDIS);
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        assertTrue(restarted.handoffs.isEmpty());
        restarted.setMethod(BungeeMethod.PLUGINMESSAGING);
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        assertEquals(1, restarted.handoffs.size());
        assertEquals("backend-b", restarted.handoffs.getFirst().server());
        restarted.closeDispatcher();
    }

    @Test
    void pluginVotePartyFailedDurableSaveReleasesBatchAndRetainsThreshold(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        when(proxy.getConfig().getVotePartyEnabled()).thenReturn(true);
        when(proxy.getConfig().getVotePartySendToAllServers()).thenReturn(true);
        when(proxy.getConfig().getVotePartyBroadcast()).thenReturn("");
        when(proxy.getConfig().getVotePartyBungeeCommands()).thenReturn(List.of());
        proxy.setVotePartyVotes(1);
        proxy.setCurrentVotePartyVotesRequired(1);
        proxy.failDurablePartySave = true;
        assertThrows(IllegalStateException.class, proxy::checkVoteParty);
        assertEquals(1, proxy.getVotePartyVotes());
        assertEquals(1, proxy.getCurrentVotePartyVotesRequired());
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(proxy.handoffs.isEmpty());
        proxy.failDurablePartySave = false;
        proxy.checkVoteParty();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(0, proxy.getVotePartyVotes());
        assertEquals(2, proxy.handoffs.size());
        proxy.checkVoteParty();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(2, proxy.handoffs.size(), "the completed threshold was replayed");
    }

    @Test
    void saturatedControlLaneDoesNotAbortProbesForAnotherBackend(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_LANE_CONTROLS; i++)
            proxy.sendPluginMessageServer(BACKEND, 0, control("occupied"));

        assertDoesNotThrow(proxy::sendServerNameMessage);
        assertDoesNotThrow(proxy::sendServerNameMessage);
        assertEquals(1, proxy.getWarnings().size(), "backpressure diagnostics were not bounded");
        proxy.time.advance(TimeUnit.SECONDS.toNanos(6));
        assertEquals(2, proxy.handoffs.stream().filter(handoff -> "backend-b".equals(handoff.server())
                && VotingPluginWire.SUB_SERVER_NAME.equals(handoff.envelope().getSubChannel())).count());
        assertPaced(proxy.handoffs);
    }

    @Test
    void schedulerRejectionDoesNotThrowFromTheBestEffortControlApi(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        when(proxy.time.executor.schedule(any(Runnable.class), anyLong(), eq(TimeUnit.NANOSECONDS)))
                .thenThrow(new RejectedExecutionException("synthetic scheduler rejection"));
        assertDoesNotThrow(proxy::sendServerNameMessage);
        assertDoesNotThrow(proxy::sendServerNameMessage);
        assertTrue(proxy.handoffs.isEmpty());
        assertEquals(1, proxy.getWarnings().size());
    }

    @Test
    void bootstrapServerMetadataStillDrainsWhenAnotherTransportIsSelected(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.setMethod(BungeeMethod.REDIS);
        proxy.sendServerNameMessage();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));

        assertEquals(2, proxy.handoffs.size());
        assertEquals(List.of(BACKEND, "backend-b"), proxy.handoffs.stream().map(Handoff::server).sorted().toList());
        for (Handoff handoff : proxy.handoffs) {
            assertEquals(VotingPluginWire.SUB_SERVER_NAME, handoff.envelope().getSubChannel());
            assertEquals(handoff.server(), handoff.envelope().getFields().get(VotingPluginWire.K_SERVER));
        }
        assertPaced(proxy.handoffs);
    }

    @Test
    void backendReplayCannotConsumeAnotherBackendsOnlineReward(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().add(BACKEND);
        proxy.legacy().add("backend-b");
        proxy.playerServer = "backend-b";
        OfflineBungeeVote cached = cached(400);
        assertTrue(proxy.cache.addOnlineVoteDurably(cached.getUuid(), cached));
        assertTrue(proxy.sendPluginMessageImmediately(BACKEND, control("wake")));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(proxy.handoffs.stream().noneMatch(handoff -> VotingPluginWire.SUB_VOTE
                .equals(handoff.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE_ONLINE
                        .equals(handoff.envelope().getSubChannel())));
        assertFalse(cached.isRewardDelivered());
        proxy.checkOnlineVotes(cached.getPlayerName(), cached.getUuid(), "backend-b");
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel()) && "backend-b".equals(handoff.server())).count());
    }

    @Test
    void rejectedLiveBroadcastRemainsPendingWithoutRepeatingTheAcceptedReward(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.broadcastValidationEnabled = true;
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getProxyBroadcastEnabled()).thenReturn(true);
        when(proxy.getConfig().getProxyBroadcastScopeMode()).thenReturn("ALL_SERVERS");
        setField(proxy, "proxyBroadcastDecider", new com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider(
                proxy::getConfig, proxy::getAllAvailableServers, proxy::isServerValid, ignored -> false));
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_LANE_CONTROLS; i++)
            proxy.sendPluginMessageServer("backend-b", 0, control("occupied"));
        UUID id = new UUID(2, 503);
        String player = new UUID(1, 503).toString();

        assertDoesNotThrow(
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        assertTrue(proxy.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel())));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(6));
        proxy.playerServer = "backend-b";
        proxy.retryPendingOnlineBroadcasts();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel()) && "backend-b".equals(h.server())).count());
        assertEquals(1, proxy.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())).count(), "broadcast retry repeated the accepted reward");
        assertPaced(proxy.handoffs);
    }

    private static void enableLiveBroadcasts(CapturingProxy proxy) throws Exception {
        proxy.broadcastValidationEnabled = true;
        proxy.legacy().addAll(proxy.availableTargets);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getProxyBroadcastEnabled()).thenReturn(true);
        when(proxy.getConfig().getProxyBroadcastScopeMode()).thenReturn("ALL_SERVERS");
        setField(proxy, "proxyBroadcastDecider", new com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider(
                proxy::getConfig, proxy::getAllAvailableServers, proxy::isServerValid, ignored -> false));
    }

    @Test
    void unavailableBroadcastTargetDoesNotBlockAnotherBackendOrVoteUpdates(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.availableTargets.add("backend-c");
        proxy.setAvailableServers(BACKEND, "backend-b", "backend-c");
        proxy.unavailableBackends.add("backend-b");
        enableLiveBroadcasts(proxy);
        UUID id = new UUID(2, 504);
        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false,
                1L, null, new UUID(1, 504).toString(), id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, proxy.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel()) && "backend-c".equals(h.server())).count());
        assertTrue(updateHandoffs(proxy).stream().anyMatch(h -> "backend-c".equals(h.server())));
        assertTrue(proxy.cache.getOnlineVotes(new UUID(1, 504).toString()).stream()
                .anyMatch(v -> v.isRewardDelivered() && v.needsBroadcastOn("backend-b")));
    }

    @Test
    void restartPreservesCompletedRewardAndItsOriginalSingleServer(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        proxy.unavailableBackends.add("backend-b");
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = new UUID(2, 505);
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        String player = new UUID(1, 505).toString();
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        com.bencodez.votingplugin.timequeue.VoteTimeQueue snapshot = proxy.snapshotIncomingVoteForRestart(
                new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L));
        assertFalse(snapshot.hasPendingHttpBroadcastDeliveryIds(), "completion marker became a transport owner");
        PendingIncomingVoteJournal emergency = PendingIncomingVoteJournal.rescue(directory);
        emergency.merge(List.of(snapshot));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        restarted.playerServer = "backend-b";
        restarted.cache.load();
        assertTrue(restarted.retainIncomingVoteForRestart(emergency.load().get(0)));
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())),
                "restart or backend movement repeated a completed reward");
    }

    @Test
    void existingTimedQueuePersistsAcceptedRewardBeforeLaterWorkFails(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        proxy.unavailableBackends.add("backend-b");
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = new UUID(2, 506);
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        com.bencodez.votingplugin.timequeue.VoteTimeQueue original =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        original.setUuid(new UUID(1, 506).toString());
        original.setWasOnline(true);
        VoteTotalsSnapshot totals = new VoteTotalsSnapshot(25, 10, 7, 2, 11, 3, 50, 10);
        original.setTotals(totals.toString());
        original.setTotalsApplied(true);
        assertTrue(proxy.cache.addTimeVoteToCache(original));
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.RETRY, VotingPluginProxyAccess.replayQueuedVote(proxy, original, totals, true));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.cache.load();
        enableLiveBroadcasts(restarted);
        restarted.playerServer = "backend-b";
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(recovered);
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())));
        assertFalse(updateHandoffs(restarted).isEmpty());
        assertTrue(updateHandoffs(restarted).stream().allMatch(h -> totals.toString().equals(
                h.envelope().getFields().get(VotingPluginWire.K_TOTALS))), "restart replaced saved totals with zero");
    }

    @Test
    void independentlyCompletedBroadcastIsNotRepeatedAfterRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        proxy.unavailableBackends.add("backend-b");
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = new UUID(2, 507);
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        String player = new UUID(1, 507).toString();
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.unavailableBackends.clear();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        proxy.time.advance(INTERVAL);
        proxy.retryPendingOnlineBroadcasts();
        assertEquals(1, proxy.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel()) && "backend-b".equals(h.server())).count());
        com.bencodez.votingplugin.timequeue.VoteTimeQueue snapshot = proxy.snapshotIncomingVoteForRestart(
                new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L));
        assertTrue(snapshot.getBroadcastForwardedServers().contains("backend-b"));
        assertTrue(proxy.cache.addTimeVoteToCache(snapshot));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        restarted.cache.load();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        restarted.retryPendingOnlineBroadcasts();
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel())), "restart repeated an independently completed broadcast");
    }

    @Test
    void restartReusesCanonicalBroadcastOwnerAndIndependentCompletion(@TempDir Path directory) throws Exception {
        for (boolean finishBeforeReplay : List.of(false, true)) {
            Path data = directory.resolve(Boolean.toString(finishBeforeReplay));
            CapturingProxy proxy = new CapturingProxy(data);
            enableLiveBroadcasts(proxy);
            proxy.unavailableBackends.add("backend-b");
            when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
            VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
            UUID id = UUID.randomUUID();
            when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                    anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
            setField(proxy, "voteLogMysqlTable", log);
            String player = UUID.randomUUID().toString();
            assertThrows(VotingPluginProxy.VoteRetryException.class,
                    () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
            assertTrue(proxy.cache.addTimeVoteToCache(proxy.snapshotIncomingVoteForRestart(
                    new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L))));
            proxy.closeDispatcher();

            CapturingProxy restarted = new CapturingProxy(data);
            enableLiveBroadcasts(restarted);
            restarted.cache.load();
            if (finishBeforeReplay) restarted.checkOnlineVotes("Synthetic", player, BACKEND);
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
            assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                    VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            restarted.retryPendingOnlineBroadcasts();
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            restarted.retryPendingOnlineBroadcasts();
            assertEquals(1, restarted.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                    .equals(h.envelope().getSubChannel()) && "backend-b".equals(h.server())).count(),
                    "canonical and timed-source recovery repeated the same broadcast");
        }
    }

    @Test
    void rejectedRewardCacheBecomesTheDurableBroadcastOwnerOnSuccessfulRetry(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        proxy.deliveryAccepted = false;
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = UUID.randomUUID();
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        String player = UUID.randomUUID().toString();
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        assertFalse(proxy.cache.getOnlineVotes(player).get(0).isProxyBroadcastHandled());
        proxy.deliveryAccepted = true;
        proxy.unavailableBackends.add("backend-b");
        proxy.time.advance(TimeUnit.SECONDS.toNanos(2));
        doReturn("synthetic-entry").when(log).logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt());
        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        assertEquals(1, proxy.cache.getOnlineVotes(player).size());
        assertTrue(proxy.cache.getOnlineVotes(player).get(0).isRewardDelivered());
        assertTrue(proxy.cache.getOnlineVotes(player).get(0).needsBroadcastOn("backend-b"));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        restarted.cache.load();
        restarted.retryPendingOnlineBroadcasts();
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        restarted.retryPendingOnlineBroadcasts();
        assertEquals(1, restarted.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel()) && "backend-b".equals(h.server())).count());
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())));
    }

    @Test
    void legacyHandledTimedBroadcastStillDeliversFirstSingleServerReward(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L,
                        true, Set.of(BACKEND, "backend-b"), Set.of(BACKEND, "backend-b"),
                        "1,2,3,4,5,6,7,8", false, player);
        assertFalse(source.isWasOnlineKnown(), "legacy timed rows have no vote-time online sample");
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(false);
        restarted.cache.load();
        assertTrue(restarted.cache.getTimeChangeQueue().peek().isProxyBroadcastHandled());
        assertFalse(restarted.cache.getTimeChangeQueue().peek().isWasOnlineKnown());
        assertTrue(restarted.isPlayerOnline("Synthetic"), "the player joined before legacy replay");
        restarted.processQueue();
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));

        List<Handoff> rewards = restarted.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE
                .equals(h.envelope().getSubChannel())).toList();
        assertEquals(1, rewards.size(), "broadcast-only completion suppressed the first reward");
        assertEquals(BACKEND, rewards.get(0).server());
        assertEquals(id.toString(), rewards.get(0).envelope().getFields().get(VotingPluginWire.K_VOTE_ID));
        assertTrue(restarted.cache.getTimeChangeQueue().isEmpty());
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_BROADCAST
                .equals(h.envelope().getSubChannel())), "replay repeated an already-forwarded broadcast");
        restarted.closeDispatcher();
    }

    @Test
    void acceptedSingleServerRewardIgnoresChangedModeAndRemovedBackend(@TempDir Path directory) throws Exception {
        for (boolean changedMode : List.of(false, true)) {
            Path data = directory.resolve(Boolean.toString(changedMode));
            CapturingProxy proxy = new CapturingProxy(data);
            enableLiveBroadcasts(proxy);
            proxy.unavailableBackends.add("backend-b");
            when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
            VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
            UUID id = UUID.randomUUID();
            when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                    anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
            setField(proxy, "voteLogMysqlTable", log);
            String player = UUID.randomUUID().toString();
            assertThrows(VotingPluginProxy.VoteRetryException.class,
                    () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
            assertTrue(proxy.cache.addTimeVoteToCache(proxy.snapshotIncomingVoteForRestart(
                    new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L))));
            proxy.closeDispatcher();

            CapturingProxy restarted = new CapturingProxy(data);
            enableLiveBroadcasts(restarted);
            when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(changedMode);
            restarted.availableTargets.remove(BACKEND);
            restarted.setAvailableServers("backend-b");
            restarted.playerServer = "backend-b";
            restarted.cache.load();
            com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
            assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                    VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            restarted.checkOnlineVotes("Synthetic", player, "backend-b");
            restarted.checkCachedVotes("backend-b");
            assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_ONLINE
                    .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())),
                    "routing edits reissued an accepted single-server reward");
        }
    }

    @Test
    void allServerRecoveryKeepsOriginalTargetsAfterRoutingEdits(@TempDir Path directory) throws Exception {
        exerciseAllServerRecovery(false, directory);
    }

    @Test
    void offlineForwardRecoveryKeepsCanonicalProgressAfterRoutingEdits(@TempDir Path directory) throws Exception {
        exerciseAllServerRecovery(true, directory);
    }

    private void exerciseAllServerRecovery(boolean offlineForward, Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        when(proxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
        if (offlineForward) {
            proxy.offlinePlayers.add("Synthetic");
            when(proxy.getConfig().getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
        }
        proxy.unavailableBackends.add("backend-b");
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = UUID.randomUUID();
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        String player = UUID.randomUUID().toString();
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        assertTrue(proxy.cache.addTimeVoteToCache(proxy.snapshotIncomingVoteForRestart(
                new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L))));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(false);
        if (offlineForward) when(restarted.getConfig().getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
        restarted.availableTargets.add("backend-c");
        restarted.setAvailableServers(BACKEND, "backend-b", "backend-c");
        restarted.playerServer = "backend-c";
        restarted.cache.load();
        restarted.failingTimeWrites = 1;
        restarted.checkCachedVotes("backend-b");
        assertFalse(restarted.cache.getVotes("backend-b").isEmpty(), "failed source write deleted the target owner");
        restarted.checkCachedVotes("backend-b");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        restarted.checkCachedVotes("backend-b");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        restarted.retryPendingOnlineBroadcasts();
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        List<Handoff> rewards = java.util.stream.Stream.concat(proxy.handoffs.stream(), restarted.handoffs.stream()).filter(h -> VotingPluginWire.SUB_VOTE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())).toList();
        assertEquals(List.of(BACKEND, "backend-b"), rewards.stream().map(Handoff::server).sorted().toList(),
                "routing edits changed or repeated the original remaining reward plan");
        if (offlineForward) assertEquals(2, java.util.stream.Stream.concat(proxy.handoffs.stream(), restarted.handoffs.stream())
                .filter(h -> VotingPluginWire.SUB_VOTE_BROADCAST.equals(h.envelope().getSubChannel())).count(),
                "offline recovery repeated a standalone broadcast");
    }

    @Test
    void earlyFailurePreservesAllServerPlanThroughRescueJournal(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        when(proxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
        proxy.failVoteParty = true;
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        PendingIncomingVoteJournal rescue = PendingIncomingVoteJournal.rescue(directory);
        rescue.merge(List.of(proxy.snapshotIncomingVoteForRestart(
                new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L))));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(false);
        restarted.availableTargets.add("backend-c");
        restarted.setAvailableServers(BACKEND, "backend-b", "backend-c");
        restarted.playerServer = "backend-c";
        restarted.cache.load();
        assertTrue(restarted.retainIncomingVoteForRestart(rescue.load().get(0)));
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        List<String> targets = restarted.handoffs.stream().filter(h -> VotingPluginWire.SUB_VOTE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())).map(Handoff::server).sorted().toList();
        assertEquals(List.of(BACKEND, "backend-b"), targets, "early failure lost the original target plan");
    }

    @Test
    void incompleteRebuiltOwnerCannotReviveAcceptedTargetsAfterRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        source.setTotalsApplied(true);
        source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":all", id.toString());
        for (String server : List.of(BACKEND, "backend-b")) {
            source.setHttpBroadcastDeliveryId(VotingPluginWire.PLANNED_REWARD_TARGET_PREFIX + server, id.toString());
            source.setHttpBroadcastDeliveryId(VotingPluginWire.ACCEPTED_REWARD_TARGET_PREFIX + server, id.toString());
        }
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        proxy.failAfterOnlineInsert = true;
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.RETRY, VotingPluginProxyAccess.replayQueuedVote(proxy, source, null, true));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        enableLiveBroadcasts(restarted);
        restarted.cache.load();
        restarted.retryPendingOnlineBroadcasts();
        restarted.checkCachedVotes(BACKEND);
        restarted.checkCachedVotes("backend-b");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())), "rebuilt incomplete owner revived accepted targets");
    }

    @Test
    void existingIncompleteOwnerIsReconciledWithAcceptedSourceBeforeReplay(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        enableLiveBroadcasts(proxy);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        source.setTotalsApplied(true);
        source.setVotePartyApplied(true);
        source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":all", id.toString());
        OfflineBungeeVote owner = new OfflineBungeeVote(id, "Synthetic", player, "synthetic-service", 1L, true, "");
        for (String server : List.of(BACKEND, "backend-b")) {
            source.setHttpBroadcastDeliveryId(VotingPluginWire.PLANNED_REWARD_TARGET_PREFIX + server, id.toString());
            source.setHttpBroadcastDeliveryId(VotingPluginWire.ACCEPTED_REWARD_TARGET_PREFIX + server, id.toString());
            owner.setHttpDeliveryId("__vp_reward_target__:" + server, UUID.randomUUID().toString());
        }
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.cache.addOnlineVoteDurably(player, owner));
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(proxy, source, null, true));
        assertTrue(owner.isRewardDelivered(), "replay left a stale incomplete canonical owner");
        proxy.cache.getTimeChangeQueue().clear();
        proxy.retryPendingOnlineBroadcasts();
        proxy.checkCachedVotes(BACKEND);
        proxy.checkCachedVotes("backend-b");
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(proxy.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel())));
    }

    @Test
    void singleServerSourceReplayRetiresCanonicalRewardBeforePlayerMoves(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        source.setVotePartyApplied(true);
        source.setTotalsApplied(true);
        source.setTotals("1,2,3,4,5,6,7,8");
        source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":single", id.toString());
        OfflineBungeeVote owner = new OfflineBungeeVote(id, "Synthetic", player, "synthetic-service", 1L,
                true, source.getTotals());
        owner.setWasOnline(true);
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.cache.addOnlineVoteDurably(player, owner));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(false);
        when(restarted.getConfig().getProxyBroadcastEnabled()).thenReturn(false);
        restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
        restarted.cache.load();
        assertFalse(restarted.cache.getOnlineVotes(player).get(0).isRewardDelivered());
        restarted.processQueue();
        assertTrue(restarted.cache.getTimeChangeQueue().isEmpty(), "source did not durably complete");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(BACKEND), restarted.handoffs.stream()
                .filter(h -> VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel()))
                .map(Handoff::server).toList());

        restarted.playerServer = "backend-b";
        restarted.checkOnlineVotes("Synthetic", player, "backend-b");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(BACKEND), restarted.handoffs.stream()
                .filter(h -> VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel()))
                .map(Handoff::server).toList(), "stale canonical reward was delivered again after the player moved");
        restarted.closeDispatcher();
    }

    @Test
    void acceptedSingleServerSourceFencesOnlineCacheBeforeReplayOnAnotherBackend(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        source.setVotePartyApplied(true);
        source.setTotalsApplied(true);
        source.setTotals("1,2,3,4,5,6,7,8");
        source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":single", id.toString());
        source.setHttpBroadcastDeliveryId(VotingPluginWire.ACCEPTED_REWARD_TARGET_PREFIX + BACKEND, id.toString());
        OfflineBungeeVote owner = new OfflineBungeeVote(id, "Synthetic", player, "synthetic-service", 1L,
                true, source.getTotals());
        owner.setWasOnline(true);
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.cache.addOnlineVoteDurably(player, owner));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        when(restarted.getConfig().getSendVotesToAllServers()).thenReturn(false);
        when(restarted.getConfig().getProxyBroadcastEnabled()).thenReturn(false);
        restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
        restarted.playerServer = "backend-b";
        restarted.cache.load();
        restarted.checkOnlineVotes("Synthetic", player, "backend-b");
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertFalse(restarted.cache.getTimeChangeQueue().isEmpty(), "test unexpectedly replayed the timed source");
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(h.envelope().getSubChannel()) || VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())),
                "cache delivery ignored the original backend's accepted single-server occurrence");
        restarted.closeDispatcher();
    }

    @Test
    void offlineAcceptedMarkersAreNotPendingHttpButRealBroadcastIdsAre(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        UUID id = new UUID(2, 508);
        String player = new UUID(1, 508).toString();
        OfflineBungeeVote vote = new OfflineBungeeVote(id, "Synthetic", player, "synthetic-service", 1L,
                true, "", false, true, Set.of("backend-b"), Set.of(), true, Map.of(),
                Map.of(VotingPluginWire.ACCEPTED_REWARD_TARGET_PREFIX + BACKEND, id.toString()));
        assertFalse(vote.hasPendingHttpDeliveryIds());
        assertTrue(proxy.cache.addOnlineVoteDurably(player, vote));
        Method pending = VotingPluginProxy.class.getDeclaredMethod("hasPendingCachedHttpDeliveries");
        pending.setAccessible(true);
        assertEquals(false, pending.invoke(proxy));
        vote.setHttpBroadcastDeliveryId("backend-b", UUID.randomUUID().toString());
        assertTrue(vote.hasPendingHttpDeliveryIds());
        assertTrue(proxy.cache.updateOnlineVote(player, vote));
        assertEquals(true, pending.invoke(proxy));
    }

    @Test
    void fullControlLaneDoesNotPreventDurableVoteUpdateAdmission(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_LANE_CONTROLS; i++)
            proxy.sendPluginMessageServer("backend-b", 0, control("occupied"));
        UUID id = new UUID(2, 500);
        String player = new UUID(1, 500).toString();

        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(6));
        assertEquals(List.of(BACKEND, "backend-b"), updateHandoffs(proxy).stream()
                .map(Handoff::server).sorted().toList());
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel())).count());
        assertPaced(proxy.handoffs);
    }

    @Test
    void unavailableVoteUpdateTargetSurvivesRestartWithoutOccupyingControlCapacity(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.unavailableBackends.add("backend-b");
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        UUID id = new UUID(2, 502);
        String player = new UUID(1, 502).toString();
        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(BACKEND), updateHandoffs(proxy).stream().map(Handoff::server).toList());
        assertEquals(0, (PluginMessageDispatcherAccess.view(field(proxy, "pluginMessageDispatcher"))).pendingControls());
        assertEquals(1, proxy.outbox.size());
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of("backend-b"), updateHandoffs(restarted).stream().map(Handoff::server).toList());
        assertEquals(id.toString(), updateHandoffs(restarted).get(0).envelope().getFields().get(VotingPluginWire.K_VOTE_ID));
        assertEquals(0, new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).size());
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(1, updateHandoffs(restarted).size());
    }

    @Test
    void quarantinedMixedServerBatchKeepsReliablePreparedVoteProgress(@TempDir Path directory) throws Exception {
        verifyQuarantinedMixedBatch(directory.resolve("clean-reliable"), false, false, true);
        verifyQuarantinedMixedBatch(directory.resolve("dirty-reliable"), false, true, true);
        verifyQuarantinedMixedBatch(directory.resolve("clean-legacy"), false, false, false);
    }

    @Test
    void quarantinedMixedOnlineBatchKeepsReliablePreparedVoteProgress(@TempDir Path directory) throws Exception {
        verifyQuarantinedMixedBatch(directory.resolve("clean-reliable"), true, false, true);
        verifyQuarantinedMixedBatch(directory.resolve("dirty-reliable"), true, true, true);
        verifyQuarantinedMixedBatch(directory.resolve("clean-legacy"), true, false, false);
    }

    private static void verifyQuarantinedMixedBatch(Path directory, boolean online, boolean dirty,
            boolean reliable) throws Exception {
        java.nio.file.Files.createDirectories(directory);
        CapturingProxy proxy = new CapturingProxy(directory);
        if (reliable) proxy.reliable().add(BACKEND);
        else proxy.legacy().add(BACKEND);
        String player = new UUID(14, 1).toString();
        String owner = online ? player : BACKEND;
        OfflineBungeeVote prepared = new OfflineBungeeVote(new UUID(15, 1), "Synthetic", player,
                "synthetic-service-a", 1L, true, "");
        OfflineBungeeVote unprepared = new OfflineBungeeVote(new UUID(15, 2), "Synthetic", player,
                "synthetic-service-b", 2L, true, "");
        if (online) assertTrue(proxy.cache.addOnlineVoteDurably(owner, prepared));
        else assertTrue(proxy.cache.addServerVoteDurably(owner, prepared));
        assertTrue(proxy.cache.prepareCachedVoteBatch(owner, online, List.of(prepared), 1));
        assertTrue(prepared.hasCachedVoteBatch());
        assertTrue(prepared.isCachedBatchDurable());
        assertFalse(prepared.isDeliveryStateDirty());
        if (online) assertTrue(proxy.cache.addOnlineVoteDurably(owner, unprepared));
        else assertTrue(proxy.cache.addServerVoteDurably(owner, unprepared));
        if (dirty) prepared.setDeliveryStateDirty(true);
        proxy.failCacheDurableSave = true;
        proxy.failCacheReload = true;

        if (online) proxy.checkOnlineVotes("Synthetic", player, BACKEND);
        else proxy.checkCachedVotes(BACKEND);

        assertTrue(proxy.cache.isJsonStorageQuarantined(), "failed rollback did not quarantine JSON writes");
        assertFalse(unprepared.hasCachedVoteBatch(), "failed preparation published the new batch position");
        assertFalse(unprepared.isRewardDelivered());
        assertEquals(List.of(prepared, unprepared), online ? proxy.cache.getOnlineVotes(owner) : proxy.cache.getVotes(owner),
                "failed completion persistence removed a cache owner");
        String subChannel = online ? VotingPluginWire.SUB_VOTE_ONLINE : VotingPluginWire.SUB_VOTE;
        ReliableVoteDeliveryOutboxAccess durable = new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat"));
        if (reliable && !dirty) {
            assertEquals(1, durable.size(), "unprepared sibling blocked a clean reliable batch");
            assertSingleVoteBatch(durable.snapshot().get(0).envelope(), subChannel, prepared.getVoteId());
            assertEquals(1, proxy.handoffs.size());
            assertSingleVoteBatch(proxy.handoffs.get(0).envelope(), subChannel, prepared.getVoteId());
            assertTrue(prepared.isRewardDelivered());
            assertTrue(prepared.isDeliveryStateDirty(), "failed cache completion was reported durable");
        } else {
            assertEquals(0, durable.size(), "dirty or legacy work crossed the quarantined journal boundary");
            assertTrue(proxy.handoffs.isEmpty());
            assertFalse(prepared.isRewardDelivered());
        }
        assertFalse(durable.find(BACKEND, unprepared.getVoteId(), subChannel).isPresent());
        proxy.closeDispatcher();

        CapturingProxy recovered = new CapturingProxy(directory);
        recovered.cache.load();
        List<OfflineBungeeVote> recoveredVotes = online ? recovered.cache.getOnlineVotes(owner) : recovered.cache.getVotes(owner);
        assertEquals(List.of(prepared.getVoteId(), unprepared.getVoteId()), recoveredVotes.stream()
                .map(OfflineBungeeVote::getVoteId).toList(), "quarantine changed the last readable cache snapshot");
        assertEquals(1, recoveredVotes.get(0).getCachedBatchNumber());
        assertEquals(1, recoveredVotes.get(0).getCachedBatchSize());
        assertFalse(recoveredVotes.get(0).isRewardDelivered());
        assertFalse(recoveredVotes.get(1).hasCachedVoteBatch());
        recovered.closeDispatcher();
    }

    @Test
    void quarantinedCachedCompletionKeepsReceiptAndOriginalBackendAcrossRestart(@TempDir Path base) throws Exception {
        for (boolean online : List.of(false, true)) for (BungeeMethod recoveryMethod : List.of(
                BungeeMethod.PLUGINMESSAGING, BungeeMethod.MYSQL)) {
            Path directory = base.resolve(online ? "online" : "server").resolve(recoveryMethod.name());
            java.nio.file.Files.createDirectories(directory);
            CapturingProxy proxy = new CapturingProxy(directory);
            proxy.reliable().add(BACKEND);
            OfflineBungeeVote first = cached(1501);
            OfflineBungeeVote sibling = cached(1502);
            String owner = online ? first.getUuid() : BACKEND;
            if (online) assertTrue(proxy.cache.addOnlineVoteDurably(owner, first));
            else assertTrue(proxy.cache.addServerVoteDurably(owner, first));
            assertTrue(proxy.cache.prepareCachedVoteBatch(owner, online, List.of(first), 1));
            if (online) assertTrue(proxy.cache.addOnlineVoteDurably(owner, sibling));
            else assertTrue(proxy.cache.addServerVoteDurably(owner, sibling));
            proxy.failCacheDurableSave = true;
            proxy.failCacheReload = true;
            if (online) proxy.checkOnlineVotes("Synthetic", owner, BACKEND);
            else proxy.checkCachedVotes(BACKEND);
            String kind = online ? VotingPluginWire.SUB_VOTE_ONLINE : VotingPluginWire.SUB_VOTE;
            assertEquals(1, proxy.outbox.size());
            invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                    VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, first.getVoteId(), kind));
            proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
            assertTrue(proxy.handoffs.stream().noneMatch(h ->
                    VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(h.envelope().getSubChannel())));
            invoke(proxy, "handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class,
                    VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(BACKEND, first.getVoteId(), kind));
            assertEquals(1, proxy.outbox.size(), "unpersisted cache acceptance released its durable receipt owner");
            assertTrue(proxy.outbox.snapshot().get(0).awaitingReceiptRelease());
            proxy.closeDispatcher();

            CapturingProxy restarted = new CapturingProxy(directory);
            restarted.setMethod(recoveryMethod);
            restarted.reliable().addAll(List.of(BACKEND, "backend-b"));
            restarted.cache.load();
            if (online) restarted.checkOnlineVotes("Synthetic", owner, "backend-b");
            else restarted.checkCachedVotes(BACKEND);
            if (online) assertFalse(restarted.outbox.find("backend-b", first.getVoteId(), kind).isPresent(),
                    "recovery created a second backend owner for the original accepted reward");
            assertTrue(restarted.handoffs.stream().noneMatch(h -> first.getVoteId().toString()
                    .equals(h.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))
                    && (VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())
                            || VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel()))),
                    "cache recovery repeated the original accepted reward");
            invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            assertTrue(restarted.handoffs.stream().noneMatch(h -> first.getVoteId().toString()
                    .equals(h.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))
                    && (VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())
                            || VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel()))),
                    "delayed transport recovery repeated the original accepted reward");
            assertTrue(restarted.handoffs.stream().anyMatch(h ->
                    VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(h.envelope().getSubChannel())
                            && first.getVoteId().toString().equals(h.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))));
            invoke(restarted, "handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class,
                    VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(BACKEND, first.getVoteId(), kind));
            assertFalse(restarted.outbox.find(BACKEND, first.getVoteId(), kind).isPresent());
            restarted.closeDispatcher();
        }
    }

    @Test
    void timedSourceWithoutCacheRetainsOriginalRewardTargetBeforeReceiptRelease(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        VoteTotalsSnapshot totals = new VoteTotalsSnapshot(25, 10, 7, 2, 11, 3, 50, 10);
        source.setTotals(totals.toString());
        source.setTotalsApplied(true);
        source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":single", id.toString());
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.outbox.offer(BACKEND, VotingPluginWire.voteOnline(
                "Synthetic", player, "synthetic-service", 1L, true, true, totals.toString(), id, false, false, 1, 1)));
        assertTrue(proxy.cache.getOnlineVotes(player).isEmpty());
        proxy.failingTimeWrites = 2;
        invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE));
        assertTrue(proxy.handoffs.isEmpty(), "receipt released before the surviving timed source was durable");
        invoke(proxy, "handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE));
        assertEquals(1, proxy.outbox.size());
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.reliable().addAll(List.of(BACKEND, "backend-b"));
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        restarted.cache.load();
        restarted.playerServer = "backend-b";
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(recovered);
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered, totals, true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())
                || VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel())),
                "restart repeated the admitted reward on a new backend");
        invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().anyMatch(h ->
                VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(h.envelope().getSubChannel())), "persisted timed acceptance did not release its original receipt");
        invoke(restarted, "handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE));
        assertFalse(restarted.outbox.find(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE).isPresent());
        restarted.closeDispatcher();
    }

    @Test
    void legacyRetirementRetainsTimedOwnerAfterWriteFailureAndRestart(@TempDir Path base) throws Exception {
        for (boolean recoveredFence : List.of(false, true)) {
            Path directory = base.resolve(recoveredFence ? "fenced" : "pending");
            java.nio.file.Files.createDirectories(directory);
            CapturingProxy proxy = new CapturingProxy(directory);
            proxy.legacy().add(BACKEND);
            UUID id = UUID.randomUUID();
            String player = UUID.randomUUID().toString();
            com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                    new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
            source.setUuid(player);
            source.setWasOnline(true);
            VoteTotalsSnapshot totals = new VoteTotalsSnapshot(25, 10, 7, 2, 11, 3, 50, 10);
            source.setTotals(totals.toString());
            source.setTotalsApplied(true);
            source.setHttpBroadcastDeliveryId(VotingPluginWire.REWARD_ROUTING_MODE_KEY + ":single", id.toString());
            assertTrue(proxy.cache.addTimeVoteToCache(source));
            assertTrue(proxy.outbox.offer(BACKEND, VotingPluginWire.voteOnline(
                    "Synthetic", player, "synthetic-service", 1L, true, true, totals.toString(), id, false, false, 1, 1)));
            if (recoveredFence) assertTrue(proxy.outbox.beginLegacyDelivery(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE));
            proxy.failingTimeWrites = 1;
            proxy.retry();
            assertTrue(proxy.outbox.find(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE).isPresent(),
                    "legacy retirement discarded the last durable original-target owner");
            assertEquals(recoveredFence ? 0 : 1, proxy.handoffs.size());
            proxy.closeDispatcher();

            CapturingProxy restarted = new CapturingProxy(directory);
            when(restarted.getConfig().getOnlineMode()).thenReturn(true);
            restarted.cache.load();
            restarted.playerServer = "backend-b";
            restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
            restarted.retry();
            assertFalse(restarted.outbox.find(BACKEND, id, VotingPluginWire.SUB_VOTE_ONLINE).isPresent());
            com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
            assertNotNull(recovered);
            assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered, totals, true));
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
            assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())
                    || VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel())),
                    "legacy recovery repeated an already fenced reward");
            restarted.closeDispatcher();
        }
    }

    @Test
    void retiredUpdateAdmissionsSurviveLaterFailureAndRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(2, updateHandoffs(proxy).size());
        assertTrue(proxy.outbox.snapshot().stream().noneMatch(e -> VotingPluginWire.SUB_VOTE_UPDATE
                .equals(e.envelope().getSubChannel())));
        assertTrue(proxy.retainIncomingVoteForRestart(new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L)));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        restarted.cache.load();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(recovered);
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(updateHandoffs(restarted).isEmpty(), "restart replayed updates that already handed off");
        restarted.closeDispatcher();
    }

    @Test
    void legacyUpdateFenceWithUnresolvedTimedIdentityRecoversWithoutReplay(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        proxy.resolvedUuids.put("Synthetic", player);
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setWasOnline(false);
        source.setTotalsApplied(true);
        VoteTotalsSnapshot totals = new VoteTotalsSnapshot(25, 10, 7, 2, 11, 3, 50, 10);
        source.setTotals(totals.toString());
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.outbox.retainUpdate(BACKEND, VotingPluginWire.voteUpdate(player, 3, 50,
                "synthetic-service", 1L, totals.toString()).toBuilder().put(VotingPluginWire.K_VOTE_ID, id).build(), true));
        assertTrue(proxy.outbox.beginLegacyDelivery(BACKEND, id, VotingPluginWire.SUB_VOTE_UPDATE));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.resolvedUuids.put("Synthetic", player);
        restarted.cache.load();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(recovered);
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered, totals, true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(updateHandoffs(restarted).stream().noneMatch(h -> BACKEND.equals(h.server())),
                "resolving a timed source repeated its already fenced update");
        assertFalse(restarted.outbox.find(BACKEND, id, VotingPluginWire.SUB_VOTE_UPDATE).isPresent());
        restarted.closeDispatcher();
    }

    @Test
    void legacyTransportCompletionPersistsOriginalOnlineCacheOwnerAcrossRestart(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                new com.bencodez.votingplugin.timequeue.VoteTimeQueue(id, "Synthetic", "synthetic-service", 1L);
        source.setUuid(player);
        source.setWasOnline(true);
        assertTrue(proxy.cache.addTimeVoteToCache(source));
        assertTrue(proxy.sendPluginMessageImmediately(BACKEND,
                JsonEnvelope.builder("SyntheticControl").put("value", "occupied").build()));
        proxy.failAfterOnlineInsert = true;
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.RETRY, VotingPluginProxyAccess.replayQueuedVote(proxy, source, null, true));
        assertEquals(1, proxy.cache.getOnlineVotes(player).size());
        assertFalse(proxy.cache.getOnlineVotes(player).get(0).isRewardDelivered());
        assertTrue(proxy.retainIncomingVoteForRestart(
                new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L)));
        when(proxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        proxy.reloadFromControl();
        GlobalMessageProxyHandler first = mock(GlobalMessageProxyHandler.class);
        proxy.setGlobalMessageProxyHandlerForTest(first);
        proxy.processQueue();
        verify(first).sendMessage(eq(BACKEND), anyInt(), argThat(envelope ->
                VotingPluginWire.SUB_VOTE_ONLINE.equals(envelope.getSubChannel())));
        assertTrue(proxy.cache.getTimeChangeQueue().isEmpty());
        proxy.completeRuntimeReplacementShutdown();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.setMethod(BungeeMethod.MYSQL);
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
        restarted.playerServer = "backend-b";
        restarted.cache.load();
        assertTrue(restarted.cache.getTimeChangeQueue().isEmpty());
        GlobalMessageProxyHandler replay = mock(GlobalMessageProxyHandler.class);
        restarted.setGlobalMessageProxyHandlerForTest(replay);
        restarted.checkOnlineVotes("Synthetic", player, "backend-b");
        verify(replay, never()).sendMessage(anyString(), anyInt(), argThat(envelope ->
                VotingPluginWire.SUB_VOTE_ONLINE.equals(envelope.getSubChannel())
                        || VotingPluginWire.SUB_VOTE.equals(envelope.getSubChannel())));
        restarted.closeDispatcher();
    }

    @Test
    void actualTransportReloadPreservesOriginalAcceptedTargetsAcrossRestart(@TempDir Path directory) throws Exception {
        exerciseTransportReloadRecovery(false, directory);
    }

    @Test
    void transportResumeKeepsParkedPluginUpdatesWithTheirOriginalOwner(@TempDir Path directory) throws Exception {
        exerciseTransportReloadRecovery(true, directory);
    }

    private void exerciseTransportReloadRecovery(boolean parked, Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        if (parked) proxy.unavailableBackends.add("backend-b");
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = UUID.randomUUID();
        String player = UUID.randomUUID().toString();
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("synthetic log failure"));
        setField(proxy, "voteLogMysqlTable", log);
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        PendingIncomingVote pending = new PendingIncomingVote(id, "Synthetic", "synthetic-service", 1L);
        Map<String, String> original = new java.util.LinkedHashMap<>(proxy.snapshotIncomingVoteForRestart(pending)
                .getHttpBroadcastDeliveryIds());
        assertEquals(4, original.size());
        when(proxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        proxy.reloadFromControl();
        assertEquals(BungeeMethod.MYSQL, proxy.getMethod());
        when(proxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
        GlobalMessageProxyHandler resumed = mock(GlobalMessageProxyHandler.class);
        proxy.setGlobalMessageProxyHandlerForTest(resumed);
        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        verify(resumed, never()).sendMessage(anyString(), anyInt(), argThat(envelope ->
                VotingPluginWire.SUB_VOTE_UPDATE.equals(envelope.getSubChannel())
                        || VotingPluginWire.SUB_VOTE.equals(envelope.getSubChannel())
                        || VotingPluginWire.SUB_VOTE_ONLINE.equals(envelope.getSubChannel())));
        if (parked) assertTrue(proxy.outbox.find("backend-b", id, VotingPluginWire.SUB_VOTE_UPDATE).isPresent(),
                "transport resume discarded the parked original update owner");
        assertEquals(original, proxy.snapshotIncomingVoteForRestart(pending).getHttpBroadcastDeliveryIds(),
                "transport reload discarded original routing and accepted targets");
        assertTrue(proxy.retainIncomingVoteForRestart(pending));
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        restarted.playerServer = "backend-b";
        restarted.cache.load();
        com.bencodez.votingplugin.timequeue.VoteTimeQueue recovered = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(recovered);
        assertEquals(VotingPluginProxyAccess.QueuedVoteResult.SUCCESS, VotingPluginProxyAccess.replayQueuedVote(restarted, recovered,
                VoteTotalsSnapshot.parseStorage(recovered.getTotals()), true));
        restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertTrue(restarted.handoffs.stream().noneMatch(h -> VotingPluginWire.SUB_VOTE.equals(h.envelope().getSubChannel())
                || VotingPluginWire.SUB_VOTE_ONLINE.equals(h.envelope().getSubChannel())),
                "transport switch and restart repeated an accepted reward");
        assertEquals(parked ? 1 : 0, updateHandoffs(restarted).size());
        assertTrue(updateHandoffs(restarted).stream().allMatch(h -> "backend-b".equals(h.server())
                && id.toString().equals(h.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))),
                "recovery repeated a completed update or replaced the parked identity");
        restarted.closeDispatcher();
    }

    @Test
    void saturatedUpdateQuotaPreservesIndependentQueuedVoteProgressAndDeferredUpdates(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.unavailableBackends.add("backend-b");
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        setField(proxy, "voteLogMysqlTable", log);
        String player = new UUID(11, 1).toString();
        Set<String> existingUpdateIds = new LinkedHashSet<>();
        for (int i = 0; i < ReliableVoteDeliveryOutboxAccess.MAX_UPDATE_ENTRIES; i++) {
            UUID id = new UUID(12, i);
            existingUpdateIds.add(id.toString());
            assertTrue(proxy.outbox.offer("backend-b", VotingPluginWire.voteUpdate(player, 1, 10,
                    "synthetic-service-a", i + 1L, "").toBuilder().put(VotingPluginWire.K_VOTE_ID, id).build()));
        }
        List<UUID> newVoteIds = List.of(new UUID(13, 1), new UUID(13, 2));
        for (int i = 0; i < newVoteIds.size(); i++) {
            com.bencodez.votingplugin.timequeue.VoteTimeQueue source =
                    new com.bencodez.votingplugin.timequeue.VoteTimeQueue(newVoteIds.get(i), "Synthetic",
                            "synthetic-service-" + (i == 0 ? "a" : "b"), 2048L + i);
            source.setUuid(player);
            source.setWasOnline(true);
            assertTrue(proxy.cache.addTimeVoteToCache(source));
        }

        proxy.processQueue();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));

        for (UUID id : newVoteIds) {
            verify(log).logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), eq(player),
                    eq("Synthetic"), anyLong(), anyInt());
        }
        assertEquals(newVoteIds.stream().map(UUID::toString).collect(java.util.stream.Collectors.toSet()),
                proxy.handoffs.stream().filter(handoff -> BACKEND.equals(handoff.server())
                        && VotingPluginWire.SUB_VOTE_ONLINE.equals(handoff.envelope().getSubChannel()))
                        .map(handoff -> handoff.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))
                        .collect(java.util.stream.Collectors.toSet()), "an empty backend blocked another backend's reward");
        assertTrue(updateHandoffs(proxy).stream().noneMatch(handoff -> "backend-b".equals(handoff.server())));
        assertTrue(new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).snapshot().stream()
                .map(entry -> entry.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))
                .collect(java.util.stream.Collectors.toSet()).containsAll(existingUpdateIds),
                "quota handling discarded existing update occurrences");
        proxy.closeDispatcher();

        CapturingProxy restarted = new CapturingProxy(directory);
        restarted.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(restarted.getConfig().getOnlineMode()).thenReturn(true);
        when(restarted.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        setField(restarted, "voteLogMysqlTable", log);
        restarted.cache.load();
        for (int i = 0; i < 30; i++) {
            restarted.processQueue();
            invoke(restarted, "retryReliableVoteDeliveries", String.class, null);
            restarted.time.advance(TimeUnit.SECONDS.toNanos(1));
        }
        Set<String> expectedUpdateIds = new LinkedHashSet<>(existingUpdateIds);
        newVoteIds.stream().map(UUID::toString).forEach(expectedUpdateIds::add);
        List<String> deliveredUpdateIds = updateHandoffs(restarted).stream()
                .filter(handoff -> "backend-b".equals(handoff.server()))
                .map(handoff -> handoff.envelope().getFields().get(VotingPluginWire.K_VOTE_ID)).toList();
        assertEquals(expectedUpdateIds, new LinkedHashSet<>(deliveredUpdateIds),
                "deferred updates lost their original vote IDs across restart");
        assertEquals(expectedUpdateIds.size(), deliveredUpdateIds.size(), "restart repeated an accepted update");
        assertTrue(restarted.cache.getTimeChangeQueue().isEmpty(), "settled source rows remained queued");
        assertEquals(0, new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat")).size());
        assertTrue(restarted.handoffs.stream().noneMatch(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel())), "restart repeated an accepted reward");
        for (UUID id : newVoteIds) {
            verify(log, times(1)).logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(),
                    eq(player), eq("Synthetic"), anyLong(), anyInt());
        }
        assertPaced(proxy.handoffs);
        assertPaced(restarted.handoffs);
        restarted.closeDispatcher();
    }

    @Test
    void failureAfterUpdateAdmissionCannotRepeatUpdatesOnTheSameVoteRetry(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getVoteLoggingEnabled()).thenReturn(true);
        VoteLogMysqlTable log = mock(VoteLogMysqlTable.class);
        UUID id = new UUID(2, 501);
        String player = new UUID(1, 501).toString();
        when(log.logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(), anyString(),
                anyString(), anyLong(), anyInt())).thenThrow(new IllegalStateException("Synthetic log failure"))
                .thenReturn(id.toString());
        setField(proxy, "voteLogMysqlTable", log);

        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(2, updateHandoffs(proxy).size());

        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(BACKEND, "backend-b"), updateHandoffs(proxy).stream()
                .map(Handoff::server).sorted().toList());
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel())).count(), "retry repeated the accepted reward");
        verify(log, times(2)).logVote(eq(id), any(VoteLogMysqlTable.VoteLogStatus.class), anyString(),
                anyString(), anyString(), anyLong(), anyInt());
        assertPaced(proxy.handoffs);
    }

    private static List<Handoff> updateHandoffs(CapturingProxy proxy) {
        return proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_UPDATE
                .equals(handoff.envelope().getSubChannel())).toList();
    }

    private static List<Handoff> rewardHandoffs(CapturingProxy proxy) {
        return proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE
                .equals(handoff.envelope().getSubChannel())).toList();
    }

    private static JsonEnvelope control(String value) {
        return JsonEnvelope.builder("SyntheticControl").put("value", value).build();
    }

    private static JsonEnvelope vote(int value) {
        return VotingPluginWire.vote("Synthetic", new UUID(1, value).toString(), "synthetic-service", 1L,
                true, true, "", new UUID(2, value), false, false, 1, 1);
    }

    private static OfflineBungeeVote cached(int value) {
        return new OfflineBungeeVote(new UUID(2, value), "Synthetic", new UUID(1, value).toString(),
                "synthetic-service", 1L, true, "");
    }

    private static void assertPaced(List<Handoff> handoffs) {
        Map<String, Long> previous = new HashMap<>();
        for (Handoff handoff : handoffs) {
            Long before = previous.put(handoff.server(), handoff.time());
            if (before != null) assertTrue(handoff.time() - before >= INTERVAL, "same-backend handoffs overlap");
        }
    }

    private record Handoff(long time, String server, JsonEnvelope envelope) { }

    private static final class CapturingProxy extends VotingPluginProxyTestImpl {
        final ManualTime time = new ManualTime();
        final List<Handoff> handoffs = new ArrayList<>();
        final VoteCacheHandler cache;
        final BungeeJsonVoteCache storage;
        final ReliableVoteDeliveryOutboxAccess outbox;
        boolean carrier = true;
        boolean broadcastValidationEnabled;
        final Set<String> unavailableBackends = new LinkedHashSet<>();
        final Set<String> availableTargets = new LinkedHashSet<>(List.of(BACKEND, "backend-b"));
        boolean failDurablePartySave;
        boolean persistPartyState;
        boolean failVoteParty;
        boolean failCacheDurableSave;
        boolean failCacheReload;
        int failingTimeWrites;
        boolean failAfterOnlineInsert;
        boolean deliveryAccepted = true;
        String playerServer = BACKEND;
        final Map<String, String> playerRoutes = new HashMap<>();
        final Map<String, String> resolvedUuids = new HashMap<>();
        final Set<String> offlinePlayers = new LinkedHashSet<>();
        int transportAttempts;
        int transportDepth;
        int maxTransportDepth;
        Consumer<JsonEnvelope> onHandoff = ignored -> { };

        CapturingProxy(Path directory) throws Exception {
            setMethod(BungeeMethod.PLUGINMESSAGING);
            setAvailableServers(BACKEND, "backend-b");
            setDataFolder(directory.toFile());
            setField(this, "enabled", true);
            when(getConfig().getBlockedServers()).thenReturn(List.of());
            VotingPluginBungee platform = mock(VotingPluginBungee.class);
            when(platform.getDataFolder()).thenReturn(directory.toFile());
            storage = new BungeeJsonVoteCache(platform) {
                @Override public synchronized void saveDurably() throws java.io.IOException {
                    if (failCacheDurableSave) throw new java.io.IOException("synthetic cache save failure");
                    super.saveDurably();
                }
                @Override public synchronized void reload() {
                    if (failCacheReload) throw new java.io.UncheckedIOException(
                            new java.io.IOException("synthetic cache rollback reload failure"));
                    super.reload();
                }
            };
            cache = new VoteCacheHandler(null, false, false, null, false, storage) {
                @Override public boolean addOnlineVoteDurably(String uuid, OfflineBungeeVote vote) {
                    boolean result = super.addOnlineVoteDurably(uuid, vote);
                    if (result && failAfterOnlineInsert) {
                        failAfterOnlineInsert = false;
                        throw new IllegalStateException("synthetic failure after durable owner insertion");
                    }
                    return result;
                }
                @Override public boolean updateTimeVote(com.bencodez.votingplugin.timequeue.VoteTimeQueue vote) {
                    if (failingTimeWrites > 0) { failingTimeWrites--; return false; }
                    return super.updateTimeVote(vote);
                }
                @Override public void logInfo1(String message) { }
                @Override public void logSevere1(String message) { }
                @Override public void debug1(Exception failure) { }
                @Override public void debug1(Throwable failure) { }
                @Override public void debug1(String message) { }
            };
            outbox = new ReliableVoteDeliveryOutboxAccess(directory.resolve("outbox.dat"));
            setField(this, "voteCacheHandler", cache);
            setField(this, "reliableVoteDeliveryOutbox", outbox);
            GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
            doAnswer(call -> {
                sendPluginMessageServer(call.getArgument(0), call.getArgument(1), call.getArgument(2));
                return null;
            }).when(messages).sendMessage(anyString(), anyInt(), any(JsonEnvelope.class));
            setGlobalMessageProxyHandlerForTest(messages);
        }

        @Override public void addVoteParty() {
            if (failVoteParty) throw new IllegalStateException("synthetic early effect failure");
            super.addVoteParty();
        }

        @Override public int getVoteCacheVotePartyIncreaseVotesRequired() {
            return persistPartyState ? storage.getVotePartyInreaseVotesRequired()
                    : super.getVoteCacheVotePartyIncreaseVotesRequired();
        }
        @Override public void setVoteCacheVotePartyCurrentVotes(int votes) {
            if (persistPartyState) storage.setVotePartyCurrentVotes(votes);
            else super.setVoteCacheVotePartyCurrentVotes(votes);
        }
        @Override public void setVoteCacheVotePartyIncreaseVotesRequired(int votes) {
            if (persistPartyState) storage.setVotePartyInreaseVotesRequired(votes);
            else super.setVoteCacheVotePartyIncreaseVotesRequired(votes);
        }

        @Override public void saveVotePartyStateDurably() {
            if (failDurablePartySave) throw new IllegalStateException("synthetic durable write failure");
            super.saveVotePartyStateDurably();
            if (persistPartyState) {
                storage.setVotePartyCurrentVotes(getVotePartyVotes());
                storage.setVotePartyInreaseVotesRequired(getVoteCacheVotePartyIncreaseVotesRequired());
                try {
                    com.bencodez.votingplugin.proxy.cache.VotePartyCacheDurability.saveAndVerify(
                            storage.getStoragePath(), storage);
                } catch (java.io.IOException failure) {
                    throw new IllegalStateException("synthetic durable party transaction failed", failure);
                }
            }
        }

        @Override protected long pluginMessageNanoTime() { return time.now; }
        @Override protected boolean canForwardStandaloneBroadcast(boolean managesTotals) {
            return broadcastValidationEnabled || super.canForwardStandaloneBroadcast(managesTotals);
        }
        @Override public ScheduledExecutorService getScheduler() { return time.executor; }
        @Override public Set<String> getAllAvailableServers() { return new LinkedHashSet<>(availableTargets); }
        @Override public boolean isSomeoneOnlineServer(String server) { return carrier && !unavailableBackends.contains(server); }
        @Override public String getUUID(String player) {
            return resolvedUuids.containsKey(player) ? resolvedUuids.get(player) : super.getUUID(player);
        }
        @Override public String getCurrentPlayerServer(String player) { return playerRoutes.getOrDefault(player, playerServer); }
        @Override public boolean isPlayerOnline(String player) { return !offlinePlayers.contains(player) && super.isPlayerOnline(player); }
        @Override public boolean sendPluginMessageData(String server, String channel, byte[] data, boolean queue) {
            assertFalse(queue);
            transportAttempts++;
            if (!isSomeoneOnlineServer(server) || !deliveryAccepted) return false;
            transportDepth++;
            maxTransportDepth = Math.max(maxTransportDepth, transportDepth);
            try (DataInputStream input = new DataInputStream(new ByteArrayInputStream(data))) {
                String subChannel = input.readUTF();
                int payloadLength = input.readInt();
                String payload = input.readUTF();
                assertEquals(payload.getBytes(StandardCharsets.UTF_8).length, payloadLength);
                JsonEnvelope envelope = JsonEnvelopeCodec.decode(payload);
                assertEquals(subChannel, envelope.getSubChannel());
                handoffs.add(new Handoff(time.now, server, envelope));
                onHandoff.accept(envelope);
                return true;
            } catch (java.io.IOException failure) {
                throw new AssertionError(failure);
            } finally {
                transportDepth--;
            }
        }

        @SuppressWarnings("unchecked") Set<String> legacy() throws Exception {
            return (Set<String>) field(this, "legacyVoteDeliveryServers");
        }
        @SuppressWarnings("unchecked") Set<String> reliable() throws Exception {
            return (Set<String>) field(this, "reliableVoteDeliveryServers");
        }
        void retry() { invoke(this, "retryReliableVoteDeliveries", String.class, BACKEND); }
        void closeDispatcher() throws Exception { (PluginMessageDispatcherAccess.view(field(this, "pluginMessageDispatcher"))).close(); }
    }

    static final class ManualTime {
        long now;
        final ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
        private final PriorityQueue<Wakeup> wakes = new PriorityQueue<>(Comparator.comparingLong(Wakeup::due)
                .thenComparingLong(Wakeup::order));
        private long sequence;

        ManualTime() {
            when(executor.schedule(any(Runnable.class), anyLong(), any(TimeUnit.class))).thenAnswer(call -> {
                ScheduledFuture<?> future = mock(ScheduledFuture.class);
                AtomicBoolean cancelled = new AtomicBoolean();
                AtomicBoolean done = new AtomicBoolean();
                when(future.cancel(false)).thenAnswer(ignored -> { cancelled.set(true); return true; });
                when(future.isCancelled()).thenAnswer(ignored -> cancelled.get());
                when(future.isDone()).thenAnswer(ignored -> done.get());
                TimeUnit unit = call.getArgument(2);
                wakes.add(new Wakeup(now + unit.toNanos(call.getArgument(1, Long.class)), ++sequence,
                        call.getArgument(0), future, done));
                return future;
            });
        }

        void advance(long nanos) {
            long target = now + nanos;
            int ran = 0;
            while (!wakes.isEmpty() && wakes.peek().due() <= target) {
                Wakeup wake = wakes.remove();
                if (wake.future().isCancelled()) continue;
                now = Math.max(now, wake.due());
                wake.done().set(true);
                wake.action().run();
                assertTrue(++ran < 10000, "unbounded scheduler loop");
            }
            now = Math.max(now, target);
        }

        void stall(long nanos) { now += nanos; advance(0); }
        List<Runnable> callbacks() { return wakes.stream().map(Wakeup::action).toList(); }
    }

    private record Wakeup(long due, long order, Runnable action, ScheduledFuture<?> future, AtomicBoolean done) { }

    static Object field(Object target, String name) throws ReflectiveOperationException {
        Field field = VotingPluginProxy.class.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(target);
    }

    static void setField(Object target, String name, Object value) throws ReflectiveOperationException {
        Field field = VotingPluginProxy.class.getDeclaredField(name);
        field.setAccessible(true);
        ReflectiveAccess.set(field, target, value);
    }

    private static void invoke(Object target, String name, Class<?> type, Object value) {
        try {
            Method method = VotingPluginProxy.class.getDeclaredMethod(name, type);
            method.setAccessible(true);
            ReflectiveAccess.invoke(method, target, new Object[]{value});
        } catch (InvocationTargetException failure) {
            throw new AssertionError(failure.getCause());
        } catch (ReflectiveOperationException failure) {
            throw new AssertionError(failure);
        }
    }
}
