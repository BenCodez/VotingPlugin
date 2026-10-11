package com.bencodez.votingplugin.proxy;

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
        assertEquals(0, new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat")).size());
        assertPaced(proxy.handoffs);
    }

    @Test
    void fullReliableWindowAllowsReceiptsAndReleasesCapacityAfterDurableCompletion(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i <= PluginMessageDispatcher.MAX_IN_FLIGHT; i++)
            assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(100 + i)));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT + 1, proxy.outbox.size());

        UUID completed = new UUID(2, 100);
        invoke(proxy, "handleVoteDeliveryAcknowledgement", JsonEnvelope.class,
                VotingPluginWire.voteDeliveryAcknowledgement(BACKEND, completed, VotingPluginWire.SUB_VOTE));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT + 1, rewardHandoffs(proxy).size());
        assertTrue(proxy.handoffs.stream().anyMatch(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                .equals(handoff.envelope().getSubChannel())));
        assertTrue(proxy.outbox.find(BACKEND, completed, VotingPluginWire.SUB_VOTE).orElseThrow().awaitingReceiptRelease());
        assertPaced(proxy.handoffs);
    }

    @Test
    void outstandingReceiptsDoNotConsumeTheRewardWindow(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i < PluginMessageDispatcher.MAX_IN_FLIGHT + 1; i++) {
            assertTrue(proxy.outbox.offer(BACKEND, vote(200 + i)));
            assertTrue(proxy.outbox.acknowledgeCompletion(BACKEND, new UUID(2, 200 + i), VotingPluginWire.SUB_VOTE));
        }
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(300)));
        proxy.retry();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertTrue(rewardHandoffs(proxy).stream().anyMatch(handoff -> new UUID(2, 300).toString()
                .equals(handoff.envelope().getFields().get(VotingPluginWire.K_VOTE_ID))));
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT + 1, proxy.handoffs.stream()
                .filter(handoff -> VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE
                        .equals(handoff.envelope().getSubChannel())).count());
        assertPaced(proxy.handoffs);
    }

    @Test
    void durableLegacyRetirementReleasesTheFullWindowBeforeReliableDeliveryReturns(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.reliable().add(BACKEND);
        for (int i = 0; i < PluginMessageDispatcher.MAX_IN_FLIGHT; i++)
            assertTrue(proxy.sendVoteEnvelopeAcceptedForTest(BACKEND, 1, vote(600 + i)));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(8));
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());
        assertEquals(PluginMessageDispatcher.MAX_IN_FLIGHT, proxy.outbox.size());

        proxy.reliable().remove(BACKEND);
        proxy.legacy().add(BACKEND);
        proxy.retry();
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(0, proxy.outbox.size());
        assertEquals(0, new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat")).size());
        assertEquals(2 * PluginMessageDispatcher.MAX_IN_FLIGHT, rewardHandoffs(proxy).size());

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
        assertEquals(0, new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat")).size());

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
    void fullTargetLaneCannotPartiallyAdmitAnUpdateBatchBeforeTheVoteRetries(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy(directory);
        proxy.legacy().addAll(List.of(BACKEND, "backend-b"));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        for (int i = 0; i < PluginMessageDispatcher.MAX_LANE_CONTROLS; i++)
            proxy.sendPluginMessageServer("backend-b", 0, control("occupied"));
        UUID id = new UUID(2, 500);
        String player = new UUID(1, 500).toString();

        assertThrows(VotingPluginProxy.VoteRetryException.class,
                () -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(6));
        assertEquals(0, updateHandoffs(proxy).size(), "a rejected batch admitted an earlier target");
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel())).count());

        assertDoesNotThrow(() -> proxy.vote("Synthetic", "synthetic-service", true, false, 1L, null, player, id));
        proxy.time.advance(TimeUnit.SECONDS.toNanos(1));
        assertEquals(List.of(BACKEND, "backend-b"), updateHandoffs(proxy).stream()
                .map(Handoff::server).sorted().toList());
        assertEquals(1, proxy.handoffs.stream().filter(handoff -> VotingPluginWire.SUB_VOTE_ONLINE
                .equals(handoff.envelope().getSubChannel())).count(), "retry repeated the accepted reward");
        assertPaced(proxy.handoffs);
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
        final ReliableVoteDeliveryOutbox outbox;
        boolean carrier = true;
        boolean failDurablePartySave;
        boolean deliveryAccepted = true;
        String playerServer = BACKEND;
        final Map<String, String> playerRoutes = new HashMap<>();
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
            BungeeJsonVoteCache storage = new BungeeJsonVoteCache(platform);
            cache = new VoteCacheHandler(null, false, false, null, false, storage) {
                @Override public void logInfo1(String message) { }
                @Override public void logSevere1(String message) { }
                @Override public void debug1(Exception failure) { }
                @Override public void debug1(Throwable failure) { }
                @Override public void debug1(String message) { }
            };
            outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
            setField(this, "voteCacheHandler", cache);
            setField(this, "reliableVoteDeliveryOutbox", outbox);
            GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
            doAnswer(call -> {
                sendPluginMessageServer(call.getArgument(0), call.getArgument(1), call.getArgument(2));
                return null;
            }).when(messages).sendMessage(anyString(), anyInt(), any(JsonEnvelope.class));
            setGlobalMessageProxyHandlerForTest(messages);
        }

        @Override public void saveVotePartyStateDurably() {
            if (failDurablePartySave) throw new IllegalStateException("synthetic durable write failure");
            super.saveVotePartyStateDurably();
        }

        @Override protected long pluginMessageNanoTime() { return time.now; }
        @Override public ScheduledExecutorService getScheduler() { return time.executor; }
        @Override public Set<String> getAllAvailableServers() { return new LinkedHashSet<>(List.of(BACKEND, "backend-b")); }
        @Override public boolean isSomeoneOnlineServer(String server) { return carrier; }
        @Override public String getCurrentPlayerServer(String player) { return playerRoutes.getOrDefault(player, playerServer); }
        @Override public boolean isPlayerOnline(String player) { return !offlinePlayers.contains(player) && super.isPlayerOnline(player); }
        @Override public boolean sendPluginMessageData(String server, String channel, byte[] data, boolean queue) {
            assertFalse(queue);
            transportAttempts++;
            if (!carrier || !deliveryAccepted) return false;
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
        void closeDispatcher() throws Exception { ((PluginMessageDispatcher) field(this, "pluginMessageDispatcher")).close(); }
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
        field.set(target, value);
    }

    private static void invoke(Object target, String name, Class<?> type, Object value) {
        try {
            Method method = VotingPluginProxy.class.getDeclaredMethod(name, type);
            method.setAccessible(true);
            method.invoke(target, value);
        } catch (InvocationTargetException failure) {
            throw new AssertionError(failure.getCause());
        } catch (ReflectiveOperationException failure) {
            throw new AssertionError(failure);
        }
    }
}
