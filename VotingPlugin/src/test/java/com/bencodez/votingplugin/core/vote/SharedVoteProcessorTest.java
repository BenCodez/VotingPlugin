package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

class SharedVoteProcessorTest {
    private static final UUID PLAYER = UUID.fromString("9837d441-a4d6-461f-aa86-17958c01bc8c");
    private static final SharedVotePolicy POLICY = new SharedVotePolicy(false, true, true, true, true);

    @Test
    void persistsThenExecutesUsingCurrentRewardDefinition() {
        RecordingAdapters adapters = new RecordingAdapters();
        adapters.rewardMessage = "first";
        SharedVoteProcessingResult first = processor(adapters, true).process(input(true, true, false, false), POLICY)
                .toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 5), first.persistedState());
        assertEquals(RewardDisposition.EXECUTED, first.rewardDisposition());
        assertEquals(List.of("persist", "reward:first:1"), adapters.events);

        adapters.rewardMessage = "updated";
        SharedVoteProcessingResult second = processor(adapters, true).process(input(true, true, false, false), POLICY)
                .toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(2, 2, 2, 2, 10), second.persistedState());
        assertEquals(List.of("persist", "reward:first:1", "persist", "reward:updated:2"), adapters.events);
    }

    @Test
    void offlineNativeVoteUsesExistingDeferredRewardPath() {
        RecordingAdapters adapters = new RecordingAdapters();
        SharedVotePolicy policy = new SharedVotePolicy(false, true, true, true, false);
        SharedVoteProcessingResult result = processor(adapters, false).process(input(true, true, false, false), policy)
                .toCompletableFuture().join();
        assertEquals(RewardDisposition.DEFERRED, result.rewardDisposition());
        assertEquals(List.of("persist", "defer:ExampleSite:1"), adapters.events);
    }

    @Test
    void proxyOriginUsesHistoricalOnlineForDeliveryAndCurrentOnlineForTotals() {
        RecordingAdapters adapters = new RecordingAdapters();
        SharedVotePolicy policy = new SharedVotePolicy(false, true, false, false, false);
        SharedVoteInput proxy = input(true, true, true, true);
        SharedVoteProcessingResult result = processor(adapters, false).process(proxy, policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 5), result.persistedState());
        assertEquals(RewardDisposition.EXECUTED, result.rewardDisposition());
        assertEquals(List.of("persist", "reward:configured:0"), adapters.events);
    }

    @Test
    void fakeVoteAndAddTotalsRulesStillAwardPointsIndependentlyOfTotals() {
        RecordingAdapters adapters = new RecordingAdapters();
        SharedVotePolicy policy = new SharedVotePolicy(true, false, false, true, true);
        SharedVoteProcessingResult result = processor(adapters, true).process(input(false, true, false, false), policy)
                .toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 5), result.persistedState());
        processor(adapters, true).process(input(true, false, false, false), policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 5), adapters.state);
    }

    @Test
    void normalizesZeroTimeAndKeepsProxyFlagsSeparate() {
        RecordingAdapters adapters = new RecordingAdapters();
        SharedVoteInput input = new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 0,
                true, true, true, false, false);
        processor(adapters, true).process(input, POLICY).toCompletableFuture().join();
        assertTrue(adapters.lastMutation.voteTime() > 0);
        assertTrue(adapters.lastInput.proxyVote());
        assertFalse(adapters.lastInput.forceProxyRouting());
    }

    @Test
    void failedOrUnfinishedPersistenceDoesNotStartRewardDelivery() {
        RecordingAdapters adapters = new RecordingAdapters();
        adapters.persistence = new CompletableFuture<>();
        CompletionStage<SharedVoteProcessingResult> pending = processor(adapters, true).process(
                input(true, true, false, false), POLICY);
        assertEquals(List.of("persist"), adapters.events);
        assertFalse(pending.toCompletableFuture().isDone());
        adapters.persistence.completeExceptionally(new IllegalStateException("storage failed"));
        CompletionException failure = assertThrows(CompletionException.class, () -> pending.toCompletableFuture().join());
        assertEquals("storage failed", failure.getCause().getMessage());
        assertEquals(List.of("persist"), adapters.events);
    }

    @Test
    void rewardFailurePropagatesAfterPersistence() {
        RecordingAdapters adapters = new RecordingAdapters();
        adapters.failReward = true;
        CompletionException failure = assertThrows(CompletionException.class,
                () -> processor(adapters, true).process(input(true, true, false, false), POLICY).toCompletableFuture().join());
        assertEquals("reward failed", failure.getCause().getMessage());
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 5), adapters.state);
    }

    private static SharedVoteInput input(boolean real, boolean addTotals, boolean proxy, boolean wasOnline) {
        return new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 1000L,
                real, addTotals, proxy, proxy, wasOnline);
    }

    private static SharedVoteProcessor processor(RecordingAdapters adapters, boolean online) {
        return new SharedVoteProcessor(input -> {
            adapters.lastInput = input;
            return CompletableFuture.completedFuture(new SharedVoteIdentity(PLAYER, "Ben", online));
        }, adapters, adapters);
    }

    private static class RecordingAdapters implements SharedVoteUserServices, SharedVoteRewardServices {
        SharedVoteUserSnapshot state = new SharedVoteUserSnapshot(0, 0, 0, 0, 0);
        SharedVoteMutation lastMutation;
        SharedVoteInput lastInput;
        String rewardMessage = "configured";
        boolean failReward;
        CompletableFuture<SharedVoteUserSnapshot> persistence;
        final List<String> events = new ArrayList<>();

        @Override public CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity,
                SharedVoteMutation mutation) {
            events.add("persist");
            lastMutation = mutation;
            if (persistence != null) return persistence;
            int total = mutation.countTotals() ? 1 : 0;
            state = new SharedVoteUserSnapshot(state.allTimeTotal() + total, state.monthTotal() + total,
                    state.weeklyTotal() + total, state.dailyTotal() + total,
                    state.points() + (mutation.awardConfiguredPoints() ? 5 : 0));
            return CompletableFuture.completedFuture(state);
        }

        @Override public CompletionStage<SharedVoteUserSnapshot> load(UUID uuid) {
            return CompletableFuture.completedFuture(state);
        }

        @Override public CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                SharedVoteUserSnapshot snapshot) {
            if (failReward) return CompletableFuture.failedFuture(new IllegalStateException("reward failed"));
            events.add("reward:" + rewardMessage + ":" + snapshot.allTimeTotal());
            return CompletableFuture.completedFuture(null);
        }

        @Override public CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                SharedVoteUserSnapshot snapshot) {
            events.add("defer:" + input.serviceSite() + ":" + snapshot.allTimeTotal());
            return CompletableFuture.completedFuture(null);
        }
    }
}
