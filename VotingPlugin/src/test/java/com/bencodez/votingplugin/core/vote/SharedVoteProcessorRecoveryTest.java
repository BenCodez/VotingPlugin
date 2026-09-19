package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/** Restart and acknowledgement boundaries with test-only file transaction/replay owners. */
@Timeout(15)
class SharedVoteProcessorRecoveryTest {
    @TempDir Path directory;
    private static final UUID USER = UUID.fromString("9837d441-a4d6-461f-aa86-17958c01bc8c");
    private static final SharedVotePolicy POLICY = new SharedVotePolicy(false, true, true, true, true);

    @Test void crashAfterCommitRecoversPendingRewardWithoutAnotherIngressVote() {
        SharedVoteTestStore store = store();
        store.failAfterCommit = true;
        SharedVoteInput input = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(input, POLICY).toCompletableFuture().join());
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 5), store.load(USER).toCompletableFuture().join());
        assertTrue(store.events().isEmpty());
        SharedVoteTestStore reopened = store();
        SharedVoteProcessor recovered = new SharedVoteProcessor(vote -> { throw new AssertionError("Do not resolve a committed vote again"); }, reopened, reopened);
        assertEquals(1, recovered.recoverPending(10).toCompletableFuture().join().size());
        assertEquals(2, reopened.events().size());
        assertEquals(1, reopened.load(USER).toCompletableFuture().join().allTimeTotal());
        assertTrue(reopened.pendingVotes(10).toCompletableFuture().join().isEmpty());
    }

    @Test void lostCommitAckRetryUsesOriginalIdentityPolicyAndSnapshot() {
        SharedVoteTestStore store = store();
        store.failAfterCommit = true;
        SharedVoteInput input = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(input, POLICY).toCompletableFuture().join());
        SharedVoteTestStore reopened = store();
        SharedVoteProcessor retry = new SharedVoteProcessor(vote -> { throw new AssertionError("Do not re-resolve"); }, reopened, reopened);
        SharedVoteProcessingResult result = retry.process(input,
                new SharedVotePolicy(false, false, false, false, false)).toCompletableFuture().join();
        assertEquals(RewardDisposition.EXECUTED, result.rewardDisposition());
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 5), result.persistedState());
    }

    @Test void failureBeforeCommitCreatesNeitherMutationNorRewardWork() {
        SharedVoteTestStore store = store();
        store.failBeforeCommit = true;
        SharedVoteInput input = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(input, POLICY).toCompletableFuture().join());
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 0), store.load(USER).toCompletableFuture().join());
        assertEquals(null, store.findVote(input.voteId()).toCompletableFuture().join());
        assertTrue(store.events().isEmpty());
        runtime(store).process(input, POLICY).toCompletableFuture().join();
        assertEquals(1, store.mutationCount);
    }

    @Test void failedRewardDeliveryRemainsPendingForRestart() {
        SharedVoteTestStore store = store();
        store.failBeforeDelivery = true;
        SharedVoteInput input = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(input, POLICY).toCompletableFuture().join());
        assertTrue(store.findVote(input.voteId()).toCompletableFuture().join().pending());
        SharedVoteTestStore reopened = store();
        runtime(reopened).recover(input.voteId()).toCompletableFuture().join();
        assertEquals(2, reopened.events().size());
        assertEquals(1, reopened.load(USER).toCompletableFuture().join().allTimeTotal());
    }

    @Test void lostRewardOwnerAcknowledgementDoesNotRepeatCompletedEffects() {
        lostAck(true, false, false);
    }
    @Test void failedReceiptAcknowledgementDoesNotRepeatCompletedEffects() {
        lostAck(false, true, false);
    }
    @Test void lostTerminalReceiptAcknowledgementDoesNotRepeatCompletedEffects() {
        lostAck(false, false, true);
    }
    private void lostAck(boolean reward, boolean beforeMark, boolean afterMark) {
        SharedVoteTestStore store = store();
        store.failAfterDelivery = reward;
        store.failBeforeMark = beforeMark;
        store.failAfterMark = afterMark;
        SharedVoteInput input = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(input, POLICY).toCompletableFuture().join());
        assertEquals(2, store.events().size());
        SharedVoteTestStore reopened = store();
        SharedVoteProcessingResult result = runtime(reopened).process(input, POLICY).toCompletableFuture().join();
        assertEquals(RewardDisposition.EXECUTED, result.rewardDisposition());
        assertEquals(2, reopened.events().size());
        assertEquals(1, reopened.load(USER).toCompletableFuture().join().allTimeTotal());
    }

    @Test void duplicateVoteIdWithDifferentInputIsRejectedBeforeAnotherMutation() {
        SharedVoteTestStore store = store();
        SharedVoteInput input = input();
        runtime(store).process(input, POLICY).toCompletableFuture().join();
        SharedVoteInput conflict = new SharedVoteInput(input.voteId(), "Other", "Example", input.voteTime(), true, true, false, true);
        assertThrows(CompletionException.class, () -> runtime(store).process(conflict, POLICY).toCompletableFuture().join());
        assertEquals(1, store.mutationCount);
        assertEquals(2, store.events().size());
    }

    @Test void concurrentDuplicateSubmissionsShareThePersistenceAndRewardOwners() throws Exception {
        SharedVoteTestStore store = store();
        SharedVoteInput input = input();
        var workers = Executors.newFixedThreadPool(4);
        try {
            List<java.util.concurrent.Future<?>> tasks = new ArrayList<>();
            for (int i = 0; i < 12; i++) tasks.add(workers.submit(() -> runtime(store).process(input, POLICY).toCompletableFuture().join()));
            for (var task : tasks) task.get(5, TimeUnit.SECONDS);
            assertEquals(1, store.mutationCount);
            assertEquals(2, store.events().size());
        } finally {
            workers.shutdownNow();
            assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void unacknowledgedAtomicPersistenceDoesNotTriggerTheOriginatingRewardCall() {
        SharedVoteTestStore store = store();
        store.commitAck = new CompletableFuture<>();
        var result = runtime(store).process(input(), POLICY).toCompletableFuture();
        assertFalse(result.isDone());
        assertTrue(store.events().isEmpty());
        store.commitAck.complete(null);
        assertEquals(RewardDisposition.EXECUTED, result.join().rewardDisposition());
    }

    @Test void offlineHandoffIsIdempotentAcrossRestartAndLostAcknowledgement() {
        SharedVoteTestStore store = store();
        store.failBeforeMark = true;
        SharedVoteInput input = input();
        SharedVoteProcessor offline = new SharedVoteProcessor(vote -> CompletableFuture.completedFuture(new SharedVoteIdentity(USER, "Ben", false)), store, store);
        SharedVotePolicy defer = new SharedVotePolicy(false, true, true, true, false);
        assertThrows(CompletionException.class, () -> offline.process(input, defer).toCompletableFuture().join());
        SharedVoteTestStore reopened = store();
        assertEquals(RewardDisposition.DEFERRED, runtime(reopened).recover(input.voteId()).toCompletableFuture().join().rewardDisposition());
        assertEquals(List.of("defer:Example:1"), reopened.events());
    }

    @Test void recoveryRejectsInvalidBoundsWithoutScanningStorage() {
        SharedVoteTestStore store = store();
        assertThrows(IllegalArgumentException.class, () -> runtime(store).recoverPending(0));
        assertThrows(IllegalArgumentException.class, () -> runtime(store).recoverPending(101));
    }

    @Test void aFailedRecoveryEntryDoesNotBlockOtherPendingEntries() {
        SharedVoteTestStore store = store();
        store.failBeforeDelivery = true;
        SharedVoteInput first = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(first, POLICY).toCompletableFuture().join());
        store.failBeforeDelivery = true;
        SharedVoteInput second = input();
        assertThrows(CompletionException.class, () -> runtime(store).process(second, POLICY).toCompletableFuture().join());
        store.failBeforeDelivery = true;
        assertThrows(CompletionException.class, () -> runtime(store).recoverPending(10).toCompletableFuture().join());
        assertEquals(1, store.pendingVotes(10).toCompletableFuture().join().size());
        assertEquals(2, store.events().size());
    }

    private SharedVoteTestStore store() { return new SharedVoteTestStore(directory.resolve("users.properties"), 5); }
    private static SharedVoteProcessor runtime(SharedVoteTestStore store) {
        return new SharedVoteProcessor(vote -> CompletableFuture.completedFuture(new SharedVoteIdentity(USER, "Ben", true)), store, store);
    }
    private static SharedVoteInput input() { return new SharedVoteInput(UUID.randomUUID(), "Ben", "Example", 1000, true, true, false, true); }
}
