package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SharedVoteReviewFollowupTest {
    @TempDir Path directory;
    private static final UUID USER = UUID.fromString("9837d441-a4d6-461f-aa86-17958c01bc8c");
    private static final SharedVotePolicy POLICY = new SharedVotePolicy(false, true, true, true, true);

    @Test
    void zeroTimestampIsNormalizedOnceAndZeroSentinelRetryMatchesTheReceipt() {
        SharedVoteTestStore store = new SharedVoteTestStore(directory.resolve("zero.properties"), 5);
        SharedVoteProcessor processor = runtime(store);
        UUID voteId = UUID.randomUUID();
        SharedVoteInput sentinel = new SharedVoteInput(voteId, "Ben", "Example", 0, true, true, false, true);
        processor.process(sentinel, POLICY).toCompletableFuture().join();
        SharedVoteReceipt receipt = store.findVote(voteId).toCompletableFuture().join();
        assertTrue(receipt.input().voteTime() > 0);
        long normalized = receipt.input().voteTime();
        processor.process(sentinel, POLICY).toCompletableFuture().join();
        assertEquals(1, store.mutationCount);
        assertEquals(normalized, store.findVote(voteId).toCompletableFuture().join().input().voteTime());
    }

    @Test
    void concurrentZeroSentinelCandidatesUseTheFirstAtomicReceipt() throws Exception {
        SharedVoteTestStore store = new SharedVoteTestStore(directory.resolve("zero-race.properties"), 5);
        UUID voteId = UUID.randomUUID();
        SharedVoteInput sentinel = new SharedVoteInput(voteId, "Ben", "Example", 0, true, true, false, true);
        SharedVoteIdentity identity = new SharedVoteIdentity(USER, "Ben", true);
        SharedVoteRewardPlan firstPlan = new SharedVoteRewardPlan("vote-site:Example", "v1");
        SharedVoteRewardPlan losingPlan = new SharedVoteRewardPlan("vote-site:Example", "v2");
        CountDownLatch ready = new CountDownLatch(2);
        CountDownLatch start = new CountDownLatch(1);
        var workers = Executors.newFixedThreadPool(2);
        try {
            CompletableFuture<SharedVoteReceipt> firstAttempt = CompletableFuture.supplyAsync(() -> {
                awaitStart(ready, start);
                return store.persistVoteWithReward(sentinel, identity,
                        new SharedVoteMutation(voteId, "Example", 1001, true, true), true, firstPlan)
                        .toCompletableFuture().join();
            }, workers);
            CompletableFuture<SharedVoteReceipt> retryAttempt = CompletableFuture.supplyAsync(() -> {
                awaitStart(ready, start);
                return store.persistVoteWithReward(sentinel, identity,
                        new SharedVoteMutation(voteId, "Example", 2002, true, true), true, losingPlan)
                        .toCompletableFuture().join();
            }, workers);
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();
            SharedVoteReceipt first = firstAttempt.join();
            SharedVoteReceipt retry = retryAttempt.join();
            assertEquals(first, retry);
            SharedVoteRewardPlan winningPlan = first.input().voteTime() == 1001 ? firstPlan : losingPlan;
            assertEquals(winningPlan, first.rewardPlan());
            assertTrue(first.input().voteTime() == 1001 || first.input().voteTime() == 2002);
            assertEquals(1, store.mutationCount);
        } finally {
            start.countDown();
            workers.shutdownNow();
        }
    }

    @Test
    void restartUsesThePersistedPreparedRewardVersionInsteadOfCurrentConfiguration() {
        Path file = directory.resolve("plan.properties");
        SharedVoteTestStore first = new SharedVoteTestStore(file, 5);
        first.preparedPlanVersion = "config-v1";
        first.failAfterCommit = true;
        SharedVoteInput input = new SharedVoteInput(UUID.randomUUID(), "Ben", "Example", 1000, true, true, false, true);
        assertThrows(CompletionException.class, () -> runtime(first).process(input, POLICY).toCompletableFuture().join());
        assertEquals("config-v1", first.findVote(input.voteId()).toCompletableFuture().join().rewardPlan().versionReference());

        SharedVoteTestStore reopened = new SharedVoteTestStore(file, 5);
        reopened.preparedPlanVersion = "config-v2";
        runtime(reopened).recover(input.voteId()).toCompletableFuture().join();
        assertEquals("config-v1", reopened.lastDeliveredPlanVersion);
        assertEquals("config-v1", reopened.findVote(input.voteId()).toCompletableFuture().join().rewardPlan().versionReference());
    }

    @Test
    void proxyOriginAndForcedRoutingRemainIndependentAcrossPersistence() {
        SharedVoteTestStore store = new SharedVoteTestStore(directory.resolve("proxy-flags.properties"), 5);
        UUID voteId = UUID.randomUUID();
        SharedVoteInput input = new SharedVoteInput(voteId, "Ben", "Example", 1234,
                true, true, true, false, true);
        SharedVoteIdentity identity = new SharedVoteIdentity(USER, "Ben", true);
        SharedVoteReceipt receipt = store.persistVoteWithReward(input, identity,
                new SharedVoteMutation(voteId, "Example", 1234, true, true), true,
                new SharedVoteRewardPlan("vote-site:Example", "v1")).toCompletableFuture().join();
        assertTrue(receipt.input().proxyVote());
        assertFalse(receipt.input().forceProxyRouting());
        SharedVoteReceipt reopened = new SharedVoteTestStore(store.file, 5).findVote(voteId).toCompletableFuture().join();
        assertTrue(reopened.input().proxyVote());
        assertFalse(reopened.input().forceProxyRouting());
    }

    private static SharedVoteProcessor runtime(SharedVoteTestStore store) {
        return new SharedVoteProcessor(
                vote -> CompletableFuture.completedFuture(new SharedVoteIdentity(USER, "Ben", true)), store, store);
    }

    private static void awaitStart(CountDownLatch ready, CountDownLatch start) {
        ready.countDown();
        try {
            if (!start.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("Timed out awaiting concurrent start");
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted awaiting concurrent start", interrupted);
        }
    }
}
