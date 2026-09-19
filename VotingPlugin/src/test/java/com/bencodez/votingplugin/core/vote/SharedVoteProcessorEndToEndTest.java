package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SharedVoteProcessorEndToEndTest {
    @TempDir Path tempDir;

    @Test void votePersistsThenExecutesCommandAndMessageAcrossRestartAndOfflineVote() {
        UUID uuid = UUID.randomUUID();
        Path storeFile = tempDir.resolve("users.properties");
        ArrayList<String> rewards = new ArrayList<>();
        SharedVotePolicy policy = new SharedVotePolicy(false, true, true, true, true);
        SharedVoteTestStore firstStore = new SharedVoteTestStore(storeFile, 10);
        SharedVoteProcessor firstRuntime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", true)), firstStore, recording(firstStore, rewards));
        SharedVoteProcessingResult first = firstRuntime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 1000L, true, true, false, true), policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 10), first.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.EXECUTED, first.rewardDisposition());
        assertTrue(Files.isRegularFile(storeFile));
        assertEquals(List.of("command:say Thanks Ben:1", "message:Thanks Ben:10"), rewards);

        SharedVoteTestStore restartedStore = new SharedVoteTestStore(storeFile, 10);
        assertEquals(first.persistedState(), restartedStore.load(uuid).toCompletableFuture().join());
        SharedVoteProcessor restartedRuntime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", false)), restartedStore, recording(restartedStore, rewards));
        SharedVoteProcessingResult second = restartedRuntime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 2000L, true, true, false, false), policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(2, 2, 2, 2, 20), second.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.EXECUTED, second.rewardDisposition());
        assertEquals(List.of("command:say Thanks Ben:1", "message:Thanks Ben:10",
                "command:say Thanks Ben:2", "message:Thanks Ben:20"), rewards);
    }

    @Test void offlineIneligibleRewardIsDurablyDelegatedAfterPersistence() {
        UUID uuid = UUID.randomUUID();
        ArrayList<String> rewards = new ArrayList<>();
        SharedVoteTestStore store = new SharedVoteTestStore(tempDir.resolve("deferred.properties"), 3);
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", false)), store, recording(store, rewards));
        SharedVoteProcessingResult result = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 3000L, true, true, false, false),
                new SharedVotePolicy(false, true, true, true, false)).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 3), result.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.DEFERRED, result.rewardDisposition());
        assertEquals(List.of("defer:ExampleSite:1"), rewards);
    }

    @Test void proxyTotalsUseLiveStateInsteadOfHistoricalWasOnline() {
        UUID uuid = UUID.randomUUID();
        SharedVotePolicy policy = new SharedVotePolicy(false, true, false, true, true);
        SharedVoteTestStore offlineStore = new SharedVoteTestStore(tempDir.resolve("proxy-offline.properties"), 2);
        SharedVoteProcessingResult offline = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", false)), offlineStore, recording(offlineStore, new ArrayList<>())).process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 3100L, true, true, true, true), policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 2), offline.persistedState());
        SharedVoteTestStore onlineStore = new SharedVoteTestStore(tempDir.resolve("proxy-online.properties"), 2);
        SharedVoteProcessingResult online = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", true)), onlineStore, recording(onlineStore, new ArrayList<>())).process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 3200L, true, true, true, false), policy).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 2), online.persistedState());
    }

    @Test void persistenceFailurePreventsRewardExecution() {
        ArrayList<String> rewards = new ArrayList<>();
        SharedVoteTestStore failing = new SharedVoteTestStore(tempDir.resolve("failure.properties"), 1) {
            @Override public CompletionStage<SharedVoteReceipt> persistVoteWithReward(SharedVoteInput input,
                    SharedVoteIdentity identity, SharedVoteMutation mutation, boolean execute, SharedVoteRewardPlan rewardPlan) {
                return CompletableFuture.failedFuture(new IllegalStateException("storage failed"));
            }
        };
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(UUID.randomUUID(), "Ben", true)), failing, recording(failing, rewards));
        CompletionException failure = assertThrows(CompletionException.class, () -> runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 4000L, true, true, false, true),
                new SharedVotePolicy(false, true, true, true, true)).toCompletableFuture().join());
        assertEquals("storage failed", failure.getCause().getMessage());
        assertTrue(rewards.isEmpty());
    }

    @Test void fakeVoteAndAddTotalsPolicyPreserveExistingCountingRules() {
        UUID uuid = UUID.randomUUID();
        SharedVoteTestStore store = new SharedVoteTestStore(tempDir.resolve("policy.properties"), 5);
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", true)), store, recording(store, new ArrayList<>()));
        SharedVoteProcessingResult ignoredFake = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 5000L, false, true, false, true),
                new SharedVotePolicy(false, true, true, true, true)).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 0), ignoredFake.persistedState());
        SharedVoteProcessingResult countedFake = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 6000L, false, true, false, true),
                new SharedVotePolicy(true, false, true, true, true)).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 5), countedFake.persistedState());
    }

    private static SharedVoteRewardServices recording(SharedVoteTestStore store, List<String> events) {
        return new SharedVoteRewardServices() {
            @Override public CompletionStage<SharedVoteRewardPlan> prepareVoteRewards(SharedVoteInput input,
                    SharedVoteIdentity identity, boolean executeRewardsNow) {
                return store.prepareVoteRewards(input, identity, executeRewardsNow);
            }
            @Override public CompletionStage<SharedVoteProcessingResult.RewardDisposition> deliverOnce(SharedVoteReceipt receipt) {
                return store.deliverOnce(receipt).thenApply(result -> { events.clear(); events.addAll(store.events()); return result; });
            }
            @Override public CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                    SharedVoteUserSnapshot state) { throw new AssertionError("Unkeyed execution"); }
            @Override public CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                    SharedVoteUserSnapshot state) { throw new AssertionError("Unkeyed deferral"); }
        };
    }
}
