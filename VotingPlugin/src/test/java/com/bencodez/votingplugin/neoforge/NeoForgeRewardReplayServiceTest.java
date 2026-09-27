package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

class NeoForgeRewardReplayServiceTest {
    @TempDir Path directory;

    @Test
    void disabledRewardProcessingKeepsRetainedVotePending() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("Config.yml"), Files.readString(directory.resolve("Config.yml"))
                .replace("ProcessRewards: true", "ProcessRewards: false"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void replayUsesCurrentUuidIdentityNameForActionsAndAccounting() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "OldName", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            runtime.players().joined(new SharedVoteIdentity(playerId, "NewName", true));
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    runOne(runtime, replay, actions).status());
            assertEquals(List.of("say NewName"), actions.rendered);
            assertEquals("NewName", runtime.accounting().load(playerId).orElseThrow().playerName());
        }
    }

    @Test
    void replayUsesRetainedOnlineStateForOfflineAccountingPolicy() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("Config.yml"), Files.readString(directory.resolve("Config.yml"))
                .replace("AddTotalsOffline: true", "AddTotalsOffline: false"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));

            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    runOne(runtime, replay, actions).status());
            assertEquals(List.of("say Alex"), actions.rendered);
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void replayUsesCurrentConfiguredServiceSiteForRewardPlaceholders() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("VoteSites.yml"), Files.readString(directory.resolve("VoteSites.yml"))
                .replace("'say %player%'", "'say %ServiceSite%'"));
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, voteId, playerId, "Supported");
        }
        Files.writeString(directory.resolve("VoteSites.yml"), Files.readString(directory.resolve("VoteSites.yml"))
                .replace("ServiceSite: Service", "ServiceSite: RenamedService"));
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));

            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    runOne(runtime, replay, actions).status());
            assertEquals(List.of("say RenamedService"), actions.rendered);
        }
    }

    @Test
    void rewardKeysHonorConfiguredYamlCaseSensitivity() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("VoteSites.yml"), Files.readString(directory.resolve("VoteSites.yml"))
                .replace("Commands:", "commands:"));
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        RecordingActions strictActions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, strictActions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, voteId, playerId, "Service");
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED,
                    replay.replayOnce().get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(0, strictActions.calls.get());
        }
        Files.writeString(directory.resolve("Config.yml"), Files.readString(directory.resolve("Config.yml"))
                + "CaseInsensitiveYMLFiles: true\n");
        RecordingActions insensitiveActions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, insensitiveActions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    runOne(runtime, replay, insensitiveActions).status());
        }
    }

    @Test
    void unjoinedCommandPlaceholderNameMustUseMinecraftSyntax() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteResult result = runtime.voteProcessor().process(new NeoForgeVoteRequest(
                    UUID.randomUUID(), playerId, "Alex;op", "Service", 100L, true, true, false,
                    NeoForgeVoteRequest.Scope.COMPLETE));
            assertEquals(NeoForgeVoteResult.Status.UNKNOWN_PLAYER, result.status());
            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
        }
    }

    @Test
    void persistedLegacyNameIsRevalidatedBeforeRewardExecution() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(playerId).write(com.bencodez.advancedcore.api.user.UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES, new com.bencodez.simpleapi.sql.data.DataValueString(
                            "v1|" + voteId + "|QGE|U2VydmljZQ|U3VwcG9ydGVk|100|true|true|false"));
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            List<NeoForgeRewardReplayService.ReplayResult> results = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, results.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void synchronousActionFailureReleasesClaimForLaterReplay() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            actions.throwSynchronously = true;
            try (NeoForgeRewardReplayService replay = service(runtime, actions)) {
                assertEquals(NeoForgeRewardReplayService.Status.REWARD_FAILED,
                        replay.replayOnce().get(5, TimeUnit.SECONDS).get(0).status());
            }
            actions.throwSynchronously = false;
            try (NeoForgeRewardReplayService replay = service(runtime, actions)) {
                assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                        runOne(runtime, replay, actions).status());
            }
        }
    }

    @Test
    void specialRewardGuardsHonorStrictYamlCasing() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("SpecialRewards.yml"), """
                VoteParty:
                  Enabled: false
                VoteMilestones:
                  First:
                    enabled: false
                    Rewards:
                      Commands: ['say milestone']
                """);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED,
                    replay.replayOnce().get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(0, actions.calls.get());
        }
    }

    @Test
    void supportedRewardCompletesAccountingAndTombstoneOnceAcrossRestart() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, voteId, playerId, "Service");
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            NeoForgeRewardReplayService.ReplayResult result = runOne(runtime, replay, actions);
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED, result.status());
            assertEquals(List.of("say Alex"), actions.rendered);
            assertEquals(1, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(playerId, voteId));
            assertTrue(replay.replayOnce().get(5, TimeUnit.SECONDS).isEmpty());
            assertEquals(1, actions.calls.get());
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertEquals(1, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(playerId, voteId));
        }
    }

    @Test
    void failedRewardStaysPendingAndCanRetry() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, voteId, playerId, "Service");
            RecordingActions failing = new RecordingActions();
            failing.fail = true;
            try (NeoForgeRewardReplayService replay = service(runtime, failing)) {
                assertEquals(NeoForgeRewardReplayService.Status.REWARD_FAILED,
                        runOne(runtime, replay, failing).status());
            }
            assertFalse(runtime.deferredVotes().pending(playerId).isEmpty());
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());

            RecordingActions succeeding = new RecordingActions();
            try (NeoForgeRewardReplayService replay = service(runtime, succeeding)) {
                assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                        runOne(runtime, replay, succeeding).status());
            }
            assertEquals(1, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void unsupportedRewardAndOfflinePlayerRemainDurable() throws Exception {
        writeConfiguration(true);
        UUID blockedPlayer = UUID.randomUUID();
        UUID offlinePlayer = UUID.randomUUID();
        UUID healthyPlayer = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(blockedPlayer, "Blocked", true));
            runtime.players().joined(new SharedVoteIdentity(healthyPlayer, "Healthy", true));
            retain(runtime, UUID.randomUUID(), blockedPlayer, "Unsupported");
            retain(runtime, UUID.randomUUID(), offlinePlayer, "Service");
            retain(runtime, UUID.randomUUID(), healthyPlayer, "Service");
            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> pending = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            runtime.scheduler().onServerTick();
            List<NeoForgeRewardReplayService.ReplayResult> results = pending.get(5, TimeUnit.SECONDS);
            assertTrue(results.stream().anyMatch(result -> result.status()
                    == NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED));
            assertTrue(results.stream().anyMatch(result -> result.status()
                    == NeoForgeRewardReplayService.Status.WAITING_FOR_PLAYER));
            assertTrue(results.stream().anyMatch(result -> result.status()
                    == NeoForgeRewardReplayService.Status.COMPLETED));
            assertEquals(1, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(blockedPlayer).size());
            assertEquals(1, runtime.deferredVotes().pending(offlinePlayer).size());
            assertTrue(runtime.deferredVotes().pending(healthyPlayer).isEmpty());
            assertEquals(0, runtime.accounting().load(blockedPlayer).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void configuredAnySiteRewardIsNotSilentlySkipped() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("SpecialRewards.yml"), """
                VoteParty:
                  Enabled: false
                VoteMilestones: {}
                VoteStreaks: {}
                VoteStreak: {}
                Cumulative: {}
                MileStones: {}
                FirstVote: {}
                FirstVoteToday: {}
                AnySiteRewards:
                  Commands:
                  - 'say any site'
                """);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> results = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, results.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void defaultEnabledMilestonesRemainPending() throws Exception {
        writeConfiguration(false);
        Files.writeString(directory.resolve("SpecialRewards.yml"), """
                VoteParty:
                  Enabled: false
                VoteMilestones:
                  First:
                    At: 1
                    Rewards:
                      Commands: ['say milestone']
                VoteStreaks: {}
                VoteStreak: {}
                Cumulative: {}
                MileStones: {}
                FirstVote: {}
                FirstVoteToday: {}
                """);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void namedRewardListsRemainPending() throws Exception {
        writeConfiguration(false);
        String sites = Files.readString(directory.resolve("VoteSites.yml"));
        Files.writeString(directory.resolve("VoteSites.yml"), sites.replace(
                "Commands:\n      - 'say %player%'",
                "- named-reward"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void playerMessageOnlyRewardIsSupported() throws Exception {
        writeConfiguration(false);
        String sites = Files.readString(directory.resolve("VoteSites.yml"));
        Files.writeString(directory.resolve("VoteSites.yml"), sites.replace(
                "Commands:\n      - 'say %player%'",
                "Messages:\n        Player: 'Thanks %player% from %ServiceSite% at %SiteName%'"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> completion = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            runtime.scheduler().onServerTick();
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    completion.get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(List.of("Thanks Alex from Service at Supported Display"), actions.rendered);
        }
    }

    @Test
    void mixedCommandsAndMessagesRemainPendingBeforeAnyEffect() throws Exception {
        writeConfiguration(false);
        String sites = Files.readString(directory.resolve("VoteSites.yml"));
        Files.writeString(directory.resolve("VoteSites.yml"), sites.replace(
                "Commands:\n      - 'say %player%'",
                "Commands:\n      - 'say %player%'\n      Messages:\n        Player: 'Thanks %player%'"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void multipleCommandsRemainPendingBeforeAnyEffect() throws Exception {
        writeConfiguration(false);
        String sites = Files.readString(directory.resolve("VoteSites.yml"));
        Files.writeString(directory.resolve("VoteSites.yml"), sites.replace(
                "- 'say %player%'", "- 'say first'\n      - 'say second'"));
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
        }
    }

    @Test
    void disabledSiteAfterRetentionRemainsPending() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
        }
        String sites = Files.readString(directory.resolve("VoteSites.yml"));
        Files.writeString(directory.resolve("VoteSites.yml"), sites.replaceFirst("Enabled: true", "Enabled: false"));
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            List<NeoForgeRewardReplayService.ReplayResult> result = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.get(0).status());
            assertEquals(0, actions.calls.get());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void blockedEarlierOccurrenceDoesNotStarveLaterHealthyOccurrence() throws Exception {
        writeConfiguration(true);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Unsupported");
            retain(runtime, UUID.randomUUID(), playerId, "Service");

            List<NeoForgeRewardReplayService.ReplayResult> first = replay.replayOnce().get(5, TimeUnit.SECONDS);
            assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, first.get(0).status());

            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> second = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            runtime.scheduler().onServerTick();
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    second.get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
            assertEquals(1, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void saturatedScanAdvancesToLaterUsers() throws Exception {
        writeConfiguration(true);
        RecordingActions actions = new RecordingActions();
        UUID healthy = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            for (int index = 0; index < NeoForgeRewardReplayService.MAX_PER_RUN; index++) {
                UUID blocked = UUID.randomUUID();
                runtime.players().joined(new SharedVoteIdentity(blocked, "Blocked" + index, true));
                retain(runtime, UUID.randomUUID(), blocked, "Unsupported");
            }
            runtime.players().joined(new SharedVoteIdentity(healthy, "Healthy", true));
            retain(runtime, UUID.randomUUID(), healthy, "Service");
            assertEquals(NeoForgeRewardReplayService.MAX_PER_RUN,
                    replay.replayOnce().get(5, TimeUnit.SECONDS).size());

            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> second = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            runtime.scheduler().onServerTick();
            assertTrue(second.get(5, TimeUnit.SECONDS).stream().anyMatch(result ->
                    result.status() == NeoForgeRewardReplayService.Status.COMPLETED));
            assertTrue(runtime.deferredVotes().pending(healthy).isEmpty());
        }
    }

    @Test
    void platformActionsRunOnlyWhenServerSchedulerTicks() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> result = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            assertFalse(result.isDone());
            Thread tickThread = Thread.currentThread();
            runtime.scheduler().onServerTick();
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    result.get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(tickThread, actions.executionThread.get());
        }
    }

    @Test
    void shutdownRejectsQueuedRewardAndLeavesVoteRecoverable() throws Exception {
        writeConfiguration(false);
        UUID playerId = UUID.randomUUID();
        RecordingActions actions = new RecordingActions();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
                NeoForgeRewardReplayService replay = service(runtime, actions)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            retain(runtime, UUID.randomUUID(), playerId, "Service");
            CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> result = replay.replayOnce();
            assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
            runtime.scheduler().close();
            assertEquals(NeoForgeRewardReplayService.Status.REWARD_FAILED,
                    result.get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void unsuccessfulNativeCommandDoesNotReportRewardSuccess() {
        NeoForgeServerScheduler scheduler = new NeoForgeServerScheduler();
        try {
            NeoForgeNativeRewardActions actions = new NeoForgeNativeRewardActions(
                    new FailingCommandServer(), scheduler, new NeoForgePlayerDirectory());
            NeoForgeDeferredVote vote = new NeoForgeDeferredVote(UUID.randomUUID(), UUID.randomUUID(),
                    "Alex", "Service", "Supported", 100L, true, true, true);
            NeoForgeRewardPlan plan = new NeoForgeRewardPlan(NeoForgeRewardPlan.Status.READY,
                    List.of(new NeoForgeRewardPlan.Action(
                            NeoForgeRewardPlan.ActionType.CONSOLE_COMMAND, "unknown")), false, "test");
            CompletableFuture<Void> completion = actions.execute(vote, plan);
            scheduler.onServerTick();
            assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> completion.get(5, TimeUnit.SECONDS));
        } finally {
            scheduler.close();
        }
    }

    private NeoForgeRewardReplayService service(NeoForgeRuntime runtime, RecordingActions actions) {
        actions.scheduler = runtime.scheduler();
        return new NeoForgeRewardReplayService(runtime.voteConfiguration(),
                new NeoForgeRewardConfiguration(runtime.config(), runtime.voteSites(), runtime.specialRewards()),
                runtime.accounting(), runtime.deferredVotes(), runtime.players(), actions);
    }

    private NeoForgeRewardReplayService.ReplayResult runOne(NeoForgeRuntime runtime,
            NeoForgeRewardReplayService replay, RecordingActions actions) throws Exception {
        CompletableFuture<List<NeoForgeRewardReplayService.ReplayResult>> result = replay.replayOnce();
        assertTrue(actions.scheduled.await(5, TimeUnit.SECONDS));
        runtime.scheduler().onServerTick();
        return result.get(5, TimeUnit.SECONDS).get(0);
    }

    private static void retain(NeoForgeRuntime runtime, UUID voteId, UUID playerId, String service) {
        NeoForgeVoteResult result = runtime.voteProcessor().process(new NeoForgeVoteRequest(voteId, playerId,
                playerId.toString().substring(0, 8), service, 100L, true, true,
                runtime.players().online(playerId).isPresent(), NeoForgeVoteRequest.Scope.COMPLETE));
        assertEquals(NeoForgeVoteResult.Status.DEFERRED, result.status());
    }

    private void writeConfiguration(boolean includeUnsupported) throws IOException {
        Files.createDirectories(directory);
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AllowUnjoined: true
                AddTotals: true
                AddTotalsOffline: true
                CountFakeVotes: true
                ProcessRewards: true
                PointsOnVote: 1
                LimitVotePoints: -1
                UseVoteStreaks: false
                PerSiteCoolDownEvents: false
                VoteBroadcast:
                  Type: NONE
                """);
        Files.writeString(directory.resolve("SpecialRewards.yml"), """
                VoteParty:
                  Enabled: false
                VoteMilestones: {}
                VoteStreaks: {}
                VoteStreak: {}
                Cumulative: {}
                MileStones: {}
                FirstVote: {}
                FirstVoteToday: {}
                """);
        String rewards = includeUnsupported ? "Items:\n                        prize:\n                          Material: DIAMOND"
                : "Commands: []";
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  Supported:
                    Enabled: true
                    Name: Supported Display
                    ServiceSite: Service
                    WaitUntilVoteDelay: false
                    Rewards:
                      Commands:
                      - 'say %player%'
                  Unsupported:
                    Enabled: true
                    Name: Unsupported
                    ServiceSite: Unsupported
                    WaitUntilVoteDelay: false
                    Rewards:
                      __REWARD__
                EverySiteReward: {}
                """.replace("__REWARD__", rewards));
    }

    private static final class RecordingActions implements NeoForgeRewardActions {
        final AtomicInteger calls = new AtomicInteger();
        final List<String> rendered = new ArrayList<>();
        final CountDownLatch scheduled = new CountDownLatch(1);
        final AtomicReference<Thread> executionThread = new AtomicReference<>();
        NeoForgeServerScheduler scheduler;
        boolean fail;
        boolean throwSynchronously;

        @Override public CompletableFuture<Void> execute(NeoForgeDeferredVote vote, NeoForgeRewardPlan plan) {
            calls.incrementAndGet();
            if (throwSynchronously) throw new IllegalStateException("expected synchronous failure");
            CompletableFuture<Void> result = scheduler.executeAsync(() -> {
                executionThread.set(Thread.currentThread());
                if (fail) throw new IllegalStateException("expected");
                plan.actions().forEach(action -> rendered.add(replace(action.value(), vote)));
            });
            scheduled.countDown();
            return result;
        }

        private static String replace(String value, NeoForgeDeferredVote vote) {
            return value.replace("%player%", vote.playerName()).replace("%ServiceSite%", vote.serviceSite());
        }
    }

    public static final class FailingCommandServer {
        public FailingCommands getCommands() { return new FailingCommands(); }
        public Object createCommandSourceStack() { return new Object(); }
    }

    public static final class FailingCommands {
        public FailingDispatcher getDispatcher() { return new FailingDispatcher(); }
        public int performPrefixedCommand(Object source, String command) { return 0; }
    }

    public static final class FailingDispatcher {
        public FailingParseResult parse(String command, Object source) { return new FailingParseResult(); }
    }

    public static final class FailingParseResult {
        public java.util.Map<String, String> getExceptions() { return java.util.Map.of("unknown", "command"); }
    }
}
