package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SharedVoteProcessorEndToEndTest {
    @TempDir
    Path tempDir;

    @Test
    void votePersistsThenExecutesCommandAndMessageAcrossRestartAndOfflineVote() {
        UUID uuid = UUID.randomUUID();
        Path storeFile = tempDir.resolve("users.properties");
        ArrayList<String> rewards = new ArrayList<>();
        SharedVotePolicy policy = new SharedVotePolicy(false, true, true, true, true);

        FileBackedUserServices firstStore = new FileBackedUserServices(storeFile, 10);
        SharedVoteProcessor firstRuntime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", true)),
                firstStore, new RecordingRewards(rewards));

        SharedVoteProcessingResult first = firstRuntime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 1000L, true, true, false, true),
                policy).toCompletableFuture().join();

        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 10), first.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.EXECUTED, first.rewardDisposition());
        assertTrue(Files.isRegularFile(storeFile));
        assertEquals(List.of("command:say Thanks Ben:1", "message:Thanks Ben:10"), rewards);

        // Simulate a process restart: construct new storage/core objects and reload the
        // same durable file before processing a vote while the user is offline.
        FileBackedUserServices restartedStore = new FileBackedUserServices(storeFile, 10);
        assertEquals(first.persistedState(), restartedStore.load(uuid).toCompletableFuture().join());
        SharedVoteProcessor restartedRuntime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", false)),
                restartedStore, new RecordingRewards(rewards));

        SharedVoteProcessingResult second = restartedRuntime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 2000L, true, true, false, false),
                policy).toCompletableFuture().join();

        assertEquals(new SharedVoteUserSnapshot(2, 2, 2, 2, 20), second.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.EXECUTED, second.rewardDisposition());
        assertEquals(List.of(
                "command:say Thanks Ben:1", "message:Thanks Ben:10",
                "command:say Thanks Ben:2", "message:Thanks Ben:20"), rewards);

        FileBackedUserServices secondRestart = new FileBackedUserServices(storeFile, 10);
        assertEquals(second.persistedState(), secondRestart.load(uuid).toCompletableFuture().join());
    }

    @Test
    void offlineIneligibleRewardIsDurablyDelegatedAfterPersistence() {
        UUID uuid = UUID.randomUUID();
        ArrayList<String> rewards = new ArrayList<>();
        FileBackedUserServices store = new FileBackedUserServices(tempDir.resolve("deferred.properties"), 3);
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", false)),
                store, new RecordingRewards(rewards));

        SharedVoteProcessingResult result = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 3000L, true, true, false, false),
                new SharedVotePolicy(false, true, true, true, false)).toCompletableFuture().join();

        assertEquals(new SharedVoteUserSnapshot(1, 1, 1, 1, 3), result.persistedState());
        assertEquals(SharedVoteProcessingResult.RewardDisposition.DEFERRED, result.rewardDisposition());
        assertEquals(List.of("defer:ExampleSite:1"), rewards);
    }

    @Test
    void persistenceFailurePreventsRewardExecution() {
        ArrayList<String> rewards = new ArrayList<>();
        SharedVoteUserServices failing = new SharedVoteUserServices() {
            @Override
            public CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity,
                    SharedVoteMutation mutation) {
                return CompletableFuture.failedFuture(new IllegalStateException("storage failed"));
            }

            @Override
            public CompletionStage<SharedVoteUserSnapshot> load(UUID uuid) {
                return CompletableFuture.failedFuture(new IllegalStateException("storage failed"));
            }
        };
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(UUID.randomUUID(), "Ben", true)),
                failing, new RecordingRewards(rewards));

        assertThrows(CompletionException.class, () -> runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 4000L, true, true, false, true),
                new SharedVotePolicy(false, true, true, true, true)).toCompletableFuture().join());
        assertTrue(rewards.isEmpty());
    }

    @Test
    void fakeVoteAndAddTotalsPolicyPreserveExistingCountingRules() {
        UUID uuid = UUID.randomUUID();
        FileBackedUserServices store = new FileBackedUserServices(tempDir.resolve("policy.properties"), 5);
        SharedVoteProcessor runtime = new SharedVoteProcessor(
                input -> CompletableFuture.completedFuture(new SharedVoteIdentity(uuid, "Ben", true)),
                store, new RecordingRewards(new ArrayList<>()));

        SharedVoteProcessingResult ignoredFake = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 5000L, false, true, false, true),
                new SharedVotePolicy(false, true, true, true, true)).toCompletableFuture().join();
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 0), ignoredFake.persistedState());

        SharedVoteProcessingResult countedFake = runtime.process(
                new SharedVoteInput(UUID.randomUUID(), "Ben", "ExampleSite", 6000L, false, true, false, true),
                new SharedVotePolicy(true, false, true, true, true)).toCompletableFuture().join();
        // Config.AddTotals=false suppresses totals, but current Bukkit behavior still
        // awards configured points when the event itself allows totals.
        assertEquals(new SharedVoteUserSnapshot(0, 0, 0, 0, 5), countedFake.persistedState());
    }

    private static final class RecordingRewards implements SharedVoteRewardServices {
        private final List<String> events;

        private RecordingRewards(List<String> events) {
            this.events = events;
        }

        @Override
        public CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                SharedVoteUserSnapshot persistedState) {
            events.add("command:say Thanks " + identity.playerName() + ":" + persistedState.allTimeTotal());
            events.add("message:Thanks " + identity.playerName() + ":" + persistedState.points());
            return CompletableFuture.completedFuture(null);
        }

        @Override
        public CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
                SharedVoteUserSnapshot persistedState) {
            events.add("defer:" + input.serviceSite() + ":" + persistedState.allTimeTotal());
            return CompletableFuture.completedFuture(null);
        }
    }

    /** Test-only durable implementation; production persistence remains an AdvancedCore adapter. */
    private static final class FileBackedUserServices implements SharedVoteUserServices {
        private final Path file;
        private final int configuredPoints;

        private FileBackedUserServices(Path file, int configuredPoints) {
            this.file = file;
            this.configuredPoints = configuredPoints;
        }

        @Override
        public synchronized CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity,
                SharedVoteMutation mutation) {
            try {
                Properties properties = read();
                String prefix = identity.uuid() + ".";
                int all = integer(properties, prefix + "all");
                int month = integer(properties, prefix + "month");
                int week = integer(properties, prefix + "week");
                int day = integer(properties, prefix + "day");
                int points = integer(properties, prefix + "points");
                if (mutation.countTotals()) {
                    all++;
                    month++;
                    week++;
                    day++;
                }
                if (mutation.awardConfiguredPoints()) {
                    points += configuredPoints;
                }
                properties.setProperty(prefix + "all", Integer.toString(all));
                properties.setProperty(prefix + "month", Integer.toString(month));
                properties.setProperty(prefix + "week", Integer.toString(week));
                properties.setProperty(prefix + "day", Integer.toString(day));
                properties.setProperty(prefix + "points", Integer.toString(points));
                properties.setProperty(prefix + "lastSite", mutation.serviceSite());
                properties.setProperty(prefix + "lastTime", Long.toString(mutation.voteTime()));
                properties.setProperty(prefix + "lastVoteId", mutation.voteId().toString());
                write(properties);
                return CompletableFuture.completedFuture(new SharedVoteUserSnapshot(all, month, week, day, points));
            } catch (IOException e) {
                return CompletableFuture.failedFuture(e);
            }
        }

        @Override
        public synchronized CompletionStage<SharedVoteUserSnapshot> load(UUID uuid) {
            try {
                Properties properties = read();
                String prefix = uuid + ".";
                return CompletableFuture.completedFuture(new SharedVoteUserSnapshot(
                        integer(properties, prefix + "all"), integer(properties, prefix + "month"),
                        integer(properties, prefix + "week"), integer(properties, prefix + "day"),
                        integer(properties, prefix + "points")));
            } catch (IOException e) {
                return CompletableFuture.failedFuture(e);
            }
        }

        private Properties read() throws IOException {
            Properties properties = new Properties();
            if (Files.isRegularFile(file)) {
                try (InputStream input = Files.newInputStream(file)) {
                    properties.load(input);
                }
            }
            return properties;
        }

        private void write(Properties properties) throws IOException {
            Files.createDirectories(file.getParent());
            Path temporary = file.resolveSibling(file.getFileName() + ".tmp");
            try (OutputStream output = Files.newOutputStream(temporary)) {
                properties.store(output, "shared vote test store");
            }
            try {
                Files.move(temporary, file, java.nio.file.StandardCopyOption.REPLACE_EXISTING,
                        java.nio.file.StandardCopyOption.ATOMIC_MOVE);
            } catch (java.nio.file.AtomicMoveNotSupportedException e) {
                Files.move(temporary, file, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            }
        }

        private static int integer(Properties properties, String key) {
            return Integer.parseInt(properties.getProperty(key, "0"));
        }
    }
}
