package com.bencodez.votingplugin.npc;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class VotingNPCRegistryTest {
    @TempDir Path directory;

    private VotingNPCDefinition definition(int id) {
        return new VotingNPCDefinition("npc-" + id, new UUID(0, id), new UUID(1, 0),
                0.5, 64, 0.5, 0, 0, "Vote");
    }

    private static <T> T await(CompletionStage<T> stage) throws Exception {
        return stage.toCompletableFuture().get(5, TimeUnit.SECONDS);
    }

    private static void close(VotingNPCRegistry registry) throws InterruptedException {
        registry.close();
        assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
    }

    private static void waitOn(CountDownLatch latch) throws IOException {
        try {
            if (!latch.await(5, TimeUnit.SECONDS)) throw new IOException("Synthetic worker latch timed out");
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Synthetic worker interrupted", interrupted);
        }
    }

    @Test void linkedDirectoryAliasesShareTheSameExclusiveOwnerAndLeafLockLinksAreRejected() throws Exception {
        Path real = Files.createDirectory(directory.resolve("real"));
        Path linked = Files.createSymbolicLink(directory.resolve("linked"), real);
        Path file = linked.resolve("npcs.json");
        var first = new VotingNPCRegistry(file);
        try {
            await(first.initialLoad()); await(first.create(definition(1)));
            var alias = new VotingNPCRegistry(real.resolve("npcs.json"));
            try { assertThrows(ExecutionException.class, () -> await(alias.initialLoad())); }
            finally { close(alias); }
        } finally { close(first); }
        var recovered = new VotingNPCRegistry(file);
        try { assertEquals(List.of(definition(1)), await(recovered.initialLoad())); }
        finally { close(recovered); }
        Path lock = linked.resolve("other.json.lock");
        Path unrelated = real.resolve("unrelated"); Files.writeString(unrelated, "unchanged");
        Files.createSymbolicLink(lock, unrelated);
        var rejected = new VotingNPCRegistry(linked.resolve("other.json"));
        try { assertThrows(ExecutionException.class, () -> await(rejected.initialLoad())); }
        finally { close(rejected); }
        assertEquals("unchanged", Files.readString(unrelated));
    }

    @Test void orderedDefinitionsAreDurableAndDuplicateRequestsDoNotOverwrite() throws Exception {
        Path file = directory.resolve("npcs.json");
        var registry = new VotingNPCRegistry(file);
        try {
            assertEquals(List.of(), await(registry.initialLoad()));
            var first = registry.create(definition(1));
            var duplicate = registry.create(definition(1));
            var second = registry.create(definition(2));
            assertEquals(definition(1), await(first));
            assertInstanceOf(IllegalArgumentException.class,
                    assertThrows(ExecutionException.class, () -> await(duplicate)).getCause());
            await(second);
            assertEquals(List.of(definition(1), definition(2)), new VotingNPCStore(file).load());
            assertTrue(await(registry.remove("npc-1")));
            assertFalse(await(registry.remove("npc-1")));
        } finally { close(registry); }
        var restarted = new VotingNPCRegistry(file);
        try { assertEquals(List.of(definition(2)), await(restarted.initialLoad())); }
        finally { close(restarted); }
    }

    @Test void failedReloadRetainsLastSnapshotButCannotPublishUntilRepaired() throws Exception {
        Path file = directory.resolve("npcs.json");
        var registry = new VotingNPCRegistry(file);
        try {
            await(registry.initialLoad()); await(registry.create(definition(1)));
            byte[] healthy = Files.readAllBytes(file);
            Files.writeString(file, "{invalid");
            assertThrows(ExecutionException.class, () -> await(registry.reload()));
            assertFalse(registry.available());
            assertEquals(List.of(definition(1)), registry.snapshot());
            assertThrows(ExecutionException.class, () -> await(registry.create(definition(2))));
            assertEquals("{invalid", Files.readString(file));
            Files.write(file, healthy);
            assertEquals(List.of(definition(1)), await(registry.reload()));
            await(registry.create(definition(2)));
        } finally { close(registry); }
    }

    @Test void invalidInitialFileDoesNotAppearReadyOrGetReplaced() throws Exception {
        Path file = directory.resolve("npcs.json"); Files.writeString(file, "{invalid");
        var registry = new VotingNPCRegistry(file);
        try {
            assertThrows(ExecutionException.class, () -> await(registry.initialLoad()));
            assertFalse(registry.available());
            assertThrows(ExecutionException.class, () -> await(registry.create(definition(1))));
            assertEquals("{invalid", Files.readString(file));
        } finally { close(registry); }
    }

    @Test void closeReturnsWhileAcceptedWriteDrainsAndKeepsExclusiveFileOwnership() throws Exception {
        Path file = directory.resolve("npcs.json");
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        var store = new VotingNPCStore(file) {
            @Override synchronized void save(List<VotingNPCDefinition> values) throws IOException {
                entered.countDown(); waitOn(release); super.save(values);
            }
        };
        var first = new VotingNPCRegistry(file, store);
        VotingNPCRegistry replacement = null;
        try {
            await(first.initialLoad());
            var writing = first.create(definition(1));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            first.close(); first.close();
            assertFalse(first.awaitClosed(0, TimeUnit.SECONDS));
            assertThrows(ExecutionException.class, () -> await(first.remove("npc-1")));
            replacement = new VotingNPCRegistry(file);
            var next = replacement;
            assertThrows(ExecutionException.class, () -> await(next.initialLoad()));
            assertFalse(replacement.available());
            release.countDown(); await(writing);
            assertTrue(first.awaitClosed(5, TimeUnit.SECONDS));
            assertEquals(List.of(definition(1)), await(replacement.reload()));
        } finally {
            release.countDown(); close(first);
            if (replacement != null) close(replacement);
        }
    }

    @Test void saturatedReloadIsRejectedWithoutDisablingTheHealthyOwner() throws Exception {
        Path file = directory.resolve("npcs.json");
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        var store = new VotingNPCStore(file) {
            @Override synchronized void save(List<VotingNPCDefinition> values) throws IOException {
                entered.countDown(); waitOn(release); super.save(values);
            }
        };
        var registry = new VotingNPCRegistry(file, store);
        try {
            await(registry.initialLoad());
            var writing = registry.create(definition(1));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            var queued = new java.util.ArrayList<CompletionStage<Boolean>>();
            for (int i = 0; i < 128; i++) queued.add(registry.remove("missing"));
            assertThrows(ExecutionException.class, () -> await(registry.reload()));
            assertTrue(registry.available(), "rejected reload must leave existing readiness intact");
            release.countDown(); await(writing);
            for (var entry : queued) assertFalse(await(entry));
            await(registry.create(definition(2)));
        } finally { release.countDown(); close(registry); }
    }

    @Test void failedWriteFencesAlreadyQueuedMutationsUntilCheckedReload() throws Exception {
        Path file = directory.resolve("npcs.json");
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        AtomicBoolean fail = new AtomicBoolean(true);
        var store = new VotingNPCStore(file) {
            @Override synchronized void save(List<VotingNPCDefinition> values) throws IOException {
                if (fail.getAndSet(false)) {
                    entered.countDown(); waitOn(release);
                    super.save(values);
                    throw new IOException("Synthetic failure after visible write");
                }
                super.save(values);
            }
        };
        var registry = new VotingNPCRegistry(file, store);
        try {
            await(registry.initialLoad());
            var first = registry.create(definition(1));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            var second = registry.create(definition(2));
            release.countDown();
            assertThrows(ExecutionException.class, () -> await(first));
            assertThrows(ExecutionException.class, () -> await(second));
            assertFalse(registry.available());
            assertEquals(List.of(), registry.snapshot());
            assertEquals(List.of(definition(1)), new VotingNPCStore(file).load());
            assertEquals(List.of(definition(1)), await(registry.reload()));
            await(registry.create(definition(2)));
            assertEquals(List.of(definition(1), definition(2)), registry.snapshot());
        } finally { release.countDown(); close(registry); }
    }

    @Test void completionCallbackMayCloseWithoutDeadlockingOrInterruptingCommittedWork() throws Exception {
        var registry = new VotingNPCRegistry(directory.resolve("npcs.json"));
        try {
            await(registry.initialLoad());
            CompletableFuture<Void> callback = registry.create(definition(1)).thenAccept(value -> registry.close())
                    .toCompletableFuture();
            await(callback);
            assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            assertEquals(List.of(definition(1)), new VotingNPCStore(directory.resolve("npcs.json")).load());
        } finally { close(registry); }
    }

    @Test void capacityAndIdentityRejectionsLeaveHealthyStorageAvailable() throws Exception {
        Path file = directory.resolve("npcs.json");
        var seed = new VotingNPCStore(file); seed.load();
        seed.save(java.util.stream.IntStream.rangeClosed(1, 63).mapToObj(this::definition).toList());
        var registry = new VotingNPCRegistry(file);
        try {
            await(registry.initialLoad());
            byte[] original = Files.readAllBytes(file);
            var duplicateIdentity = new VotingNPCDefinition("another", definition(1).identity(), definition(1).world(),
                    0, 64, 0, 0, 0, "Vote");
            assertThrows(ExecutionException.class, () -> await(registry.create(duplicateIdentity)));
            assertTrue(registry.available(), "invalid new identity must not disable existing NPC storage");
            assertArrayEquals(original, Files.readAllBytes(file));
            await(registry.create(definition(64)));
            byte[] atLimit = Files.readAllBytes(file);
            assertThrows(ExecutionException.class, () -> await(registry.create(definition(65))));
            assertTrue(registry.available(), "one extra creation must not disable 64 existing NPCs");
            assertEquals(64, registry.snapshot().size());
            assertArrayEquals(atLimit, Files.readAllBytes(file));
        } finally { close(registry); }
    }
}
