package com.bencodez.votingplugin.tests.listeners;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BooleanSupplier;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue;

class VotifierOverflowGenerationOwnershipTest {
    @TempDir Path directory;

    private VotingPluginMain plugin() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getDataFolder()).thenReturn(directory.toFile());
        when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        when(plugin.getOptions().getBedrockPlayerPrefix()).thenReturn(".");
        return plugin;
    }

    private static ScheduledThreadPoolExecutor worker(VotifierVoteOverflowQueue queue) throws Exception {
        var field = VotifierVoteOverflowQueue.class.getDeclaredField("worker");
        field.setAccessible(true);
        return (ScheduledThreadPoolExecutor) field.get(queue);
    }

    private static void turn(VotifierVoteOverflowQueue queue) throws Exception {
        worker(queue).submit(() -> { }).get(5, TimeUnit.SECONDS);
    }

    private static void await(BooleanSupplier condition) throws Exception {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) Thread.sleep(10);
        assertTrue(condition.getAsBoolean(), "bounded lifecycle condition did not complete");
    }

    private static void add(VotifierVoteOverflowQueue queue, UUID id) throws Exception {
        var accepted = new AtomicBoolean(); var done = new CountDownLatch(1);
        queue.enqueueAfterInitialization("TestPlayer", "example.org", 1234L, id,
                result -> { accepted.set(result); done.countDown(); });
        assertTrue(done.await(5, TimeUnit.SECONDS));
        assertTrue(accepted.get()); turn(queue);
    }

    private List<?> rows() throws Exception {
        var yaml = new YamlConfiguration();
        yaml.load(directory.resolve("VotifierVoteQueue.yml").toFile());
        return yaml.getList("Votes");
    }

    @Test void nativeOwnershipSurvivesSameJvmContenderAndReleasesAfterClose() throws Exception {
        var plugin = plugin();
        var first = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
        VotifierVoteOverflowQueue successor = null;
        Path owner = directory.resolve("VotifierVoteQueue.yml.owner");
        try {
            await(first::isInitializationComplete);
            successor = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
            assertTrue(Files.exists(owner));
            turn(successor);
            assertFalse(successor.isInitializationComplete(), "same-JVM contender must wait before opening a descriptor");
            assertEquals(3, externalLockProbe(owner));
            first.close();
            await(successor::isInitializationComplete);
            successor.close();
            assertEquals(0, externalLockProbe(owner), "native lock must be released after final queue close");
        } finally {
            first.close(); if (successor != null) successor.close();
        }
    }

    private static int externalLockProbe(Path path) throws Exception {
        String java = Path.of(System.getProperty("java.home"), "bin").resolve(
                System.getProperty("os.name", "").startsWith("Windows") ? "java.exe" : "java").toString();
        Process process = new ProcessBuilder(java, "-cp", System.getProperty("surefire.test.class.path", System.getProperty("java.class.path")),
                LockProbe.class.getName(), path.toString()).start();
        try { assertTrue(process.waitFor(10, TimeUnit.SECONDS)); return process.exitValue(); }
        finally { process.destroyForcibly(); process.waitFor(10, TimeUnit.SECONDS); }
    }

    public static final class LockProbe {
        public static void main(String[] args) throws Exception {
            try (var channel = java.nio.channels.FileChannel.open(Path.of(args[0]), StandardOpenOption.WRITE);
                 var lock = channel.tryLock()) { System.exit(lock == null ? 3 : 0); }
        }
    }

    @Test void isolatedClassloaderCannotOpenSecondNativeOwner() throws Exception {
        var first = new VotifierVoteOverflowQueue(plugin(), (s, u) -> { });
        try (var loader = isolatedQueueLoader()) {
            Class<?> type = loader.loadClass(VotifierVoteOverflowQueue.class.getName());
            assertNotSame(VotifierVoteOverflowQueue.class, type);
            var constructor = type.getConstructor(VotingPluginMain.class, java.util.function.BiConsumer.class);
            Object contender = constructor.newInstance(plugin(), (java.util.function.BiConsumer<String, String>) (s, u) -> { });
            try {
                var enqueue = type.getMethod("enqueue", String.class, String.class);
                assertFalse((Boolean) enqueue.invoke(contender, "TestPlayer", "example.org"));
                assertEquals(3, externalLockProbe(directory.resolve("VotifierVoteQueue.yml.owner")));
            } finally { type.getMethod("close").invoke(contender); }
        } finally { first.close(); }
    }

    @Test void failedOwnerOpenReclaimsReservationAndRepeatedCloseCannotClearSuccessor() throws Exception {
        Path obstruction = directory.resolve("VotifierVoteQueue.yml.owner");
        Files.createDirectory(obstruction);
        var failed = new VotifierVoteOverflowQueue(plugin(), (s, u) -> { });
        assertFalse(failed.enqueue("TestPlayer", "example.org"));
        failed.close(); Files.delete(obstruction);
        var first = new VotifierVoteOverflowQueue(plugin(), (s, u) -> { });
        first.close();
        var second = new VotifierVoteOverflowQueue(plugin(), (s, u) -> { });
        try {
            assertTrue(second.enqueue("TestPlayer", "example.org"));
            first.close(); failed.close();
            assertEquals(3, externalLockProbe(obstruction));
        } finally { second.close(); }
        assertEquals(0, externalLockProbe(obstruction));
    }

    private static URLClassLoader isolatedQueueLoader() {
        URL location = VotifierVoteOverflowQueue.class.getProtectionDomain().getCodeSource().getLocation();
        return new URLClassLoader(new URL[] { location }, VotifierVoteOverflowQueue.class.getClassLoader()) {
            @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                if (!name.equals(VotifierVoteOverflowQueue.class.getName())
                        && !name.startsWith(VotifierVoteOverflowQueue.class.getName() + "$")) return super.loadClass(name, resolve);
                synchronized (getClassLoadingLock(name)) {
                    Class<?> loaded = findLoadedClass(name);
                    if (loaded == null) loaded = findClass(name);
                    if (resolve) resolveClass(loaded);
                    return loaded;
                }
            }
        };
    }

    @Test void failedOwnershipInitializationRejectsAdmissionWithoutOverwritingObstruction() throws Exception {
        Path obstruction = directory.resolve("blocked-data");
        java.nio.file.Files.writeString(obstruction, "synthetic obstruction");
        var plugin = plugin(); when(plugin.getDataFolder()).thenReturn(obstruction.toFile());
        try (var queue = new VotifierVoteOverflowQueue(plugin, (s, u) -> { })) {
            assertFalse(queue.enqueue("TestPlayer", "example.org"));
            assertEquals(0, queue.size());
        }
        assertEquals("synthetic obstruction", java.nio.file.Files.readString(obstruction));
    }

    @Test void successorWaitsUntilRetiredWriterPublishesAndReleasesFileOwnership() throws Exception {
        var plugin = plugin(); UUID original = UUID.randomUUID(), next = UUID.randomUUID();
        var first = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
        VotifierVoteOverflowQueue successor = null;
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        try {
            turn(first); add(first, original);
            worker(first).execute(() -> {
                entered.countDown();
                try { if (!release.await(10, TimeUnit.SECONDS)) throw new AssertionError("worker release timeout"); }
                catch (InterruptedException failure) { Thread.currentThread().interrupt(); throw new AssertionError(failure); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            first.close(); // Returns after the bounded wait while its final publication remains queued.
            assertFalse(worker(first).isTerminated());
            successor = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
            turn(successor);
            assertFalse(successor.isInitializationComplete(), "successor must not load/publish ahead of the old writer");
            assertEquals(1, rows().size());
            release.countDown();
            assertTrue(worker(first).awaitTermination(5, TimeUnit.SECONDS));
            var current = successor; await(current::isInitializationComplete);
            assertEquals(1, current.size()); add(current, next); current.close();
            var stored = rows(); assertEquals(2, stored.size());
            var ids = stored.stream().map(row -> ((Map<?, ?>) row).get("LocalOccurrenceId")).toList();
            assertTrue(ids.contains(original.toString())); assertTrue(ids.contains(next.toString()));
        } finally {
            release.countDown(); first.close();
            worker(first).awaitTermination(5, TimeUnit.SECONDS);
            if (successor != null) successor.close();
        }
    }

    @Test void closingSuccessorBeforeOwnershipAcquisitionCannotPublishAnEmptySnapshot() throws Exception {
        var plugin = plugin(); UUID original = UUID.randomUUID();
        var first = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
        VotifierVoteOverflowQueue successor = null;
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        try {
            turn(first); add(first, original);
            worker(first).execute(() -> {
                entered.countDown();
                try { release.await(10, TimeUnit.SECONDS); }
                catch (InterruptedException failure) { Thread.currentThread().interrupt(); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS)); first.close();
            successor = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (s, u, t, id) -> true, () -> false);
            turn(successor); assertFalse(successor.isInitializationComplete());
            successor.close();
            assertEquals(1, rows().size());
            assertEquals(original.toString(), ((Map<?, ?>) rows().getFirst()).get("LocalOccurrenceId"));
            release.countDown(); assertTrue(worker(first).awaitTermination(5, TimeUnit.SECONDS));
            assertEquals(1, rows().size());
        } finally {
            release.countDown(); first.close(); worker(first).awaitTermination(5, TimeUnit.SECONDS);
            if (successor != null) successor.close();
        }
    }
}
