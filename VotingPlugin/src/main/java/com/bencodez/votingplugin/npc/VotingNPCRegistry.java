package com.bencodez.votingplugin.npc;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * One bounded, daemon file owner. Completion means the definition was persisted,
 * not that a world entity has spawned. Callbacks have no player/region ownership.
 */
final class VotingNPCRegistry implements AutoCloseable {
    enum Rejection { DUPLICATE_ID, DUPLICATE_IDENTITY, LIMIT }
    static final class ValidationFailure extends IllegalArgumentException {
        private static final long serialVersionUID = 1L;
        final Rejection reason;
        ValidationFailure(Rejection reason) { super("Voting NPC request rejected"); this.reason = reason; }
    }

    private final Object admission = new Object();
    private final ArrayBlockingQueue<Runnable> queue = new ArrayBlockingQueue<>(128);
    private final CountDownLatch stopped = new CountDownLatch(1);
    private final VotingNPCStore store;
    private final Path lockPath;
    private final CompletableFuture<List<VotingNPCDefinition>> initial;
    private volatile Map<String, VotingNPCDefinition> definitions = Map.of();
    private volatile boolean available;
    private volatile boolean closed;
    // Only the file worker accesses these resources.
    private FileChannel lockChannel;
    private FileLock lease;
    private boolean storageReady;

    VotingNPCRegistry(Path file) {
        this(file, new VotingNPCStore(file));
    }

    VotingNPCRegistry(Path file, VotingNPCStore store) {
        this.store = store;
        lockPath = file.toAbsolutePath().normalize().resolveSibling(file.getFileName() + ".lock");
        initial = submit(false, this::readDefinitions);
        Thread thread = new Thread(this::work, "VotingPlugin-NPC-storage");
        thread.setDaemon(true);
        try { thread.start(); }
        catch (RuntimeException | Error failure) {
            closed = true;
            initial.completeExceptionally(failure);
            stopped.countDown();
            throw failure;
        }
    }

    CompletionStage<List<VotingNPCDefinition>> initialLoad() { return initial; }
    boolean available() { return !closed && available; }
    List<VotingNPCDefinition> snapshot() { return List.copyOf(definitions.values()); }

    CompletionStage<List<VotingNPCDefinition>> reload() {
        synchronized (admission) {
            if (closed) return rejected("Voting NPC registry is closed");
            var completion = submit(false, this::readDefinitions);
            // A rejected reload changes nothing. The worker publishes readiness
            // under this same short lock, after admission has finished.
            if (!completion.isCompletedExceptionally()) available = false;
            return completion;
        }
    }

    CompletionStage<VotingNPCDefinition> create(VotingNPCDefinition definition) {
        return submit(true, () -> {
            if (definition == null) throw new IllegalArgumentException("Voting NPC definition is required");
            if (definitions.containsKey(definition.id())) throw new ValidationFailure(Rejection.DUPLICATE_ID);
            if (definitions.values().stream().anyMatch(value -> value.identity().equals(definition.identity()))) {
                throw new ValidationFailure(Rejection.DUPLICATE_IDENTITY);
            }
            if (definitions.size() >= VotingNPCStore.MAX_NPCS) throw new ValidationFailure(Rejection.LIMIT);
            var next = new LinkedHashMap<>(definitions);
            next.put(definition.id(), definition);
            publish(next);
            return definition;
        });
    }

    CompletionStage<Boolean> remove(String id) {
        return submit(true, () -> {
            if (!definitions.containsKey(id)) return false;
            var next = new LinkedHashMap<>(definitions);
            next.remove(id);
            publish(next);
            return true;
        });
    }

    private List<VotingNPCDefinition> readDefinitions() throws IOException {
        try {
            ownFile();
            List<VotingNPCDefinition> loaded = store.load();
            var next = new LinkedHashMap<String, VotingNPCDefinition>();
            for (VotingNPCDefinition value : loaded) next.put(value.id(), value);
            definitions = java.util.Collections.unmodifiableMap(next);
            storageReady = true;
            synchronized (admission) { available = true; }
            return loaded;
        } catch (IOException | RuntimeException failure) {
            storageReady = false;
            synchronized (admission) { available = false; }
            throw failure;
        }
    }

    private void publish(LinkedHashMap<String, VotingNPCDefinition> next) throws IOException {
        // The worker holds the process-independent lease through shutdown drain.
        try { store.save(List.copyOf(next.values())); }
        catch (IOException | RuntimeException | Error failure) {
            // A failed write may already be visible. Neither later admitted
            // mutations nor a new request may overwrite it from the old map.
            storageReady = false;
            synchronized (admission) { available = false; }
            throw failure;
        }
        definitions = java.util.Collections.unmodifiableMap(next);
    }

    private <T> CompletableFuture<T> submit(boolean needsReady, Callable<T> action) {
        synchronized (admission) {
            if (closed || (needsReady && !available)) return rejected("Voting NPC storage is not available");
            var completion = new CompletableFuture<T>();
            Runnable task = () -> {
                try {
                    if (needsReady && !storageReady) throw new RejectedExecutionException("Reload valid voting NPC storage before editing");
                    completion.complete(action.call());
                }
                catch (Throwable failure) { completion.completeExceptionally(failure); }
            };
            if (!queue.offer(task)) completion.completeExceptionally(new RejectedExecutionException("Voting NPC storage queue is full"));
            return completion;
        }
    }

    private static <T> CompletableFuture<T> rejected(String message) {
        return CompletableFuture.failedFuture(new RejectedExecutionException(message));
    }

    private void ownFile() throws IOException {
        if (lease != null && lease.isValid()) return;
        if (!Files.isDirectory(lockPath.getParent().toRealPath(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Voting NPC data directory is unavailable");
        }
        FileChannel candidate = FileChannel.open(lockPath, StandardOpenOption.CREATE,
                StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS);
        try {
            FileLock acquired;
            try { acquired = candidate.tryLock(); }
            catch (OverlappingFileLockException overlap) { throw new IOException("Voting NPC file still has an active owner", overlap); }
            if (acquired == null) throw new IOException("Voting NPC file still has an active owner");
            lockChannel = candidate;
            lease = acquired;
        } finally {
            if (lockChannel != candidate) candidate.close();
        }
    }

    private void work() {
        try {
            while (!closed || !queue.isEmpty()) {
                Runnable task;
                try { task = queue.take(); }
                catch (InterruptedException interrupted) { continue; }
                task.run();
            }
        } finally {
            available = false;
            try {
                if (lockChannel != null) lockChannel.close();
            } catch (IOException ignored) {
                // Closing this channel releases its OS lease; never do this I/O
                // under an admission or executor lock on a gameplay caller.
            } finally { stopped.countDown(); }
        }
    }

    /** Stops admission without blocking or interrupting an accepted file write. */
    @Override public void close() {
        synchronized (admission) {
            if (closed) return;
            closed = true;
            queue.offer(() -> { }); // Wake an idle worker; a full queue already wakes it.
        }
    }

    /** Test/owner-worker observation only; gameplay callers must never wait here. */
    boolean awaitClosed(long time, TimeUnit unit) throws InterruptedException { return stopped.await(time, unit); }
}
