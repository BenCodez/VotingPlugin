package com.bencodez.votingplugin.user;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/** Bounded ownership of admitted player callbacks until they actually start. */
public final class OfflineVoteOwnerHandoffs {
    private static final int LIMIT = 4096;
    private final Map<CompletableFuture<Void>, Pending> pending = new HashMap<>();
    private boolean paused;
    private boolean closed;

    private static final class Pending {
        final CompletableFuture<Void> result;
        final Consumer<BooleanSupplier> submission;
        long generation;
        Pending(CompletableFuture<Void> result, Consumer<BooleanSupplier> submission) {
            this.result = result;
            this.submission = submission;
        }
    }

    public void submit(CompletableFuture<Void> result, Consumer<BooleanSupplier> submission) {
        Pending entry = new Pending(result, submission);
        synchronized (this) {
            if (closed || pending.size() >= LIMIT) {
                result.completeExceptionally(new IllegalStateException("Offline replay owner admission unavailable"));
                return;
            }
            pending.put(result, entry);
        }
        schedule(entry);
    }

    private void schedule(Pending entry) {
        final long generation;
        synchronized (this) {
            if (closed || paused || pending.get(entry.result) != entry) return;
            generation = ++entry.generation;
        }
        try {
            entry.submission.accept(() -> begin(entry, generation));
        } catch (RuntimeException | Error failure) {
            if (begin(entry, generation)) entry.result.completeExceptionally(failure);
        }
    }

    private synchronized boolean begin(Pending entry, long generation) {
        if (closed || paused || entry.generation != generation || pending.get(entry.result) != entry) return false;
        pending.remove(entry.result);
        return true;
    }

    /** Invalidate callbacks the old scheduler may cancel; retain their original completion. */
    public synchronized void pause() {
        paused = true;
        pending.values().forEach(entry -> entry.generation++);
    }

    /** Resubmit only callbacks that never started, after configuration/storage replacement. */
    public void resume() {
        List<Pending> entries;
        synchronized (this) {
            if (closed) return;
            paused = false;
            entries = List.copyOf(pending.values());
        }
        entries.forEach(this::schedule);
    }

    public void close() {
        List<Pending> entries;
        synchronized (this) {
            closed = true;
            paused = true;
            entries = List.copyOf(pending.values());
            pending.clear();
        }
        entries.forEach(entry -> entry.result.completeExceptionally(
                new IllegalStateException("Offline replay owner callback retired on shutdown")));
    }

    synchronized int pendingCount() { return pending.size(); }
}
