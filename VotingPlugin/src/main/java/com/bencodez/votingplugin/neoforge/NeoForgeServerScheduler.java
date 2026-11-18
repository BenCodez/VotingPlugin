package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionException;

/** Runs bootstrap work on NeoForge's server tick lane. */
public final class NeoForgeServerScheduler implements AutoCloseable {
    private final ConcurrentLinkedQueue<ScheduledTask> pending = new ConcurrentLinkedQueue<>();
    // Queue removal transfers execution selection, not lifecycle ownership.
    // Guarded by this scheduler's monitor, including task start and shutdown.
    private final Set<ScheduledTask> owned = new HashSet<>();
    private volatile boolean open = true;

    public synchronized void execute(Runnable task) {
        enqueue(task, true);
    }

    CompletableFuture<Void> executeAsync(Runnable task) {
        return enqueue(task, false);
    }

    private CompletableFuture<Void> enqueue(Runnable task, boolean propagateFailure) {
        Objects.requireNonNull(task, "task");
        synchronized (this) {
            if (!open) throw new RejectedExecutionException("NeoForge runtime has stopped");
            ScheduledTask scheduled = new ScheduledTask(task, propagateFailure);
            owned.add(scheduled);
            pending.add(scheduled);
            return scheduled.completion;
        }
    }

    /** Called by the NeoForge server tick event; work submitted during a tick runs on the next tick. */
    public void onServerTick() {
        if (!open) return;
        int count = pending.size();
        for (int i = 0; i < count && open; i++) {
            ScheduledTask task = pending.poll();
            if (task == null) break;
            task.run();
        }
    }

    @Override public synchronized void close() {
        open = false;
        RejectedExecutionException stopped = new RejectedExecutionException("NeoForge runtime has stopped");
        pending.clear();
        for (ScheduledTask task : List.copyOf(owned)) {
            if (!task.started) {
                owned.remove(task);
                task.completion.completeExceptionally(stopped);
            }
        }
    }

    private final class ScheduledTask implements Runnable {
        private final Runnable task;
        private final boolean propagateFailure;
        private final CompletableFuture<Void> completion = new CompletableFuture<>();
        private boolean started;

        private ScheduledTask(Runnable task, boolean propagateFailure) {
            this.task = task;
            this.propagateFailure = propagateFailure;
        }

        @Override public void run() {
            synchronized (NeoForgeServerScheduler.this) {
                if (!open) {
                    owned.remove(this);
                    completion.completeExceptionally(
                            new RejectedExecutionException("NeoForge runtime has stopped"));
                    return;
                }
                // A tick may have removed this task before shutdown drained the queue.
                // Fence task execution and close together so teardown cannot overtake it.
                started = true;
                try {
                    runWhileOpen();
                } finally {
                    owned.remove(this);
                }
            }
        }

        private void runWhileOpen() {
            try {
                task.run();
                completion.complete(null);
            } catch (Throwable failure) {
                completion.completeExceptionally(failure);
                if (propagateFailure) {
                    if (failure instanceof RuntimeException runtime) throw runtime;
                    if (failure instanceof Error error) throw error;
                    throw new IllegalStateException("NeoForge scheduled task failed", failure);
                }
            }
        }
    }
}
