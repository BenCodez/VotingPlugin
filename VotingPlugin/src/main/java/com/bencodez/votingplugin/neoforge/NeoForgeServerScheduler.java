package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionException;

/** Runs bootstrap work on NeoForge's server tick lane. */
public final class NeoForgeServerScheduler implements AutoCloseable {
    private final ConcurrentLinkedQueue<ScheduledTask> pending = new ConcurrentLinkedQueue<>();
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
        ScheduledTask task;
        while ((task = pending.poll()) != null) task.completion.completeExceptionally(stopped);
    }

    private static final class ScheduledTask implements Runnable {
        private final Runnable task;
        private final boolean propagateFailure;
        private final CompletableFuture<Void> completion = new CompletableFuture<>();

        private ScheduledTask(Runnable task, boolean propagateFailure) {
            this.task = task;
            this.propagateFailure = propagateFailure;
        }

        @Override public void run() {
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
