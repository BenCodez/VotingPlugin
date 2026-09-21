package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionException;

/** Runs bootstrap work on NeoForge's server tick lane. */
public final class NeoForgeServerScheduler implements AutoCloseable {
    private final ConcurrentLinkedQueue<Runnable> pending = new ConcurrentLinkedQueue<>();
    private volatile boolean open = true;

    public synchronized void execute(Runnable task) {
        Objects.requireNonNull(task, "task");
        if (!open) throw new RejectedExecutionException("NeoForge runtime has stopped");
        pending.add(task);
    }

    /** Called by the NeoForge server tick event; work submitted during a tick runs on the next tick. */
    public void onServerTick() {
        if (!open) return;
        int count = pending.size();
        for (int i = 0; i < count && open; i++) {
            Runnable task = pending.poll();
            if (task == null) break;
            task.run();
        }
    }

    @Override public synchronized void close() {
        open = false;
        pending.clear();
    }
}
