package com.bencodez.votingplugin.proxy;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

/** Paces handoffs; durable vote queues, rather than this dispatcher, own votes. */
final class PluginMessageDispatcher {
    static final long INTERVAL = TimeUnit.MILLISECONDS.toNanos(20);
    static final long FAILURE_DELAY = TimeUnit.SECONDS.toNanos(1);
    static final long RETRY_DELAY = TimeUnit.SECONDS.toNanos(10);
    static final long METADATA_LIFETIME = TimeUnit.SECONDS.toNanos(30);
    static final int MAX_BACKENDS = 256;
    static final int MAX_CONTROLS = 2048;
    static final int MAX_LANE_CONTROLS = 256;
    static final int MAX_BYTES = 4 * 1024 * 1024;
    static final int MAX_IN_FLIGHT = 32;
    private final LongSupplier clock;
    private final Supplier<ScheduledExecutorService> executor;
    private final Consumer<String> replay;
    private final Map<String, Lane> lanes = new LinkedHashMap<>();
    private final ThreadLocal<Lane> replaying = new ThreadLocal<>();
    private int controls;
    private int bytes;
    private boolean closed;

    private static final class Lane {
        final String server;
        final ArrayDeque<Control> controls = new ArrayDeque<>();
        final Map<String, Attempt> attempts = new HashMap<>();
        ScheduledFuture<?> wake;
        long generation;
        long next;
        long ownerDue;
        long wakeDue;
        int inFlight;
        boolean sending;
        boolean ownerPending;
        boolean ownerRunning;
        boolean controlTurn;
        int ownerTurn;
        int onlineTurn;
        int broadcastTurn;
        Lane(String server, long now) { this.server = server; next = now; }
    }
    private record Attempt(long due, boolean receipt) { }
    private record Control(int bytes, long due, boolean blocksReplacement, BooleanSupplier handoff) { }

    PluginMessageDispatcher(LongSupplier clock, Supplier<ScheduledExecutorService> executor,
            Consumer<String> replay) {
        this.clock = clock;
        this.executor = executor;
        this.replay = replay;
    }

    private synchronized Lane lane(String server) {
        if (closed || server == null || server.isBlank()) return null;
        String key = server.trim().toLowerCase(Locale.ROOT);
        Lane lane = lanes.get(key);
        if (lane == null) {
            long now = clock.getAsLong();
            if (lanes.size() >= MAX_BACKENDS) {
                lanes.values().removeIf(old -> !old.sending && !old.ownerRunning && !old.ownerPending
                        && old.controls.isEmpty() && old.attempts.isEmpty() && now - old.next >= 0);
            }
            if (lanes.size() >= MAX_BACKENDS) return null;
            lane = new Lane(server, now);
            lanes.put(key, lane);
        }
        return lane;
    }

    synchronized boolean hasBudget(String server, String key, boolean receipt) {
        Lane lane = lane(server);
        if (lane == null) return false;
        long now = clock.getAsLong();
        Attempt previous = key == null ? null : lane.attempts.get(key);
        if (!unavailable(lane, key, receipt, previous, now)) return true;
        defer(lane, key, receipt, previous, now);
        return false;
    }

    private boolean unavailable(Lane lane, String key, boolean receipt, Attempt previous, long now) {
        return lane.sending || now - lane.next < 0
                || replaying.get() != lane && (lane.ownerRunning || !lane.controls.isEmpty())
                || previous != null && now - previous.due() < 0
                || key != null && previous == null && (lane.attempts.size() >= 256
                        || !receipt && lane.inFlight >= MAX_IN_FLIGHT);
    }

    private void defer(Lane lane, String key, boolean receipt, Attempt previous, long now) {
        long due = previous != null && now - previous.due() < 0 ? previous.due()
                : key != null && previous == null && !receipt && lane.inFlight >= MAX_IN_FLIGHT
                        ? lane.attempts.values().stream().mapToLong(Attempt::due).min().orElse(now + RETRY_DELAY)
                        : Math.max(now + INTERVAL, lane.next);
        requestOwner(lane, due);
        schedule(lane);
    }

    boolean trySend(String server, BooleanSupplier handoff) {
        return trySend(server, null, false, handoff);
    }

    boolean trySend(String server, String key, boolean receipt, BooleanSupplier handoff) {
        Lane lane;
        boolean firstAttempt;
        synchronized (this) {
            lane = lane(server);
            if (lane == null) return false;
            long now = clock.getAsLong();
            Attempt previous = key == null ? null : lane.attempts.get(key);
            if (unavailable(lane, key, receipt, previous, now)) {
                defer(lane, key, receipt, previous, now);
                return false;
            }
            firstAttempt = previous == null;
            // Reserve before calling transport: acknowledgements may be reentrant.
            if (key != null) {
                lane.attempts.put(key, new Attempt(now + RETRY_DELAY, receipt));
                if (previous == null && !receipt) lane.inFlight++;
            }
            lane.sending = true;
        }
        boolean accepted = false;
        try {
            accepted = handoff.getAsBoolean();
            return accepted;
        } finally {
            synchronized (this) {
                lane.next = clock.getAsLong() + (accepted ? INTERVAL : FAILURE_DELAY);
                lane.sending = false;
                if (key != null && !accepted && firstAttempt) removeAttempt(lane, key);
                if (key != null && lane.attempts.containsKey(key)) {
                    lane.attempts.put(key, new Attempt(clock.getAsLong() + RETRY_DELAY, receipt));
                }
                if (!closed) requestOwner(lane, lane.next);
                schedule(lane);
            }
        }
    }

    synchronized void acknowledge(String server, String key) {
        Lane lane = lanes.get(server.trim().toLowerCase(Locale.ROOT));
        if (lane != null) {
            removeAttempt(lane, key);
            requestOwner(lane, clock.getAsLong() + INTERVAL);
            schedule(lane);
        }
    }

    private void removeAttempt(Lane lane, String key) {
        Attempt old = lane.attempts.remove(key);
        if (old != null && !old.receipt()) lane.inFlight--;
    }

    private void requestOwner(Lane lane, long due) {
        if (closed) return;
        lane.ownerDue = lane.ownerPending ? Math.min(lane.ownerDue, due) : due;
        lane.ownerPending = true;
    }

    synchronized int ownerTurn(String server, int families) {
        Lane lane = lane(server);
        return lane == null ? 0 : Math.floorMod(lane.ownerTurn++, families);
    }

    final class Batch {
        private final Map<Lane, Control> retained = new LinkedHashMap<>();
        private volatile boolean active;
        private volatile boolean cancelled;
        void activate() { active = true; }
        void cancel() {
            synchronized (PluginMessageDispatcher.this) {
                cancelled = true;
                if (closed) return;
                for (Map.Entry<Lane, Control> entry : retained.entrySet()) {
                    if (entry.getKey().controls.remove(entry.getValue())) { controls--; bytes -= entry.getValue().bytes(); }
                }
            }
        }
    }

    /** Reserves a complete control batch before its owner consumes an activation. */
    synchronized Batch reserveBatch(Map<String, BooleanSupplier> handoffs, int size) {
        if (closed || handoffs.size() > MAX_BACKENDS || size < 0 || size > MAX_BYTES || controls + (long) handoffs.size() > MAX_CONTROLS
                || bytes + (long) size * handoffs.size() > MAX_BYTES) return null;
        Batch batch = new Batch();
        for (Map.Entry<String, BooleanSupplier> entry : handoffs.entrySet()) {
            Lane lane = lane(entry.getKey());
            if (lane == null || lane.controls.size() >= MAX_LANE_CONTROLS || batch.retained.containsKey(lane)) return null;
            batch.retained.put(lane, new Control(size, clock.getAsLong(), true,
                    () -> batch.cancelled || batch.active && entry.getValue().getAsBoolean()));
        }
        if (!lanes.values().containsAll(batch.retained.keySet())) return null;
        for (Map.Entry<Lane, Control> entry : batch.retained.entrySet()) {
            entry.getKey().controls.addLast(entry.getValue()); controls++; bytes += size;
        }
        for (Lane lane : batch.retained.keySet()) {
            if (!schedule(lane)) { batch.cancel(); return null; }
        }
        return batch;
    }

    synchronized int cacheTurn(String server, boolean broadcast, int size) {
        Lane lane = lane(server);
        if (lane == null || size == 0) return 0;
        return Math.floorMod(broadcast ? lane.broadcastTurn++ : lane.onlineTurn++, size);
    }

    synchronized boolean enqueue(String server, int size, long delay, BooleanSupplier handoff) {
        return enqueue(server, size, delay, true, handoff);
    }

    synchronized boolean enqueue(String server, int size, long delay, boolean blocksReplacement, BooleanSupplier handoff) {
        Lane lane = lane(server);
        if (lane == null || size < 0 || size > MAX_BYTES || controls >= MAX_CONTROLS
                || lane.controls.size() >= MAX_LANE_CONTROLS || bytes > MAX_BYTES - size) return false;
        Control control = new Control(size, clock.getAsLong() + Math.max(0, delay), blocksReplacement, handoff);
        lane.controls.addLast(control);
        controls++;
        bytes += size;
        if (!schedule(lane)) {
            lane.controls.remove(control);
            controls--;
            bytes -= size;
            return false;
        }
        return true;
    }

    private boolean schedule(Lane lane) {
        if (closed) return false;
        if (lane.sending || lane.ownerRunning) return true;
        long due = Long.MAX_VALUE;
        if (lane.ownerPending) due = lane.ownerDue;
        for (Control control : lane.controls) due = Math.min(due, control.due());
        if (due == Long.MAX_VALUE) return true;
        long now = clock.getAsLong();
        due = Math.max(Math.max(due, lane.next), now + INTERVAL);
        if (lane.wake != null && !lane.wake.isDone() && !lane.wake.isCancelled() && lane.wakeDue <= due) return true;
        // Generation fencing also invalidates a callback whose cancellation fails.
        if (lane.wake != null) {
            try { lane.wake.cancel(false); } catch (RuntimeException ignored) { }
        }
        long ticket = ++lane.generation;
        try {
            ScheduledExecutorService service = executor.get();
            if (service == null || service.isShutdown()) { lane.wake = null; return false; }
            lane.wakeDue = due;
            lane.wake = service.schedule(() -> drain(lane, ticket), due - now, TimeUnit.NANOSECONDS);
            return true;
        } catch (RuntimeException rejected) {
            lane.wake = null;
            return false;
        }
    }

    private void drain(Lane lane, long ticket) {
        Control control = null;
        boolean owner = false;
        synchronized (this) {
            if (closed || ticket != lane.generation) return;
            lane.wake = null;
            long now = clock.getAsLong();
            // Probes are regenerated periodically. An absent carrier must not
            // retain stale metadata forever; effect-bearing controls stay owned.
            for (var iterator = lane.controls.iterator(); iterator.hasNext();) {
                Control candidate = iterator.next();
                if (!candidate.blocksReplacement() && now - candidate.due() >= METADATA_LIFETIME) {
                    iterator.remove();
                    controls--;
                    bytes -= candidate.bytes();
                }
            }
            if (lane.sending || now - lane.next < 0) { schedule(lane); return; }
            // A delayed control must not block an unrelated ready message.
            for (Control candidate : lane.controls) {
                if (now - candidate.due() >= 0) { control = candidate; break; }
            }
            if (lane.ownerPending && now - lane.ownerDue >= 0 && (control == null || !lane.controlTurn)) {
                owner = true;
                control = null;
                lane.ownerPending = false;
                lane.ownerRunning = true;
                lane.controlTurn = true;
            } else if (control != null) {
                lane.sending = true;
                lane.controlTurn = false;
            }
        }
        if (owner) {
            try {
                replaying.set(lane);
                replay.accept(lane.server);
            } catch (RuntimeException failure) {
                synchronized (this) { requestOwner(lane, clock.getAsLong() + FAILURE_DELAY); lane.next = clock.getAsLong() + FAILURE_DELAY; }
            } finally {
                replaying.remove();
                synchronized (this) { lane.ownerRunning = false; schedule(lane); }
            }
        } else if (control != null) {
            boolean accepted = false;
            try { accepted = control.handoff().getAsBoolean(); }
            catch (RuntimeException failure) { /* Retain for a later handoff. */ }
            finally {
                synchronized (this) {
                    lane.sending = false;
                    lane.next = clock.getAsLong() + (accepted ? INTERVAL : FAILURE_DELAY);
                    if (!closed && accepted && lane.controls.remove(control)) { controls--; bytes -= control.bytes(); }
                    schedule(lane);
                }
            }
        } else synchronized (this) { schedule(lane); }
    }

    synchronized int pendingControls() { return controls; }

    /** Includes selected controls until their handoff has completed. */
    synchronized boolean hasPendingEffects() {
        for (Lane lane : lanes.values())
            for (Control control : lane.controls)
                if (control.blocksReplacement()) return true;
        return false;
    }

    synchronized void close() {
        closed = true;
        for (Lane lane : lanes.values()) {
            lane.generation++;
            if (lane.wake != null) {
                try { lane.wake.cancel(false); } catch (RuntimeException ignored) { }
            }
        }
        lanes.clear();
        controls = bytes = 0;
    }
}
