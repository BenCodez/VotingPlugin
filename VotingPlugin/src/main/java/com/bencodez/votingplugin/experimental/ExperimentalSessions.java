package com.bencodez.votingplugin.experimental;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.LongSupplier;

/**
 * Bounded ownership of temporary menus, independent of production inventory registrations.
 * Cleanup actions dispatch to their platform owner; this class never accesses Bukkit state.
 */
final class ExperimentalSessions {
    enum State { CREATING, ACTIVE, CLOSED }

    static final class Session {
        final UUID id = UUID.randomUUID();
        final UUID player;
        final ExperimentalGUIType type;
        final long createdAtNanos;
        final long lifetimeNanos;
        private State state = State.CREATING;
        private final List<Runnable> cleanup = new ArrayList<>();

        private Session(UUID player, ExperimentalGUIType type, long now, long lifetimeNanos) {
            this.player = player;
            this.type = type;
            this.createdAtNanos = now;
            this.lifetimeNanos = lifetimeNanos;
        }

        synchronized State state() { return state; }
        synchronized boolean activate() {
            if (state != State.CREATING) return false;
            state = State.ACTIVE;
            return true;
        }
        boolean expired(long now) { return now - createdAtNanos >= lifetimeNanos; }

        // Registration races with shutdown: late resources must still be retired exactly once.
        void own(Runnable release, Consumer<RuntimeException> errors) {
            Objects.requireNonNull(release);
            boolean full;
            synchronized (this) {
                full = state != State.CLOSED && cleanup.size() >= 128;
                if (state != State.CLOSED) {
                    if (!full) {
                        cleanup.add(release);
                        return;
                    }
                }
            }
            release(release, errors);
            if (full) throw new IllegalStateException("Experimental resource limit reached");
        }

        private void close(Consumer<RuntimeException> errors) {
            List<Runnable> releases;
            synchronized (this) {
                if (state == State.CLOSED) return;
                state = State.CLOSED;
                releases = List.copyOf(cleanup);
                cleanup.clear();
            }
            for (Runnable action : releases) release(action, errors);
        }

        private static void release(Runnable action, Consumer<RuntimeException> errors) {
            try { action.run(); }
            catch (RuntimeException failure) {
                // A reporting failure must not prevent cleanup of the remaining resources.
                try { errors.accept(failure); } catch (RuntimeException ignored) { }
            }
        }
    }

    private final Map<UUID, Session> sessions = new HashMap<>();
    private final LongSupplier clock;
    private final Consumer<RuntimeException> errors;
    private boolean stopped;

    ExperimentalSessions(LongSupplier clock, Consumer<RuntimeException> errors) {
        this.clock = Objects.requireNonNull(clock);
        this.errors = Objects.requireNonNull(errors);
    }

    Session open(UUID player, ExperimentalGUIType type, int limit, int timeoutSeconds) {
        Objects.requireNonNull(player);
        Objects.requireNonNull(type);
        if (limit < 1 || limit > 64 || timeoutSeconds < 1 || timeoutSeconds > 60)
            throw new IllegalArgumentException("Session limit must be 1..64 and timeout 1..60 seconds");
        Session previous;
        Session next;
        synchronized (this) {
            if (stopped || (!sessions.containsKey(player) && sessions.size() >= limit)) return null;
            next = new Session(player, type, clock.getAsLong(), timeoutSeconds * 1_000_000_000L);
            previous = sessions.put(player, next);
        }
        if (previous != null) previous.close(errors);
        return next;
    }

    synchronized boolean current(Session session) {
        return !stopped && sessions.get(session.player) == session
                && session.state() != State.CLOSED && !session.expired(clock.getAsLong());
    }

    void close(Session session) {
        synchronized (this) { sessions.remove(session.player, session); }
        session.close(errors);
    }

    void close(UUID player) {
        Session session;
        synchronized (this) { session = sessions.remove(player); }
        if (session != null) session.close(errors);
    }

    synchronized List<Session> snapshot() { return List.copyOf(sessions.values()); }

    void expire() {
        for (Session session : snapshot()) if (session.expired(clock.getAsLong())) close(session);
    }

    void clear(boolean shutdown) {
        List<Session> retired;
        synchronized (this) {
            if (shutdown) stopped = true;
            retired = List.copyOf(sessions.values());
            sessions.clear();
        }
        for (Session session : retired) session.close(errors);
    }
}
