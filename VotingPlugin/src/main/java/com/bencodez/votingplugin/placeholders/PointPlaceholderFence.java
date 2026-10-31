package com.bencodez.votingplugin.placeholders;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/** Coordinates point rendering with in-flight shared-storage mutations. */
final class PointPlaceholderFence {
	private static final class State {
		long generation;
		int pending;
		int readers;
		int refills;
		boolean uncertain;
		boolean snapshotUnavailable;
	}

	private final Map<UUID, State> states = new HashMap<>();
	private long generation;
	private long lifecycle;
	record Stamp(long lifecycle, long generation) { }

	synchronized Stamp generation(UUID uuid) {
		State state = states.computeIfAbsent(uuid, ignored -> {
			State created = new State();
			created.generation = ++generation;
			return created;
		});
		state.readers++;
		return new Stamp(lifecycle, state.generation);
	}

	synchronized boolean readable(UUID uuid, Stamp observed) {
		if (lifecycle != observed.lifecycle()) return false;
		State state = states.get(uuid);
		return state != null && state.generation == observed.generation()
				&& state.pending == 0 && state.refills == 0 && !state.uncertain && !state.snapshotUnavailable;
	}

	synchronized void release(UUID uuid) {
		State state = states.get(uuid);
		if (state == null || state.readers == 0) throw new IllegalStateException("Unbalanced point reader");
		state.readers--;
		retireIfSafe(uuid, state);
	}

	private void retireIfSafe(UUID uuid, State state) {
		if (state.readers == 0 && state.pending == 0 && state.refills == 0 && !state.uncertain && !state.snapshotUnavailable) states.remove(uuid);
	}

	synchronized void invalidateLifecycle() { lifecycle++; }
	synchronized long lifecycle() { return lifecycle; }
	synchronized boolean recoverable(UUID uuid) {
		State state = states.get(uuid);
		return state != null && state.snapshotUnavailable && !state.uncertain && state.pending == 0 && state.refills == 0;
	}

	synchronized Stamp beginRefill(UUID uuid, Runnable invalidate) {
		State state = states.computeIfAbsent(uuid, ignored -> new State());
		state.generation = ++generation;
		state.snapshotUnavailable = true;
		state.refills++;
		state.readers++;
		invalidate.run();
		return new Stamp(lifecycle, state.generation);
	}

	synchronized void begin(UUID uuid, Runnable invalidate) {
		State state = states.computeIfAbsent(uuid, ignored -> new State());
		state.generation = ++generation;
		state.pending++;
		invalidate.run();
	}

	synchronized void finish(UUID uuid, boolean knownOutcome, boolean trustworthy) {
		State state = states.get(uuid);
		if (state == null || state.pending == 0) throw new IllegalStateException("Unbalanced point mutation");
		state.uncertain |= !knownOutcome;
		state.snapshotUnavailable = !trustworthy || state.refills != 0;
		state.pending--;
		state.generation = ++generation;
		retireIfSafe(uuid, state);
	}

	synchronized void finishAdmission(UUID uuid, boolean workerSettled) {
		State state = states.get(uuid);
		if (state == null || state.pending == 0) throw new IllegalStateException("Unbalanced point admission");
		state.pending--;
		state.snapshotUnavailable |= !workerSettled;
		state.generation = ++generation;
		retireIfSafe(uuid, state);
	}

	synchronized void finishRefill(UUID uuid, Stamp observed, boolean verified) {
		State state = states.get(uuid);
		if (state == null || state.refills == 0) throw new IllegalStateException("Unbalanced point refill");
		boolean current = lifecycle == observed.lifecycle() && state.generation == observed.generation();
		state.refills--;
		state.snapshotUnavailable = !(verified && current && state.pending == 0 && state.refills == 0 && !state.uncertain);
		// Even an obsolete read completion can replace storage data: retire every older render.
		state.generation = ++generation;
	}

	synchronized void publish(UUID uuid, Stamp observed, Runnable publication) {
		if (readable(uuid, observed)) publication.run();
	}
}
