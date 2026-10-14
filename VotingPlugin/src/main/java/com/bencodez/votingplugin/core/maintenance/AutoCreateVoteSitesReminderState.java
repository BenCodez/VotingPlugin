package com.bencodez.votingplugin.core.maintenance;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Cumulative active-runtime clock for the owner-facing auto-create setup reminder.
 * This class performs no persistence or notification. One owned worker must
 * serialize the entire propose, persist, commit, then notify sequence, including
 * reload and shutdown checkpoints. Method synchronization alone does not serialize
 * the external persistence step. Only notify after a successful commit.
 *
 * <p>Reconstruct this state from its durable snapshot and a fresh monotonic clock
 * reading on startup; never persist the process-local clock anchor. Samples must
 * be less than {@link Long#MAX_VALUE} nanoseconds apart (the normal nanoTime elapsed
 * time contract). Negative clock readings and signed clock wrap are supported.
 */
public final class AutoCreateVoteSitesReminderState {

	public static final long NEVER_REMINDED = -1;
	public static final long DEFAULT_THRESHOLD_MILLIS = TimeUnit.DAYS.toMillis(7);
	public static final long DEFAULT_REPEAT_MILLIS = TimeUnit.DAYS.toMillis(7);
	public static final long MAX_INTERVAL_MILLIS = Long.MAX_VALUE / 1_000_000;
	private static final long NANOS_PER_MILLI = 1_000_000;

	/** Immutable durable data; a reminder marker must refer to accrued runtime. */
	public record Snapshot(long totalRuntimeMillis, long lastReminderRuntimeMillis) {
		public Snapshot {
			if (totalRuntimeMillis < 0 || lastReminderRuntimeMillis < NEVER_REMINDED
					|| lastReminderRuntimeMillis > totalRuntimeMillis) {
				throw new IllegalArgumentException("Invalid reminder runtime snapshot");
			}
		}
	}

	/**
	 * A tentative checkpoint, valid only until this state commits any candidate.
	 * Persist {@link #snapshot()} before committing; discard on a failed write and
	 * propose again. Creating or discarding candidates changes no committed state.
	 */
	public static final class Candidate {
		private final Object epoch;
		private final Snapshot snapshot;
		private final long checkpointNanos;
		private final boolean reminderDue;

		private Candidate(Object epoch, Snapshot snapshot, long checkpointNanos, boolean reminderDue) {
			this.epoch = epoch;
			this.snapshot = snapshot;
			this.checkpointNanos = checkpointNanos;
			this.reminderDue = reminderDue;
		}

		public Snapshot snapshot() {
			return snapshot;
		}

		public boolean reminderDue() {
			return reminderDue;
		}
	}

	private Snapshot snapshot;
	private long checkpointNanos;
	private Object epoch = new Object();

	public AutoCreateVoteSitesReminderState(Snapshot snapshot, long nowNanos) {
		this.snapshot = Objects.requireNonNull(snapshot, "snapshot");
		this.checkpointNanos = nowNanos;
	}

	public synchronized Snapshot snapshot() {
		return snapshot;
	}

	/**
	 * Accrue runtime regardless of either flag. Both flags are required to propose a
	 * reminder, at the inclusive threshold or repeat boundary. Settings are sampled
	 * for each proposal; changing them does not reset accumulated runtime or markers.
	 * Backward samples accrue nothing and leave the clock anchor unchanged.
	 */
	public synchronized Candidate propose(long nowNanos, boolean reminderEnabled, boolean autoCreateEnabled,
			long thresholdMillis, long repeatMillis) {
		validateInterval(thresholdMillis);
		validateInterval(repeatMillis);
		long elapsedNanos = nowNanos - checkpointNanos;
		long elapsedMillis = elapsedNanos > 0 ? elapsedNanos / NANOS_PER_MILLI : 0;
		long total = saturatingAdd(snapshot.totalRuntimeMillis(), elapsedMillis);
		long lastReminder = snapshot.lastReminderRuntimeMillis();
		boolean due = reminderEnabled && autoCreateEnabled && total >= thresholdMillis
				&& (lastReminder == NEVER_REMINDED || total - lastReminder >= repeatMillis);
		Snapshot next = new Snapshot(total, due ? total : lastReminder);
		// Consume only whole milliseconds so frequent checkpoints do not lose time.
		long nextCheckpoint = checkpointNanos + elapsedMillis * NANOS_PER_MILLI;
		return new Candidate(epoch, next, nextCheckpoint, due);
	}

	/**
	 * Publish a checkpoint only after its durable write succeeded. A successful
	 * commit invalidates every other candidate from that epoch, including this one.
	 * Foreign, stale, or duplicate candidates are rejected without changing state.
	 */
	public synchronized void commit(Candidate candidate) {
		Objects.requireNonNull(candidate, "candidate");
		if (candidate.epoch != epoch) {
			throw new IllegalArgumentException("Candidate is not current for this reminder state");
		}
		snapshot = candidate.snapshot;
		checkpointNanos = candidate.checkpointNanos;
		epoch = new Object();
	}

	private static void validateInterval(long millis) {
		if (millis <= 0 || millis > MAX_INTERVAL_MILLIS) {
			throw new IllegalArgumentException("Reminder interval is outside the supported range");
		}
	}

	private static long saturatingAdd(long total, long elapsed) {
		return elapsed > Long.MAX_VALUE - total ? Long.MAX_VALUE : total + elapsed;
	}
}
