package com.bencodez.votingplugin.maintenance;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.LongSupplier;
import java.util.logging.Logger;

import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState;
import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Candidate;
import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Snapshot;
import com.bencodez.votingplugin.util.DurableFiles.PublishedException;

/** Owner-only console reminder; all file work runs on its bounded background worker. */
public final class AutoCreateVoteSitesReminderService {

	public record Settings(boolean enabled, boolean autoCreateEnabled, long afterMillis, long repeatMillis) {
		public Settings {
			if (afterMillis < 0 || repeatMillis <= 0
					|| afterMillis > AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS
					|| repeatMillis > AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS) {
				throw new IllegalArgumentException("Reminder intervals must be non-negative and bounded");
			}
		}
	}

	@FunctionalInterface
	interface StoreFactory {
		AutoCreateVoteSitesReminderStore create(Path directory) throws IOException;
	}

	private final Path directory;
	private final ScheduledExecutorService executor;
	private final Logger logger;
	private final LongSupplier clock;
	private final StoreFactory storeFactory;
	private final long startedNanos;
	private final AtomicBoolean started = new AtomicBoolean();
	private final AtomicBoolean recoveryRequested = new AtomicBoolean();
	private record CloseRequest(CompletableFuture<Void> result, long stoppedNanos) { }
	private final AtomicReference<CloseRequest> closing = new AtomicReference<>();
	private final Object clockGate = new Object();
	private volatile Settings settings;
	private volatile ScheduledFuture<?> task;
	// Accessed only inside the serialized worker checkpoint, never by gameplay callers.
	private AutoCreateVoteSitesReminderStore store;
	private AutoCreateVoteSitesReminderState state;
	private boolean failed;
	private boolean checkpointRequired;
	private boolean waitingReported;

	public AutoCreateVoteSitesReminderService(Path directory, ScheduledExecutorService executor,
			Logger logger, Settings settings) {
		this(directory, executor, logger, settings, System::nanoTime, AutoCreateVoteSitesReminderStore::new);
	}

	AutoCreateVoteSitesReminderService(Path directory, ScheduledExecutorService executor,
			Logger logger, Settings settings, LongSupplier clock, StoreFactory storeFactory) {
		this.directory = Objects.requireNonNull(directory);
		this.executor = Objects.requireNonNull(executor);
		this.logger = Objects.requireNonNull(logger);
		this.settings = Objects.requireNonNull(settings);
		this.clock = Objects.requireNonNull(clock);
		this.storeFactory = Objects.requireNonNull(storeFactory);
		startedNanos = clock.getAsLong();
	}

	public void start() {
		if (closing.get() != null || !started.compareAndSet(false, true)) return;
		try {
			task = executor.scheduleWithFixedDelay(this::tick, 0, 60, TimeUnit.SECONDS);
			if (closing.get() != null) task.cancel(false);
		} catch (RuntimeException rejected) {
			logger.warning("AutoCreateVoteSites setup reminder could not start; voting is unaffected.");
		}
	}

	/** Reload is a snapshot update, never synchronous file I/O or another periodic task. */
	public void reload(Settings next) {
		settings = Objects.requireNonNull(next);
		recoveryRequested.set(true);
	}

	private void tick() {
		long nowNanos;
		synchronized (clockGate) {
			if (closing.get() != null) return;
			nowNanos = clock.getAsLong();
		}
		try {
			checkpoint(nowNanos, false);
		} catch (IOException | RuntimeException failure) {
			logger.warning("AutoCreateVoteSites setup reminder paused after a checkpoint failure; "
					+ "repair its checkpoint if necessary and reload or restart. Voting is unaffected.");
		}
	}

	private synchronized void checkpoint(long nowNanos, boolean finalCheckpoint) throws IOException {
		CloseRequest stopped = closing.get();
		if (stopped != null) {
			if (!finalCheckpoint) return;
			nowNanos = stopped.stoppedNanos();
		}
		if (failed && !recoveryRequested.getAndSet(false)) {
			if (finalCheckpoint) throw new IOException("Reminder checkpoint remains unavailable");
			return;
		}
		try {
			if (store == null) store = storeFactory.create(directory);
			if (!store.tryAcquireLease()) {
				if (finalCheckpoint) throw new IOException("Final reminder checkpoint has no storage ownership");
				if (!waitingReported) {
					logger.warning("AutoCreateVoteSites setup reminder is waiting for a retiring reminder worker; "
							+ "voting is unaffected.");
					waitingReported = true;
				}
				return;
			}
			waitingReported = false;
			if (state == null || failed) {
				Snapshot saved;
				try {
					saved = store.load();
				} catch (IOException | RuntimeException unverified) {
					checkpointRequired = true;
					throw unverified;
				}
				if (saved == null && checkpointRequired) {
					throw new IOException("Reminder checkpoint was removed while running");
				}
				if (saved != null) checkpointRequired = true;
				state = new AutoCreateVoteSitesReminderState(saved == null ? new Snapshot(0, -1) : saved,
						state == null && !failed ? startedNanos : nowNanos);
				failed = false;
			} else if (checkpointRequired && store.load() == null) {
				throw new IOException("Reminder checkpoint was removed while running");
			}
			recoveryRequested.set(false);
			Settings current = settings;
			Candidate candidate = state.propose(nowNanos, !finalCheckpoint && current.enabled(),
					current.autoCreateEnabled(), current.afterMillis(), current.repeatMillis());
			try {
				store.save(candidate.snapshot());
			} catch (PublishedException uncertain) {
				checkpointRequired = true;
				throw uncertain;
			}
			checkpointRequired = true;
			state.commit(candidate);
			// Do not publish a stale warning if shutdown or configuration changed during I/O.
			if (candidate.reminderDue() && closing.get() == null && current.equals(settings)) {
				logger.warning("After voting-site setup, set AutoCreateVoteSites: false in Config.yml.");
			}
		} catch (IOException | RuntimeException failure) {
			failed = true;
			throw failure;
		}
	}

	/** Cancel promptly; enqueue the final checkpoint without blocking the server thread. */
	public CompletableFuture<Void> closeAsync() {
		CloseRequest request;
		synchronized (clockGate) {
			CloseRequest existing = closing.get();
			if (existing != null) return existing.result();
			request = new CloseRequest(new CompletableFuture<>(), clock.getAsLong());
			closing.set(request);
		}
		CompletableFuture<Void> result = request.result();
		long stoppedNanos = request.stoppedNanos();
		ScheduledFuture<?> current = task;
		if (current != null) current.cancel(false);
		if (!started.get()) {
			result.complete(null);
			return result;
		}
		try {
			executor.execute(() -> {
				Exception failure = null;
				try {
					checkpoint(stoppedNanos, true);
				} catch (IOException | RuntimeException checkpointFailure) {
					failure = checkpointFailure;
				} finally {
					if (store != null) {
						try {
							store.releaseLease();
						} catch (IOException | RuntimeException cleanupFailure) {
							if (failure == null) failure = cleanupFailure;
							else failure.addSuppressed(cleanupFailure);
						}
					}
				}
				if (failure == null) result.complete(null);
				else {
					logger.warning("AutoCreateVoteSites reminder checkpoint or cleanup did not complete; "
							+ "inspect its storage before reloading.");
					result.completeExceptionally(failure);
				}
			});
		} catch (RuntimeException rejected) {
			result.completeExceptionally(rejected);
		}
		return result;
	}
}
