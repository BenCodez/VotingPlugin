package com.bencodez.votingplugin.listeners;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Properties;
import java.util.UUID;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.bukkit.configuration.file.YamlConfiguration;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.DurableFiles;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

/**
 * Durable overflow for votes that cannot currently be admitted to the bounded
 * vote executor. The queue is drained from a daemon worker and never blocks
 * the Bukkit event thread.
 */
public final class VotifierVoteOverflowQueue implements AutoCloseable {
	/* Keep the overflow bounded independently of the executor's 256 admissions. */
	private static final int MAX_ENTRIES = 256;
    public static final int MAX_CAPTURED = 256;
    private static final int MAX_RETAINED_ENTRIES = MAX_ENTRIES + MAX_CAPTURED;
	private static final long MAX_FILE_BYTES = 4L * 1024L * 1024L;
	private static final long RETRY_DELAY_MILLIS = 250L;
	private static final String QUEUE_FILE = "VotifierVoteQueue.yml";
	private static final String OWNER_FILE = "VotifierVoteQueue.yml.owner";
	private static final String SHARED_OWNER_PROPERTY_PREFIX = "com.bencodez.votingplugin.VotifierVoteOverflowQueue.owner.";
	private static final int MAX_OWNER_RESERVATIONS = 1024;

	private final VotingPluginMain plugin;
	@FunctionalInterface
	public interface VoteProcessor { void process(String serviceSite, String username, long occurredAt); }
	@FunctionalInterface
	public interface OccurrenceProcessor {
        void process(String serviceSite, String username, long occurredAt, UUID localOccurrenceId);
        /** Legacy callbacks confirm completion by returning normally. */
        default boolean processConfirmed(String serviceSite, String username, long occurredAt, UUID localOccurrenceId) {
            process(serviceSite, username, occurredAt, localOccurrenceId);
            return true;
        }
    }
    @FunctionalInterface
    public interface ConfirmedOccurrenceProcessor {
        boolean processConfirmed(String serviceSite, String username, long occurredAt, UUID localOccurrenceId);
    }
	private final OccurrenceProcessor processor;
    private final java.util.function.BooleanSupplier processingAllowed;
	private final Path file;
	private final Path ownerFile;
	private FileChannel ownerChannel;
	private FileLock ownerLock;
	private String ownerKey;
	private String ownerToken;
	private Properties ownerProperties;
	private final ScheduledThreadPoolExecutor worker;
	private final Object lock = new Object();
	private final Object persistenceWriteLock = new Object();
	private final ArrayDeque<PendingVote> entries = new ArrayDeque<>();
	private boolean drainScheduled;
    private boolean drainRetryScheduled;
	private boolean captureRetryScheduled;
	private boolean persistenceScheduled;
	private boolean persistenceDirty;
	private boolean started;
	private boolean closed;
    private boolean closing;
    private final java.util.concurrent.CompletableFuture<Void> closePublication = new java.util.concurrent.CompletableFuture<>();
    private boolean closeFailureReported;
    private volatile boolean loaded;
    private volatile boolean loadFailed;
    private int pendingIngress;
    private int pendingCaptureAdmissions;
    private int pendingRetirementCaptures;
    private boolean retirementRequested;
    private boolean initializationRetryScheduled;
    private boolean initializationFailureReported;
    private boolean queueFileObserved;
    private final Set<UUID> captureReservations = new java.util.HashSet<>();
    private final boolean asynchronousInitialization;
	private long stateVersion;
	private long durableVersion;

	/**
	 * Creates and loads the overflow queue.
	 *
	 * @param plugin the owning plugin
	 * @param processor callback receiving service site and player name
	 */
	public VotifierVoteOverflowQueue(VotingPluginMain plugin, BiConsumer<String, String> processor) {
        this(plugin, (site, user, occurredAt, id) -> processor.accept(site, user), () -> true);
    }

    /** Additive callback retains receipt occurrence and fences a retired startup generation. */
    public VotifierVoteOverflowQueue(VotingPluginMain plugin, VoteProcessor processor,
            java.util.function.BooleanSupplier processingAllowed) {
        this(plugin, (site, user, time, id) -> processor.process(site, user, time), processingAllowed);
    }

    public VotifierVoteOverflowQueue(VotingPluginMain plugin, OccurrenceProcessor processor,
            java.util.function.BooleanSupplier processingAllowed) {
        this(plugin, processor, processingAllowed, false);
    }

    /** Memory-only construction; all loading and handoffs run on the existing queue worker. */
    public static VotifierVoteOverflowQueue initializeAsync(VotingPluginMain plugin, OccurrenceProcessor processor,
            java.util.function.BooleanSupplier processingAllowed) {
        return new VotifierVoteOverflowQueue(plugin, processor, processingAllowed, true);
    }

    /** Named factory preserves existing void callback/lambda contracts without ambiguous overloads. */
    public static VotifierVoteOverflowQueue initializeConfirmedAsync(VotingPluginMain plugin,
            ConfirmedOccurrenceProcessor processor, java.util.function.BooleanSupplier processingAllowed) {
        java.util.Objects.requireNonNull(processor, "processor");
        return initializeAsync(plugin, new OccurrenceProcessor() {
            @Override public void process(String site, String user, long time, UUID id) {
                processor.processConfirmed(site, user, time, id);
            }
            @Override public boolean processConfirmed(String site, String user, long time, UUID id) {
                return processor.processConfirmed(site, user, time, id);
            }
        }, processingAllowed);
    }

    private VotifierVoteOverflowQueue(VotingPluginMain plugin, OccurrenceProcessor processor,
            java.util.function.BooleanSupplier processingAllowed, boolean asynchronousInitialization) {
        this.asynchronousInitialization = asynchronousInitialization;
		this.plugin = plugin;
		this.processor = processor;
        this.processingAllowed = processingAllowed;
		this.file = new File(plugin.getDataFolder(), QUEUE_FILE).toPath();
		this.ownerFile = new File(plugin.getDataFolder(), OWNER_FILE).toPath();
		this.worker = new ScheduledThreadPoolExecutor(1, runnable -> {
			Thread thread = new Thread(runnable, "VotingPlugin-Votifier-Overflow");
			thread.setDaemon(true);
			return thread;
		});
		this.worker.setRemoveOnCancelPolicy(true);
		if (asynchronousInitialization) worker.execute(this::load);
        else load();
	}

	/**
	 * Starts draining persisted and newly queued votes. Call this only after the
	 * Votifier and vote-processing listeners have been registered so a recovered
	 * vote cannot be processed before its event handlers are available.
	 */
	public void start() {
		synchronized (lock) {
			if (closed || started) return;
			started = true;
			scheduleDrainLocked();
		}
	}

	/**
	 * Adds a validated vote to the durable queue. This method only takes a small
	 * monitor and schedules file I/O on the daemon worker.
	 *
	 * @param username the validated player name
	 * @param serviceSite the validated service site
	 * @return false when the bounded overflow is full or shutting down
	 */
	public boolean enqueue(String username, String serviceSite) {
        return enqueue(username, serviceSite, System.currentTimeMillis());
    }

    public boolean enqueue(String username, String serviceSite, long occurredAt) {
        return enqueue(username, serviceSite, occurredAt, UUID.randomUUID());
    }

    public boolean enqueue(String username, String serviceSite, long occurredAt, UUID localOccurrenceId) {
        return enqueueStored(username, serviceSite, occurredAt, localOccurrenceId, false);
    }

    /** Transfer stays behind initial loading and is bounded independently of executor admissions. */
    public void enqueueAfterInitialization(String username, String serviceSite, long occurredAt, UUID localOccurrenceId,
            Consumer<Boolean> completion) {
        enqueueAfterInitialization(username, serviceSite, occurredAt, localOccurrenceId, () -> true, completion);
    }

    /** A STARTED original callback retains execution ownership until its outcome is known. */
    public void enqueueAfterInitialization(String username, String serviceSite, long occurredAt, UUID localOccurrenceId,
            java.util.function.BooleanSupplier executionReleased, Consumer<Boolean> completion) {
        if (username == null || serviceSite == null || occurredAt <= 0 || localOccurrenceId == null) {
            completion.accept(false);
            return;
        }
        var finished = new java.util.concurrent.atomic.AtomicBoolean();
        class Transfer implements Runnable {
            void finish(boolean accepted) {
                if (!finished.compareAndSet(false, true)) return;
                synchronized (lock) { pendingIngress--; }
                completion.accept(accepted);
            }
            @Override public void run() {
                try {
                    synchronized (lock) {
                        if (!closed && !loaded) {
                            // Admission owns this receipt, including the saturated
                            // path which has no entry in the listener's capture map.
                            scheduleInitializationRetryLocked();
                            worker.schedule(this, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
                            return;
                        }
                    }
                } catch (RejectedExecutionException rejected) { finish(false); return; }
                boolean accepted = false;
                try { accepted = enqueueStored(username, serviceSite, occurredAt, localOccurrenceId, true, executionReleased); }
                finally { finish(accepted); }
            }
        }
        boolean submitted = false;
        synchronized (lock) {
            if (!closed && !closing && pendingIngress < MAX_ENTRIES) {
                pendingIngress++;
                try {
                    worker.execute(new Transfer());
                    submitted = true;
                } catch (RejectedExecutionException rejected) { pendingIngress--; }
            }
        }
        if (!submitted) completion.accept(false);
    }

    private boolean enqueueStored(String username, String serviceSite, long occurredAt, UUID localOccurrenceId,
            boolean admittedTransfer) {
        return enqueueStored(username, serviceSite, occurredAt, localOccurrenceId, admittedTransfer, () -> true);
    }

    private boolean enqueueStored(String username, String serviceSite, long occurredAt, UUID localOccurrenceId,
            boolean admittedTransfer, java.util.function.BooleanSupplier executionReleased) {
		if (username == null || serviceSite == null || occurredAt <= 0 || localOccurrenceId == null) return false;
		synchronized (lock) {
            if (closed || closing && !admittedTransfer || !loaded || loadFailed) return false;
            var existing = occurrence(localOccurrenceId);
            if (existing != null) {
                boolean same = matches(existing, username, serviceSite, occurredAt);
                if (same) captureReservations.remove(localOccurrenceId);
                return same;
            }
            int entryLimit = closing && admittedTransfer ? MAX_RETAINED_ENTRIES : MAX_ENTRIES;
            if (entries.size() >= entryLimit || !captureReservations.contains(localOccurrenceId)
                    && entries.size() + captureReservations.size() >= MAX_RETAINED_ENTRIES) return false;
            entries.addLast(new PendingVote(username, serviceSite, occurredAt, localOccurrenceId, executionReleased));
            captureReservations.remove(localOccurrenceId);
			stateVersion++;
			requestPersistenceLocked();
			scheduleDrainLocked();
			return true;
		}
	}

    public enum CaptureAdmission { ACCEPTED, WAITING_FOR_LOAD, REJECTED }

    /** Reserve final-snapshot capacity before the normal executor owns this receipt. */
    public CaptureAdmission reserveCapture(UUID id) {
        java.util.Objects.requireNonNull(id);
        synchronized (lock) {
            if (closed || closing || loadFailed) return CaptureAdmission.REJECTED;
            if (!loaded) return CaptureAdmission.WAITING_FOR_LOAD;
            if (captureReservations.contains(id)) return CaptureAdmission.ACCEPTED;
            if (captureReservations.size() >= MAX_CAPTURED
                    || entries.size() + captureReservations.size() >= MAX_RETAINED_ENTRIES)
                return CaptureAdmission.REJECTED;
            captureReservations.add(id);
            return CaptureAdmission.ACCEPTED;
        }
    }

    /** Unknown disk capacity is provisional; keep at most one deferred admission per capture. */
    public void reserveCaptureAfterInitialization(UUID id, Consumer<Boolean> completion) {
        java.util.Objects.requireNonNull(id); java.util.Objects.requireNonNull(completion);
        boolean admitted;
        synchronized (lock) {
            admitted = !closed && !closing && pendingCaptureAdmissions < MAX_CAPTURED;
            if (admitted) pendingCaptureAdmissions++;
        }
        if (!admitted) { completion.accept(false); return; }
        var finished = new java.util.concurrent.atomic.AtomicBoolean();
        class Admission implements Runnable {
            void finish(boolean accepted) {
                if (!finished.compareAndSet(false, true)) return;
                synchronized (lock) { pendingCaptureAdmissions--; }
                completion.accept(accepted);
            }
            @Override public void run() {
                var result = reserveCapture(id);
                if (result != CaptureAdmission.WAITING_FOR_LOAD) {
                    finish(result == CaptureAdmission.ACCEPTED);
                    return;
                }
                try { worker.schedule(this, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS); }
                catch (RejectedExecutionException rejected) { finish(false); }
            }
        }
        var admission = new Admission();
        try { worker.execute(admission); }
        catch (RejectedExecutionException rejected) { admission.finish(false); }
    }

    public void releaseCapture(UUID id) {
        synchronized (lock) { captureReservations.remove(id); }
    }

    public record RetiringCapture(String username, String serviceSite, long time, UUID id,
            java.util.function.BooleanSupplier executionReleased,
            java.util.function.BooleanSupplier completed, Consumer<Boolean> completion) {
        public RetiringCapture {
            java.util.Objects.requireNonNull(username); java.util.Objects.requireNonNull(serviceSite);
            java.util.Objects.requireNonNull(id); java.util.Objects.requireNonNull(executionReleased);
            java.util.Objects.requireNonNull(completed); java.util.Objects.requireNonNull(completion);
            if (time <= 0) throw new IllegalArgumentException("capture receipt time must be positive");
        }
    }

    /** One FIFO batch, before close's final snapshot; normal overflow admission stays capped at256. */
    public void retireCaptures(List<RetiringCapture> captures) {
        var snapshot = List.copyOf(captures);
        if (snapshot.size() > MAX_CAPTURED) throw new IllegalArgumentException("capture retirement exceeds capacity");
        if (snapshot.isEmpty()) return;
        class Transfer implements Runnable {
            @Override public void run() {
                synchronized (lock) {
                    if (!closed && (!loaded || loadFailed)) {
                        scheduleInitializationRetryLocked();
                        worker.schedule(this, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
                        return;
                    }
                }
                boolean[] results = new boolean[snapshot.size()];
                try {
                    for (int index = 0; index < snapshot.size(); index++) {
                        var capture = snapshot.get(index);
                        boolean completed = false;
                        try { completed = capture.completed().getAsBoolean(); }
                        catch (RuntimeException | Error unknown) { /* Retain unconfirmed completion. */ }
                        synchronized (lock) {
                            if (!closed && loaded && !loadFailed) {
                                var existing = occurrence(capture.id());
                                if (completed) {
                                    captureReservations.remove(capture.id());
                                    results[index] = true;
                                } else if (existing != null) {
                                    results[index] = matches(existing, capture.username(), capture.serviceSite(), capture.time());
                                    if (results[index]) captureReservations.remove(capture.id());
                                } else if (entries.size() < MAX_RETAINED_ENTRIES
                                        && (captureReservations.contains(capture.id())
                                            || entries.size() + captureReservations.size() < MAX_RETAINED_ENTRIES)) {
                                    entries.addLast(new PendingVote(capture.username(), capture.serviceSite(), capture.time(),
                                            capture.id(), capture.executionReleased()));
                                    captureReservations.remove(capture.id());
                                    stateVersion++; requestPersistenceLocked();
                                    results[index] = true;
                                }
                            }
                        }
                    }
                    // Retain the whole batch before any external completion callback.
                    for (int index = 0; index < snapshot.size(); index++) {
                        try { snapshot.get(index).completion().accept(results[index]); }
                        catch (RuntimeException | Error failure) {
                            try { plugin.getLogger().warning("Votifier retirement callback failed; queue ownership is retained"); }
                            catch (RuntimeException | Error ignored) { }
                        }
                    }
                } finally {
                    synchronized (lock) { pendingRetirementCaptures -= snapshot.size(); }
                }
            }
        }
        boolean submitted = false;
        synchronized (lock) {
            if (!closed && !closing && pendingRetirementCaptures + snapshot.size() <= MAX_CAPTURED) {
                pendingRetirementCaptures += snapshot.size();
                try { worker.execute(new Transfer()); retirementRequested = true; submitted = true; }
                catch (RejectedExecutionException rejected) { pendingRetirementCaptures -= snapshot.size(); }
            }
        }
        if (!submitted) for (var capture : snapshot) {
            try { capture.completion().accept(false); }
            catch (RuntimeException | Error ignored) { /* Reject every bounded caller, including the suffix. */ }
        }
    }

    private void scheduleInitializationRetryLocked() {
        if (closed || initializationRetryScheduled || loaded && !loadFailed) return;
        initializationRetryScheduled = true;
        worker.schedule(() -> {
            synchronized (lock) {
                initializationRetryScheduled = false;
                if (closed) return;
            }
            load();
        }, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
    }

    private PendingVote occurrence(UUID id) {
        for (var vote : entries) if (vote.localOccurrenceId.equals(id)) return vote;
        return null;
    }

    private static boolean matches(PendingVote vote, String username, String site, long time) {
        return vote.username.equals(username) && vote.serviceSite.equals(site) && vote.time == time;
    }

    /** Completion only; transfers separately report load/capacity admission failure. */
    public boolean isInitializationComplete() { return loaded; }

	/**
	 * Returns the number of votes waiting for admission or completion.
	 *
	 * @return queue size
	 */
	public int size() {
		synchronized (lock) {
			return entries.size();
		}
	}

	private void scheduleDrain() {
		synchronized (lock) {
			scheduleDrainLocked();
		}
	}

	private void scheduleDrainLocked() {
		if (closed || closing || !loaded || loadFailed || !started || drainScheduled) return;
		drainScheduled = true;
		try {
			worker.schedule(this::drain, 0, TimeUnit.MILLISECONDS);
		} catch (RejectedExecutionException ignored) {
			drainScheduled = false;
		}
	}

	private void drain() {
		while (true) {
            // A generation fence parks its receipts instead of repeatedly submitting
            // callbacks which are forbidden to execute (including inline submitters).
            if (!processingAllowed.getAsBoolean()) {
                synchronized (lock) { drainScheduled = false; }
                return;
            }
			synchronized (lock) {
                if (closed || closing || !loaded || loadFailed) { drainScheduled = false; return; }
				if (durableVersion != stateVersion) {
					drainScheduled = false;
					return;
				}
				PendingVote pending = nextUnsubmittedLocked();
				if (pending == null) {
					drainScheduled = false;
					return;
				}
				pending.submitted = true;
				try {
					// Serialize admission with enqueue so the version proven durable
					// above cannot change in the gap before submit accepts this vote.
					plugin.getVoteTimer().submit(() -> {
						boolean attempted = false;
						boolean completed = false;
						try {
                            if (!processingAllowed.getAsBoolean()) return;
                            synchronized (lock) {
                                if (closed || closing || !entries.contains(pending)) return;
                            }
                            attempted = true;
							completed = processor.processConfirmed(pending.serviceSite, pending.username,
                                    pending.time, pending.localOccurrenceId) && !Thread.currentThread().isInterrupted();
                        } catch (RuntimeException | Error failure) {
                            plugin.getLogger().warning("Queued Votifier processing failed; retained receipt for retry: "
                                    + failure.getClass().getSimpleName());
                            throw failure;
						} finally {
                            if (completed) acknowledge(pending);
                            else retainFailedAttempt(pending, attempted);
						}
					});
				} catch (RejectedExecutionException rejected) {
					pending.submitted = false;
					drainScheduled = false;
					try {
						worker.schedule(this::drain, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
						drainScheduled = true;
					} catch (RejectedExecutionException ignored) {
						// The queue is closing; the persisted entry remains for restart.
					}
					return;
				} catch (RuntimeException | Error failure) {
                    // An inline submitter can expose callback failure on this worker.
                    // Retain retry ownership without leaving drainScheduled stuck true.
                    drainScheduled = false;
                    retainFailedAttempt(pending, true);
                    throw failure;
				}
			}
		}
	}

	private PendingVote nextUnsubmittedLocked() {
		long now = System.nanoTime();
        boolean retryWaiting = false;
		for (PendingVote pending : entries) {
			if (pending.submitted || !pending.executionReleased.getAsBoolean()) continue;
            if (pending.retryNotBeforeNanos != 0 && now - pending.retryNotBeforeNanos < 0) {
                retryWaiting = true;
                continue;
            }
            return pending;
		}
        if (retryWaiting) scheduleDrainRetryLocked();
		return null;
	}

    private void retainFailedAttempt(PendingVote pending, boolean attempted) {
        synchronized (lock) {
            pending.submitted = false;
            if (closed || closing || !entries.contains(pending) || !attempted) return;
            // Inline submitters must not retry this same receipt in the current drain.
            pending.retryNotBeforeNanos = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(RETRY_DELAY_MILLIS);
            scheduleDrainRetryLocked();
        }
    }

    private void scheduleDrainRetryLocked() {
        if (closed || closing || !loaded || loadFailed || !started || drainRetryScheduled) return;
        drainRetryScheduled = true;
        try {
            worker.schedule(() -> {
                synchronized (lock) {
                    drainRetryScheduled = false;
                    scheduleDrainLocked();
                }
            }, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
        } catch (RejectedExecutionException rejected) { drainRetryScheduled = false; }
    }

	private void acknowledge(PendingVote pending) {
		synchronized (lock) {
            if (closed) {
                plugin.getLogger().warning("Completed Votifier callback outlived queue shutdown; retained occurrence requires reconciliation");
                return;
            }
			if (!entries.remove(pending)) return;
			stateVersion++;
			requestPersistenceLocked();
			scheduleDrainLocked();
		}
	}

    /** Reconcile a receipt whose original normal callback completed during transfer. */
    public void acknowledgeOccurrence(UUID localOccurrenceId) {
        synchronized (lock) {
            if (closed) {
                // A nonterminating callback outlived the final snapshot. A retired
                // writer cannot overwrite a successor's queue; retain uncertainty.
                plugin.getLogger().warning("Completed Votifier callback outlived queue shutdown; retained occurrence requires reconciliation");
                return;
            }
            if (!entries.removeIf(pending -> pending.localOccurrenceId.equals(localOccurrenceId))) return;
            stateVersion++; requestPersistenceLocked(); scheduleDrainLocked();
        }
    }
    /** Wake the same bounded drain when an original transferred callback finishes uncertainly. */
    public void resumeTransfers() { scheduleDrain(); }

    /** One wakeup for the ingress listener's bounded failed-transfer captures.
     * Retry runs outside the queue monitor and never adds another executor or waiter list. */
    public void retryCaptureTransfers(Runnable retry) {
        synchronized (lock) {
            if (closed || closing || captureRetryScheduled) return;
            captureRetryScheduled = true;
            try {
                worker.schedule(() -> {
                    synchronized (lock) {
                        captureRetryScheduled = false;
                        if (closed || closing) return;
                    }
                    retry.run();
                }, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
            } catch (RejectedExecutionException rejected) { captureRetryScheduled = false; }
        }
    }

	private void requestPersistenceLocked() {
		persistenceDirty = true;
		if (persistenceScheduled) return;
		persistenceScheduled = true;
		try {
			worker.execute(this::persistLoop);
		} catch (RejectedExecutionException ignored) {
			persistenceScheduled = false;
		}
	}

	private void persistLoop() {
		while (true) {
			List<PendingVote> snapshot;
			long snapshotVersion;
			synchronized (lock) {
				if (!persistenceDirty) {
					persistenceScheduled = false;
					return;
				}
				persistenceDirty = false;
				snapshot = new ArrayList<>(entries);
				snapshotVersion = stateVersion;
			}
			try {
				writeSnapshot(snapshot);
			} catch (IOException failure) {
				plugin.getLogger().warning("Unable to persist queued Votifier votes: " + failure.getClass().getSimpleName());
				synchronized (lock) {
					persistenceDirty = true;
					try {
						worker.schedule(this::persistLoop, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
					} catch (RejectedExecutionException ignored) {
						persistenceScheduled = false;
					}
				}
				return;
			}
			synchronized (lock) {
				durableVersion = Math.max(durableVersion, snapshotVersion);
				if (!persistenceDirty) {
					persistenceScheduled = false;
					scheduleDrainLocked();
					return;
				}
			}
		}
	}

	private void load() {
        ArrayDeque<PendingVote> recovered = new ArrayDeque<>();
        boolean successful = false;
		try {
			synchronized (lock) { if (closed || loaded && !loadFailed) return; }
			if (!acquireOwner()) {
                synchronized (lock) {
                    if (asynchronousInitialization && !closing || pendingRetirementCaptures > 0 || pendingIngress > 0) {
                        scheduleInitializationRetryLocked();
                        return;
                    }
                }
				throw new IOException("queue file is owned by another active instance");
			}
            if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) {
                if (queueFileObserved) throw new IOException("queue file disappeared before recovery");
                successful = true;
                return;
            }
            queueFileObserved = true;
			YamlConfiguration yaml = new YamlConfiguration();
			yaml.loadFromString(readQueueFile());
			Object raw = yaml.get("Votes");
			if (!(raw instanceof List<?> values)) throw new IOException("queue votes are not a list");
            if (values.size() > MAX_RETAINED_ENTRIES) throw new IOException("queue exceeds entry capacity");
			boolean skipped = false;
			for (Object value : values) {
				if (!(value instanceof Map<?, ?> map)) {
					skipped = true;
					continue;
				}
				Object username = map.get("Username");
				Object serviceSite = map.get("ServiceSite");
				Object time = map.get("Time");
				if (!(username instanceof String name) || !(serviceSite instanceof String site)
						|| !(time instanceof Number timestamp)
						|| !MinecraftUsernameValidator.isValid(name, plugin.getOptions().getBedrockPlayerPrefix())
						|| !ServiceSiteValidator.isValid(site) || timestamp.longValue() <= 0) {
					skipped = true;
					continue;
				}
				UUID localOccurrenceId;
                if (!map.containsKey("LocalOccurrenceId")) {
                    localOccurrenceId = UUID.randomUUID();
                    skipped = true; // Publish the migration before admitting any recovered row.
                } else {
                    Object rawId = map.get("LocalOccurrenceId");
                    try {
                        if (!(rawId instanceof String id)) throw new IllegalArgumentException();
                        localOccurrenceId = UUID.fromString(id);
                        if (!localOccurrenceId.toString().equalsIgnoreCase(id)) throw new IllegalArgumentException();
                    } catch (IllegalArgumentException malformed) {
                        skipped = true;
                        continue; // Explicit corrupt IDs cannot become fresh occurrences.
                    }
                }
                recovered.addLast(new PendingVote(name, site, timestamp.longValue(), localOccurrenceId));
			}
            synchronized (lock) { entries.clear(); entries.addAll(recovered); }
			if (skipped) {
				synchronized (lock) {
					stateVersion++;
					requestPersistenceLocked();
				}
			}
            successful = true;
		} catch (Exception failure) {
            loadFailed = true;
            if (!initializationFailureReported) {
                initializationFailureReported = true;
                plugin.getLogger().warning("Unable to load queued Votifier votes: " + failure.getClass().getSimpleName());
            }
		} finally {
            if (ownerLock == null && !loadFailed) return;
            if (successful) loadFailed = false;
            loaded = true;
            synchronized (lock) {
                scheduleDrainLocked();
                if (loadFailed && pendingRetirementCaptures > 0) scheduleInitializationRetryLocked();
            }
        }
	}

	private boolean acquireOwner() throws IOException {
		if (ownerLock != null && ownerLock.isValid()) return true;
		Path parent = ownerFile.toAbsolutePath().normalize().getParent();
		if (parent == null || Files.isSymbolicLink(ownerFile)) throw new IOException("queue ownership file is unsafe");
		Files.createDirectories(parent);
		String key = SHARED_OWNER_PROPERTY_PREFIX + hashOwnerPath(parent.toRealPath().resolve(ownerFile.getFileName()));
		String token = UUID.randomUUID().toString();
		Properties properties = System.getProperties();
		synchronized (properties) {
			if (properties.getProperty(key) != null) return false;
			int reservations = 0;
			for (Object property : properties.keySet())
				if (property instanceof String name && name.startsWith(SHARED_OWNER_PROPERTY_PREFIX)) reservations++;
			if (reservations >= MAX_OWNER_RESERVATIONS) return false;
			properties.setProperty(key, token);
		}
		try {
			FileChannel channel = FileChannel.open(ownerFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE,
					LinkOption.NOFOLLOW_LINKS);
			try {
				FileLock acquired;
				try { acquired = channel.tryLock(); }
				catch (OverlappingFileLockException busy) { acquired = null; }
				if (acquired == null) { channel.close(); clearOwnerReservation(properties, key, token); return false; }
				ownerKey = key; ownerToken = token; ownerProperties = properties; ownerChannel = channel; ownerLock = acquired;
				return true;
			} catch (IOException | RuntimeException failure) {
				channel.close();
				clearOwnerReservation(properties, key, token);
				throw failure;
			}
		} catch (IOException | RuntimeException failure) {
			clearOwnerReservation(properties, key, token);
			throw failure;
		}
	}

	private void releaseOwner() {
		try { if (ownerLock != null) ownerLock.release(); } catch (IOException ignored) { }
		try { if (ownerChannel != null) ownerChannel.close(); } catch (IOException ignored) { }
		clearOwnerReservation(ownerProperties, ownerKey, ownerToken);
		ownerLock = null;
		ownerChannel = null;
		ownerKey = null;
		ownerToken = null;
		ownerProperties = null;
	}

	private static void clearOwnerReservation(Properties properties, String key, String token) {
		if (properties == null || key == null || token == null) return;
		synchronized (properties) {
			if (token.equals(properties.getProperty(key))) properties.remove(key);
		}
	}

	private static String hashOwnerPath(Path path) throws IOException {
		try {
			byte[] digest = MessageDigest.getInstance("SHA-256").digest(path.toString().getBytes(StandardCharsets.UTF_8));
			StringBuilder result = new StringBuilder(digest.length * 2);
			for (byte value : digest) result.append(String.format("%02x", value));
			return result.toString();
		} catch (NoSuchAlgorithmException impossible) {
			throw new IOException("SHA-256 unavailable", impossible);
		}
	}

	private String readQueueFile() throws IOException {
		Set<java.nio.file.OpenOption> options = Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS);
		try (SeekableByteChannel channel = Files.newByteChannel(file, options)) {
			ByteBuffer bytes = ByteBuffer.allocate((int) MAX_FILE_BYTES + 1);
			while (bytes.hasRemaining() && channel.read(bytes) != -1) {
				// Continue until EOF or one byte beyond the configured limit.
			}
			if (!bytes.hasRemaining()) throw new IOException("queue file exceeds size limit");
			return new String(bytes.array(), 0, bytes.position(), StandardCharsets.UTF_8);
		}
	}

	private void writeSnapshot(List<PendingVote> snapshot) throws IOException {
		synchronized (persistenceWriteLock) {
			if (closed) return;
			writeSnapshotLocked(snapshot);
		}
	}

	private void writeSnapshotLocked(List<PendingVote> snapshot) throws IOException {
		if (ownerLock == null || !ownerLock.isValid()) throw new IOException("queue file ownership is unavailable");
        if (snapshot.size() > MAX_RETAINED_ENTRIES) throw new IOException("queue snapshot exceeds entry capacity");
		YamlConfiguration yaml = new YamlConfiguration();
		List<Map<String, Object>> values = new ArrayList<>();
		for (PendingVote pending : snapshot) {
			Map<String, Object> value = new LinkedHashMap<>();
			value.put("Username", pending.username);
			value.put("ServiceSite", pending.serviceSite);
			value.put("Time", pending.time);
            value.put("LocalOccurrenceId", pending.localOccurrenceId.toString());
			values.add(value);
		}
		yaml.set("Votes", values);
		byte[] bytes = yaml.saveToString().getBytes(StandardCharsets.UTF_8);
		if (bytes.length > MAX_FILE_BYTES) throw new IOException("queue snapshot exceeds size limit");
		Path parent = file.toAbsolutePath().normalize().getParent();
		if (parent == null) throw new IOException("queue has no parent");
		Files.createDirectories(parent);
		Path temporary = Files.createTempFile(parent, "VotifierVoteQueue-", ".tmp");
		try {
			Files.write(temporary, bytes, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.forceFile(temporary);
			try {
				Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException unsupported) {
				Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING);
			}
			DurableFiles.forceDirectory(parent);
		} finally {
			Files.deleteIfExists(temporary);
		}
	}

	@Override
	public void close() {
        List<PendingVote> snapshot = null;
        boolean initializedWorker;
		synchronized (lock) {
            initializedWorker = asynchronousInitialization || retirementRequested || pendingIngress > 0;
            if (!initializedWorker) {
                if (closed) return;
                closed = true;
                // Legacy callers keep their existing close path until retirement admission.
                snapshot = new ArrayList<>(entries);
                persistenceScheduled = false;
            }
		}
        if (initializedWorker) { closeInitializedWorker(); return; }
		worker.shutdownNow();
		try {
			worker.awaitTermination(1, TimeUnit.SECONDS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
		if (loadFailed) { releaseOwner(); return; }
		try {
			synchronized (persistenceWriteLock) {
				writeSnapshotLocked(snapshot);
			}
		} catch (IOException failure) {
			plugin.getLogger().warning("Unable to persist queued Votifier votes during shutdown: "
					+ failure.getClass().getSimpleName());
		}
		releaseOwner();
	}

    private void closeInitializedWorker() {
        synchronized (lock) {
            if (!closing && !closed) {
                closing = true;
                // Keep the existing worker and file owner through failed final publication.
                worker.execute(this::finishClosePublication);
            }
        }
        try { closePublication.get(1, TimeUnit.SECONDS); }
        catch (java.util.concurrent.TimeoutException pending) {
            plugin.getLogger().warning("Votifier queue shutdown publication is still pending on its worker; recovery is unconfirmed");
        } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
        catch (java.util.concurrent.ExecutionException failure) {
            throw new IllegalStateException("Votifier queue shutdown publication failed", failure.getCause());
        }
    }

    private void finishClosePublication() {
        List<PendingVote> snapshot;
        long version;
        synchronized (lock) {
            if (closed) return;
            if (pendingRetirementCaptures > 0 || pendingIngress > 0) {
                if (!loaded || loadFailed) scheduleInitializationRetryLocked();
                worker.schedule(this::finishClosePublication, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
                return;
            }
            snapshot = new ArrayList<>(entries);
            version = stateVersion;
        }
        if (loadFailed || !loaded) {
            // Unreadable/unowned storage is unknown; never replace it with an empty guess.
            synchronized (lock) { closed = true; }
            releaseOwner(); worker.shutdown(); closePublication.complete(null);
            return;
        }
        try { synchronized (persistenceWriteLock) { writeSnapshotLocked(snapshot); } }
        catch (IOException failure) {
            if (!closeFailureReported) {
                closeFailureReported = true;
                try { plugin.getLogger().warning("Unable to publish final Votifier queue; its worker retains ownership for retry"); }
                catch (RuntimeException | Error ignored) { }
            }
            worker.schedule(this::finishClosePublication, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
            return;
        }
        synchronized (lock) {
            // A confirmed original callback may have removed a receipt during IO.
            if (version != stateVersion) {
                worker.execute(this::finishClosePublication);
                return;
            }
            closed = true;
            durableVersion = version;
        }
        releaseOwner(); worker.shutdown(); closePublication.complete(null);
    }

	private static final class PendingVote {
		private final String username;
		private final String serviceSite;
		private final long time;
        private final UUID localOccurrenceId;
		private final java.util.function.BooleanSupplier executionReleased;
		private boolean submitted;
        private long retryNotBeforeNanos;

		private PendingVote(String username, String serviceSite, long time, UUID localOccurrenceId) {
			this(username, serviceSite, time, localOccurrenceId, () -> true);
		}

		private PendingVote(String username, String serviceSite, long time, UUID localOccurrenceId,
                java.util.function.BooleanSupplier executionReleased) {
			this.username = username;
			this.serviceSite = serviceSite;
			this.time = time;
            this.localOccurrenceId = localOccurrenceId;
            this.executionReleased = java.util.Objects.requireNonNull(executionReleased);
		}
	}
}
