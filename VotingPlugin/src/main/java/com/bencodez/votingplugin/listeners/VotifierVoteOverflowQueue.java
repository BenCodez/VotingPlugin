package com.bencodez.votingplugin.listeners;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
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
import java.util.UUID;
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
	private static final long MAX_FILE_BYTES = 4L * 1024L * 1024L;
	private static final long RETRY_DELAY_MILLIS = 250L;
	private static final String QUEUE_FILE = "VotifierVoteQueue.yml";

	private final VotingPluginMain plugin;
	@FunctionalInterface
	public interface VoteProcessor { void process(String serviceSite, String username, long occurredAt); }
	@FunctionalInterface
	public interface OccurrenceProcessor { void process(String serviceSite, String username, long occurredAt, UUID localOccurrenceId); }
	private final OccurrenceProcessor processor;
    private final java.util.function.BooleanSupplier processingAllowed;
	private final Path file;
	private final ScheduledThreadPoolExecutor worker;
	private final Object lock = new Object();
	private final Object persistenceWriteLock = new Object();
	private final ArrayDeque<PendingVote> entries = new ArrayDeque<>();
	private boolean drainScheduled;
	private boolean persistenceScheduled;
	private boolean persistenceDirty;
	private boolean started;
	private boolean closed;
    private boolean closing;
    private volatile boolean loaded;
    private volatile boolean loadFailed;
    private int pendingIngress;
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

    private VotifierVoteOverflowQueue(VotingPluginMain plugin, OccurrenceProcessor processor,
            java.util.function.BooleanSupplier processingAllowed, boolean asynchronousInitialization) {
        this.asynchronousInitialization = asynchronousInitialization;
		this.plugin = plugin;
		this.processor = processor;
        this.processingAllowed = processingAllowed;
		this.file = new File(plugin.getDataFolder(), QUEUE_FILE).toPath();
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
        boolean submitted = false;
        synchronized (lock) {
            if (!closed && !closing && pendingIngress < MAX_ENTRIES) {
                pendingIngress++;
                try {
                    worker.execute(() -> {
                        boolean accepted;
                        try { accepted = enqueueStored(username, serviceSite, occurredAt, localOccurrenceId, true); }
                        finally { synchronized (lock) { pendingIngress--; } }
                        completion.accept(accepted);
                    });
                    submitted = true;
                } catch (RejectedExecutionException rejected) { pendingIngress--; }
            }
        }
        if (!submitted) completion.accept(false);
    }

    private boolean enqueueStored(String username, String serviceSite, long occurredAt, UUID localOccurrenceId,
            boolean admittedTransfer) {
		if (username == null || serviceSite == null || occurredAt <= 0 || localOccurrenceId == null) return false;
		synchronized (lock) {
			if (closed || closing && !admittedTransfer || !loaded || loadFailed || entries.size() >= MAX_ENTRIES) return false;
			entries.addLast(new PendingVote(username, serviceSite, occurredAt, localOccurrenceId));
			stateVersion++;
			requestPersistenceLocked();
			scheduleDrainLocked();
			return true;
		}
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
                        if (!processingAllowed.getAsBoolean()) {
                            synchronized (lock) { pending.submitted = false; }
                            return; // Retired generation retains the durable entry for restart.
                        }
						try {
							processor.process(pending.serviceSite, pending.username, pending.time, pending.localOccurrenceId);
						} finally {
							acknowledge(pending);
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
				}
			}
		}
	}

	private PendingVote nextUnsubmittedLocked() {
		for (PendingVote pending : entries) {
			if (!pending.submitted) return pending;
		}
		return null;
	}

	private void acknowledge(PendingVote pending) {
		synchronized (lock) {
			entries.remove(pending);
			stateVersion++;
			requestPersistenceLocked();
			scheduleDrainLocked();
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
		try {
			if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) return;
			YamlConfiguration yaml = new YamlConfiguration();
			yaml.loadFromString(readQueueFile());
			Object raw = yaml.get("Votes");
			if (!(raw instanceof List<?> values)) throw new IOException("queue votes are not a list");
            if (values.size() > MAX_ENTRIES) throw new IOException("queue exceeds entry capacity");
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
            synchronized (lock) { entries.addAll(recovered); }
			if (skipped) {
				synchronized (lock) {
					stateVersion++;
					requestPersistenceLocked();
				}
			}
		} catch (Exception failure) {
            loadFailed = true;
			plugin.getLogger().warning("Unable to load queued Votifier votes: " + failure.getClass().getSimpleName());
		} finally {
            loaded = true;
            synchronized (lock) { scheduleDrainLocked(); }
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
        if (asynchronousInitialization) { closeInitializedWorker(); return; }
		List<PendingVote> snapshot;
		synchronized (lock) {
			if (closed) return;
			closed = true;
			// At-least-once delivery: retain any unacknowledged callback. A callback
			// interrupted during shutdown must be replayed rather than silently lost.
			snapshot = new ArrayList<>(entries);
			persistenceScheduled = false;
		}
		worker.shutdownNow();
		try {
			worker.awaitTermination(1, TimeUnit.SECONDS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
        if (loadFailed) return;
		try {
			synchronized (persistenceWriteLock) {
				writeSnapshotLocked(snapshot);
			}
		} catch (IOException failure) {
			plugin.getLogger().warning("Unable to persist queued Votifier votes during shutdown: "
					+ failure.getClass().getSimpleName());
		}
	}

    private void closeInitializedWorker() {
        synchronized (lock) {
            if (closed || closing) return;
            closing = true;
            // FIFO finalization follows loading and every accepted ingress transfer.
            // Never replace an unreadable disk queue with a guessed empty snapshot.
            worker.execute(() -> {
                List<PendingVote> snapshot;
                synchronized (lock) { closed = true; snapshot = new ArrayList<>(entries); }
                if (loadFailed) return;
                try { synchronized (persistenceWriteLock) { writeSnapshotLocked(snapshot); } }
                catch (IOException failure) {
                    plugin.getLogger().warning("Unable to persist queued Votifier votes during shutdown: "
                            + failure.getClass().getSimpleName());
                }
            });
            worker.shutdown();
        }
        try {
            if (!worker.awaitTermination(1, TimeUnit.SECONDS)) {
                plugin.getLogger().warning("Votifier queue shutdown publication is still pending on its worker");
            }
        } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
    }

	private static final class PendingVote {
		private final String username;
		private final String serviceSite;
		private final long time;
        private final UUID localOccurrenceId;
		private boolean submitted;

		private PendingVote(String username, String serviceSite, long time, UUID localOccurrenceId) {
			this.username = username;
			this.serviceSite = serviceSite;
			this.time = time;
            this.localOccurrenceId = localOccurrenceId;
		}
	}
}
