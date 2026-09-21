package com.bencodez.votingplugin.backendproxy;

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
import java.util.List;
import java.util.Set;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import org.bukkit.configuration.file.YamlConfiguration;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.util.DurableFiles;

/**
 * Bounded durable spill for ordered backend proxy vote messages. Processing is
 * owned by the active BackendProxyHandler; this class only persists FIFO state
 * across handler replacement and process restart.
 */
public final class BackendOrderedVoteOverflowQueue implements AutoCloseable {
	static final int MAX_NORMAL_ENTRIES = 512;
	private static final int MAX_ENTRIES = MAX_NORMAL_ENTRIES + BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE;
	static final int MAX_FAILED_ENTRIES = 256;
	private static final long MAX_FILE_BYTES = 8L * 1024L * 1024L;
	private static final long RETRY_DELAY_MILLIS = 250L;
	private static final long CLOSE_GRACE_MILLIS = 3_000L;
	private static final String QUEUE_FILE = "BackendProxyVoteQueue.yml";

	private final VotingPluginMain plugin;
	private final Path file;
	private final ScheduledThreadPoolExecutor worker;
	private final Object lock = new Object();
	private final Object persistenceWriteLock = new Object();
	private final ArrayDeque<PendingEnvelope> entries = new ArrayDeque<>();
	private final ArrayDeque<PendingAdmission> admissions = new ArrayDeque<>();
	private final ArrayDeque<String> failedEntries = new ArrayDeque<>();
	private boolean admissionPersistenceScheduled;
	private boolean persistenceScheduled;
	private boolean persistenceDirty;
	private boolean closed;
	private final AtomicBoolean closeRequested = new AtomicBoolean();
	private boolean loadFailed;
	private PendingFailure pendingFailure;
	private PendingAcknowledgement pendingAcknowledgement;
	private volatile Thread closeThread;
	private volatile CloseState closeState = CloseState.OPEN;
	private long stateVersion;
	private long durableVersion;
	private Object wakeupOwner;
	private Runnable wakeup;

	public BackendOrderedVoteOverflowQueue(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.file = new File(plugin.getDataFolder(), QUEUE_FILE).toPath();
		this.worker = new ScheduledThreadPoolExecutor(1, runnable -> {
			Thread thread = new Thread(runnable, "VotingPlugin-BackendVote-Overflow");
			thread.setDaemon(true);
			return thread;
		});
		this.worker.setRemoveOnCancelPolicy(true);
		load();
	}

	public boolean enqueue(JsonEnvelope envelope) {
		return enqueue(envelope, 0);
	}

	boolean enqueue(JsonEnvelope envelope, int reservedPrefixEntries) {
		PendingEnvelope pending = pending(envelope);
		if (pending == null || reservedPrefixEntries < 0
				|| reservedPrefixEntries > BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE) return false;
		Runnable notify = null;
		synchronized (persistenceWriteLock) {
			synchronized (lock) {
				// Admission returns only after the overflow snapshot reaches disk.
				// Keep older in-memory work's shutdown capacity reserved as well.
				if (closeRequested.get() || closed || loadFailed || !admissions.isEmpty()
						|| entries.size() + reservedPrefixEntries >= MAX_ENTRIES) return false;
				List<String> snapshot = payloadSnapshotLocked();
				snapshot.add(pending.payload);
				try {
					writeSnapshotLocked(snapshot, failedSnapshotLocked());
				} catch (IOException failure) {
					warn("Unable to admit ordered proxy vote overflow", failure);
					return false;
				}
				entries.addLast(pending);
				durableVersion = ++stateVersion;
				if (wakeup != null) notify = wakeup;
			}
		}
		runWakeup(notify);
		return true;
	}

	/** Reserves bounded FIFO capacity and persists it on the overflow worker. */
	boolean enqueueAsync(JsonEnvelope envelope, int reservedPrefixEntries, Consumer<Boolean> completion) {
		PendingEnvelope pending = pending(envelope);
		if (pending == null || completion == null || reservedPrefixEntries < 0
				|| reservedPrefixEntries > BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE) return false;
		PendingAdmission admission = new PendingAdmission(pending, completion);
		boolean schedulePersistence;
		synchronized (lock) {
			if (closeRequested.get() || closed || loadFailed
					|| entries.size() + admissions.size() + reservedPrefixEntries >= MAX_ENTRIES) return false;
			admissions.addLast(admission);
			schedulePersistence = !admissionPersistenceScheduled;
			admissionPersistenceScheduled = true;
		}
		if (!schedulePersistence) return true;
		try {
			worker.execute(this::persistAdmissions);
			return true;
		} catch (RejectedExecutionException rejected) {
			synchronized (lock) {
				admissions.removeLastOccurrence(admission);
				admissionPersistenceScheduled = false;
			}
			return false;
		}
	}

	private void persistAdmissions() {
		while (true) {
			List<PendingAdmission> batch;
			List<PendingAdmission> admitted = new ArrayList<>();
			List<String> snapshot;
			List<String> failures;
			long snapshotVersion;
			Runnable notify = null;
			synchronized (persistenceWriteLock) {
				synchronized (lock) {
					if (closed || loadFailed || admissions.isEmpty()) {
						admissionPersistenceScheduled = false;
						return;
					}
					batch = new ArrayList<>(admissions);
					snapshot = payloadSnapshotLocked();
					failures = failedSnapshotLocked();
					snapshotVersion = stateVersion;
				}
				try {
					writeSnapshotLocked(snapshot, failures);
				} catch (IOException failure) {
					warn("Unable to admit ordered proxy vote overflow", failure);
					try {
						worker.schedule(this::persistAdmissions, RETRY_DELAY_MILLIS, TimeUnit.MILLISECONDS);
					} catch (RejectedExecutionException ignored) {
						synchronized (lock) {
							admissionPersistenceScheduled = false;
						}
					}
					return;
				}
				synchronized (lock) {
					for (PendingAdmission candidate : batch) {
						if (admissions.peekFirst() != candidate) break;
						admissions.removeFirst();
						entries.addLast(candidate.pending);
						admitted.add(candidate);
					}
					durableVersion = Math.max(durableVersion, snapshotVersion);
					if (durableVersion == stateVersion && wakeup != null) notify = wakeup;
				}
			}
			for (PendingAdmission admission : admitted) admission.complete(true);
			runWakeup(notify);
		}
	}

	/** Adds older in-memory work ahead of already spilled newer messages. */
	public boolean prepend(List<JsonEnvelope> envelopes) {
		if (envelopes == null || envelopes.isEmpty()) return true;
		List<PendingEnvelope> pending = new ArrayList<>(envelopes.size());
		for (JsonEnvelope envelope : envelopes) {
			PendingEnvelope value = pending(envelope);
			if (value == null) return false;
			pending.add(value);
		}
		synchronized (lock) {
			if (closeRequested.get() || closed || loadFailed
					|| entries.size() + admissions.size() + pending.size() > MAX_ENTRIES) return false;
			for (int index = pending.size() - 1; index >= 0; index--) {
				entries.addFirst(pending.get(index));
			}
			stateVersion++;
			requestPersistenceLocked();
			return true;
		}
	}

	public int size() {
		synchronized (lock) {
			return entries.size() + admissions.size();
		}
	}

	public boolean hasEntries() {
		return size() != 0;
	}

	int failedSize() {
		synchronized (lock) {
			return failedEntries.size();
		}
	}

	boolean isQuarantineCapacityExhausted() {
		synchronized (lock) {
			return loadFailed || failedEntries.size() >= MAX_FAILED_ENTRIES;
		}
	}

	/** Moves an ambiguous processing failure out of the active lane in one durable snapshot. */
	private Boolean quarantine(PendingFailure request) {
		synchronized (persistenceWriteLock) {
			synchronized (lock) {
				if (closed) return null; // Final close owns the pending failure.
				if (loadFailed || pendingFailure != request
						|| (request.expected != null && entries.peekFirst() != request.expected)) return false;
				if (failedEntries.size() >= MAX_FAILED_ENTRIES) return false;
				List<String> active = payloadSnapshotLocked();
				if (request.expected != null) active.remove(0);
				List<String> failures = failedSnapshotLocked();
				failures.add(request.failed.payload);
				try {
					writeSnapshotLocked(active, failures);
				} catch (IOException failure) {
					warn("Unable to quarantine ordered proxy vote", failure);
					return false;
				}
				if (request.expected != null) entries.removeFirst();
				failedEntries.addLast(request.failed.payload);
				durableVersion = ++stateVersion;
				request.stored = true;
				pendingFailure = null;
				return true;
			}
		}
	}

	/** Stages durable isolation without adding filesystem work to the lifecycle thread. */
	boolean quarantineForShutdown(PendingEnvelope expected, JsonEnvelope envelope) {
		PendingEnvelope failed = pending(envelope);
		PendingFailure request;
		synchronized (lock) {
			if (closeRequested.get() || closed || loadFailed || failed == null || pendingFailure != null
					|| pendingAcknowledgement != null || failedEntries.size() >= MAX_FAILED_ENTRIES) return false;
			request = new PendingFailure(expected, failed, ignored -> { });
			pendingFailure = request;
		}
		try {
			worker.execute(() -> quarantine(request));
			return true;
		} catch (RejectedExecutionException rejected) {
			synchronized (lock) {
				if (pendingFailure == request) pendingFailure = null;
			}
			return false;
		}
	}

	/** Writes failure evidence off the Bukkit owner thread before releasing the lane. */
	void quarantineAsync(PendingEnvelope expected, JsonEnvelope envelope, Consumer<Boolean> completion) {
		PendingEnvelope failed = pending(envelope);
		PendingFailure request = null;
		synchronized (lock) {
			if (!closeRequested.get() && !closed && !loadFailed && failed != null && pendingFailure == null
					&& pendingAcknowledgement == null) {
				request = new PendingFailure(expected, failed, completion);
				pendingFailure = request;
			}
		}
		if (request == null) {
			completion.accept(false);
			return;
		}
		PendingFailure queued = request;
		try {
			worker.execute(() -> {
				Boolean stored = quarantine(queued);
				if (stored == null) return;
				try {
					queued.complete(stored);
				} finally {
					if (stored) {
						synchronized (lock) {
							if (pendingFailure == queued) pendingFailure = null;
						}
					}
				}
			});
		} catch (RejectedExecutionException rejected) {
			queued.complete(false);
		}
	}

	boolean hasPendingFailure(JsonEnvelope envelope) {
		synchronized (lock) {
			return pendingFailure != null && pendingFailure.failed.envelope == envelope
					&& !loadFailed;
		}
	}

	PendingEnvelope peekDurable() {
		synchronized (lock) {
			if (closed || loadFailed || durableVersion != stateVersion) return null;
			return entries.peekFirst();
		}
	}

	/** Persists completion off the platform thread before removing the durable head. */
	void acknowledgeAsync(PendingEnvelope expected, Consumer<Boolean> completion) {
		PendingAcknowledgement request = null;
		synchronized (lock) {
			if (!closeRequested.get() && !closed && !loadFailed && expected != null && entries.peekFirst() == expected
					&& pendingAcknowledgement == null && pendingFailure == null) {
				request = new PendingAcknowledgement(expected, completion);
				pendingAcknowledgement = request;
			}
		}
		if (request == null) {
			completion.accept(false);
			return;
		}
		PendingAcknowledgement queued = request;
		try {
			worker.execute(() -> {
				Boolean stored = acknowledge(queued);
				if (stored == null) return;
				try {
					queued.complete(stored);
				} finally {
					if (stored) {
						synchronized (lock) {
							if (pendingAcknowledgement == queued) pendingAcknowledgement = null;
						}
					}
				}
			});
		} catch (RejectedExecutionException rejected) {
			queued.complete(false);
		}
	}

	private Boolean acknowledge(PendingAcknowledgement request) {
		synchronized (persistenceWriteLock) {
			synchronized (lock) {
				if (closed) return null; // Final close owns the pending acknowledgement.
				if (loadFailed || pendingAcknowledgement != request || entries.peekFirst() != request.expected) {
					return false;
				}
				List<String> active = payloadSnapshotLocked();
				active.remove(0);
				try {
					writeSnapshotLocked(active, failedSnapshotLocked());
				} catch (IOException failure) {
					warn("Unable to acknowledge ordered proxy vote overflow", failure);
					return false;
				}
				entries.removeFirst();
				durableVersion = ++stateVersion;
				request.stored = true;
				pendingAcknowledgement = null;
				return true;
			}
		}
	}

	void bindWakeup(Object owner, Runnable callback) {
		Runnable notify = null;
		synchronized (lock) {
			wakeupOwner = owner;
			wakeup = callback;
			if (!closed && durableVersion == stateVersion && !entries.isEmpty()) notify = callback;
		}
		runWakeup(notify);
	}

	void unbindWakeup(Object owner) {
		synchronized (lock) {
			if (wakeupOwner != owner) return;
			wakeupOwner = null;
			wakeup = null;
		}
	}

	/** Retries a failed delivery without keeping the ordered lane active or spinning. */
	boolean retryLater(Object owner, Runnable callback) {
		synchronized (lock) {
			if (closed || wakeupOwner != owner) return false;
			try {
				worker.schedule(() -> {
					synchronized (lock) {
						if (closed || wakeupOwner != owner) return;
					}
					runWakeup(callback);
				}, 1L, TimeUnit.SECONDS);
				return true;
			} catch (RejectedExecutionException rejected) {
				return false;
			}
		}
	}

	private PendingEnvelope pending(JsonEnvelope envelope) {
		if (envelope == null || !isOrderedVoteMessage(envelope)) return null;
		try {
			return new PendingEnvelope(JsonEnvelopeCodec.encode(envelope), envelope);
		} catch (RuntimeException failure) {
			return null;
		}
	}

	private static boolean isOrderedVoteMessage(JsonEnvelope envelope) {
		String subChannel = envelope.getSubChannel();
		return VotingPluginWire.SUB_VOTE.equals(subChannel)
				|| VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)
				|| VotingPluginWire.SUB_VOTE_UPDATE.equals(subChannel);
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
			long snapshotVersion;
			try {
				synchronized (persistenceWriteLock) {
					List<String> snapshot;
					List<String> failures;
					synchronized (lock) {
						if (!persistenceDirty) {
							persistenceScheduled = false;
							return;
						}
						persistenceDirty = false;
						snapshot = payloadSnapshotLocked();
						failures = failedSnapshotLocked();
						snapshotVersion = stateVersion;
					}
					writeSnapshotLocked(snapshot, failures);
				}
			} catch (IOException failure) {
				warn("Unable to persist ordered proxy vote overflow", failure);
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
			Runnable notify = null;
			synchronized (lock) {
				durableVersion = Math.max(durableVersion, snapshotVersion);
				if (!persistenceDirty) {
					persistenceScheduled = false;
					if (!closed && durableVersion == stateVersion && !entries.isEmpty()) notify = wakeup;
				}
			}
			runWakeup(notify);
			if (notify != null) return;
		}
	}

	private void load() {
		try {
			if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) return;
			YamlConfiguration yaml = new YamlConfiguration();
			yaml.loadFromString(readQueueFile());
			List<String> payloads = yaml.getStringList("Envelopes");
			List<String> failures = yaml.getStringList("FailedEnvelopes");
			if (failures.size() > MAX_FAILED_ENTRIES) {
				throw new IOException("failed envelope history exceeds configured limit");
			}
			if (payloads.size() > MAX_ENTRIES) {
				throw new IOException("active envelope history exceeds configured limit");
			}
			for (String failed : failures) {
				failedEntries.addLast(failed);
			}
			for (String payload : payloads) {
				JsonEnvelope envelope;
				try {
					envelope = JsonEnvelopeCodec.decode(payload);
				} catch (RuntimeException invalid) {
					throw new IOException("invalid ordered proxy vote envelope", invalid);
				}
				if (!isOrderedVoteMessage(envelope)) {
					throw new IOException("unsupported ordered proxy vote envelope");
				}
				entries.addLast(new PendingEnvelope(payload, envelope));
			}
		} catch (Exception failure) {
			loadFailed = true;
			entries.clear();
			failedEntries.clear();
			warn("Unable to load ordered proxy vote overflow", failure);
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

	private List<String> payloadSnapshotLocked() {
		List<String> snapshot = new ArrayList<>(entries.size() + admissions.size());
		for (PendingEnvelope pending : entries) snapshot.add(pending.payload);
		for (PendingAdmission admission : admissions) snapshot.add(admission.pending.payload);
		return snapshot;
	}

	private List<String> failedSnapshotLocked() {
		return new ArrayList<>(failedEntries);
	}

	private void writeSnapshotLocked(List<String> snapshot, List<String> failures) throws IOException {
		writeSnapshotLocked(snapshot, failures, false);
	}

	private void writeSnapshotLocked(List<String> snapshot, List<String> failures, boolean finalWrite) throws IOException {
		YamlConfiguration yaml = new YamlConfiguration();
		yaml.set("Envelopes", snapshot);
		yaml.set("FailedEnvelopes", failures);
		byte[] bytes = yaml.saveToString().getBytes(StandardCharsets.UTF_8);
		if (bytes.length > MAX_FILE_BYTES) throw new IOException("queue snapshot exceeds size limit");
		Path parent = file.toAbsolutePath().normalize().getParent();
		if (parent == null) throw new IOException("queue has no parent");
		Files.createDirectories(parent);
		Path temporary = Files.createTempFile(parent, "BackendProxyVoteQueue-", ".tmp");
		try {
			Files.write(temporary, bytes, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.forceFile(temporary);
			try {
				Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException unsupported) {
				Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING);
			}
			if (finalWrite) closeState = CloseState.SNAPSHOT_REPLACED;
			DurableFiles.forceDirectory(parent);
		} finally {
			Files.deleteIfExists(temporary);
		}
	}

	private void runWakeup(Runnable callback) {
		if (callback == null) return;
		try {
			callback.run();
		} catch (RuntimeException failure) {
			if (plugin != null) plugin.debug(failure);
		}
	}

	private void warn(String message, Exception failure) {
		if (plugin != null && plugin.getLogger() != null) {
			plugin.getLogger().warning(message + ": " + failure.getClass().getSimpleName());
		}
	}

	@Override
	public void close() {
		if (!closeRequested.compareAndSet(false, true)) return;
		closeState = CloseState.CLOSING;
		Thread finalWrite = new Thread(this::closeBlocking, "VotingPlugin-BackendVote-Close");
		finalWrite.setDaemon(true);
		closeThread = finalWrite;
		finalWrite.start();
		try {
			finalWrite.join(CLOSE_GRACE_MILLIS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
		if (finalWrite.isAlive() && plugin != null && plugin.getLogger() != null) {
			if (closeState == CloseState.SNAPSHOT_REPLACED) {
				plugin.getLogger().warning("Ordered proxy vote overflow snapshot was replaced, but its durability tail "
						+ "is still running after the bounded shutdown grace");
			} else {
				plugin.getLogger().severe("Ordered proxy vote overflow snapshot was not replaced within the bounded "
						+ "shutdown grace; final persistence remains pending");
			}
		}
	}

	boolean awaitClose(long timeoutMillis) throws InterruptedException {
		Thread active = closeThread;
		if (active == null) return true;
		active.join(timeoutMillis);
		return !active.isAlive();
	}

	private void closeBlocking() {
		List<String> snapshot;
		List<String> failures;
		PendingFailure closingFailure;
		PendingAcknowledgement closingAcknowledgement;
		boolean canStoreFailure;
		boolean canStoreAcknowledgement;
		synchronized (lock) {
			if (closed) return;
			closed = true;
			wakeupOwner = null;
			wakeup = null;
			snapshot = payloadSnapshotLocked();
			failures = failedSnapshotLocked();
			closingFailure = pendingFailure;
			closingAcknowledgement = pendingAcknowledgement;
			canStoreFailure = closingFailure != null && !closingFailure.stored
					&& failures.size() < MAX_FAILED_ENTRIES
					&& (closingFailure.expected == null || entries.peekFirst() == closingFailure.expected);
			canStoreAcknowledgement = closingAcknowledgement != null && !closingAcknowledgement.stored
					&& entries.peekFirst() == closingAcknowledgement.expected;
			if (canStoreAcknowledgement) snapshot.remove(0);
			if (canStoreFailure) {
				if (closingFailure.expected != null) snapshot.remove(0);
				failures.add(closingFailure.failed.payload);
			}
			persistenceScheduled = false;
		}
		worker.shutdownNow();
		try {
			worker.awaitTermination(1, TimeUnit.SECONDS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
		if (loadFailed) {
			closeState = CloseState.FAILED;
			if (closingFailure != null) closingFailure.complete(false);
			if (closingAcknowledgement != null) closingAcknowledgement.complete(false);
			return;
		}
		boolean saved = false;
		try {
			closeState = CloseState.WAITING_TO_PERSIST;
			synchronized (persistenceWriteLock) {
				writeSnapshotLocked(snapshot, failures, true);
			}
			saved = true;
		} catch (IOException failure) {
			closeState = CloseState.FAILED;
			severe("Unable to persist ordered proxy vote overflow during shutdown", failure);
		}
		if (saved && canStoreFailure) {
			synchronized (lock) {
				if (closingFailure.expected != null) entries.removeFirst();
				failedEntries.addLast(closingFailure.failed.payload);
				closingFailure.stored = true;
			}
		}
		if (saved && canStoreAcknowledgement) {
			synchronized (lock) {
				entries.removeFirst();
				closingAcknowledgement.stored = true;
			}
		}
		if (closingFailure != null) closingFailure.complete(saved && (closingFailure.stored || canStoreFailure));
		if (closingAcknowledgement != null) {
			closingAcknowledgement.complete(saved
					&& (closingAcknowledgement.stored || canStoreAcknowledgement));
		}
		if (saved) closeState = CloseState.COMPLETE;
	}

	private void severe(String message, Exception failure) {
		if (plugin != null && plugin.getLogger() != null) {
			plugin.getLogger().severe(message + ": " + failure.getClass().getSimpleName());
		}
	}

	enum CloseState { OPEN, CLOSING, WAITING_TO_PERSIST, SNAPSHOT_REPLACED, COMPLETE, FAILED }

	CloseState closeState() { return closeState; }

	private static final class PendingAcknowledgement {
		private final PendingEnvelope expected;
		private final Consumer<Boolean> completion;
		private final AtomicBoolean completed = new AtomicBoolean();
		private boolean stored;

		private PendingAcknowledgement(PendingEnvelope expected, Consumer<Boolean> completion) {
			this.expected = expected;
			this.completion = completion;
		}

		private void complete(boolean success) {
			if (completed.compareAndSet(false, true)) completion.accept(success);
		}
	}

	private static final class PendingFailure {
		private final PendingEnvelope expected;
		private final PendingEnvelope failed;
		private final Consumer<Boolean> completion;
		private final AtomicBoolean completed = new AtomicBoolean();
		private boolean stored;

		private PendingFailure(PendingEnvelope expected, PendingEnvelope failed, Consumer<Boolean> completion) {
			this.expected = expected;
			this.failed = failed;
			this.completion = completion;
		}

		private void complete(boolean success) {
			if (completed.compareAndSet(false, true)) completion.accept(success);
		}
	}

	private static final class PendingAdmission {
		private final PendingEnvelope pending;
		private final Consumer<Boolean> completion;
		private final AtomicBoolean completed = new AtomicBoolean();

		private PendingAdmission(PendingEnvelope pending, Consumer<Boolean> completion) {
			this.pending = pending;
			this.completion = completion;
		}

		private void complete(boolean success) {
			if (completed.compareAndSet(false, true)) completion.accept(success);
		}
	}

	static final class PendingEnvelope {
		private final String payload;
		private final JsonEnvelope envelope;

		private PendingEnvelope(String payload, JsonEnvelope envelope) {
			this.payload = payload;
			this.envelope = envelope;
		}

		JsonEnvelope envelope() {
			return envelope;
		}
	}
}
