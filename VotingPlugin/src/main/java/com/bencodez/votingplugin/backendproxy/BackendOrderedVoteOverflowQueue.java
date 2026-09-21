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
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

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
	private static final int MAX_ENTRIES = 512;
	private static final long MAX_FILE_BYTES = 8L * 1024L * 1024L;
	private static final long RETRY_DELAY_MILLIS = 250L;
	private static final String QUEUE_FILE = "BackendProxyVoteQueue.yml";

	private final VotingPluginMain plugin;
	private final Path file;
	private final ScheduledThreadPoolExecutor worker;
	private final Object lock = new Object();
	private final Object persistenceWriteLock = new Object();
	private final ArrayDeque<PendingEnvelope> entries = new ArrayDeque<>();
	private boolean persistenceScheduled;
	private boolean persistenceDirty;
	private boolean closed;
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
		PendingEnvelope pending = pending(envelope);
		if (pending == null) return false;
		synchronized (lock) {
			if (closed || entries.size() >= MAX_ENTRIES) return false;
			entries.addLast(pending);
			stateVersion++;
			requestPersistenceLocked();
			return true;
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
			if (closed || entries.size() + pending.size() > MAX_ENTRIES) return false;
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
			return entries.size();
		}
	}

	public boolean hasEntries() {
		return size() != 0;
	}

	PendingEnvelope peekDurable() {
		synchronized (lock) {
			if (closed || durableVersion != stateVersion) return null;
			return entries.peekFirst();
		}
	}

	void acknowledge(PendingEnvelope expected) {
		if (expected == null) return;
		synchronized (lock) {
			if (entries.peekFirst() != expected) {
				throw new IllegalStateException("Ordered proxy vote overflow acknowledgement is out of order");
			}
			entries.removeFirst();
			stateVersion++;
			requestPersistenceLocked();
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
			List<String> snapshot;
			long snapshotVersion;
			synchronized (lock) {
				if (!persistenceDirty) {
					persistenceScheduled = false;
					return;
				}
				persistenceDirty = false;
				snapshot = payloadSnapshotLocked();
				snapshotVersion = stateVersion;
			}
			try {
				writeSnapshot(snapshot);
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
			boolean skipped = false;
			for (String payload : payloads) {
				if (entries.size() >= MAX_ENTRIES) {
					skipped = true;
					break;
				}
				try {
					JsonEnvelope envelope = JsonEnvelopeCodec.decode(payload);
					if (!isOrderedVoteMessage(envelope)) {
						skipped = true;
						continue;
					}
					entries.addLast(new PendingEnvelope(payload, envelope));
				} catch (RuntimeException invalid) {
					skipped = true;
				}
			}
			if (skipped) {
				synchronized (lock) {
					stateVersion++;
					requestPersistenceLocked();
				}
			}
		} catch (Exception failure) {
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
		if (entries.isEmpty()) return Collections.emptyList();
		List<String> snapshot = new ArrayList<>(entries.size());
		for (PendingEnvelope pending : entries) snapshot.add(pending.payload);
		return snapshot;
	}

	private void writeSnapshot(List<String> snapshot) throws IOException {
		synchronized (persistenceWriteLock) {
			writeSnapshotLocked(snapshot);
		}
	}

	private void writeSnapshotLocked(List<String> snapshot) throws IOException {
		YamlConfiguration yaml = new YamlConfiguration();
		yaml.set("Envelopes", snapshot);
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
		List<String> snapshot;
		synchronized (lock) {
			if (closed) return;
			closed = true;
			wakeupOwner = null;
			wakeup = null;
			snapshot = payloadSnapshotLocked();
			persistenceScheduled = false;
		}
		worker.shutdownNow();
		try {
			worker.awaitTermination(1, TimeUnit.SECONDS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
		try {
			synchronized (persistenceWriteLock) {
				writeSnapshotLocked(snapshot);
			}
		} catch (IOException failure) {
			warn("Unable to persist ordered proxy vote overflow during shutdown", failure);
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
