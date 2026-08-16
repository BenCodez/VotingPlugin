package com.bencodez.votingplugin.proxy.presence;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;

import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker.BackendGenerationState;

/**
 * Atomically persists the small, bounded proxy-local ordering fence for backend
 * process incarnations. The file contains no player data.
 */
public final class BackendGenerationStateStore {
	private static final int MAGIC = 0x56504753;
	private static final int VERSION = 1;
	private static final int MAX_BACKENDS = 1024;
	private static final int MAX_RETIRED_INCARNATIONS = 64;
	private static final int MAX_SERVER_CHARACTERS = 128;
	private static final long MAX_SERIALIZED_SERVER_BYTES = 2L + MAX_SERVER_CHARACTERS * 3L;
	private static final long MAX_SERIALIZED_ENTRY_BYTES = MAX_SERIALIZED_SERVER_BYTES + 16L + 8L + 8L + 1L
			+ 4L + MAX_RETIRED_INCARNATIONS * 16L;
	private static final long MAX_FILE_BYTES = 12L + MAX_BACKENDS * MAX_SERIALIZED_ENTRY_BYTES;
	private static final String FILE_NAME = "backend-presence-generations.dat";

	private final Path file;

	public BackendGenerationStateStore(Path dataDirectory) {
		this.file = dataDirectory.resolve(FILE_NAME);
	}

	/**
	 * Restores only configured backend names into an empty tracker.
	 *
	 * @param tracker destination tracker
	 * @param configuredServers currently configured backend names
	 * @param now current proxy time
	 * @return active restored backends requiring liveness/snapshot recovery
	 * @throws IOException when the bounded state file is malformed or unreadable
	 */
	public Set<String> loadInto(BackendPlayerPresenceTracker tracker, Collection<String> configuredServers,
			long now) throws IOException {
		if (!Files.exists(file)) {
			return Set.of();
		}
		long fileSize = Files.size(file);
		if (fileSize <= 0L || fileSize > MAX_FILE_BYTES) {
			throw new IOException("Backend generation state file has an invalid size");
		}
		Set<String> configured = new HashSet<>();
		if (configuredServers != null) {
			for (String server : configuredServers) {
				if (server != null && !server.isBlank()) {
					configured.add(server.trim().toLowerCase(Locale.ROOT));
				}
			}
		}
		List<BackendGenerationState> states = new ArrayList<>();
		Set<String> restoredServerKeys = new HashSet<>();
		try (DataInputStream input = new DataInputStream(new BufferedInputStream(Files.newInputStream(file)))) {
			if (input.readInt() != MAGIC || input.readInt() != VERSION) {
				throw new IOException("Unsupported backend generation state format");
			}
			int backendCount = input.readInt();
			if (backendCount < 0 || backendCount > MAX_BACKENDS) {
				throw new IOException("Invalid backend generation state count");
			}
			for (int index = 0; index < backendCount; index++) {
				String server = input.readUTF();
				String normalizedServer = server.trim();
				String serverKey = normalizedServer.toLowerCase(Locale.ROOT);
				boolean configuredEntry = configured.contains(serverKey);
				UUID current = readUuid(input);
				long backendStartedAt = input.readLong();
				long lastLifecycleTimestamp = input.readLong();
				boolean stopped = input.readBoolean();
				int retiredCount = input.readInt();
				if (retiredCount < 0 || retiredCount > MAX_RETIRED_INCARNATIONS) {
					throw new IOException("Invalid retired backend incarnation count");
				}
				Set<UUID> retired = new LinkedHashSet<>();
				for (int retiredIndex = 0; retiredIndex < retiredCount; retiredIndex++) {
					UUID retiredIncarnation = readUuid(input);
					if (!retired.add(retiredIncarnation) && configuredEntry) {
						throw new IOException("Duplicate retired backend incarnation");
					}
				}
				if (configuredEntry) {
					if (normalizedServer.isEmpty() || normalizedServer.length() > MAX_SERVER_CHARACTERS
							|| !server.equals(normalizedServer) || backendStartedAt <= 0L
							|| lastLifecycleTimestamp < backendStartedAt || retired.contains(current)
							|| !restoredServerKeys.add(serverKey)) {
						throw new IOException("Invalid configured backend generation state");
					}
					states.add(new BackendGenerationState(server, current, backendStartedAt,
							lastLifecycleTimestamp, stopped, retired));
				}
			}
			if (input.read() != -1) {
				throw new IOException("Unexpected trailing backend generation state data");
			}
		} catch (EOFException e) {
			throw new IOException("Truncated backend generation state file", e);
		}
		return tracker.restoreBackendGenerationStates(states, now);
	}

	/**
	 * Writes one complete state snapshot and atomically replaces the previous file.
	 *
	 * @param tracker source tracker
	 * @throws IOException when the state cannot be durably replaced
	 */
	public void save(BackendPlayerPresenceTracker tracker) throws IOException {
		List<BackendGenerationState> states = new ArrayList<>(tracker.getBackendGenerationStates());
		if (states.size() > MAX_BACKENDS) {
			throw new IOException("Too many backend generation states to persist");
		}
		states.sort(Comparator.comparing(BackendGenerationState::getServer, String.CASE_INSENSITIVE_ORDER));
		for (BackendGenerationState state : states) {
			if (state.getServer() == null || state.getServer().isBlank()
					|| state.getServer().length() > MAX_SERVER_CHARACTERS
					|| state.getBackendIncarnationId() == null || state.getBackendStartedAt() <= 0L
					|| state.getLastLifecycleTimestamp() < state.getBackendStartedAt()
					|| state.getRetiredIncarnations().size() > MAX_RETIRED_INCARNATIONS) {
				throw new IOException("Invalid backend generation state cannot be persisted");
			}
		}
		Path parent = file.getParent();
		if (parent != null) {
			Files.createDirectories(parent);
		}
		Path temporary = file.resolveSibling(file.getFileName() + ".tmp");
		try (FileChannel channel = FileChannel.open(temporary, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
				DataOutputStream output = new DataOutputStream(
						new BufferedOutputStream(Channels.newOutputStream(channel)))) {
			output.writeInt(MAGIC);
			output.writeInt(VERSION);
			output.writeInt(states.size());
			for (BackendGenerationState state : states) {
				output.writeUTF(state.getServer());
				writeUuid(output, state.getBackendIncarnationId());
				output.writeLong(state.getBackendStartedAt());
				output.writeLong(state.getLastLifecycleTimestamp());
				output.writeBoolean(state.isStopped());
				output.writeInt(state.getRetiredIncarnations().size());
				for (UUID retired : state.getRetiredIncarnations()) {
					writeUuid(output, retired);
				}
			}
			output.flush();
			channel.force(true);
		}
		try {
			Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
		} catch (AtomicMoveNotSupportedException e) {
			Files.deleteIfExists(temporary);
			throw new IOException("Atomic backend generation state replacement is not supported", e);
		}
		// force(true) above makes the file contents durable, while forcing the parent
		// directory makes the atomic rename itself durable across sudden power loss.
		if (parent != null) {
			try (FileChannel directory = FileChannel.open(parent, StandardOpenOption.READ)) {
				directory.force(true);
			}
		}
	}

	private static UUID readUuid(DataInputStream input) throws IOException {
		return new UUID(input.readLong(), input.readLong());
	}

	private static void writeUuid(DataOutputStream output, UUID uuid) throws IOException {
		output.writeLong(uuid.getMostSignificantBits());
		output.writeLong(uuid.getLeastSignificantBits());
	}
}
