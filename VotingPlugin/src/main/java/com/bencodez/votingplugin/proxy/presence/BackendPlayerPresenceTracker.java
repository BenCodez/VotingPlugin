package com.bencodez.votingplugin.proxy.presence;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.bencodez.votingplugin.proxy.VotingPluginWire.PresencePlayer;

/**
 * Tracks player locations reported through the existing global-message system.
 *
 * <p>
 * Backend server names are the stable backend identity. Per-player connection
 * identifiers prevent delayed logout messages from clearing a newer login. The
 * tracker is deliberately independent from vote routing so the presence
 * protocol can be introduced without changing existing proxy behavior.
 * </p>
 */
public class BackendPlayerPresenceTracker {
	private static final int MAX_PLAYER_NAME_LENGTH = 64;
	private static final int MAX_SERVER_NAME_LENGTH = 128;
	private static final int MAX_SNAPSHOT_PLAYERS = 100000;
	private static final int MAX_SNAPSHOT_CHUNKS = 10000;

	private final Map<UUID, PlayerPresence> playersByUuid = new HashMap<>();
	private final Map<String, UUID> uuidByPlayerName = new HashMap<>();
	private final Map<UUID, Long> lastPlayerEventSequences = new HashMap<>();
	private final Map<String, BackendState> backends = new HashMap<>();
	private final Map<String, PendingSnapshot> pendingSnapshots = new HashMap<>();
	private long eventSequence;

	public synchronized boolean playerOnline(String playerName, String uuid, String server, UUID connectionId,
			long now) {
		UUID playerUuid = parseUuid(uuid);
		String normalizedName = normalizePlayerName(playerName);
		String normalizedServer = normalizeServer(server);
		if (playerUuid == null || normalizedName == null || normalizedServer == null || connectionId == null) {
			return false;
		}

		long sequence = ++eventSequence;
		PlayerPresence presence = new PlayerPresence(playerUuid, playerName.trim(), normalizedServer, connectionId,
				sequence, now);
		putPresence(presence);
		markBackendAvailable(normalizedServer, now);
		prunePlayerEventSequences();
		return true;
	}

	public synchronized boolean playerOffline(String uuid, String server, UUID connectionId, long now) {
		UUID playerUuid = parseUuid(uuid);
		String normalizedServer = normalizeServer(server);
		if (playerUuid == null || normalizedServer == null || connectionId == null) {
			return false;
		}

		long sequence = ++eventSequence;
		markBackendAvailable(normalizedServer, now);
		PlayerPresence current = playersByUuid.get(playerUuid);
		if (current == null || !current.getServer().equalsIgnoreCase(normalizedServer)
				|| !current.getConnectionId().equals(connectionId)) {
			return false;
		}
		removePresence(playerUuid, sequence);
		prunePlayerEventSequences();
		return true;
	}

	public synchronized void backendStarted(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return;
		}
		long sequence = ++eventSequence;
		removePlayersOnServer(normalizedServer, sequence);
		pendingSnapshots.remove(serverKey(normalizedServer));
		markBackendAvailable(normalizedServer, now);
		prunePlayerEventSequences();
	}

	public synchronized void backendStopped(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return;
		}
		long sequence = ++eventSequence;
		removePlayersOnServer(normalizedServer, sequence);
		pendingSnapshots.remove(serverKey(normalizedServer));
		BackendState state = backends.computeIfAbsent(serverKey(normalizedServer), key -> new BackendState(normalizedServer));
		state.lastSeen = now;
		state.available = false;
		prunePlayerEventSequences();
	}

	public synchronized void heartbeat(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer != null) {
			markBackendAvailable(normalizedServer, now);
		}
	}

	public synchronized UUID beginSnapshot(String server) {
		return beginSnapshot(server, UUID.randomUUID());
	}

	public synchronized UUID beginSnapshot(String server, UUID requestId) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || requestId == null) {
			return null;
		}
		pendingSnapshots.put(serverKey(normalizedServer), new PendingSnapshot(requestId, eventSequence));
		prunePlayerEventSequences();
		return requestId;
	}

	public synchronized boolean applySnapshot(String server, UUID requestId, Collection<PresencePlayer> players,
			long now) {
		return applySnapshotChunk(server, requestId, 0, 1, players, now);
	}

	public synchronized boolean applySnapshotChunk(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || requestId == null || players == null || chunkIndex < 0 || chunkCount <= 0
				|| chunkCount > MAX_SNAPSHOT_CHUNKS || chunkIndex >= chunkCount
				|| players.size() > MAX_SNAPSHOT_PLAYERS) {
			return false;
		}

		String serverKey = serverKey(normalizedServer);
		PendingSnapshot pending = pendingSnapshots.get(serverKey);
		if (pending == null || !pending.requestId.equals(requestId)) {
			return false;
		}
		if ((pending.chunkCount != 0 && pending.chunkCount != chunkCount)
				|| pending.chunks.containsKey(chunkIndex)) {
			return false;
		}

		for (PresencePlayer player : players) {
			if (parseSnapshotPlayer(player) == null) {
				return false;
			}
		}
		pending.chunkCount = chunkCount;
		pending.chunks.put(chunkIndex, new ArrayList<>(players));
		if (pending.chunks.size() < chunkCount) {
			return true;
		}

		List<PresencePlayer> completeSnapshot = new ArrayList<>();
		for (int index = 0; index < chunkCount; index++) {
			List<PresencePlayer> chunk = pending.chunks.get(index);
			if (chunk == null || completeSnapshot.size() + chunk.size() > MAX_SNAPSHOT_PLAYERS) {
				return false;
			}
			completeSnapshot.addAll(chunk);
		}
		return applyCompleteSnapshot(normalizedServer, serverKey, pending, completeSnapshot, now);
	}

	private boolean applyCompleteSnapshot(String normalizedServer, String serverKey, PendingSnapshot pending,
			Collection<PresencePlayer> players, long now) {
		Map<UUID, SnapshotPresence> snapshotPlayers = new LinkedHashMap<>();
		Set<String> snapshotNames = new HashSet<>();
		for (PresencePlayer player : players) {
			SnapshotPresence parsed = parseSnapshotPlayer(player);
			if (parsed == null || !snapshotNames.add(parsed.normalizedName)
					|| snapshotPlayers.put(parsed.uuid, parsed) != null) {
				return false;
			}
		}

		long snapshotSequence = ++eventSequence;
		List<UUID> toRemove = new ArrayList<>();
		for (PlayerPresence current : playersByUuid.values()) {
			if (current.getServer().equalsIgnoreCase(normalizedServer)
					&& current.getLastEventSequence() <= pending.eventWatermark
					&& !snapshotPlayers.containsKey(current.getUuid())) {
				toRemove.add(current.getUuid());
			}
		}
		for (UUID uuid : toRemove) {
			removePresence(uuid, snapshotSequence);
		}

		for (SnapshotPresence snapshot : snapshotPlayers.values()) {
			long lastPlayerEvent = lastPlayerEventSequences.getOrDefault(snapshot.uuid, 0L);
			if (lastPlayerEvent > pending.eventWatermark) {
				continue;
			}
			putPresence(new PlayerPresence(snapshot.uuid, snapshot.playerName, normalizedServer,
					snapshot.connectionId, snapshotSequence, now));
		}

		pendingSnapshots.remove(serverKey);
		markBackendAvailable(normalizedServer, now);
		prunePlayerEventSequences();
		return true;
	}

	public synchronized Set<String> expireBackends(long now, long timeoutMillis) {
		if (timeoutMillis < 0) {
			return Collections.emptySet();
		}
		Set<String> expired = new LinkedHashSet<>();
		for (BackendState state : backends.values()) {
			if (state.available && now - state.lastSeen > timeoutMillis) {
				state.available = false;
				expired.add(state.server);
			}
		}
		if (!expired.isEmpty()) {
			long sequence = ++eventSequence;
			for (String server : expired) {
				removePlayersOnServer(server, sequence);
				pendingSnapshots.remove(serverKey(server));
			}
			prunePlayerEventSequences();
		}
		return Collections.unmodifiableSet(expired);
	}

	public synchronized Optional<PlayerPresence> getPlayer(UUID uuid) {
		return Optional.ofNullable(playersByUuid.get(uuid));
	}

	public synchronized Optional<PlayerPresence> getPlayer(String playerName) {
		String normalizedName = normalizePlayerName(playerName);
		if (normalizedName == null) {
			return Optional.empty();
		}
		UUID uuid = uuidByPlayerName.get(normalizedName);
		return uuid == null ? Optional.empty() : Optional.ofNullable(playersByUuid.get(uuid));
	}

	public synchronized List<PlayerPresence> getOnlinePlayers() {
		return Collections.unmodifiableList(new ArrayList<>(playersByUuid.values()));
	}

	public synchronized BackendPresenceStatus getBackendStatus(String server) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return null;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		if (state == null) {
			return null;
		}
		return new BackendPresenceStatus(state.server, state.lastSeen, state.available,
				countPlayersOnServer(state.server));
	}

	public synchronized int getOnlinePlayerCount() {
		return playersByUuid.size();
	}

	public synchronized int getPendingSnapshotCount() {
		return pendingSnapshots.size();
	}

	private SnapshotPresence parseSnapshotPlayer(PresencePlayer player) {
		if (player == null) {
			return null;
		}
		UUID uuid = parseUuid(player.uuid);
		UUID connectionId = parseUuid(player.connectionId);
		String normalizedName = normalizePlayerName(player.player);
		if (uuid == null || connectionId == null || normalizedName == null) {
			return null;
		}
		return new SnapshotPresence(uuid, player.player.trim(), normalizedName, connectionId);
	}

	private void putPresence(PlayerPresence presence) {
		String nameKey = nameKey(presence.getPlayerName());
		UUID previousNameUuid = uuidByPlayerName.get(nameKey);
		if (previousNameUuid != null && !previousNameUuid.equals(presence.getUuid())) {
			removePresence(previousNameUuid, presence.getLastEventSequence());
		}
		PlayerPresence previous = playersByUuid.put(presence.getUuid(), presence);
		if (previous != null) {
			uuidByPlayerName.remove(nameKey(previous.getPlayerName()), previous.getUuid());
		}
		uuidByPlayerName.put(nameKey, presence.getUuid());
		lastPlayerEventSequences.put(presence.getUuid(), presence.getLastEventSequence());
	}

	private void removePresence(UUID uuid, long sequence) {
		PlayerPresence removed = playersByUuid.remove(uuid);
		if (removed != null) {
			uuidByPlayerName.remove(nameKey(removed.getPlayerName()), uuid);
		}
		lastPlayerEventSequences.put(uuid, sequence);
	}

	private void removePlayersOnServer(String server, long sequence) {
		List<UUID> toRemove = new ArrayList<>();
		for (PlayerPresence presence : playersByUuid.values()) {
			if (presence.getServer().equalsIgnoreCase(server)) {
				toRemove.add(presence.getUuid());
			}
		}
		for (UUID uuid : toRemove) {
			removePresence(uuid, sequence);
		}
	}

	private void prunePlayerEventSequences() {
		if (pendingSnapshots.isEmpty()) {
			lastPlayerEventSequences.keySet().retainAll(playersByUuid.keySet());
			return;
		}

		long oldestWatermark = Long.MAX_VALUE;
		for (PendingSnapshot pending : pendingSnapshots.values()) {
			oldestWatermark = Math.min(oldestWatermark, pending.eventWatermark);
		}
		final long retainAfter = oldestWatermark;
		lastPlayerEventSequences.entrySet()
				.removeIf(entry -> !playersByUuid.containsKey(entry.getKey()) && entry.getValue() <= retainAfter);
	}

	private int countPlayersOnServer(String server) {
		int count = 0;
		for (PlayerPresence presence : playersByUuid.values()) {
			if (presence.getServer().equalsIgnoreCase(server)) {
				count++;
			}
		}
		return count;
	}

	private void markBackendAvailable(String server, long now) {
		BackendState state = backends.computeIfAbsent(serverKey(server), key -> new BackendState(server));
		state.lastSeen = now;
		state.available = true;
	}

	private static UUID parseUuid(String value) {
		if (value == null || value.isBlank()) {
			return null;
		}
		try {
			return UUID.fromString(value.trim());
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	private static String normalizePlayerName(String playerName) {
		if (playerName == null) {
			return null;
		}
		String trimmed = playerName.trim();
		if (trimmed.isEmpty() || trimmed.length() > MAX_PLAYER_NAME_LENGTH) {
			return null;
		}
		return nameKey(trimmed);
	}

	private static String normalizeServer(String server) {
		if (server == null) {
			return null;
		}
		String trimmed = server.trim();
		if (trimmed.isEmpty() || trimmed.length() > MAX_SERVER_NAME_LENGTH) {
			return null;
		}
		return trimmed;
	}

	private static String nameKey(String playerName) {
		return playerName.toLowerCase(Locale.ROOT);
	}

	private static String serverKey(String server) {
		return server.toLowerCase(Locale.ROOT);
	}

	private static final class BackendState {
		private final String server;
		private long lastSeen;
		private boolean available;

		private BackendState(String server) {
			this.server = server;
		}
	}

	private static final class PendingSnapshot {
		private final UUID requestId;
		private final long eventWatermark;
		private final Map<Integer, List<PresencePlayer>> chunks = new HashMap<>();
		private int chunkCount;

		private PendingSnapshot(UUID requestId, long eventWatermark) {
			this.requestId = requestId;
			this.eventWatermark = eventWatermark;
		}
	}

	private static final class SnapshotPresence {
		private final UUID uuid;
		private final String playerName;
		private final String normalizedName;
		private final UUID connectionId;

		private SnapshotPresence(UUID uuid, String playerName, String normalizedName, UUID connectionId) {
			this.uuid = uuid;
			this.playerName = playerName;
			this.normalizedName = normalizedName;
			this.connectionId = connectionId;
		}
	}
}
