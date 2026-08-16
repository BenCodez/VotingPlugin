package com.bencodez.votingplugin.proxy.presence;

import java.io.IOException;
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
 * Backend server names are the stable backend identity. Automatic incarnation
 * identifiers fence delayed messages from replaced backend processes, while
 * per-player connection identifiers prevent delayed logout messages from
 * clearing a newer login. The tracker is deliberately independent from vote
 * routing so the presence protocol can be introduced without changing existing
 * proxy behavior.
 * </p>
 */
public class BackendPlayerPresenceTracker {
	private static final int MAX_PLAYER_NAME_LENGTH = 64;
	private static final int MAX_SERVER_NAME_LENGTH = 128;
	private static final int MAX_SNAPSHOT_PLAYERS = 100000;
	private static final int MAX_SNAPSHOT_CHUNKS = 10000;
	private static final int DEFAULT_MAX_TRACKED_BACKENDS = 1024;
	private static final int MAX_RETIRED_INCARNATIONS_PER_BACKEND = 64;
	private static final long SNAPSHOT_TIMEOUT_MILLIS = 120000L;
	private static final long SNAPSHOT_REQUEST_MIN_INTERVAL_MILLIS = 30000L;

	private final Map<UUID, PlayerPresence> playersByUuid = new HashMap<>();
	private final Map<String, UUID> uuidByPlayerName = new HashMap<>();
	private final Map<UUID, Long> lastPlayerEventSequences = new HashMap<>();
	private final Map<UUID, String> lastPlayerEventServers = new HashMap<>();
	private final Map<String, BackendState> backends = new HashMap<>();
	private final Map<String, PendingSnapshot> pendingSnapshots = new HashMap<>();
	private final int maxTrackedBackends;
	private final int maxTrackedPlayerEvents;
	private long eventSequence;

	public BackendPlayerPresenceTracker() {
		this(DEFAULT_MAX_TRACKED_BACKENDS, MAX_SNAPSHOT_PLAYERS);
	}

	/**
	 * Creates a tracker with a hard backend-state limit.
	 *
	 * @param maxTrackedBackends maximum configured backend identities retained
	 */
	public BackendPlayerPresenceTracker(int maxTrackedBackends) {
		this(maxTrackedBackends, MAX_SNAPSHOT_PLAYERS);
	}

	/**
	 * Creates a tracker with explicit backend and retained player-event limits.
	 *
	 * @param maxTrackedBackends maximum configured backend identities retained
	 * @param maxTrackedPlayerEvents maximum live players plus snapshot tombstones
	 */
	public BackendPlayerPresenceTracker(int maxTrackedBackends, int maxTrackedPlayerEvents) {
		if (maxTrackedBackends <= 0) {
			throw new IllegalArgumentException("maxTrackedBackends must be positive");
		}
		if (maxTrackedPlayerEvents <= 0 || maxTrackedPlayerEvents > MAX_SNAPSHOT_PLAYERS) {
			throw new IllegalArgumentException("maxTrackedPlayerEvents must be between 1 and 100000");
		}
		this.maxTrackedBackends = maxTrackedBackends;
		this.maxTrackedPlayerEvents = maxTrackedPlayerEvents;
	}

	public synchronized boolean playerOnline(String playerName, String uuid, String server, UUID connectionId,
			long now) {
		return playerOnline(playerName, uuid, server, connectionId, null, 0L, now, now, false);
	}

	public synchronized boolean playerOnline(String playerName, String uuid, String server, UUID connectionId,
			long backendStartedAt, long presenceTimestamp, long now) {
		return playerOnline(playerName, uuid, server, connectionId, legacyIncarnation(backendStartedAt),
				backendStartedAt, presenceTimestamp, now);
	}

	public synchronized boolean playerOnline(String playerName, String uuid, String server, UUID connectionId,
			UUID backendIncarnationId, long backendStartedAt, long presenceTimestamp, long now) {
		return playerOnline(playerName, uuid, server, connectionId, backendIncarnationId, backendStartedAt,
				presenceTimestamp, now, true);
	}

	private boolean playerOnline(String playerName, String uuid, String server, UUID connectionId,
			UUID backendIncarnationId, long backendStartedAt, long presenceTimestamp, long now, boolean fenced) {
		UUID playerUuid = parseUuid(uuid);
		String normalizedName = normalizePlayerName(playerName);
		String normalizedServer = normalizeServer(server);
		if (playerUuid == null || normalizedName == null || normalizedServer == null || connectionId == null) {
			return false;
		}
		BackendState backendState = null;
		if (fenced) {
			if (!isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)
					|| !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
				return false;
			}
			backendState = backends.get(serverKey(normalizedServer));
			if (presenceTimestamp <= backendState.lastPlayerEventTimestamp) {
				return false;
			}
			boolean conflictingBackend = false;
			PlayerPresence current = playersByUuid.get(playerUuid);
			if (current != null) {
				if (!current.getServer().equalsIgnoreCase(normalizedServer)) {
					conflictingBackend = true;
				} else if (current.getLastSeen() >= presenceTimestamp) {
					return false;
				}
			}
			UUID currentNameOwnerUuid = uuidByPlayerName.get(normalizedName);
			PlayerPresence currentNameOwner = playersByUuid.get(currentNameOwnerUuid);
			if (currentNameOwner != null) {
				if (!currentNameOwner.getServer().equalsIgnoreCase(normalizedServer)) {
					conflictingBackend = true;
				} else if (currentNameOwner.getLastSeen() >= presenceTimestamp) {
					return false;
				}
			}
			if (conflictingBackend) {
				// A cross-backend login needs a proxy-confirmed destination snapshot. It is
				// still an ordered event from the current generation, so advance that
				// backend's replay fence before asking for the handoff snapshot.
				if (!markBackendAvailable(normalizedServer, backendIncarnationId, backendStartedAt,
						presenceTimestamp, now)) {
					return false;
				}
				backendState.lastPlayerEventTimestamp = presenceTimestamp;
				return false;
			}
		}
		boolean newIdentity = !playersByUuid.containsKey(playerUuid) && !uuidByPlayerName.containsKey(normalizedName);
		if (newIdentity && playersByUuid.size() >= maxTrackedPlayerEvents) {
			return false;
		}
		if (!ensurePlayerEventCapacity(playerUuid)) {
			return false;
		}
		if (fenced ? !markBackendAvailable(normalizedServer, backendIncarnationId, backendStartedAt,
				presenceTimestamp, now)
				: !markBackendAvailable(normalizedServer, now)) {
			return false;
		}

		long sequence = ++eventSequence;
		PlayerPresence presence = new PlayerPresence(playerUuid, playerName.trim(), normalizedServer, connectionId,
				sequence, presenceTimestamp);
		putPresence(presence);
		if (backendState != null) {
			backendState.lastPlayerEventTimestamp = presenceTimestamp;
		}
		prunePlayerEventSequences();
		return true;
	}

	public synchronized boolean playerOffline(String uuid, String server, UUID connectionId, long now) {
		return playerOffline(uuid, server, connectionId, null, 0L, now, now, false);
	}

	public synchronized boolean playerOffline(String uuid, String server, UUID connectionId, long backendStartedAt,
			long presenceTimestamp, long now) {
		return playerOffline(uuid, server, connectionId, legacyIncarnation(backendStartedAt), backendStartedAt,
				presenceTimestamp, now);
	}

	public synchronized boolean playerOffline(String uuid, String server, UUID connectionId,
			UUID backendIncarnationId, long backendStartedAt, long presenceTimestamp, long now) {
		return playerOffline(uuid, server, connectionId, backendIncarnationId, backendStartedAt, presenceTimestamp,
				now, true);
	}

	private boolean playerOffline(String uuid, String server, UUID connectionId, UUID backendIncarnationId,
			long backendStartedAt, long presenceTimestamp, long now, boolean fenced) {
		UUID playerUuid = parseUuid(uuid);
		String normalizedServer = normalizeServer(server);
		if (playerUuid == null || normalizedServer == null || connectionId == null) {
			return false;
		}
		if (fenced && (!isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)
				|| !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp))) {
			return false;
		}
		BackendState backendState = fenced ? backends.get(serverKey(normalizedServer)) : null;
		if (backendState != null && presenceTimestamp <= backendState.lastPlayerEventTimestamp) {
			return false;
		}

		PlayerPresence current = playersByUuid.get(playerUuid);
		if (current == null || !current.getServer().equalsIgnoreCase(normalizedServer)
				|| !current.getConnectionId().equals(connectionId)) {
			String key = serverKey(normalizedServer);
			if (!fenced || !pendingSnapshots.containsKey(key) || !ensurePlayerEventCapacity(playerUuid)
					|| !pendingSnapshots.containsKey(key)
					|| !markBackendAvailable(normalizedServer, backendIncarnationId, backendStartedAt,
							presenceTimestamp, now)) {
				return false;
			}
			// A destination logout can race the snapshot used to confirm a cross-backend
			// handoff. Keep the current source presence, but retain a destination tombstone
			// so a snapshot captured before this logout cannot restore the stale session.
			long sequence = ++eventSequence;
			lastPlayerEventSequences.put(playerUuid, sequence);
			lastPlayerEventServers.put(playerUuid, normalizedServer);
			backendState.lastPlayerEventTimestamp = presenceTimestamp;
			prunePlayerEventSequences();
			return true;
		}
		if (fenced && current.getLastSeen() > presenceTimestamp) {
			return false;
		}
		long sequence = ++eventSequence;
		removePresence(playerUuid, sequence);
		if (fenced) {
			markBackendAvailable(normalizedServer, backendIncarnationId, backendStartedAt, presenceTimestamp, now);
			backendState.lastPlayerEventTimestamp = presenceTimestamp;
		} else {
			markBackendAvailable(normalizedServer, now);
		}
		prunePlayerEventSequences();
		return true;
	}

	public synchronized void backendStarted(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return;
		}
		if (!markBackendAvailable(normalizedServer, now)) {
			return;
		}
		long sequence = ++eventSequence;
		removePlayersOnServer(normalizedServer, sequence);
		pendingSnapshots.remove(serverKey(normalizedServer));
		prunePlayerEventSequences();
	}

	public synchronized boolean hasConflictingPresence(String playerName, String uuid, String server) {
		UUID playerUuid = parseUuid(uuid);
		String normalizedName = normalizePlayerName(playerName);
		String normalizedServer = normalizeServer(server);
		if (playerUuid == null || normalizedName == null || normalizedServer == null) {
			return false;
		}
		PlayerPresence uuidPresence = playersByUuid.get(playerUuid);
		if (uuidPresence != null && !uuidPresence.getServer().equalsIgnoreCase(normalizedServer)) {
			return true;
		}
		UUID nameOwnerUuid = uuidByPlayerName.get(normalizedName);
		PlayerPresence namePresence = playersByUuid.get(nameOwnerUuid);
		return namePresence != null && !namePresence.getServer().equalsIgnoreCase(normalizedServer);
	}

	public synchronized boolean backendStarted(String server, long backendStartedAt, long presenceTimestamp,
			long now) {
		return backendStarted(server, legacyIncarnation(backendStartedAt), backendStartedAt, presenceTimestamp, now);
	}

	public synchronized boolean backendStarted(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || backendIncarnationId == null
				|| !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
			return false;
		}
		String key = serverKey(normalizedServer);
		BackendState state = backends.get(key);
		if (state == null) {
			state = createBackendState(normalizedServer, now);
			if (state == null) {
				return false;
			}
			state.backendIncarnationId = backendIncarnationId;
			state.backendStartedAt = backendStartedAt;
			state.lastPlayerEventTimestamp = 0L;
			state.lastHeartbeatTimestamp = 0L;
			state.lastSnapshotRequestedAt = 0L;
		} else if (!backendIncarnationId.equals(state.backendIncarnationId)) {
			if (state.retiredIncarnations.contains(backendIncarnationId)) {
				return false;
			}
			// Only a BackendStarted event can advance the proxy-local restart order. The
			// current and retired UUIDs are persisted by the proxy so this ordering
			// survives proxy restarts without comparing backend clocks.
			retireCurrentIncarnation(state);
			long sequence = ++eventSequence;
			removePlayersOnServer(normalizedServer, sequence);
			pendingSnapshots.remove(key);
			state.backendIncarnationId = backendIncarnationId;
			state.backendStartedAt = backendStartedAt;
			state.lastLifecycleTimestamp = 0L;
			state.lastPlayerEventTimestamp = 0L;
			state.lastHeartbeatTimestamp = 0L;
			state.lastSnapshotRequestedAt = 0L;
			state.stopped = false;
			prunePlayerEventSequences();
		} else if (state.backendStartedAt != backendStartedAt) {
			return false;
		} else if (state.stopped) {
			return false;
		}
		if (presenceTimestamp <= state.lastLifecycleTimestamp) {
			return false;
		}
		state.lastSeen = now;
		state.available = true;
		state.stopped = false;
		state.lastLifecycleTimestamp = presenceTimestamp;
		return true;
	}

	/**
	 * Applies a backend-start transition only when its resulting generation fence
	 * can be persisted. Any persistence failure restores all tracker state changed
	 * by the transition before the exception is returned to the caller.
	 *
	 * @param server configured backend server name
	 * @param backendIncarnationId automatic process incarnation identifier
	 * @param backendStartedAt lifecycle capture timestamp
	 * @param presenceTimestamp message capture timestamp
	 * @param now proxy receipt time
	 * @param persister durable generation-state writer
	 * @return true when the transition was accepted and persisted
	 * @throws IOException when the resulting generation fence cannot be persisted
	 */
	public synchronized boolean backendStartedDurably(String server, UUID backendIncarnationId,
			long backendStartedAt, long presenceTimestamp, long now,
			BackendGenerationStatePersister persister) throws IOException {
		if (persister == null) {
			throw new IllegalArgumentException("persister must not be null");
		}
		if (!canAcceptBackendStarted(server, backendIncarnationId, backendStartedAt, presenceTimestamp)) {
			return false;
		}
		TrackerState before = captureState();
		if (!backendStarted(server, backendIncarnationId, backendStartedAt, presenceTimestamp, now)) {
			throw new IllegalStateException("Validated backend-start transition was not accepted");
		}
		try {
			persister.save(this);
			return true;
		} catch (IOException | RuntimeException e) {
			restoreState(before);
			throw e;
		}
	}

	public synchronized void backendStopped(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return;
		}
		long sequence = ++eventSequence;
		removePlayersOnServer(normalizedServer, sequence);
		pendingSnapshots.remove(serverKey(normalizedServer));
		BackendState state = backends.get(serverKey(normalizedServer));
		if (state != null) {
			state.lastSeen = now;
			state.available = false;
			state.stopped = true;
		}
		prunePlayerEventSequences();
	}

	public synchronized boolean backendStopped(String server, long backendStartedAt, long presenceTimestamp,
			long now) {
		return backendStopped(server, legacyIncarnation(backendStartedAt), backendStartedAt, presenceTimestamp, now);
	}

	public synchronized boolean backendStopped(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null
				|| !isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)) {
			return false;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		if (!isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)
				|| presenceTimestamp < state.lastLifecycleTimestamp) {
			return false;
		}
		long sequence = ++eventSequence;
		removePlayersOnServer(normalizedServer, sequence);
		pendingSnapshots.remove(serverKey(normalizedServer));
		state.lastSeen = now;
		state.lastLifecycleTimestamp = presenceTimestamp;
		state.available = false;
		state.stopped = true;
		prunePlayerEventSequences();
		return true;
	}

	/**
	 * Applies a backend-stop transition only when its resulting lifecycle fence can
	 * be persisted. Any persistence failure restores the live player and backend
	 * state removed by the attempted stop.
	 *
	 * @param server configured backend server name
	 * @param backendIncarnationId automatic process incarnation identifier
	 * @param backendStartedAt lifecycle capture timestamp
	 * @param presenceTimestamp message capture timestamp
	 * @param now proxy receipt time
	 * @param persister durable generation-state writer
	 * @return true when the transition was accepted and persisted
	 * @throws IOException when the resulting lifecycle fence cannot be persisted
	 */
	public synchronized boolean backendStoppedDurably(String server, UUID backendIncarnationId,
			long backendStartedAt, long presenceTimestamp, long now,
			BackendGenerationStatePersister persister) throws IOException {
		if (persister == null) {
			throw new IllegalArgumentException("persister must not be null");
		}
		if (!canAcceptBackendStopped(server, backendIncarnationId, backendStartedAt, presenceTimestamp)) {
			return false;
		}
		TrackerState before = captureState();
		if (!backendStopped(server, backendIncarnationId, backendStartedAt, presenceTimestamp, now)) {
			throw new IllegalStateException("Validated backend-stop transition was not accepted");
		}
		try {
			persister.save(this);
			return true;
		} catch (IOException | RuntimeException e) {
			restoreState(before);
			throw e;
		}
	}

	public synchronized void heartbeat(String server, long now) {
		expirePendingSnapshots(now);
		String normalizedServer = normalizeServer(server);
		if (normalizedServer != null) {
			markBackendAvailable(normalizedServer, now);
		}
	}

	public synchronized boolean heartbeat(String server, long backendStartedAt, long presenceTimestamp, long now) {
		return heartbeat(server, legacyIncarnation(backendStartedAt), backendStartedAt, presenceTimestamp, now);
	}

	public synchronized boolean heartbeat(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now) {
		expirePendingSnapshots(now);
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null
				|| !isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)
				|| !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
			return false;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		if (presenceTimestamp <= state.lastHeartbeatTimestamp
				|| !markBackendAvailable(normalizedServer, backendIncarnationId, backendStartedAt,
						presenceTimestamp, now)) {
			return false;
		}
		state.lastHeartbeatTimestamp = presenceTimestamp;
		return true;
	}

	public synchronized UUID beginSnapshot(String server) {
		return beginSnapshot(server, UUID.randomUUID(), System.currentTimeMillis());
	}

	public synchronized UUID beginSnapshot(String server, UUID requestId) {
		return beginSnapshot(server, requestId, System.currentTimeMillis());
	}

	public synchronized UUID beginSnapshot(String server, UUID requestId, long now) {
		return beginSnapshot(server, requestId, null, 0L, now, false);
	}

	public synchronized UUID beginSnapshot(String server, UUID requestId, long backendStartedAt, long now) {
		return beginSnapshot(server, requestId, legacyIncarnation(backendStartedAt), backendStartedAt, now);
	}

	public synchronized UUID beginSnapshot(String server, UUID requestId, UUID backendIncarnationId,
			long backendStartedAt, long now) {
		return beginSnapshot(server, requestId, backendIncarnationId, backendStartedAt, now, true);
	}

	private UUID beginSnapshot(String server, UUID requestId, UUID backendIncarnationId, long backendStartedAt,
			long now, boolean fenced) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || requestId == null) {
			return null;
		}
		if (fenced && !isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)) {
			return null;
		}
		expirePendingSnapshots(now);
		String serverKey = serverKey(normalizedServer);
		if (pendingSnapshots.containsKey(serverKey)) {
			return null;
		}
		if (pendingSnapshots.size() >= maxTrackedBackends) {
			return null;
		}
		if (fenced) {
			BackendState state = backends.get(serverKey);
			if (state == null || state.lastSnapshotRequestedAt > now
					|| (state.lastSnapshotRequestedAt > 0L
							&& now - state.lastSnapshotRequestedAt < SNAPSHOT_REQUEST_MIN_INTERVAL_MILLIS)) {
				return null;
			}
			state.lastSnapshotRequestedAt = now;
		}
		pendingSnapshots.put(serverKey,
				new PendingSnapshot(requestId, eventSequence, now, backendIncarnationId, backendStartedAt));
		prunePlayerEventSequences();
		return requestId;
	}

	public synchronized boolean applySnapshot(String server, UUID requestId, Collection<PresencePlayer> players,
			long now) {
		return applySnapshotChunk(server, requestId, 0, 1, players, now);
	}

	public synchronized boolean applySnapshotChunk(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players, long now) {
		return applySnapshotChunk(server, requestId, chunkIndex, chunkCount, players, null, 0L, now, now, false);
	}

	public synchronized boolean applySnapshotChunk(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players, long backendStartedAt, long presenceTimestamp, long now) {
		return applySnapshotChunk(server, requestId, chunkIndex, chunkCount, players,
				legacyIncarnation(backendStartedAt), backendStartedAt, presenceTimestamp, now);
	}

	public synchronized boolean applySnapshotChunk(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now) {
		return applySnapshotChunk(server, requestId, chunkIndex, chunkCount, players, backendIncarnationId,
				backendStartedAt, presenceTimestamp, now, true);
	}

	private boolean applySnapshotChunk(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now, boolean fenced) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || requestId == null || players == null || chunkIndex < 0 || chunkCount <= 0
				|| chunkCount > MAX_SNAPSHOT_CHUNKS || chunkIndex >= chunkCount
				|| players.size() > MAX_SNAPSHOT_PLAYERS) {
			return false;
		}

		String serverKey = serverKey(normalizedServer);
		expirePendingSnapshots(now);
		PendingSnapshot pending = pendingSnapshots.get(serverKey);
		if (pending == null || !pending.requestId.equals(requestId)
				|| !java.util.Objects.equals(pending.backendIncarnationId, backendIncarnationId)
				|| pending.backendStartedAt != backendStartedAt
				|| (fenced && !isCurrentBackendGeneration(normalizedServer, backendIncarnationId,
						backendStartedAt))) {
			return false;
		}
		if (fenced && !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
			return false;
		}
		if (fenced) {
			if (pending.snapshotTimestamp == 0L) {
				pending.snapshotTimestamp = presenceTimestamp;
			} else if (pending.snapshotTimestamp != presenceTimestamp) {
				return false;
			}
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
		if (pending.playerCount > MAX_SNAPSHOT_PLAYERS - players.size()) {
			pendingSnapshots.remove(serverKey);
			prunePlayerEventSequences();
			return false;
		}
		pending.chunkCount = chunkCount;
		pending.chunks.put(chunkIndex, new ArrayList<>(players));
		pending.playerCount += players.size();
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
		Set<UUID> retainedEventIdentities = new HashSet<>(lastPlayerEventSequences.keySet());
		retainedEventIdentities.addAll(snapshotPlayers.keySet());
		if (retainedEventIdentities.size() > maxTrackedPlayerEvents) {
			pendingSnapshots.remove(serverKey);
			prunePlayerEventSequences();
			return false;
		}
		int retainedPlayers = playersByUuid.size();
		for (PlayerPresence current : playersByUuid.values()) {
			if (current.getServer().equalsIgnoreCase(normalizedServer)
					&& current.getLastEventSequence() <= pending.eventWatermark) {
				retainedPlayers--;
			}
		}
		if (retainedPlayers + snapshotPlayers.size() > MAX_SNAPSHOT_PLAYERS) {
			return false;
		}
		if (pending.backendStartedAt > 0L ? !markBackendAvailable(normalizedServer, pending.backendIncarnationId,
				pending.backendStartedAt, pending.snapshotTimestamp, now)
				: !markBackendAvailable(normalizedServer, now)) {
			return false;
		}
		BackendState backendState = backends.get(serverKey);
		if (pending.snapshotTimestamp > 0L && backendState != null) {
			backendState.lastPlayerEventTimestamp = Math.max(backendState.lastPlayerEventTimestamp,
					pending.snapshotTimestamp);
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
				PlayerPresence currentPresence = playersByUuid.get(snapshot.uuid);
				String lastEventServer = lastPlayerEventServers.get(snapshot.uuid);
				if (currentPresence != null || lastEventServer == null
						|| lastEventServer.equalsIgnoreCase(normalizedServer)) {
					continue;
				}
			}
			UUID nameOwnerUuid = uuidByPlayerName.get(snapshot.normalizedName);
			if (nameOwnerUuid != null && !nameOwnerUuid.equals(snapshot.uuid)) {
				PlayerPresence nameOwner = playersByUuid.get(nameOwnerUuid);
				if (nameOwner != null && nameOwner.getLastEventSequence() > pending.eventWatermark) {
					continue;
				}
			}
			putPresence(new PlayerPresence(snapshot.uuid, snapshot.playerName, normalizedServer,
					snapshot.connectionId, snapshotSequence,
					pending.snapshotTimestamp > 0L ? pending.snapshotTimestamp : now));
		}

		pendingSnapshots.remove(serverKey);
		prunePlayerEventSequences();
		return true;
	}

	public synchronized Set<String> expireBackends(long now, long timeoutMillis) {
		if (timeoutMillis < 0) {
			return Collections.emptySet();
		}
		expirePendingSnapshots(now);
		Set<String> expired = new LinkedHashSet<>();
		for (Map.Entry<String, BackendState> entry : backends.entrySet()) {
			BackendState state = entry.getValue();
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

	/**
	 * Returns the active snapshot request for a backend, expiring stale requests
	 * first.
	 *
	 * @param server configured backend server name
	 * @param now current proxy time in milliseconds
	 * @return active request identifier, or null when none remains
	 */
	public synchronized UUID getPendingSnapshotRequestId(String server, long now) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return null;
		}
		expirePendingSnapshots(now);
		PendingSnapshot pending = pendingSnapshots.get(serverKey(normalizedServer));
		return pending == null ? null : pending.requestId;
	}

	public synchronized int getTrackedBackendCount() {
		return backends.size();
	}

	public synchronized int getRetainedPlayerEventCount() {
		return lastPlayerEventSequences.size();
	}

	/**
	 * Exports the bounded lifecycle-ordering state needed to fence old process
	 * reannouncements after a proxy restart. Player presence and availability are
	 * deliberately not persisted.
	 *
	 * @return immutable lifecycle state snapshot
	 */
	public synchronized List<BackendGenerationState> getBackendGenerationStates() {
		List<BackendGenerationState> states = new ArrayList<>();
		for (BackendState state : backends.values()) {
			if (state.backendIncarnationId != null && state.backendStartedAt > 0L) {
				states.add(new BackendGenerationState(state.server, state.backendIncarnationId,
						state.backendStartedAt, state.lastLifecycleTimestamp, state.stopped,
						state.retiredIncarnations));
			}
		}
		return Collections.unmodifiableList(states);
	}

	/**
	 * Restores lifecycle ordering before proxy message listeners start. Restored
	 * active generations begin unavailable and must prove liveness with a heartbeat;
	 * their players are recovered through a fresh snapshot.
	 *
	 * @param states validated persisted states
	 * @param now current proxy time
	 * @return backend names that require heartbeat and snapshot recovery
	 */
	public synchronized Set<String> restoreBackendGenerationStates(Collection<BackendGenerationState> states,
			long now) {
		if (states == null || !backends.isEmpty()) {
			return Collections.emptySet();
		}
		Set<String> recoveryServers = new LinkedHashSet<>();
		for (BackendGenerationState persisted : states) {
			if (persisted == null || backends.size() >= maxTrackedBackends) {
				break;
			}
			String server = normalizeServer(persisted.server);
			if (server == null || persisted.backendIncarnationId == null || persisted.backendStartedAt <= 0L
					|| persisted.lastLifecycleTimestamp < persisted.backendStartedAt
					|| persisted.retiredIncarnations.size() > MAX_RETIRED_INCARNATIONS_PER_BACKEND) {
				continue;
			}
			String key = serverKey(server);
			if (backends.containsKey(key)) {
				continue;
			}
			BackendState state = new BackendState(server);
			state.backendIncarnationId = persisted.backendIncarnationId;
			state.backendStartedAt = persisted.backendStartedAt;
			state.lastLifecycleTimestamp = persisted.lastLifecycleTimestamp;
			state.lastSeen = now;
			state.available = false;
			state.stopped = persisted.stopped;
			state.retiredIncarnations.addAll(persisted.retiredIncarnations);
			state.retiredIncarnations.remove(state.backendIncarnationId);
			backends.put(key, state);
			if (!state.stopped) {
				recoveryServers.add(server);
			}
		}
		return Collections.unmodifiableSet(recoveryServers);
	}

	public synchronized UUID getBackendIncarnationId(String server) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return null;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		return state == null || !state.available || state.stopped ? null : state.backendIncarnationId;
	}

	public synchronized long getBackendStartedAt(String server) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null) {
			return 0L;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		return state == null || !state.available || state.stopped ? 0L : state.backendStartedAt;
	}

	private TrackerState captureState() {
		return new TrackerState(this);
	}

	private boolean canAcceptBackendStarted(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null || backendIncarnationId == null
				|| !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
			return false;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		if (state == null) {
			if (backends.size() < maxTrackedBackends) {
				return true;
			}
			for (BackendState candidate : backends.values()) {
				if (!candidate.available) {
					return true;
				}
			}
			return false;
		}
		if (!backendIncarnationId.equals(state.backendIncarnationId)) {
			return !state.retiredIncarnations.contains(backendIncarnationId);
		}
		return state.backendStartedAt == backendStartedAt && !state.stopped
				&& presenceTimestamp > state.lastLifecycleTimestamp;
	}

	private boolean canAcceptBackendStopped(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp) {
		String normalizedServer = normalizeServer(server);
		if (normalizedServer == null
				|| !isCurrentBackendGeneration(normalizedServer, backendIncarnationId, backendStartedAt)) {
			return false;
		}
		BackendState state = backends.get(serverKey(normalizedServer));
		return isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)
				&& presenceTimestamp >= state.lastLifecycleTimestamp;
	}

	private void restoreState(TrackerState state) {
		playersByUuid.clear();
		playersByUuid.putAll(state.playersByUuid);
		uuidByPlayerName.clear();
		uuidByPlayerName.putAll(state.uuidByPlayerName);
		lastPlayerEventSequences.clear();
		lastPlayerEventSequences.putAll(state.lastPlayerEventSequences);
		lastPlayerEventServers.clear();
		lastPlayerEventServers.putAll(state.lastPlayerEventServers);
		backends.clear();
		backends.putAll(state.backends);
		pendingSnapshots.clear();
		pendingSnapshots.putAll(state.pendingSnapshots);
		eventSequence = state.eventSequence;
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
		lastPlayerEventServers.put(presence.getUuid(), presence.getServer());
	}

	private void removePresence(UUID uuid, long sequence) {
		PlayerPresence removed = playersByUuid.remove(uuid);
		if (removed != null) {
			uuidByPlayerName.remove(nameKey(removed.getPlayerName()), uuid);
			lastPlayerEventServers.put(uuid, removed.getServer());
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
			lastPlayerEventServers.keySet().retainAll(playersByUuid.keySet());
			return;
		}

		long oldestWatermark = Long.MAX_VALUE;
		for (PendingSnapshot pending : pendingSnapshots.values()) {
			oldestWatermark = Math.min(oldestWatermark, pending.eventWatermark);
		}
		final long retainAfter = oldestWatermark;
		lastPlayerEventSequences.entrySet()
				.removeIf(entry -> !playersByUuid.containsKey(entry.getKey()) && entry.getValue() <= retainAfter);
		lastPlayerEventServers.keySet().retainAll(lastPlayerEventSequences.keySet());
	}

	private void expirePendingSnapshots(long now) {
		boolean removed = pendingSnapshots.values().removeIf(pending -> pending.isExpired(now));
		if (removed) {
			prunePlayerEventSequences();
		}
	}

	private boolean ensurePlayerEventCapacity(UUID playerUuid) {
		if (lastPlayerEventSequences.containsKey(playerUuid)
				|| lastPlayerEventSequences.size() < maxTrackedPlayerEvents) {
			return true;
		}
		if (!pendingSnapshots.isEmpty()) {
			pendingSnapshots.clear();
			prunePlayerEventSequences();
		}
		return lastPlayerEventSequences.size() < maxTrackedPlayerEvents;
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

	private boolean markBackendAvailable(String server, long now) {
		String key = serverKey(server);
		BackendState state = backends.get(key);
		if (state == null) {
			if (backends.size() >= maxTrackedBackends) {
				evictOldestUnavailableBackend();
			}
			if (backends.size() >= maxTrackedBackends) {
				return false;
			}
			state = new BackendState(server);
			backends.put(key, state);
		}
		state.lastSeen = now;
		state.available = true;
		state.stopped = false;
		return true;
	}

	private boolean markBackendAvailable(String server, UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, long now) {
		if (backendIncarnationId == null || !isValidPresenceTimestamp(backendStartedAt, presenceTimestamp)) {
			return false;
		}
		String key = serverKey(server);
		BackendState state = backends.get(key);
		if (state == null) {
			state = createBackendState(server, now);
			if (state == null) {
				return false;
			}
			state.backendIncarnationId = backendIncarnationId;
			state.backendStartedAt = backendStartedAt;
		} else if (!backendIncarnationId.equals(state.backendIncarnationId)
				|| backendStartedAt != state.backendStartedAt || state.stopped) {
			return false;
		}
		state.lastSeen = now;
		state.available = true;
		return true;
	}

	private boolean isCurrentBackendGeneration(String server, UUID backendIncarnationId, long backendStartedAt) {
		if (backendIncarnationId == null || backendStartedAt <= 0L) {
			return false;
		}
		BackendState state = backends.get(serverKey(server));
		return state != null && backendIncarnationId.equals(state.backendIncarnationId)
				&& state.backendStartedAt == backendStartedAt && !state.stopped;
	}

	private static boolean isValidPresenceTimestamp(long backendStartedAt, long presenceTimestamp) {
		return backendStartedAt > 0L && presenceTimestamp >= backendStartedAt;
	}

	private BackendState createBackendState(String server, long now) {
		if (backends.size() >= maxTrackedBackends) {
			evictOldestUnavailableBackend();
		}
		if (backends.size() >= maxTrackedBackends) {
			return null;
		}
		BackendState state = new BackendState(server);
		state.lastSeen = now;
		backends.put(serverKey(server), state);
		return state;
	}

	private void retireCurrentIncarnation(BackendState state) {
		if (state.backendIncarnationId == null) {
			return;
		}
		retireIncarnation(state, state.backendIncarnationId);
	}

	private void retireIncarnation(BackendState state, UUID backendIncarnationId) {
		state.retiredIncarnations.add(backendIncarnationId);
		while (state.retiredIncarnations.size() > MAX_RETIRED_INCARNATIONS_PER_BACKEND) {
			java.util.Iterator<UUID> iterator = state.retiredIncarnations.iterator();
			if (iterator.hasNext()) {
				iterator.next();
				iterator.remove();
			}
		}
	}

	private static UUID legacyIncarnation(long backendStartedAt) {
		return backendStartedAt <= 0L ? null : new UUID(0L, backendStartedAt);
	}

	private void evictOldestUnavailableBackend() {
		String oldestKey = null;
		long oldestLastSeen = Long.MAX_VALUE;
		for (Map.Entry<String, BackendState> entry : backends.entrySet()) {
			BackendState state = entry.getValue();
			if (!state.available && state.lastSeen < oldestLastSeen) {
				oldestKey = entry.getKey();
				oldestLastSeen = state.lastSeen;
			}
		}
		if (oldestKey != null) {
			backends.remove(oldestKey);
			if (pendingSnapshots.remove(oldestKey) != null) {
				prunePlayerEventSequences();
			}
		}
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
		private final Set<UUID> retiredIncarnations = new LinkedHashSet<>();
		private UUID backendIncarnationId;
		private long backendStartedAt;
		private long lastLifecycleTimestamp;
		private long lastPlayerEventTimestamp;
		private long lastHeartbeatTimestamp;
		private long lastSnapshotRequestedAt;
		private long lastSeen;
		private boolean available;
		private boolean stopped;

		private BackendState(String server) {
			this.server = server;
		}
	}

	/**
	 * Writes the tracker's current backend-generation snapshot while the lifecycle
	 * transition is held under the tracker lock.
	 */
	@FunctionalInterface
	public interface BackendGenerationStatePersister {
		void save(BackendPlayerPresenceTracker tracker) throws IOException;
	}

	/**
	 * Durable, non-secret backend lifecycle ordering record.
	 */
	public static final class BackendGenerationState {
		private final String server;
		private final UUID backendIncarnationId;
		private final long backendStartedAt;
		private final long lastLifecycleTimestamp;
		private final boolean stopped;
		private final Set<UUID> retiredIncarnations;

		public BackendGenerationState(String server, UUID backendIncarnationId, long backendStartedAt,
				long lastLifecycleTimestamp, boolean stopped, Collection<UUID> retiredIncarnations) {
			this.server = server;
			this.backendIncarnationId = backendIncarnationId;
			this.backendStartedAt = backendStartedAt;
			this.lastLifecycleTimestamp = lastLifecycleTimestamp;
			this.stopped = stopped;
			LinkedHashSet<UUID> retired = retiredIncarnations == null ? new LinkedHashSet<>()
					: new LinkedHashSet<>(retiredIncarnations);
			retired.remove(null);
			this.retiredIncarnations = Collections.unmodifiableSet(retired);
		}

		public String getServer() {
			return server;
		}

		public UUID getBackendIncarnationId() {
			return backendIncarnationId;
		}

		public long getBackendStartedAt() {
			return backendStartedAt;
		}

		public long getLastLifecycleTimestamp() {
			return lastLifecycleTimestamp;
		}

		public boolean isStopped() {
			return stopped;
		}

		public Set<UUID> getRetiredIncarnations() {
			return retiredIncarnations;
		}
	}

	private static final class PendingSnapshot {
		private final UUID requestId;
		private final long eventWatermark;
		private final long requestedAt;
		private final UUID backendIncarnationId;
		private final long backendStartedAt;
		private final Map<Integer, List<PresencePlayer>> chunks = new HashMap<>();
		private int chunkCount;
		private int playerCount;
		private long snapshotTimestamp;

		private PendingSnapshot(UUID requestId, long eventWatermark, long requestedAt, UUID backendIncarnationId,
				long backendStartedAt) {
			this.requestId = requestId;
			this.eventWatermark = eventWatermark;
			this.requestedAt = requestedAt;
			this.backendIncarnationId = backendIncarnationId;
			this.backendStartedAt = backendStartedAt;
		}

		private boolean isExpired(long now) {
			return now >= requestedAt && now - requestedAt >= SNAPSHOT_TIMEOUT_MILLIS;
		}
	}

	private static final class TrackerState {
		private final Map<UUID, PlayerPresence> playersByUuid;
		private final Map<String, UUID> uuidByPlayerName;
		private final Map<UUID, Long> lastPlayerEventSequences;
		private final Map<UUID, String> lastPlayerEventServers;
		private final Map<String, BackendState> backends = new HashMap<>();
		private final Map<String, PendingSnapshot> pendingSnapshots = new HashMap<>();
		private final long eventSequence;

		private TrackerState(BackendPlayerPresenceTracker tracker) {
			playersByUuid = new HashMap<>(tracker.playersByUuid);
			uuidByPlayerName = new HashMap<>(tracker.uuidByPlayerName);
			lastPlayerEventSequences = new HashMap<>(tracker.lastPlayerEventSequences);
			lastPlayerEventServers = new HashMap<>(tracker.lastPlayerEventServers);
			for (Map.Entry<String, BackendState> entry : tracker.backends.entrySet()) {
				BackendState source = entry.getValue();
				BackendState copy = new BackendState(source.server);
				copy.retiredIncarnations.addAll(source.retiredIncarnations);
				copy.backendIncarnationId = source.backendIncarnationId;
				copy.backendStartedAt = source.backendStartedAt;
				copy.lastLifecycleTimestamp = source.lastLifecycleTimestamp;
				copy.lastPlayerEventTimestamp = source.lastPlayerEventTimestamp;
				copy.lastHeartbeatTimestamp = source.lastHeartbeatTimestamp;
				copy.lastSnapshotRequestedAt = source.lastSnapshotRequestedAt;
				copy.lastSeen = source.lastSeen;
				copy.available = source.available;
				copy.stopped = source.stopped;
				backends.put(entry.getKey(), copy);
			}
			for (Map.Entry<String, PendingSnapshot> entry : tracker.pendingSnapshots.entrySet()) {
				PendingSnapshot source = entry.getValue();
				PendingSnapshot copy = new PendingSnapshot(source.requestId, source.eventWatermark,
						source.requestedAt, source.backendIncarnationId, source.backendStartedAt);
				for (Map.Entry<Integer, List<PresencePlayer>> chunk : source.chunks.entrySet()) {
					copy.chunks.put(chunk.getKey(), new ArrayList<>(chunk.getValue()));
				}
				copy.chunkCount = source.chunkCount;
				copy.playerCount = source.playerCount;
				copy.snapshotTimestamp = source.snapshotTimestamp;
				pendingSnapshots.put(entry.getKey(), copy);
			}
			eventSequence = tracker.eventSequence;
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
