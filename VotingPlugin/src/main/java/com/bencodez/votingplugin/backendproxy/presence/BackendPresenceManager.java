package com.bencodez.votingplugin.backendproxy.presence;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Owns backend player-presence lifecycle, sessions, heartbeats, and snapshots.
 */
public class BackendPresenceManager {

	private static final long HEARTBEAT_SECONDS = 30;
	private static final long RESYNC_REQUEST_MIN_INTERVAL_NANOS = TimeUnit.SECONDS.toNanos(5);
	private static final long SNAPSHOT_REQUEST_MIN_INTERVAL_NANOS = TimeUnit.SECONDS.toNanos(30);
	private static final int SNAPSHOT_CHUNK_SIZE = 100;

	private final VotingPluginMain plugin;
	private final BungeeMethod method;
	private final GlobalMessageHandler globalMessageHandler;
	private final ConcurrentHashMap<String, BackendPlayerPresenceSession> playerSessions = new ConcurrentHashMap<>();
	private final Object lifecycleLock = new Object();

	private boolean reporting;
	private String server;
	private UUID incarnationId;
	private long startedAt;
	private long lastTimestamp;
	private UUID lastResyncRequestId;
	private long lastResyncRequestAtNanos;
	private UUID lastSnapshotRequestId;
	private long lastSnapshotRequestAtNanos;
	private ScheduledFuture<?> heartbeatTask;

	public BackendPresenceManager(VotingPluginMain plugin, BungeeMethod method,
			GlobalMessageHandler globalMessageHandler) {
		this.plugin = plugin;
		this.method = method;
		this.globalMessageHandler = globalMessageHandler;
	}

	public void start() {
		if (globalMessageHandler == null || method == null || !method.supportsBackendPresence()) {
			return;
		}
		String configuredServer = plugin.getBungeeSettings().getServer();
		synchronized (lifecycleLock) {
			long now = System.currentTimeMillis();
			incarnationId = UUID.randomUUID();
			startedAt = now;
			lastTimestamp = now;
			server = configuredServer;
			reporting = true;
			lastResyncRequestId = null;
			lastResyncRequestAtNanos = 0L;
			lastSnapshotRequestId = null;
			lastSnapshotRequestAtNanos = 0L;
			if (heartbeatTask != null) {
				heartbeatTask.cancel(false);
			}
			try {
				heartbeatTask = plugin.getTimer().scheduleAtFixedRate(new Runnable() {
					@Override
					public void run() {
						sendHeartbeat();
					}
				}, HEARTBEAT_SECONDS, HEARTBEAT_SECONDS, TimeUnit.SECONDS);
				seedOnlinePlayers();
			} catch (RuntimeException failure) {
				if (heartbeatTask != null) heartbeatTask.cancel(false);
				heartbeatTask = null;
				reporting = false;
				server = null;
				incarnationId = null;
				throw failure;
			}
			send(VotingPluginWire.backendStarted(server, incarnationId, startedAt, now));
			send(VotingPluginWire.backendHeartbeat(server, incarnationId, startedAt, nextTimestamp()));
		}
	}

	public void stop() {
		synchronized (lifecycleLock) {
			String activeServer = server;
			UUID activeIncarnationId = incarnationId;
			long activeStartedAt = startedAt;
			boolean wasReporting = reporting;
			reporting = false;
			server = null;
			if (heartbeatTask != null) {
				heartbeatTask.cancel(false);
				heartbeatTask = null;
			}
			if (wasReporting && activeServer != null && activeIncarnationId != null) {
				send(VotingPluginWire.backendStopped(activeServer, activeIncarnationId, activeStartedAt,
						nextTimestamp()));
			}
			incarnationId = null;
			lastResyncRequestId = null;
			lastResyncRequestAtNanos = 0L;
			lastSnapshotRequestId = null;
			lastSnapshotRequestAtNanos = 0L;
			playerSessions.clear();
		}
	}

	public void reload() {
		String configuredServer = plugin.getBungeeSettings().getServer();
		synchronized (lifecycleLock) {
			if (reporting && server != null && server.equalsIgnoreCase(configuredServer)) {
				return;
			}
		}
		stop();
		start();
	}

	public void playerOnline(String playerName, String uuid) {
		if (!method.supportsBackendPresence()) {
			globalMessageHandler.sendMessage(VotingPluginWire.login(playerName, uuid,
					plugin.getBungeeSettings().getServer()));
			return;
		}

		BackendPlayerPresenceSession session = BackendPlayerPresenceSession.create(playerName, uuid);
		if (session == null) {
			plugin.getLogger().warning("Unable to report player login with invalid identity: "
					+ (playerName == null ? "" : playerName));
			return;
		}

		synchronized (lifecycleLock) {
			if (!reporting) {
				return;
			}
			playerSessions.put(BackendPlayerPresenceSession.playerKey(session.getPlayerName()), session);
			reannounceStarted();
			long eventTimestamp = nextTimestamp();
			send(VotingPluginWire.login(session.getPlayerName(), session.getUuid(), server,
					session.getConnectionId(), incarnationId, startedAt, eventTimestamp));
		}
	}

	public void playerOffline(String playerName) {
		synchronized (lifecycleLock) {
			if (!reporting) {
				return;
			}
			BackendPlayerPresenceSession session = playerSessions
					.remove(BackendPlayerPresenceSession.playerKey(playerName));
			if (session == null) {
				return;
			}
			send(VotingPluginWire.logout(session.getPlayerName(), session.getUuid(), server,
					session.getConnectionId(), incarnationId, startedAt, nextTimestamp()));
		}
	}

	public void handleResyncRequest(JsonEnvelope msg) {
		VotingPluginWire.PresenceResyncRequest request = VotingPluginWire.readPresenceResyncRequest(msg);
		synchronized (lifecycleLock) {
			if (!reporting || server == null || incarnationId == null || request.requestId == null
					|| request.requestedAt <= 0L || request.server.isEmpty()
					|| !server.equalsIgnoreCase(request.server)) {
				return;
			}
			long receivedAtNanos = System.nanoTime();
			if (request.requestId.equals(lastResyncRequestId)
					|| (lastResyncRequestId != null
							&& receivedAtNanos - lastResyncRequestAtNanos < RESYNC_REQUEST_MIN_INTERVAL_NANOS)) {
				return;
			}
			lastResyncRequestId = request.requestId;
			lastResyncRequestAtNanos = receivedAtNanos;
			lastSnapshotRequestId = null;
			lastSnapshotRequestAtNanos = 0L;
			send(VotingPluginWire.backendStarted(server, incarnationId, startedAt, nextTimestamp()));
		}
	}

	public void handleSnapshotRequest(JsonEnvelope msg) {
		VotingPluginWire.PresenceSnapshotRequest request = VotingPluginWire.readPresenceSnapshotRequest(msg);
		String activeServer;
		UUID activeIncarnationId;
		long activeStartedAt;
		synchronized (lifecycleLock) {
			activeServer = server;
			activeIncarnationId = incarnationId;
			activeStartedAt = startedAt;
			if (!reporting || activeServer == null || request.requestId == null || request.server.isEmpty()
					|| activeIncarnationId == null || !activeServer.equalsIgnoreCase(request.server)
					|| request.backendStartedAt != activeStartedAt
					|| !activeIncarnationId.equals(request.backendIncarnationId)
					|| request.presenceTimestamp <= 0L) {
				return;
			}
			long receivedAtNanos = System.nanoTime();
			if (request.requestId.equals(lastSnapshotRequestId)
					|| (lastSnapshotRequestId != null
							&& receivedAtNanos - lastSnapshotRequestAtNanos < SNAPSHOT_REQUEST_MIN_INTERVAL_NANOS)) {
				return;
			}
			lastSnapshotRequestId = request.requestId;
			lastSnapshotRequestAtNanos = receivedAtNanos;
		}

		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {
			@Override
			public void run() {
				if (!plugin.isEnabled() || !isActiveGeneration(activeServer, activeIncarnationId, activeStartedAt)) {
					return;
				}

				long snapshotTimestamp;
				List<VotingPluginWire.PresencePlayer> players = new ArrayList<>();
				synchronized (lifecycleLock) {
					if (!isActiveGeneration(activeServer, activeIncarnationId, activeStartedAt)) {
						return;
					}
					for (Player player : Bukkit.getOnlinePlayers()) {
						BackendPlayerPresenceSession session = getOrCreateSession(player);
						if (session != null) {
							players.add(new VotingPluginWire.PresencePlayer(session.getPlayerName(), session.getUuid(),
									session.getConnectionId().toString()));
						}
					}
					snapshotTimestamp = nextTimestamp();
				}

				int chunkCount = Math.max(1, (players.size() + SNAPSHOT_CHUNK_SIZE - 1) / SNAPSHOT_CHUNK_SIZE);
				for (int chunkIndex = 0; chunkIndex < chunkCount; chunkIndex++) {
					int fromIndex = chunkIndex * SNAPSHOT_CHUNK_SIZE;
					int toIndex = Math.min(players.size(), fromIndex + SNAPSHOT_CHUNK_SIZE);
					sendActive(activeServer, activeIncarnationId, activeStartedAt,
							VotingPluginWire.presenceSnapshot(activeServer, request.requestId, chunkIndex, chunkCount,
									players.subList(fromIndex, toIndex), activeIncarnationId, activeStartedAt,
									snapshotTimestamp));
				}
			}
		});
	}

	private BackendPlayerPresenceSession getOrCreateSession(Player player) {
		if (!reporting) {
			return null;
		}
		String key = BackendPlayerPresenceSession.playerKey(player.getName());
		BackendPlayerPresenceSession current = playerSessions.get(key);
		if (current != null) {
			return current;
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		String uuid = user == null ? player.getUniqueId().toString() : user.getUUID();
		BackendPlayerPresenceSession created = BackendPlayerPresenceSession.create(player.getName(), uuid);
		if (created == null) {
			return null;
		}
		BackendPlayerPresenceSession raced = playerSessions.putIfAbsent(key, created);
		return raced == null ? created : raced;
	}

	private void seedOnlinePlayers() {
		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {
			@Override
			public void run() {
				if (!plugin.isEnabled()) {
					return;
				}
				synchronized (lifecycleLock) {
					reannounceStarted();
				}
				for (Player player : Bukkit.getOnlinePlayers()) {
					synchronized (lifecycleLock) {
						BackendPlayerPresenceSession session = getOrCreateSession(player);
						String activeServer = server;
						if (session != null && reporting && activeServer != null) {
							JsonEnvelope login = VotingPluginWire.login(session.getPlayerName(), session.getUuid(),
									activeServer, session.getConnectionId(), incarnationId, startedAt, nextTimestamp());
							sendActive(activeServer, incarnationId, startedAt, login);
						}
					}
				}
			}
		});
	}

	private void sendHeartbeat() {
		synchronized (lifecycleLock) {
			if (reporting && server != null && incarnationId != null) {
				reannounceStarted();
				send(VotingPluginWire.backendHeartbeat(server, incarnationId, startedAt, nextTimestamp()));
			}
		}
	}

	private boolean isActiveGeneration(String expectedServer, UUID expectedIncarnationId, long expectedStartedAt) {
		return reporting && server != null && server.equalsIgnoreCase(expectedServer) && incarnationId != null
				&& incarnationId.equals(expectedIncarnationId) && startedAt == expectedStartedAt;
	}

	private void sendActive(String expectedServer, UUID expectedIncarnationId, long expectedStartedAt,
			JsonEnvelope envelope) {
		synchronized (lifecycleLock) {
			if (isActiveGeneration(expectedServer, expectedIncarnationId, expectedStartedAt)) {
				send(envelope);
			}
		}
	}

	private long nextTimestamp() {
		long now = System.currentTimeMillis();
		lastTimestamp = Math.max(now, lastTimestamp + 1L);
		return lastTimestamp;
	}

	private void reannounceStarted() {
		if (!reporting || server == null || incarnationId == null) {
			return;
		}
		send(VotingPluginWire.backendStarted(server, incarnationId, startedAt, startedAt));
	}

	private void send(JsonEnvelope envelope) {
		try {
			globalMessageHandler.sendMessage(envelope);
		} catch (RuntimeException e) {
			plugin.debug("Unable to send backend presence message " + envelope.getSubChannel());
			plugin.debug(e);
		}
	}

	public boolean isReporting() {
		synchronized (lifecycleLock) {
			return reporting;
		}
	}

	public int getTrackedSessionCount() {
		return playerSessions.size();
	}
}
