package com.bencodez.votingplugin.proxy.presence;

import java.util.UUID;

/**
 * Immutable player location reported by a backend server.
 */
public final class PlayerPresence {
	private final UUID uuid;
	private final String playerName;
	private final String server;
	private final UUID connectionId;
	private final long lastEventSequence;
	private final long lastSeen;

	public PlayerPresence(UUID uuid, String playerName, String server, UUID connectionId, long lastEventSequence,
			long lastSeen) {
		this.uuid = uuid;
		this.playerName = playerName;
		this.server = server;
		this.connectionId = connectionId;
		this.lastEventSequence = lastEventSequence;
		this.lastSeen = lastSeen;
	}

	public UUID getUuid() {
		return uuid;
	}

	public String getPlayerName() {
		return playerName;
	}

	public String getServer() {
		return server;
	}

	public UUID getConnectionId() {
		return connectionId;
	}

	public long getLastEventSequence() {
		return lastEventSequence;
	}

	public long getLastSeen() {
		return lastSeen;
	}
}
