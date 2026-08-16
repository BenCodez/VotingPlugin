package com.bencodez.votingplugin.proxy.presence;

/**
 * Read-only backend presence status exposed for diagnostics and later routing.
 */
public final class BackendPresenceStatus {
	private final String server;
	private final long lastSeen;
	private final boolean available;
	private final int playerCount;

	public BackendPresenceStatus(String server, long lastSeen, boolean available, int playerCount) {
		this.server = server;
		this.lastSeen = lastSeen;
		this.available = available;
		this.playerCount = playerCount;
	}

	public String getServer() {
		return server;
	}

	public long getLastSeen() {
		return lastSeen;
	}

	public boolean isAvailable() {
		return available;
	}

	public int getPlayerCount() {
		return playerCount;
	}
}
