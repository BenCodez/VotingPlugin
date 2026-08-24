package com.bencodez.votingplugin.backendproxy.presence;

import java.util.Locale;
import java.util.UUID;

/**
 * Represents one backend-side player connection tracked for proxy presence.
 */
public final class BackendPlayerPresenceSession {

	private final String playerName;
	private final String uuid;
	private final UUID connectionId;

	private BackendPlayerPresenceSession(String playerName, String uuid, UUID connectionId) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.connectionId = connectionId;
	}

	public static BackendPlayerPresenceSession create(String playerName, String uuid) {
		String name = normalize(playerName);
		String parsedUuid = normalize(uuid);
		if (name.isEmpty() || parsedUuid.isEmpty()) {
			return null;
		}
		try {
			parsedUuid = UUID.fromString(parsedUuid).toString();
		} catch (IllegalArgumentException e) {
			return null;
		}
		return new BackendPlayerPresenceSession(name, parsedUuid, UUID.randomUUID());
	}

	public static String playerKey(String playerName) {
		return normalize(playerName).toLowerCase(Locale.ROOT);
	}

	private static String normalize(String value) {
		return value == null ? "" : value.trim();
	}

	public String getPlayerName() {
		return playerName;
	}

	public String getUuid() {
		return uuid;
	}

	public UUID getConnectionId() {
		return connectionId;
	}
}
