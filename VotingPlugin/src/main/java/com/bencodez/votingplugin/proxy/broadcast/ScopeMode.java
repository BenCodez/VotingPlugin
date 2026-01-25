package com.bencodez.votingplugin.proxy.broadcast;

import java.util.Locale;

/**
 * Proxy-side routing scope for broadcast triggers.
 */
public enum ScopeMode {
	/**
	 * Send only to the backend server the player is currently on.
	 */
	PLAYER_SERVER,

	/**
	 * Send to all connected backend servers.
	 */
	ALL_SERVERS,

	/**
	 * Send only to the configured server list.
	 */
	SERVERS,

	/**
	 * Send to all servers except the configured server list.
	 */
	ALL_EXCEPT;

	public static ScopeMode parse(String value, ScopeMode def) {
		if (value == null) {
			return def;
		}
		String v = value.trim();
		if (v.isEmpty()) {
			return def;
		}
		v = v.toUpperCase(Locale.ROOT);

		// allow a couple common aliases
		if (v.equals("ALL")) {
			return ALL_SERVERS;
		}
		if (v.equals("PLAYER")) {
			return PLAYER_SERVER;
		}

		try {
			return ScopeMode.valueOf(v);
		} catch (IllegalArgumentException e) {
			return def;
		}
	}

	public static ScopeMode parse(String value) {
		return parse(value, ALL_SERVERS);
	}
}
