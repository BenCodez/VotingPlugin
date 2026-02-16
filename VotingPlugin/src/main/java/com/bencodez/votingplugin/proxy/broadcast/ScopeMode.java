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

	/**
	 * Parses a string value to a ScopeMode with a default fallback.
	 *
	 * @param value the string value to parse
	 * @param def the default value if parsing fails
	 * @return the parsed ScopeMode or the default
	 */
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

	/**
	 * Parses a string value to a ScopeMode with ALL_SERVERS as default.
	 *
	 * @param value the string value to parse
	 * @return the parsed ScopeMode or ALL_SERVERS
	 */
	public static ScopeMode parse(String value) {
		return parse(value, ALL_SERVERS);
	}
}
