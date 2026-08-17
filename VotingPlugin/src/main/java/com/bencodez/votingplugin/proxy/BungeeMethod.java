package com.bencodez.votingplugin.proxy;

/**
 * Enumeration of available Bungee connection methods.
 */
public enum BungeeMethod {
	/** MySQL database connection. */
	MYSQL,
	/** Plugin messaging channel. */
	PLUGINMESSAGING,
	/** Socket connection. */
	SOCKETS,
	/** Redis connection. */
	REDIS,
	/** MQTT message broker. */
	MQTT;
	
	/**
	 * Checks if this method requires a player to be online.
	 *
	 * @return true if player must be online
	 */
	public boolean requiresPlayerOnline() {
		return this == PLUGINMESSAGING;
	}

	/**
	 * Checks whether backend-reported player presence is required for this
	 * transport. Plugin messaging runs on the player-facing proxy, which already
	 * has an authoritative view of online players and their current servers.
	 *
	 * @return true when the transport requires backend presence messages
	 */
	public boolean supportsBackendPresence() {
		return this != PLUGINMESSAGING;
	}

	/**
	 * Gets a BungeeMethod by name.
	 *
	 * @param str the method name
	 * @return the matching BungeeMethod, or PLUGINMESSAGING if not found
	 */
	public static BungeeMethod getByName(String str) {
		for (BungeeMethod method : values()) {
			if (method.toString().equalsIgnoreCase(str)) {
				return method;
			}
		}
		return PLUGINMESSAGING;
	}
}
