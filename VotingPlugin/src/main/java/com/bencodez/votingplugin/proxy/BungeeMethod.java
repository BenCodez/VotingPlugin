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
