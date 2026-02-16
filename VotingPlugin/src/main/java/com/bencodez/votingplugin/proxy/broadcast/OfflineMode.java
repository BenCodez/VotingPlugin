package com.bencodez.votingplugin.proxy.broadcast;

import java.util.Locale;

/**
 * Enum for offline broadcast modes.
 */
public enum OfflineMode {
	/** No offline handling. */
	NONE,
	/** Queue votes for offline players. */
	QUEUE,
	/** Forward to other servers. */
	FORWARD;

	/**
	 * Parse offline mode from string.
	 * @param value the string to parse
	 * @param def the default value
	 * @return the offline mode
	 */
	public static OfflineMode parse(String value, OfflineMode def) {
		if (value == null)
			return def;
		String v = value.trim();
		if (v.isEmpty())
			return def;
		try {
			return OfflineMode.valueOf(v.toUpperCase(Locale.ROOT));
		} catch (IllegalArgumentException e) {
			return def;
		}
	}
}