package com.bencodez.votingplugin.placeholders;

/**
 * Enum for placeholder cache levels.
 */
public enum PlaceholderCacheLevel {
	/** No caching. */
	NONE,
	/** Specific placeholders cached. */
	SPECIFIC,
	/** Auto-cache for online players. */
	AUTO,
	/** Auto-cache for all players. */
	AUTOALL,
	/** Specific cache for all players. */
	SPECIFICALL;

	/**
	 * Get cache level from string.
	 * @param str the string to parse
	 * @return the cache level
	 */
	public static PlaceholderCacheLevel getCache(String str) {
		for (PlaceholderCacheLevel v : PlaceholderCacheLevel.values()) {
			if (v.toString().equalsIgnoreCase(str)) {
				return v;
			}
		}
		return AUTO;
	}

	/**
	 * Check if cache is for online players only.
	 * @return true if online only
	 */
	public boolean onlineOnly() {
		if ((this == AUTOALL) || (this == SPECIFICALL)) {
			return false;
		}
		return true;
	}

	/**
	 * Check if caching should be enabled.
	 * @return true if should cache
	 */
	public boolean shouldCache() {
		switch (this) {
		case AUTO:
			return true;
		case NONE:
			return false;
		case SPECIFIC:
			return false;
		case SPECIFICALL:
			return false;
		case AUTOALL:
			return true;
		default:
			return false;
		}
	}

	/**
	 * Check if cache should always be used.
	 * @return true if cache always
	 */
	public boolean isCacheAlways() {
		if ((this == AUTOALL) || (this == SPECIFICALL)) {
			return true;
		}
		return false;
	}

}
