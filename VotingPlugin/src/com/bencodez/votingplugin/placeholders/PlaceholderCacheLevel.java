package com.bencodez.votingplugin.placeholders;

public enum PlaceholderCacheLevel {
	NONE, SPECIFC, AUTO;

	public static PlaceholderCacheLevel getCache(String str) {
		for (PlaceholderCacheLevel v : PlaceholderCacheLevel.values()) {
			if (v.toString().equalsIgnoreCase(str)) {
				return v;
			}
		}
		return AUTO;
	}

	public boolean shouldCache() {
		switch (this) {
		case AUTO:
			return true;
		case NONE:
			return false;
		case SPECIFC:
			return false;
		default:
			return false;
		}
	}

}
