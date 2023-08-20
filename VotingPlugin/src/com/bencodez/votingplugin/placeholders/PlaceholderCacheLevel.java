package com.bencodez.votingplugin.placeholders;

public enum PlaceholderCacheLevel {
	NONE, SPECIFIC, AUTO, AUTOALL, SPECIFICALL;

	public static PlaceholderCacheLevel getCache(String str) {
		for (PlaceholderCacheLevel v : PlaceholderCacheLevel.values()) {
			if (v.toString().equalsIgnoreCase(str)) {
				return v;
			}
		}
		return AUTO;
	}

	public boolean onlineOnly() {
		if (this == AUTOALL) {
			return false;
		}
		if (this == SPECIFICALL) {
			return false;
		}
		return true;
	}

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

}
