package com.bencodez.votingplugin.proxy.broadcast;

import java.util.Locale;

public enum OfflineMode {
	NONE, QUEUE, FORWARD;

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