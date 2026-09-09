package com.bencodez.votingplugin.user;

import java.util.Map;
import java.util.UUID;

import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.votingplugin.VotingPluginMain;

/** Invalidates only fields changed directly by a shared-MySQL mutation. */
public final class SharedMysqlCacheReconciler {
	private SharedMysqlCacheReconciler() {
	}

	/**
	 * Invalidates an existing cache without creating one or doing JDBC work. The
	 * mutation has already committed before this method runs.
	 */
	public static void invalidate(VotingPluginMain plugin, String uuid, String... columns) {
		if (plugin == null || uuid == null || columns == null || columns.length == 0) return;
		final UUID playerUuid;
		try {
			playerUuid = UUID.fromString(uuid);
		} catch (IllegalArgumentException invalidUuid) {
			plugin.debug(invalidUuid);
			return;
		}
		UserDataCache cache = plugin.getUserManager().getDataManager().getUserDataCache().get(playerUuid);
		if (cache == null) return;
		synchronized (cache) {
			var values = cache.getCache();
			if (values == null) return;
			for (String column : columns) {
				if (column != null) values.remove(column);
			}
		}
	}

	/**
	 * Persists and detaches every live cache containing {@code column}. This must
	 * run before a database-wide reset so an older queued absolute value cannot
	 * be written after the reset transaction.
	 */
	public static void drainAll(VotingPluginMain plugin, String column) {
		if (plugin == null || column == null) return;
		var caches = plugin.getUserManager().getDataManager().getUserDataCache();
		if (caches == null) return;
		for (Map.Entry<UUID, UserDataCache> entry : Map.copyOf(caches).entrySet()) {
			UserDataCache cache = entry.getValue();
			if (cache == null || !cache.isCached(column) || !caches.remove(entry.getKey(), cache)) continue;
			cache.dump();
		}
	}

	/** Removes a reset column from caches recreated while a shared reset ran. */
	public static void invalidateAll(VotingPluginMain plugin, String column) {
		if (plugin == null || column == null) return;
		var caches = plugin.getUserManager().getDataManager().getUserDataCache();
		if (caches == null) return;
		for (UserDataCache cache : caches.values()) {
			if (cache == null) continue;
			synchronized (cache) {
				var values = cache.getCache();
				if (values != null) values.remove(column);
			}
		}
	}
}
