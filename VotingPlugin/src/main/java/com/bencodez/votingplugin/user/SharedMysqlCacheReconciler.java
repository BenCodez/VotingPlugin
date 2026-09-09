package com.bencodez.votingplugin.user;

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

	/** Invalidates changed fields and schedules a nonblocking authoritative refill. */
	public static void invalidateAndRefresh(VotingPluginMain plugin, String uuid, String... columns) {
		invalidate(plugin, uuid, columns);
		if (plugin == null || uuid == null) return;
		try {
			plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid), false).cacheAsync();
		} catch (RuntimeException refreshFailure) {
			plugin.debug(refreshFailure);
		}
	}

	/** Removes a reset column from every currently live cache without flushing it. */
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

	/** Invalidates a shared column and repopulates live user caches asynchronously. */
	public static void invalidateAllAndRefresh(VotingPluginMain plugin, String column) {
		if (plugin == null || column == null) return;
		var caches = plugin.getUserManager().getDataManager().getUserDataCache();
		if (caches == null) return;
		UUID[] users = caches.keySet().toArray(UUID[]::new);
		invalidateAll(plugin, column);
		try {
			plugin.getBukkitScheduler().runTaskAsynchronously(plugin, () -> {
				for (UUID uuid : users) {
					try {
						plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false).cache();
					} catch (RuntimeException refreshFailure) {
						plugin.debug(refreshFailure);
					}
				}
			});
		} catch (RuntimeException schedulingFailure) {
			plugin.debug(schedulingFailure);
		}
	}
}
