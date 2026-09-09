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
}
