package com.bencodez.votingplugin.user;

import java.util.UUID;

import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.votingplugin.VotingPluginMain;

/** Invalidates only shared-MySQL fields changed by an off-thread recovery refund. */
public final class SharedMysqlCacheReconciler {
	private SharedMysqlCacheReconciler() {
	}

	/**
	 * Invalidates an existing cache without creating one or doing JDBC work. The
	 * recovery worker has already committed the refund before this method runs.
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
			if (cache.getCache() == null) return;
			for (String column : columns) {
				if (column != null) cache.getCache().remove(column);
			}
		}
	}
}
