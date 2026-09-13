package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.votingplugin.VotingPluginMain;

class SharedMysqlCacheReconcilerTest {
	@Test
	void invalidateAllCopiesTheLiveRegistryBeforeWalkingCaches() {
		UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		UserDataCache cache = mock(UserDataCache.class);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("VoteShopLimitdaily", mock(DataValue.class));
		when(cache.getCache()).thenReturn(values);

		ConcurrentHashMap<UUID, UserDataCache> liveCaches = new ConcurrentHashMap<>() {
			@Override
			public java.util.Collection<UserDataCache> values() {
				throw new AssertionError("reset invalidation must not walk the live values view");
			}
		};
		liveCaches.put(uuid, cache);
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(liveCaches);

		SharedMysqlCacheReconciler.invalidateAll(plugin, "VoteShopLimitdaily");

		assertFalse(values.containsKey("VoteShopLimitdaily"));
	}
}
