package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
		UserDataCache cache = new UserDataCache(null, uuid);
		HashMap<String, DataValue> values = new HashMap<>();
		values.put("VoteShopLimitdaily", mock(DataValue.class));
		cache.updateCache(values);

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

		assertFalse(cache.snapshot().containsKey("VoteShopLimitdaily"));
		assertFalse(cache.hasPublishedStorageSnapshot());
	}

	@Test void workerRefillPublishesAuthoritativeValueBeforeMutationCompletionAndOutsideMonitor() {
		UUID uuid = UUID.randomUUID();
		UserDataCache cache = new UserDataCache(null, uuid);
		cache.updateCache(new HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(38))));
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		var manager = plugin.getUserManager().getDataManager();
		when(manager.getUserDataCache()).thenReturn(new ConcurrentHashMap<>(java.util.Map.of(uuid, cache)));
		org.mockito.Mockito.doAnswer(call -> {
			org.junit.jupiter.api.Assertions.assertFalse(Thread.holdsLock(cache));
			assertFalse(cache.hasPublishedStorageSnapshot());
			cache.updateSharedSnapshot(new HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(2))),
					cache.getSharedSnapshotVersion());
			return null;
		}).when(manager).cacheUser(uuid, null);
		SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(plugin, uuid.toString(), "Points");
		org.junit.jupiter.api.Assertions.assertEquals(2, cache.snapshotIfPublished().get("Points").getInt());
	}

	@Test void failedRefillNeverAdvertisesMissingPointsAsPublishedZero() {
		UUID uuid = UUID.randomUUID();
		UserDataCache cache = new UserDataCache(null, uuid);
		cache.updateCache(new HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(38))));
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		var manager = plugin.getUserManager().getDataManager();
		when(manager.getUserDataCache()).thenReturn(new ConcurrentHashMap<>(java.util.Map.of(uuid, cache)));
		org.mockito.Mockito.doThrow(new IllegalStateException("SQL unavailable")).when(manager).cacheUser(uuid, null);
		SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(plugin, uuid.toString(), "Points");
		assertFalse(cache.hasPublishedStorageSnapshot());
		org.junit.jupiter.api.Assertions.assertNull(cache.snapshotIfPublished());
	}
}
