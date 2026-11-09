package com.bencodez.votingplugin.tests.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.OptionalInt;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VotingPluginUserPointReadCompatibilityTest {
    private static final UUID USER_ID = UUID.fromString("00000000-0000-0000-0000-000000000001");

    @Test
    void coldSharedMySqlSnapshotReturnsFailClosedZeroAndQueuesOneRecovery() throws Exception {
        Fixture fixture = fixture("Points");

        assertTrue(fixture.user.getPointsIfLoaded().isEmpty());
        assertEquals(0, fixture.user.getPoints());
        assertEquals(0, fixture.user.getPoints());
        verify(fixture.user, never()).getUserData();
        verify(fixture.timer, times(1)).execute(any(Runnable.class));
    }

    @Test
    void publishedBalanceAndGenuineZeroRemainDistinguishableFromLoading() throws Exception {
        Fixture fixture = fixture("Points");
        UserDataCache cache = published(37);
        fixture.caches.put(USER_ID, cache);

        assertEquals(OptionalInt.of(37), fixture.user.getPointsIfLoaded());
        assertEquals(37, fixture.user.getPoints());

        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(0))));
        assertEquals(OptionalInt.of(0), fixture.user.getPointsIfLoaded());
        assertEquals(0, fixture.user.getPoints());
        verify(fixture.timer, never()).execute(any(Runnable.class));
        verify(fixture.user, never()).getUserData();
    }

    @Test
    void completeRowWithoutPointColumnUsesHistoricalZeroDefault() throws Exception {
        Fixture fixture = fixture("Points");
        UserDataCache cache = new UserDataCache(null, USER_ID);
        cache.updateCache(new HashMap<>());
        fixture.caches.put(USER_ID, cache);

        assertEquals(OptionalInt.of(0), fixture.user.getPointsIfLoaded());
        assertEquals(0, fixture.user.getPoints());
    }

    @Test
    void unpublishedSnapshotCannotExposeRetainedOrInvalidatedPointValues() throws Exception {
        Fixture fixture = fixture("Points");
        UserDataCache cache = published(28);
        fixture.caches.put(USER_ID, cache);

        // Retiring even a different column must not make an unverified snapshot readable.
        cache.invalidateStorageSnapshot("OtherColumn");
        assertTrue(fixture.user.getPointsIfLoaded().isEmpty());
        assertEquals(0, fixture.user.getPoints());

        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(19))));
        assertEquals(OptionalInt.of(19), fixture.user.getPointsIfLoaded());

        cache.invalidateStorageSnapshot("Points");
        assertEquals(0, fixture.user.getPoints());
        assertTrue(fixture.user.getPointsIfLoaded().isEmpty());

        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(6))));
        assertEquals(6, fixture.user.getPoints());
        verify(fixture.user, never()).getUserData();
    }

    @Test
    void pendingMutationCannotExposePreviouslyPublishedBalance() throws Exception {
        Fixture fixture = fixture("Points");
        fixture.caches.put(USER_ID, published(55));

        fixture.placeholders.beginSharedPointDebit(USER_ID);
        try {
            assertTrue(fixture.user.getPointsIfLoaded().isEmpty());
            assertEquals(0, fixture.user.getPoints());
            verify(fixture.user, never()).getUserData();
        } finally {
            fixture.placeholders.finishSharedPointDebit(USER_ID, true, true);
        }

        assertEquals(OptionalInt.of(55), fixture.user.getPointsIfLoaded());
    }

    @Test
    void malformedPublishedPointValueIsNotTreatedAsKnownZero() throws Exception {
        Fixture fixture = fixture("Points");
        UserDataCache cache = new UserDataCache(null, USER_ID);
        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueString("invalid"))));
        fixture.caches.put(USER_ID, cache);

        assertTrue(fixture.user.getPointsIfLoaded().isEmpty());
        assertEquals(0, fixture.user.getPoints());
        verify(fixture.timer, never()).execute(any(Runnable.class));
    }

    @Test
    void perServerPointsUseTheirOwnColumn() throws Exception {
        Fixture fixture = fixture("lobby_Points");
        UserDataCache cache = new UserDataCache(null, USER_ID);
        cache.updateCache(new HashMap<>(Map.of(
                "Points", new DataValueInt(99), "lobby_Points", new DataValueInt(12))));
        fixture.caches.put(USER_ID, cache);

        assertEquals(OptionalInt.of(12), fixture.user.getPointsIfLoaded());
        assertEquals(12, fixture.user.getPoints());
    }

    @Test
    void mysqlWorkerRetainsTheExistingAuthoritativeReadPath() throws Exception {
        Fixture fixture = fixture("Points");
        when(fixture.plugin.getUserManager().getDataManager().mustDeferSharedStorageAccess()).thenReturn(false);
        UserData data = mock(UserData.class);
        doReturn(data).when(fixture.user).getUserData();
        when(data.getInt("Points")).thenReturn(63);

        assertEquals(63, fixture.user.getPoints());
        verify(data).getInt("Points");
        verify(fixture.timer, never()).execute(any(Runnable.class));
    }

    @Test
    void nonMysqlStorageKeepsItsExistingReadBehavior() throws Exception {
        Fixture fixture = fixture("Points");
        when(fixture.plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
        UserData data = mock(UserData.class);
        doReturn(data).when(fixture.user).getUserData();
        when(data.getInt("Points")).thenReturn(18);

        assertEquals(18, fixture.user.getPoints());
        assertEquals(OptionalInt.of(18), fixture.user.getPointsIfLoaded());
        verify(data, times(2)).getInt("Points");
        verify(fixture.timer, never()).execute(any(Runnable.class));
    }

    private static UserDataCache published(int amount) {
        UserDataCache cache = new UserDataCache(null, USER_ID);
        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(amount))));
        return cache;
    }

    private static Fixture fixture(String pointColumn) throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        VotingPluginUser user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        Field field = VotingPluginUser.class.getDeclaredField("plugin");
        field.setAccessible(true);
        field.set(user, plugin);
        doReturn(USER_ID.toString()).when(user).getUUID();
        doReturn(USER_ID).when(user).getJavaUUID();
        doReturn(pointColumn).when(user).getPointsPath();

        when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getUserManager().getDataManager().mustDeferSharedStorageAccess()).thenReturn(true);
        when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTOALL);

        ConcurrentHashMap<UUID, UserDataCache> caches = new ConcurrentHashMap<>();
        when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(caches);
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        when(plugin.getTimer()).thenReturn(timer);

        PlaceHolders placeholders = new PlaceHolders(plugin);
        when(plugin.getPlaceholders()).thenReturn(placeholders);
        return new Fixture(plugin, user, placeholders, caches, timer);
    }

    private record Fixture(VotingPluginMain plugin, VotingPluginUser user, PlaceHolders placeholders,
                           ConcurrentHashMap<UUID, UserDataCache> caches, ScheduledExecutorService timer) { }
}
