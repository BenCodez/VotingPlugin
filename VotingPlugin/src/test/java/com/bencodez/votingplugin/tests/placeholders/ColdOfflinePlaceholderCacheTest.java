package com.bencodez.votingplugin.tests.placeholders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.placeholder.CalculatingPlaceholder;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

class ColdOfflinePlaceholderCacheTest {

    @org.junit.jupiter.api.io.TempDir java.nio.file.Path transferFallbackData;


    @Test void activeRefillCannotBeReopenedByKnownDebitCompletion() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholders.beginSharedPointDebit(f.uuid);
        var refill = f.placeholders.sharedPointRefillCompletion(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true, true);
        try {
            f.placeholders.onUpdate(f.user, true);
            verify(f.user, never()).getPoints();
            assertEquals("...", f.request("points_nocache"));
        } finally { refill.accept(true); }
    }

    @Test void normalPointRequestRecoversKnownUnavailableSnapshotOnWorker() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        when(f.user.getPoints()).thenReturn(4);
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true, false);
        assertEquals("...", f.request("points"));
        assertEquals(1, f.pending.size(), "ordinary requests must admit one recovery worker");
        assertEquals("...", f.request("points_nocache"));
        f.placeholders.onUpdate(f.user, false);
        assertEquals(1, f.pending.size(), "requests and onUpdate must coalesce while recovery is pending");
        f.drain();
        assertEquals("4", f.request("points_nocache"));
    }

    @Test void publishedAbsentPerServerPointFieldUsesZeroWithoutStorageRecovery() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true);
        when(f.plugin.getBungeeSettings().getServerNameStorage()).thenReturn("serverA");
        when(f.user.getPointsPath()).thenReturn("serverA_Points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Other", new com.bencodez.simpleapi.sql.data.DataValueInt(7))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        assertEquals("0", f.request("points_nocache"));
        assertEquals("0", f.request("points_format_nocache"));
        assertEquals("false", f.request("enoughpoints_16_nocache"));
        assertTrue(f.pending.isEmpty());
        verify(f.user, never()).getPoints();
        verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedMalformedPointsAUTO() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.AUTO, new com.bencodez.simpleapi.sql.data.DataValueString("invalid"));
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedUnsupportedPointsAUTO() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.AUTO, new com.bencodez.simpleapi.sql.data.DataValueBoolean(true));
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedNullPointsAUTO() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.AUTO, null);
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedMalformedPointsSPECIFIC() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.SPECIFIC, new com.bencodez.simpleapi.sql.data.DataValueString("invalid"));
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedUnsupportedPointsSPECIFIC() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.SPECIFIC, new com.bencodez.simpleapi.sql.data.DataValueBoolean(true));
    }

    @Test void commonOfflineMonthlyWorkerDoesNotReloadPublishedNullPointsSPECIFIC() {
        assertCommonOfflineInvalidPoints(PlaceholderCacheLevel.SPECIFIC, null);
    }

    private void assertCommonOfflineInvalidPoints(PlaceholderCacheLevel level, com.bencodez.simpleapi.sql.data.DataValue invalid) {
        Fixture f = new Fixture(level, "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var row = new java.util.HashMap<String,com.bencodez.simpleapi.sql.data.DataValue>();
        row.put("Points", invalid);
        cache.updateCache(row);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> { cache.updateCache(new java.util.HashMap<>(row)); return null; })
                .when(manager).cacheUser(f.uuid, null);
        for (int n = 0; n < 3; n++) {
            assertEquals("...", f.request("total_monthly"));
            assertEquals(1, f.pending.size());
            f.pending.removeFirst().run();
            assertTrue(f.pending.isEmpty());
            assertFalse(f.placeholder("total_monthly").getCache().get("total_monthly").containsKey(f.uuid));
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty());
        }
        verify(manager, never()).cacheUser(any(), any());
        verify(f.user, never()).getTotal(TopVoter.Monthly);
        verify(f.user, never()).getPoints();
    }

    @Test void commonThenDirectColdRequestsShareQueuedAndExecutingRecovery() {
        assertCommonAndDirectColdRecovery(false);
    }

    @Test void directThenCommonColdRequestsShareQueuedAndExecutingRecovery() {
        assertCommonAndDirectColdRecovery(true);
    }

    private void assertCommonAndDirectColdRecovery(boolean directFirst) {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points", "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            // Reentrant public requests model admission while the worker owns its token/refill.
            assertEquals("...", f.request("total_monthly"));
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "executing common/direct recovery must not queue nested work");
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        assertEquals("...", f.request(directFirst ? "points_nocache" : "total_monthly"));
        assertEquals("...", f.request(directFirst ? "total_monthly" : "points_nocache"));
        assertEquals("...", f.request("total_monthly"));
        assertEquals(1, f.pending.size(), "queued common/direct requests must share admission");
        f.pending.removeFirst().run();
        assertTrue(f.pending.isEmpty());
        assertEquals("4", f.request("points_nocache"));
        assertEquals("42", f.request("total_monthly"), "ordinary nonpoint warming must still complete");
        verify(manager, times(1)).cacheUser(f.uuid, null);
        verify(f.user, never()).getPoints();
    }

    @Test void commonWorkerRepairsKnownTransientSnapshotWithoutNestedTask() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true, false);
        assertEquals("...", f.request("total_monthly"));
        f.pending.removeFirst().run();
        assertTrue(f.pending.isEmpty());
        assertEquals("4", f.request("points_nocache"));
        assertEquals("42", f.request("total_monthly"));
        verify(manager, times(1)).cacheUser(f.uuid, null);
    }

    @Test void commonWorkerWarmsNonPointsWithoutRefillingPendingOrUnknownPoints() {
        for (boolean unknown : List.of(false, true)) {
            Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
            when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
            var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            var manager = f.plugin.getUserManager().getDataManager();
            when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
            f.placeholders.beginSharedPointDebit(f.uuid);
            if (unknown) f.placeholders.finishSharedPointDebit(f.uuid, false, false);
            try {
                assertEquals("...", f.request("total_monthly"));
                f.pending.removeFirst().run();
                assertEquals("42", f.request("total_monthly"));
                assertEquals("...", f.request("points_nocache"));
                assertTrue(f.pending.isEmpty());
                verify(manager, never()).cacheUser(any(), any());
            } finally {
                if (!unknown) f.placeholders.finishSharedPointDebit(f.uuid, true, false);
            }
        }
    }

    @Test void staleCommonWorkersDoNotReadStorageOrWarmAfterDisableOrReplacement() {
        for (boolean disabled : List.of(false, true)) {
            Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
            when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
            assertEquals("...", f.request("total_monthly"));
            if (disabled) when(f.plugin.isEnabled()).thenReturn(false);
            else when(f.plugin.getPlaceholders()).thenReturn(mock(PlaceHolders.class));
            f.pending.removeFirst().run();
            assertTrue(f.pending.isEmpty());
            verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
            verify(f.user, never()).getTotal(TopVoter.Monthly);
        }
    }

    @Test void staleReloadedCommonWorkerCannotRemoveNewRecoveryAdmission() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        assertEquals("...", f.request("total_monthly"));
        Runnable retired = f.pending.removeFirst();
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            f.placeholders.reload();
        }
        assertEquals("...", f.request("total_monthly"));
        assertEquals(1, f.pending.size());
        retired.run();
        verify(manager, never()).cacheUser(any(), any());
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size(), "stale token retirement must preserve new worker admission");
        f.pending.removeFirst().run();
        assertEquals("4", f.request("points_nocache"));
        assertEquals("42", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
        verify(manager, times(1)).cacheUser(f.uuid, null);
    }

    @Test void rejectedCommonWorkerAdmissionAllowsLaterDirectRecovery() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        doThrow(new java.util.concurrent.RejectedExecutionException("closed"))
                .doAnswer(call -> { f.pending.addLast(call.getArgument(0)); return null; })
                .when(f.timer).execute(any(Runnable.class));
        assertEquals("...", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run();
        assertEquals("4", f.request("points_nocache"));
        assertEquals("42", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
        verify(manager, times(1)).cacheUser(f.uuid, null);
    }

    @Test void lateAutomaticNonPointAdmissionIsNotLostWhileCommonWorkerWarms() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
        when(f.user.getLastMonthTotal()).thenReturn(7);
        doAnswer(call -> {
            assertEquals("..", f.request("lastmonthtotal"));
            return 42;
        }).doReturn(42).when(f.user).getTotal(TopVoter.Monthly);
        assertEquals("...", f.request("total_monthly"));
        f.pending.removeFirst().run();
        assertEquals(1, f.pending.size(), "late new identifier needs a retained warming continuation");
        f.drain();
        assertEquals("7", f.request("lastmonthtotal"));
        assertEquals("42", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
    }

    @Test void disableDuringCommonStorageRecoveryPreventsNonPointWarming() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "total_monthly");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            when(f.plugin.isEnabled()).thenReturn(false);
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        assertEquals("...", f.request("total_monthly"));
        f.pending.removeFirst().run();
        assertTrue(f.pending.isEmpty());
        verify(manager, times(1)).cacheUser(f.uuid, null);
        verify(f.user, never()).getTotal(TopVoter.Monthly);
    }

    @Test void completedWorkerPublishedMalformedPointsDoNotLoopRecovery() throws Exception {
        assertCompletedInvalidSnapshot(new com.bencodez.simpleapi.sql.data.DataValueString("invalid"));
    }
    @Test void completedWorkerPublishedUnsupportedPointsDoNotLoopRecovery() throws Exception {
        assertCompletedInvalidSnapshot(new com.bencodez.simpleapi.sql.data.DataValueBoolean(true));
    }
    @Test void completedWorkerPublishedNullPointsDoNotBecomeZeroOrLoopRecovery() throws Exception {
        assertCompletedInvalidSnapshot(null);
    }

    private void assertCompletedInvalidSnapshot(com.bencodez.simpleapi.sql.data.DataValue invalid) {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            var values = new java.util.HashMap<String,com.bencodez.simpleapi.sql.data.DataValue>();
            values.put("Points", invalid); cache.updateCache(values); return null;
        }).when(manager).cacheUser(f.uuid, null);
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run();
        assertTrue(f.pending.isEmpty());
        for (int n = 0; n < 5; n++) {
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "a completed invalid published row must not be reloaded per request");
        }
        verify(manager, times(1)).cacheUser(f.uuid, null);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
        assertEquals("4", f.request("points_nocache"));
        assertTrue(f.pending.isEmpty(), "new valid published data needs no extra storage query");
        verify(f.user, never()).getPoints();
    }

    @Test void publicKnownUnstartedSetAcceptsCompletePublishedAbsentPointField() throws Exception {
        Fixture f = mysqlPublicFixture();
        when(f.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true);
        when(f.plugin.getBungeeSettings().getServerNameStorage()).thenReturn("serverA");
        when(f.user.getPointsPath()).thenReturn("serverA_Points");
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(null);
        var manager = f.plugin.getUserManager().getDataManager();
        doAnswer(call -> {
            var complete = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            complete.updateCache(new java.util.HashMap<>());
            manager.getUserDataCache().put(f.uuid, complete); return null;
        }).when(manager).cacheUser(f.uuid, null);
        org.mockito.Mockito.doCallRealMethod().when(f.user).setPointsStorageAware(eq(4), any());
        var results = new java.util.ArrayList<Boolean>();
        f.user.setPointsStorageAware(4, results::add);
        f.pending.removeFirst().run();
        assertEquals(List.of(false), results);
        assertEquals("0", f.request("points_nocache"));
        assertTrue(f.pending.isEmpty(), "published absence after known completion must not start another recovery");
        verify(manager, times(1)).cacheUser(f.uuid, null);
        verify(f.user, never()).getPoints();
    }

    @Test void publishedInvalidThenValidSnapshotUpdatesCachedPointRendering() throws Exception {
        Fixture f = mysqlPublicFixture();
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueString("invalid"))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        assertEquals("...", f.request("points"));
        assertTrue(f.pending.isEmpty());
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
        f.placeholders.onUpdate(f.user, true);
        assertEquals("4", f.request("points"));
        assertTrue(f.pending.isEmpty());
        verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
        verify(f.user, never()).getPoints();
    }

    @Test void unpublishedAbsenceAndGenuineUncertaintyNeverDefaultToZero() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size());
        f.placeholders.beginSharedPointDebit(f.uuid);
        cache.updateCache(new java.util.HashMap<>());
        assertEquals("...", f.request("points_nocache"), "an active mutation cannot publish default zero");
        f.placeholders.finishSharedPointDebit(f.uuid, false, true);
        f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
        assertEquals("...", f.request("points_nocache"), "published absence cannot clear transaction uncertainty");
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of()); f.placeholders.reload();
        }
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size(), "unknown state must not add recovery work");
        verify(f.user, never()).getPoints();
    }

    @Test void perServerNumericStringSnapshotPreservesPointValue() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true);
        when(f.plugin.getBungeeSettings().getServerNameStorage()).thenReturn("serverA");
        when(f.user.getPointsPath()).thenReturn("serverA_Points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("serverA_Points", new com.bencodez.simpleapi.sql.data.DataValueString("17"))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        assertEquals("17", f.request("points_nocache"));
        assertEquals("17", f.request("points_format_nocache"));
        assertEquals("true", f.request("enoughpoints_16_nocache"));
    }

    @Test void reorderedRefillsCannotReopenBeforeEveryReadRetires() {
        for (boolean reverse : List.of(false, true)) {
            Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
            f.placeholders.beginSharedPointDebit(f.uuid);
            var first = f.placeholders.sharedPointRefillCompletion(f.uuid);
            var second = f.placeholders.sharedPointRefillCompletion(f.uuid);
            f.placeholders.finishSharedPointDebit(f.uuid, true, true);
            (reverse ? second : first).accept(true);
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "an active storage read must not admit recovery");
            (reverse ? first : second).accept(true);
            assertEquals("...", f.request("points_nocache"));
            assertEquals(1, f.pending.size(), "an obsolete completion requires a fresh coalesced read");
        }
    }

    @Test void rejectedRecoveryAdmissionCanBeRetriedWithoutRetryingDebit() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true, false);
        doThrow(new java.util.concurrent.RejectedExecutionException("stopped"))
                .doAnswer(call -> { f.pending.add(call.getArgument(0)); return null; })
                .when(f.timer).execute(any(Runnable.class));
        assertEquals("...", f.request("points"));
        assertTrue(f.pending.isEmpty());
        assertEquals("...", f.request("points"));
        assertEquals(1, f.pending.size());
        verify(f.user, never()).removePoints(org.mockito.ArgumentMatchers.anyInt(), any(java.util.function.Consumer.class));
    }

    @Test void recoveryQueuedBeforeReloadCannotReadStorageInNewLifecycle() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true, false);
        assertEquals("...", f.request("points"));
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            f.placeholders.reload();
        }
        f.pending.removeFirst().run();
        verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
        assertEquals("...", f.request("points"));
        assertEquals(1, f.pending.size());
    }

    @Test void malformedNumericStringRemainsUnavailableWithoutOwnerStorageRead() {
        for (String text : List.of("invalid", "", "2147483648")) {
            Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
            when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
            var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueString(text))));
            when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                    .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "published invalid data must not schedule storage recovery");
            verify(f.user, never()).getPoints();
            verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
        }
    }

    @Test void publicCallbackAdditionAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("callback"); }
    @Test void publicStageAdditionAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("stage"); }
    @Test void publicLegacyAdditionAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("add"); }
    @Test void publicLegacyDebitAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("remove"); }
    @Test void publicVoteCapAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("cap"); }
    @Test void publicVoteAddAndCapAmbiguityCannotPublishAnUnrelatedRefill() throws Exception { assertPublicArithmeticAmbiguity("addcap"); }

    @Test void confirmedPublicArithmeticPublishesTheReconciledSnapshot() throws Exception { assertPublicArithmeticPhase("confirmed"); }
    @Test void rejectedPublicArithmeticPublishesTheReconciledSnapshot() throws Exception { assertPublicArithmeticPhase("rejected"); }
    @Test void preparationFailureRemainsRecoverableAcrossPublicArithmetic() throws Exception { assertPublicArithmeticPhase("prepare"); }
    @Test void runtimeExecutionFailureRemainsUnavailableAcrossPublicArithmetic() throws Exception { assertPublicArithmeticPhase("runtime"); }

    @Test void rejectedPublicArithmeticAdmissionRecoversWithoutSqlOrRetry() throws Exception { assertPublicArithmeticPhase("admission"); }
    @Test void stoppedPublicArithmeticCannotReopenNumericPublication() throws Exception { assertPublicArithmeticPhase("shutdown"); }

    @Test void callbackCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("callback", "drain"); }
    @Test void stageCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("stage", "drain"); }
    @Test void addCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("add", "drain"); }
    @Test void removeCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("remove", "drain"); }
    @Test void capCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("cap", "drain"); }
    @Test void addcapCacheAdmissionFailureCanRecoverWithoutPointExecution() throws Exception { assertPublicArithmeticOutcome("addcap", "drain"); }
    @Test void priorUncertainPointWriteSurvivesCachePreparationFailure() throws Exception { assertPublicArithmeticPhase("drain-uncertain"); }

    private void assertPublicArithmeticPhase(String phase) throws Exception {
        for (String operation : List.of("callback", "stage", "add", "remove", "cap", "addcap")) {
            assertPublicArithmeticOutcome(operation, phase);
        }
    }

    private void assertPublicArithmeticAmbiguity(String operation) throws Exception {
        assertPublicArithmeticOutcome(operation, "ambiguous");
    }

    private void assertPublicArithmeticOutcome(String operation, String phase) throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        org.mockito.Mockito.doReturn(f.player).when(f.user).getPlayer();
        var field = VotingPluginUser.class.getDeclaredField("plugin");
        field.setAccessible(true);
        field.set(f.user, f.plugin);
        org.mockito.Mockito.doCallRealMethod().when(f.user).addPointsStorageAware(
                org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.<java.util.function.BiConsumer<Boolean, Integer>>any());
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        when(f.user.getCache()).thenReturn(cache);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(36))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        var connection = mock(java.sql.Connection.class);
        var statement = mock(java.sql.PreparedStatement.class);
        when(f.plugin.getMysql().getTableName()).thenReturn("SyntheticPoints");
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(connection);
        when(connection.prepareStatement(anyString())).thenReturn(statement);
        if (phase.equals("prepare")) {
            when(connection.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("synthetic prepare rejected"));
        } else if (phase.equals("ambiguous")) {
            when(statement.executeUpdate()).thenThrow(new java.sql.SQLException("synthetic lost update acknowledgement"));
        } else if (phase.equals("runtime")) {
            when(statement.executeUpdate()).thenThrow(new IllegalStateException("synthetic driver failure"));
        } else {
            when(statement.executeUpdate()).thenReturn(phase.equals("confirmed") || phase.equals("shutdown") ? 1 : 0);
            var row = mock(java.sql.ResultSet.class);
            when(row.next()).thenReturn(true);
            when(row.getInt(1)).thenReturn(36);
            when(statement.executeQuery()).thenReturn(row);
        }
        when(f.scheduler.getFoliaLib()).thenReturn(null);
        doAnswer(call -> { ((Runnable) call.getArgument(1)).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class), eq(f.player));
        if (phase.equals("admission")) {
            doThrow(new java.util.concurrent.RejectedExecutionException("synthetic full worker"))
                    .when(f.timer).execute(any(Runnable.class));
        }
        var cacheWrites = new java.util.concurrent.atomic.AtomicInteger();
        boolean drainFailure = phase.startsWith("drain");
        if (drainFailure) {
            // AC138's real exclusive cache gate rejects before its writer runs.
            cache.configureSharedStorage(values -> cacheWrites.incrementAndGet(),
                    task -> { throw new IllegalStateException("synthetic cache admission retired"); });
            when(f.user.isCached()).thenReturn(true);
            doAnswer(call -> { cache.clearCache(); return null; }).when(manager).removeCache(f.uuid, null);
            if (phase.equals("drain-uncertain")) {
                f.placeholders.beginSharedPointDebit(f.uuid);
                f.placeholders.finishSharedPointDebit(f.uuid, false, false);
            }
        }
        var callbacks = new ArrayList<Boolean>();
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getPluginManager).thenReturn(mock(org.bukkit.plugin.PluginManager.class));
            switch (operation) {
                case "callback" -> f.user.addPointsStorageAware(16, (java.util.function.BiConsumer<Boolean, Integer>) (success, total) -> callbacks.add(success));
                case "stage" -> {
                    org.mockito.Mockito.doCallRealMethod().when(f.user).addPointsStorageAwareAsync(org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.isNull());
                    f.user.addPointsStorageAwareAsync(16, null).whenComplete((total, failure) -> callbacks.add(failure == null));
                }
                case "add" -> {
                    org.mockito.Mockito.doCallRealMethod().when(f.user).addPoints(org.mockito.ArgumentMatchers.anyInt(), eq(true));
                    f.user.addPoints(16, true);
                }
                case "remove" -> {
                    org.mockito.Mockito.doCallRealMethod().when(f.user).removePoints(org.mockito.ArgumentMatchers.anyInt(), eq(true));
                    f.user.removePoints(16, true);
                }
                case "cap", "addcap" -> {
                    when(f.plugin.getConfigFile().getPointsOnVote()).thenReturn(operation.equals("cap") ? 0 : 16);
                    when(f.plugin.getConfigFile().getLimitVotePoints()).thenReturn(40);
                    org.mockito.Mockito.doCallRealMethod().when(f.user).addPoints();
                    f.user.addPoints();
                }
                default -> throw new AssertionError(operation);
            }
        }
        verify(f.plugin.getMysql().getMysql().getConnectionManager(), never()).getConnection();
        verify(statement, never()).executeUpdate();
        if (phase.equals("admission")) {
            assertTrue(f.pending.isEmpty());
            doAnswer(call -> { f.pending.addLast(call.getArgument(0)); return null; })
                    .when(f.timer).execute(any(Runnable.class));
            assertEquals("...", f.request("points_nocache"));
            assertEquals(1, f.pending.size(), "rejected admission permits one ordinary source-recovery worker");
        }
        if (phase.equals("shutdown")) when(f.plugin.isEnabled()).thenReturn(false);
        boolean uncertain = phase.equals("ambiguous") || phase.equals("runtime") || phase.equals("drain-uncertain");
        Runnable worker = f.pending.removeFirst();
        if ((phase.equals("runtime") || drainFailure) && !operation.equals("callback") && !operation.equals("stage")) {
            org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class, worker::run);
        } else worker.run();
        assertEquals(operation.equals("callback") || operation.equals("stage") ? List.of(phase.equals("confirmed") || phase.equals("shutdown")) : List.of(), callbacks);
        verify(statement, times(phase.equals("prepare") || phase.equals("admission") || drainFailure ? 0 : 1)).executeUpdate();
        if (drainFailure) {
            assertEquals(0, cacheWrites.get(), "actual AC138 cache writer was never admitted");
            verify(f.plugin.getMysql().getMysql().getConnectionManager(), never()).getConnection();
            assertEquals("...", f.request("points_nocache"));
            if (!uncertain) {
                assertEquals(1, f.pending.size(), "known pre-point failure must allow ordinary recovery");
                f.pending.removeFirst().run();
            }
        }
        if (operation.equals("stage") && !uncertain && !phase.equals("confirmed") && !phase.equals("admission") && !phase.equals("shutdown") && !drainFailure) {
            assertEquals("...", f.request("points_nocache"), operation + "/" + phase);
            assertEquals(1, f.pending.size(), "known failed stage recovers through the ordinary placeholder worker");
            f.pending.removeFirst().run();
        }
        assertEquals(uncertain || phase.equals("shutdown") ? "..." : "36", f.request("points_nocache"), operation + "/" + phase);
        f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
        assertEquals(uncertain || phase.equals("shutdown") ? "..." : "36", f.request("points_nocache"));
        assertTrue(f.pending.isEmpty(), "no retry of an ambiguous addition");
    }

    @Test void publicReplayAdditionClosesRenderingBeforeWorkerAdmission() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        org.mockito.Mockito.doReturn(f.player).when(f.user).getPlayer();
        var field = VotingPluginUser.class.getDeclaredField("plugin");
        field.setAccessible(true);
        field.set(f.user, f.plugin);
        org.mockito.Mockito.doCallRealMethod().when(f.user).addPointsStorageAwareAsync(org.mockito.ArgumentMatchers.anyInt(), anyString());
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        var completion = f.user.addPointsStorageAwareAsync(16, "synthetic-public-entry-" + UUID.randomUUID());
        try { assertEquals("...", f.request("points")); }
        finally { completion.toCompletableFuture().cancel(false); }
    }

    @Test void publicSharedAndPerServerReplayFailuresRemainUnavailableAfterRefill() throws Exception {
        for (boolean perServer : new boolean[]{false, true}) {
            Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
            when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
            when(f.plugin.getBungeeSettings().isPerServerPoints()).thenReturn(perServer);
            when(f.plugin.getBungeeSettings().getServerNameStorage()).thenReturn("Synthetic");
            String column = perServer ? "Synthetic_Points" : "Points";
            when(f.user.getPointsPath()).thenReturn(column);
            org.mockito.Mockito.doReturn(f.player).when(f.user).getPlayer();
            var field = VotingPluginUser.class.getDeclaredField("plugin");
            field.setAccessible(true);
            field.set(f.user, f.plugin);
            org.mockito.Mockito.doCallRealMethod().when(f.user).addPointsStorageAwareAsync(org.mockito.ArgumentMatchers.anyInt(), anyString());
            when(f.plugin.getMysql().getTableName()).thenReturn("SyntheticPoints");
            when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection())
                    .thenThrow(new IllegalStateException("synthetic unavailable acknowledgement"));
            var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of(column, new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
            when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                    .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
            var completion = f.user.addPointsStorageAwareAsync(16, "synthetic-replay-failure-" + UUID.randomUUID()).toCompletableFuture();
            assertEquals("...", f.request("points"));
            f.pending.removeFirst().run();
            assertTrue(completion.isCompletedExceptionally());
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of(column, new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "uncertain completion must not schedule an automatic retry/refill");
            verify(f.user, never()).getPoints();
        }
    }

    @Test void completedTransferRefreshesSourceAndTargetRender() throws Exception { assertTransferRender("COMPLETED"); }
    @Test void refundedTransferRefreshesSourceAndTargetRender() throws Exception { assertTransferRender("REFUNDED"); }
    @Test void indeterminateTransferDoesNotPublishRefillAsConfirmation() throws Exception { assertTransferRender("INDETERMINATE"); }

    private void assertTransferRender(String outcome) throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        UUID targetUuid = UUID.fromString("00000000-0000-0000-0000-000000000002");
        var target = mock(VotingPluginUser.class);
        when(target.getUUID()).thenReturn(targetUuid.toString());
        when(target.getJavaUUID()).thenReturn(targetUuid);
        when(target.getPointsPath()).thenReturn("Points");
        var offlineTarget = mock(OfflinePlayer.class);
        when(f.plugin.getVotingPluginUserManager().getVotingPluginUser(offlineTarget)).thenReturn(target);
        var caches = new java.util.concurrent.ConcurrentHashMap<UUID, com.bencodez.advancedcore.api.user.usercache.UserDataCache>();
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(caches);
        for (UUID uuid : List.of(f.uuid, targetUuid)) {
            var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, uuid);
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(uuid.equals(f.uuid) ? 20 : 0))));
            caches.put(uuid, cache);
        }
        doAnswer(call -> {
            UUID uuid = call.getArgument(0);
            int balance = uuid.equals(f.uuid) ? (outcome.equals("REFUNDED") ? 20 : 4) : (outcome.equals("REFUNDED") ? 0 : 16);
            caches.get(uuid).updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(balance))));
            return null;
        }).when(manager).cacheUser(any(UUID.class), org.mockito.ArgumentMatchers.isNull());
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        f.placeholder("points").getCache().get("points").put(targetUuid, "0");
        var journal = com.bencodez.votingplugin.tests.user.SharedPointTransferJournalAccess.mock();
        journal.stub(org.mockito.Mockito.doReturn(com.bencodez.votingplugin.tests.support.ReflectiveAccess.field(
                "com.bencodez.votingplugin.user.SharedPointTransferJournal$SettlementOutcome", outcome)))
                .settleWithConfirmation("synthetic-transfer", "owner", f.uuid.toString(), "Points", targetUuid.toString(), "Points", 16, 16);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        var completions = new java.util.ArrayList<com.bencodez.votingplugin.user.PointTransferResult>();
        java.util.function.Consumer<com.bencodez.votingplugin.user.PointTransferResult> completion = completions::add;
        com.bencodez.votingplugin.tests.support.ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator",
                new com.bencodez.votingplugin.tests.user.SharedMysqlPointMutatorAccess(f.plugin), "settleTransfer",
                new String[]{"VotingPluginUser", "VotingPluginUser", "Player", "int", "Consumer",
                        "SharedPointTransferJournal", "String", "String", "String", "String", "Integer"},
                new Object[]{f.user, target, null, 16, completion, journal, "synthetic-transfer", "owner", "Points", "Points", 16});
        assertEquals(1, completions.size());
        journal.verify(org.mockito.Mockito.times(1)).settleWithConfirmation("synthetic-transfer", "owner", f.uuid.toString(), "Points", targetUuid.toString(), "Points", 16, 16);
        if (outcome.equals("INDETERMINATE")) {
            assertEquals(com.bencodez.votingplugin.user.PointTransferResult.PENDING_CONFIRMATION, completions.getFirst());
            assertEquals("...", f.request("points_nocache"));
        } else {
            assertEquals(outcome.equals("COMPLETED") ? com.bencodez.votingplugin.user.PointTransferResult.SUCCESS
                    : com.bencodez.votingplugin.user.PointTransferResult.CANCELLED, completions.getFirst());
            assertEquals("...", f.request("points"));
            assertEquals(outcome.equals("REFUNDED") ? "20" : "4", f.request("points_nocache"));
            assertEquals(outcome.equals("REFUNDED") ? "0" : "16", f.placeholders.getPlaceholderValue(offlineTarget, "points", false, true, false));
        }
    }

    @Test void confirmedRefundRetiresStaleRenderedBalance() throws Exception { assertRefundRender(false); }
    @Test void ambiguousRefundCannotPublishAsConfirmedBalance() throws Exception { assertRefundRender(true); }

    private void assertRefundRender(boolean ambiguous) throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        var journal = com.bencodez.votingplugin.tests.user.SharedPointTransferJournalAccess.mock();
        if (ambiguous) when(journal.refundHookStarted("synthetic-refund", f.uuid.toString(), "Points", 16))
                .thenThrow(new java.sql.SQLException("synthetic acknowledgement lost"));
        else when(journal.refundHookStarted("synthetic-refund", f.uuid.toString(), "Points", 16)).thenReturn(true);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        var completions = new java.util.ArrayList<com.bencodez.votingplugin.user.PointTransferResult>();
        new com.bencodez.votingplugin.tests.user.SharedMysqlPointMutatorAccess(f.plugin).refundClaimedAfterSchedulingFailure(
                f.user, null, completions::add, journal, "synthetic-refund", "Points", 16, new java.util.concurrent.RejectedExecutionException("synthetic retired owner"));
        assertEquals(List.of(com.bencodez.votingplugin.user.PointTransferResult.UNAVAILABLE), completions);
        journal.verify(org.mockito.Mockito.times(1)).refundHookStarted("synthetic-refund", f.uuid.toString(), "Points", 16);
        if (ambiguous) assertEquals("...", f.request("points_nocache"));
        else {
            assertEquals("...", f.request("points"));
            assertEquals("4", f.request("points_nocache"));
        }
    }

    @Test void publicSetAdmissionRetiresRenderedValueBeforeWorkerExecution() throws Exception {
        Fixture f = mysqlPublicFixture();
        org.mockito.Mockito.doCallRealMethod().when(f.user).setPointsStorageAware(eq(4), any());
        f.user.setPointsStorageAware(4, ignored -> { });
        assertEquals(1, f.pending.size());
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size(), "pending mutation must not admit a competing recovery");
    }

    @Test void publicPurchaseAdmissionRetiresRenderedValueBeforeWorkerExecution() throws Exception {
        Fixture f = mysqlPublicFixture();
        startPublicPurchase(f, ignored -> { });
        assertEquals(1, f.pending.size());
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size(), "pending reservation must not admit a competing recovery");
    }

    @Test void publicSetNullConnectionIsRecoverableWithoutExecutingUpdate() throws Exception { assertPublicSetPhase("acquire"); }
    @Test void publicSetPreparationFailureIsRecoverableWithoutExecutingUpdate() throws Exception { assertPublicSetPhase("prepare"); }
    @Test void publicSetBindFailureIsRecoverableWithoutExecutingUpdate() throws Exception { assertPublicSetPhase("bind"); }
    @Test void publicSetExecutionFailureStaysUncertain() throws Exception { assertPublicSetPhase("execute"); }
    @Test void publicSetConfirmedUpdatePublishesFreshSnapshot() throws Exception { assertPublicSetPhase("confirmed"); }

    private void assertPublicSetPhase(String phase) throws Exception {
        Fixture f = mysqlPublicFixture();
        var connection = mock(java.sql.Connection.class);
        var statement = mock(java.sql.PreparedStatement.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(phase.equals("acquire") ? null : connection);
        if (phase.equals("prepare")) when(connection.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("synthetic prepare failure"));
        else when(connection.prepareStatement(anyString())).thenReturn(statement);
        if (phase.equals("bind")) doThrow(new java.sql.SQLException("synthetic bind failure")).when(statement).setInt(1, 4);
        if (phase.equals("execute")) when(statement.executeUpdate()).thenThrow(new java.sql.SQLException("synthetic acknowledgement failure"));
        else when(statement.executeUpdate()).thenReturn(1);
        org.mockito.Mockito.doCallRealMethod().when(f.user).setPointsStorageAware(eq(4), any());
        var results = new java.util.ArrayList<Boolean>();
        f.user.setPointsStorageAware(4, results::add);
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run();
        assertEquals(List.of(phase.equals("confirmed")), results);
        boolean attempted = phase.equals("execute") || phase.equals("confirmed");
        verify(statement, times(attempted ? 1 : 0)).executeUpdate();
        assertEquals("...", f.request("points"));
        if (!phase.equals("execute")) {
            f.drain();
            assertEquals("4", f.request("points_nocache"), "known unstarted writes must recover through ordinary requests");
        } else {
            assertTrue(f.pending.isEmpty(), "ambiguous writes must not be retried/reopened");
            assertEquals("...", f.request("points_nocache"));
        }
        verify(f.user, never()).getPoints();
    }

    private Fixture mysqlPublicFixture() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var field = VotingPluginUser.class.getDeclaredField("plugin"); field.setAccessible(true); field.set(f.user, f.plugin);
        org.mockito.Mockito.doReturn(f.player).when(f.user).getPlayer();
        when(f.scheduler.getFoliaLib()).thenReturn(null);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class), eq(f.player));
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var caches = new java.util.concurrent.ConcurrentHashMap<UUID, com.bencodez.advancedcore.api.user.usercache.UserDataCache>();
        caches.put(f.uuid, cache);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(caches);
        doAnswer(call -> {
            var fresh = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            fresh.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            caches.put(f.uuid, fresh); return null;
        }).when(manager).cacheUser(f.uuid, null);
        when(f.plugin.getMysql().getTableName()).thenReturn("SyntheticPoints");
        when(f.plugin.getMysql().qi(anyString())).thenAnswer(call -> "`" + call.getArgument(0) + "`");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        return f;
    }

    @Test void publicTransferRejectedReservationRepairsRecreatedSourceBeforeCallback() throws Exception { assertPublicTransferReserve("rejected"); }
    @Test void publicTransferAmbiguousReservationCannotPublishRefillAsConfirmation() throws Exception { assertPublicTransferReserve("ambiguous"); }

    @Test void publicTransferPreparationFailureRecoversWithoutDebit() throws Exception { assertPublicTransferReserve("prepare"); }
    @Test void publicTransferAcquisitionFailureRecoversWithoutDebit() throws Exception { assertPublicTransferReserve("acquire"); }

    private void assertPublicTransferReserve(String phase) throws Exception {
        boolean ambiguous = phase.equals("ambiguous");
        Fixture f = mysqlPublicFixture();
        var target = mock(VotingPluginUser.class);
        when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
        when(target.getPointsPath()).thenReturn("Points");
        org.mockito.Mockito.doCallRealMethod().when(f.user).transferPointsWithResult(eq(target), eq(16), any());
        var connection = mock(java.sql.Connection.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(phase.equals("acquire") ? null : connection);
        var updates = new java.util.concurrent.atomic.AtomicInteger();
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            String sql = call.getArgument(0);
            if (phase.equals("prepare") && sql.startsWith("INSERT INTO")) throw new java.sql.SQLException("synthetic reservation preparation failure");
            var statement = mock(java.sql.PreparedStatement.class);
            when(statement.executeQuery()).thenReturn(mock(java.sql.ResultSet.class));
            if (sql.startsWith("UPDATE `SyntheticPoints` SET")) when(statement.executeUpdate()).thenAnswer(update -> {
                updates.incrementAndGet();
                var recreated = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
                recreated.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
                f.plugin.getUserManager().getDataManager().getUserDataCache().put(f.uuid, recreated);
                if (ambiguous) throw new java.sql.SQLException("synthetic unconfirmed reservation");
                return 0;
            });
            return statement;
        });
        var results = new java.util.ArrayList<com.bencodez.votingplugin.user.PointTransferResult>();
        var callbackBalances = new java.util.ArrayList<String>();
        f.user.transferPointsWithResult(target, 16, result -> {
            results.add(result); callbackBalances.add(f.request("points_nocache"));
        });
        f.pending.removeFirst().run();
        assertEquals(List.of(phase.equals("rejected") ? com.bencodez.votingplugin.user.PointTransferResult.INSUFFICIENT_POINTS
                : com.bencodez.votingplugin.user.PointTransferResult.UNAVAILABLE), results);
        assertEquals(phase.equals("prepare") || phase.equals("acquire") ? 0 : 1, updates.get());
        assertEquals(List.of(ambiguous ? "..." : "4"), callbackBalances);
        verify(target, never()).addPoints(org.mockito.ArgumentMatchers.anyInt());
        verify(f.plugin.getRewardHandler(), never()).giveReward(any(), any(), any(), any());
        if (ambiguous) {
            f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty(), "ambiguous reservation is never automatically retried");
        }
    }

    @Test void publicTransferApprovalRejectionConfirmedRefundRepairsBeforeCallback() throws Exception { assertPublicTransferApprovalRejection("confirmed"); }
    @Test void publicTransferApprovalRejectionAmbiguousRefundRemainsUnavailable() throws Exception { assertPublicTransferApprovalRejection("ambiguous"); }
    @Test void publicTransferApprovalRejectionUnstartedRefundRemainsRecoverable() throws Exception { assertPublicTransferApprovalRejection("prepare"); }

    private void assertPublicTransferApprovalRejection(String phase) throws Exception {
        Fixture f = mysqlPublicFixture();
        var target = mock(VotingPluginUser.class);
        when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
        when(target.getPointsPath()).thenReturn("Points");
        org.mockito.Mockito.doCallRealMethod().when(f.user).transferPointsWithResult(eq(target), eq(16), any());
        var storage = new java.util.concurrent.atomic.AtomicInteger(20);
        var debits = new java.util.concurrent.atomic.AtomicInteger();
        var refunds = new java.util.concurrent.atomic.AtomicInteger();
        var manager = f.plugin.getUserManager().getDataManager();
        doAnswer(call -> {
            var fresh = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            fresh.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(storage.get()))));
            manager.getUserDataCache().put(f.uuid, fresh); return null;
        }).when(manager).cacheUser(f.uuid, null);
        var connection = mock(java.sql.Connection.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(connection);
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            String sql = call.getArgument(0);
            boolean points = sql.startsWith("UPDATE `SyntheticPoints` SET");
            boolean refund = points && sql.contains(" + ?");
            if (refund && phase.equals("prepare")) throw new java.sql.SQLException("synthetic refund prepare failure");
            var statement = mock(java.sql.PreparedStatement.class);
            when(statement.executeQuery()).thenAnswer(query -> {
                var result = mock(java.sql.ResultSet.class);
                if (sql.contains("FOR UPDATE")) {
                    when(result.next()).thenReturn(true, false); when(result.getString(1)).thenReturn("RESERVED");
                }
                return result;
            });
            when(statement.executeUpdate()).thenAnswer(update -> {
                if (points) {
                    if (refund) { refunds.incrementAndGet(); storage.set(20); }
                    else { debits.incrementAndGet(); storage.set(4); }
                    if (refund && phase.equals("ambiguous")) throw new java.sql.SQLException("synthetic unconfirmed refund");
                }
                return 1;
            });
            return statement;
        });
        doThrow(new java.util.concurrent.RejectedExecutionException("synthetic approval rejected"))
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        var results = new java.util.ArrayList<com.bencodez.votingplugin.user.PointTransferResult>();
        var balances = new java.util.ArrayList<String>();
        f.user.transferPointsWithResult(target, 16, result -> { results.add(result); balances.add(f.request("points_nocache")); });
        f.pending.removeFirst().run();
        assertEquals(List.of(com.bencodez.votingplugin.user.PointTransferResult.UNAVAILABLE), results);
        assertEquals(1, debits.get()); assertEquals(phase.equals("prepare") ? 0 : 1, refunds.get());
        assertEquals(List.of(phase.equals("ambiguous") ? "..." : phase.equals("prepare") ? "4" : "20"), balances);
        verify(target, never()).addPoints(org.mockito.ArgumentMatchers.anyInt());
        assertTrue(f.pending.isEmpty(), "no retry may be scheduled for a refund outcome");
    }

    @Test void publicClaimedTransferRefundAcquisitionFailureRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("acquire"); }
    @Test void publicClaimedTransferRefundSelectPreparationRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("select"); }
    @Test void publicClaimedTransferRefundReadFailureRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("read"); }
    @Test void publicClaimedTransferRefundStatementPreparationRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("prepare"); }
    @Test void publicClaimedTransferRefundBindFailureRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("bind"); }
    @Test void publicClaimedTransferRefundSelectCloseFailureRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("select-close"); }
    @Test void publicClaimedTransferRefundConfirmedRestoresBalanceOnce() throws Exception { assertPublicClaimedTransferRefund("confirmed"); }
    @Test void publicClaimedTransferRefundExecutionFailureStaysUnavailable() throws Exception { assertPublicClaimedTransferRefund("execute"); }
    @Test void publicClaimedTransferRefundCommitFailureStaysUnavailable() throws Exception { assertPublicClaimedTransferRefund("commit"); }
    @Test void publicClaimedTransferRefundPostWriteCloseFailureStaysUnavailable() throws Exception { assertPublicClaimedTransferRefund("close"); }

    @Test void publicMarkerFailureFallbackExecutionRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("execute", "marker-failure"); }
    @Test void publicMarkerFailureFallbackCommitRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("commit", "marker-failure"); }
    @Test void publicMarkerFailureFallbackConfirmedRefundRemainsUsable() throws Exception { assertPublicClaimedTransferRefund("confirmed", "marker-failure"); }
    @Test void publicMarkerFailureFallbackUnstartedRefundRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("prepare", "marker-failure"); }
    @Test void publicMarkerFailureFallbackRefusedRefundDoesNotMutate() throws Exception { assertPublicClaimedTransferRefund("refused", "marker-failure"); }
    @Test void publicIndeterminateClaimExecutionRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("execute", "indeterminate"); }
    @Test void publicIndeterminateClaimCommitRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("commit", "indeterminate"); }
    @Test void publicIndeterminateClaimConfirmedRefundRemainsUsable() throws Exception { assertPublicClaimedTransferRefund("confirmed", "indeterminate"); }
    @Test void publicIndeterminateClaimUnstartedRefundRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("prepare", "indeterminate"); }
    @Test void publicIndeterminateClaimRefusedRefundDoesNotMutate() throws Exception { assertPublicClaimedTransferRefund("refused", "indeterminate"); }
    @Test void publicIndeterminateClaimMarkerFallbackExecutionRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("execute", "indeterminate-marker-failure"); }
    @Test void publicIndeterminateClaimMarkerFallbackCommitRemainsUnavailable() throws Exception { assertPublicClaimedTransferRefund("commit", "indeterminate-marker-failure"); }
    @Test void publicIndeterminateClaimMarkerFallbackConfirmedRefundRemainsUsable() throws Exception { assertPublicClaimedTransferRefund("confirmed", "indeterminate-marker-failure"); }
    @Test void publicIndeterminateClaimMarkerFallbackUnstartedRefundRemainsRecoverable() throws Exception { assertPublicClaimedTransferRefund("prepare", "indeterminate-marker-failure"); }
    @Test void publicIndeterminateClaimMarkerFallbackRefusedRefundDoesNotMutate() throws Exception { assertPublicClaimedTransferRefund("refused", "indeterminate-marker-failure"); }

    private void assertPublicClaimedTransferRefund(String phase) throws Exception {
        assertPublicClaimedTransferRefund(phase, "normal");
    }

    private void assertPublicClaimedTransferRefund(String phase, String branch) throws Exception {
        Fixture f = mysqlPublicFixture();
        boolean indeterminateClaim = branch.startsWith("indeterminate");
        boolean failedMarker = branch.contains("marker-failure");
        when(f.plugin.getDataFolder()).thenReturn(transferFallbackData.toFile());
        var target = mock(VotingPluginUser.class);
        var targetPlayer = mock(Player.class);
        when(target.getUUID()).thenReturn("00000000-0000-0000-0000-000000000002");
        when(target.getPointsPath()).thenReturn("Points"); when(target.getPlayer()).thenReturn(targetPlayer);
        org.mockito.Mockito.doCallRealMethod().when(f.user).transferPointsWithResult(eq(target), eq(16), any());
        var state = new java.util.concurrent.atomic.AtomicReference<String>("RESERVED");
        var storage = new java.util.concurrent.atomic.AtomicInteger(20);
        var marked = new java.util.concurrent.atomic.AtomicBoolean();
        var debits = new java.util.concurrent.atomic.AtomicInteger();
        var refunds = new java.util.concurrent.atomic.AtomicInteger();
        var manager = f.plugin.getUserManager().getDataManager();
        doAnswer(call -> {
            var fresh = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            fresh.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(storage.get()))));
            manager.getUserDataCache().put(f.uuid, fresh); return null;
        }).when(manager).cacheUser(f.uuid, null);
        var connection = mock(java.sql.Connection.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenAnswer(call ->
                marked.get() && phase.equals("acquire") ? null : connection);
        doAnswer(call -> {
            if (refunds.get() > 0 && phase.equals("commit")) throw new java.sql.SQLException("synthetic unconfirmed refund commit");
            if (state.get().equals("COMPENSATING")) marked.set(true);
            return null;
        }).when(connection).commit();
        doAnswer(call -> {
            if (refunds.get() > 0 && phase.equals("close")) throw new java.sql.SQLException("synthetic post-write connection close");
            return null;
        }).when(connection).close();
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            String sql = call.getArgument(0);
            boolean refundRead = marked.get() && sql.contains("FOR UPDATE");
            boolean points = sql.startsWith("UPDATE `SyntheticPoints` SET");
            boolean refund = points && sql.contains(" + ?");
            if (refundRead && phase.equals("select")) throw new java.sql.SQLException("synthetic pre-write select preparation");
            if (refund && phase.equals("prepare")) throw new java.sql.SQLException("synthetic pre-write refund preparation");
            var statement = mock(java.sql.PreparedStatement.class);
            var bound = new java.util.HashMap<Integer,String>();
            doAnswer(bind -> { bound.put(bind.getArgument(0), bind.getArgument(1)); return null; })
                    .when(statement).setString(org.mockito.ArgumentMatchers.anyInt(), any());
            if (refund && phase.equals("bind")) doThrow(new java.sql.SQLException("synthetic pre-write bind"))
                    .when(statement).setInt(1, 16);
            if (refundRead && phase.equals("select-close")) doThrow(new java.sql.SQLException("synthetic pre-write select close"))
                    .when(statement).close();
            when(statement.executeQuery()).thenAnswer(query -> {
                if (refundRead && phase.equals("read")) throw new java.sql.SQLException("synthetic pre-write read");
                var result = mock(java.sql.ResultSet.class);
                if (sql.contains("FOR UPDATE")) {
                    if (indeterminateClaim && !marked.get() && sql.contains("hook_owner")) state.set("HOOK_STARTED");
                    when(result.next()).thenReturn(true, false);
                    when(result.getString(1)).thenReturn(marked.get() && phase.equals("refused") ? "COMPLETED" : state.get());
                    if (indeterminateClaim && !marked.get()) when(result.getString(2)).thenReturn("synthetic-other-claim");
                }
                // Other journal lookups/recovery remain empty, including confirmation after a lost commit.
                return result;
            });
            when(statement.executeUpdate()).thenAnswer(update -> {
                if (points) {
                    if (refund) {
                        refunds.incrementAndGet(); storage.set(20);
                        if (phase.equals("execute")) {
                            // Durable terminal evidence becomes visible only to the later guarded fallback,
                            // while this invocation still loses its execution acknowledgement.
                            state.set("REFUNDED");
                            throw new java.sql.SQLException("synthetic unconfirmed refund execution");
                        }
                    } else { debits.incrementAndGet(); storage.set(4); }
                } else if (sql.startsWith("UPDATE") && bound.containsKey(1)) {
                    if (bound.get(1).equals("COMPENSATING")) {
                        marked.set(true);
                        if (failedMarker) throw new java.sql.SQLException("synthetic compensation marker acknowledgement lost");
                    }
                    state.set(bound.get(1));
                }
                return 1;
            });
            return statement;
        });
        // First global task admits the claim. After the real claim succeeds, both approval lanes reject.
        var globals = new java.util.concurrent.atomic.AtomicInteger();
        doAnswer(call -> {
            if (globals.incrementAndGet() == 2) throw new java.util.concurrent.RejectedExecutionException("synthetic approval fallback rejected");
            call.getArgument(1, Runnable.class).run(); return null;
        }).when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        doThrow(new java.util.concurrent.RejectedExecutionException("synthetic target entity retired"))
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class), eq(targetPlayer));
        var results = new java.util.ArrayList<com.bencodez.votingplugin.user.PointTransferResult>();
        f.user.transferPointsWithResult(target, 16, results::add);
        f.pending.removeFirst().run(); // reserve -> first global admission
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run(); // real claim -> rejected entity/fallback -> compensation admission
        if (!indeterminateClaim) {
            assertEquals("HOOK_STARTED", state.get());
            assertEquals(1, f.pending.size());
            f.pending.removeFirst().run(); // real markCompensating -> refundHookStarted -> completion
        }
        assertTrue(marked.get(), "must reach the post-claim durable compensation path");
        assertEquals(List.of(com.bencodez.votingplugin.user.PointTransferResult.UNAVAILABLE), results);
        assertEquals(1, debits.get());
        boolean attempted = List.of("confirmed", "execute", "commit", "close").contains(phase);
        assertEquals(attempted ? 1 : 0, refunds.get());
        assertTrue(f.pending.isEmpty(), "no automatic transaction retry may be scheduled");
        if (attempted && !phase.equals("confirmed")) {
            com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(f.plugin, f.uuid.toString(), "Points");
            assertEquals("...", f.request("points_nocache"));
            f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
            assertEquals("...", f.request("points_nocache"));
            assertTrue(f.pending.isEmpty());
        } else {
            f.request("points_nocache"); f.drain();
            assertEquals(phase.equals("confirmed") ? "20" : "4", f.request("points_nocache"));
        }

        if (phase.equals("refused") || failedMarker && !phase.equals("confirmed")) {
            assertTrue(java.nio.file.Files.isDirectory(transferFallbackData.resolve(".point-transfer-compensations")),
                    "the original local compensation proof must survive an unresolved fallback");
        }
        verify(target, never()).addPoints(org.mockito.ArgumentMatchers.anyInt());
        verify(f.plugin.getRewardHandler(), never()).giveReward(any(), any(), any(), any());
        verify(f.plugin.getRewardHandler(), never()).giveRewardAsync(any(), any(), any(), any());
    }

    @Test void publicPurchaseNullConnectionRemainsRecoverableWithoutDebit() throws Exception { assertPublicPurchasePreparation("acquire"); }
    @Test void publicPurchaseStatementPreparationRemainsRecoverableWithoutDebit() throws Exception { assertPublicPurchasePreparation("prepare"); }

    @Test void publicPurchaseReservePreparationHasNoDebitAndRemainsRecoverable() throws Exception { assertPublicPurchasePreparation("reserve-prepare"); }
    @Test void publicPurchaseReserveBindHasNoDebitAndRemainsRecoverable() throws Exception { assertPublicPurchasePreparation("reserve-bind"); }

    private void assertPublicPurchasePreparation(String phase) throws Exception {
        Fixture f = mysqlPublicFixture();
        var connection = mock(java.sql.Connection.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(phase.equals("acquire") ? null : connection);
        var pointUpdates = new java.util.concurrent.atomic.AtomicInteger();
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            String sql = call.getArgument(0);
            if (phase.equals("prepare") || (phase.equals("reserve-prepare") && sql.startsWith("INSERT INTO")))
                throw new java.sql.SQLException("synthetic preparation failed");
            var statement = mock(java.sql.PreparedStatement.class);
            if (phase.equals("reserve-bind") && sql.startsWith("UPDATE `SyntheticPoints` SET"))
                doThrow(new java.sql.SQLException("synthetic bind failure")).when(statement).setInt(1, 16);
            when(statement.executeUpdate()).thenAnswer(update -> { if (sql.startsWith("UPDATE `SyntheticPoints` SET")) pointUpdates.incrementAndGet(); return 1; });
            return statement;
        });
        var results = new java.util.ArrayList<com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult>();
        startPublicPurchase(f, results::add);
        f.pending.removeFirst().run();
        assertEquals(List.of(com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult.FAILED), results);
        assertEquals("...", f.request("points"));
        f.drain();
        assertEquals("4", f.request("points_nocache"));
        assertEquals(0, pointUpdates.get(), "no points UPDATE may be attempted in preparation failures");
        verify(f.plugin.getRewardHandler(), never()).giveRewardAsync(any(), any(), any(), any());
    }

    @Test void publicPurchaseCompensationConfirmedRefundRestoresBalance() throws Exception { assertPublicCompensation("confirmed"); }
    @Test void publicPurchaseCompensationGuardDoesNotRefundOrReward() throws Exception { assertPublicCompensation("guard"); }
    @Test void publicPurchaseCompensationUnconfirmedRefundStaysUnavailable() throws Exception { assertPublicCompensation("ambiguous"); }
    @Test void publicPurchaseCompensationPreparationFailureDoesNotInventUncertainty() throws Exception { assertPublicCompensation("prepare"); }

    @Test void publicLimitedPurchaseEpochReadFailureIsRecoverableBeforeRefundUpdate() throws Exception { assertPublicCompensation("epoch"); }
    @Test void publicLimitedPurchaseConfirmedRefundKeepsEpochGuard() throws Exception { assertPublicCompensation("epoch-confirmed"); }
    @Test void publicLimitedPurchaseUnconfirmedRefundStaysUnavailable() throws Exception { assertPublicCompensation("epoch-ambiguous"); }

    private void assertPublicCompensation(String phase) throws Exception {
        Fixture f = mysqlPublicFixture();
        boolean limited = phase.startsWith("epoch");
        boolean ambiguous = phase.equals("ambiguous") || phase.equals("epoch-ambiguous");
        var storage = new java.util.concurrent.atomic.AtomicInteger(20);
        var state = new java.util.concurrent.atomic.AtomicReference<String>("PENDING");
        var refunds = new java.util.concurrent.atomic.AtomicInteger();
        var debits = new java.util.concurrent.atomic.AtomicInteger();
        var connection = mock(java.sql.Connection.class);
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection()).thenReturn(connection);
        doAnswer(call -> {
            if (ambiguous && refunds.get() > 0) throw new java.sql.SQLException("synthetic refund acknowledgement lost");
            return null;
        }).when(connection).commit();
        var manager = f.plugin.getUserManager().getDataManager();
        doAnswer(call -> {
            var fresh = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            fresh.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(storage.get()))));
            manager.getUserDataCache().put(f.uuid, fresh); return null;
        }).when(manager).cacheUser(f.uuid, null);
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            String sql = call.getArgument(0);
            if (phase.equals("epoch") && state.get().equals("COMPENSATING") && sql.startsWith("SELECT `epoch`"))
                throw new java.sql.SQLException("synthetic epoch prerequisite failure");
            boolean pointUpdate = sql.startsWith("UPDATE `SyntheticPoints` SET");
            boolean refund = pointUpdate && sql.contains(" + ?");
            if (refund && phase.equals("prepare")) throw new java.sql.SQLException("synthetic refund preparation failure");
            var statement = mock(java.sql.PreparedStatement.class);
            var bound = new java.util.HashMap<Integer,String>();
            doAnswer(bind -> { bound.put(bind.getArgument(0), bind.getArgument(1)); return null; }).when(statement).setString(org.mockito.ArgumentMatchers.anyInt(), any());
            when(statement.executeUpdate()).thenAnswer(update -> {
                if (pointUpdate) {
                    if (refund) { refunds.incrementAndGet(); storage.set(20); }
                    else { debits.incrementAndGet(); storage.set(4); }
                } else if (sql.startsWith("UPDATE") && bound.containsKey(1)) state.set(bound.get(1));
                return 1;
            });
            when(statement.executeQuery()).thenAnswer(query -> {
                var result = mock(java.sql.ResultSet.class);
                if (sql.contains("FOR UPDATE") || (ambiguous && sql.startsWith("SELECT"))) {
                    when(result.next()).thenReturn(true, false);
                    when(result.getString(1)).thenReturn(phase.equals("guard") ? "HOOK_STARTED"
                            : ambiguous && refunds.get() > 0 ? "COMPENSATING" : state.get());
                    when(result.getString(2)).thenReturn(f.uuid.toString());
                    when(result.getString(3)).thenReturn("Points");
                    when(result.getInt(5)).thenReturn(16);
                    if (limited) {
                        when(result.getString(4)).thenReturn("VoteShopLimitsynthetic-public-item");
                        when(result.getObject(8)).thenReturn(7L);
                        when(result.getLong(1)).thenReturn(7L);
                    }
                    when(result.wasNull()).thenReturn(!limited);
                }
                return result;
            });
            return statement;
        });
        var asyncAdmissions = new java.util.concurrent.atomic.AtomicInteger();
        doAnswer(call -> {
            if (asyncAdmissions.incrementAndGet() == 2) throw new java.util.concurrent.RejectedExecutionException("synthetic claim scheduler stopped");
            call.getArgument(1, Runnable.class).run(); return null;
        }).when(f.scheduler).runTaskAsynchronously(eq(f.plugin), any(Runnable.class));
        var results = new java.util.ArrayList<com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult>();
        startPublicPurchase(f, results::add, limited ? 3 : 0);
        f.pending.removeFirst().run(); // reserve, then rejected claim schedules compensation
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run();
        assertEquals(List.of(com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult.FAILED), results);
        assertEquals(1, debits.get());
        assertEquals(phase.equals("prepare") || phase.equals("guard") || phase.equals("epoch") ? 0 : 1, refunds.get());
        assertTrue(f.pending.isEmpty(), "compensation must not retry its mutation automatically");
        if (ambiguous) {
            assertEquals("...", f.request("points_nocache"), "an unconfirmed refund cannot reopen numeric publication");
            f.placeholders.sharedPointRefillCompletion(f.uuid).accept(true);
            assertEquals("...", f.request("points_nocache"));
        } else {
            f.request("points_nocache"); f.drain();
            assertEquals(phase.equals("confirmed") || phase.equals("epoch-confirmed") ? "20" : "4", f.request("points_nocache"));
        }
        verify(f.plugin.getRewardHandler(), never()).giveReward(any(), any(), any(), any());
        verify(f.plugin.getRewardHandler(), never()).giveRewardAsync(any(), any(), any(), any());
    }

    private void startPublicPurchase(Fixture f, java.util.function.Consumer<com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult> completion) {
        startPublicPurchase(f, completion, 0);
    }

    private void startPublicPurchase(Fixture f, java.util.function.Consumer<com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult> completion, int limit) {
        var definition = mock(com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition.class);
        when(definition.isEnabled()).thenReturn(true);
        var item = mock(com.bencodez.votingplugin.voteshop.shop.VoteShopItem.class);
        when(item.getPermission()).thenReturn(""); when(item.getCost()).thenReturn(16); when(item.getLimit()).thenReturn(limit);
        when(item.getIdentifier()).thenReturn("synthetic-public-item");
        new com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService(f.plugin, definition).purchase(f.player, f.user, item, completion);
    }

    @Test void confirmedSetRetiresRenderedBalanceAndPublishesRefill() throws Exception { assertSetOutcome(1, false); }
    @Test void zeroRowSetRetiresRenderedBalanceAndPublishesRefill() throws Exception { assertSetOutcome(0, false); }
    @Test void ambiguousSetDoesNotPublishAsAConfirmedBalance() throws Exception { assertSetOutcome(0, true); }

    private void assertSetOutcome(int affectedRows, boolean ambiguous) throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        var table = f.plugin.getMysql();
        when(table.getTableName()).thenReturn("VotingPlugin_Users");
        when(table.qi(anyString())).thenAnswer(call -> "`" + call.getArgument(0) + "`");
        var connection = mock(java.sql.Connection.class);
        var statement = mock(java.sql.PreparedStatement.class);
        when(table.getMysql().getConnectionManager().getConnection()).thenReturn(connection);
        when(connection.prepareStatement(anyString())).thenReturn(statement);
        if (ambiguous) when(statement.executeUpdate()).thenThrow(new java.sql.SQLException("synthetic acknowledgement lost"));
        else when(statement.executeUpdate()).thenReturn(affectedRows);
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        boolean result = new com.bencodez.votingplugin.tests.user.SharedMysqlPointMutatorAccess(f.plugin).setCommitted(f.user, 4);
        assertEquals(!ambiguous && affectedRows == 1, result);
        verify(statement, org.mockito.Mockito.times(1)).executeUpdate();
        if (ambiguous) assertEquals("...", f.request("points_nocache"));
        else {
            assertEquals("...", f.request("points"));
            f.drain();
            assertEquals("4", f.request("points"));
        }
    }

    @Test void falseVoteShopReservationDoesNotRewardAndRetiresRecreatedStaleRender() throws Exception { assertFalseShopReservation(true); }
    @Test void falseVoteShopReservationRecoversFromExplicitAuthoritativeRefill() throws Exception { assertFalseShopReservation(false); }

    private void assertFalseShopReservation(boolean requireImmediateInvalidation) throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        when(f.user.isCached()).thenReturn(true);
        var old = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        old.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var caches = new java.util.concurrent.ConcurrentHashMap<java.util.UUID, com.bencodez.advancedcore.api.user.usercache.UserDataCache>();
        caches.put(f.uuid, old);
        when(f.user.getCache()).thenReturn(old);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(caches);
        doAnswer(call -> { caches.remove(f.uuid); return null; }).when(manager).removeCache(f.uuid, null);
        doAnswer(call -> {
            var fresh = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
            fresh.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            caches.put(f.uuid, fresh);
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        var table = f.plugin.getMysql();
        when(table.getTableName()).thenReturn("VotingPlugin_Users");
        when(table.qi(anyString())).thenAnswer(call -> "`" + call.getArgument(0) + "`");
        var connection = mock(java.sql.Connection.class);
        when(table.getMysql().getConnectionManager().getConnection()).thenReturn(connection);
        when(connection.prepareStatement(anyString())).thenAnswer(call -> {
            var statement = mock(java.sql.PreparedStatement.class);
            if (call.<String>getArgument(0).startsWith("UPDATE `VotingPlugin_Users` SET")) {
                when(statement.executeUpdate()).thenAnswer(update -> {
                    // Recreate a stale read snapshot while the authoritative conditional debit rejects.
                    var recreated = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
                    recreated.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
                    caches.put(f.uuid, recreated);
                    return 0;
                });
            }
            return statement;
        });
        when(f.scheduler.getFoliaLib()).thenReturn(null);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class), eq(f.player));
        var definition = mock(com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition.class);
        when(definition.isEnabled()).thenReturn(true);
        var item = mock(com.bencodez.votingplugin.voteshop.shop.VoteShopItem.class);
        when(item.getPermission()).thenReturn("");
        when(item.getCost()).thenReturn(16);
        when(item.getLimit()).thenReturn(0);
        when(item.getIdentifier()).thenReturn("synthetic-item");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        var result = new java.util.concurrent.atomic.AtomicReference<com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult>();
        new com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService(f.plugin, definition)
                .purchase(f.player, f.user, item, result::set);
        assertEquals(1, f.pending.size());
        f.pending.removeFirst().run();
        assertEquals(com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult.NOT_ENOUGH_POINTS, result.get());
        verify(f.plugin.getRewardHandler(), never()).giveReward(any(), any(), any(), any());
        verify(f.plugin.getRewardHandler(), never()).giveRewardAsync(any(), any(), any(), any());
        if (requireImmediateInvalidation) assertEquals("...", f.request("points"), "a failed reservation must retire its stale rendered balance");
        com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(f.plugin, f.uuid.toString(), "Points");
        assertEquals("4", f.request("points_nocache"));
    }
    private VotingPluginMain previousPlugin;

    @BeforeEach void rememberPlugin() { previousPlugin = VotingPluginMain.plugin; }
    @AfterEach void restorePlugin() { VotingPluginMain.plugin = previousPlugin; }

    @Test void autoAllPopulatesConfiguredMonthlyTotal() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.AUTOALL, "total_monthly", "42");
    }

    @Test void specificAllPopulatesConfiguredMonthlyTotal() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.SPECIFICALL, "total_monthly", "42");
    }

    @Test void autoAllPopulatesConfiguredPoints() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.AUTOALL, "points", "17");
    }

    @Test void specificAllPopulatesConfiguredPoints() {
        assertConfiguredColdEntry(PlaceholderCacheLevel.SPECIFICALL, "points", "17");
    }

    @Test void autoAllAdmitsAndPopulatesUnconfiguredMonthlyTotal() {
        assertAutomaticAdmission("total_monthly", "42");
    }

    @Test void autoAllAdmitsAndPopulatesUnconfiguredPoints() {
        assertAutomaticAdmission("points", "17");
    }

    @Test void onlineOnlyModesLeaveColdOfflineEntriesEmpty() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("...", f.request("points"));
            f.drain();
            assertEquals("...", f.request("points"));
            f.drain();
            verify(f.user, never()).getPoints();
            assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        }
    }

    @Test void configuredColdOnlineEntriesStillPopulateInEveryCachingMode() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC,
                PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.presence.playerOnline(f.uuid, f.player);
            assertEquals("...", f.request("points"));
            f.drain();
            assertEquals("17", f.request("points"));
            assertTrue(f.pending.isEmpty());
        }
    }

    @Test void warmOfflineEntriesStillRefreshInBothAllPlayerModes() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.placeholder("points").getCache().get("points").put(f.uuid, "old");
            f.placeholders.schedulePlaceholderCheck(f.user);
            f.drain();
            assertEquals("17", f.request("points"));
            assertTrue(f.pending.isEmpty());
        }
    }

    @Test void noneModeKeepsDirectUncachedEvaluation() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        assertEquals("17", f.request("points"));
        assertEquals("42", f.request("total_monthly"));
        assertTrue(f.pending.isEmpty());
        assertFalse(f.placeholder("points").isUsesCache());
    }

    @Test void explicitNoCacheAndProcessSuffixesKeepTheirContracts() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("17", f.request("points_nocache"));
            assertEquals("17", f.request("points_process"));
            assertTrue(f.pending.isEmpty());
            assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        }
    }

    @Test void reconciledDebitRetiresTheStaleRenderedBalance() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        assertEquals("20", f.request("points"));
        f.placeholders.beginSharedPointDebit(f.uuid);
        assertEquals("...", f.request("points"));
        assertEquals("...", f.request("points_nocache"));
        when(f.user.getPoints()).thenReturn(4);
        f.placeholders.finishSharedPointDebit(f.uuid, true);
        assertEquals("...", f.request("points"));
        f.drain();
        assertEquals("4", f.request("points"));
        assertEquals("4", f.request("points_nocache"));
    }

    @Test void uncertainOrUnavailableSnapshotNeverPublishesOldPoints() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, false);
        assertEquals("...", f.request("points"));
        assertEquals("...", f.request("points_nocache"));
        f.placeholders.onUpdate(f.user, true);
        verify(f.user, never()).getPoints();
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true);
        assertEquals("...", f.request("points"), "a later debit cannot resolve an uncertain earlier outcome");
    }

    @Test void confirmedDebitCanRecoverAfterTransientRefillFailure() {
        assertDebitRefillRecovery(true);
    }

    @Test void rejectedDebitCanRecoverAfterTransientRefillFailure() {
        assertDebitRefillRecovery(true);
    }

    @Test void unresolvedTransactionIsNotClearedByVerifiedRefill() { assertDebitRefillRecovery(false); }

    private static void assertDebitRefillRecovery(boolean known) {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, known, false);
        assertEquals("...", f.request("points_nocache"));
        when(f.user.getPoints()).thenReturn(4);
        com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(f.plugin, f.uuid.toString(), "Points");
        assertEquals(known ? "4" : "...", f.request("points_nocache"));
    }

    @Test void workerPointReconciliationRetiresSiblingMutationRenderedCache() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        when(f.user.getPoints()).thenReturn(4);
        com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(f.plugin, f.uuid.toString(), "Points");
        assertEquals("...", f.request("points"));
        f.drain();
        assertEquals("4", f.request("points"));
    }

    @Test void asyncPointReconciliationRetiresRenderedBalanceBeforeRefill() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.invalidateAndRefresh(f.plugin, f.uuid.toString(), "Points");
        assertEquals("...", f.request("points"));
        assertEquals("...", f.request("points_nocache"));
        verify(f.user, never()).getPoints();
    }

    @Test void mysqlNoCachePlaceholderUsesPublishedSnapshotWithoutStorageGetter() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        when(f.user.getUserDataFetchMode()).thenReturn(com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doThrow(new AssertionError("platform placeholder entered storage-backed getter")).when(f.user).getPoints();
        assertEquals("4", f.request("points_nocache"));
    }

    @Test void coldMysqlNoCacheReadQueuesWorkerWithoutEnteringStorage() {
        Fixture f = new Fixture(PlaceholderCacheLevel.NONE);
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>());
        doThrow(new AssertionError("cold platform read entered storage")).when(f.user).getPoints();
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size());
        verify(f.plugin.getUserManager().getDataManager(), never()).cacheUser(any(), any());
    }

    @Test void rejectedBulkAdmissionRetiresStaleRenderedPoints() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        doThrow(new java.util.concurrent.RejectedExecutionException("full")).when(f.timer).execute(any(Runnable.class));
        VotingPluginUser.removePointsStorageAware(f.plugin, List.of(f.user), new java.util.IdentityHashMap<>(),
                16, "synthetic-batch", (user, success) -> { });
        assertEquals("...", f.request("points"));
    }

    @Test void rejectedDuplicateBulkAdmissionsEachRetireAndPermitWorkerRecovery() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        doThrow(new java.util.concurrent.RejectedExecutionException("full"))
                .doAnswer(call -> { f.pending.addLast(call.getArgument(0)); return null; })
                .when(f.timer).execute(any(Runnable.class));
        VotingPluginUser.removePointsStorageAware(f.plugin, List.of(f.user, f.user), new java.util.IdentityHashMap<>(),
                16, "synthetic-duplicate-batch", (user, success) -> { });
        assertEquals("...", f.request("points_nocache"));
        assertEquals(1, f.pending.size(), "all rejected occurrences must retire before coalesced recovery");
        f.pending.removeFirst().run();
        assertEquals("4", f.request("points_nocache"));
    }

    @Test void acceptedBulkAdmissionClosesRenderingUntilItsWorkerSettles() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        when(f.plugin.getUserManager().getDataManager().getUserDataCache())
                .thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        VotingPluginUser.removePointsStorageAware(f.plugin, List.of(f.user), new java.util.IdentityHashMap<>(),
                16, "synthetic-accepted-batch", (user, success) -> { });
        assertEquals(1, f.pending.size());
        assertEquals("...", f.request("points"));
        assertEquals(1, f.pending.size(), "an owned mutation must not admit a separate recovery");
    }

    @Test void indeterminateAdditionCannotPublishFromAnUnrelatedSuccessfulRefill() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        when(f.plugin.getStorageType()).thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
        when(f.user.getPointsPath()).thenReturn("Points");
        when(f.plugin.getMysql().getTableName()).thenReturn("VotingPlugin_Users");
        when(f.plugin.getMysql().getMysql().getConnectionManager().getConnection())
                .thenReturn((java.sql.Connection) null);
        var cache = new com.bencodez.advancedcore.api.user.usercache.UserDataCache(null, f.uuid);
        cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(20))));
        var manager = f.plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(f.uuid, cache)));
        doAnswer(call -> {
            cache.updateCache(new java.util.HashMap<>(java.util.Map.of("Points", new com.bencodez.simpleapi.sql.data.DataValueInt(4))));
            return null;
        }).when(manager).cacheUser(f.uuid, null);
        var result = new com.bencodez.votingplugin.tests.user.SharedMysqlPointMutatorAccess(f.plugin)
                .addCommitted(f.user, 16, "synthetic-addition");
        assertEquals(com.bencodez.votingplugin.tests.user.SharedMysqlPointMutatorAccess.MutationOutcome.INDETERMINATE, result.outcome());
        assertEquals("...", f.request("points_nocache"));
    }

    @Test void stoppedPlaceholderWorkerDoesNotExposeRetiredBalanceOrThrow() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true);
        doThrow(new java.util.concurrent.RejectedExecutionException("stopped"))
                .when(f.timer).execute(any(Runnable.class));
        assertEquals("...", f.request("points"));
        verify(f.user, never()).getPoints();
    }

    @Test void overlappingDebitsRemainUnavailableUntilBothHaveSettled() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        f.placeholder("points").getCache().get("points").put(f.uuid, "20");
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.beginSharedPointDebit(f.uuid);
        f.placeholders.finishSharedPointDebit(f.uuid, true);
        assertEquals("...", f.request("points"));
        f.placeholders.finishSharedPointDebit(f.uuid, false);
        assertEquals("...", f.request("points"));
        verify(f.user, never()).getPoints();
    }

    @Test void oldRenderCannotPublishAfterNewerDebit() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var entered = new java.util.concurrent.CountDownLatch(1);
        var resume = new java.util.concurrent.CountDownLatch(1);
        when(f.user.getPoints()).thenAnswer(call -> {
            entered.countDown();
            assertTrue(resume.await(5, TimeUnit.SECONDS));
            return 20;
        });
        try (var worker = Executors.newSingleThreadExecutor()) {
            var old = worker.submit(() -> f.placeholders.onUpdate(f.user, true));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            f.placeholders.beginSharedPointDebit(f.uuid);
            f.placeholders.finishSharedPointDebit(f.uuid, true);
            resume.countDown();
            old.get(5, TimeUnit.SECONDS);
        } finally { resume.countDown(); }
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        when(f.user.getPoints()).thenReturn(4);
        assertEquals("...", f.request("points"));
        f.drain();
        assertEquals("4", f.request("points"));
    }

    @Test void anotherPlayersDebitDoesNotDiscardThisPlayersRender() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var entered = new java.util.concurrent.CountDownLatch(1);
        var resume = new java.util.concurrent.CountDownLatch(1);
        when(f.user.getPoints()).thenAnswer(call -> {
            entered.countDown();
            assertTrue(resume.await(5, TimeUnit.SECONDS));
            return 17;
        });
        try (var worker = Executors.newSingleThreadExecutor()) {
            var render = worker.submit(() -> f.placeholders.onUpdate(f.user, true));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            UUID other = UUID.randomUUID();
            f.placeholders.beginSharedPointDebit(other);
            f.placeholders.finishSharedPointDebit(other, true);
            resume.countDown();
            render.get(5, TimeUnit.SECONDS);
        } finally { resume.countDown(); }
        assertEquals("17", f.request("points"));
    }

    @Test void renderStartedBeforeReloadCannotPublishAfterReload() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        var entered = new java.util.concurrent.CountDownLatch(1);
        var resume = new java.util.concurrent.CountDownLatch(1);
        when(f.user.getPoints()).thenAnswer(call -> {
            entered.countDown();
            assertTrue(resume.await(5, TimeUnit.SECONDS));
            return 20;
        });
        try (var worker = Executors.newSingleThreadExecutor()) {
            var render = worker.submit(() -> f.placeholders.onUpdate(f.user, true));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            try (var bukkit = mockStatic(Bukkit.class)) {
                bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
                f.placeholders.reload();
            }
            resume.countDown();
            render.get(5, TimeUnit.SECONDS);
        } finally { resume.countDown(); }
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
    }

    @Test void specificAllDoesNotAutomaticallyAdmitAnUnconfiguredIdentifier() {
        Fixture f = new Fixture(PlaceholderCacheLevel.SPECIFICALL);
        assertEquals("..", f.request("points"));
        assertTrue(f.pending.isEmpty());
        assertFalse(f.placeholder("points").isUsesCache());
        verify(f.user, never()).getPoints();
    }

    @Test void unknownIdentifiersDoNotScheduleOrReadUserData() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            assertEquals("Not a valid placeholder", f.request("unknown_test_placeholder"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, never()).getPoints();
        }
    }

    @Test void admittedOfflinePopulationCanRunOnAnActualWorkerWithoutBukkitReads() throws Exception {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points", "total_monthly");
        assertEquals("...", f.request("points"));
        try (var worker = Executors.newSingleThreadExecutor()) {
            worker.submit(() -> {
                try (var bukkit = mockStatic(Bukkit.class)) {
                    f.drain();
                    bukkit.verifyNoInteractions();
                }
            }).get(5, TimeUnit.SECONDS);
        }
        assertEquals("17", f.request("points"));
        assertEquals("42", f.request("total_monthly"));
        verify(f.user, never()).isOnline();
        verify(f.user, never()).getPlayer();
    }

    @Test void realCalculatingPlaceholderPopulatesDataAndValueTogether() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "next_example");
            assertEquals("...", f.request("next_example"));
            f.drain();
            CalculatingPlaceholder<?> calculating = (CalculatingPlaceholder<?>) f.placeholder("next_example");
            assertEquals("1234", calculating.getCacheData().get(f.uuid));
            assertEquals("wait", calculating.getCache().get("next_example").get(f.uuid));
            assertEquals("wait", f.request("next_example"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, times(1)).getTime(f.site);
            verify(f.user, never()).voteCommandNextInfo(f.site);
        }
    }

    @Test void coldPlatformPlaceholdersUseOnlyTheExplicitOfflineFallback() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "sitesavailable", "sitesavailabletotal");
            when(f.user.getSitesNotVotedOnWithoutOnlinePermissions()).thenReturn(4);
            when(f.user.getTotalNumberOfSitesWithoutOnlinePermissions()).thenReturn(6);
            assertEquals("...", f.request("sitesavailable"));
            try (var bukkit = mockStatic(Bukkit.class)) {
                f.drain();
                bukkit.verifyNoInteractions();
            }
            assertEquals("4", f.request("sitesavailable"));
            assertEquals("6", f.request("sitesavailabletotal"));
            assertTrue(f.pending.isEmpty());
            verify(f.user, never()).getSitesNotVotedOn();
            verify(f.user, never()).getTotalNumberOfSites();
            verify(f.scheduler, never()).runTask(eq(f.plugin), any(Runnable.class), any(Player.class));
        }
    }

    @Test void aPlatformPlaceholderWithoutOfflineFallbackIsNotEvaluatedOnTheWorker() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL);
        PlaceHolder<VotingPluginUser> liveOnly = new PlaceHolder<>("liveonly") {
            @Override public String placeholderRequest(VotingPluginUser user, String identifier) {
                throw new AssertionError("live-only producer evaluated without entity ownership");
            }
        };
        f.placeholders.getPlaceholders().add(PlaceHoldersAccess.platformOwned(f.placeholders, liveOnly));
        PlaceHoldersAccess.publishUserDataChangePlaceholders(f.placeholders);
        assertEquals("..", f.request("liveonly"));
        f.drain();
        assertFalse(liveOnly.getCache().get("liveonly").containsKey(f.uuid));
        verify(f.scheduler, never()).runTask(eq(f.plugin), any(Runnable.class), any(Player.class));
    }

    @Test void coldPlatformWorkMovesToReplacementOwnerBeforePublishing() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "sitesavailable");
        when(f.user.canVoteSite(f.site)).thenReturn(true);
        when(f.site.getPermissionToView()).thenReturn("");
        f.presence.playerOnline(f.uuid, f.player);
        assertEquals("...", f.request("sitesavailable"));
        f.drain();
        ArgumentCaptor<Runnable> first = ArgumentCaptor.forClass(Runnable.class);
        verify(f.scheduler).runTask(eq(f.plugin), first.capture(), eq(f.player));
        assertFalse(f.placeholder("sitesavailable").getCache().get("sitesavailable").containsKey(f.uuid));
        Player replacement = mock(Player.class);
        f.presence.playerOffline(f.uuid, f.player);
        f.placeholders.onLogout(f.uuid);
        f.presence.playerOnline(f.uuid, replacement);
        first.getValue().run();
        assertFalse(f.placeholder("sitesavailable").getCache().get("sitesavailable").containsKey(f.uuid));
        ArgumentCaptor<Runnable> second = ArgumentCaptor.forClass(Runnable.class);
        verify(f.scheduler).runTask(eq(f.plugin), second.capture(), eq(replacement));
        second.getValue().run();
        assertEquals("1", f.request("sitesavailable"));
        verify(f.user, never()).getSitesNotVotedOn();
    }

    @Test void queuedColdWorkHonorsTheReloadedOnlineOnlyMode() {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL, "points");
        assertEquals("...", f.request("points"));
        when(f.plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTO);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            f.placeholders.reload();
        }
        f.drain();
        verify(f.user, never()).getPoints();
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
    }

    @Test void logoutBeforeAdmittedWorkPreservesEachModesOfflinePolicy() {
        for (PlaceholderCacheLevel level : List.of(PlaceholderCacheLevel.AUTO, PlaceholderCacheLevel.SPECIFIC,
                PlaceholderCacheLevel.AUTOALL, PlaceholderCacheLevel.SPECIFICALL)) {
            Fixture f = new Fixture(level, "points");
            f.presence.playerOnline(f.uuid, f.player);
            assertEquals("...", f.request("points"));
            f.presence.playerOffline(f.uuid, f.player);
            f.placeholders.onLogout(f.uuid);
            f.drain();
            if (level.onlineOnly()) {
                assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
                verify(f.user, never()).getPoints();
            } else {
                assertEquals("17", f.request("points"));
                assertTrue(f.pending.isEmpty());
            }
        }
    }

    @Test void autoAllNonLoginCompletionDoesNotReadColdStorage() {
        assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel.AUTOALL);
    }

    @Test void specificAllNonLoginCompletionDoesNotReadColdStorage() {
        assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel.SPECIFICALL);
    }

    private static void assertNonLoginCompletionRetainsColdAdmission(PlaceholderCacheLevel level) {
        Fixture f = new Fixture(level, "points", "total_monthly");
        doThrow(new AssertionError("platform completion read cold points storage")).when(f.user).getPoints();
        doThrow(new AssertionError("platform completion read cold total storage"))
                .when(f.user).getTotal(TopVoter.Monthly);
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class));
        BukkitCompletionScheduler.run(f.plugin, null, () -> f.placeholders.onUpdate(f.user, false));
        assertFalse(f.placeholder("points").getCache().get("points").containsKey(f.uuid));
        assertFalse(f.placeholder("total_monthly").getCache().get("total_monthly").containsKey(f.uuid));
        assertTrue(f.pending.isEmpty());
        verify(f.user, never()).getPoints();
        verify(f.user, never()).getTotal(TopVoter.Monthly);
    }

    private static void assertConfiguredColdEntry(PlaceholderCacheLevel level, String identifier, String value) {
        Fixture f = new Fixture(level, identifier);
        assertEquals("...", f.request(identifier));
        assertEquals(1, f.pending.size());
        f.drain();
        assertEquals(value, f.request(identifier));
        assertEquals(value, f.request(identifier));
        assertTrue(f.pending.isEmpty(), "successful first population must stop empty rescheduling");
        if (identifier.equals("points")) verify(f.user, times(1)).getPoints();
        else verify(f.user, times(1)).getTotal(TopVoter.Monthly);
    }

    private static void assertAutomaticAdmission(String identifier, String value) {
        Fixture f = new Fixture(PlaceholderCacheLevel.AUTOALL);
        assertEquals("..", f.request(identifier));
        assertEquals(1, f.pending.size());
        f.drain();
        assertEquals(value, f.request(identifier));
        assertEquals(value, f.request(identifier));
        assertTrue(f.pending.isEmpty());
        verify(f.plugin.getServerData(), times(1)).addAutoCachedPlaceholder(identifier);
    }

    private static final class Fixture {
        final UUID uuid = UUID.fromString("f2a01ff9-0787-4c22-a693-3e2c80354c65");
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final OfflinePlayer offline = mock(OfflinePlayer.class);
        final Player player = mock(Player.class);
        final VoteSite site = mock(VoteSite.class);
        final BukkitScheduler scheduler = mock(BukkitScheduler.class);
        final PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
        final ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        final Deque<Runnable> pending = new ArrayDeque<>();
        final PlaceHolders placeholders;

        Fixture(PlaceholderCacheLevel level, String... configured) {
            when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(level);
            when(plugin.getConfigFile().getCachedPlaceholders()).thenReturn(new ArrayList<>(List.of(configured)));
            when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
            when(plugin.getTimer()).thenReturn(timer);
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            when(site.getKey()).thenReturn("example");
            when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(List.of(site)));
            when(plugin.isEnabled()).thenReturn(true);
            doAnswer(call -> { pending.addLast(call.getArgument(0)); return null; })
                    .when(timer).execute(any(Runnable.class));
            when(offline.getUniqueId()).thenReturn(uuid);
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(offline)).thenReturn(user);
            when(user.getJavaUUID()).thenReturn(uuid);
            when(user.getUUID()).thenReturn(uuid.toString());
            when(user.getPointsPath()).thenReturn("Points");
            when(user.getTotal(TopVoter.Monthly)).thenReturn(42);
            when(user.getPoints()).thenReturn(17);
            when(user.getTime(site)).thenReturn(1234L);
            when(user.voteCommandNextInfo(site, 1234L)).thenReturn("wait");
            doThrow(new AssertionError("worker read live player state")).when(user).isOnline();
            doThrow(new AssertionError("worker requested live player")).when(user).getPlayer();
            placeholders = new PlaceHolders(plugin);
            when(plugin.getPlaceholders()).thenReturn(placeholders);
            VotingPluginMain.plugin = plugin;
            placeholders.load();
        }

        String request(String identifier) {
            try (var bukkit = mockStatic(Bukkit.class)) {
                bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
                return placeholders.getPlaceHolder(offline, identifier, false);
            }
        }

        void drain() {
            int budget = 20;
            while (!pending.isEmpty()) {
                assertTrue(budget-- > 0, "callback queue must terminate");
                pending.removeFirst().run();
            }
        }

        PlaceHolder<VotingPluginUser> placeholder(String identifier) {
            return placeholders.getPlaceholders().stream().filter(p -> p.matches(identifier)).findFirst().orElseThrow();
        }
    }
}
