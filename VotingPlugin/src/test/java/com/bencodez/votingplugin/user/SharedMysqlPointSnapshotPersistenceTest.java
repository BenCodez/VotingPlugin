package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;

/** Validates the JDBC-to-snapshot publication seam using SQLite, not MySQL locking semantics. */
class SharedMysqlPointSnapshotPersistenceTest {
    @Test
    void committedJdbcBalanceIsPublishedToWorkerAndMainReadsAndSurvivesReconnect() throws Exception {
        Path file = Files.createTempFile("vp-points-snapshot-", ".db");
        String url = "jdbc:sqlite:" + file;
        UUID uuid = UUID.randomUUID();
        try {
            try (Connection connection = DriverManager.getConnection(url)) {
                connection.createStatement().executeUpdate("CREATE TABLE points (uuid TEXT PRIMARY KEY, balance INTEGER NOT NULL)");
                try (PreparedStatement insert = connection.prepareStatement("INSERT INTO points VALUES (?, ? )")) {
                    insert.setString(1, uuid.toString());
                    insert.setInt(2, 38);
                    insert.executeUpdate();
                }
            }

            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            UserDataManager manager = mock(UserDataManager.class);
            when(plugin.getUserManager().getDataManager()).thenReturn(manager);
            when(manager.mustDeferSharedStorageAccess()).thenAnswer(call -> Thread.currentThread().getName().equals("main"));
            AdvancedCoreUser user = mock(AdvancedCoreUser.class);
            when(user.getPlugin()).thenReturn(plugin);
            UserDataCache cache = new UserDataCache(null, uuid);
            cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(38))));
            when(user.getCache()).thenReturn(cache);
            when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(Map.of(uuid, cache)));
            UserData data = new UserData(user);
            assertEquals(38, data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT));

            try (Connection connection = DriverManager.getConnection(url);
                    PreparedStatement update = connection.prepareStatement("UPDATE points SET balance = ? WHERE uuid = ?")) {
                connection.setAutoCommit(false);
                update.setInt(1, 41);
                update.setString(2, uuid.toString());
                assertEquals(1, update.executeUpdate());
                connection.commit();
            }
            cache.invalidateStorageSnapshot("Points");
            assertThrows(IllegalStateException.class,
                    () -> data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT));

            doAnswer(invocation -> {
                long expectedVersion = cache.getSharedSnapshotVersion();
                int balance;
                try (Connection connection = DriverManager.getConnection(url);
                        PreparedStatement select = connection.prepareStatement("SELECT balance FROM points WHERE uuid = ?")) {
                    select.setString(1, uuid.toString());
                    try (var result = select.executeQuery()) {
                        result.next();
                        balance = result.getInt(1);
                    }
                }
                HashMap<String, DataValue> values = new HashMap<>(Map.of("Points", new DataValueInt(balance)));
                cache.updateSharedSnapshot(values, expectedVersion);
                return null;
            }).when(manager).cacheUser(uuid, null);
            try (var workers = java.util.concurrent.Executors.newFixedThreadPool(2)) {
                assertEquals(41, workers.submit(() -> {
                    SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(plugin, uuid.toString(), "Points");
                    return data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT);
                }).get());
                java.util.List<java.util.concurrent.Future<?>> changes = new java.util.ArrayList<>();
                for (int i = 0; i < 2; i++) {
                    changes.add(workers.submit(() -> {
                        try (Connection connection = DriverManager.getConnection(url);
                                PreparedStatement update = connection.prepareStatement("UPDATE points SET balance = balance + 1 WHERE uuid = ?")) {
                            update.setString(1, uuid.toString());
                            assertEquals(1, update.executeUpdate());
                        } catch (java.sql.SQLException failure) { throw new RuntimeException(failure); }
                        SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(plugin, uuid.toString(), "Points");
                    }));
                }
                for (var change : changes) change.get();
                assertEquals(43, workers.submit(() -> data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT)).get());
            }
            assertEquals(43, data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT));

            // Recreate the storage connection and cache after a restart boundary.
            try (Connection connection = DriverManager.getConnection(url);
                    PreparedStatement select = connection.prepareStatement("SELECT balance FROM points WHERE uuid = ?")) {
                select.setString(1, uuid.toString());
                try (var result = select.executeQuery()) {
                    result.next();
                    assertEquals(43, result.getInt(1));
                }
            }
            UserDataCache restartedCache = new UserDataCache(null, uuid);
            when(manager.getUserDataCache()).thenReturn(new java.util.concurrent.ConcurrentHashMap<>(Map.of(uuid, restartedCache)));
            doAnswer(invocation -> {
                long expectedVersion = restartedCache.getSharedSnapshotVersion();
                try (Connection connection = DriverManager.getConnection(url);
                        var select = connection.prepareStatement("SELECT balance FROM points WHERE uuid = ?")) {
                    select.setString(1, uuid.toString());
                    try (var row = select.executeQuery()) {
                        row.next();
                        restartedCache.updateSharedSnapshot(new HashMap<>(Map.of("Points", new DataValueInt(row.getInt(1)))), expectedVersion);
                    }
                }
                return null;
            }).when(manager).cacheUser(uuid, null);
            SharedMysqlCacheReconciler.invalidateAndRefreshOnWorker(plugin, uuid.toString(), "Points");
            when(user.getCache()).thenReturn(restartedCache);
            assertEquals(43, data.getInt(UserStorage.MYSQL, "Points", 0, UserDataFetchMode.DEFAULT));
        } finally {
            Files.deleteIfExists(file);
        }
    }
}
