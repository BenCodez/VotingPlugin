package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.webhook.VotingPluginWebhookManager;

class VotingPluginMainShutdownTest {
    @Test
    void lateVoteTimerIsOptionalDuringPartialStartup() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        invokeShutdownVoteTimer(plugin);
        assertNull(field(plugin, "voteTimer"));
    }

    @Test
    void voteTimerCleanupIsRepeatable() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        ScheduledExecutorService timer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor();
        setField(plugin, "voteTimer", timer);
        invokeShutdownVoteTimer(plugin);
        assertDoesNotThrow(() -> invokeShutdownVoteTimer(plugin));
        org.junit.jupiter.api.Assertions.assertSame(timer, field(plugin, "voteTimer"));
        org.junit.jupiter.api.Assertions.assertTrue(timer.isShutdown());
    }

    @Test
    void publicUnloadSkipsLateOptionalResourcesAndIsIdempotent() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "plugin", plugin);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);

        assertDoesNotThrow(plugin::onUnLoad);
        assertDoesNotThrow(plugin::onUnLoad);
        assertNull(field(plugin, "signs"));
        assertNull(field(plugin, "timeQueueHandler"));
        verify((java.util.concurrent.ExecutorService) field(plugin, "backendHostedControlLifecycle")).shutdownNow();
        verify((java.util.concurrent.ExecutorService) field(plugin, "backendControlConnectorLifecycle")).shutdownNow();
    }

    @Test
    void stoppedTimerRemainsVisibleUntilOverflowAndProducerHandlersClose() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        var timer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor();
        var queue = mock(com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.class);
        setField(plugin, "voteTimer", timer);
        setField(plugin, "votifierVoteOverflowQueue", queue);
        var duringClose = new java.util.concurrent.atomic.AtomicReference<ScheduledExecutorService>();
        var rejected = new java.util.concurrent.atomic.AtomicBoolean();
        var ran = new java.util.concurrent.atomic.AtomicBoolean();
        org.mockito.Mockito.doAnswer(call -> {
            duringClose.set(plugin.getVoteTimer());
            rejected.set(!com.bencodez.votingplugin.util.VoteTaskAdmission.trySubmit(plugin.getVoteTimer(),
                    () -> ran.set(true)));
            return null;
        }).when(queue).close();
        try (var handlers = org.mockito.Mockito.mockStatic(org.bukkit.event.HandlerList.class)) {
            plugin.onUnLoad();
            org.junit.jupiter.api.Assertions.assertSame(timer, duringClose.get());
            org.junit.jupiter.api.Assertions.assertTrue(rejected.get());
            org.junit.jupiter.api.Assertions.assertFalse(ran.get());
            handlers.verify(() -> org.bukkit.event.HandlerList.unregisterAll(plugin));
            assertNull(plugin.getVoteTimer());
        } finally { timer.shutdownNow(); }
    }

    @Test
    void publicUnloadCleansInitializedQueueAndSignResources() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "plugin", plugin);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        Signs signs = mock(Signs.class);
        TimeQueueHandler timeQueue = mock(TimeQueueHandler.class);
        setField(plugin, "signs", signs);
        setField(plugin, "timeQueueHandler", timeQueue);

        plugin.onUnLoad();

        verify(signs).storeSigns();
        verify(timeQueue).saveForShutdown();
        plugin.onUnLoad();
        verify(signs).storeSigns();
    }

    @Test
    void failedCleanupDoesNotSkipLaterResources() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "plugin", plugin);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        Signs signs = mock(Signs.class);
        TimeQueueHandler timeQueue = mock(TimeQueueHandler.class);
        VotingPluginWebhookManager webhook = mock(VotingPluginWebhookManager.class);
        doThrow(new IllegalStateException("webhook close failed")).when(webhook).shutdown();
        doThrow(new IllegalStateException("queue save failed")).when(timeQueue).saveForShutdown();
        setField(plugin, "webhookManager", webhook);
        setField(plugin, "signs", signs);
        setField(plugin, "timeQueueHandler", timeQueue);

        assertDoesNotThrow(plugin::onUnLoad);

        verify(webhook).shutdown();
        verify(timeQueue).saveForShutdown();
        verify(signs).storeSigns();
    }

    @Test
    void repeatedUnloadRetriesOnlyFailedTimedVotePersistence() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        TimeQueueHandler timeQueue = mock(TimeQueueHandler.class);
        Signs signs = mock(Signs.class);
        org.mockito.Mockito.doThrow(new IllegalStateException("disk unavailable")).doNothing()
                .when(timeQueue).saveForShutdown();
        setField(plugin, "timeQueueHandler", timeQueue);
        setField(plugin, "signs", signs);
        assertDoesNotThrow(plugin::onUnLoad);
        assertDoesNotThrow(plugin::onUnLoad);
        verify(timeQueue, org.mockito.Mockito.times(2)).saveForShutdown();
        verify(signs).storeSigns();
    }

    @Test
    void gracefulTimerShutdownDoesNotForceStop() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        org.mockito.Mockito.when(timer.awaitTermination(1, java.util.concurrent.TimeUnit.SECONDS)).thenReturn(true);
        setField(plugin, "voteTimer", timer);

        invokeShutdownVoteTimer(plugin);

        verify(timer).shutdown();
        verify(timer, never()).shutdownNow();
    }

    @Test
    void timerTimeoutForcesStop() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        org.mockito.Mockito.when(timer.awaitTermination(1, java.util.concurrent.TimeUnit.SECONDS)).thenReturn(false);
        setField(plugin, "voteTimer", timer);

        invokeShutdownVoteTimer(plugin);

        verify(timer).shutdownNow();
    }

    @Test
    void interruptedTimerShutdownRestoresInterruptAndForcesStop() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        doThrow(new InterruptedException()).when(timer).awaitTermination(1, java.util.concurrent.TimeUnit.SECONDS);
        setField(plugin, "voteTimer", timer);

        invokeShutdownVoteTimer(plugin);

        try {
            org.junit.jupiter.api.Assertions.assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
        verify(timer).shutdownNow();
    }

    @Test
    void failedForcedTimerStopStillClosesInitializedQueuesAndListenersOnce() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        setField(plugin, "plugin", plugin);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        doThrow(new IllegalStateException("controlled forced-stop failure")).when(timer).shutdownNow();
        setField(plugin, "voteTimer", timer);
        var queue = mock(com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.class);
        var cooldown = mock(com.bencodez.votingplugin.cooldown.CoolDownCheck.class);
        var reminders = mock(com.bencodez.votingplugin.votereminding.VoteRemindersManager.class);
        setField(plugin, "votifierVoteOverflowQueue", queue);
        setField(plugin, "coolDownCheck", cooldown);
        setField(plugin, "voteRemindersManager", reminders);
        try (var handlers = org.mockito.Mockito.mockStatic(org.bukkit.event.HandlerList.class)) {
            assertDoesNotThrow(plugin::onUnLoad);
            assertDoesNotThrow(plugin::onUnLoad);
            verify(queue).close();
            verify(cooldown).shutdown();
            verify(reminders).shutdown();
            handlers.verify(() -> org.bukkit.event.HandlerList.unregisterAll(plugin));
        }
        assertNull(field(plugin, "voteTimer"));
        assertNull(field(plugin, "votifierVoteOverflowQueue"));
    }

    @Test
    void rejectedMysqlStartupRetainsItsCauseAndFullDisableCleansEarlyResources() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        var options = mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class,
                org.mockito.Mockito.RETURNS_DEEP_STUBS);
        org.mockito.Mockito.doReturn(options).when(plugin).getOptions();
        org.mockito.Mockito.doReturn(java.util.logging.Logger.getLogger("startup-fixture")).when(plugin).getLogger();
        org.mockito.Mockito.when(options.getDebug().isDebug()).thenReturn(true);
        setField(plugin, "plugin", plugin);
        setField(plugin, "unloadCompleted", new java.util.concurrent.atomic.AtomicBoolean());
        setField(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        initializeEarlyLifecycle(plugin);
        var logger = mock(java.util.logging.Logger.class);
        org.mockito.Mockito.doReturn(logger).when(plugin).getLogger();
        ScheduledExecutorService coreTimer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor();
        setField(plugin, "timer", coreTimer);
        setField(plugin, "loadUserData", true);
        var configuration = new org.bukkit.configuration.file.YamlConfiguration();
        configuration.set("Host", "127.0.0.1"); configuration.set("Port", 3306);
        configuration.set("Username", "fixture"); configuration.set("Password", "fixture");
        configuration.set("Database", "fixture"); configuration.set("MaxConnections", 1);
        configuration.set("ConnectionTimeout", 1000); configuration.set("MinimumIdle", 0);
        configuration.set("Driver", RejectingDriver.class.getName());
        configuration.set("Debug", true);
        try {
            IllegalStateException failure = org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
                    () -> new com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL(plugin, "Users", configuration));
            Throwable original = failure;
            while (original.getCause() != null) original = original.getCause();
            org.junit.jupiter.api.Assertions.assertInstanceOf(java.sql.SQLException.class, original);
            org.junit.jupiter.api.Assertions.assertEquals("controlled authentication rejection", original.getMessage());
            assertNull(plugin.getMysql(), "rejected constructor must not publish storage");
            assertDoesNotThrow(plugin::onDisable);
            org.junit.jupiter.api.Assertions.assertTrue(coreTimer.isTerminated());
            assertNull(field(plugin, "voteTimer"));
            assertNull(field(plugin, "signs"));
            assertDoesNotThrow(plugin::onDisable);
            org.junit.jupiter.api.Assertions.assertSame(original, rootCause(failure));
            verify(logger, never()).warning(org.mockito.ArgumentMatchers.anyString());
            verify(logger, never()).log(org.mockito.ArgumentMatchers.eq(java.util.logging.Level.WARNING),
                    org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any(Throwable.class));
        } finally { coreTimer.shutdownNow(); }
    }

    private static Throwable rootCause(Throwable failure) {
        while (failure.getCause() != null) failure = failure.getCause();
        return failure;
    }

    public static final class RejectingDriver implements java.sql.Driver {
        public java.sql.Connection connect(String url, java.util.Properties properties) throws java.sql.SQLException {
            throw new java.sql.SQLException("controlled authentication rejection", "28000", 1045);
        }
        public boolean acceptsURL(String url) { return url != null && url.startsWith("jdbc:"); }
        public java.sql.DriverPropertyInfo[] getPropertyInfo(String url, java.util.Properties properties) {
            return new java.sql.DriverPropertyInfo[0];
        }
        public int getMajorVersion() { return 1; }
        public int getMinorVersion() { return 0; }
        public boolean jdbcCompliant() { return false; }
        public java.util.logging.Logger getParentLogger() { return java.util.logging.Logger.getGlobal(); }
    }

    private static void initializeEarlyLifecycle(VotingPluginMain plugin) throws Exception {
        var hosted = mock(java.util.concurrent.ExecutorService.class);
        var connector = mock(ScheduledExecutorService.class);
        org.mockito.Mockito.when(hosted.awaitTermination(10, java.util.concurrent.TimeUnit.SECONDS)).thenReturn(true);
        org.mockito.Mockito.when(connector.awaitTermination(10, java.util.concurrent.TimeUnit.SECONDS)).thenReturn(true);
        setField(plugin, "pendingNativeUserStorageCloseLock", new Object());
        setField(plugin, "pendingNativeUserStorageCloses", new java.util.ArrayList<>());
        setField(plugin, "backendHostedControlLifecycle", hosted);
        setField(plugin, "backendControlConnectorLifecycle", connector);
        setField(plugin, "backendHostedControlManagers", java.util.concurrent.ConcurrentHashMap.newKeySet());
        org.mockito.Mockito.doReturn(java.util.logging.Logger.getLogger("shutdown-fixture")).when(plugin).getLogger();
    }

    private static void invokeShutdownVoteTimer(VotingPluginMain plugin) throws Exception {
        Method method = VotingPluginMain.class.getDeclaredMethod("shutdownVoteTimer");
        method.setAccessible(true);
        method.invoke(plugin);
    }

    private static Object field(VotingPluginMain plugin, String name) throws Exception {
        Field field = VotingPluginMain.class.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(plugin);
    }

    private static void setField(VotingPluginMain plugin, String name, Object value) throws Exception {
        Field field = findField(name);
        field.setAccessible(true);
        field.set(plugin, value);
    }

    private static Field findField(String name) throws Exception {
        Class<?> type = VotingPluginMain.class;
        while (type != null) {
            try { return type.getDeclaredField(name); }
            catch (NoSuchFieldException ignored) { type = type.getSuperclass(); }
        }
        throw new NoSuchFieldException(name);
    }
}
