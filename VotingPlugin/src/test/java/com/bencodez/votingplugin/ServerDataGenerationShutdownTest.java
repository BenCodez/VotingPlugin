package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.concurrent.*;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.lifecycle.AdvancedCoreLifecycle;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;

class ServerDataGenerationShutdownTest {
    @TempDir Path directory;
    private static final class StartupReached extends IllegalStateException { }

    private static ScheduledExecutorService terminatedExecutor() {
        var executor = mock(ScheduledExecutorService.class);
        when(executor.isTerminated()).thenReturn(true);
        return executor;
    }

    @Test void failedStartupWithoutATimedQueueKeepsCoreWritesFencedThroughActualDisable() throws Exception {
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (var plugin : java.util.List.of(old, next)) {
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        ScheduledThreadPoolExecutor writer = new ScheduledThreadPoolExecutor(1);
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                writer, terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var shutdown = mockConstruction(AdvancedCoreLifecycle.class, (mock, context) -> {
                    doAnswer(call -> { writer.shutdown(); return null; }).when(mock).shutdown();
                })) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            assertFalse(assertThrows(IllegalStateException.class, next::onEnable) instanceof StartupReached);
            Future<?> running = writer.submit(() -> {
                entered.countDown();
                try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                catch (InterruptedException failure) { throw new AssertionError(failure); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            old.onDisable();
            assertFalse(assertThrows(IllegalStateException.class, next::onEnable) instanceof StartupReached);
            var completion = old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors());
            assertFalse(completion.toCompletableFuture().isDone());
            release.countDown(); running.get(5, TimeUnit.SECONDS);
            assertTrue(writer.awaitTermination(5, TimeUnit.SECONDS));
            completion.toCompletableFuture().get(5, TimeUnit.SECONDS);
            assertThrows(StartupReached.class, next::onEnable);
            next.getServerDataPublicationFence().retire();
        } finally { release.countDown(); writer.shutdownNow(); assertTrue(writer.awaitTermination(5, TimeUnit.SECONDS)); }
    }

    @Test void heartbeatWriterRemainsFencedAfterEmptyTimedSnapshot() throws Exception { checkCoreWriter("heartbeat"); }
    @Test void loginWriterRemainsFencedAfterEmptyTimedSnapshot() throws Exception { checkCoreWriter("login"); }
    @Test void inventoryWriterRemainsFencedAfterEmptyTimedSnapshot() throws Exception { checkCoreWriter("inventory"); }
    @Test void timeCheckerWriterRemainsFencedAfterEmptyTimedSnapshot() throws Exception { checkCoreWriter("timechecker"); }

    private void checkCoreWriter(String writerKind) throws Exception {
        Path target = directory.resolve("ServerData.yml");
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (var plugin : java.util.List.of(old, next)) {
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = spy(new YamlConfiguration());
        yaml.set("Adjacent.Value", "old");
        yaml.save(target.toFile());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.getData()).thenReturn(yaml);
        when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(old).getServerDataFile();
        doReturn(new ServerData(old)).when(old).getServerData();
        doReturn(terminatedExecutor()).when(old).getVoteTimer();
        ScheduledThreadPoolExecutor writer = new ScheduledThreadPoolExecutor(1);
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retire();
            switch (writerKind) {
                case "heartbeat" -> doReturn(writer).when(old).getTimer();
                case "login" -> doReturn(writer).when(old).getLoginTimer();
                case "inventory" -> doReturn(writer).when(old).getInventoryTimer();
                case "timechecker" -> {
                    var checker = mock(com.bencodez.advancedcore.api.time.TimeChecker.class);
                    doReturn(checker).when(old).getTimeChecker();
                    when(checker.getTimer()).thenReturn(writer);
                }
                default -> throw new AssertionError(writerKind);
            }
            TimeQueueHandler handler = new TimeQueueHandler(old);
            doAnswer(call -> {
                entered.countDown(); assertTrue(release.await(10, TimeUnit.SECONDS));
                return call.callRealMethod();
            }).when(yaml).save(target.toFile());
            Future<?> writing = writer.submit(() -> {
                try { file.getData().save(file.getdFile()); }
                catch (java.io.IOException failure) { throw new java.io.UncheckedIOException(failure); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            caller.submit(handler::saveForShutdown).get(2, TimeUnit.SECONDS);
            IllegalStateException blocked = assertThrows(IllegalStateException.class, next::onEnable);
            assertFalse(blocked instanceof StartupReached);
            verify(next, never()).onPreLoad();
            writer.shutdown(); release.countDown(); writing.get(5, TimeUnit.SECONDS);
            assertTrue(writer.awaitTermination(5, TimeUnit.SECONDS));
            handler.saveForShutdown();
            assertThrows(StartupReached.class, next::onEnable);
        } finally {
            release.countDown(); writer.shutdownNow(); caller.shutdownNow();
            assertTrue(writer.awaitTermination(5, TimeUnit.SECONDS));
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void initialShutdownWriteFailureStillRetainsTheRunningProducerReservation() throws Exception {
        Path target = directory.resolve("ServerData.yml");
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (var plugin : java.util.List.of(old, next)) {
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = spy(new YamlConfiguration());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.getData()).thenReturn(yaml); when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(old).getServerDataFile();
        doReturn(new ServerData(old)).when(old).getServerData();
        ScheduledThreadPoolExecutor producers = new ScheduledThreadPoolExecutor(1);
        producers.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        doReturn(producers).when(old).getVoteTimer();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retire();
            TimeQueueHandler handler = new TimeQueueHandler(old);
            handler.addVote("First", "site");
            var late = producers.submit(() -> {
                entered.countDown();
                try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                catch (InterruptedException failure) { throw new AssertionError(failure); }
                handler.addVote("Late", "site");
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS)); producers.shutdown();
            doThrow(new java.io.IOException("synthetic initial write failure")).doCallRealMethod()
                    .when(yaml).save(target.toFile());
            caller.submit(handler::saveForShutdown).get(2, TimeUnit.SECONDS);
            IllegalStateException blocked = assertThrows(IllegalStateException.class, next::onEnable);
            assertFalse(blocked instanceof StartupReached);
            release.countDown(); late.get(5, TimeUnit.SECONDS);
            assertTrue(producers.awaitTermination(5, TimeUnit.SECONDS));
            try { handler.saveForShutdown(); } catch (java.io.UncheckedIOException observedInitialFailure) { }
            handler.saveForShutdown();
            assertThrows(StartupReached.class, next::onEnable);
            assertEquals("First", YamlConfiguration.loadConfiguration(target.toFile()).getString("VotingPlugin.TimedVoteCache.0.Name"));
            assertEquals("Late", YamlConfiguration.loadConfiguration(target.toFile()).getString("VotingPlugin.TimedVoteCache.1.Name"));
        } finally {
            release.countDown(); producers.shutdownNow(); caller.shutdownNow();
            assertTrue(producers.awaitTermination(5, TimeUnit.SECONDS));
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void emptyShutdownSnapshotStillOwnsLateProducersUntilTheyFinish() throws Exception {
        Path target = directory.resolve("ServerData.yml");
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (var plugin : java.util.List.of(old, next)) {
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = new YamlConfiguration();
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.getData()).thenReturn(yaml);
        when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(old).getServerDataFile();
        var data = new ServerData(old);
        doReturn(data).when(old).getServerData();
        ScheduledThreadPoolExecutor producers = new ScheduledThreadPoolExecutor(1);
        producers.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        doReturn(producers).when(old).getVoteTimer();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retire();
            TimeQueueHandler handler = new TimeQueueHandler(old);
            Future<?> late = producers.submit(() -> {
                entered.countDown();
                try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                catch (InterruptedException failure) { throw new AssertionError(failure); }
                handler.addVote("Late", "site");
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            producers.shutdown();
            caller.submit(handler::saveForShutdown).get(2, TimeUnit.SECONDS);
            IllegalStateException blocked = assertThrows(IllegalStateException.class, next::onEnable);
            assertFalse(blocked instanceof StartupReached);
            verify(next, never()).onPreLoad();
            release.countDown();
            late.get(5, TimeUnit.SECONDS);
            assertTrue(producers.awaitTermination(5, TimeUnit.SECONDS));
            handler.saveForShutdown();
            assertThrows(StartupReached.class, next::onEnable);
            assertEquals("Late", YamlConfiguration.loadConfiguration(target.toFile())
                    .getString("VotingPlugin.TimedVoteCache.0.Name"));
        } finally {
            release.countDown(); producers.shutdownNow(); caller.shutdownNow();
            assertTrue(producers.awaitTermination(5, TimeUnit.SECONDS));
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void replacementCannotReadOrWriteServerDataWhilePreviousShutdownPublicationIsBlocked() throws Exception {
        Path target = directory.resolve("ServerData.yml");
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (VotingPluginMain plugin : java.util.List.of(old, next)) {
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
        }
        YamlConfiguration yaml = spy(new YamlConfiguration());
        yaml.set("Adjacent.Value", "old");
        yaml.save(target.toFile());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.getData()).thenReturn(yaml);
        when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(old).getServerDataFile();
        ServerData data = new ServerData(old);
        doReturn(data).when(old).getServerData();
        doReturn(terminatedExecutor()).when(old).getVoteTimer();
        doThrow(new StartupReached()).when(old).onPreLoad();
        doAnswer(call -> {
            YamlConfiguration replacement = YamlConfiguration.loadConfiguration(target.toFile());
            replacement.set("Adjacent.Value", "replacement");
            replacement.save(target.toFile());
            throw new StartupReached();
        }).when(next).onPreLoad();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(),
                terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retire();
            TimeQueueHandler handler = new TimeQueueHandler(old);
            handler.addVote("First", "site");
            doAnswer(call -> {
                entered.countDown();
                assertTrue(release.await(10, TimeUnit.SECONDS));
                return call.callRealMethod();
            }).when(yaml).save(target.toFile());
            Future<?> shutdown = caller.submit(handler::saveForShutdown);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            shutdown.get(2, TimeUnit.SECONDS);
            IllegalStateException blocked = assertThrows(IllegalStateException.class, next::onEnable);
            assertFalse(blocked instanceof StartupReached, "replacement must be fenced before any startup read/write");
            verify(next, never()).onPreLoad();
            release.countDown();
            handler.saveForShutdown();
            assertThrows(StartupReached.class, next::onEnable);
            assertThrows(IllegalStateException.class, () -> data.addTimeVoted(1,
                    new com.bencodez.votingplugin.timequeue.VoteTimeQueue("Stale", "site", 123L)));
            assertEquals("replacement", YamlConfiguration.loadConfiguration(target.toFile()).getString("Adjacent.Value"));
        } finally {
            release.countDown();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }
}
