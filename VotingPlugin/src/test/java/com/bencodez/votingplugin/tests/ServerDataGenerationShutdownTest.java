package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

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

    private static void initializePlugin(VotingPluginMain plugin) throws Exception {
        // Constructor-bypassing Mockito objects need the real SDK lifecycle lock.
        var lock = com.bencodez.advancedcore.AdvancedCorePlugin.class.getDeclaredField("lifecycleLock");
        lock.setAccessible(true);
        lock.set(plugin, new Object());
        doNothing().when(plugin).registerEvents(any());
    }

    private static org.mockito.MockedConstruction<AdvancedCoreLifecycle> completedCleanup() {
        // Only platform cleanup is controlled. Actual plugin admission, timed
        // publication and generation/writer reservations remain under test.
        return mockConstruction(AdvancedCoreLifecycle.class, (cleanup, context) -> {
            var plugin = (VotingPluginMain) context.arguments().get(0);
            doAnswer(call -> {
                for (var writer : plugin.getServerDataWriterExecutors())
                    if (writer != null) writer.shutdown();
                var completed = CompletableFuture.<Void>completedFuture(null);
                plugin.onShutdownComplete(completed);
                plugin.releaseServerDataOwnershipAfter(completed);
                return null;
            }).when(cleanup).shutdown(anyLong());
        });
    }

    private static ScheduledExecutorService terminatedExecutor() {
        var executor = mock(ScheduledExecutorService.class);
        when(executor.isTerminated()).thenReturn(true);
        return executor;
    }

    @Test void failedShutdownSuffixKeepsItsGenerationUntilExplicitRetryPersistsEveryVote() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        initializePlugin(plugin);
        doReturn(mock(Logger.class)).when(plugin).getLogger();
        Path target = directory.resolve("ServerData.yml");
        var fence = com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target);
        var field = VotingPluginMain.class.getDeclaredField("serverDataPublicationFence");
        field.setAccessible(true); field.set(plugin, fence);
        var yaml = spy(new YamlConfiguration());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
        when(file.getData()).thenReturn(yaml); when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(plugin).getServerDataFile();
        doReturn(new ServerData(plugin)).when(plugin).getServerData();
        doReturn(new ScheduledExecutorService[0]).when(plugin).getServerDataWriterExecutors();
        var producer = new ScheduledThreadPoolExecutor(1);
        producer.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        doReturn(producer).when(plugin).getVoteTimer();
        try {
            var handler = new TimeQueueHandler(plugin);
            handler.addVote("First", "site");
            handler.addVote("Second", "site");
            handler.addVote("Third", "site");
            producer.shutdown(); assertTrue(producer.awaitTermination(5, TimeUnit.SECONDS));
            var writes = new java.util.concurrent.atomic.AtomicInteger();
            doAnswer(call -> {
                int attempt = writes.incrementAndGet();
                if (attempt == 2 || attempt == 3) throw new java.io.IOException("synthetic suffix failure");
                return call.callRealMethod();
            }).when(yaml).save(target.toFile());
            assertThrows(java.io.UncheckedIOException.class, handler::saveForShutdown);
            fence.retire();
            assertEquals(2, handler.getTimeChangeQueue().size());
            assertThrows(IllegalStateException.class, () ->
                    com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target));
            assertThrows(java.io.UncheckedIOException.class, handler::saveForShutdown);
            assertEquals(2, handler.getTimeChangeQueue().size());
            assertThrows(IllegalStateException.class, () ->
                    com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target));
            handler.saveForShutdown();
            assertTrue(handler.getTimeChangeQueue().isEmpty());
            var recovered = YamlConfiguration.loadConfiguration(target.toFile());
            assertEquals("First", recovered.getString("VotingPlugin.TimedVoteCache.0.Name"));
            assertEquals("Second", recovered.getString("VotingPlugin.TimedVoteCache.1.Name"));
            assertEquals("Third", recovered.getString("VotingPlugin.TimedVoteCache.2.Name"));
            com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target).retire();
        } finally {
            producer.shutdownNow(); assertTrue(producer.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void rejectedShutdownWorkerRetryRetainsTheOriginalProducerAfterUnloadClearsItsField() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        initializePlugin(plugin);
        doReturn(mock(Logger.class)).when(plugin).getLogger();
        Path target = directory.resolve("ServerData.yml");
        var fence = com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target);
        var field = VotingPluginMain.class.getDeclaredField("serverDataPublicationFence");
        field.setAccessible(true); field.set(plugin, fence);
        var yaml = spy(new YamlConfiguration());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
        when(file.getData()).thenReturn(yaml); when(file.getdFile()).thenReturn(target.toFile());
        doReturn(file).when(plugin).getServerDataFile();
        doReturn(new ServerData(plugin)).when(plugin).getServerData();
        var releaseCore = new CountDownLatch(1);
        var coreWriter = mock(ScheduledExecutorService.class);
        when(coreWriter.isTerminated()).thenAnswer(call -> releaseCore.getCount() == 0);
        when(coreWriter.awaitTermination(anyLong(), any())).thenAnswer(call ->
                releaseCore.await(call.getArgument(0), call.getArgument(1)));
        var capturedWriters = new ScheduledExecutorService[] { coreWriter };
        doReturn(capturedWriters).when(plugin).getServerDataWriterExecutors();
        var producer = new ScheduledThreadPoolExecutor(1);
        producer.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        doReturn(producer).when(plugin).getVoteTimer();
        var failWrites = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(call -> {
            if (failWrites.get()) throw new java.io.IOException("synthetic late suffix failure");
            return call.callRealMethod();
        }).when(yaml).save(target.toFile());
        var handler = new TimeQueueHandler(plugin) {
            private boolean reject = true;
            @Override protected void startShutdownWorker(Runnable task) {
                if (reject) { reject = false; throw new RejectedExecutionException("synthetic admission failure"); }
                super.startShutdownWorker(task);
            }
        };
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        var lateFailure = new java.util.concurrent.atomic.AtomicReference<Throwable>();
        try {
            producer.submit(() -> {
                entered.countDown();
                boolean interrupted = false;
                while (true) {
                    try { if (release.await(10, TimeUnit.SECONDS)) break; throw new AssertionError("release timeout"); }
                    catch (InterruptedException ignored) { interrupted = true; }
                }
                try { handler.addVote("Late", "site"); }
                catch (Throwable failure) { lateFailure.set(failure); }
                finally { if (interrupted) Thread.currentThread().interrupt(); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            producer.shutdownNow();
            assertThrows(RejectedExecutionException.class, handler::saveForShutdown);
            fence.retire();
            // The actual unload path clears this accessor even if shutdownNow did not terminate work.
            doReturn(null).when(plugin).getVoteTimer();
            capturedWriters[0] = null;
            handler.saveForShutdown();
            assertThrows(IllegalStateException.class, () ->
                    com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target).retire());
            release.countDown(); assertTrue(producer.awaitTermination(5, TimeUnit.SECONDS));
            assertInstanceOf(java.io.UncheckedIOException.class, lateFailure.get());
            // The original core writer is also retained, independent of the caller's mutable array.
            assertThrows(IllegalStateException.class, () ->
                    com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target).retire());
            releaseCore.countDown();
            assertThrows(java.io.UncheckedIOException.class, handler::saveForShutdown);
            assertEquals(1, handler.getTimeChangeQueue().size());
            assertThrows(IllegalStateException.class, () ->
                    com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target));
            failWrites.set(false); handler.saveForShutdown();
            assertTrue(handler.getTimeChangeQueue().isEmpty());
            assertEquals("Late", YamlConfiguration.loadConfiguration(target.toFile())
                    .getString("VotingPlugin.TimedVoteCache.0.Name"));
            com.bencodez.votingplugin.util.ServerDataPublicationFence.beginGeneration(target).retire();
        } finally {
            release.countDown(); releaseCore.countDown(); producer.shutdownNow();
            assertTrue(producer.awaitTermination(5, TimeUnit.SECONDS));
            failWrites.set(false);
            // Do not start a new retry from cleanup or hide the failed ownership assertion.
            fence.retire();
        }
    }

    @Test void failedStartupWithoutATimedQueueKeepsCoreWritesFencedThroughActualDisable() throws Exception {
        VotingPluginMain old = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        VotingPluginMain next = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        for (var plugin : java.util.List.of(old, next)) {
            initializePlugin(plugin);
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
                var controlledCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            var runningReference = new java.util.concurrent.atomic.AtomicReference<Future<?>>();
            doAnswer(startup -> {
                runningReference.set(writer.submit(() -> {
                    entered.countDown();
                    try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                    catch (InterruptedException failure) { throw new AssertionError(failure); }
                }));
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                throw new StartupReached();
            }).when(old).onPreLoad();
            assertThrows(StartupReached.class, old::onEnable);
            assertFalse(assertThrows(IllegalStateException.class, next::onEnable) instanceof StartupReached);
            Future<?> running = runningReference.get();
            assertNotNull(running);
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
            initializePlugin(plugin);
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = spy(new YamlConfiguration());
        yaml.set("Adjacent.Value", "old");
        yaml.save(target.toFile());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
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
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var controlledCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
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
            initializePlugin(plugin);
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = spy(new YamlConfiguration());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
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
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var controlledCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
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
            initializePlugin(plugin);
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
            doThrow(new StartupReached()).when(plugin).onPreLoad();
        }
        var yaml = new YamlConfiguration();
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
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
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var controlledCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
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
            initializePlugin(plugin);
            doReturn(directory.toFile()).when(plugin).getDataFolder();
            doReturn(mock(Logger.class)).when(plugin).getLogger();
        }
        YamlConfiguration yaml = spy(new YamlConfiguration());
        yaml.set("Adjacent.Value", "old");
        yaml.save(target.toFile());
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(file.publicationMonitor()).thenCallRealMethod();
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
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var controlledCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
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
