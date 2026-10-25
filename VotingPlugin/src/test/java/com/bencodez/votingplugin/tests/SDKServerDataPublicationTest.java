package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.PluginDescriptionFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.data.ServerData;
import com.bencodez.advancedcore.lifecycle.AdvancedCoreLifecycle;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;

class SDKServerDataPublicationTest {
    @TempDir Path directory;
    private static final class StartupReached extends IllegalStateException { }

    private VotingPluginMain plugin() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        // Mockito bypasses constructors; use the SDK's actual lifecycle lock.
        var lock = AdvancedCorePlugin.class.getDeclaredField("lifecycleLock");
        lock.setAccessible(true);
        lock.set(plugin, new Object());
        doNothing().when(plugin).registerEvents(any());
        doReturn(directory.toFile()).when(plugin).getDataFolder();
        doReturn(mock(Logger.class)).when(plugin).getLogger();
        doReturn("VotingPlugin").when(plugin).getName();
        doReturn(new PluginDescriptionFile("VotingPlugin", "1", "synthetic.Main")).when(plugin).getDescription();
        doThrow(new StartupReached()).when(plugin).onPreLoad();
        return plugin;
    }

    private static org.mockito.MockedConstruction<AdvancedCoreLifecycle> completedCleanup() {
        // Control only unrelated platform cleanup. Real SDK startup admission,
        // file publishers and VotingPlugin generation ownership stay in use.
        // These fixtures do not claim full platform shutdown validation.
        return mockConstruction(AdvancedCoreLifecycle.class, (cleanup, context) -> {
            var plugin = (VotingPluginMain) context.arguments().get(0);
            doAnswer(call -> {
                var completed = CompletableFuture.<Void>completedFuture(null);
                plugin.onShutdownComplete(completed);
                plugin.releaseServerDataOwnershipAfter(completed);
                return null;
            }).when(cleanup).shutdown(anyLong());
        });
    }

    private static void awaitCapturedRetirement(VotingPluginMain plugin) throws Exception {
        plugin.getServerDataPublicationFence().retainUntilTermination(plugin.getServerDataWriterExecutors())
                .toCompletableFuture().get(5, TimeUnit.SECONDS);
    }

    private static void installSDKFile(VotingPluginMain plugin, ServerData file) throws Exception {
        var field = AdvancedCorePlugin.class.getDeclaredField("serverDataFile");
        field.setAccessible(true);
        field.set(plugin, file);
    }

    private static ScheduledExecutorService terminatedExecutor() {
        var executor = mock(ScheduledExecutorService.class);
        when(executor.isTerminated()).thenReturn(true);
        return executor;
    }

    @Test void rejectedSDKAdmissionRetiresOnlyItsUnusedPluginGenerationAndCanRetryAfterRelease() throws Exception {
        var plugin = plugin();
        var delayedRelease = new CompletableFuture<Void>();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var cleanup = mockConstruction(AdvancedCoreLifecycle.class, (owner, context) -> {
                    doAnswer(call -> {
                        plugin.onShutdownComplete(CompletableFuture.completedFuture(null));
                        plugin.releaseServerDataOwnershipAfter(delayedRelease);
                        return null;
                    }).when(owner).shutdown(anyLong());
                })) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, plugin::onEnable);
            awaitCapturedRetirement(plugin);
            for (int attempt = 0; attempt < 2; attempt++) {
                var failure = assertThrows(IllegalStateException.class, plugin::onEnable);
                assertEquals("Previous ServerData shutdown has not finished", failure.getMessage());
                plugin.onDisable();
            }
            delayedRelease.complete(null);
            assertThrows(StartupReached.class, plugin::onEnable);
            awaitCapturedRetirement(plugin);
        } finally {
            delayedRelease.complete(null);
            plugin.onShutdownComplete(CompletableFuture.completedFuture(null));
            if (plugin.getServerDataPublicationFence() != null) plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void admittedSDKInitializationFailureStillRetainsItsPendingFinalPublication() throws Exception {
        var plugin = plugin();
        var pendingPublication = new CompletableFuture<Void>();
        var initializationFailure = new IllegalStateException("synthetic executor initialization failure");
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var cleanup = mockConstruction(AdvancedCoreLifecycle.class, (owner, context) -> {
                    doAnswer(call -> {
                        plugin.onShutdownComplete(pendingPublication);
                        plugin.releaseServerDataOwnershipAfter(pendingPublication);
                        return null;
                    }).when(owner).shutdown(anyLong());
                })) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any()))
                    .thenThrow(initializationFailure).thenReturn(runtime);
            assertSame(initializationFailure, assertThrows(IllegalStateException.class, plugin::onEnable));
            verify(plugin, never()).onPreLoad();
            assertFalse(pendingPublication.isDone());
            assertThrows(IllegalStateException.class, plugin::onEnable);
            pendingPublication.complete(null);
            awaitCapturedRetirement(plugin);
            assertThrows(StartupReached.class, plugin::onEnable);
            awaitCapturedRetirement(plugin);
        } finally {
            pendingPublication.complete(null);
            if (plugin.getServerDataPublicationFence() != null) plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void actualSDKVersionCallbackRetainsOwnershipOutsideCapturedExecutors() throws Exception {
        var old = plugin(); var next = plugin();
        Path target = directory.resolve("ServerData.yml");
        var yaml = spy(new YamlConfiguration());
        var scheduler = mock(BukkitScheduler.class);
        var callback = new AtomicReference<Runnable>();
        doAnswer(call -> { callback.set(call.getArgument(1)); return null; })
                .when(scheduler).runTaskAsynchronously(any(), any());
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(scheduler,
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService externalWriter = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var cleanup = completedCleanup();
                var constructors = mockConstruction(BukkitScheduler.class)) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            var writingReference = new AtomicReference<Future<?>>();
            doAnswer(startup -> {
                var backing = new ServerData(old);
                backing.setData(yaml); backing.setPluginVersion(old);
                installSDKFile(old, backing);
                assertSame(yaml, old.getServerDataFile().getData());
                old.setLoadServerData(true);
                old.checkPluginUpdate();
                assertNotNull(callback.get());
                doAnswer(call -> {
                    entered.countDown(); assertTrue(release.await(10, TimeUnit.SECONDS));
                    return call.callRealMethod();
                }).when(yaml).save(target.toFile());
                // The SDK now cleans up a failed startup itself. Start the
                // real captured writer before that cleanup retires startup.
                writingReference.set(externalWriter.submit(callback.get()));
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                throw new StartupReached();
            }).when(old).onPreLoad();
            assertThrows(StartupReached.class, old::onEnable);
            Future<?> writing = writingReference.get();
            assertNotNull(writing);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            old.onDisable();
            old.getServerDataPublicationFence().retainUntilTermination(old.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
            assertFalse(assertThrows(IllegalStateException.class, next::onEnable) instanceof StartupReached,
                    "SDK publication must retain ownership even when every captured executor is terminated");
            verify(next, never()).onPreLoad();
            release.countDown(); writing.get(5, TimeUnit.SECONDS);
            var successor = YamlConfiguration.loadConfiguration(target.toFile());
            successor.set("Adjacent.Value", "successor"); successor.save(target.toFile());
            doAnswer(startup -> {
                // Run while the SDK's replacement plugin identity is live,
                // before its sentinel failure triggers automatic cleanup.
                assertThrows(IllegalStateException.class, callback.get()::run,
                        "an old callback that starts after replacement cannot publish");
                assertThrows(IllegalStateException.class, () -> old.getServerDataFile().setPrevDay(12));
                assertEquals("successor", YamlConfiguration.loadConfiguration(target.toFile())
                        .getString("Adjacent.Value"));
                throw new StartupReached();
            }).when(next).onPreLoad();
            assertThrows(StartupReached.class, next::onEnable);
            awaitCapturedRetirement(next);
            next.getServerDataPublicationFence().retire();
        } finally {
            release.countDown(); externalWriter.shutdownNow();
            assertTrue(externalWriter.awaitTermination(5, TimeUnit.SECONDS));
            if (old.getServerDataPublicationFence() != null) old.getServerDataPublicationFence().retire();
        }
    }

    @Test void inheritedFileOperationsKeepTheSDKConfigurationAuthoritative() throws Exception {
        var plugin = plugin();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var completedCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, plugin::onEnable);
            var backing = new ServerData(plugin); backing.setData(new YamlConfiguration());
            installSDKFile(plugin, backing);
            var facade = plugin.getServerDataFile();
            assertSame(backing.getData(), facade.getData());
            facade.setIgnoreCase(true);
            assertTrue(backing.isIgnoreCase()); assertTrue(facade.isIgnoreCase());
            assertSame(backing.getData(), facade.getData());
            facade.setValue("Sample.Value", "kept");
            facade.createSection("Sample.Section");
            var merged = new YamlConfiguration(); merged.set("Adjacent.Value", "merged");
            facade.setData(merged); facade.saveData();
            facade.reloadData();
            assertSame(backing.getData(), facade.getData());
            assertEquals("kept", facade.getData().getString("Sample.Value"));
            assertEquals("merged", facade.getData().getString("Adjacent.Value"));
            assertNotNull(facade.getData().getConfigurationSection("Sample.Section"));
            assertEquals(backing.isJustCreated(), facade.isJustCreated());
            assertEquals(backing.isFailedToRead(), facade.isFailedToRead());
            assertSame(backing.getPlugin(), facade.getPlugin());
            assertEquals(backing.getdFile(), facade.getdFile());
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void oldVotingPluginWrapperDoesNotBorrowTheSamePluginObjectsReplacementFileOrFence() throws Exception {
        var plugin = plugin();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var completedCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, plugin::onEnable);
            var first = new ServerData(plugin); first.setData(new YamlConfiguration());
            installSDKFile(plugin, first);
            var stale = new com.bencodez.votingplugin.data.ServerData(plugin);
            stale.getData().set("Adjacent.Value", "first"); stale.saveData();
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
            assertThrows(StartupReached.class, plugin::onEnable);
            var replacement = new ServerData(plugin); replacement.setData(new YamlConfiguration());
            installSDKFile(plugin, replacement);
            var current = new com.bencodez.votingplugin.data.ServerData(plugin);
            current.getData().set("Adjacent.Value", "successor"); current.saveData();
            assertThrows(IllegalStateException.class, () -> stale.addTimeVoted(0,
                    new com.bencodez.votingplugin.timequeue.VoteTimeQueue("Stale", "site", 123L)));
            assertEquals("successor", current.getData().getString("Adjacent.Value"));
            assertNull(current.getData().get("TimedVoteCache"));
            assertEquals("successor", YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile())
                    .getString("VotingPlugin.Adjacent.Value"));
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void firstUseOfAnOldWrapperAfterReenableDoesNotMutateTheReplacementFile() throws Exception {
        var plugin = plugin();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var completedCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, plugin::onEnable);
            var first = new ServerData(plugin); first.setData(new YamlConfiguration());
            installSDKFile(plugin, first);
            var stale = new com.bencodez.votingplugin.data.ServerData(plugin);
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
            assertThrows(StartupReached.class, plugin::onEnable);
            var replacement = new ServerData(plugin); replacement.setData(new YamlConfiguration());
            installSDKFile(plugin, replacement);
            var current = new com.bencodez.votingplugin.data.ServerData(plugin);
            current.getData().set("Adjacent.Value", "successor"); current.saveData();
            assertThrows(IllegalStateException.class, () -> stale.addTimeVoted(0,
                    new com.bencodez.votingplugin.timequeue.VoteTimeQueue("Stale", "site", 123L)));
            assertEquals("successor", current.getData().getString("Adjacent.Value"));
            assertNull(current.getData().get("TimedVoteCache"));
            assertEquals("successor", YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile())
                    .getString("VotingPlugin.Adjacent.Value"));
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void failedReenableBeforeFileReplacementCanRecoverAfterNativeTimestampCleanup() throws Exception {
        var plugin = plugin();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        var observed = new AtomicReference<CompletionStage<Void>>();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var cleanup = mockConstruction(AdvancedCoreLifecycle.class, (owner, context) -> {
                    doAnswer(call -> {
                        // Exercise the actual SDK timestamp submission, while
                        // unrelated platform teardown remains controlled.
                        var platform = new com.bencodez.advancedcore.bukkit.runtime.BukkitRuntimePlatform(plugin);
                        var method = platform.getClass().getDeclaredMethod("submitTimestampPersistence", Executor.class);
                        method.setAccessible(true);
                        var workerField = platform.getClass().getDeclaredField("persistenceWorker");
                        workerField.setAccessible(true);
                        var worker = (ExecutorService) workerField.get(platform);
                        CompletionStage<Void> stage;
                        try {
                            @SuppressWarnings("unchecked")
                            var submitted = (CompletionStage<Void>) method.invoke(platform, worker);
                            stage = submitted;
                        } finally {
                            worker.shutdown();
                        }
                        observed.set(stage);
                        plugin.onShutdownComplete(stage);
                        plugin.releaseServerDataOwnershipAfter(stage);
                        return null;
                    }).when(owner).shutdown(anyLong());
                })) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            var initialFailure = assertThrows(StartupReached.class, plugin::onEnable);
            assertEquals(0, initialFailure.getSuppressed().length,
                    "native timestamp cleanup must actually execute without hidden fixture failures");
            assertNotNull(observed.get(), "capture the actual SDK final-publication stage");
            observed.get().toCompletableFuture().get(5, TimeUnit.SECONDS);
            awaitCapturedRetirement(plugin);
            var backing = new ServerData(plugin); backing.setData(new YamlConfiguration());
            installSDKFile(plugin, backing);
            var retiredFacade = plugin.getServerDataFile();
            assertNotNull(retiredFacade);
            assertThrows(StartupReached.class, plugin::onEnable);
            observed.get().toCompletableFuture().get(5, TimeUnit.SECONDS);
            awaitCapturedRetirement(plugin);
            assertThrows(IllegalStateException.class, () -> retiredFacade.setPrevDay(12));
            assertThrows(StartupReached.class, plugin::onEnable,
                    "a failed pre-load must not permanently reserve either generation owner");
            observed.get().toCompletableFuture().get(5, TimeUnit.SECONDS);
            awaitCapturedRetirement(plugin);
        }
    }

    @Test void fileReadBetweenReenableAndSDKReplacementWithholdsPredecessorAndKeepsCapturedFence() throws Exception {
        var plugin = plugin();
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var completedCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, plugin::onEnable);
            var backing = new ServerData(plugin); backing.setData(new YamlConfiguration());
            installSDKFile(plugin, backing);
            backing.setPrevDay(1);
            var predecessor = plugin.getServerDataFile();
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
            var intervening = new AtomicReference<ServerData>();
            doAnswer(call -> {
                intervening.set(plugin.getServerDataFile());
                throw new StartupReached();
            }).when(plugin).onPreLoad();
            assertThrows(StartupReached.class, plugin::onEnable);
            var replacement = new ServerData(plugin); replacement.setData(new YamlConfiguration());
            installSDKFile(plugin, replacement);
            plugin.getServerDataFile().setValue("Adjacent.Value", "successor");
            assertNull(intervening.get(), "pre-load must not expose a predecessor file to native final publication");
            assertThrows(IllegalStateException.class, () -> predecessor.setPrevDay(12));
            assertEquals("successor", YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile())
                    .getString("Adjacent.Value"));
            awaitCapturedRetirement(plugin);
            plugin.getServerDataPublicationFence().retire();
        }
    }

    @Test void sdkDurableTransitionPublicationAlsoOwnsTheGenerationUntilItReturns() throws Exception {
        var old = plugin(); var next = plugin();
        Path target = directory.resolve("ServerData.yml");
        var yaml = spy(new YamlConfiguration());
        var runtime = new AdvancedCoreLifecycle.RuntimeExecutors(mock(BukkitScheduler.class),
                terminatedExecutor(), terminatedExecutor(), terminatedExecutor());
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        ExecutorService externalWriter = Executors.newSingleThreadExecutor();
        try (var lifecycle = mockStatic(AdvancedCoreLifecycle.class);
                var commands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class);
                var constructors = mockConstruction(BukkitScheduler.class);
                var completedCleanup = completedCleanup()) {
            lifecycle.when(() -> AdvancedCoreLifecycle.createRuntimeExecutors(any())).thenReturn(runtime);
            assertThrows(StartupReached.class, old::onEnable);
            var backing = new ServerData(old); backing.setData(yaml); installSDKFile(old, backing);
            doAnswer(call -> {
                entered.countDown(); assertTrue(release.await(10, TimeUnit.SECONDS));
                return call.callRealMethod();
            }).when(yaml).saveToString();
            var file = old.getServerDataFile();
            Future<ServerData.TimeChangeTransitionState> writing = externalWriter.submit(() -> file.beginTimeChangeTransition(TimeType.DAY, "synthetic-period", "12"));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            old.getServerDataPublicationFence().retire();
            assertFalse(assertThrows(IllegalStateException.class, next::onEnable) instanceof StartupReached);
            release.countDown(); var transition = writing.get(5, TimeUnit.SECONDS);
            var restored = new ServerData(old); restored.setData(YamlConfiguration.loadConfiguration(target.toFile()));
            assertEquals(transition, restored.getPendingTimeChangeTransition(TimeType.DAY));
            assertThrows(StartupReached.class, next::onEnable);
            assertThrows(IllegalStateException.class, () -> file.completeTimeChangeTransition(transition));
            next.getServerDataPublicationFence().retire();
        } finally {
            release.countDown(); externalWriter.shutdownNow();
            assertTrue(externalWriter.awaitTermination(5, TimeUnit.SECONDS));
            if (old.getServerDataPublicationFence() != null) old.getServerDataPublicationFence().retire();
        }
    }
}
