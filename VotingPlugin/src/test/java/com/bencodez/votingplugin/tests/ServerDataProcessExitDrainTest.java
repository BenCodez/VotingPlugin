package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.signs.SignHandler;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class ServerDataProcessExitDrainTest {
    @TempDir Path directory;

    /** Observe entry to the real bounded wait without replacing its completion semantics. */
    private static final class ObservedCompletion extends CompletableFuture<Void> {
        final CountDownLatch entered = new CountDownLatch(1);
        volatile long nanos;
        @Override public Void get(long timeout, TimeUnit unit)
                throws InterruptedException, ExecutionException, TimeoutException {
            nanos = unit.toNanos(timeout);
            entered.countDown();
            return super.get(timeout, unit);
        }
    }

    private static Object field(Object owner, String name) throws Exception {
        var f = owner.getClass().getDeclaredField(name);
        f.setAccessible(true);
        return f.get(owner);
    }

    private static ObservedCompletion observe(ServerDataPublicationFence fence) throws Exception {
        var completion = new ObservedCompletion();
        var f = ServerDataPublicationFence.class.getDeclaredField("processExitCompletion");
        f.setAccessible(true);
        f.set(fence, completion);
        return completion;
    }

    @Test void outstandingLeaseKeepsTheActualProcessHookWaitingUntilRetirementDrains() throws Exception {
        var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        var completion = observe(fence);
        var lease = fence.acquire();
        fence.retainForProcessExit();
        var hook = (Thread) field(fence, "processHook");
        fence.retainForProcessExit();
        assertSame(hook, field(fence, "processHook"), "Repeated enrollment must not register another hook");
        fence.beginShutdown(System.nanoTime());
        fence.retire();
        var caller = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            var exit = caller.submit(hook::run);
            assertTrue(completion.entered.await(5, TimeUnit.SECONDS));
            assertFalse(completion.isDone());
            assertFalse(exit.isDone(), "A daemon reservation must still be represented at process exit");
            lease.close();
            exit.get(5, TimeUnit.SECONDS);
            assertTrue(completion.isDone());
            ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml")).retire();
        } finally {
            lease.close();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void repeatedDisableCannotRenewTheOriginalSharedDeadline() throws Exception {
        var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        var completion = observe(fence);
        var lease = fence.acquire();
        fence.retainForProcessExit();
        long original = System.nanoTime() - TimeUnit.SECONDS.toNanos(1);
        fence.beginShutdown(original);
        fence.beginShutdown(System.nanoTime());
        var started = (CompletableFuture<?>) field(fence, "processShutdownStarted");
        assertEquals(original, started.get());
        fence.retire();
        var caller = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            var hook = (Thread) field(fence, "processHook");
            var exit = caller.submit(hook::run);
            assertTrue(completion.entered.await(5, TimeUnit.SECONDS));
            assertTrue(completion.nanos > 0 && completion.nanos <= TimeUnit.SECONDS.toNanos(14),
                    "Process exit must use the remaining disable budget, not another fifteen seconds");
            lease.close();
            exit.get(5, TimeUnit.SECONDS);
        } finally {
            lease.close();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void expiredProcessBudgetDoesNotCertifyFailedPublicationOrLiftGenerationAdmission() throws Exception {
        var target = directory.resolve("ServerData.yml");
        var fence = ServerDataPublicationFence.beginGeneration(target);
        var completion = observe(fence);
        fence.retainForProcessExit();
        fence.retainUntilSuccessfulPublication(CompletableFuture.failedFuture(new IllegalStateException("Synthetic I/O")));
        var hook = (Thread) field(fence, "processHook");
        fence.beginShutdown(System.nanoTime() - TimeUnit.SECONDS.toNanos(16));
        fence.retire();
        var caller = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            caller.submit(hook::run).get(5, TimeUnit.SECONDS);
            assertFalse(completion.isDone(), "Deadline expiry cannot report persistence success");
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
        } finally {
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void interruptedProcessWaitPreservesInterruptionWithoutAcknowledgingTheWriter() throws Exception {
        var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        var completion = observe(fence);
        var lease = fence.acquire();
        fence.retainForProcessExit();
        fence.beginShutdown(System.nanoTime());
        fence.retire();
        var interrupted = new java.util.concurrent.atomic.AtomicBoolean();
        var hook = (Thread) field(fence, "processHook");
        var observer = new Thread(() -> {
            hook.run();
            interrupted.set(Thread.currentThread().isInterrupted());
        });
        try {
            observer.start();
            assertTrue(completion.entered.await(5, TimeUnit.SECONDS));
            observer.interrupt();
            observer.join(5000);
            assertFalse(observer.isAlive());
            assertTrue(interrupted.get());
            assertFalse(completion.isDone());
        } finally {
            lease.close();
            observer.interrupt();
            observer.join(5000);
        }
    }

    @Test void retainedExecutorTerminationIsIncludedWithoutAPreStorageDependencyCycle() throws Exception {
        var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        var completion = observe(fence);
        fence.retainForProcessExit();
        var writer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor();
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        writer.execute(() -> {
            entered.countDown();
            try { assertTrue(release.await(5, TimeUnit.SECONDS)); }
            catch (InterruptedException failure) { throw new AssertionError(failure); }
        });
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        writer.shutdown();
        var retirement = fence.retainUntilTermination(new java.util.concurrent.ScheduledExecutorService[] { writer });
        fence.beginShutdown(System.nanoTime());
        fence.retire();
        var caller = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            var exit = caller.submit(((Thread) field(fence, "processHook"))::run);
            assertTrue(completion.entered.await(5, TimeUnit.SECONDS));
            assertFalse(completion.isDone());
            release.countDown();
            retirement.toCompletableFuture().get(5, TimeUnit.SECONDS);
            exit.get(5, TimeUnit.SECONDS);
        } finally {
            release.countDown();
            writer.shutdownNow();
            caller.shutdownNow();
            assertTrue(writer.awaitTermination(5, TimeUnit.SECONDS));
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void queuedSignPublicationRemainsInTheProcessDrainAfterTheNativeSdkOwnerReleases() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var target = directory.resolve("ServerData.yml");
            var nativeOwner = com.bencodez.advancedcore.lifecycle.ServerDataOwnership.acquire(target);
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var completion = observe(fence);
            fence.retainForProcessExit();
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            var workers = new ArrayList<Runnable>();
            var signs = new Signs(plugin) {
                @Override protected void startShutdownWorker(Runnable worker) { workers.add(worker); }
            };
            var world = mock(World.class);
            when(world.getName()).thenReturn("SyntheticWorld");
            var sign = mock(SignHandler.class, CALLS_REAL_METHODS);
            for (var entry : java.util.Map.<String, Object>of("sign", "0", "position", 1,
                    "data", "SyntheticSign").entrySet()) {
                var f = SignHandler.class.getDeclaredField(entry.getKey());
                f.setAccessible(true);
                f.set(sign, entry.getValue());
            }
            sign.setLocation(new Location(world, 1, 2, 3));
            signs.getSigns().add(sign);
            var published = signs.storeSignsForShutdown();
            long started = System.nanoTime();
            nativeOwner.beginShutdown(started);
            // Exercise the actual SDK ownership release once its own aggregate has settled.
            // This fixture does not simulate unrelated native cleanup actions.
            nativeOwner.releaseAfter(CompletableFuture.completedFuture(null));
            assertTrue(nativeOwner.isReleased());
            fence.beginShutdown(started);
            fence.retire();
            var caller = java.util.concurrent.Executors.newSingleThreadExecutor();
            try {
                var exit = caller.submit(((Thread) field(fence, "processHook"))::run);
                assertTrue(completion.entered.await(5, TimeUnit.SECONDS));
                assertFalse(completion.isDone(), "The native release cannot discard the consumer reservation");
                assertFalse(published.toCompletableFuture().isDone());
                workers.getFirst().run();
                published.toCompletableFuture().get(5, TimeUnit.SECONDS);
                exit.get(5, TimeUnit.SECONDS);
                assertEquals("SyntheticSign", YamlConfiguration.loadConfiguration(target.toFile())
                        .getString("VotingPlugin.Signs.0.Data"));
            } finally {
                if (!published.toCompletableFuture().isDone()) workers.getFirst().run();
                caller.shutdownNow();
                assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
            }
        }
    }
}
