package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.management.ManagementFactory;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class DeferredServerDataOwnerPublicationTest {
    @TempDir Path directory;

    @Test void deferredNativePublicationSerializesCheckpointsAndRetainsItsActualAcknowledgement() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var ownerWorker = Executors.newSingleThreadExecutor();
            var checkpointWorker = Executors.newSingleThreadExecutor();
            var mutationEntered = new CountDownLatch(1);
            var releaseMutation = new CountDownLatch(1);
            var nativePublished = new CountDownLatch(1);
            var releaseAcknowledgement = new CountDownLatch(1);
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin) {
                @Override public CompletionStage<Void> mutateFinalData(Consumer<FileConfiguration> mutation) {
                    return CompletableFuture.supplyAsync(() -> super.mutateFinalData(mutation), ownerWorker)
                            .thenCompose(stage -> stage.thenRun(() -> {
                                assertFalse(Thread.holdsLock(this), "acknowledgement must run outside YAML ownership");
                                nativePublished.countDown();
                                await(releaseAcknowledgement);
                            }));
                }
            };
            backing.setData(new YamlConfiguration());
            backing.getData().set("Adjacent.Value", "preserved");
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var inner = new GenerationFencedServerDataFile(plugin, backing, fence);
            var facade = new GenerationFencedServerDataFile(plugin, inner, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            data.getData();
            var checkpointThread = new AtomicReference<Thread>();
            var checkpointStarted = new CountDownLatch(1);
            try {
                // This returns a genuinely deferred owner stage, not a mocked future.
                var observation = facade.mutateFinalData(snapshot -> {
                    assertTrue(Thread.holdsLock(backing));
                    assertEquals("preserved", snapshot.getString("Adjacent.Value"));
                    mutationEntered.countDown();
                    await(releaseMutation);
                    snapshot.set("Final.Value", "confirmed");
                }).toCompletableFuture();
                assertTrue(mutationEntered.await(5, TimeUnit.SECONDS));
                assertFalse(observation.isDone());
                var checkpoint = checkpointWorker.submit(() -> {
                    checkpointThread.set(Thread.currentThread());
                    checkpointStarted.countDown();
                    data.setInt("VoteParty.Total", 7);
                });
                assertTrue(checkpointStarted.await(5, TimeUnit.SECONDS));
                var threads = ManagementFactory.getThreadMXBean();
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                boolean blockedByOwner = false;
                while (!checkpoint.isDone() && System.nanoTime() < deadline) {
                    var info = threads.getThreadInfo(checkpointThread.get().threadId());
                    if (info != null && info.getThreadState() == Thread.State.BLOCKED
                            && info.getLockInfo() != null
                            && info.getLockInfo().getIdentityHashCode() == System.identityHashCode(backing)) {
                        blockedByOwner = true;
                        break;
                    }
                    Thread.yield();
                }
                assertTrue(blockedByOwner, "ordinary checkpoints must wait for the deferred native YAML owner");
                releaseMutation.countDown();
                assertTrue(nativePublished.await(5, TimeUnit.SECONDS));
                checkpoint.get(5, TimeUnit.SECONDS);
                var disk = YamlConfiguration.loadConfiguration(target.toFile());
                assertEquals("confirmed", disk.getString("Final.Value"));
                assertEquals("preserved", disk.getString("Adjacent.Value"));
                assertEquals(7, disk.getInt("VotingPlugin.VoteParty.Total"));
                fence.retire();
                assertFalse(observation.isDone(), "visible native bytes alone do not settle the owner's stage");
                assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
                releaseAcknowledgement.countDown();
                observation.get(5, TimeUnit.SECONDS);
                var successor = ServerDataPublicationFence.beginGeneration(target);
                successor.retire();
            } finally {
                releaseMutation.countDown();
                releaseAcknowledgement.countDown();
                ownerWorker.shutdownNow();
                checkpointWorker.shutdownNow();
                assertTrue(ownerWorker.awaitTermination(5, TimeUnit.SECONDS));
                assertTrue(checkpointWorker.awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        }
    }

    private static void await(CountDownLatch latch) {
        try { assertTrue(latch.await(10, TimeUnit.SECONDS)); }
        catch (InterruptedException failure) {
            Thread.currentThread().interrupt();
            throw new AssertionError(failure);
        }
    }
}
