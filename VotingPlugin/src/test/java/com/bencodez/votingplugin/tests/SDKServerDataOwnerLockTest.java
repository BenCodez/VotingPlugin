package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.management.ManagementFactory;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.item.FullInventoryHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class SDKServerDataOwnerLockTest {
    @TempDir Path directory;

    @Test void nativeInventoryOwnerExcludesOrdinaryPublication() throws Exception {
        assertInventoryReplacementIsAtomic(false, false);
    }

    @Test void nativeInventoryOwnerExcludesFinalPublication() throws Exception {
        assertInventoryReplacementIsAtomic(false, true);
    }

    @Test void wrappedInventoryOwnerExcludesOrdinaryPublication() throws Exception {
        assertInventoryReplacementIsAtomic(true, false);
    }

    @Test void wrappedInventoryOwnerExcludesFinalPublication() throws Exception {
        assertInventoryReplacementIsAtomic(true, true);
    }

    @Test void composedInventoryOwnerExcludesOrdinaryPublication() throws Exception {
        assertInventoryReplacementIsAtomic(2, false);
    }

    @Test void composedInventoryOwnerExcludesFinalPublication() throws Exception {
        assertInventoryReplacementIsAtomic(2, true);
    }

    @Test void composedFacadeAdvertisesTheInnermostYamlOwner() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            try {
                var inner = new GenerationFencedServerDataFile(plugin, backing, fence);
                var outer = new GenerationFencedServerDataFile(plugin, inner, fence);
                assertSame(backing, outer.publicationMonitor(), "the delegate can itself be an adapter");
            } finally { fence.retire(); }
        }
    }

    private void assertInventoryReplacementIsAtomic(boolean wrapped, boolean finalPublication) throws Exception {
        assertInventoryReplacementIsAtomic(wrapped ? 1 : 0, finalPublication);
    }

    private void assertInventoryReplacementIsAtomic(int facadeDepth, boolean finalPublication) throws Exception {
        org.bukkit.configuration.serialization.ConfigurationSerialization.registerClass(PendingItem.class);
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            when(plugin.getBukkitScheduler()).thenReturn(mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class));
            // Exercise the public SDK writer. Startup has no optional file yet;
            // install the real owner before admitting either writer below.
            var inventory = new FullInventoryHandler(plugin);
            inventory.getTimer().shutdownNow();
            var cleared = new CountDownLatch(1);
            var release = new CountDownLatch(1);
            var armed = new AtomicBoolean();
            var observedIntermediate = new AtomicBoolean();
            var yaml = new YamlConfiguration() {
                @Override public void set(String path, Object value) {
                    super.set(path, value);
                    if (path.equals("FullInventory") && value == null && armed.compareAndSet(true, false)) {
                        cleared.countDown();
                        try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                        catch (InterruptedException failure) { throw new AssertionError(failure); }
                    }
                }

                @Override public String saveToString() {
                    String text = super.saveToString();
                    if (cleared.getCount() == 0 && release.getCount() != 0 && !contains("FullInventory")) {
                        observedIntermediate.set(true);
                    }
                    return text;
                }
            };
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin) {
                @Override public void saveDataChecked() {
                    // New SDK owners mutate an isolated candidate, so clearing
                    // the live YAML is no longer a guaranteed observable seam.
                    // Pause the actual native publication instead. The legacy
                    // clear hook above still exercises its temporary-live gap.
                    if (armed.compareAndSet(true, false)) {
                        assertTrue(Thread.holdsLock(this),
                                "native snapshot publication must own the backing monitor");
                        cleared.countDown();
                        try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                        catch (InterruptedException failure) { throw new AssertionError(failure); }
                    }
                    super.saveDataChecked();
                }
            };
            backing.setData(yaml);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            com.bencodez.advancedcore.data.ServerData composed = backing;
            for (int depth = 0; depth < facadeDepth; depth++) {
                composed = new GenerationFencedServerDataFile(plugin, composed, fence);
            }
            var exposed = composed;
            when(plugin.getServerDataFile()).thenReturn(exposed);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            data.getData();
            UUID previous = new UUID(0, 2);
            inventory.getItems().put(previous, new ArrayList<>(java.util.List.of(new PendingItem(1))));
            assertTrue(inventory.saveDurably());
            var initialDisk = YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile());
            assertInstanceOf(PendingItem.class, initialDisk.get("FullInventory." + previous + ".Items.0"));
            assertEquals(1, initialDisk.getItemStack("FullInventory." + previous + ".Items.0").getAmount());
            inventory.getItems().clear();
            UUID replacement = new UUID(0, 1);
            inventory.getItems().put(replacement, new ArrayList<>(java.util.List.of(new PendingItem(2))));
            var workers = Executors.newFixedThreadPool(2);
            var inventoryThread = new AtomicReference<Thread>();
            var competingThread = new AtomicReference<Thread>();
            var competingStarted = new CountDownLatch(1);
            try {
                armed.set(true);
                var saving = workers.submit(() -> {
                    inventoryThread.set(Thread.currentThread());
                    return inventory.saveDurably();
                });
                assertTrue(cleared.await(5, TimeUnit.SECONDS));
                var competing = workers.submit(() -> {
                    competingThread.set(Thread.currentThread());
                    competingStarted.countDown();
                    if (finalPublication) {
                        exposed.mutateFinalData(snapshot -> {
                            if (release.getCount() != 0 && !snapshot.contains("FullInventory")) {
                                observedIntermediate.set(true);
                            }
                            snapshot.set("SyntheticFinal.Value", "confirmed");
                        }).toCompletableFuture().join();
                    } else {
                        data.setInt("VoteParty.Total", 7);
                    }
                });
                assertTrue(competingStarted.await(5, TimeUnit.SECONDS));
                var threads = ManagementFactory.getThreadMXBean();
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                boolean blockedByOwner = false;
                while (!observedIntermediate.get() && !competing.isDone() && System.nanoTime() < deadline) {
                    var info = threads.getThreadInfo(competingThread.get().threadId());
                    if (info != null && info.getThreadState() == Thread.State.BLOCKED
                            && info.getLockOwnerId() == inventoryThread.get().threadId()
                            && info.getLockInfo() != null
                            && info.getLockInfo().getIdentityHashCode() == System.identityHashCode(backing)) {
                        blockedByOwner = true;
                        break;
                    }
                    Thread.yield();
                }
                assertTrue(blockedByOwner || observedIntermediate.get() || competing.isDone(),
                        "competing writer never reached the ownership or serialization boundary");
                // Confirm the defective publication before releasing the SDK
                // replacement, rather than racing two unsynchronized YAML edits.
                if (observedIntermediate.get()) competing.get(5, TimeUnit.SECONDS);
                boolean publishedIntermediate = observedIntermediate.get();
                release.countDown();
                assertTrue(saving.get(5, TimeUnit.SECONDS));
                competing.get(5, TimeUnit.SECONDS);
                var disk = YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile());
                assertTrue(disk.contains("FullInventory." + replacement + ".Time"));
                assertEquals(2, disk.getItemStack("FullInventory." + replacement + ".Items.0").getAmount());
                assertFalse(disk.contains("FullInventory." + previous));
                if (finalPublication) assertEquals("confirmed", disk.getString("SyntheticFinal.Value"));
                else assertEquals(7, disk.getInt("VotingPlugin.VoteParty.Total"));
                assertFalse(publishedIntermediate,
                        "a successful publication serialized the SDK's temporarily cleared recovery section");
                assertTrue(blockedByOwner, "the competing writer must share the actual SDK transaction monitor");
            } finally {
                release.countDown();
                workers.shutdownNow();
                assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
                inventory.getTimer().shutdownNow();
                assertTrue(inventory.getTimer().awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        } finally {
            org.bukkit.configuration.serialization.ConfigurationSerialization.unregisterClass(PendingItem.class);
        }
    }

    /** Headless item serialization keeps the actual SDK writer and recovery YAML in the test. */
    @org.bukkit.configuration.serialization.SerializableAs("VPServerDataMonitorTestItem")
    public static final class PendingItem extends org.bukkit.inventory.ItemStack {
        public PendingItem(int amount) { super(org.bukkit.Material.STONE, amount); }
        @Override public java.util.Map<String, Object> serialize() { return java.util.Map.of("amount", getAmount()); }
        public static PendingItem deserialize(java.util.Map<String, Object> values) {
            return new PendingItem(((Number) values.get("amount")).intValue());
        }
    }
}
