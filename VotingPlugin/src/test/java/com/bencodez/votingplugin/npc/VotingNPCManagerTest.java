package com.bencodez.votingplugin.npc;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.mockito.AdditionalMatchers.aryEq;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.Chunk;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.entity.Villager;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.world.ChunkLoadEvent;
import org.bukkit.event.world.ChunkUnloadEvent;
import org.bukkit.event.world.WorldLoadEvent;
import org.bukkit.event.world.WorldUnloadEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VotingNPCManagerTest {
    @TempDir Path directory;
    private static final UUID WORLD = new UUID(1, 0);

    private static VotingNPCDefinition definition(int identity) {
        return new VotingNPCDefinition("voting", new UUID(0, identity), WORLD, 8, 64, 8, 0, 0, "&aVote");
    }

    private static VotingNPCDefinition definitionAt(int identity, double x) {
        return new VotingNPCDefinition("npc-" + identity, new UUID(0, identity), WORLD, x, 64, 8, 0, 0, "&aVote");
    }

    private static <T> T await(CompletionStage<T> stage) throws Exception {
        return stage.toCompletableFuture().get(5, TimeUnit.SECONDS);
    }

    private static class Scheduler extends VotingNPCScheduler {
        final LinkedBlockingQueue<Runnable> global = new LinkedBlockingQueue<>();
        final LinkedBlockingQueue<Runnable> regions = new LinkedBlockingQueue<>();
        final LinkedBlockingQueue<Runnable> entities = new LinkedBlockingQueue<>();
        String owner = "none";
        boolean rejectCleanup;
        Scheduler(VotingPluginMain plugin) { super(plugin); }
        @Override boolean owns(Location value) { return owner.equals("region"); }
        @Override boolean owns(Entity value) { return owner.equals("entity"); }
        @Override void global(Runnable action, Consumer<Throwable> failed) { global.add(action); }
        @Override void region(Location anchor, Runnable action, Consumer<Throwable> failed) { regions.add(action); }
        @Override void entity(Entity entity, Runnable action, Runnable retired, Consumer<Throwable> failed) {
            if (rejectCleanup) failed.accept(new IllegalStateException("Synthetic rejection"));
            else entities.add(action);
        }
        void runNext(LinkedBlockingQueue<Runnable> queue, String phase) throws Exception {
            Runnable action = queue.poll(5, TimeUnit.SECONDS);
            assertNotNull(action, "Expected scheduled " + phase + " work");
            owner = phase;
            try { action.run(); } finally { owner = "none"; }
        }
    }

    @Test void duplicateChunkNotificationsDoNotReconstructAnotherEntityAndOrdinaryEntitiesAreIgnored() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin);
            List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, player -> fail("Unexpected menu"));
            try {
                manager.configure(true);
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                Chunk chunk = chunk(world);
                manager.chunkLoad(new ChunkLoadEvent(chunk, false));
                manager.chunkLoad(new ChunkLoadEvent(chunk, false));
                assertEquals(1, spawned.size()); assertEquals(1, manager.runtimeCount());
                verify(spawned.getFirst()).setPersistent(false);
                verify(spawned.getFirst()).setAI(false);
                verify(spawned.getFirst()).setCustomName("\u00a7aVote");
                Player player = mock(Player.class); when(player.getUniqueId()).thenReturn(new UUID(0, 100));
                Villager unrelated = mock(Villager.class); when(unrelated.getUniqueId()).thenReturn(new UUID(0, 99));
                var interact = new PlayerInteractEntityEvent(player, unrelated, EquipmentSlot.HAND);
                manager.interact(interact); assertFalse(interact.isCancelled());
                var damage = mock(EntityDamageEvent.class); when(damage.getEntity()).thenReturn(unrelated);
                manager.damage(damage); verify(damage, never()).setCancelled(anyBoolean());
                var protectedDamage = mock(EntityDamageEvent.class); when(protectedDamage.getEntity()).thenReturn(spawned.getFirst());
                manager.damage(protectedDamage); verify(protectedDamage).setCancelled(true);
                var hostileTarget = new org.bukkit.event.entity.EntityTargetEvent(unrelated, spawned.getFirst(),
                        org.bukkit.event.entity.EntityTargetEvent.TargetReason.CLOSEST_ENTITY);
                manager.target(hostileTarget);
                assertTrue(hostileTarget.isCancelled(), "ordinary mobs must not target a tracked NPC");
                var normalTarget = new org.bukkit.event.entity.EntityTargetEvent(unrelated, player,
                        org.bukkit.event.entity.EntityTargetEvent.TargetReason.CLOSEST_ENTITY);
                manager.target(normalTarget); assertFalse(normalTarget.isCancelled());
                var clearedTarget = new org.bukkit.event.entity.EntityTargetEvent(unrelated, null,
                        org.bukkit.event.entity.EntityTargetEvent.TargetReason.FORGOT_TARGET);
                manager.target(clearedTarget); assertFalse(clearedTarget.isCancelled());
                var witch = mock(org.bukkit.entity.Witch.class);
                var transform = new org.bukkit.event.entity.EntityTransformEvent(spawned.getFirst(), List.of(witch),
                        org.bukkit.event.entity.EntityTransformEvent.TransformReason.LIGHTNING);
                var unrelatedTransform = new org.bukkit.event.entity.EntityTransformEvent(unrelated, List.of(witch),
                        org.bukkit.event.entity.EntityTransformEvent.TransformReason.LIGHTNING);
                // Discover actual listener methods, as event registration does;
                // absence of a transformation handler must fail this regression.
                for (var method : VotingNPCManager.class.getDeclaredMethods()) {
                    if (method.isAnnotationPresent(org.bukkit.event.EventHandler.class) && method.getParameterCount() == 1
                            && method.getParameterTypes()[0].isInstance(transform)) {
                        method.invoke(manager, transform); method.invoke(manager, unrelatedTransform);
                    }
                }
                assertTrue(transform.isCancelled(), "lightning must not replace a tracked villager with an untracked entity");
                assertFalse(unrelatedTransform.isCancelled());
            } finally {
                manager.close(); scheduler.runNext(scheduler.entities, "entity");
                assertEquals(0, manager.runtimeCount()); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void freedFullCapacityWakesOnePendingDifferentId() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("capacity.json")); await(registry.initialLoad());
            for (int i = 1; i <= VotingNPCStore.MAX_NPCS; i++) await(registry.create(definitionAt(i, i * 2)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                for (int i = 0; i < VotingNPCStore.MAX_NPCS; i++) scheduler.runNext(scheduler.regions, "region");
                assertEquals(VotingNPCStore.MAX_NPCS, manager.runtimeCount());
                await(manager.remove("npc-1")); await(manager.create(definitionAt(1000, 2000)));
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.global, "global");
                scheduler.runNext(scheduler.entities, "entity");
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                assertEquals(VotingNPCStore.MAX_NPCS, manager.runtimeCount());
                assertEquals(VotingNPCStore.MAX_NPCS + 1, spawned.size());
            } finally {
                manager.close(); while (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void successorWaitsForExactPredecessorCleanupAndCleanupRejectionRetainsOwnership() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                scheduler.rejectCleanup = true;
                assertTrue(await(manager.remove("voting")));
                await(manager.create(definition(2)));
                // Process both durable mutation notifications while old removal is rejected.
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.global, "global");
                assertEquals(1, spawned.size()); assertEquals(1, manager.runtimeCount());
                verify(spawned.getFirst(), never()).remove();
                // A rejected cleanup keeps ownership; repeated load notifications may
                // request a retry, but must never create a successor or duplicate task.
                manager.chunkLoad(new ChunkLoadEvent(chunk(world), false));
                manager.chunkLoad(new ChunkLoadEvent(chunk(world), false));
                assertEquals(1, spawned.size()); assertEquals(1, manager.runtimeCount());
                scheduler.rejectCleanup = false;
                manager.chunkLoad(new ChunkLoadEvent(chunk(world), false));
                scheduler.runNext(scheduler.entities, "entity");
                verify(spawned.getFirst()).remove();
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                assertEquals(2, spawned.size()); assertEquals(1, manager.runtimeCount());
            } finally {
                scheduler.rejectCleanup = false; manager.close();
                if (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void transientUnloadCleanupRetriesAtLoadAndCoalescesUntilRemovalFinishes() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                Chunk chunk = chunk(world); scheduler.rejectCleanup = true;
                manager.chunkUnload(new ChunkUnloadEvent(chunk));
                assertEquals(1, manager.runtimeCount()); assertTrue(scheduler.global.isEmpty());
                assertTrue(scheduler.entities.isEmpty(), "rejection must not create an autonomous retry loop");
                scheduler.rejectCleanup = false;
                manager.chunkLoad(new ChunkLoadEvent(chunk, false));
                manager.chunkLoad(new ChunkLoadEvent(chunk, false));
                assertEquals(1, scheduler.entities.size(), "a queued exact predecessor cleanup must coalesce");
                assertEquals(1, spawned.size());
                scheduler.runNext(scheduler.entities, "entity"); verify(spawned.getFirst()).remove();
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                assertEquals(2, spawned.size()); assertEquals(1, manager.runtimeCount());
                assertTrue(scheduler.entities.isEmpty()); assertTrue(scheduler.global.isEmpty());
            } finally {
                scheduler.rejectCleanup = false; manager.close();
                if (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void queuedRemovedReservationWakesPendingCapacityWithoutEntity() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("queued-capacity.json")); await(registry.initialLoad());
            for (int i = 1; i <= VotingNPCStore.MAX_NPCS; i++) await(registry.create(definitionAt(i, i * 2)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                await(manager.remove("npc-1")); await(manager.create(definitionAt(1000, 2000)));
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.global, "global");
                for (int i = 0; i <= VotingNPCStore.MAX_NPCS; i++) scheduler.runNext(scheduler.regions, "region");
                assertTrue(scheduler.regions.isEmpty()); assertTrue(scheduler.global.isEmpty());
                assertEquals(VotingNPCStore.MAX_NPCS, manager.runtimeCount());
                assertEquals(VotingNPCStore.MAX_NPCS, spawned.size());
            } finally {
                manager.close(); while (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void unloadedQueuedReservationWithPendingCapacityDoesNotRetry() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(false);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("unloaded-capacity.json")); await(registry.initialLoad());
            for (int i = 1; i <= VotingNPCStore.MAX_NPCS; i++) await(registry.create(definitionAt(i, i * 2)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                await(manager.remove("npc-1")); await(manager.create(definitionAt(1000, 2000)));
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.global, "global");
                for (int i = 0; i <= VotingNPCStore.MAX_NPCS; i++) scheduler.runNext(scheduler.regions, "region");
                assertEquals(0, spawned.size()); assertTrue(scheduler.global.isEmpty());
                assertTrue(scheduler.regions.isEmpty()); assertEquals(0, manager.runtimeCount());
            } finally { manager.close(); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS)); }
        }
    }

    @Test void unloadedChunksNeverSpawnOrLoadAndLaterLoadReconstructsOnce() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                assertTrue(spawned.isEmpty()); assertEquals(0, manager.runtimeCount());
                verify(world, never()).getChunkAt(anyInt(), anyInt()); verify(world, never()).loadChunk(anyInt(), anyInt());
                when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
                manager.chunkLoad(new ChunkLoadEvent(chunk(world), false)); scheduler.runNext(scheduler.regions, "region");
                assertEquals(1, spawned.size());
                manager.chunkUnload(new ChunkUnloadEvent(chunk(world))); scheduler.runNext(scheduler.entities, "entity");
                assertTrue(scheduler.global.isEmpty(), "unload must not immediately retry reconstruction");
                assertTrue(scheduler.regions.isEmpty());
                manager.chunkLoad(new ChunkLoadEvent(chunk(world), false)); scheduler.runNext(scheduler.regions, "region");
                assertEquals(2, spawned.size());
            } finally { manager.close(); scheduler.runNext(scheduler.entities, "entity"); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS)); }
        }
    }

    @Test void partialSpawnFailureRemovesOnlyTheCapturedEntityAndCloseInvalidatesQueuedSpawns() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                // Close before a deferred creation runs: no entity may be created.
                manager.configure(false); scheduler.runNext(scheduler.regions, "region");
                assertTrue(spawned.isEmpty()); assertEquals(0, manager.runtimeCount());
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                doAnswer(invocation -> {
                    Villager entity = mock(Villager.class); when(entity.getUniqueId()).thenReturn(new UUID(0, 17));
                    doThrow(new IllegalStateException("Synthetic entity property failure")).when(entity).setAI(false);
                    spawned.add(entity);
                    Consumer<Villager> initializer = invocation.getArgument(2); initializer.accept(entity); return entity;
                }).when(world).spawn(any(Location.class), eq(Villager.class), any(Consumer.class));
                scheduler.runNext(scheduler.regions, "region");
                assertEquals(1, manager.runtimeCount());
                scheduler.runNext(scheduler.entities, "entity");
                verify(spawned.getFirst()).remove(); assertEquals(0, manager.runtimeCount());
                assertTrue(scheduler.global.isEmpty(), "failed spawn must not cause an automatic retry loop");
            } finally { manager.close(); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS)); }
        }
    }

    @Test void occupiedNeighbourUnloadRetiresAndReloadReconstructsOnceAtBothCoordinateSigns() throws Exception {
        for (double position : new double[] {15.75, -.25}) {
            var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
            try (var bukkit = mockStatic(Bukkit.class)) {
                World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
                bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
                when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
                var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
                var registry = new VotingNPCRegistry(directory.resolve("boundary-" + (position > 0) + ".json"));
                await(registry.initialLoad());
                var definition = new VotingNPCDefinition("voting", new UUID(0, 1), WORLD, position, 64, position, 0, 0, "Vote");
                await(registry.create(definition));
                var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
                try {
                    manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                    manager.chunkUnload(new ChunkUnloadEvent(atChunk(world, 7, 7)));
                    assertTrue(scheduler.entities.isEmpty(), "unrelated chunk unload must leave NPC alone");
                    int neighbour = definition.chunkX() + 1;
                    manager.chunkUnload(new ChunkUnloadEvent(atChunk(world, neighbour, neighbour)));
                    assertEquals(1, scheduler.entities.size(), "occupied adjacent chunk must trigger entity-owned cleanup");
                    // Bukkit may still report the unloading chunk as loaded during its event.
                    manager.chunkLoad(new ChunkLoadEvent(atChunk(world, definition.chunkX(), definition.chunkZ()), false));
                    scheduler.runNext(scheduler.entities, "entity");
                    assertEquals(0, manager.runtimeCount()); assertTrue(scheduler.regions.isEmpty());
                    manager.chunkLoad(new ChunkLoadEvent(atChunk(world, neighbour, neighbour), false));
                    manager.chunkLoad(new ChunkLoadEvent(atChunk(world, neighbour, neighbour), false));
                    scheduler.runNext(scheduler.regions, "region");
                    assertEquals(2, spawned.size()); assertEquals(1, manager.runtimeCount());
                    verify(world, never()).getChunkAt(anyInt(), anyInt()); verify(world, never()).loadChunk(anyInt(), anyInt());
                } finally {
                    manager.close(); if (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                    assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
                }
            }
        }
    }

    @Test void neighbourUnloadFenceSurvivesReconciliationAndBlocksDeferredCreation() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("pending-boundary.json")); await(registry.initialLoad());
            await(registry.create(new VotingNPCDefinition("voting", new UUID(0, 1), WORLD, 15.75, 64, 15.75, 0, 0, "Vote")));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global");
                manager.chunkUnload(new ChunkUnloadEvent(atChunk(world, 1, 1)));
                scheduler.runNext(scheduler.regions, "region");
                assertTrue(spawned.isEmpty(), "a deferred creation cannot cross an occupied chunk unload");
                assertEquals(0, manager.runtimeCount());
                await(manager.create(new VotingNPCDefinition("other", new UUID(0, 2), WORLD, 40, 64, 40, 0, 0, "Vote")));
                scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                assertEquals(1, spawned.size(), "unrelated mutation must not erase the first NPC's occupied-chunk fence");
                assertTrue(scheduler.regions.isEmpty());
            } finally {
                manager.close(); while (!scheduler.entities.isEmpty()) scheduler.runNext(scheduler.entities, "entity");
                assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS));
            }
        }
    }

    private static Chunk atChunk(World world, int x, int z) {
        Chunk chunk = mock(Chunk.class); when(chunk.getWorld()).thenReturn(world);
        when(chunk.getX()).thenReturn(x); when(chunk.getZ()).thenReturn(z); return chunk;
    }

    @Test void disabledFeatureDoesNotReadStorageOrRegisterAnyRuntimeWork() {
        var plugin = mock(VotingPluginMain.class);
        var scheduler = new Scheduler(plugin);
        var manager = new VotingNPCManager(plugin, scheduler, null, ignored -> fail("Disabled menu"));
        manager.configure(false);
        assertFalse(manager.available()); assertEquals(0, manager.runtimeCount());
        assertTrue(scheduler.global.isEmpty()); assertTrue(scheduler.regions.isEmpty());
        verify(plugin, never()).getDataFolder();
        manager.close(); manager.close();
    }

    @Test void worldUnloadPreservesDefinitionsAndAWorldLoadAfterCleanupRecreatesOnce() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            var manager = new VotingNPCManager(plugin, scheduler, registry, ignored -> { });
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                manager.worldUnload(new WorldUnloadEvent(world));
                // A load before delayed cleanup must record demand, not spawn a duplicate.
                manager.worldLoad(new WorldLoadEvent(world)); scheduler.runNext(scheduler.global, "global");
                assertEquals(1, spawned.size());
                scheduler.runNext(scheduler.entities, "entity"); scheduler.runNext(scheduler.global, "global");
                scheduler.runNext(scheduler.regions, "region");
                assertEquals(2, spawned.size()); assertEquals(List.of(definition(1)), manager.definitions());
            } finally { manager.close(); scheduler.runNext(scheduler.entities, "entity"); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS)); }
        }
    }

    @Test void cancelledAndOffhandClicksDoNotOpenAndRemovalInvalidatesAQueuedPlayerCallback() throws Exception {
        var plugin = mock(VotingPluginMain.class); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        var bukkitScheduler = mock(BukkitScheduler.class); when(plugin.getBukkitScheduler()).thenReturn(bukkitScheduler);
        var playerCallbacks = new LinkedBlockingQueue<Runnable>();
        doAnswer(invocation -> { playerCallbacks.add(invocation.getArgument(1)); return null; })
                .when(bukkitScheduler).runTask(eq(plugin), any(Runnable.class), any(Entity.class));
        try (var bukkit = mockStatic(Bukkit.class)) {
            World world = mock(World.class); when(world.getUID()).thenReturn(WORLD);
            bukkit.when(() -> Bukkit.getWorld(WORLD)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            var scheduler = new Scheduler(plugin); List<Villager> spawned = installSpawner(world, scheduler);
            var registry = new VotingNPCRegistry(directory.resolve("npcs.json")); await(registry.initialLoad());
            await(registry.create(definition(1)));
            List<Player> opened = new ArrayList<>();
            var manager = new VotingNPCManager(plugin, scheduler, registry, opened::add);
            try {
                manager.configure(true); scheduler.runNext(scheduler.global, "global"); scheduler.runNext(scheduler.regions, "region");
                Player player = mock(Player.class); when(player.getUniqueId()).thenReturn(new UUID(0, 50));
                when(player.isOnline()).thenReturn(true); when(player.getWorld()).thenReturn(world);
                when(player.getLocation()).thenReturn(new Location(world, 8, 64, 9));
                var cancelled = new PlayerInteractEntityEvent(player, spawned.getFirst(), EquipmentSlot.HAND); cancelled.setCancelled(true);
                manager.interact(cancelled);
                manager.interact(new PlayerInteractEntityEvent(player, spawned.getFirst(), EquipmentSlot.OFF_HAND));
                assertTrue(playerCallbacks.isEmpty());
                var click = new PlayerInteractEntityEvent(player, spawned.getFirst(), EquipmentSlot.HAND);
                manager.interact(click); assertTrue(click.isCancelled());
                assertEquals(1, playerCallbacks.size());
                playerCallbacks.remove().run();
                assertEquals(List.of(player), opened, "healthy main-hand selection opens on the player callback");
                Player second = mock(Player.class); when(second.getUniqueId()).thenReturn(new UUID(0, 51));
                when(second.isOnline()).thenReturn(true); when(second.getWorld()).thenReturn(world);
                when(second.getLocation()).thenReturn(new Location(world, 8, 64, 9));
                manager.interact(new PlayerInteractEntityEvent(second, spawned.getFirst(), EquipmentSlot.HAND));
                playerCallbacks.remove().run();
                assertEquals(List.of(player, second), opened, "one player's selection must not block another player");
                Player third = mock(Player.class); when(third.getUniqueId()).thenReturn(new UUID(0, 52));
                when(third.isOnline()).thenReturn(true); when(third.getWorld()).thenReturn(world);
                when(third.getLocation()).thenReturn(new Location(world, 8, 64, 9));
                manager.interact(new PlayerInteractEntityEvent(third, spawned.getFirst(), EquipmentSlot.HAND));
                await(manager.remove("voting"));
                playerCallbacks.remove().run(); assertEquals(List.of(player, second), opened);
            } finally { manager.close(); scheduler.runNext(scheduler.entities, "entity"); assertTrue(registry.awaitClosed(5, TimeUnit.SECONDS)); }
        }
    }

    @Test void productionMenuEntryDelegatesToTheExistingUrlCommandRatherThanAnotherMenuOrRewardPath() {
        var plugin = mock(VotingPluginMain.class);
        var unrelated = mock(com.bencodez.advancedcore.api.command.CommandHandler.class);
        var url = mock(com.bencodez.advancedcore.api.command.CommandHandler.class);
        var later = mock(com.bencodez.advancedcore.api.command.CommandHandler.class);
        var player = mock(Player.class);
        when(plugin.getVoteCommand()).thenReturn(new ArrayList<>(List.of(unrelated, url, later)));
        when(url.runCommand(eq(player), aryEq(new String[] { "URL" }))).thenReturn(true);
        VotingNPCManager.openVotingSites(plugin, player);
        verify(unrelated).runCommand(eq(player), aryEq(new String[] { "URL" }));
        verify(url).runCommand(eq(player), aryEq(new String[] { "URL" }));
        verifyNoInteractions(later);
        verify(player, never()).performCommand(anyString());
    }

    private static Chunk chunk(World world) {
        Chunk chunk = mock(Chunk.class); when(chunk.getWorld()).thenReturn(world); when(chunk.getX()).thenReturn(0); when(chunk.getZ()).thenReturn(0); return chunk;
    }

    @SuppressWarnings("unchecked")
    private static List<Villager> installSpawner(World world, Scheduler scheduler) {
        List<Villager> spawned = new ArrayList<>();
        doAnswer(invocation -> {
            assertEquals("region", scheduler.owner);
            Villager entity = mock(Villager.class); when(entity.getUniqueId()).thenReturn(new UUID(2, spawned.size() + 1));
            when(entity.isValid()).thenReturn(true);
            Consumer<Villager> initializer = invocation.getArgument(2); initializer.accept(entity);
            doAnswer(removal -> { assertEquals("entity", scheduler.owner); return null; }).when(entity).remove();
            spawned.add(entity); return entity;
        }).when(world).spawn(any(Location.class), eq(Villager.class), any(Consumer.class));
        return spawned;
    }
}
