package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.bukkit.entity.Villager;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.entity.EntityTeleportEvent;
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.world.WorldUnloadEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;

class ExperimentalNPCTest {
    private static final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        final AtomicLong clock = new AtomicLong();
        final ExperimentalSessions sessions = new ExperimentalSessions(clock::get, failure -> fail(failure));
        final ExperimentalNPC npc = new ExperimentalNPC(plugin, scheduler, sessions);
        final UUID owner = UUID.randomUUID();
        final UUID worldId = UUID.randomUUID();
        final World world = mock(World.class);
        final Player player = mock(Player.class);
        final Villager villager = mock(Villager.class);
        final Location anchor = new Location(world, 8, 64, 8);
        final Queue<Runnable> players = new ArrayDeque<>();
        final Queue<Runnable> regions = new ArrayDeque<>();
        final AtomicInteger selections = new AtomicInteger();
        final AtomicInteger spawns = new AtomicInteger();
        final MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class);
        ExperimentalSessions.Session session;
        boolean inPlayer;
        boolean inRegion;
        boolean owns = true;

        Fixture() {
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
            when(world.getUID()).thenAnswer(inv -> { assertTrue(inRegion || inPlayer && owns); return worldId; });
            bukkit.when(() -> Bukkit.getWorld(worldId)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenAnswer(inv -> { assertTrue(inRegion); return true; });
            when(player.getUniqueId()).thenReturn(owner);
            when(player.isOnline()).thenAnswer(inv -> { assertTrue(inPlayer); return true; });
            when(player.hasPermission(anyString())).thenAnswer(inv -> { assertTrue(inPlayer); return true; });
            when(player.getWorld()).thenAnswer(inv -> { assertTrue(inPlayer); return world; });
            when(player.getEyeLocation()).thenAnswer(inv -> { assertTrue(inPlayer); return anchor; });
            when(villager.getUniqueId()).thenReturn(UUID.randomUUID());
            when(villager.isValid()).thenAnswer(inv -> { assertTrue(inRegion); return true; });
            doAnswer(inv -> { assertTrue(inRegion || inPlayer && owns); return null; }).when(villager).remove();
            doAnswer(inv -> {
                assertTrue(inPlayer);
                assertTrue(owns);
                return null;
            }).when(player).showEntity(plugin, villager);
            doAnswer(inv -> { players.add(inv.getArgument(1)); return null; })
                    .when(scheduler).player(any(), any(), any());
            doAnswer(inv -> { regions.add(inv.getArgument(1)); return null; })
                    .when(scheduler).region(any(), any());
            when(scheduler.owns(any())).thenAnswer(inv -> owns && (inPlayer || inRegion));
            doAnswer(inv -> {
                assertTrue(inRegion);
                assertTrue(owns);
                spawns.incrementAndGet();
                inv.getArgument(2, Consumer.class).accept(villager);
                return villager;
            }).when(world).spawn(any(Location.class), eq(Villager.class), any());
            session = sessions.open(owner, ExperimentalGUIType.NPC, 64, 60);
        }

        void open() { npc.open(session, player, anchor, "Voting guide", () -> {
            assertTrue(inPlayer);
            selections.incrementAndGet();
        }); }

        void playerNext() {
            inPlayer = true;
            try { players.remove().run(); } finally { inPlayer = false; }
        }

        void regionNext() {
            inRegion = true;
            try { regions.remove().run(); } finally { inRegion = false; }
        }

        void spawned() { open(); playerNext(); regionNext(); playerNext(); }

        PlayerInteractEntityEvent click(Player who, Villager target) {
            PlayerInteractEntityEvent event = mock(PlayerInteractEntityEvent.class);
            when(event.getPlayer()).thenReturn(who);
            when(event.getRightClicked()).thenReturn(target);
            when(event.getHand()).thenReturn(EquipmentSlot.HAND);
            return event;
        }

        @Override public void close() { bukkit.close(); }
    }

    @Test void spawnAndVisibilityRunOnTheirOwnersAndRefreshCannotDuplicate() {
        try (Fixture f = new Fixture()) {
            assertTrue(ExperimentalNPC.supported());
            f.open();
            f.open();
            assertEquals(1, f.players.size());
            assertEquals(0, f.spawns.get());
            f.playerNext();
            assertEquals(0, f.spawns.get());
            f.regionNext();
            assertEquals(1, f.npc.entityCount());
            verify(f.player, never()).showEntity(any(), any());
            f.playerNext();
            assertEquals(ExperimentalSessions.State.ACTIVE, f.session.state());
            verify(f.villager).setPersistent(false);
            verify(f.villager).setAI(false);
            verify(f.villager).setGravity(false);
            verify(f.villager).setInvulnerable(true);
            verify(f.villager).setSilent(true);
            verify(f.villager).setCollidable(false);
            verify(f.villager).setCanPickupItems(false);
            verify(f.villager).setCustomName("Voting guide");
            verify(f.villager).setVisibleByDefault(false);
            verify(f.player).showEntity(f.plugin, f.villager);
            f.open();
            assertTrue(f.players.isEmpty());
            assertEquals(1, f.spawns.get());
        }
    }

    @Test void onlyExactOwnedEntityAndOwningPlayersMainHandCanSelectOnce() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            Villager unrelated = mock(Villager.class);
            when(unrelated.getUniqueId()).thenReturn(UUID.randomUUID());
            var normal = f.click(f.player, unrelated);
            f.npc.interact(normal);
            verify(normal, never()).setCancelled(anyBoolean());
            Player stranger = mock(Player.class);
            when(stranger.getUniqueId()).thenReturn(UUID.randomUUID());
            var wrongPlayer = f.click(stranger, f.villager);
            f.npc.interact(wrongPlayer);
            verify(wrongPlayer).setCancelled(true);
            var offhand = f.click(f.player, f.villager);
            when(offhand.getHand()).thenReturn(EquipmentSlot.OFF_HAND);
            f.npc.interact(offhand);
            var cancelled = f.click(f.player, f.villager);
            when(cancelled.isCancelled()).thenReturn(true);
            f.npc.interact(cancelled);
            assertTrue(f.players.isEmpty());
            var owner = f.click(f.player, f.villager);
            f.npc.interact(owner);
            verify(owner).setCancelled(true);
            PlayerInteractAtEntityEvent duplicate = mock(PlayerInteractAtEntityEvent.class);
            when(duplicate.getPlayer()).thenReturn(f.player);
            when(duplicate.getRightClicked()).thenReturn(f.villager);
            when(duplicate.getHand()).thenReturn(EquipmentSlot.HAND);
            f.npc.interactAt(duplicate);
            assertEquals(1, f.players.size());
            assertEquals(0, f.selections.get());
            f.playerNext();
            assertEquals(1, f.selections.get());
        }
    }

    @Test void protectionDoesNotInterceptUnrelatedVillagers() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            Villager unrelated = mock(Villager.class);
            when(unrelated.getUniqueId()).thenReturn(UUID.randomUUID());
            for (Villager entity : new Villager[] { f.villager, unrelated }) {
                EntityDamageEvent damage = mock(EntityDamageEvent.class);
                when(damage.getEntity()).thenReturn(entity);
                EntityTeleportEvent teleport = mock(EntityTeleportEvent.class);
                when(teleport.getEntity()).thenReturn(entity);
                f.npc.damage(damage);
                f.npc.teleport(teleport);
                if (entity == f.villager) {
                    verify(damage).setCancelled(true);
                    verify(teleport).setCancelled(true);
                } else {
                    verify(damage, never()).setCancelled(anyBoolean());
                    verify(teleport, never()).setCancelled(anyBoolean());
                }
            }
        }
    }

    @Test void closeBeforePlayerAdmissionAndBeforeRegionSpawnFencesLateCallbacks() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.sessions.close(f.session);
            f.playerNext();
            assertTrue(f.regions.isEmpty());
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.open();
            f.playerNext();
            f.sessions.close(f.session);
            f.regionNext();
            assertEquals(0, f.spawns.get());
            assertEquals(0, f.npc.entityCount());
        }
    }

    @Test void replacementTimeoutAndShutdownInvalidatePendingSpawns() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.playerNext();
            var replacement = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.regionNext();
            assertTrue(f.sessions.current(replacement));
            f.session = replacement;
            f.open();
            f.playerNext();
            f.clock.set(60_000_000_000L);
            f.regionNext();
            assertEquals(ExperimentalSessions.State.CLOSED, replacement.state());
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.open();
            f.playerNext();
            f.sessions.clear(true);
            f.regionNext();
            assertEquals(0, f.spawns.get());
        }
    }

    @Test void cleanupIsIdempotentAndUsesFixedRegionEvenAfterClose() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            f.sessions.close(f.session);
            f.sessions.close(f.session);
            assertEquals(1, f.regions.size());
            verify(f.villager, never()).remove();
            f.regionNext();
            verify(f.villager).remove();
            assertEquals(0, f.npc.entityCount());
            assertTrue(f.sessions.snapshot().isEmpty());
        }
    }

    @Test void pendingSelectionAndVisibilityCannotActAfterClose() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.playerNext();
            f.regionNext();
            f.sessions.close(f.session);
            f.playerNext();
            verify(f.player, never()).showEntity(any(), any());
            f.regionNext();
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.spawned();
            f.npc.interact(f.click(f.player, f.villager));
            f.sessions.close(f.session);
            f.playerNext();
            assertEquals(0, f.selections.get());
            f.regionNext();
        }
    }

    @Test void selectionRevalidatesWorldDistanceAndPermissionOnPlayersScheduler() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            f.npc.interact(f.click(f.player, f.villager));
            doReturn(new Location(f.world, 100, 64, 100)).when(f.player).getEyeLocation();
            f.playerNext();
            assertEquals(0, f.selections.get());
            assertFalse(f.sessions.current(f.session));
            assertEquals(0, f.npc.entityCount());
            doReturn(f.anchor).when(f.player).getEyeLocation();
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.spawned();
            f.npc.interact(f.click(f.player, f.villager));
            doReturn(false).when(f.player).hasPermission(anyString());
            f.playerNext();
            assertEquals(0, f.selections.get());
            assertEquals(0, f.npc.entityCount());
        }
    }

    @Test void unownedAndUnloadedAreasNeverSpawnOrLoadChunks() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.playerNext();
            f.owns = false;
            f.regionNext();
            assertEquals(0, f.spawns.get());
            verify(f.world, never()).isChunkLoaded(anyInt(), anyInt());
            f.owns = true;
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            doReturn(false).when(f.world).isChunkLoaded(anyInt(), anyInt());
            f.open();
            f.playerNext();
            f.regionNext();
            assertEquals(0, f.spawns.get());
            verify(f.world, never()).getChunkAt(anyInt(), anyInt());
            verify(f.world, never()).loadChunk(anyInt(), anyInt());
            assertTrue(f.sessions.snapshot().isEmpty());
        }
    }

    @Test void neighboringRegionBoundaryAndUnloadedWorldAreRejectedBeforeSpawn() {
        try (Fixture f = new Fixture()) {
            doAnswer(inv -> {
                Location location = inv.getArgument(0);
                return (f.inPlayer || f.inRegion) && location.getX() <= 8;
            }).when(f.scheduler).owns(any());
            f.open();
            f.playerNext();
            f.regionNext();
            assertEquals(0, f.spawns.get());
            doAnswer(inv -> f.inPlayer || f.inRegion).when(f.scheduler).owns(any());
            f.bukkit.when(() -> Bukkit.getWorld(f.worldId)).thenReturn(null);
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            f.open();
            f.playerNext();
            f.regionNext();
            assertEquals(0, f.spawns.get());
            verify(f.world, never()).isChunkLoaded(anyInt(), anyInt());
        }
    }

    @Test void playerThreadCannotShowAcrossAnUnownedRegionBoundary() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.playerNext();
            f.regionNext();
            f.owns = false;
            f.playerNext();
            verify(f.player, never()).showEntity(any(), any());
            assertFalse(f.sessions.current(f.session));
            f.owns = true;
            f.regionNext();
            verify(f.villager).remove();
        }
    }

    @Test void partialSpawnFailureRemovesCapturedEntityOnOwningRegion() {
        try (Fixture f = new Fixture()) {
            doThrow(new IllegalStateException("configuration failed")).when(f.villager).setAI(false);
            f.open();
            f.playerNext();
            f.regionNext();
            verify(f.villager).setPersistent(false);
            verify(f.villager).remove();
            assertEquals(0, f.npc.entityCount());
            assertTrue(f.sessions.snapshot().isEmpty());
            assertTrue(f.players.isEmpty());
        }
    }

    @Test void cancelledSpawnAndRejectedSchedulingDoNotLeaveSessionsOrEntities() {
        try (Fixture f = new Fixture()) {
            doReturn(false).when(f.villager).isValid();
            f.open();
            f.playerNext();
            f.regionNext();
            verify(f.villager).remove();
            assertEquals(0, f.npc.entityCount());
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            doThrow(new IllegalStateException("region rejected")).when(f.scheduler).region(any(), any());
            f.open();
            f.playerNext();
            assertEquals(1, f.spawns.get());
            assertTrue(f.sessions.snapshot().isEmpty());
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.NPC, 64, 60);
            doThrow(new IllegalStateException("player rejected")).when(f.scheduler).player(any(), any(), any());
            f.open();
            assertTrue(f.sessions.snapshot().isEmpty());
        }
    }

    @Test void rejectedCleanupRetainsExactOwnershipUntilOwnedRetryOrWorldUnload() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            doThrow(new IllegalStateException("region stopped")).when(f.scheduler).region(any(), any());
            f.sessions.close(f.session);
            assertEquals(1, f.npc.entityCount());
            assertTrue(f.sessions.snapshot().isEmpty());
            EntityDamageEvent damage = mock(EntityDamageEvent.class);
            when(damage.getEntity()).thenReturn(f.villager);
            f.inRegion = true;
            try { f.npc.damage(damage); } finally { f.inRegion = false; }
            verify(damage).setCancelled(true);
            verify(f.villager).remove();
            assertEquals(0, f.npc.entityCount());
        }
        try (Fixture f = new Fixture()) {
            f.spawned();
            doThrow(new IllegalStateException("region stopped")).when(f.scheduler).region(any(), any());
            f.sessions.close(f.session);
            WorldUnloadEvent unload = mock(WorldUnloadEvent.class);
            when(unload.getWorld()).thenReturn(f.world);
            f.npc.unload(unload);
            assertEquals(0, f.npc.entityCount());
        }
    }

    @Test void requiredPersistenceImplementationFailureNeverFallsBackToSavedVillager() {
        try (Fixture f = new Fixture()) {
            doThrow(new AbstractMethodError("old entity implementation")).when(f.villager).setPersistent(false);
            f.open();
            f.playerNext();
            f.regionNext();
            verify(f.villager).remove();
            assertEquals(0, f.npc.entityCount());
            assertTrue(f.sessions.snapshot().isEmpty());
        }
    }

    @Test void cleanupCommandCanRetryRetiredRecordsWithoutTouchingActiveNPCs() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            f.npc.retryCleanup();
            assertTrue(f.regions.isEmpty());
            doThrow(new IllegalStateException("region stopped")).when(f.scheduler).region(any(), any());
            f.sessions.close(f.session);
            assertEquals(1, f.npc.entityCount());
            doAnswer(inv -> { f.regions.add(inv.getArgument(1)); return null; })
                    .when(f.scheduler).region(any(), any());
            f.npc.retryCleanup();
            f.npc.retryCleanup();
            assertEquals(1, f.regions.size());
            f.regionNext();
            assertEquals(0, f.npc.entityCount());
            verify(f.villager).remove();
        }
    }

    @Test void playerWorldChangeAndRetirementCallbackInvalidateSelections() {
        try (Fixture f = new Fixture()) {
            f.spawned();
            f.npc.interact(f.click(f.player, f.villager));
            doReturn(mock(World.class)).when(f.player).getWorld();
            f.playerNext();
            assertEquals(0, f.selections.get());
            assertEquals(0, f.npc.entityCount());
        }
        try (Fixture f = new Fixture()) {
            doAnswer(inv -> { inv.getArgument(2, Runnable.class).run(); return null; })
                    .when(f.scheduler).player(any(), any(), any());
            f.open();
            assertTrue(f.sessions.snapshot().isEmpty());
            assertEquals(0, f.spawns.get());
            assertEquals(0, f.npc.entityCount());
        }
    }
}
