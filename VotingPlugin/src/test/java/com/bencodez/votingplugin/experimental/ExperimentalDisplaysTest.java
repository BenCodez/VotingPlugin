package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Interaction;
import org.bukkit.entity.Player;
import org.bukkit.entity.TextDisplay;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;

class ExperimentalDisplaysTest {
    private static final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        final ExperimentalSessions sessions = new ExperimentalSessions(System::nanoTime, failure -> fail(failure));
        final ExperimentalDisplays displays = new ExperimentalDisplays(plugin, scheduler, sessions);
        final UUID owner = UUID.randomUUID();
        final UUID worldId = UUID.randomUUID();
        final World world = mock(World.class);
        final Player player = mock(Player.class);
        final TextDisplay text = mock(TextDisplay.class);
        final Interaction interaction = mock(Interaction.class);
        final Location anchor = new Location(world, 8, 64, 8);
        final Queue<Runnable> players = new ArrayDeque<>(), regions = new ArrayDeque<>();
        final AtomicInteger spawns = new AtomicInteger(), selections = new AtomicInteger();
        final AtomicReference<ExperimentalDisplays.Target> selected = new AtomicReference<>();
        final MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class);
        boolean playerOwner, regionOwner;
        boolean owns = true;
        ExperimentalSessions.Session session;

        Fixture() {
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
            when(player.getUniqueId()).thenReturn(owner);
            when(player.isOnline()).thenAnswer(i -> { assertTrue(playerOwner); return true; });
            when(player.hasPermission(anyString())).thenAnswer(i -> { assertTrue(playerOwner); return true; });
            when(player.getWorld()).thenAnswer(i -> { assertTrue(playerOwner); return world; });
            when(player.getEyeLocation()).thenAnswer(i -> { assertTrue(playerOwner); return anchor; });
            when(world.getUID()).thenReturn(worldId);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenAnswer(i -> { assertTrue(regionOwner || playerOwner); return true; });
            bukkit.when(() -> Bukkit.getWorld(worldId)).thenReturn(world);
            when(scheduler.owns(any())).thenAnswer(i -> owns && (regionOwner || playerOwner));
            doAnswer(i -> { players.add(i.getArgument(1)); return null; }).when(scheduler).player(any(), any(), any());
            doAnswer(i -> { regions.add(i.getArgument(1)); return null; }).when(scheduler).region(any(), any());
            when(text.getUniqueId()).thenReturn(UUID.randomUUID());
            when(interaction.getUniqueId()).thenReturn(UUID.randomUUID());
            when(text.isValid()).thenReturn(true);
            when(interaction.isValid()).thenReturn(true);
            doAnswer(i -> { assertTrue(regionOwner || playerOwner); return null; }).when(text).remove();
            doAnswer(i -> { assertTrue(regionOwner || playerOwner); return null; }).when(interaction).remove();
            doAnswer(i -> {
                Class<?> type = i.getArgument(1);
                Consumer<Entity> init = i.getArgument(2);
                Entity entity = type == TextDisplay.class ? text : interaction;
                spawns.incrementAndGet(); init.accept(entity); return entity;
            }).when(world).spawn(any(Location.class), any(Class.class), any());
            session = sessions.open(owner, ExperimentalGUIType.RADIAL, 64, 60);
        }

        void open(List<ExperimentalDisplays.Target> targets) {
            displays.render(session, player, anchor, targets, target -> { assertTrue(playerOwner); selected.set(target); selections.incrementAndGet(); });
        }
        void playerNext() { playerOwner = true; try { players.remove().run(); } finally { playerOwner = false; } }
        void regionNext() { regionOwner = true; try { regions.remove().run(); } finally { regionOwner = false; } }
        void spawnFrame() { open(List.of(new ExperimentalDisplays.Target("site-a", "Site A", null, 0, 0, ExperimentalDisplays.Action.SITE))); playerNext(); regionNext(); while (!players.isEmpty()) playerNext(); }
        PlayerInteractEntityEvent click(Entity entity) {
            PlayerInteractEntityEvent event = mock(PlayerInteractEntityEvent.class);
            when(event.getPlayer()).thenReturn(player); when(event.getRightClicked()).thenReturn(entity); when(event.getHand()).thenReturn(EquipmentSlot.HAND); return event;
        }
        @Override public void close() { bukkit.close(); }
    }

    @Test void rendersBoundedOwnedFrameAndDispatchesStableTargetOnOwnerScheduler() {
        try (Fixture f = new Fixture()) {
            f.spawnFrame();
            assertEquals(2, f.spawns.get());
            assertEquals(2, f.displays.entityCount());
            PlayerInteractEntityEvent click = f.click(f.interaction);
            f.displays.interact(click);
            verify(click).setCancelled(true);
            assertEquals(0, f.selections.get());
            f.playerNext();
            assertEquals("site-a", f.selected.get().id());
        }
    }

    @Test void fullTenTargetFrameHasThirtyEntitiesAndCleanupRemovesEveryOwnedEntity() {
        try (Fixture f = new Fixture()) {
            f.bukkit.when(Bukkit::getItemFactory).thenReturn(mock(org.bukkit.inventory.ItemFactory.class));
            java.util.List<Entity> owned = new java.util.ArrayList<>();
            doAnswer(inv -> {
                assertTrue(f.regionOwner);
                Class<? extends Entity> type = inv.getArgument(1);
                Entity entity = mock(type);
                when(entity.getUniqueId()).thenReturn(UUID.randomUUID());
                when(entity.isValid()).thenReturn(true);
                Consumer<Entity> initializer = inv.getArgument(2);
                owned.add(entity); initializer.accept(entity); return entity;
            }).when(f.world).spawn(any(Location.class), any(Class.class), any());
            java.util.List<ExperimentalDisplays.Target> targets = new java.util.ArrayList<>();
            for (int i = 0; i < 10; i++) targets.add(new ExperimentalDisplays.Target("target" + i, "Site " + i,
                    new org.bukkit.inventory.ItemStack(org.bukkit.Material.CHEST), 0, 0, ExperimentalDisplays.Action.INFO));
            f.open(targets); f.playerNext(); f.regionNext(); while (!f.players.isEmpty()) f.playerNext();
            assertEquals(30, owned.size());
            assertEquals(30, f.displays.entityCount());
            f.displays.close(f.session); f.regionNext();
            assertEquals(0, f.displays.entityCount());
            for (Entity entity : owned) verify(entity).remove();
        }
    }

    @Test void oversizedFrameIsRejectedBeforeSchedulingOrSpawning() {
        try (Fixture f = new Fixture()) {
            var target = new ExperimentalDisplays.Target("x", "x", null, 0, 0, ExperimentalDisplays.Action.INFO);
            f.open(java.util.Collections.nCopies(11, target));
            assertTrue(f.players.isEmpty());
            assertTrue(f.regions.isEmpty());
            assertFalse(f.sessions.current(f.session));
            assertEquals(0, f.spawns.get());
        }
    }

    @Test void showcaseRotationUsesRegionOwnerAndRetiresOnOwnershipLoss() {
        try (Fixture f = new Fixture()) {
            f.bukkit.when(Bukkit::getItemFactory).thenReturn(mock(org.bukkit.inventory.ItemFactory.class));
            var item = mock(org.bukkit.entity.ItemDisplay.class);
            when(item.getUniqueId()).thenReturn(UUID.randomUUID()); when(item.isValid()).thenReturn(true);
            doAnswer(inv -> { assertTrue(f.regionOwner); return null; }).when(item).setRotation(anyFloat(), anyFloat());
            doAnswer(inv -> {
                Class<?> type = inv.getArgument(1);
                Entity entity = type == TextDisplay.class ? f.text : type == org.bukkit.entity.ItemDisplay.class ? item : f.interaction;
                Consumer<Entity> initializer = inv.getArgument(2); initializer.accept(entity); return entity;
            }).when(f.world).spawn(any(Location.class), any(Class.class), any());
            f.session = f.sessions.open(f.owner, ExperimentalGUIType.REWARD_SHOWCASE, 64, 60);
            f.open(List.of(new ExperimentalDisplays.Target("reward", "Reward", new org.bukkit.inventory.ItemStack(org.bukkit.Material.CHEST), 0, 0, ExperimentalDisplays.Action.INFO)));
            f.playerNext(); f.regionNext(); while (!f.players.isEmpty()) f.playerNext();
            f.displays.rotateShowcase(f.session, 1);
            verify(item, never()).setRotation(anyFloat(), anyFloat());
            f.regionNext(); verify(item).setRotation(15f, 0f);
            f.owns = false;
            f.displays.rotateShowcase(f.session, 2); f.regionNext();
            assertFalse(f.sessions.current(f.session));
            while (!f.regions.isEmpty()) f.regionNext(); // Retained cleanup fails safe while unowned.
            f.owns = true; f.displays.retryCleanup(); f.regionNext();
            assertEquals(0, f.displays.entityCount());
            f.displays.rotateShowcase(f.session, 3);
            assertTrue(f.regions.isEmpty());
            verify(item, times(1)).setRotation(anyFloat(), anyFloat());
        }
    }

    @Test void identicalFrameDoesNotRespawnOrRegisterAnotherCleanup() {
        try (Fixture f = new Fixture()) {
            List<ExperimentalDisplays.Target> targets = List.of(new ExperimentalDisplays.Target("site-a", "Site A", null, 0, 0, ExperimentalDisplays.Action.SITE));
            f.open(targets); f.open(targets);
            assertEquals(1, f.players.size());
            f.playerNext(); f.regionNext(); while (!f.players.isEmpty()) f.playerNext();
            assertEquals(2, f.spawns.get());
        }
    }

    @Test void completeFootprintIsPreflightedBeforeAnySpawn() {
        try (Fixture f = new Fixture()) {
            f.owns = false;
            f.open(List.of(new ExperimentalDisplays.Target("site-a", "Site A", null, 0, 0, ExperimentalDisplays.Action.SITE)));
            f.playerNext();
            assertEquals(1, f.regions.size());
            f.regions.remove().run();
            assertEquals(0, f.spawns.get());
        }
    }

    @Test void foreignPlayerAndOffhandCannotDispatchTarget() {
        try (Fixture f = new Fixture()) {
            f.spawnFrame();
            Player stranger = mock(Player.class);
            when(stranger.getUniqueId()).thenReturn(UUID.randomUUID());
            PlayerInteractEntityEvent foreign = f.click(f.interaction); when(foreign.getPlayer()).thenReturn(stranger); f.displays.interact(foreign);
            PlayerInteractEntityEvent offhand = f.click(f.interaction); when(offhand.getHand()).thenReturn(EquipmentSlot.OFF_HAND); f.displays.interact(offhand);
            assertEquals(0, f.selections.get());
            verify(foreign).setCancelled(true); verify(offhand).setCancelled(true);
        }
    }

    @Test void failedRegionCleanupIsRetainedAndRetryable() {
        try (Fixture f = new Fixture()) {
            f.spawnFrame();
            f.owns = false;
            doAnswer(i -> { if (!f.owns) throw new IllegalStateException("region rejected"); f.regions.add(i.getArgument(1)); return null; })
                    .when(f.scheduler).region(any(), any());
            f.displays.close(f.session);
            assertEquals(2, f.displays.entityCount());
            f.owns = true;
            f.displays.retryCleanup();
            f.regionNext();
            assertEquals(0, f.displays.entityCount());
        }
    }

    @Test void partialSpawnIsOwnedBeforeSetterFailureAndCanBeRemoved() {
        try (Fixture f = new Fixture()) {
            doAnswer(i -> { f.owns = false; throw new IllegalStateException("text configuration failed"); })
                    .when(f.text).setText(anyString());
            f.open(List.of(new ExperimentalDisplays.Target("site-a", "Site A", null, 0, 0, ExperimentalDisplays.Action.SITE)));
            f.playerNext();
            f.regionNext();
            assertEquals(1, f.displays.entityCount());
            f.owns = true;
            f.regionNext();
            assertEquals(0, f.displays.entityCount());
        }
    }


    @Test void targetBoundsAndCapabilityProbeAreSafe() {
        assertThrows(IllegalArgumentException.class, () -> new ExperimentalDisplays.Target("x", "x", null, 9, 0, ExperimentalDisplays.Action.SITE));
        assertTrue(ExperimentalDisplays.supported());
    }
}
