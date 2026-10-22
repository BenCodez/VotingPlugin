package com.bencodez.votingplugin.tests.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Interaction;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.DynamicTest;

import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.mockito.MockedStatic;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.experimental.ExperimentalGUIManager;
import com.bencodez.votingplugin.experimental.ExperimentalGUIType;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;
import com.bencodez.votingplugin.votesites.VoteSite;

class ExperimentalRadialFloorTest {
    private static final class NativeFrame implements AutoCloseable {
        final Fixture f = new Fixture();
        final Queue<Runnable> regions = new ArrayDeque<>();
        final List<Location> hits = new ArrayList<>();
        final List<Entity> entities = new ArrayList<>();
        final List<Interaction> hitEntities = new ArrayList<>();
        final Location feet;
        final Location eyes;

        NativeFrame(float pitch) {
            this(90, pitch, 1.25, 2.5, 1.62, 5);
        }

        NativeFrame(float yaw, float pitch, double radius, double distance, double eyeHeight, int siteCount) {
            World world = f.player.getWorld();
            feet = new Location(world, 8, 64, 8);
            eyes = new Location(world, 8, feet.getY() + eyeHeight, 8, yaw, pitch);
            f.config.set("Experimental.VoteGUIs.Radial.Radius", radius);
            f.config.set("Experimental.VoteGUIs.Distance", distance);
            when(f.player.getLocation()).thenAnswer(inv -> feet.clone());
            when(f.player.getEyeLocation()).thenAnswer(inv -> eyes.clone());
            when(f.player.getEyeHeight()).thenReturn(eyeHeight);
            UUID worldId = UUID.randomUUID();
            when(world.getUID()).thenReturn(worldId);
            f.bukkit.when(() -> Bukkit.getWorld(worldId)).thenReturn(world);
            when(world.isChunkLoaded(anyInt(), anyInt())).thenReturn(true);
            when(f.scheduler.owns(any(Location.class))).thenReturn(true);
            doAnswer(inv -> { regions.add(inv.getArgument(1)); return null; })
                    .when(f.scheduler).region(any(), any());
            doAnswer(inv -> {
                Class<? extends Entity> type = inv.getArgument(1);
                Entity entity = mock(type);
                when(entity.getUniqueId()).thenReturn(UUID.randomUUID());
                when(entity.isValid()).thenReturn(true);
                entities.add(entity);
                if (type == Interaction.class) {
                    hits.add(((Location) inv.getArgument(0)).clone());
                    hitEntities.add((Interaction) entity);
                }
                Consumer<Entity> initialize = inv.getArgument(2);
                initialize.accept(entity);
                return entity;
            }).when(world).spawn(any(Location.class), any(Class.class), any());
            var sites = new ArrayList<com.bencodez.votingplugin.votesites.VoteSite>();
            for (int i = 0; i < siteCount; i++) sites.add(f.site("site" + i, "https://example.test/" + i));
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(sites);
        }

        void open() {
            f.manager.open(f.player, ExperimentalGUIType.RADIAL);
            f.owner();
            assertEquals(1, f.storage.size());
            f.snapshot();
            assertTrue(hits.isEmpty(), "Native entities must wait for the region callback");
            while (!regions.isEmpty()) regions.remove().run();
            f.owner();
            assertEquals(23, entities.size(), "Exercise the real full-page native renderer");
            assertEquals(9, hits.size(), "Five sites, title and three controls");
        }

        void nextPage() {
            PlayerInteractEntityEvent click = mock(PlayerInteractEntityEvent.class);
            when(click.getPlayer()).thenReturn(f.player);
            when(click.getRightClicked()).thenReturn(hitEntities.get(7));
            when(click.getHand()).thenReturn(EquipmentSlot.HAND);
            f.interact(click);
            verify(click).setCancelled(true);
            for (int i = 0; i < 8 && (!f.owner.isEmpty() || !regions.isEmpty()); i++) {
                f.owner();
                while (!regions.isEmpty()) regions.remove().run();
            }
            f.owner();
            assertTrue(regions.isEmpty());
            assertEquals(34, entities.size(), "Second page uses one site, title and three controls");
            assertEquals(14, hits.size());
        }

        @Override public void close() {
            f.manager.clear();
            f.owner();
            while (!regions.isEmpty()) regions.remove().run();
            f.owner();
            for (Entity entity : entities) verify(entity).remove();
            f.close();
        }
    }

    @Test void defaultLevelRadialControlsStayAboveTheFlatFloor() {
        try (var frame = new NativeFrame(0)) {
            frame.open();
            assertTrue(frame.hits.stream().allMatch(hit -> hit.getY() >= frame.feet.getY()),
                    "Every native interaction, including navigation controls, must clear the foot plane");
        }
    }

    @Test void lookingDownDoesNotBuryTheRadialSitesOrControls() {
        try (var frame = new NativeFrame(75)) {
            frame.open();
            assertTrue(frame.hits.stream().allMatch(hit -> hit.getY() >= frame.feet.getY()),
                    "Downward viewing pitch must not place native interactions below the flat floor");
        }
    }

    @Test void lookingVerticallyPreservesConfiguredHorizontalDistance() {
        try (var frame = new NativeFrame(90)) {
            frame.open();
            Location middleControl = frame.hits.get(7);
            double horizontal = Math.hypot(middleControl.getX() - frame.eyes.getX(),
                    middleControl.getZ() - frame.eyes.getZ());
            assertEquals(2.5, horizontal, 1e-9);
        }
    }

    @TestFactory Stream<DynamicTest> radiusPoseAndHeadingBoundaryCases() {
        return Stream.of(new double[] {0, -90, .9, 1.5, 1.62}, new double[] {90, 90, 1.8, 3, 1.62},
                new double[] {180, 75, .9, 2.5, 1.27}, new double[] {270, -75, 1.8, 2.5, 1.27},
                new double[] {45, 0, 1.25, 2.5, 1.62}, new double[] {225, 90, 1.25, 1.5, 1.27})
                .map(row -> DynamicTest.dynamicTest("yaw " + row[0] + ", pitch " + row[1] + ", radius " + row[2],
                        () -> radiusPoseAndHeadingBoundsKeepNativeHitboxesClear((float) row[0], (float) row[1],
                                row[2], row[3], row[4])));
    }

    void radiusPoseAndHeadingBoundsKeepNativeHitboxesClear(float yaw, float pitch, double radius,
            double distance, double eyeHeight) {
        try (var frame = new NativeFrame(yaw, pitch, radius, distance, eyeHeight, 5)) {
            frame.open();
            double lowest = frame.hits.stream().mapToDouble(Location::getY).min().orElseThrow();
            assertEquals(frame.feet.getY() + .12, lowest, 1e-9);
            Location middle = frame.hits.get(7);
            assertEquals(distance, Math.hypot(middle.getX() - frame.eyes.getX(),
                    middle.getZ() - frame.eyes.getZ()), 1e-9);
            verify(frame.f.player.getWorld(), never()).getChunkAt(anyInt(), anyInt());
            verify(frame.f.player.getWorld(), never()).loadChunk(anyInt(), anyInt());
        }
    }

    @Test void pagingKeepsTheOpeningAnchorAfterPlayerMovementAndRotation() {
        try (var frame = new NativeFrame(90, 75, 1.25, 2.5, 1.62, 6)) {
            frame.open();
            Location title = frame.hits.get(5).clone();
            frame.eyes.add(.25, 0, 0).setYaw(180);
            frame.eyes.setPitch(-75);
            frame.feet.add(.25, 0, 0);
            frame.nextPage();
            assertEquals(title, frame.hits.get(10));
        }
    }

    @Test void increasingRadiusBetweenPagesRetainsFootPlaneClearance() {
        try (var frame = new NativeFrame(90, 0, .9, 2.5, 1.62, 6)) {
            frame.open();
            Location title = frame.hits.get(5).clone();
            frame.f.config.set("Experimental.VoteGUIs.Radial.Radius", 1.8);
            frame.nextPage();
            assertTrue(frame.hits.subList(9, 14).stream().allMatch(hit -> hit.getY() >= frame.feet.getY() + .1));
            assertEquals(title.getX(), frame.hits.get(10).getX(), 1e-9);
            assertEquals(title.getZ(), frame.hits.get(10).getZ(), 1e-9);
        }
    }

    @TestFactory Stream<DynamicTest> invalidRadiusCases() {
        return java.util.stream.DoubleStream.of(.89, 1.81, Double.NaN, Double.POSITIVE_INFINITY)
                .mapToObj(radius -> DynamicTest.dynamicTest("invalid radius " + radius,
                        () -> invalidRadiusNeverAdmitsSnapshotOrNativeEntities(radius)));
    }

    void invalidRadiusNeverAdmitsSnapshotOrNativeEntities(double radius) {
        try (var frame = new NativeFrame(90, 0, radius, 2.5, 1.62, 5)) {
            var production = frame.f.top.get();
            frame.f.manager.open(frame.f.player, ExperimentalGUIType.RADIAL);
            frame.f.owner();
            assertTrue(frame.f.storage.isEmpty());
            assertTrue(frame.entities.isEmpty());
            assertSame(production, frame.f.top.get());
        }
    }

    // Reuse the exact parent fixture across its namespace-only migration.
    // Reflection is limited to existing test setup and the native listener seam.
    private static final class Fixture implements AutoCloseable {
        private final Object delegate = create();
        final VotingPluginMain plugin = field("plugin");
        final HologramMenuScheduler scheduler = field("scheduler");
        final Player player = field("player");
        final YamlConfiguration config = field("config");
        final Queue<Runnable> owner = field("owner");
        final Queue<Runnable> storage = field("storage");
        final AtomicReference<Inventory> top = field("top");
        final MockedStatic<Bukkit> bukkit = field("bukkit");
        final ExperimentalGUIManager manager = field("manager");

        private static Object create() {
            for (String name : List.of(
                    "com.bencodez.votingplugin.tests.experimental.ExperimentalGUIManagerTest$Fixture",
                    "com.bencodez.votingplugin.experimental.ExperimentalGUIManagerTest$Fixture")) {
                try {
                    var constructor = Class.forName(name).getDeclaredConstructor();
                    constructor.setAccessible(true);
                    return constructor.newInstance();
                } catch (ClassNotFoundException absent) {
                    // Only a missing namespace permits trying the other name.
                } catch (ReflectiveOperationException failure) {
                    throw accessFailure(failure);
                }
            }
            throw new AssertionError("Existing experimental GUI fixture is unavailable");
        }

        @SuppressWarnings("unchecked")
        private <T> T field(String name) {
            try {
                var field = delegate.getClass().getDeclaredField(name);
                field.setAccessible(true);
                return (T) field.get(delegate);
            } catch (ReflectiveOperationException failure) {
                throw accessFailure(failure);
            }
        }

        @SuppressWarnings("unchecked")
        private static <T> T invoke(Object receiver, String name, Class<?>[] parameters, Object... arguments) {
            try {
                var method = receiver.getClass().getDeclaredMethod(name, parameters);
                method.setAccessible(true);
                return (T) method.invoke(receiver, arguments);
            } catch (ReflectiveOperationException failure) {
                throw accessFailure(failure);
            }
        }

        private static AssertionError accessFailure(ReflectiveOperationException failure) {
            Throwable cause = failure instanceof InvocationTargetException invocation
                    ? invocation.getCause() : failure;
            if (cause instanceof RuntimeException runtime) throw runtime;
            if (cause instanceof Error error) throw error;
            return new AssertionError(cause);
        }

        void owner() { invoke(delegate, "owner", new Class<?>[0]); }
        void snapshot() { invoke(delegate, "snapshot", new Class<?>[0]); }
        VoteSite site(String key, String url) {
            return invoke(delegate, "site", new Class<?>[] {String.class, String.class}, key, url);
        }
        void interact(PlayerInteractEntityEvent event) {
            Object listener = invoke(manager, "displaysListener", new Class<?>[0]);
            invoke(listener, "interact", new Class<?>[] {PlayerInteractEntityEvent.class}, event);
        }
        @Override public void close() { invoke(delegate, "close", new Class<?>[0]); }
    }
}
