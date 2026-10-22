package com.bencodez.votingplugin.tests.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.Material;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.InvocationTargetException;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.experimental.ExperimentalGUIManager;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;
import com.bencodez.votingplugin.user.VotingPluginUser;

class ExperimentalAnimatedInventoryLifecycleTest {
    @Test void repeatedAnimationReusesOneInventoryAndWatchWithoutReadingStorageAgain() {
        try (var f = new Fixture()) {
            f.open();
            f.snapshot();
            Inventory inventory = f.top.get();
            clearInvocations(f.user, inventory, f.player);

            for (int i = 0; i < 120; i++) {
                f.watches.getFirst().run();
                f.owner();
                assertSame(inventory, f.top.get());
                assertTrue(f.storage.isEmpty());
                assertTrue(f.owner.isEmpty());
            }

            assertEquals(1, f.created.size());
            assertEquals(1, f.watches.size());
            verify(f.scheduler, times(1)).watchPlayer(eq(f.player), any(), any());
            verify(inventory, times(120)).clear();
            var border = ArgumentCaptor.forClass(ItemStack.class);
            verify(inventory, times(120)).setItem(eq(0), border.capture());
            assertEquals(List.of(Material.BLUE_STAINED_GLASS_PANE, Material.GRAY_STAINED_GLASS_PANE),
                    border.getAllValues().stream().limit(2).map(ItemStack::getType).toList());
            assertEquals(2, border.getAllValues().stream().map(ItemStack::getType).distinct().count());
            verifyNoInteractions(f.user);
            verify(f.player, never()).openInventory(any(Inventory.class));
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void disconnectBeforeSnapshotCannotReadUserOrOpenAnInventoryLater() {
        try (var f = new Fixture()) {
            Inventory production = f.top.get();
            f.open();
            assertEquals(1, f.storage.size());
            PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
            when(quit.getPlayer()).thenReturn(f.player);
            f.manager.quit(quit);
            f.owner();
            f.snapshot();
            f.watches.getFirst().run();
            f.owner();

            assertSame(production, f.top.get());
            assertTrue(f.created.isEmpty());
            assertTrue(f.storage.isEmpty());
            verify(f.plugin, never()).getUser(f.id);
            verifyNoInteractions(f.user);
            verify(f.player, never()).openInventory(any(Inventory.class));
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void animationRetiresOnceWithoutClosingOrChangingAProductionSuccessor() {
        try (var f = new Fixture()) {
            AtomicInteger cancellations = new AtomicInteger();
            doAnswer(invocation -> {
                f.watches.add(invocation.getArgument(1));
                return (Runnable) cancellations::incrementAndGet;
            }).when(f.scheduler).watchPlayer(any(), any(), any());
            f.open();
            f.snapshot();
            Inventory old = f.top.get();
            Inventory production = mock(Inventory.class);
            f.top.set(production);
            clearInvocations(old, f.player, f.user);

            for (int i = 0; i < 3; i++) {
                f.watches.getFirst().run();
                f.owner();
            }

            assertEquals(1, cancellations.get());
            assertSame(production, f.top.get());
            verifyNoInteractions(production, old, f.user);
            verify(f.player, never()).closeInventory();
            verify(f.player, never()).openInventory(any(Inventory.class));
            assertTrue(f.storage.isEmpty());
        }
    }

    @Test void oldAnimationCannotRenderOrCloseTheReplacementExperimentalSession() {
        try (var f = new Fixture()) {
            f.open();
            f.snapshot();
            Inventory first = f.top.get();
            Runnable retired = f.watches.getFirst();
            f.open();
            f.snapshot();
            Inventory replacement = f.top.get();
            assertNotSame(first, replacement);
            clearInvocations(first, replacement, f.player, f.user);

            retired.run();
            f.owner();

            assertSame(replacement, f.top.get());
            verifyNoInteractions(first, replacement, f.user);
            verify(f.player, never()).closeInventory();
            verify(f.player, never()).openInventory(any(Inventory.class));
            assertTrue(f.storage.isEmpty());
            f.watches.getLast().run();
            f.owner();
            verify(replacement).clear();
        }
    }

    // Reuse the existing fixture without widening production or legacy test visibility.
    // Accept its namespace-only move when the integrator relocates the parent tests.
    private static final class Fixture implements AutoCloseable {
        private final Object delegate = create();
        final VotingPluginMain plugin = field("plugin");
        final HologramMenuScheduler scheduler = field("scheduler");
        final Player player = field("player");
        final UUID id = field("id");
        final VotingPluginUser user = field("user");
        final Queue<Runnable> owner = field("owner");
        final Queue<Runnable> storage = field("storage");
        final List<Runnable> watches = field("watches");
        final List<Inventory> created = field("created");
        final AtomicReference<Inventory> top = field("top");
        final ExperimentalGUIManager manager = field("manager");

        private static Object create() {
            for (String name : List.of(
                    "com.bencodez.votingplugin.tests.experimental.ExperimentalGUIManagerTest$Fixture",
                    "com.bencodez.votingplugin.experimental.ExperimentalGUIManagerTest$Fixture")) {
                try {
                    var type = Class.forName(name);
                    var constructor = type.getDeclaredConstructor();
                    constructor.setAccessible(true);
                    return constructor.newInstance();
                } catch (ClassNotFoundException absent) {
                    // Try only the alternate namespace, never hide a fixture setup failure.
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

        private void call(String name) {
            try {
                var method = delegate.getClass().getDeclaredMethod(name);
                method.setAccessible(true);
                method.invoke(delegate);
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

        void open() { call("open"); }
        void owner() { call("owner"); }
        void snapshot() { call("snapshot"); }
        @Override public void close() { call("close"); }
    }
}
