package com.bencodez.votingplugin.commands.gui.player;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;
import com.bencodez.votingplugin.listeners.DateVoteMenuOpenGuard;

/** Real AdvancedCore inventory construction and deferred open; Bukkit is the external boundary. */
class DateVoteMenuOpenBoundaryTest {
    @Test
    void permissionRevokedAfterAdvancedCoreQueuesOpenCancelsFinalPlatformOpen() {
        try (Boundary boundary = new Boundary()) {
            boundary.open();
            assertEquals(1, boundary.scheduled.size());
            when(boundary.player.hasPermission("VotingPlugin.Commands.Vote.DateEvents")).thenReturn(false);
            boundary.finish(false);
        }
    }

    @Test
    void reloadBetweenInventoryConstructionAndFinalOpenCancelsStaleSnapshot() {
        try (Boundary boundary = new Boundary()) {
            boundary.open();
            boundary.current.set(false);
            boundary.finish(false);
        }
    }

    @Test
    void pluginShutdownAndPlayerDisconnectEachCancelTheFinalOpen() {
        try (Boundary boundary = new Boundary()) {
            boundary.open();
            when(boundary.plugin.isEnabled()).thenReturn(false);
            boundary.finish(false);
        }
        try (Boundary boundary = new Boundary()) {
            boundary.open();
            when(boundary.player.isOnline()).thenReturn(false);
            boundary.finish(false);
        }
    }

    @Test
    void currentSnapshotOpensAndUnrelatedInventoriesRemainAllowed() {
        try (Boundary boundary = new Boundary()) {
            boundary.open();
            boundary.finish(true);
            BInventory unrelated = new BInventory("Another menu");
            assertTrue(DateVoteEventsMenu.permitsOpen(unrelated, boundary.player));
            InventoryView view = mock(InventoryView.class);
            Inventory ordinary = mock(Inventory.class);
            when(view.getTopInventory()).thenReturn(ordinary);
            when(view.getPlayer()).thenReturn(boundary.player);
            InventoryOpenEvent event = new InventoryOpenEvent(view);
            new DateVoteMenuOpenGuard().onOpen(event);
            assertFalse(event.isCancelled());
        }
    }

    private static final class Boundary implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final Player player = mock(Player.class);
        final AtomicBoolean current = new AtomicBoolean(true);
        final AtomicInteger visible = new AtomicInteger();
        final ArrayDeque<Runnable> scheduled = new ArrayDeque<>();
        final MockedConstruction<ItemBuilder> builders;
        final MockedStatic<Bukkit> bukkit;
        final MockedStatic<AdvancedCorePlugin> core;
        InventoryOpenEvent finalEvent;

        Boundary() {
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getOptions()).thenReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class));
            when(player.isOnline()).thenReturn(true);
            when(player.getUniqueId()).thenReturn(new UUID(1, 1));
            when(player.hasPermission("VotingPlugin.Commands.Vote.DateEvents")).thenReturn(true);
            BukkitScheduler scheduler = mock(BukkitScheduler.class);
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            doAnswer(call -> { scheduled.add(call.getArgument(1)); return null; })
                    .when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
            core = mockStatic(AdvancedCorePlugin.class);
            core.when(AdvancedCorePlugin::getInstance).thenReturn(plugin);
            builders = mockConstruction(ItemBuilder.class, withSettings().defaultAnswer(RETURNS_SELF),
                    (builder, context) -> {
                        ItemStack stack = mock(ItemStack.class);
                        when(stack.getItemMeta()).thenReturn(mock(ItemMeta.class));
                        when(builder.toItemStack()).thenReturn(stack);
                    });
            bukkit = mockStatic(Bukkit.class);
            bukkit.when(() -> Bukkit.createInventory(any(InventoryHolder.class), anyInt(), anyString()))
                    .thenAnswer(call -> {
                        Inventory inventory = mock(Inventory.class);
                        when(inventory.getHolder()).thenReturn(call.getArgument(0));
                        return inventory;
                    });
            when(player.openInventory(any(Inventory.class))).thenAnswer(call -> {
                InventoryView view = mock(InventoryView.class);
                when(view.getTopInventory()).thenReturn(call.getArgument(0));
                when(view.getPlayer()).thenReturn(player);
                finalEvent = new InventoryOpenEvent(view);
                new DateVoteMenuOpenGuard().onOpen(finalEvent);
                if (!finalEvent.isCancelled()) visible.incrementAndGet();
                return view;
            });
        }

        void open() {
            DateVoteEventsMenu.open(plugin, player, new DateVoteProgressSnapshot(1800000000000L, List.of()),
                    current::get);
            assertEquals(0, visible.get());
            verify(player, never()).openInventory(any(Inventory.class));
        }

        void finish(boolean allowed) {
            assertEquals(1, scheduled.size());
            scheduled.remove().run();
            assertNotNull(finalEvent);
            assertEquals(!allowed, finalEvent.isCancelled());
            assertEquals(allowed ? 1 : 0, visible.get());
            assertTrue(scheduled.isEmpty());
        }

        @Override public void close() {
            bukkit.close();
            builders.close();
            core.close();
        }
    }
}
