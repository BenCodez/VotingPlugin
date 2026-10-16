package com.bencodez.votingplugin.tests.streaks;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.streaks.VoteStreaksMenu;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.*;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.inventory.*;
import org.bukkit.inventory.meta.ItemMeta;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VoteStreaksMenuTest {
    private static class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final BukkitScheduler scheduler = mock(BukkitScheduler.class);
        final ArrayDeque<Runnable> owners = new ArrayDeque<>(), workers = new ArrayDeque<>();
        final List<Inventory> inventories = new ArrayList<>();
        final java.util.Map<UUID, AtomicReference<Inventory>> tops = new java.util.HashMap<>();
        final MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class);
        final java.util.concurrent.atomic.AtomicInteger reads = new java.util.concurrent.atomic.AtomicInteger();
        final VoteStreaksMenu menu;
        boolean workerOwner, failRead, rejectWorker, rejectScheduler;
        Runnable duringOpen = () -> { };
        Fixture() {
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            when(plugin.getGui().getChestVoteStreaksName()).thenReturn("&6Vote streaks");
            doAnswer(call -> { if (rejectScheduler) throw new java.util.concurrent.RejectedExecutionException("Synthetic scheduler rejection"); owners.add(call.getArgument(1)); return null; })
                .when(scheduler).runTask(eq(plugin), any(Runnable.class), any(Entity.class));
            doAnswer(call -> { if (rejectScheduler) throw new java.util.concurrent.RejectedExecutionException("Synthetic scheduler rejection"); owners.add(call.getArgument(1)); return null; })
                .when(scheduler).runTask(eq(plugin), any(Runnable.class));
            var factory = mock(ItemFactory.class);
            bukkit.when(Bukkit::getItemFactory).thenReturn(factory);
            when(factory.getItemMeta(any(Material.class))).thenAnswer(call -> {
                var meta = mock(ItemMeta.class); var name = new AtomicReference<String>(); var lore = new AtomicReference<List<String>>();
                doAnswer(change -> { name.set(change.getArgument(0)); return null; }).when(meta).setDisplayName(anyString());
                doAnswer(change -> { lore.set(change.getArgument(0)); return null; }).when(meta).setLore(anyList());
                when(meta.getDisplayName()).thenAnswer(ignored -> name.get()); when(meta.getLore()).thenAnswer(ignored -> lore.get());
                when(meta.clone()).thenReturn(meta); return meta;
            });
            when(factory.isApplicable(any(ItemMeta.class), any(Material.class))).thenReturn(true);
            when(factory.asMetaFor(any(ItemMeta.class), any(Material.class))).thenAnswer(call -> call.getArgument(0));
            bukkit.when(() -> Bukkit.createInventory(any(InventoryHolder.class), eq(54), anyString())).thenAnswer(call -> {
                assertFalse(workerOwner, "inventory creation must be player-owned");
                Inventory inv = mock(Inventory.class); when(inv.getHolder()).thenReturn(call.getArgument(0));
                ItemStack[] slots = new ItemStack[54];
                doAnswer(change -> { slots[change.getArgument(0)] = change.getArgument(1); return null; }).when(inv).setItem(anyInt(), any(ItemStack.class));
                when(inv.getItem(anyInt())).thenAnswer(read -> slots[read.getArgument(0)]); inventories.add(inv); return inv;
            });
            menu = VoteStreaksMenuAccess.create(plugin, task -> {
                if (rejectWorker) throw new java.util.concurrent.RejectedExecutionException("Synthetic worker rejection");
                workers.add(task);
            }, (uuid, name) -> {
                assertTrue(workerOwner, "storage reads must be worker-owned"); reads.incrementAndGet();
                if (failRead) throw new IllegalStateException("Synthetic storage diagnostic");
                return VoteStreaksSnapshotTest.snapshot(7);
            });
        }
        Player player(int n) {
            Player p = mock(Player.class); UUID id = new UUID(0, n); when(p.getUniqueId()).thenReturn(id);
            when(p.getName()).thenReturn("test-player"); when(p.isOnline()).thenReturn(true); when(p.hasPermission(VoteStreaksMenu.PERMISSION)).thenReturn(true);
            Inventory original = mock(Inventory.class);
            var top = new AtomicReference<>(original); tops.put(id, top);
            InventoryView view = mock(InventoryView.class); when(view.getTopInventory()).thenAnswer(ignored -> top.get());
            when(p.getOpenInventory()).thenReturn(view);
            when(p.openInventory(any(Inventory.class))).thenAnswer(call -> { assertFalse(workerOwner); top.set(call.getArgument(0)); duringOpen.run(); return view; });
            doAnswer(call -> { assertFalse(workerOwner); top.set(original); return null; }).when(p).closeInventory();
            return p;
        }
        void owner() { assertFalse(workerOwner); owners.remove().run(); }
        void work() { workerOwner = true; try { workers.remove().run(); } finally { workerOwner = false; } }
        Inventory open(Player p) { menu.open(p); owner(); work(); owner(); return p.getOpenInventory().getTopInventory(); }
        InventoryClickEvent click(Player p, int slot, boolean left) {
            var event = mock(InventoryClickEvent.class); doReturn(p.getOpenInventory()).when(event).getView();
            when(event.getWhoClicked()).thenReturn(p); when(event.getRawSlot()).thenReturn(slot); when(event.isLeftClick()).thenReturn(left); return event;
        }
        @Override public void close() { menu.close(); bukkit.close(); }
    }
    @Test void refreshRetainsVisibleOwnershipAcrossResetAndReadFailure() {
        for (boolean failed : List.of(false, true)) {
            try (var f = new Fixture()) {
                Player p = f.player(1); Inventory visible = f.open(p);
                f.menu.click(f.click(p, 50, true));
                if (failed) { f.failRead = true; f.work(); }
                else { f.menu.reset(); f.work(); }
                while (!f.owners.isEmpty()) f.owner();
                assertNotSame(visible, p.getOpenInventory().getTopInventory());
                assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu));
            }
        }
    }
    @Test void disabledPluginClosesItsOwnViewDirectlyOnThePrimaryOwner() throws Exception {
        try (var f = new Fixture()) {
            Player p = f.player(1); Inventory visible = f.open(p);
            f.bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
            try {
                var publicOwner = Bukkit.class.getMethod("isOwnedByCurrentRegion", Entity.class);
                f.bukkit.when(() -> publicOwner.invoke(null, p)).thenReturn(true);
            } catch (NoSuchMethodException legacyBukkit) { /* primary-thread ownership is authoritative here */ }
            f.rejectScheduler = true; f.menu.close();
            assertNotSame(visible, p.getOpenInventory().getTopInventory());
            verify(p).closeInventory(); assertTrue(f.owners.isEmpty());
        }
    }
    public static final class OwnedPlatform {
        public static boolean isOwnedByCurrentRegion(Entity player) { return true; }
    }
    public static final class ForeignPlatform {
        public static boolean isOwnedByCurrentRegion(Entity player) { return false; }
    }
    public static final class FailedPlatform {
        public static boolean isOwnedByCurrentRegion(Entity player) { throw new IllegalStateException("Synthetic ownership failure"); }
    }
    @Test void shutdownCompatibilityRequiresPositivePublicOwnershipAndNeverAssumesMissingOrFailedApis() {
        try (var f = new Fixture()) {
            Player p = f.player(1);
            assertTrue(VoteStreaksMenuAccess.ownsForShutdown(OwnedPlatform.class, p));
            assertFalse(VoteStreaksMenuAccess.ownsForShutdown(ForeignPlatform.class, p));
            assertFalse(VoteStreaksMenuAccess.ownsForShutdown(FailedPlatform.class, p));
            assertFalse(VoteStreaksMenuAccess.ownsForShutdown(Object.class, p));
            f.bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
            assertFalse(VoteStreaksMenuAccess.ownsForShutdown(ForeignPlatform.class, p), "a tick thread need not own another entity");
            assertFalse(VoteStreaksMenuAccess.ownsForShutdown(FailedPlatform.class, p));
        }
    }
    @Test void readsOnWorkerRendersOnOwnerAndDisplaysFinalVisibleStreakText() {
        try (var f = new Fixture()) {
            Player player = f.player(1); f.menu.open(player); assertEquals(0, f.reads.get());
            f.owner(); assertEquals(0, f.reads.get()); assertTrue(f.inventories.isEmpty());
            f.work(); assertEquals(1, f.reads.get()); assertTrue(f.inventories.isEmpty());
            f.owner(); Inventory inventory = player.getOpenInventory().getTopInventory();
            assertInstanceOf(ReflectiveAccess.type("com.bencodez.votingplugin.streaks.VoteStreaksMenu$Holder"), inventory.getHolder());
            assertEquals("§6DAILY streak", inventory.getItem(11).getItemMeta().getDisplayName());
            assertEquals(List.of("§7Current: §e3 days", "§7Best: §e7 days"), inventory.getItem(11).getItemMeta().getLore());
            assertTrue(inventory.getItem(20).getItemMeta().getLore().contains("§7Progress only; this does not claim rewards."));
        }
    }
    @Test void productionConstructorUsesPersistenceWorkerForUuidUserLoadingAndStateReads() {
        try (var f = new Fixture()) {
            var timer = f.plugin.getUserManager().getDataManager().getTimer();
            doAnswer(call -> { f.workers.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            var users = f.plugin.getVotingPluginUserManager();
            var user = mock(com.bencodez.votingplugin.user.VotingPluginUser.class);
            when(users.getVotingPluginUser(any(UUID.class), anyString())).thenAnswer(call -> {
                assertTrue(f.workerOwner); return user;
            });
            when(f.plugin.getVoteStreakHandler().snapshotForDisplay(user)).thenAnswer(call -> {
                assertTrue(f.workerOwner); return VoteStreaksSnapshotTest.snapshot(1);
            });
            var production = VoteStreaksMenuAccess.create(f.plugin);
            try {
                Player p = f.player(1); production.open(p); f.owner();
                verify(users, never()).getVotingPluginUser(any(UUID.class));
                f.work(); f.owner(); verify(users).getVotingPluginUser(p.getUniqueId(), "test-player");
                assertEquals(1, f.inventories.size());
            } finally { production.close(); }
        }
    }
    @Test void replacementAndUnrelatedMenuOpeningInvalidateOldSnapshotCallbacks() {
        try (var f = new Fixture()) {
            Player p = f.player(1); f.menu.open(p); f.owner(); f.menu.open(p); f.owner();
            f.work(); assertTrue(f.owners.isEmpty(), "replaced request must not reopen");
            f.work(); f.owner(); assertEquals(1, f.inventories.size());
            f.menu.open(p); f.owner(); f.work();
            var other = mock(InventoryOpenEvent.class); when(other.getPlayer()).thenReturn(p); doReturn(mock(Inventory.class)).when(other).getInventory();
            f.menu.inventoryOpen(other); f.menu.unrelatedInventoryOpen(other); verify(other, never()).setCancelled(anyBoolean());
            f.owner(); assertEquals(1, f.inventories.size()); assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu));
        }
    }
    @Test void navigationIsDeferredClampedAndDoesNotRereadStorageOrRunRewards() {
        try (var f = new Fixture()) {
            Player p = f.player(1); f.open(p); int reads = f.reads.get();
            var next = f.click(p, 53, true); f.menu.click(next); verify(next).setCancelled(true);
            assertEquals(1, f.inventories.size(), "opening inside InventoryClickEvent is unsafe");
            f.owner(); assertEquals(2, f.inventories.size()); assertEquals(reads, f.reads.get());
            var holder = VoteStreaksMenuAccess.Holder.view(p.getOpenInventory().getTopInventory().getHolder()); assertEquals(1, holder.page());
            f.menu.click(f.click(p, 53, true)); f.owner(); holder = VoteStreaksMenuAccess.Holder.view(p.getOpenInventory().getTopInventory().getHolder()); assertEquals(1, holder.page());
            verifyNoInteractions(f.plugin.getRewardHandler());
        }
    }
    @Test void unrelatedClicksDragsAndClosesRemainUnmodified() {
        try (var f = new Fixture()) {
            Player p = f.player(1); var click = f.click(p, 0, true); f.menu.click(click); verify(click, never()).setCancelled(anyBoolean());
            var drag = mock(InventoryDragEvent.class); doReturn(p.getOpenInventory()).when(drag).getView(); f.menu.drag(drag); verify(drag, never()).setCancelled(anyBoolean());
            var close = mock(InventoryCloseEvent.class); doReturn(mock(Inventory.class)).when(close).getInventory(); when(close.getPlayer()).thenReturn(p); f.menu.inventoryClose(close);
            assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu));
        }
    }
    @Test void ownDragsShiftSwapsAndCancelledClicksCannotNavigateOrMoveItems() {
        try (var f = new Fixture()) {
            Player p = f.player(1); f.open(p);
            var drag = mock(InventoryDragEvent.class); doReturn(p.getOpenInventory()).when(drag).getView(); f.menu.drag(drag); verify(drag).setCancelled(true);
            for (int mode = 0; mode < 3; mode++) {
                var click = f.click(p, 53, mode != 0); when(click.isShiftClick()).thenReturn(mode == 1); when(click.isCancelled()).thenReturn(mode == 2);
                f.menu.click(click); verify(click).setCancelled(true); assertTrue(f.owners.isEmpty());
            }
        }
    }
    @Test void closeQuitReloadAndPermissionChangesPreventStaleOpens() {
        for (String mode : List.of("close", "quit", "reload", "permission", "disconnect")) {
            try (var f = new Fixture()) {
                Player p = f.player(1); f.menu.open(p); f.owner(); f.work();
                switch (mode) {
                case "close" -> { var event = mock(InventoryCloseEvent.class); doReturn(p.getOpenInventory().getTopInventory()).when(event).getInventory(); when(event.getPlayer()).thenReturn(p); f.menu.inventoryClose(event); }
                case "quit" -> f.menu.quit(new PlayerQuitEvent(p, ""));
                case "reload" -> f.menu.suspend();
                case "permission" -> when(p.hasPermission(VoteStreaksMenu.PERMISSION)).thenReturn(false);
                case "disconnect" -> when(p.isOnline()).thenReturn(false);
                }
                f.owner(); assertTrue(f.inventories.isEmpty()); assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu), mode);
            }
        }
    }
    @Test void multiplePlayersHaveIndependentInventoriesAndOneCloseDoesNotAffectTheOther() {
        try (var f = new Fixture()) {
            Player first = f.player(1), second = f.player(2); Inventory a = f.open(first), b = f.open(second); assertNotSame(a, b);
            var close = mock(InventoryCloseEvent.class); when(close.getInventory()).thenReturn(a); when(close.getPlayer()).thenReturn(first);
            f.menu.inventoryClose(close); assertEquals(1, VoteStreaksMenuAccess.sessionCount(f.menu));
            f.menu.click(f.click(second, 53, true)); f.owner(); assertEquals(1, (VoteStreaksMenuAccess.Holder.view(second.getOpenInventory().getTopInventory().getHolder())).page());
        }
    }
    @Test void workerAndSchedulerFailureReleaseStateWithoutLeakingDiagnostics() {
        for (String mode : List.of("read", "worker", "scheduler")) {
            try (var f = new Fixture()) {
                Player p = f.player(1);
                f.failRead = mode.equals("read"); f.rejectWorker = mode.equals("worker");
                f.menu.open(p); f.owner();
                if (!f.rejectWorker) {
                    f.rejectScheduler = mode.equals("scheduler"); f.work();
                    if (!f.owners.isEmpty()) f.owner();
                }
                assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu)); assertTrue(f.inventories.isEmpty());
                verify(p, never()).sendMessage(contains("Synthetic"));
            }
        }
    }
    @Test void boundedWorkerAdmissionAndNestedReloadsKeepOldCallbacksInert() {
        try (var f = new Fixture()) {
            for (int i = 0; i < 17; i++) { f.menu.open(f.player(i + 1)); f.owner(); }
            assertEquals(16, f.workers.size()); assertEquals(16, VoteStreaksMenuAccess.sessionCount(f.menu));
            f.menu.suspend(); f.menu.suspend(); f.menu.resume();
            while (!f.workers.isEmpty()) f.work(); assertTrue(f.inventories.isEmpty()); assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu));
            Player p = f.player(100); f.menu.open(p); f.owner(); assertTrue(f.workers.isEmpty());
            f.menu.resume(); f.open(p); assertEquals(1, f.inventories.size());
        }
    }
    @Test void failureAfterPartialNativeOpenCleansOnlyItsOwnInventory() {
        try (var f = new Fixture()) {
            Player p = f.player(1); f.duringOpen = () -> { throw new IllegalStateException("Synthetic native diagnostic"); };
            f.open(p); while (!f.owners.isEmpty()) f.owner();
            assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu)); verify(p).closeInventory();
            verify(p).sendMessage("Unable to open vote streaks. Please try again later.");
            verify(p, never()).sendMessage(contains("Synthetic"));
        }
    }
    @Test void reloadDuringNativeOpenClosesOnlyTheInvalidatedView() {
        try (var f = new Fixture()) {
            Player p = f.player(1); f.duringOpen = f.menu::reset;
            f.open(p); assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu)); verify(p).closeInventory();
            Inventory replacement = mock(Inventory.class); f.tops.get(p.getUniqueId()).set(replacement);
            while (!f.owners.isEmpty()) f.owner(); verify(p, times(1)).closeInventory();
            assertSame(replacement, p.getOpenInventory().getTopInventory());
        }
    }
    @Test void foreignPlayerCannotOpenAnotherPlayersSnapshotAndClosedServiceIsInert() {
        try (var f = new Fixture()) {
            Player first = f.player(1), second = f.player(2); Inventory inv = f.open(first);
            var open = mock(InventoryOpenEvent.class); when(open.getInventory()).thenReturn(inv); when(open.getPlayer()).thenReturn(second);
            f.menu.inventoryOpen(open); verify(open).setCancelled(true);
            f.menu.close(); f.menu.open(second); while (!f.owners.isEmpty()) f.owner(); assertEquals(0, VoteStreaksMenuAccess.sessionCount(f.menu)); assertEquals(1, f.reads.get());
        }
    }
}
