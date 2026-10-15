package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import org.bukkit.entity.HumanEntity;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.junit.jupiter.api.Test;

class ExperimentalInventoryListenerTest {
    final UUID owner = UUID.randomUUID();
    final ExperimentalSessions sessions = new ExperimentalSessions(() -> 0L, failure -> fail(failure));
    final AtomicInteger actions = new AtomicInteger();
    final ExperimentalInventoryListener listener = new ExperimentalInventoryListener(sessions,
            (session, slot) -> actions.incrementAndGet());

    Inventory experimental() {
        var session = sessions.open(owner, ExperimentalGUIType.ANIMATED_INVENTORY, 64, 60);
        var holder = new ExperimentalInventory(session);
        Inventory inventory = mock(Inventory.class);
        when(inventory.getHolder()).thenReturn(holder);
        when(inventory.getSize()).thenReturn(54);
        holder.bind(inventory);
        session.activate();
        return inventory;
    }

    InventoryView view(Inventory top) {
        InventoryView view = mock(InventoryView.class);
        when(view.getTopInventory()).thenReturn(top);
        return view;
    }

    HumanEntity viewer(UUID id) {
        HumanEntity viewer = mock(HumanEntity.class);
        when(viewer.getUniqueId()).thenReturn(id);
        return viewer;
    }

    InventoryClickEvent click(Inventory top, UUID player, ClickType type, int slot) {
        InventoryClickEvent event = mock(InventoryClickEvent.class);
        InventoryView view = view(top);
        HumanEntity viewer = viewer(player);
        when(event.getView()).thenReturn(view);
        when(event.getWhoClicked()).thenReturn(viewer);
        when(event.getClick()).thenReturn(type);
        when(event.getRawSlot()).thenReturn(slot);
        return event;
    }

    @Test void productionInventoriesAreUnaffectedEvenWithTheSameTitle() {
        for (String name : new String[] {"Vote", "Vote Sites", "Admin Vote", "Rewards", "Vote Site Editor"}) {
            Inventory production = mock(Inventory.class, name);
            var event = click(production, owner, ClickType.SHIFT_LEFT, 0);
            listener.click(event);
            verify(event, never()).setCancelled(anyBoolean());
            InventoryDragEvent drag = mock(InventoryDragEvent.class);
            InventoryView view = view(production);
            when(drag.getView()).thenReturn(view);
            listener.drag(drag);
            verify(drag, never()).setCancelled(anyBoolean());
            InventoryCloseEvent close = mock(InventoryCloseEvent.class);
            when(close.getView()).thenReturn(view);
            listener.close(close);
        }
        assertEquals(0, actions.get());
    }

    @Test void productionCloseEventsDoNotRetireAnActiveExperimentalSessionForTheSamePlayer() {
        Inventory owned = experimental();
        var session = ((ExperimentalInventory) owned.getHolder()).session;
        for (String menu : new String[]{"Vote", "Vote Sites", "Admin Vote", "Rewards", "Vote Site Editor"}) {
            InventoryCloseEvent event = mock(InventoryCloseEvent.class);
            InventoryView productionView = view(mock(Inventory.class, menu));
            HumanEntity productionViewer = viewer(owner);
            when(event.getView()).thenReturn(productionView);
            when(event.getPlayer()).thenReturn(productionViewer);
            listener.close(event);
            assertTrue(sessions.current(session), menu);
        }
        sessions.clear(false);
        assertTrue(sessions.snapshot().isEmpty());
    }

    @Test void concurrentPlayersKeepDistinctInventoriesAndCleanup() {
        Inventory first = experimental();
        var firstSession = ((ExperimentalInventory) first.getHolder()).session;
        UUID secondPlayer = UUID.randomUUID();
        var secondSession = sessions.open(secondPlayer, ExperimentalGUIType.STREAK_TRACK, 64, 60);
        var secondHolder = new ExperimentalInventory(secondSession);
        Inventory second = mock(Inventory.class);
        when(second.getHolder()).thenReturn(secondHolder); when(second.getSize()).thenReturn(54);
        secondHolder.bind(second); secondSession.activate();
        listener.click(click(first, secondPlayer, ClickType.LEFT, 0));
        listener.click(click(second, owner, ClickType.LEFT, 0));
        assertEquals(0, actions.get());
        listener.click(click(first, owner, ClickType.LEFT, 0));
        listener.click(click(second, secondPlayer, ClickType.LEFT, 0));
        assertEquals(2, actions.get());
        InventoryCloseEvent close = mock(InventoryCloseEvent.class);
        InventoryView firstView = view(first); HumanEntity firstViewer = viewer(owner);
        when(close.getView()).thenReturn(firstView); when(close.getPlayer()).thenReturn(firstViewer);
        listener.close(close);
        assertFalse(sessions.current(firstSession));
        assertTrue(sessions.current(secondSession));
        listener.click(click(second, secondPlayer, ClickType.LEFT, 0));
        assertEquals(3, actions.get());
    }

    @Test void exactInventoryIdentityIsRequiredEvenForAnExperimentalHolder() {
        Inventory real = experimental();
        Inventory impostor = mock(Inventory.class);
        var holder = real.getHolder();
        when(impostor.getHolder()).thenReturn(holder);
        var event = click(impostor, owner, ClickType.LEFT, 0);
        listener.click(event);
        verify(event, never()).setCancelled(anyBoolean());
        assertEquals(0, actions.get());
    }

    @Test void transfersAndSwapsAreBlockedWithoutActivatingButtons() {
        Inventory top = experimental();
        for (ClickType type : new ClickType[] {ClickType.SHIFT_LEFT, ClickType.SHIFT_RIGHT,
                ClickType.NUMBER_KEY, ClickType.SWAP_OFFHAND, ClickType.DOUBLE_CLICK, ClickType.DROP}) {
            var event = click(top, owner, type, 0);
            listener.click(event);
            verify(event).setCancelled(true);
        }
        listener.click(click(top, owner, ClickType.LEFT, 55));
        assertEquals(0, actions.get());
        listener.click(click(top, owner, ClickType.LEFT, 0));
        assertEquals(1, actions.get());
    }

    @Test void wrongPlayerStaleSessionAndExternalCancellationCannotActivate() {
        Inventory top = experimental();
        listener.click(click(top, UUID.randomUUID(), ClickType.LEFT, 0));
        var cancelled = click(top, owner, ClickType.LEFT, 0);
        when(cancelled.isCancelled()).thenReturn(true);
        listener.click(cancelled);
        var replacement = sessions.open(owner, ExperimentalGUIType.STREAK_TRACK, 64, 60);
        listener.click(click(top, owner, ClickType.LEFT, 0));
        InventoryCloseEvent stale = mock(InventoryCloseEvent.class);
        InventoryView view = view(top);
        HumanEntity viewer = viewer(owner);
        when(stale.getView()).thenReturn(view);
        when(stale.getPlayer()).thenReturn(viewer);
        listener.close(stale);
        assertTrue(sessions.current(replacement));
        assertEquals(0, actions.get());
    }

    @Test void dragAndCloseOnlyAffectTheOwnedSession() {
        Inventory top = experimental();
        InventoryView view = view(top);
        InventoryDragEvent drag = mock(InventoryDragEvent.class);
        when(drag.getView()).thenReturn(view);
        listener.drag(drag);
        verify(drag).setCancelled(true);
        InventoryCloseEvent close = mock(InventoryCloseEvent.class);
        HumanEntity viewer = viewer(owner);
        when(close.getView()).thenReturn(view);
        when(close.getPlayer()).thenReturn(viewer);
        listener.close(close);
        assertTrue(sessions.snapshot().isEmpty());
    }
}
