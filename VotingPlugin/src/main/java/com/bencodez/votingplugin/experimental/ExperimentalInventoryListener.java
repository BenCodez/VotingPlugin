package com.bencodez.votingplugin.experimental;

import java.util.function.BiConsumer;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.inventory.Inventory;

/** Protects only this experimental holder's exact view, including bottom-inventory transfers. */
final class ExperimentalInventoryListener implements Listener {
    private final ExperimentalSessions sessions;
    private final BiConsumer<ExperimentalSessions.Session, Integer> click;

    ExperimentalInventoryListener(ExperimentalSessions sessions,
            BiConsumer<ExperimentalSessions.Session, Integer> click) {
        this.sessions = sessions;
        this.click = click;
    }

    private static ExperimentalInventory identify(Inventory top) {
        if (top.getHolder() instanceof ExperimentalInventory holder && holder.getInventory() == top) return holder;
        return null;
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void click(InventoryClickEvent event) {
        Inventory top = event.getView().getTopInventory();
        ExperimentalInventory holder = identify(top);
        if (holder == null) return;
        boolean previouslyCancelled = event.isCancelled();
        event.setCancelled(true);
        if (previouslyCancelled || !sessions.current(holder.session)
                || !holder.session.player.equals(event.getWhoClicked().getUniqueId())) return;
        // Transfers, number-key swaps, double clicks and offhand swaps never activate buttons.
        if (event.getClick() != ClickType.LEFT && event.getClick() != ClickType.RIGHT) return;
        int slot = event.getRawSlot();
        if (slot >= 0 && slot < top.getSize()) click.accept(holder.session, slot);
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void drag(InventoryDragEvent event) {
        if (identify(event.getView().getTopInventory()) != null) event.setCancelled(true);
    }

    @EventHandler
    public void close(InventoryCloseEvent event) {
        ExperimentalInventory holder = identify(event.getView().getTopInventory());
        if (holder != null && holder.session.player.equals(event.getPlayer().getUniqueId()))
            sessions.close(holder.session);
    }
}
