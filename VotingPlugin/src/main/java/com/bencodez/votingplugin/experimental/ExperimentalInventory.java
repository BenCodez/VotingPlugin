package com.bencodez.votingplugin.experimental;

import java.util.Objects;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;

/** Identity is the holder AND its exact inventory, never a displayed title. */
final class ExperimentalInventory implements InventoryHolder {
    final ExperimentalSessions.Session session;
    private Inventory inventory;

    ExperimentalInventory(ExperimentalSessions.Session session) {
        this.session = Objects.requireNonNull(session);
    }

    void bind(Inventory inventory) {
        if (this.inventory != null) throw new IllegalStateException("Inventory already bound");
        this.inventory = Objects.requireNonNull(inventory);
    }

    @Override public Inventory getInventory() { return inventory; }
}
