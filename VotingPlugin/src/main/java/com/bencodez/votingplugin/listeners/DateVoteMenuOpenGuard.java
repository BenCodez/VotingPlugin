package com.bencodez.votingplugin.listeners;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryOpenEvent;
import com.bencodez.advancedcore.api.inventory.GUISession;
import com.bencodez.votingplugin.commands.gui.player.DateVoteEventsMenu;

/** Fence the actual platform open, including AdvancedCore's additional scheduling handoff. */
public final class DateVoteMenuOpenGuard implements Listener {
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public void onOpen(InventoryOpenEvent event) {
        GUISession session = GUISession.extractSession(event.getInventory());
        if (session != null && !DateVoteEventsMenu.permitsOpen(session.getInventoryGUI(), event.getPlayer()))
            event.setCancelled(true);
    }
}
