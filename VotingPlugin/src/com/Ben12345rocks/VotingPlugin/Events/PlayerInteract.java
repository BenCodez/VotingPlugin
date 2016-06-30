package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.block.Sign;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;

public class PlayerInteract implements Listener {

	private static Main plugin;

	public PlayerInteract(Main plugin) {
		PlayerInteract.plugin = plugin;
	}

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = false)
	public void onPlayerInteract(PlayerInteractEvent event) {
		Player player = event.getPlayer();
		if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
			if (event.getClickedBlock().getState() instanceof Sign) {
				for (SignHandler sign : plugin.signs) {
					if (sign.isLocationSame(player.getLocation())) {
						player.sendMessage(sign.getRightClickMessage());
					}
				}

			}
		}
	}
}
