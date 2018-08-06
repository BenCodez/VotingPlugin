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
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerInteract.
 */
public class PlayerInteract implements Listener {
	private Main main = ServiceLocator.getService(Main.class);
	
	/**
	 * Instantiates a new player interact.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public PlayerInteract() {
		
	}

	/**
	 * On player interact.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.LOW, ignoreCancelled = true)
	public void onPlayerInteract(PlayerInteractEvent event) {
		Player player = event.getPlayer();
		if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
			// plugin.debug("Checking for sign click");
			if (event.getClickedBlock().getState() instanceof Sign) {
				// plugin.debug(player.getName() + " right clicked a sign");
				for (SignHandler sign : main.signs) {
					if (sign.isLocationSame(event.getClickedBlock().getLocation())) {
						// plugin.debug(player.getName() +
						// " right clicked a top voter sign, sending message");
						UserManager.getInstance().getVotingPluginUser(player).sendMessage(sign.getRightClickMessage());
					}
				}

			}
		}
	}
}
