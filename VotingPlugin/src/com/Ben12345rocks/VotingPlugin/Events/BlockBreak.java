package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Sign;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class BlockBreak.
 */
public class BlockBreak implements Listener {
	private Main main = ServiceLocator.getService(Main.class);
	
	/**
	 * Instantiates a new block break.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public BlockBreak() {
		
	}

	/**
	 * On block break.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onBlockBreak(BlockBreakEvent event) {
		if (event.getBlock().getState() instanceof Sign) {
			Sign s = (Sign) event.getBlock().getState();
			final Location loc = s.getLocation();
			Bukkit.getScheduler().runTaskAsynchronously(main, new Runnable() {

				@Override
				public void run() {
					for (SignHandler sign : main.signs) {
						if (sign.getLocation().equals(loc)) {
							sign.removeSign();
							sign.setValid(false);
						}
					}
				}
			});
		}

	}

}
