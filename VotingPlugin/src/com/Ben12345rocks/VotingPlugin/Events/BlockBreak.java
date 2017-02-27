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

// TODO: Auto-generated Javadoc
/**
 * The Class BlockBreak.
 */
public class BlockBreak implements Listener {

	/** The plugin. */
	private static Main plugin;

	/**
	 * Instantiates a new block break.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public BlockBreak(Main plugin) {
		BlockBreak.plugin = plugin;
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
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					for (SignHandler sign : plugin.signs) {
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
