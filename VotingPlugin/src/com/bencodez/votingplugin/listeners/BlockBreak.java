package com.bencodez.votingplugin.listeners;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Sign;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.signs.SignHandler;

// TODO: Auto-generated Javadoc
/**
 * The Class BlockBreak.
 */
public class BlockBreak implements Listener {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new block break.
	 *
	 * @param plugin the plugin
	 */
	public BlockBreak(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On block break.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onBlockBreak(BlockBreakEvent event) {
		if (event.getBlock().getState() instanceof Sign) {
			Sign s = (Sign) event.getBlock().getState();
			final Location loc = s.getLocation();
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					for (SignHandler sign : plugin.getSigns().getSigns()) {
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
