package com.Ben12345rocks.VotingPlugin.Events;

import java.util.Set;

import org.bukkit.block.Sign;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;

// TODO: Auto-generated Javadoc
/**
 * The Class BlockBreak.
 */
public class BlockBreak implements Listener {

	/** The plugin. */
	@SuppressWarnings("unused")
	private static Main plugin;

	/**
	 * Instantiates a new block break.
	 *
	 * @param plugin the plugin
	 */
	public BlockBreak(Main plugin) {
		BlockBreak.plugin = plugin;
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
			if (s.getLine(0).startsWith("TopVoter")) {
				Set<String> signs = ServerData.getInstance().getSigns();
				if (signs != null) {
					for (String sign : signs) {
						if (ServerData.getInstance().getSignLocation(sign)
								.equals(event.getBlock().getLocation())) {
							ServerData.getInstance().removeSign(sign);
						}
					}
				}
			}
		}

	}

}
