package com.Ben12345rocks.VotingPlugin.Events;

import java.util.Set;

import org.bukkit.block.Sign;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;

public class BlockBreak implements Listener {

	@SuppressWarnings("unused")
	private static Main plugin;

	public BlockBreak(Main plugin) {
		BlockBreak.plugin = plugin;
	}

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
