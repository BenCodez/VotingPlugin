package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.SignChangeEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;

public class SignChange implements Listener {

	@SuppressWarnings("unused")
	private static Main plugin;

	public SignChange(Main plugin) {
		SignChange.plugin = plugin;
	}

	@EventHandler(ignoreCancelled = true)
	public void onSignChange(SignChangeEvent event) {
		if (event.getLine(0).equalsIgnoreCase("[VotingPlugin]")) {
			if (Utils.getInstance().hasPermission(event.getPlayer(),
					"Sign.Create")) {
				try {
					ServerData.getInstance().addSign(
							event.getBlock().getLocation(), event.getLine(2),
							Integer.parseInt(event.getLine(1)));
				} catch (Exception ex) {
					event.getPlayer().sendMessage(
							Utils.getInstance().colorize(
									"&cError on sign creation!"));
					ex.printStackTrace();
				}
			} else {
				event.getPlayer().sendMessage(Messages.getInstance().noPerms());
			}
		}

	}

}