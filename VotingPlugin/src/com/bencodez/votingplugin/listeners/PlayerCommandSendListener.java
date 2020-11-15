package com.bencodez.votingplugin.listeners;

import java.util.ArrayList;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerCommandSendEvent;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerCommandSendListener implements Listener {

	@SuppressWarnings("unused")
	private static VotingPluginMain plugin;

	public PlayerCommandSendListener(VotingPluginMain plugin) {
		PlayerCommandSendListener.plugin = plugin;
	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onTab(PlayerCommandSendEvent event) {
		if (Config.getInstance().isDisableAdvancedTab()) {
			return;
		}
		ArrayList<String> removeCmds = new ArrayList<String>();
		if (!VotingPluginMain.plugin.getAdvancedTab().containsKey(event.getPlayer().getUniqueId())) {
			ArrayList<String> whiteListed = new ArrayList<String>();
			for (String cmd : event.getCommands()) {
				if (CommandLoader.getInstance().isVotingPluginCommand(event.getPlayer(), cmd)) {
					if (!CommandLoader.getInstance().hasPermission(event.getPlayer(), cmd)
							&& !whiteListed.contains(cmd)) {
						removeCmds.add(cmd);
						// plugin.debug("removed " + cmd);
					} else {
						removeCmds.remove(cmd);
						whiteListed.add(cmd);
						// plugin.debug("has " + cmd);
					}
				}
			}
			VotingPluginMain.plugin.getAdvancedTab().put(event.getPlayer().getUniqueId(), removeCmds);
		} else {
			removeCmds = VotingPluginMain.plugin.getAdvancedTab().get(event.getPlayer().getUniqueId());
		}
		event.getCommands().removeAll(removeCmds);
	}
}