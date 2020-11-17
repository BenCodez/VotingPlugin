package com.bencodez.votingplugin.listeners;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.SignChangeEvent;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class SignChange.
 */
public class SignChange implements Listener {

	/** The plugin. */
	private static VotingPluginMain plugin;

	/**
	 * Instantiates a new sign change.
	 *
	 * @param plugin the plugin
	 */
	public SignChange(VotingPluginMain plugin) {
		SignChange.plugin = plugin;
	}

	/**
	 * On sign change.
	 *
	 * @param event the event
	 */
	@EventHandler(ignoreCancelled = true)
	public void onSignChange(SignChangeEvent event) {
		if (event.getLine(0).equalsIgnoreCase("[VotingPlugin]")) {
			if (PlayerUtils.getInstance().hasServerPermission(event.getPlayer().getName(), "VotingPlugin.Sign.Create")
					|| PlayerUtils.getInstance().hasServerPermission(event.getPlayer().getName(),
							"VotingPlugin.Admin")) {
				String data = event.getLine(2);
				if (!data.equalsIgnoreCase("all") && !data.equalsIgnoreCase("monthly")
						&& !data.equalsIgnoreCase("weekly") && !data.equalsIgnoreCase("daily")) {
					return;
				}
				try {
					plugin.getServerData().addSign(event.getBlock().getLocation(), event.getLine(2),
							Integer.parseInt(event.getLine(1)));
					event.getPlayer().sendMessage(StringParser.getInstance().colorize("&aAdded sign!"));
					Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

						@Override
						public void run() {
							plugin.getSigns().updateSigns();
							plugin.getSigns().storeSigns();
						}
					});
				} catch (Exception ex) {
					event.getPlayer().sendMessage(StringParser.getInstance().colorize("&cError on sign creation!"));
					ex.printStackTrace();
				}
			} else {
				event.getPlayer()
						.sendMessage(StringParser.getInstance().colorize(plugin.getConfigFile().getFormatNoPerms()));
			}
		}

	}

}