package com.bencodez.votingplugin.listeners;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.SignChangeEvent;

import com.bencodez.advancedcore.api.misc.PlayerManager;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class SignChange.
 */
public class SignChange implements Listener {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new sign change.
	 *
	 * @param plugin the plugin
	 */
	public SignChange(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On sign change.
	 *
	 * @param event the event
	 */
	@EventHandler(ignoreCancelled = true)
	public void onSignChange(SignChangeEvent event) {
		if (event.getLine(0).equalsIgnoreCase("[VotingPlugin]")) {
			if (PlayerManager.getInstance().hasServerPermission(event.getPlayer().getName(), "VotingPlugin.Sign.Create")
					|| PlayerManager.getInstance().hasServerPermission(event.getPlayer().getName(),
							"VotingPlugin.Admin")) {
				String data = event.getLine(2);
				if (!data.equalsIgnoreCase("all") && !data.equalsIgnoreCase("monthly")
						&& !data.equalsIgnoreCase("weekly") && !data.equalsIgnoreCase("daily")) {
					return;
				}
				try {
					plugin.getServerData().addSign(event.getBlock().getLocation(), event.getLine(2),
							Integer.parseInt(event.getLine(1)));
					event.getPlayer().sendMessage(MessageAPI.colorize("&aAdded sign!"));
					plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {

						@Override
						public void run() {
							plugin.getSigns().updateSigns();
							plugin.getSigns().storeSigns();
						}
					});
				} catch (Exception ex) {
					event.getPlayer().sendMessage(MessageAPI.colorize("&cError on sign creation!"));
					ex.printStackTrace();
				}
			} else {
				event.getPlayer().sendMessage(MessageAPI.colorize(plugin.getConfigFile().getFormatNoPerms()));
			}
		}

	}

}