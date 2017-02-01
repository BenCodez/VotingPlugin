package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.SignChangeEvent;

import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Signs.Signs;

// TODO: Auto-generated Javadoc
/**
 * The Class SignChange.
 */
public class SignChange implements Listener {

	/** The plugin. */
	private static Main plugin;

	/**
	 * Instantiates a new sign change.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public SignChange(Main plugin) {
		SignChange.plugin = plugin;
	}

	/**
	 * On sign change.
	 *
	 * @param event
	 *            the event
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
					ServerData.getInstance().addSign(event.getBlock().getLocation(), event.getLine(2),
							Integer.parseInt(event.getLine(1)));
					event.getPlayer().sendMessage(StringUtils.getInstance().colorize("&aAdded sign!"));
					Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

						@Override
						public void run() {
							Signs.getInstance().updateSigns();
						}
					});
				} catch (Exception ex) {
					event.getPlayer().sendMessage(StringUtils.getInstance().colorize("&cError on sign creation!"));
					ex.printStackTrace();
				}
			} else {
				event.getPlayer()
						.sendMessage(StringUtils.getInstance().colorize(Config.getInstance().getFormatNoPerms()));
			}
		}

	}

}