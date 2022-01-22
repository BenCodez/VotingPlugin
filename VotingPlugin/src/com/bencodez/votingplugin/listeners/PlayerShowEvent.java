package com.bencodez.votingplugin.listeners;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import de.myzelyam.api.vanish.PostPlayerShowEvent;

public class PlayerShowEvent implements Listener {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerShowEvent(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(PostPlayerShowEvent event) {
		Player p = event.getPlayer();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (p != null && p.isOnline()) {
					plugin.debug("Vanish login: " + p.getName());
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(p);
					if (p.isOp() && plugin.isYmlError()) {
						user.sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
					}

					boolean data = user.getData().hasData();
					// run remind
					user.loginMessage();

					if (data) {
						// give offline vote (if they voted offline)
						user.offVote();
						user.checkOfflineRewards();
						user.setLastOnline(System.currentTimeMillis());
					}

					user.loginRewards();

					if (plugin.getBungeeSettings().isUseBungeecoord()
							&& plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
						plugin.getPluginMessaging().sendPluginMessage("Login", user.getPlayerName(), user.getUUID());
					}
				}
			}
		});

	}
}
