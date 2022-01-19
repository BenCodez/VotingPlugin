package com.bencodez.votingplugin.listeners;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerQuitEvent;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerJoinEvent implements Listener {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerJoinEvent(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	private void login(Player player) {
		VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
		if (player.isOp() && plugin.isYmlError()) {
			user.sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
		}

		boolean data = user.getData().hasData();
		// run remind
		user.loginMessage();

		if (data) {
			// give offline vote (if they voted offline)
			user.offVote();
		}

		user.loginRewards();

		if (plugin.getBungeeSettings().isUseBungeecoord()
				&& plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			plugin.getPluginMessaging().sendPluginMessage("Login", user.getPlayerName(), user.getUUID());
		}
	}

	/**
	 * On player login.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(AdvancedCoreLoginEvent event) {
		if (event.getPlayer() == null || (VotingPluginMain.plugin.getStorageType().equals(UserStorage.MYSQL)
				&& VotingPluginMain.plugin.getMysql() == null)) {
			return;
		}

		int delay = plugin.getConfigFile().getDelayLoginEvent();
		if (delay <= 0) {
			login(event.getPlayer());
		} else {
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					login(event.getPlayer());
				}
			}, delay);
		}

	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerQuit(PlayerQuitEvent event) {
		if (plugin.getConfigFile().isDisableAdvancedTab()) {
			return;
		}
		final java.util.UUID uuid = event.getPlayer().getUniqueId();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				VotingPluginMain.plugin.getAdvancedTab().remove(uuid);

				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(uuid);
				user.dontCache();
				user.logoutRewards();
			}
		});

	}
}