package com.bencodez.votingplugin.listeners;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerQuitEvent;

import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.BungeeMethod;
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
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		if (player.isOp() && plugin.isYmlError()) {
			user.sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
		}

		boolean data = user.getData().hasData();
		// run remind
		user.loginMessage();

		if (data) {
			// give offline vote (if they voted offline)
			user.offVote();
		} else {
			plugin.debug("No data detected for " + player.getUniqueId().toString() + "/" + player.getName());
		}

		user.loginRewards();

		plugin.getPlaceholders().onUpdate(user, true);

		if (plugin.getBungeeSettings().isUseBungeecoord()
				&& (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
						|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.REDIS))) {
			plugin.getBungeeHandler().getGlobalMessageHandler().sendMessage("Login", user.getPlayerName(),
					user.getUUID(), plugin.getBungeeSettings().getServer());
		}
	}

	/**
	 * On player login.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(AdvancedCoreLoginEvent event) {
		if (event.getPlayer() == null || !plugin.isMySQLOkay()) {
			return;
		}

		login(event.getPlayer());

	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerQuit(PlayerQuitEvent event) {
		if (plugin != null && plugin.isEnabled()) {
			final Player player = event.getPlayer();
			plugin.getLoginTimer().execute(new Runnable() {

				@Override
				public void run() {
					VotingPluginMain.plugin.getAdvancedTab().remove(player.getUniqueId());

					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
					user.dontCache();
					user.logoutRewards();
					plugin.getPlaceholders().onLogout(user);
				}
			});
		}
	}
}