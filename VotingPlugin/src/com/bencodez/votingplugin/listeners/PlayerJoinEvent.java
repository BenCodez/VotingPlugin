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
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.usermanager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerJoinEvent implements Listener {

	/** The plugin. */
	private static VotingPluginMain plugin;

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public PlayerJoinEvent(VotingPluginMain plugin) {
		PlayerJoinEvent.plugin = plugin;
	}

	/**
	 * On player login.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(AdvancedCoreLoginEvent event) {
		if (event.getPlayer() == null
				|| (VotingPluginMain.plugin.getStorageType().equals(UserStorage.MYSQL) && VotingPluginMain.plugin.getMysql() == null)) {
			return;
		}
		Player player = event.getPlayer();

		User user = UserManager.getInstance().getVotingPluginUser(player);
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
	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerQuit(PlayerQuitEvent event) {
		if (Config.getInstance().isDisableAdvancedTab()) {
			return;
		}
		final java.util.UUID uuid = event.getPlayer().getUniqueId();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				VotingPluginMain.plugin.getAdvancedTab().remove(uuid);
				
				User user = UserManager.getInstance().getVotingPluginUser(uuid);
				user.logoutRewards();
			}
		});

	}
}