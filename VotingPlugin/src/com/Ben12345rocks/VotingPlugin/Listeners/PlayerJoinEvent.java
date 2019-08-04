package com.Ben12345rocks.VotingPlugin.Listeners;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.AdvancedCoreLoginEvent;
import com.Ben12345rocks.AdvancedCore.UserManager.UserStorage;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerJoinEvent implements Listener {

	/** The plugin. */
	@SuppressWarnings("unused")
	private static Main plugin;

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public PlayerJoinEvent(Main plugin) {
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
				|| (Main.plugin.getStorageType().equals(UserStorage.MYSQL) && Main.plugin.getMysql() == null)) {
			return;
		}
		Player player = event.getPlayer();

		User user = UserManager.getInstance().getVotingPluginUser(player);
		if (player.isOp() && plugin.isYmlError()) {
			user.sendMessage("&cVotingPlugin: Detected yml error, please chceck console for details");
		}

		// run remind
		user.loginMessage();

		if (user.getData().hasData()) {
			// give offline vote (if they voted offline)
			user.offVote();
		}

	}
}