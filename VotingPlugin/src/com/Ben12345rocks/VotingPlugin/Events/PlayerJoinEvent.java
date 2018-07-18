package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerLoginEvent;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.UserStorage;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.VoteUser;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerJoinEvent implements Listener {
	private Main main = ServiceLocator.getService(Main.class);

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public PlayerJoinEvent() {
		
	}

	/**
	 * On player login.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(PlayerLoginEvent event) {
		main.getServer().getScheduler().runTaskLaterAsynchronously(main, new Runnable() {

			@Override
			public void run() {

				if (event.getPlayer() == null
						|| (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)
								&& AdvancedCoreHook.getInstance().getMysql() == null)) {
					return;
				}
				Player player = event.getPlayer();

				if (player != null) {
					VoteUser user = UserManager.getInstance().getVotingPluginUser(player);

					// run remind
					user.loginMessage();

					if (user.getData().hasData()) {
						// give offline vote (if they voted
						// offline)

						user.offVote();

					}

				}

			}
		}, 20L);

	}
}