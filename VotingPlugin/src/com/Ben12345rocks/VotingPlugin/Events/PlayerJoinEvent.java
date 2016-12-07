package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerLoginEvent;

import com.Ben12345rocks.AdvancedCore.Thread.Thread;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerJoinEvent.
 */
public class PlayerJoinEvent implements Listener {

	/** The plugin. */
	private static Main plugin;

	/** The data. */
	Data data = Data.getInstance();

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
	public void onPlayerLogin(PlayerLoginEvent event) {
		plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				Thread.getInstance().run(new Runnable() {

					@Override
					public void run() {
						if (event.getPlayer() == null) {
							return;
						}
						Player player = event.getPlayer();

						if (!plugin.getDataFolder().exists()) {
							plugin.getDataFolder().mkdir();
						}

						User user = UserManager.getInstance().getVotingPluginUser(player);

						plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {
							@Override
							public void run() {
								Thread.getInstance().run(new Runnable() {

									@Override
									public void run() {
										if (user.getPlayer() == null) {
											return;
										}
										// give offline vote (if they voted
										// offline)
										user.offVote();

										// run remind
										user.loginMessage();
									}
								});
							}
						}, 100L);
					}
				});

			}
		}, 20L);

	}
}