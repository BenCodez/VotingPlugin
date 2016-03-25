package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerLoginEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.UserData.Data;
import com.Ben12345rocks.VotingPlugin.UserData.UUIDs;

public class PlayerJoinEvent implements Listener {

	private static Main plugin;

	Data data = Data.getInstance();

	public PlayerJoinEvent(Main plugin) {
		PlayerJoinEvent.plugin = plugin;
	}

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onPlayerLogin(PlayerLoginEvent event) {

		Player player = event.getPlayer();

		String playerName = player.getName();

		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		User user = new User(playerName);

		userDataFile(user);

		UUIDs.getInstance()
				.setName(playerName, player.getUniqueId().toString());

		plugin.getServer().getScheduler()
				.runTaskLaterAsynchronously(plugin, new Runnable() {
					@Override
					public void run() {
						// msg player if there is a update
						if (Config.getInstance().updateReminder()) {
							updateCheckLogin(player);
						}

						// give offline vote (if they voted offline)
						user.offVote();

						// msg player if he can vote
						user.loginMessage();
					}
				}, 100L);
	}

	private void updateCheckLogin(Player player) {
		Updater updater = plugin.updater;

		switch (updater.getResult()) {
		case FAIL_SPIGOT: {
			break;
		}
		case NO_UPDATE: {
			break;
		}
		case UPDATE_AVAILABLE: {
			if (player.hasPermission("VotingPlugin.remindupdate")) {
				player.sendMessage(Utils.getInstance().colorize(
						"&a" + plugin.getName()
								+ " has an update available! Your Version: &c"
								+ plugin.getDescription().getVersion()
								+ "&a New Version: &c" + updater.getVersion()));
			}
			break;
		}
		default: {
			break;
		}
		}
	}

	private void userDataFile(User user) {
		data.setup(user);
	}

}