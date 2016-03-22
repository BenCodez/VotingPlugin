package com.Ben12345rocks.VotingPlugin.Updater;

import org.bukkit.Bukkit;

import com.Ben12345rocks.VotingPlugin.Main;

public class CheckUpdate {

	private CheckUpdate() {
	}

	static CheckUpdate instance = new CheckUpdate();

	public static CheckUpdate getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public CheckUpdate(Main plugin) {
		CheckUpdate.plugin = plugin;
	}

	public void startUp() {
		Bukkit.getServer().getScheduler()
				.runTaskLaterAsynchronously(plugin, new Runnable() {
					@Override
					public void run() {
						checkUpdate();
					}
				}, 20L);
	}

	public void checkUpdate() {
		plugin.updater = new Updater(plugin, 15358, false);
		final Updater.UpdateResult result = plugin.updater.getResult();
		switch (result) {
		case FAIL_SPIGOT: {
			plugin.getLogger().info(
					"Failed to check for update for " + plugin.getName() + "!");
			break;
		}
		case NO_UPDATE: {
			plugin.getLogger().info(
					plugin.getName() + " is up to date! Version: "
							+ plugin.updater.getVersion());
			break;
		}
		case UPDATE_AVAILABLE: {
			plugin.getLogger().info(
					plugin.getName()
							+ " has an update available! Your Version: "
							+ plugin.getDescription().getVersion()
							+ " New Version: " + plugin.updater.getVersion());
			break;
		}
		default: {
			break;
		}
		}
	}

}
