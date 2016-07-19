package com.Ben12345rocks.VotingPlugin.Updater;

import org.bukkit.Bukkit;

import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class CheckUpdate.
 */
public class CheckUpdate {

	/** The instance. */
	static CheckUpdate instance = new CheckUpdate();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of CheckUpdate.
	 *
	 * @return single instance of CheckUpdate
	 */
	public static CheckUpdate getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new check update.
	 */
	private CheckUpdate() {
	}

	/**
	 * Instantiates a new check update.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CheckUpdate(Main plugin) {
		CheckUpdate.plugin = plugin;
	}

	/**
	 * Check update.
	 */
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

	/**
	 * Start up.
	 */
	public void startUp() {
		Bukkit.getServer().getScheduler()
		.runTaskLaterAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				checkUpdate();
			}
		}, 20L);
	}

}
