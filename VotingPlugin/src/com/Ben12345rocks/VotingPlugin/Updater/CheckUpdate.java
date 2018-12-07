package com.Ben12345rocks.VotingPlugin.Updater;

import java.util.Timer;
import java.util.TimerTask;

import org.bukkit.Bukkit;

import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;

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
		if (Config.getInstance().isDisableUpdateChecking()) {
			return;
		}
		plugin.setUpdater(new Updater(plugin, 15358, false));
		final Updater.UpdateResult result = plugin.getUpdater().getResult();
		switch (result) {
			case FAIL_SPIGOT: {
				plugin.getLogger().info("Failed to check for update for " + plugin.getName() + "!");
				break;
			}
			case NO_UPDATE: {
				plugin.getLogger()
						.info(plugin.getName() + " is up to date! Version: " + plugin.getUpdater().getVersion());
				break;
			}
			case UPDATE_AVAILABLE: {
				plugin.getLogger()
						.info(plugin.getName() + " has an update available! Your Version: "
								+ plugin.getDescription().getVersion() + " New Version: "
								+ plugin.getUpdater().getVersion() + " Use /av download to get the latest update!");
				break;
			}
			default: {
				break;
			}
		}
	}

	public void checkUpdateBasic() {
		if (Config.getInstance().isDisableUpdateChecking()) {
			return;
		}
		plugin.setUpdater(new Updater(plugin, 15358, false));
		final Updater.UpdateResult result = plugin.getUpdater().getResult();
		switch (result) {
			case UPDATE_AVAILABLE: {
				plugin.getLogger()
						.info(plugin.getName() + " has an update available! Your Version: "
								+ plugin.getDescription().getVersion() + " New Version: "
								+ plugin.getUpdater().getVersion() + " Use /av download to get the latest update!");
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
		if (Config.getInstance().isDisableUpdateChecking()) {
			return;
		}
		Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				checkUpdate();
				new Timer().schedule(new TimerTask() {

					@Override
					public void run() {
						checkUpdateBasic();
					}
				}, 1000 * 60 * 1200, 1000 * 60 * 1200);

			}
		}, 10l);
	}

}
