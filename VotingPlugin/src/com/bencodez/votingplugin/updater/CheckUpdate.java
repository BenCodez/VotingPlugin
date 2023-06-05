package com.bencodez.votingplugin.updater;

import java.util.Timer;
import java.util.TimerTask;

import com.bencodez.advancedcore.api.updater.Updater;
import com.bencodez.advancedcore.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class CheckUpdate.
 */
public class CheckUpdate {

	private VotingPluginMain plugin;

	public CheckUpdate(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check update.
	 */
	public void checkUpdate() {
		if (plugin.getConfigFile().isDisableUpdateChecking()) {
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
			plugin.getLogger().info(plugin.getName() + " is up to date! Version: " + plugin.getUpdater().getVersion());
			break;
		}
		case UPDATE_AVAILABLE: {
			plugin.getLogger().info(plugin.getName() + " has an update available! Your Version: "
					+ plugin.getDescription().getVersion() + " New Version: " + plugin.getUpdater().getVersion());
			break;
		}
		default: {
			break;
		}
		}
	}

	public void checkUpdateBasic() {
		if (plugin.getConfigFile().isDisableUpdateChecking()) {
			return;
		}
		plugin.setUpdater(new Updater(plugin, 15358, false));
		final Updater.UpdateResult result = plugin.getUpdater().getResult();
		switch (result) {
		case UPDATE_AVAILABLE: {
			plugin.getLogger().info(plugin.getName() + " has an update available! Your Version: "
					+ plugin.getDescription().getVersion() + " New Version: " + plugin.getUpdater().getVersion());
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
		if (plugin.getConfigFile().isDisableUpdateChecking()) {
			return;
		}
		BukkitScheduler.runTaskLaterAsynchronously(plugin, new Runnable() {

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
