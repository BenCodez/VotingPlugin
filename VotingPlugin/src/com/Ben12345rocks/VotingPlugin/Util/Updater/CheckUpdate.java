package com.Ben12345rocks.VotingPlugin.Util.Updater;

import java.util.Timer;
import java.util.TimerTask;

import org.bukkit.Bukkit;

import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.Main;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class CheckUpdate.
 */
public class CheckUpdate {

	/** The instance. */
	static CheckUpdate instance = new CheckUpdate();

	private Main main = ServiceLocator.getService(Main.class);

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
	 * Check update.
	 */
	public void checkUpdate() {
		main.updater = new Updater(main, 15358, false);
		final Updater.UpdateResult result = main.updater.getResult();
		switch (result) {
		case FAIL_SPIGOT: {
			main.getLogger().info("Failed to check for update for " + main.getName() + "!");
			break;
		}
		case NO_UPDATE: {
			main.getLogger().info(main.getName() + " is up to date! Version: " + main.updater.getVersion());
			break;
		}
		case UPDATE_AVAILABLE: {
			main.getLogger()
					.info(main.getName() + " has an update available! Your Version: "
							+ main.getDescription().getVersion() + " New Version: " + main.updater.getVersion()
							+ " Use /av download to get the latest update!");
			break;
		}
		default: {
			break;
		}
		}
	}

	public void checkUpdateBasic() {
		main.updater = new Updater(main, 15358, false);
		final Updater.UpdateResult result = main.updater.getResult();
		switch (result) {
		case UPDATE_AVAILABLE: {
			main.getLogger()
					.info(main.getName() + " has an update available! Your Version: "
							+ main.getDescription().getVersion() + " New Version: " + main.updater.getVersion()
							+ " Use /av download to get the latest update!");
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
		Bukkit.getScheduler().runTaskLaterAsynchronously(main, new Runnable() {

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
