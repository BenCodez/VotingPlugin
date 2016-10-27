package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.PluginUpdateVersionEvent;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSitesOld;
import com.Ben12345rocks.VotingPlugin.Data.ServerDataOld;

// TODO: Auto-generated Javadoc
/**
 * The Class AdvancedCoreUpdateEvent.
 */
public class VotingPluginUpdateEvent implements Listener {

	/** The plugin. */
	private static Main plugin;

	/**
	 * Instantiates a new advanced core update event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotingPluginUpdateEvent(Main plugin) {
		VotingPluginUpdateEvent.plugin = plugin;
	}

	/**
	 * On plugin update.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPluginUpdate(PluginUpdateVersionEvent event) {

		if (event.getPlugin().getName()
				.equals(plugin.getDescription().getName())) {
			if (!event.getOldVersion().equals("")) {
				plugin.getLogger().info("Updated VotingPlugin");
				if (plugin.getDescription().getVersion().equals("4.5")) {
					convertToVoteSites();
				} else if (plugin.getDescription().getVersion().equals("4.5.1")
						&& !event.getOldVersion().equals("4.5")) {
					convertToVoteSites();
				} else if (plugin.getDescription().getVersion().equals("4.5.2")) {
					if (!event.getOldVersion().equals("4.5.1")
							&& !event.getOldVersion().equals("4.5")) {
						convertToVoteSites();
					}
					updateToNewConfigs();
				} else if (plugin.getDescription().getVersion().equals("4.5.3")) {
					if (!event.getOldVersion().equals("4.5.1")
							&& !event.getOldVersion().equals("4.5")) {
						convertToVoteSites();

					}
					if (!event.getOldVersion().equals("4.5.2")) {
						updateToNewConfigs();
					}
				} else if (plugin.getDescription().getVersion().equals("4.5.4")) {
					if (!event.getOldVersion().equals("4.5.3")) {
						if (!event.getOldVersion().equals("4.5.1")
								&& !event.getOldVersion().equals("4.5")) {
							convertToVoteSites();

						}
						if (!event.getOldVersion().equals("4.5.2")) {
							updateToNewConfigs();
						}
					}
				}
			}

		}
	}

	public void convertToVoteSites() {
		plugin.getLogger().info(
				"Detected using old vote site system, converting files...");
		ConfigVoteSitesOld.getInstance().convert();
		plugin.loadVoteSites();
	}

	@SuppressWarnings("deprecation")
	public void updateToNewConfigs() {
		plugin.getLogger().info("Detected using old config setup, updating...");
		ServerDataOld.getInstance().convert();
		com.Ben12345rocks.VotingPlugin.Config.ConfigVoteReminding.getInstance()
				.convert();
		plugin.reload();
	}

}