package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Data.ServerData;
import com.Ben12345rocks.AdvancedCore.Listeners.PluginUpdateVersionEvent;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

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
		if (event.getPlugin().getName().equals(plugin.getDescription().getName())) {
			if (!event.getOldVersion().equals("")) {
				plugin.getLogger().info("Updated VotingPlugin");
				if (event.getOldVersion().equals("5.1.5")) {
					plugin.getLogger().info("Updating to new data system...");
					updateDataFiles();
					plugin.getLogger().info("Update complete");
				}
			}
		}
	}

	public void updateDataFiles() {
		if (!ServerData.getInstance().getData().getBoolean("OldDataUpdated")) {
			ServerData.getInstance().getData().set("OldDataUpdated", true);
			ServerData.getInstance().saveData();
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				user.loadFromOldData();
			}
		}
	}

}