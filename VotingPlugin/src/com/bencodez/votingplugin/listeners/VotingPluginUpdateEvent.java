package com.bencodez.votingplugin.listeners;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.listeners.PluginUpdateVersionEvent;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class AdvancedCoreUpdateEvent.
 */
public class VotingPluginUpdateEvent implements Listener {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new advanced core update event.
	 *
	 * @param plugin the plugin
	 */
	public VotingPluginUpdateEvent(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On plugin update.
	 *
	 * @param event the event
	 */
	@SuppressWarnings("deprecation")
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPluginUpdate(PluginUpdateVersionEvent event) {
		if (event.getPlugin().getName().equals(plugin.getDescription().getName())) {
			if (!event.getOldVersion().equals("")) {
				plugin.getLogger().info("VotingPlugin Updated to " + plugin.getDescription().getVersion() + " from "
						+ event.getOldVersion());
			}
		}
	}

}