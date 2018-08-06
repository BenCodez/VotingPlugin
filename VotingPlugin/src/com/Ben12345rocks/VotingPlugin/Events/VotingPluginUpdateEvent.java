package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.PluginUpdateVersionEvent;
import com.Ben12345rocks.VotingPlugin.Main;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class AdvancedCoreUpdateEvent.
 */
public class VotingPluginUpdateEvent implements Listener {
	private Main main = ServiceLocator.getService(Main.class);

	/**
	 * Instantiates a new advanced core update event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotingPluginUpdateEvent() {
		
	}

	/**
	 * On plugin update.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPluginUpdate(PluginUpdateVersionEvent event) {
		if (event.getPlugin().getName().equals(main.getDescription().getName())) {
			if (!event.getOldVersion().equals("")) {
				main.getLogger().info("VotingPlugin Updated to " + main.getDescription().getVersion() + " from "
						+ event.getOldVersion());
			}
		}
	}

}