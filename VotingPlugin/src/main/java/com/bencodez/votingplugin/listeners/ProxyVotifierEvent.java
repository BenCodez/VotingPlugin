package com.bencodez.votingplugin.listeners;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

/**
 * Handler for triggering Bungee Votifier events.
 */
public class ProxyVotifierEvent {
	/**
	 * Sends a Votifier event for a player vote.
	 *
	 * @param plugin the main plugin instance
	 * @param event  the player vote event
	 */
	public void send(VotingPluginMain plugin, PlayerVoteEvent event) {
		plugin.debug("Triggering proxy vote event");
		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				VotifierEvent e = new VotifierEvent(
						new Vote(event.getServiceSite(), event.getPlayer(), "VotingPlugin", "" + event.getTime()));
				plugin.getServer().getPluginManager().callEvent(e);
			}
		});
	}
}
