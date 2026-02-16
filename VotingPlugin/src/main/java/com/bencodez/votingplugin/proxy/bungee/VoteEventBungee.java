package com.bencodez.votingplugin.proxy.bungee;

import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import net.md_5.bungee.event.EventHandler;

/**
 * Handles vote events from Bungee proxy.
 */
public class VoteEventBungee implements net.md_5.bungee.api.plugin.Listener {
	private VotingPluginBungee plugin;

	/**
	 * Constructs a new Bungee vote event handler.
	 * @param plugin the plugin instance
	 */
	public VoteEventBungee(VotingPluginBungee plugin) {
		this.plugin = plugin;
	}

	/**
	 * Handles Votifier vote events.
	 * @param event the votifier event
	 */
	@EventHandler
	public void onVote(VotifierEvent event) {
		plugin.getProxy().getScheduler().runAsync(plugin, new Runnable() {

			@SuppressWarnings("deprecation")
			@Override
			public void run() {
				Vote vote = event.getVote();
				String serviceSite = vote.getServiceName();
				plugin.getLogger().info("Vote received " + vote.getUsername() + " from service site " + serviceSite);

				if (serviceSite.isEmpty()) {
					serviceSite = "Empty";
					vote.setServiceName(serviceSite);
				}

				plugin.getVotingPluginProxy().vote(vote.getUsername(), serviceSite, true, true, 0, null, null);
			}
		});

	}

}
