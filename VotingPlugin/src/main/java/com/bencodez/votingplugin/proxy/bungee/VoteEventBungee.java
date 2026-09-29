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
		Vote vote = event.getVote();
		String serviceName = vote.getServiceName();
		String serviceSite = serviceName == null || serviceName.isEmpty() ? "Empty" : serviceName;
		plugin.acceptIncomingVote(vote.getUsername(), serviceSite);
	}
}
