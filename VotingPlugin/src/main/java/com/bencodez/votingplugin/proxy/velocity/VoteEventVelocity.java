package com.bencodez.votingplugin.proxy.velocity;

import com.velocitypowered.api.event.Subscribe;
import com.vexsoftware.votifier.velocity.event.VotifierEvent;

/**
 * Handles vote events from Velocity proxy.
 */
public class VoteEventVelocity {
	private VotingPluginVelocity plugin;

	/**
	 * Constructs a new Velocity vote event handler.
	 * @param plugin the plugin instance
	 */
	public VoteEventVelocity(VotingPluginVelocity plugin) {
		this.plugin = plugin;
	}

	/**
	 * Handles Votifier vote events.
	 * @param event the votifier event
	 */
	@Subscribe
	public void onVotifierEvent(VotifierEvent event) {
		final String serviceSiteVote = event.getVote().getServiceName();
		final String name = event.getVote().getUsername();
		plugin.acceptIncomingVote(name,
				serviceSiteVote == null || serviceSiteVote.isEmpty() ? "Empty" : serviceSiteVote);
	}
}
