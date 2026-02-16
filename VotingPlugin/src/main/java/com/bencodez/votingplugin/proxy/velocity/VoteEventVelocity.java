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
		plugin.getTimer().execute(new Runnable() {

			@Override
			public void run() {
				String serviceSite = serviceSiteVote;
				plugin.getLogger().info("Vote received " + name + " from service site " + serviceSite);
				if (serviceSite.isEmpty()) {
					serviceSite = "Empty";
				}

				plugin.getVotingPluginProxy().vote(name, serviceSite, true, false, 0, null, null);
			}
		});

	}
}
