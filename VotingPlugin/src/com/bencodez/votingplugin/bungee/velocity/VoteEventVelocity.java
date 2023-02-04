package com.bencodez.votingplugin.bungee.velocity;

import com.velocitypowered.api.event.Subscribe;
import com.vexsoftware.votifier.velocity.event.VotifierEvent;

public class VoteEventVelocity {
	private VotingPluginVelocity plugin;

	public VoteEventVelocity(VotingPluginVelocity plugin) {
		this.plugin = plugin;
	}
	
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

				plugin.vote(name, serviceSite, true, false, 0, null, null);
			}
		});

	}
}
