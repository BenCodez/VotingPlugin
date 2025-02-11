package com.bencodez.votingplugin.bungee;

import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import net.md_5.bungee.event.EventHandler;

public class VoteEventBungee implements net.md_5.bungee.api.plugin.Listener {
	private VotingPluginBungee plugin;

	public VoteEventBungee(VotingPluginBungee plugin) {
		this.plugin = plugin;
	}

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
