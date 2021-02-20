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
		Vote vote = event.getVote();
		plugin.getLogger().info("Vote received " + vote.getUsername() + " from service site " + vote.getServiceName());

		plugin.vote(vote.getUsername(), vote.getServiceName(), true);

	}
}
