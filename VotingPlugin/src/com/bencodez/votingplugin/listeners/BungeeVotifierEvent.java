package com.bencodez.votingplugin.listeners;

import com.bencodez.advancedcore.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

public class BungeeVotifierEvent {
	public void send(VotingPluginMain plugin, PlayerVoteEvent event) {
		plugin.debug("Triggering vote event");
		BukkitScheduler.runTask(plugin, new Runnable() {

			@Override
			public void run() {
				VotifierEvent e = new VotifierEvent(
						new Vote(event.getServiceSite(), event.getPlayer(), "VotingPlugin", "" + event.getTime()));
				plugin.getServer().getPluginManager().callEvent(e);
			}
		});
	}
}
