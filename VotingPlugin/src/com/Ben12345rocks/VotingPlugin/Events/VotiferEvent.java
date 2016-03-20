package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.API.VoteRecieved;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

public class VotiferEvent implements Listener {

	Main plugin = Main.plugin;

	public VotiferEvent(Main plugin) {
		this.plugin = plugin;
	}

	Config config = Config.getInstance();

	ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	ConfigFormat format = ConfigFormat.getInstance();

	ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onVotiferEvent(VotifierEvent event) {

		Vote vote = event.getVote();
		String voteSite = vote.getServiceName();
		String voteUsername = vote.getUsername();

		if (voteUsername.length() == 0) {
			plugin.getLogger().warning("No name from vote on " + voteSite);
			return;
		}

		plugin.getLogger().info(
				"Recieved a vote from '" + voteSite + "' by player '"
						+ voteUsername + "'!");

		if (config.getDebugEnabled()) {
			plugin.getLogger().info("PlayerUsername: " + voteUsername);
			plugin.getLogger().info("VoteSite: " + voteSite);
		}
		
		BungeeVote.getInstance().sendBungeeVote(voteUsername, voteSite);

		VoteRecieved.getInstance().playerVote(voteUsername, voteSite);
	}

}
