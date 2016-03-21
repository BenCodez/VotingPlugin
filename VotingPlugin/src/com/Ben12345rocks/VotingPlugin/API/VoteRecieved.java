package com.Ben12345rocks.VotingPlugin.API;

import java.util.Set;

import org.bukkit.Bukkit;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class VoteRecieved {

	private VoteRecieved() {
	}

	static VoteRecieved instance = new VoteRecieved();

	public static VoteRecieved getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public VoteRecieved(Main plugin) {
		VoteRecieved.plugin = plugin;
	}

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	public void playerVote(String playerName, String voteSiteURL) {
		User user = new User(playerName);
		if (!user.hasJoinedBefore() && !config.allowAllNames()) {
			plugin.getLogger().info("Player has not joined before");
			return;
		}

		String voteSiteName = Utils.getInstance().getVoteSiteName(voteSiteURL);

		Set<String> sites = configVoteSites.getVoteSitesName();

		// check if a valid site
		if (!sites.contains(voteSiteName)) {
			plugin.getLogger()
					.warning(
							"Site '"
									+ voteSiteName
									+ "' is not registered! Please check it is added to your config!");
			return;
		}

		VoteSite voteSite = new VoteSite(voteSiteName);

		// broadcast vote if enabled in config
		if (config.getBroadCastVotesEnabled()) {
			voteSite.broadcastVote(user);
		}

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				// update last vote time
				user.setTime(voteSite);
				// Data.getInstance().setTime(voteSiteName, playerName);

				// add to total votes
				user.addTotal(voteSite);
				// Data.getInstance().addTotal(playerName, voteSiteName);

				user.setReminded(false);

				// check if player has voted on all sites in one day
				boolean allVotes = user.checkAllVotes();

				if (Utils.getInstance().isPlayerOnline(playerName)) {
					user.playerVote(voteSite);
					// SiteVoteReward.getInstance().giveSiteReward(user,
					// voteSite);
					if (allVotes) {
						// BonusVoteReward.getInstance().giveBonusReward(user);
						user.giveBonus();
					}
				} else {
					if (allVotes) {
						user.addBonusOfflineVote();
					}

					user.addOfflineVote(voteSite);

					if (config.getDebugEnabled()) {
						plugin.getLogger().info(
								"Offline vote set for " + playerName + " on "
										+ voteSiteName);
					}
				}

				plugin.updateTopUpdater();
			}
		});

	}

}
