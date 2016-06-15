package com.Ben12345rocks.VotingPlugin.Events;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.BonusReward.BonusVoteReward;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

public class VotiferEvent implements Listener {

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static Main plugin = Main.plugin;

	public static void playerVote(String playerName, String voteSiteURL) {
		User user = new User(playerName);
		if (!user.hasJoinedBefore() && !config.allowUnJoined()) {
			plugin.getLogger().info(
					"Player " + playerName + " has not joined before");
			return;
		}

		String voteSiteName = Utils.getInstance().getVoteSiteName(voteSiteURL);

		if (voteSiteName == null) {
			plugin.getLogger().info("SiteName = null");
			return;
		}

		ArrayList<String> sites = configVoteSites.getVoteSitesNames();

		VoteSite voteSite = plugin.getVoteSite(voteSiteName);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {

				// check if a valid site
				if (sites != null) {
					if (!sites.contains(voteSiteName)
							&& !Config.getInstance()
									.getDisableAutoCreateVoteSites()) {
						ConfigVoteSites.getInstance().generateVoteSite(
								voteSiteName);
						ConfigVoteSites.getInstance().setServiceSite(
								voteSiteName, voteSiteURL);
						return;
					}
				} else if (!Config.getInstance()
						.getDisableAutoCreateVoteSites()) {
					plugin.getLogger().warning(
							"VoteSite " + voteSiteName
									+ " doe not exist, generaterating one...");
					ConfigVoteSites.getInstance()
							.generateVoteSite(voteSiteName);
					ConfigVoteSites.getInstance().setServiceSite(voteSiteName,
							voteSiteURL);
					return;
				} else {
					return;
				}

				// broadcast vote if enabled in config
				if (config.getBroadCastVotesEnabled()
						&& !ConfigFormat.getInstance().getBroadcastWhenOnline()) {
					voteSite.broadcastVote(user);
				}

				// update last vote time
				user.setTime(voteSite);

				// add to total votes
				user.addTotal(voteSite);

				user.setReminded(false);

				// check if player has voted on all sites in one day
				boolean allVotes = BonusVoteReward.getInstance()
						.giveBonusRewardUser(user);

				if (Utils.getInstance().isPlayerOnline(playerName)) {

					user.playerVote(voteSite);

					if (allVotes) {
						user.giveBonus();
					}

					user.playVoteSound();
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

	public VotiferEvent(Main plugin) {
		VotiferEvent.plugin = plugin;
	}

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

		String voteSiteName = Utils.getInstance().getVoteSiteName(voteSite);

		PlayerVoteEvent voteEvent = new PlayerVoteEvent(
				plugin.getVoteSite(voteSiteName), new User(voteUsername));
		plugin.getServer().getPluginManager().callEvent(voteEvent);

		if (voteEvent.isCancelled()) {
			return;
		}

		try {
			BungeeVote.getInstance().sendVote(vote);
		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			e.printStackTrace();
		}

		playerVote(voteUsername, voteSite);

	}

}
