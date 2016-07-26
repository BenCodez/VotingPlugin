package com.Ben12345rocks.VotingPlugin.Events;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class VotiferEvent implements Listener {

	/** The bonus reward. */
	static ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Player vote.
	 *
	 * @param playerName
	 *            the player name
	 * @param voteSiteURL
	 *            the vote site URL
	 */
	public static void playerVote(String playerName, String voteSiteURL) {
		User user = new User(playerName);
		if (!user.hasJoinedBefore() && !config.allowUnJoined()) {
			plugin.getLogger().info(
					"Player " + playerName
							+ " has not joined before, disregarding vote");
			return;
		}

		String voteSiteName = Utils.getInstance().getVoteSiteName(voteSiteURL);

		if (voteSiteName == null) {
			plugin.getLogger().info("Error on votesite name");
			return;
		}

		// ArrayList<String> sites = configVoteSites.getVoteSitesNames();

		VoteSite voteSite = plugin.getVoteSite(voteSiteName);
		if (voteSite == null) {

			plugin.debug("VoteSite is null");

			return;
		}

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				VoteParty.getInstance().addTotal();
				VoteParty.getInstance().addVotePlayer(user);
				VoteParty.getInstance().check();

				// broadcast vote if enabled in config
				if (config.getBroadCastVotesEnabled()) {
					if (!ConfigFormat.getInstance().getBroadcastWhenOnline()) {
						voteSite.broadcastVote(user);
					} else if (Utils.getInstance().isPlayerOnline(playerName)) {
						voteSite.broadcastVote(user);
					}
				}

				// update last vote time
				user.setTime(voteSite);

				// add to total votes
				user.addTotal(voteSite);
				user.addTotalDaily(voteSite);
				user.addTotalWeekly(voteSite);
				user.addPoints();

				user.setReminded(false);

				// check if player has voted on all sites in one day
				boolean firstVote = OtherVoteReward.getInstance()
						.checkFirstVote(user);
				boolean allSites = OtherVoteReward.getInstance().checkAllSites(
						user);
				boolean cumulativeVotes = OtherVoteReward.getInstance()
						.checkCumualativeVotes(user);

				if (Utils.getInstance().isPlayerOnline(playerName)) {

					user.playerVote(voteSite, true);

					if (firstVote) {
						plugin.debug("FirstVote: true");
						OtherVoteReward.getInstance().giveFirstVoteRewards(
								user, true);

					}

					if (allSites) {
						plugin.debug("AllSites: true");
						OtherVoteReward.getInstance().giveAllSitesRewards(user,
								true);

					}

					if (cumulativeVotes) {
						plugin.debug("Cumulative: true");
						Set<String> list = ConfigOtherRewards.getInstance()
								.getCumulativeVotes();
						for (String str : list) {
							if (Utils.getInstance().isInt(str)) {
								int votesRequired = Integer.parseInt(str);
								if (votesRequired != 0) {
									if (ConfigOtherRewards.getInstance()
											.getCumulativeRewardEnabled(
													votesRequired)) {
										int offlineVote = Data.getInstance()
												.getCumulativeVotesOffline(
														user, votesRequired);
										for (int i = 0; i < offlineVote; i++) {
											OtherVoteReward.getInstance()
													.giveCumulativeVoteReward(
															user, true,
															votesRequired);

										}
										if (offlineVote != 0) {
											Data.getInstance()
													.setCumuatliveVotesOffline(
															user,
															votesRequired, 0);
										}
									}
								}
							}
						}

					}
					user.sendVoteEffects(true);
				} else {
					if (firstVote) {
						Data.getInstance()
								.setFirstVoteOffline(
										user,
										Data.getInstance().getFirstVoteOffline(
												user) + 1);
						plugin.debug("Offline first vote reward set for "
								+ playerName);
					}

					if (allSites) {
						Data.getInstance()
								.setAllSitesOffline(
										user,
										Data.getInstance().getAllSitesOffline(
												user) + 1);
						plugin.debug("Offline bonus reward set for "
								+ playerName);
					}

					if (cumulativeVotes) {
						// cumulative votes are automaticly set offline
						plugin.debug("Offline number of votes reward set for "
								+ playerName);
					}

					user.addOfflineVote(voteSite);

					plugin.debug("Offline vote set for " + playerName + " on "
							+ voteSiteName);

				}

				plugin.update();
			}
		});

	}

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotiferEvent(Main plugin) {
		VotiferEvent.plugin = plugin;
	}

	/**
	 * On votifer event.
	 *
	 * @param event
	 *            the event
	 */
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

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);

		String voteSiteName = Utils.getInstance().getVoteSiteName(voteSite);

		PlayerVoteEvent voteEvent = new PlayerVoteEvent(
				plugin.getVoteSite(voteSiteName), new User(voteUsername));
		plugin.getServer().getPluginManager().callEvent(voteEvent);

		if (voteEvent.isCancelled()) {
			return;
		}

		ArrayList<String> sites = configVoteSites.getVoteSitesNames();
		if (sites != null) {
			if (!sites.contains(voteSiteName)
					&& Config.getInstance().getAutoCreateVoteSites()) {
				plugin.getLogger().warning(
						"VoteSite " + voteSiteName
								+ " doe not exist, generaterating one...");
				ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
				ConfigVoteSites.getInstance().setServiceSite(voteSiteName,
						voteSite);
			}
		} else if (Config.getInstance().getAutoCreateVoteSites()) {
			plugin.getLogger().warning(
					"VoteSite " + voteSiteName
							+ " doe not exist, generaterating one...");
			ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
			ConfigVoteSites.getInstance()
					.setServiceSite(voteSiteName, voteSite);
		}

		try {
			BungeeVote.getInstance().sendVote(vote);
		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			e.printStackTrace();
		}

		playerVote(voteUsername, voteSite);

	}

}
