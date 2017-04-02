package com.Ben12345rocks.VotingPlugin.Events;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class VotiferEvent implements Listener {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

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
	public static synchronized void playerVote(final String playerName, final String voteSiteURL) {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				User user = UserManager.getInstance().getVotingPluginUser(playerName);
				if (!user.hasJoinedBefore() && !config.allowUnJoined()) {
					plugin.getLogger().warning("Player " + playerName
							+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
					return;
				}

				VoteSite voteSite = plugin.getVoteSite(voteSiteURL);
				if (voteSite == null) {
					if (!Config.getInstance().getDisableNoServiceSiteMessage()) {
						plugin.getLogger().warning("No voting site with the service site: '" + voteSiteURL + "'");
					}
					return;
				}

				// vote party
				if (Config.getInstance().getVotePartyEnabled()) {
					VoteParty.getInstance().addTotal(user);
					VoteParty.getInstance().addVotePlayer(user);
					VoteParty.getInstance().check();
				}

				// broadcast vote if enabled in config
				if (config.getBroadCastVotesEnabled()) {
					if (!Config.getInstance().getFormatBroadcastWhenOnline()) {
						voteSite.broadcastVote(user);
					} else if (user.isOnline()) {
						voteSite.broadcastVote(user);
					}
				}

				// update last vote time
				user.setTime(voteSite);

				boolean firstVote = OtherVoteReward.getInstance().checkFirstVote(user);

				// add to total votes
				user.addTotal(voteSite);
				user.addTotalDaily();
				user.addTotalWeekly();
				user.addPoints();

				user.setReminded(false);

				// check if player has voted on all sites in one day

				boolean allSites = OtherVoteReward.getInstance().checkAllSites(user);
				boolean cumulativeVotes = OtherVoteReward.getInstance().checkCumualativeVotes(user);
				boolean milestone = OtherVoteReward.getInstance().checkMilestone(user);

				if (user.isOnline()) {

					user.playerVote(voteSite, true, false);

					if (firstVote) {
						plugin.debug("FirstVote: true");
						OtherVoteReward.getInstance().giveFirstVoteRewards(user, true);

					}

					if (allSites) {
						plugin.debug("AllSites: true");
						OtherVoteReward.getInstance().giveAllSitesRewards(user, true);

					}

					if (milestone || cumulativeVotes) {
						//user.giveOfflineOtherRewards();
					}

					user.sendVoteEffects(true);
				} else {
					if (firstVote) {
						user.addOfflineOtherReward("FirstVote");
						plugin.debug("Offline first vote reward set for " + playerName);
					}

					if (allSites) {
						user.addOfflineOtherReward("AllSites");
						plugin.debug("Offline bonus reward set for " + playerName);
					}

					if (cumulativeVotes) {
						// cumulative votes are automaticly set offline
						plugin.debug("Offline cumulative reward set for " + playerName);
					}

					if (milestone) {
						// milestone votes are automaticly set offline
						plugin.debug("Offline milestone reward set for " + playerName);
					}

					user.addOfflineVote(voteSite.getKey());

					plugin.debug("Offline vote set for " + playerName + " on " + voteSite.getKey());

				}

				plugin.setUpdate(true);
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

		plugin.getLogger().info("Recieved a vote from '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);

		String voteSiteName = plugin.getVoteSiteName(voteSite);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(voteSiteName),
						UserManager.getInstance().getVotingPluginUser(voteUsername));
				plugin.getServer().getPluginManager().callEvent(voteEvent);

				if (voteEvent.isCancelled()) {
					return;
				}

				ArrayList<String> sites = configVoteSites.getVoteSitesNames();
				if (sites != null) {
					if (!sites.contains(voteSiteName) && Config.getInstance().getAutoCreateVoteSites()) {
						plugin.getLogger()
								.warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
						ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
						ConfigVoteSites.getInstance().setServiceSite(voteSiteName, voteSite);
					}
				} else if (Config.getInstance().getAutoCreateVoteSites()) {
					plugin.getLogger().warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
					ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
					ConfigVoteSites.getInstance().setServiceSite(voteSiteName, voteSite);
				}

				playerVote(voteUsername, voteSite);

			}
		});

	}

}
