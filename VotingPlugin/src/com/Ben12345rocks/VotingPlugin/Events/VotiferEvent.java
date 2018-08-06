package com.Ben12345rocks.VotingPlugin.Events;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.UserStorage;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class VotiferEvent implements Listener {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();
	
	private Main main = ServiceLocator.getService(Main.class);

	private static Object object = new Object();

	public static void playerVote(final String playerName, final String voteSiteURL) {
		playerVote(playerName, voteSiteURL, true);
	}

	public static void playerVote(final String playerName, final String voteSiteURL, final boolean realVote) {
		Main main = ServiceLocator.getService(Main.class);

		if (!config.allowUnJoined() && !PlayerUtils.getInstance().isValidUser(playerName)) {
			main.getLogger().warning("Player " + playerName
					+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
			return;
		}

		User user = UserManager.getInstance().getVotingPluginUser(playerName);

		VoteSite voteSite = main.getVoteSite(voteSiteURL);
		if (voteSite == null) {
			if (!Config.getInstance().getDisableNoServiceSiteMessage()) {
				main.getLogger().warning("No voting site with the service site: '" + voteSiteURL + "'");
				main.getLogger().warning(
						"Please read here on how to fix it: https://github.com/Ben12345rocks/VotingPlugin/wiki/Common-Problems");

				ArrayList<String> services = new ArrayList<String>();
				for (VoteSite site : main.getVoteSites()) {
					services.add(site.getServiceSite());
				}
				main.getLogger()
						.warning("Current known service sites: " + ArrayUtils.getInstance().makeStringList(services));
			}
			return;
		}

		synchronized (object) {

			// vote party
			VoteParty.getInstance().vote(user, realVote);

			// broadcast vote if enabled in config
			if (config.getBroadCastVotesEnabled()) {
				if (!Config.getInstance().getFormatBroadcastWhenOnline() || user.isOnline()) {
					voteSite.broadcastVote(user);
				}
			}

			// update last vote time
			user.setTime(voteSite);

			OtherVoteReward.getInstance().checkFirstVote(user);

			if (user.isReminded()) {
				user.setReminded(false);
			}

			// check if player has voted on all sites in one day

			if (user.isOnline() || voteSite.isGiveOffline()) {
				user.playerVote(voteSite, true, false);
				user.closeInv();
			} else {
				user.addOfflineVote(voteSite.getKey());
				// main.debug(ArrayUtils.getInstance().makeStringList(user.getOfflineVotes()));
				main.debug(
						"Offline vote set for " + playerName + " (" + user.getUUID() + ") on " + voteSite.getKey());
			}

			// add to total votes
			if (Config.getInstance().getCountFakeVotes() || realVote) {
				if (Config.getInstance().getAddTotals()) {
					user.addTotal();
					user.addTotalDaily();
					user.addTotalWeekly();
				}
				user.addPoints();
			}

			OtherVoteReward.getInstance().checkAllSites(user);
			OtherVoteReward.getInstance().checkCumualativeVotes(user);
			OtherVoteReward.getInstance().checkMilestone(user);

			if (Config.getInstance().getClearCacheOnVote()) {
				if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
					AdvancedCoreHook.getInstance().getMysql().removePlayer(user.getUUID());
				}
			}
		}

		main.setUpdate(true);

	}

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotiferEvent() {
		
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
		final String voteSite = vote.getServiceName();
		final String voteUsername = vote.getUsername().trim();
		Bukkit.getScheduler().runTaskAsynchronously(main, new Runnable() {

			@Override
			public void run() {
				ServerData.getInstance().addServiceSite(voteSite);
			}
		});

		if (voteUsername.length() == 0) {
			main.getLogger().warning("No name from vote on " + voteSite);
			return;
		}

		main.getLogger().info("Recieved a vote from '" + voteSite + "' by player '" + voteUsername + "'!");

		main.debug("PlayerUsername: " + voteUsername);
		main.debug("VoteSite: " + voteSite);

		Bukkit.getScheduler().runTaskAsynchronously(main, new Runnable() {

			@Override
			public void run() {
				String voteSiteName = main.getVoteSiteName(voteSite);

				ArrayList<String> sites = configVoteSites.getVoteSitesNames();
				if (sites != null) {
					if (!sites.contains(voteSiteName) && Config.getInstance().getAutoCreateVoteSites()) {
						main.getLogger()
								.warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
						ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
					}
				} else if (Config.getInstance().getAutoCreateVoteSites()) {
					main.getLogger().warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
					ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
				}

				PlayerVoteEvent voteEvent = new PlayerVoteEvent(main.getVoteSite(voteSiteName), voteUsername);
				main.getServer().getPluginManager().callEvent(voteEvent);

				if (voteEvent.isCancelled()) {
					main.debug("Vote cancelled");
					return;
				}

				playerVote(voteUsername, voteSite, true);

			}
		});

	}

}
