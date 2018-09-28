package com.Ben12345rocks.VotingPlugin.Listeners;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.UserManager.UserStorage;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
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

	private static Object object = new Object();

	public static void playerVote(final String playerName, final String voteSiteURL) {
		playerVote(playerName, voteSiteURL, true);
	}

	public static void playerVote(final String playerName, final String voteSiteURL, final boolean realVote) {

		if (!config.allowUnJoined() && !PlayerUtils.getInstance().isValidUser(playerName)) {
			plugin.getLogger().warning("Player " + playerName
					+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
			return;
		}

		User user = UserManager.getInstance().getVotingPluginUser(playerName);

		VoteSite voteSite = plugin.getVoteSite(voteSiteURL);
		
		// check valid service sites
		if (voteSite == null) {
			if (!Config.getInstance().getDisableNoServiceSiteMessage()) {
				plugin.getLogger().warning("No voting site with the service site: '" + voteSiteURL + "'");
				plugin.getLogger().warning(
						"Please read here on how to fix it: https://github.com/Ben12345rocks/VotingPlugin/wiki/Common-Problems");

				ArrayList<String> services = new ArrayList<String>();
				for (VoteSite site : plugin.getVoteSites()) {
					services.add(site.getServiceSite());
				}
				plugin.getLogger()
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

			// check first vote rewards
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
				plugin.debug(
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

			// other rewards
			OtherVoteReward.getInstance().checkAllSites(user);
			OtherVoteReward.getInstance().checkCumualativeVotes(user);
			OtherVoteReward.getInstance().checkMilestone(user);

			if (Config.getInstance().getClearCacheOnVote()) {
				if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
					AdvancedCoreHook.getInstance().getMysql().removePlayer(user.getUUID());
				}
			}
		}

		plugin.setUpdate(true);

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
		final String voteSite = vote.getServiceName();
		final String voteUsername = vote.getUsername().trim();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				ServerData.getInstance().addServiceSite(voteSite);
			}
		});

		if (voteUsername.length() == 0) {
			plugin.getLogger().warning("No name from vote on " + voteSite);
			return;
		}

		plugin.getLogger().info("Recieved a vote from '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				String voteSiteName = plugin.getVoteSiteName(voteSite);

				ArrayList<String> sites = configVoteSites.getVoteSitesNames();
				if (sites != null) {
					if (!sites.contains(voteSiteName) && Config.getInstance().getAutoCreateVoteSites()) {
						plugin.getLogger()
								.warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
						ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
					}
				} else if (Config.getInstance().getAutoCreateVoteSites()) {
					plugin.getLogger().warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
					ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
				}

				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(voteSiteName), voteUsername);
				plugin.getServer().getPluginManager().callEvent(voteEvent);

				if (voteEvent.isCancelled()) {
					plugin.debug("Vote cancelled");
					return;
				}

				playerVote(voteUsername, voteSite, true);

			}
		});

	}

}
