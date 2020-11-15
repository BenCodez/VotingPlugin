package com.bencodez.votingplugin.listeners;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.bungeeapi.pluginmessage.PluginMessage;
import com.bencodez.votingplugin.BungeeHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.usermanager.UserManager;
import com.bencodez.votingplugin.voteparty.VoteParty;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class PlayerVoteListener implements Listener {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	private static Object object = new Object();

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerVoteListener(VotingPluginMain plugin) {
		PlayerVoteListener.plugin = plugin;
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onplayerVote(PlayerVoteEvent event) {
		String playerName = event.getPlayer();
		if (!PlayerUtils.getInstance().isValidUser(playerName, Config.getInstance().isAllowUnJoinedCheckServer())) {
			if (!config.isAllowUnjoined()) {
				plugin.getLogger().warning("Player " + playerName
						+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
				return;
			}
		}
		
		if (event.isBungee()) {
			plugin.debug("BungeePlayerVote forcebungee: " + event.isForceBungee() + ", bungeetotals: " + event.getBungeeTextTotals());
		}

		VoteSite voteSite = event.getVoteSite();

		// check valid service sites
		if (voteSite == null) {
			if (!Config.getInstance().isDisableNoServiceSiteMessage()) {
				plugin.getLogger().warning("No voting site with the service site: '" + event.getServiceSite() + "'");
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

		User user = UserManager.getInstance().getVotingPluginUser(playerName);
		user.updateName();

		if (Config.getInstance().isClearCacheOnVote() || BungeeSettings.getInstance().isUseBungeecoord()) {
			user.clearCache();
		}

		if (voteSite.isWaitUntilVoteDelay() && !user.canVoteSite(voteSite)) {
			plugin.getLogger().info(user.getPlayerName() + " must wait until votedelay is over, ignoring vote");
			return;
		}

		synchronized (object) {

			// vote party
			VoteParty.getInstance().vote(user, event.isRealVote());

			// broadcast vote if enabled in config
			if (config.isBroadcastVotesEnabled()
					&& (BungeeSettings.getInstance().isBungeeBroadcast() || !event.isBungee())) {
				if (!Config.getInstance().getFormatBroadcastWhenOnline() || user.isOnline()) {
					voteSite.broadcastVote(user);
				}
			}

			// update last vote time
			if (event.getTime() != 0) {
				user.setTime(voteSite, event.getTime());
			} else {
				user.setTime(voteSite);
			}
			user.setLastVoteCoolDownCheck(false, voteSite);

			// try logging to file
			if (Config.getInstance().isLogVotesToFile()) {
				try {
					VotingPluginMain.plugin.logVote(LocalDateTime.now().atZone(ZoneId.systemDefault()).toLocalDateTime(),
							playerName, voteSite.getKey());
				} catch (Exception e) {
					e.printStackTrace();
				}
			}

			// check first vote rewards
			SpecialRewards.getInstance().checkFirstVote(user);

			if (user.isReminded()) {
				user.setReminded(false);
			}

			// check if player has voted on all sites in one day

			if (((user.isOnline() || voteSite.isGiveOffline()) && VotingPluginMain.plugin.getOptions().isProcessRewards())
					|| event.isBungee()) {
				user.playerVote(voteSite, true, false, event.isForceBungee());
				user.closeInv();
			} else {
				user.addOfflineVote(voteSite.getKey());
				plugin.debug(
						"Offline vote set for " + playerName + " (" + user.getUUID() + ") on " + voteSite.getKey());
			}

			// add to total votes
			if ((Config.getInstance().isCountFakeVotes() || event.isRealVote()) && event.isAddTotals()) {
				if (Config.getInstance().isAddTotals()) {
					user.addTotal();
					user.addTotalDaily();
					user.addTotalWeekly();
				}
				user.addPoints();
			}

			user.checkDayVoteStreak();

			// other rewards
			SpecialRewards.getInstance().checkAllSites(user);
			SpecialRewards.getInstance().checkCumualativeVotes(user, event.getBungeeTextTotals());
			SpecialRewards.getInstance().checkMilestone(user, event.getBungeeTextTotals());

			if (BungeeSettings.getInstance().isUseBungeecoord()) {
				if (BungeeHandler.getInstance().getMethod().equals(BungeeMethod.MYSQL)) {
					final String uuid = user.getUUID();
					Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

						@Override
						public void run() {
							if (Bukkit.getOnlinePlayers().size() > 0) {
								PluginMessage.getInstance().sendPluginMessage(
										PlayerUtils.getInstance().getRandomOnlinePlayer(), "VoteUpdate", uuid);
							}
						}
					}, 40);
				}
			}
		}

		plugin.setUpdate(true);
	}

}
