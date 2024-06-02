package com.bencodez.votingplugin.listeners;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.misc.PlayerManager;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class PlayerVoteListener implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerVoteListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onplayerVote(PlayerVoteEvent event) {
		if (!plugin.isEnabled()) {
			plugin.getLogger().warning("Plugin disabled, ignoring vote");
			return;
		}
		String playerName = event.getPlayer();
		plugin.debug("Processing PlayerVoteEvent: " + playerName + "/" + event.getServiceSite());
		if (!PlayerManager.getInstance().isValidUser(playerName, plugin.getConfigFile().isAllowUnJoinedCheckServer())) {
			if (!plugin.getConfigFile().isAllowUnjoined()) {
				plugin.getLogger().warning("Player " + playerName
						+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
				if (event.isBungee() && plugin.getBungeeSettings().isRemoveInvalidUsers()) {
					plugin.getVotingPluginUserManager().getVotingPluginUser(playerName).remove();
				}
				return;
			}
		}

		if (playerName.isEmpty()) {
			plugin.getLogger().warning("Empty player name from vote, ignoring");
			return;
		}

		if (event.isBungee()) {
			plugin.debug("BungeePlayerVote forcebungee: " + event.isForceBungee() + ", bungeetotals: "
					+ event.getBungeeTextTotals());

			if (plugin.getBungeeSettings().isTriggerVotifierEvent()) {
				new BungeeVotifierEvent().send(plugin, event);
			}
		}

		VoteSite voteSite = event.getVoteSite();

		if (voteSite == null) {
			voteSite = plugin.getVoteSite(plugin.getVoteSiteName(true, event.getServiceSite()), true);
		}

		// check valid service sites
		if (voteSite == null) {
			if (!plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
				plugin.getLogger().warning("No voting site with the service site: '" + event.getServiceSite() + "'");

				ArrayList<String> services = new ArrayList<String>();
				for (VoteSite site : plugin.getVoteSites()) {
					services.add(site.getServiceSite());
				}
				plugin.getLogger().warning("Currently set service sites: " + ArrayUtils.makeStringList(services));
			}
			return;
		}

		if (!voteSite.isEnabled()) {
			plugin.debug("Votesite: " + voteSite.getKey() + " is not enabled");
			return;
		}

		VotingPluginUser user = null;
		if (event.getVotingPluginUser() != null) {
			user = event.getVotingPluginUser();
		} else {
			Player p = Bukkit.getPlayerExact(playerName);
			if (p != null) {
				// player online
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
			} else {
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(playerName);
			}
		}

		if (voteSite.isWaitUntilVoteDelay() && !user.canVoteSite(voteSite)) {
			if (!event.isRealVote()) {
				plugin.getLogger().info(user.getPlayerName() + " did a not real vote, bypassing WaitUntilVoteDelay");
			} else {
				if (user.hasPermission("VotingPlugin.BypassWaitUntilVoteDelay")) {
					plugin.getLogger()
							.info(user.getPlayerName() + " has bypass permission for WaitUntilVoteDelay, bypassing");
				} else {
					plugin.getLogger().info(user.getPlayerName() + " must wait until votedelay is over, ignoring vote");
					return;
				}
			}
		}

		final String uuid = user.getUUID();

		// reupdate cache
		user.cache();

		user.updateName(true);

		if (event.isBroadcast() && !plugin.getBungeeSettings().isDisableBroadcast()) {
			// broadcast vote if enabled in config
			if (plugin.getConfigFile().isBroadcastVotesEnabled()
					&& (plugin.getBungeeSettings().isBungeeBroadcast() || !event.isBungee())) {
				if (!plugin.getConfigFile().isFormatBroadcastWhenOnline() || user.isOnline()) {
					voteSite.broadcastVote(user);
				}
			}
		}

		if (plugin.getBroadcastHandler() != null) {
			plugin.getBroadcastHandler().onVote(playerName);
		}

		long voteTime = 0;
		// update last vote time
		if (event.getTime() != 0) {
			user.setTime(voteSite, event.getTime());
			voteTime = event.getTime();
		} else {
			user.setTime(voteSite);
			voteTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		}

		// try logging to file
		if (plugin.getConfigFile().isLogVotesToFile()) {
			try {
				plugin.logVote(LocalDateTime.now().atZone(ZoneId.systemDefault()).toLocalDateTime(), playerName,
						voteSite.getKey());
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		// check first vote rewards
		plugin.getSpecialRewards().checkFirstVote(user, event.isForceBungee());
		plugin.getSpecialRewards().checkFirstVoteToday(user, event.isForceBungee());

		if (user.isReminded() && plugin.getConfigFile().isVoteRemindingRemindOnlyOnce()) {
			user.setReminded(false);
		}

		// check if player has voted on all sites in one day
		if (((user.isOnline() || voteSite.isGiveOffline()) && plugin.getOptions().isProcessRewards())
				|| event.isBungee()) {
			boolean online = true;
			if (event.isBungee()) {
				online = event.isWasOnline();
			}
			user.playerVote(voteSite, online, false, event.isForceBungee());
			if (event.getVoteNumber() == 1) {
				user.sendVoteEffects(online);
			}
			if (plugin.getConfigFile().isCloseInventoryOnVote()) {
				user.closeInv();
			}
		} else {
			if (!plugin.getConfigFile().isOfflineVotesLimitEnabled()
					|| user.getNumberOfOfflineVotes(voteSite) <= plugin.getConfigFile().getOfflineVotesLimitAmount()) {
				user.addOfflineVote(voteSite.getKey());
				plugin.debug(
						"Offline vote set for " + playerName + " (" + user.getUUID() + ") on " + voteSite.getKey());
			} else {
				plugin.debug("Not setting offline vote, offline vote limit reached");
			}
		}

		// add to total votes
		if ((plugin.getConfigFile().isCountFakeVotes() || event.isRealVote()) && event.isAddTotals()) {
			if (plugin.getConfigFile().isAddTotals()) {
				if (plugin.getConfigFile().isAddTotalsOffline() || user.isOnline()) {
					user.addTotal();
					user.addTotalDaily();
					user.addTotalWeekly();
				}
			}
			user.addPoints();
		}
		user.checkDayVoteStreak(event.isForceBungee());

		if (plugin.getConfigFile().isLimitMonthlyVotes()) {
			int value = 0;
			if (event.isBungee()) {
				value = event.getBungeeTextTotals().getMonthTotal();
			} else {
				value = user.getTotal(TopVoter.Monthly);
			}
			LocalDateTime cTime = plugin.getTimeChecker().getTime();
			int days = cTime.getDayOfMonth();
			if (value >= days * plugin.getVoteSitesEnabled().size()) {
				user.setTotal(TopVoter.Monthly, days * plugin.getVoteSitesEnabled().size());
			}
		}

		// vote party
		plugin.getVoteParty().vote(user, event.isRealVote(), event.isForceBungee());

		// other rewards
		plugin.getSpecialRewards().checkAllSites(user, event.isForceBungee());
		plugin.getSpecialRewards().checkAlmostAllSites(user, event.isForceBungee());
		plugin.getSpecialRewards().checkCumualativeVotes(user, event.getBungeeTextTotals(), event.isForceBungee());
		plugin.getSpecialRewards().checkMilestone(user, event.getBungeeTextTotals(), event.isForceBungee());
		plugin.getCoolDownCheck().vote(user, voteSite);

		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.MYSQL)) {

				plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

					@Override
					public void run() {
						if (Bukkit.getOnlinePlayers().size() > 0) {
							plugin.getBungeeHandler().getGlobalMessageHandler().sendMessage("VoteUpdate", uuid);
						}
					}
				}, 2);
			}
		}

		PlayerPostVoteEvent postVoteEvent = new PlayerPostVoteEvent(voteSite, user, event.isRealVote(),
				event.isForceBungee(), voteTime);
		plugin.getServer().getPluginManager().callEvent(postVoteEvent);

		if (user.isOnline()) {
			plugin.getPlaceholders().onUpdate(user, true);
		}

		if (!user.isOnline()) {
			user.clearCache();
		}
		plugin.setUpdate(true);

		plugin.extraDebug("Finished vote processing: " + playerName + "/" + uuid);
	}

}
