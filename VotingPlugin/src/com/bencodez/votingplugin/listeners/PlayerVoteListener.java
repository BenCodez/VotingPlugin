package com.bencodez.votingplugin.listeners;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.TimerTask;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class PlayerVoteListener implements Listener {

	private static Object object = new Object();

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
			return;
		}
		String playerName = event.getPlayer();
		if (!PlayerUtils.getInstance().isValidUser(playerName, plugin.getConfigFile().isAllowUnJoinedCheckServer())) {
			if (!plugin.getConfigFile().isAllowUnjoined()) {
				plugin.getLogger().warning("Player " + playerName
						+ " has not joined before, disregarding vote, set AllowUnjoined to true to prevent this");
				if (event.isBungee() && plugin.getBungeeSettings().isRemoveInvalidUsers()) {
					UserManager.getInstance().getVotingPluginUser(playerName).remove();
				}
				return;
			}
		}

		if (playerName.isEmpty()) {
			plugin.getLogger().warning("Empty player name for vote");
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
				plugin.getLogger()
						.warning("Current known service sites: " + ArrayUtils.getInstance().makeStringList(services));
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
			user = UserManager.getInstance().getVotingPluginUser(playerName);
		}

		if (voteSite.isWaitUntilVoteDelay() && !user.canVoteSite(voteSite)) {
			plugin.getLogger().info(user.getPlayerName() + " must wait until votedelay is over, ignoring vote");
			return;
		}

		synchronized (object) {
			if (!plugin.isEnabled()) {
				return;
			}
			// reupdate cache
			user.clearCache();
			user.cache();

			user.updateName(true);

			// vote party
			plugin.getVoteParty().vote(user, event.isRealVote(), event.isForceBungee());

			if (event.isBroadcast() && !plugin.getBungeeSettings().isDisableBroadcast()) {
				// broadcast vote if enabled in config
				if (plugin.getConfigFile().isBroadcastVotesEnabled()
						&& (plugin.getBungeeSettings().isBungeeBroadcast() || !event.isBungee())) {
					if (!plugin.getConfigFile().getFormatBroadcastWhenOnline() || user.isOnline()) {
						voteSite.broadcastVote(user);
					}
				}
			}

			if (plugin.getBroadcastHandler() != null) {
				plugin.getBroadcastHandler().onVote(playerName);
			}

			// update last vote time
			if (event.getTime() != 0) {
				user.setTime(voteSite, event.getTime());
			} else {
				user.setTime(voteSite);
			}
			plugin.getCoolDownCheck().vote(user);

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

			if (user.isReminded() && plugin.getConfigFile().getVoteRemindingRemindOnlyOnce()) {
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
				user.addOfflineVote(voteSite.getKey());
				plugin.debug(
						"Offline vote set for " + playerName + " (" + user.getUUID() + ") on " + voteSite.getKey());
			}

			// add to total votes
			if ((plugin.getConfigFile().isCountFakeVotes() || event.isRealVote()) && event.isAddTotals()) {
				if (plugin.getConfigFile().isAddTotals()) {
					user.addTotal();
					user.addTotalDaily();
					user.addTotalWeekly();
				}
				user.addPoints();
			}
			user.checkDayVoteStreak(event.isForceBungee());

			// other rewards
			plugin.getSpecialRewards().checkAllSites(user, event.isForceBungee());
			plugin.getSpecialRewards().checkCumualativeVotes(user, event.getBungeeTextTotals(), event.isForceBungee());
			plugin.getSpecialRewards().checkMilestone(user, event.getBungeeTextTotals(), event.isForceBungee());

			final String uuid = user.getUUID();
			if (plugin.getBungeeSettings().isUseBungeecoord()) {
				if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.MYSQL)) {

					Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

						@Override
						public void run() {
							if (Bukkit.getOnlinePlayers().size() > 0) {
								plugin.getPluginMessaging().sendPluginMessage("VoteUpdate", uuid);
							}
						}
					}, 40);
				}
			}

			PlayerPostVoteEvent postVoteEvent = new PlayerPostVoteEvent(voteSite, user, event.isRealVote(),
					event.isForceBungee());
			plugin.getServer().getPluginManager().callEvent(postVoteEvent);

			plugin.getTimer().schedule(new TimerTask() {

				@Override
				public void run() {
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid));
					if (!user.isOnline()) {
						user.clearCache();
					}
				}
			}, 1000 * 5);

		}

		plugin.setUpdate(true);
	}

}
