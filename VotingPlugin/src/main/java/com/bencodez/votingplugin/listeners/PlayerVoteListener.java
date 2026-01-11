package com.bencodez.votingplugin.listeners;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.bedrock.BedrockNameResolver;
import com.bencodez.advancedcore.api.misc.PlayerManager;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

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

		// check for name casing
		final String properName = plugin.getUserManager().getProperName(playerName);

		// Resolve Bedrock/Java + add prefix only when appropriate
		BedrockNameResolver.Result rn = plugin.getBedrockHandle().resolve(properName);
		String creditedName = rn.finalName;

		plugin.debug("Vote name resolved: " + properName + " -> " + creditedName + " (" + rn.rationale + ")");

		// Single validity check on the final name
		boolean allowUnJoinedCheckServer = plugin.getConfigFile().isAllowUnJoinedCheckServer();
		boolean isValid = PlayerManager.getInstance().isValidUser(creditedName, allowUnJoinedCheckServer);

		// Handle not-valid the same way you did before
		if (!isValid) {
			if (!plugin.getConfigFile().isAllowUnjoined()) {
				plugin.getLogger().warning("Player " + creditedName + " has not joined before, disregarding vote. "
						+ "Set AllowUnjoined to true to accept.");
				if (event.isBungee() && plugin.getBungeeSettings().isRemoveInvalidUsers()) {
					plugin.getVotingPluginUserManager().getVotingPluginUser(creditedName).remove();
				}
				return;
			}
		}

		// If we get here, use the resolved/possibly-prefixed name going forward
		playerName = creditedName;

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
			voteSite = plugin.getVoteSiteManager().getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, event.getServiceSite()), true);
		}

		// check valid service sites
		if (voteSite == null) {
			if (!plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
				plugin.getLogger().warning("No voting site with the service site: '" + event.getServiceSite() + "'");

				ArrayList<String> services = new ArrayList<>();
				for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) {
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
				// attempt to get exact name from cache matching casing
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(playerName);
			}
		}

		if (voteSite.isWaitUntilVoteDelay() && !user.canVoteSite(voteSite)) {
			if (!event.isRealVote()) {
				plugin.getLogger().info(user.getPlayerName() + " did a not real vote, bypassing WaitUntilVoteDelay");
			} else {
				if (!user.hasPermission("VotingPlugin.BypassWaitUntilVoteDelay")) {
					plugin.getLogger().info(user.getPlayerName() + " must wait until votedelay is over, ignoring vote");
					return;
				}
				plugin.getLogger()
						.info(user.getPlayerName() + " has bypass permission for WaitUntilVoteDelay, bypassing");
			}
		}

		UUID voteUUID = UUID.randomUUID();
		if (event.isBungee() && event.getBungeeTextTotals() != null) {
			voteUUID = event.getBungeeTextTotals().getVoteUUID();
			if (voteUUID == null) {
				voteUUID = UUID.randomUUID();
			}
		}

		final String uuid = user.getUUID();

		// reupdate cache
		user.cache();

		user.updateName(true);

		// vote party
		plugin.getVoteParty().vote(user, event.isRealVote(), event.isForceBungee());

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

		// check first vote rewards
		// plugin.getSpecialRewards().checkFirstVote(voteUUID, user,
		// event.isForceBungee());
		// plugin.getSpecialRewards().checkFirstVoteToday(voteUUID, user,
		// event.isForceBungee());

		if (user.isReminded() && plugin.getConfigFile().isVoteRemindingRemindOnlyOnce()) {
			user.setReminded(false);
		}

		boolean cached = false;
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
				cached = true;
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
			plugin.extraDebug("Current day of month: " + days + " Current total: " + value);
			if (value >= days * plugin.getVoteSiteManager().getVoteSitesEnabled().size()) {
				plugin.debug("Detected higher month total, changing. Current Total: " + value + " Days: " + days
						+ " New Total: " + days * plugin.getVoteSiteManager().getVoteSitesEnabled().size());
				user.setTotal(TopVoter.Monthly, days * plugin.getVoteSiteManager().getVoteSitesEnabled().size());
			}
		}

		plugin.getVoteMilestonesManager().handleVote(user, event.getBungeeTextTotals(), event.isForceBungee(), voteUUID,
				new HashMap<String, String>());

		// other rewards
		// plugin.getSpecialRewards().checkAllSites(voteUUID, user,
		// event.isForceBungee());
		// plugin.getSpecialRewards().checkAlmostAllSites(voteUUID, user,
		// event.isForceBungee());
		// plugin.getSpecialRewards().checkCumualativeVotes(voteUUID, user,
		// event.getBungeeTextTotals(),
		// event.isForceBungee());
		// plugin.getSpecialRewards().checkMilestone(voteUUID, user,
		// event.getBungeeTextTotals(), event.isForceBungee());
		plugin.getCoolDownCheck().vote(user, voteSite);

		plugin.getVoteStreakHandler().processVote(user, voteTime, voteUUID);

		PlayerPostVoteEvent postVoteEvent = new PlayerPostVoteEvent(voteSite, user, event.isRealVote(),
				event.isForceBungee(), voteTime, cached, voteSite.getServiceSite(), user.getJavaUUID(), playerName,
				voteUUID);
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
