package com.bencodez.votingplugin.listeners;

import java.util.UUID;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.VoteOutcome;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.bencodez.votingplugin.util.VoteTaskAdmission;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

public class VotiferEvent implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public VotiferEvent(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Processes a validated vote. The overflow queue invokes this callback from
	 * its worker after the bounded vote executor has capacity again.
	 *
	 * @param voteSite the validated service site
	 * @param voteUsername the validated player name
	 */
	public void processVote(String voteSite, String voteUsername) {
		processVote(voteSite, voteUsername, UUID.randomUUID());
	}

	public void processVote(String voteSite, String voteUsername, UUID voteId) {
		VoteOutcome outcome = processVoteAttempt(voteSite, voteUsername, voteId);
		if (outcome == VoteOutcome.RETRY) {
			retainForAccountingRetry(voteSite, voteUsername, voteId);
		} else if (outcome == VoteOutcome.QUARANTINE
				&& !plugin.getVotifierVoteOverflowQueue().quarantine(voteUsername, voteSite, voteId)) {
			plugin.getLogger().severe("Unable to retain ambiguous Votifier vote " + voteId + " for manual review");
		} else if (outcome == VoteOutcome.QUARANTINE) {
			plugin.getLogger().severe("Votifier vote " + voteId
					+ " reached an ambiguous post-effect failure and was retained for manual review");
		}
	}

	public boolean processQueuedVote(String voteSite, String voteUsername, UUID voteId) {
		return processVoteAttempt(voteSite, voteUsername, voteId) == VoteOutcome.COMPLETE;
	}

	public VoteOutcome processQueuedVoteOutcome(String voteSite, String voteUsername, UUID voteId) {
		return processVoteAttempt(voteSite, voteUsername, voteId);
	}

	private VoteOutcome processVoteAttempt(String voteSite, String voteUsername, UUID voteId) {
		try {
			plugin.getServerData().addServiceSite(voteSite);
			if (plugin.getBungeeSettings().isUseBungeecoord() && !plugin.getBungeeSettings().isVotifierBypass()
					&& (plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.SOCKETS)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.HTTP)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.MQTT)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.REDIS))) {
				plugin.getLogger().severe(
						"Ignoring vote from votifier since a proxy vote transport is enabled; receive votes on the proxy or enable VotifierBypass, then check: https://github.com/BenCodez/VotingPlugin/wiki/Bungeecord-Setups");
				return VoteOutcome.COMPLETE;
			}

			String matchSite = "";
			if (plugin.getConfigFile().isAdvancedServiceSiteHandling()) {
				if (plugin.getServiceSiteHandler() != null) {
					matchSite = plugin.getServiceSiteHandler().matchReverse(voteSite);
				}
			}

			String voteSiteNameStr = plugin.getVoteSiteManager().getVoteSiteName(false, voteSite, matchSite);
			boolean createSite = !plugin.getVoteSiteManager().hasVoteSite(voteSiteNameStr)
					&& !plugin.getVoteSiteManager().hasConfiguredVoteSite(voteSiteNameStr);

			String serviceSite = voteSite;

			if (plugin.getConfigFile().isAutoCreateVoteSites() && createSite) {
				plugin.getLogger().warning("VoteSite with service site '" + voteSiteNameStr
						+ "' does not exist, attempting to generate...");
				if (plugin.getConfigVoteSites().tryAutoGenerateVoteSite(voteSiteNameStr)) {
					plugin.getLogger().info("Current known service sites: "
							+ ArrayUtils.makeStringList(plugin.getServerData().getServiceSites()));
				} else {
					plugin.getLogger().warning("Unable to generate VoteSite for service site '"
							+ ServiceSiteValidator.sanitizeForLog(voteSiteNameStr) + "'");
				}
			}

			if (plugin.getTimeChecker().isActiveProcessing()
					&& plugin.getConfigFile().isQueueVotesDuringTimeChange()) {
				plugin.debug("Adding vote to time queue " + voteUsername + "/" + voteSite);
				plugin.getTimeQueueHandler().addVote(voteId, voteUsername, voteSite);
				return VoteOutcome.COMPLETE;
			}

			String voteSiteName = plugin.getVoteSiteManager().getVoteSiteName(true, serviceSite, matchSite);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSiteManager().getVoteSite(voteSiteName, true), voteUsername, voteSite, true);
			voteEvent.setVoteId(voteId);
			plugin.getServer().getPluginManager().callEvent(voteEvent);
			if (voteEvent.isProcessingIncomplete()) {
				return voteEvent.isReplayUnsafe() ? VoteOutcome.QUARANTINE : VoteOutcome.RETRY;
			}

			if (voteEvent.isCancelled()) {
				plugin.debug("Vote cancelled");
			}
		} catch (Exception e) {
			plugin.getLogger().severe("Error occured during vote processing");
			plugin.debug(e);
			return VoteOutcome.RETRY;
		}
		return VoteOutcome.COMPLETE;
	}

	private void retainForAccountingRetry(String voteSite, String voteUsername, UUID voteId) {
		VotifierVoteOverflowQueue overflow = plugin.getVotifierVoteOverflowQueue();
		if (overflow == null || !overflow.enqueue(voteUsername, voteSite, voteId)) {
			plugin.getLogger().severe("Unable to retain vote after shared MySQL accounting admission failed");
		} else {
			plugin.getLogger().warning("Shared MySQL accounting is unavailable; retained Votifier vote for retry");
		}
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onVotiferEvent(VotifierEvent event) {

		Vote vote = event.getVote();
		String str = vote.getServiceName();
		if (str == null || str.isEmpty()) {
			str = "Empty";
		}
		final String voteSite = str;
		final String IP = vote.getAddress();
		final String voteUsername = vote.getUsername();
		if (IP.equals("VotingPlugin")) {
			return;
		}
		if (!ServiceSiteValidator.isValid(voteSite)) {
			plugin.getLogger().warning("Rejected vote with invalid service site '"
					+ ServiceSiteValidator.sanitizeForLog(voteSite) + "'");
			return;
		}

		if (!MinecraftUsernameValidator.isValid(voteUsername, plugin.getOptions().getBedrockPlayerPrefix())) {
			plugin.getLogger().warning("Rejected vote with invalid Minecraft username '"
					+ MinecraftUsernameValidator.sanitizeForLog(voteUsername) + "' from service '"
					+ ServiceSiteValidator.sanitizeForLog(voteSite) + "'");
			return;
		}

		plugin.getLogger()
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

		UUID voteId = UUID.randomUUID();
		if (!VoteTaskAdmission.trySubmit(plugin.getVoteTimer(),
				() -> processVote(voteSite, voteUsername, voteId))) {
			VotifierVoteOverflowQueue overflow = plugin.getVotifierVoteOverflowQueue();
			if (overflow == null || !overflow.enqueue(voteUsername, voteSite, voteId)) {
				plugin.getLogger().severe("Votifier vote queue is full; vote was not admitted for "
						+ MinecraftUsernameValidator.sanitizeForLog(voteUsername));
			} else {
				plugin.getLogger().warning("Vote executor saturated; queued Votifier vote for retry");
			}
		}
	}
}
