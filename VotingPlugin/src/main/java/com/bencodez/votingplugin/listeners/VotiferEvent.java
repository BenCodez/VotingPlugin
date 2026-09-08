package com.bencodez.votingplugin.listeners;

import java.util.concurrent.RejectedExecutionException;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
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
		try {
			plugin.getServerData().addServiceSite(voteSite);
			if (plugin.getBungeeSettings().isUseBungeecoord() && !plugin.getBungeeSettings().isVotifierBypass()
					&& (plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.SOCKETS)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.MQTT)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.REDIS))) {
				plugin.getLogger().severe(
						"Ignoring vote from votifier since pluginmessaging, socket, redis, or mqtt bungee method is enabled, this means you aren't setup correctly for those methods, please check: https://github.com/BenCodez/VotingPlugin/wiki/Bungeecord-Setups");
				return;
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
				plugin.getTimeQueueHandler().addVote(voteUsername, voteSite);
				return;
			}

			String voteSiteName = plugin.getVoteSiteManager().getVoteSiteName(true, serviceSite, matchSite);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSiteManager().getVoteSite(voteSiteName, true), voteUsername, voteSite, true);
			plugin.getServer().getPluginManager().callEvent(voteEvent);

			if (voteEvent.isCancelled()) {
				plugin.debug("Vote cancelled");
			}
		} catch (Exception e) {
			plugin.getLogger().severe("Error occured during vote processing");
			e.printStackTrace();
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
					+ MinecraftUsernameValidator.sanitizeForLog(voteSite) + "'");
			return;
		}

		plugin.getLogger()
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

		try {
			plugin.getVoteTimer().submit(() -> processVote(voteSite, voteUsername));
		} catch (RejectedExecutionException rejected) {
			VotifierVoteOverflowQueue overflow = plugin.getVotifierVoteOverflowQueue();
			if (overflow == null || !overflow.enqueue(voteUsername, voteSite)) {
				plugin.getLogger().severe("Votifier vote queue is full; vote was not admitted for "
						+ MinecraftUsernameValidator.sanitizeForLog(voteUsername));
			} else {
				plugin.getLogger().warning("Vote executor saturated; queued Votifier vote for retry");
			}
		}
	}
}
