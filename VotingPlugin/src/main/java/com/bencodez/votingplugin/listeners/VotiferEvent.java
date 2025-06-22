package com.bencodez.votingplugin.listeners;

import java.util.ArrayList;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

// TODO: Auto-generated Javadoc
/**
 * The Class VotiferEvent.
 */
public class VotiferEvent implements Listener {

	/** The plugin. */
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
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onVotiferEvent(VotifierEvent event) {

		Vote vote = event.getVote();
		String str = vote.getServiceName();
		if (str.isEmpty()) {
			str = "Empty";
		}
		final String voteSite = str;
		final String IP = vote.getAddress();
		final String voteUsername = vote.getUsername().trim();
		if (IP.equals("VotingPlugin")) {
			// ignore own plugin calls of event
			return;
		}

		if (voteUsername.length() == 0) {
			plugin.getLogger().warning("No name from vote on " + voteSite);
			return;
		}

		plugin.getLogger()
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

		plugin.getVoteTimer().submit(new Runnable() {

			@Override
			public void run() {
				try {
					plugin.getServerData().addServiceSite(voteSite);
					if (plugin.getBungeeSettings().isUseBungeecoord() && !plugin.getBungeeSettings().isVotifierBypass()
							&& (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
									|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.SOCKETS)
									|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.MQTT)
									|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.REDIS))) {
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

					String voteSiteNameStr = plugin.getVoteSiteName(false, voteSite, matchSite);

					ArrayList<String> sites = plugin.getConfigVoteSites().getVoteSitesNames(false);
					boolean createSite = false;
					if (sites != null) {
						if (!ArrayUtils.containsIgnoreCase(sites, voteSiteNameStr)) {
							createSite = true;
						}
					} else {
						createSite = true;
					}

					String serviceSite = voteSite;

					if (plugin.getConfigFile().isAutoCreateVoteSites() && createSite) {
						plugin.getLogger().warning("VoteSite with service site '" + voteSiteNameStr
								+ "' does not exist, attempting to generate...");
						plugin.getConfigVoteSites().generateVoteSite(voteSiteNameStr);

						plugin.getLogger().info("Current known service sites: "
								+ ArrayUtils.makeStringList(plugin.getServerData().getServiceSites()));
					}

					if (plugin.getTimeChecker().isActiveProcessing()
							&& plugin.getConfigFile().isQueueVotesDuringTimeChange()) {
						// time change in progress
						plugin.debug("Adding vote to time queue " + voteUsername + "/" + voteSite);
						plugin.getTimeQueueHandler().addVote(voteUsername, voteSite);

						return;
					}

					String voteSiteName = plugin.getVoteSiteName(true, serviceSite, matchSite);

					PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(voteSiteName, true),
							voteUsername, voteSite, true);
					plugin.getServer().getPluginManager().callEvent(voteEvent);

					if (voteEvent.isCancelled()) {
						plugin.debug("Vote cancelled");
						return;
					}
				} catch (Exception e) {
					plugin.getLogger().severe("Error occured during vote processing");
					e.printStackTrace();
				}

			}
		});
	}

}
