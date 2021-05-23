package com.bencodez.votingplugin.listeners;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
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
		final String voteSite = vote.getServiceName();
		final String IP = vote.getAddress();
		final String voteUsername = vote.getUsername().trim();
		if (IP.equals("VotingPlugin")) {
			// ignore own plugin calls of event
			return;
		}
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				plugin.getServerData().addServiceSite(voteSite);
			}
		});

		if (voteUsername.length() == 0) {
			plugin.getLogger().warning("No name from vote on " + voteSite);
			return;
		}

		plugin.getLogger()
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {

				if (plugin.getBungeeSettings().isUseBungeecoord() && !plugin.getBungeeSettings().isVotifierBypass()
						&& (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
								|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.SOCKETS))) {
					plugin.getLogger().severe(
							"Ignoring vote from votifier since pluginmessaging or socket bungee method is enabled");
					return;
				}
				String voteSiteName = plugin.getVoteSiteName(voteSite);

				ArrayList<String> sites = plugin.getConfigVoteSites().getVoteSitesNames();
				boolean createSite = false;
				if (sites != null) {
					if (!sites.contains(voteSiteName)) {
						createSite = true;
					}
				} else {
					createSite = true;
				}

				if (plugin.getConfigFile().isAutoCreateVoteSites() && createSite) {
					plugin.getLogger().warning("VoteSite with service site '" + voteSiteName
							+ "' does not exist, attempting to generaterate...");
					plugin.getConfigVoteSites().generateVoteSite(voteSiteName);

					plugin.getLogger().info("Current known service sites: "
							+ ArrayUtils.getInstance().makeStringList(plugin.getServerData().getServiceSites()));
				}

				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(voteSiteName), voteUsername,
						voteSite, true);
				plugin.getServer().getPluginManager().callEvent(voteEvent);

				if (voteEvent.isCancelled()) {
					plugin.debug("Vote cancelled");
					return;
				}

			}
		});

	}

}
