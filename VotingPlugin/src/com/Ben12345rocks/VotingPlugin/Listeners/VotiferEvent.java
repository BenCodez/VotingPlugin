package com.Ben12345rocks.VotingPlugin.Listeners;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
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
		final String voteSiteIP = vote.getAddress();
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

		plugin.getLogger()
				.info("Recieved a vote from '" + voteSite + "(" + voteSiteIP + ")' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("VoteSiteIP: " + voteSiteIP);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				String voteSiteName = plugin.getVoteSiteName(voteSite, voteSiteIP);

				ArrayList<String> sites = configVoteSites.getVoteSitesNames();
				if (sites != null) {
					if (!sites.contains(voteSiteName) && Config.getInstance().isAutoCreateVoteSites()) {
						plugin.getLogger()
								.warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
						ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
					}
				} else if (Config.getInstance().isAutoCreateVoteSites()) {
					plugin.getLogger().warning("VoteSite " + voteSiteName + " doe not exist, generaterating one...");
					ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);
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
