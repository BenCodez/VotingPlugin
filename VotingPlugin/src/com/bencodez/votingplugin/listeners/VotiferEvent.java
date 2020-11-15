package com.bencodez.votingplugin.listeners;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;
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
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotiferEvent(VotingPluginMain plugin) {
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
		final String IP = vote.getAddress();
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
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				String voteSiteName = plugin.getVoteSiteName(voteSite);

				ArrayList<String> sites = configVoteSites.getVoteSitesNames();
				boolean createSite = false;
				if (sites != null) {
					if (!sites.contains(voteSiteName)) {
						createSite = true;
					}
				} else {
					createSite = true;
				}

				if (Config.getInstance().isAutoCreateVoteSites() && createSite) {
					plugin.getLogger().warning("VoteSite with service site '" + voteSiteName
							+ "' does not exist, attempting to generaterate...");
					ConfigVoteSites.getInstance().generateVoteSite(voteSiteName);

					ArrayList<String> services = new ArrayList<String>();
					for (VoteSite site : plugin.getVoteSites()) {
						services.add(site.getServiceSite());
					}
					plugin.getLogger()
							.info("Current known service sites: " + ArrayUtils.getInstance().makeStringList(services));
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
