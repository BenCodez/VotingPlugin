package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

// TODO: Auto-generated Javadoc
/**
 * The Class TopVoters.
 */
public class TopVoters {

	/** The instance. */
	static TopVoters instance = new TopVoters();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of TopVoters.
	 *
	 * @return single instance of TopVoters
	 */
	public static TopVoters getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new top voters.
	 */
	private TopVoters() {
	}

	/**
	 * Instantiates a new top voters.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public TopVoters(Main plugin) {
		TopVoters.plugin = plugin;
	}

	/**
	 * Store daily top voters.
	 *
	 * @param year
	 *            the year
	 * @param month
	 *            the month
	 * @param date
	 *            the date
	 * @param topVoters
	 *            the top voters
	 */
	public void storeDailyTopVoters(int year, int month, int date,
			String[] topVoters) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		if (!ConfigTopVoterAwards.getInstance().getStoreTopVotersDaily()) {
			plugin.debug("Not Storing TopVoters Daily");
			return;
		}

		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "TopVoters" + File.separator + "Daily", "TopVoters." + year
				+ "." + month + "." + date + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);

				plugin.debug("Created file: " + "TopVoters." + year + "."
						+ month + "." + date + ".yml");

			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create: " + "TopVoters."
								+ year + "." + month + "." + date + ".yml");

			}
		}

		data.set("All", topVoters);
		for (VoteSite voteSite : plugin.voteSites) {
			ArrayList<String> voteSiteTop = new ArrayList<String>();
			int i = 1;
			HashMap<User, Integer> topVoterSite = TopVoter.getInstance()
					.topVotersSortedVoteSiteDaily(voteSite);
			for (Entry<User, Integer> entry : topVoterSite.entrySet()) {
				voteSiteTop.add(i + ": " + entry.getKey().getPlayerName()
						+ ", " + entry.getValue().intValue());
				i++;
			}
			data.set(voteSite.getSiteName(), voteSiteTop);
		}
		try {
			data.save(dFile);
		} catch (IOException e) {
			plugin.getLogger().info(
					"Could not save: " + "TopVoters." + year + "." + month
							+ "." + date + ".yml");
		}

	}

	/**
	 * Store monthly top voters.
	 *
	 * @param year
	 *            the year
	 * @param month
	 *            the month
	 * @param topVoters
	 *            the top voters
	 */
	public void storeMonthlyTopVoters(int year, int month, String[] topVoters) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		if (!ConfigTopVoterAwards.getInstance().getStoreTopVotersMonthly()) {
			plugin.debug("Not Storing TopVoters Monthly");
			return;
		}

		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "TopVoters" + File.separator + "Monthly", "TopVoters." + year
				+ "." + month + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);

				plugin.debug("Created file: " + "TopVoters." + year + "."
						+ month + ".yml");

			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create: " + "TopVoters."
								+ year + "." + month + ".yml");

			}
		}

		data.set("All", topVoters);
		for (VoteSite voteSite : plugin.voteSites) {
			ArrayList<String> voteSiteTop = new ArrayList<String>();
			int i = 1;
			HashMap<User, Integer> topVoterSite = TopVoter.getInstance()
					.topVotersSortedVoteSite(voteSite);
			for (Entry<User, Integer> entry : topVoterSite.entrySet()) {
				voteSiteTop.add(i + ": " + entry.getKey().getPlayerName()
						+ ", " + entry.getValue().intValue());
				i++;
			}
			data.set(voteSite.getSiteName(), voteSiteTop);
		}
		try {
			data.save(dFile);
		} catch (IOException e) {
			plugin.getLogger().info(
					"Could not save: " + "TopVoters." + year + "." + month
							+ ".yml");
		}

	}

	/**
	 * Store weekly top voters.
	 *
	 * @param year
	 *            the year
	 * @param month
	 *            the month
	 * @param day
	 *            the day
	 * @param topVoters
	 *            the top voters
	 */
	public void storeWeeklyTopVoters(int year, int month, int day,
			String[] topVoters) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		if (!ConfigTopVoterAwards.getInstance().getStoreTopVotersWeekly()) {
			plugin.debug("Not Storing TopVoters Weekly");
			return;
		}

		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "TopVoters" + File.separator + "Weekly", "TopVoters." + year
				+ "." + month + "." + day + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);

				plugin.debug("Created file: " + "TopVoters." + year + "."
						+ month + "." + day + ".yml");

			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create: " + "TopVoters."
								+ year + "." + month + "." + day + ".yml");

			}
		}

		data.set("All", topVoters);
		for (VoteSite voteSite : plugin.voteSites) {
			ArrayList<String> voteSiteTop = new ArrayList<String>();
			int i = 1;
			HashMap<User, Integer> topVoterSite = TopVoter.getInstance()
					.topVotersSortedVoteSiteWeekly(voteSite);
			for (Entry<User, Integer> entry : topVoterSite.entrySet()) {
				voteSiteTop.add(i + ": " + entry.getKey().getPlayerName()
						+ ", " + entry.getValue().intValue());
				i++;
			}
			data.set(voteSite.getSiteName(), voteSiteTop);
		}
		try {
			data.save(dFile);
		} catch (IOException e) {
			plugin.getLogger().info(
					"Could not save: " + "TopVoters." + year + "." + month
							+ "." + day + ".yml");
		}

	}
}
