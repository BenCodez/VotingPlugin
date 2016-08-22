package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.AdvancedCore.Util.Files.FilesManager;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigOtherRewards.
 */
public class ConfigOtherRewards {

	/** The instance. */
	static ConfigOtherRewards instance = new ConfigOtherRewards();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigOtherRewards.
	 *
	 * @return single instance of ConfigOtherRewards
	 */
	public static ConfigOtherRewards getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config other rewards.
	 */
	private ConfigOtherRewards() {
	}

	/**
	 * Instantiates a new config other rewards.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public ConfigOtherRewards(Main plugin) {
		ConfigOtherRewards.plugin = plugin;
	}

	/**
	 * Gets the all sites reward.
	 *
	 * @return the all sites reward
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getAllSitesReward() {
		try {
			return (ArrayList<String>) getData().getList("AllSites");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the cumulative reward enabled.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative reward enabled
	 */
	public boolean getCumulativeRewardEnabled(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".Enabled");
	}

	/**
	 * Gets the cumulative rewards.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCumulativeRewards(int cumulative) {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"Cumulative." + cumulative + ".Rewards");
			if (list != null) {
				return list;
			}

			return new ArrayList<String>();
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the cumulative votes.
	 *
	 * @return the cumulative votes
	 */
	public Set<String> getCumulativeVotes() {
		try {
			Set<String> set = getData().getConfigurationSection("Cumulative")
					.getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the cumulative votes in same day.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative votes in same day
	 */
	public boolean getCumulativeVotesInSameDay(int cumulative) {
		return getData().getBoolean(
				"Cumulative." + cumulative + ".VotesInSameDay");
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public FileConfiguration getData() {
		return data;
	}

	/**
	 * Gets the first vote rewards.
	 *
	 * @return the first vote rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFirstVoteRewards() {
		try {
			return (ArrayList<String>) getData().getList("FirstVote");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the vote party enabled.
	 *
	 * @return the vote party enabled
	 */
	public boolean getVotePartyEnabled() {
		return getData().getBoolean("VoteParty.Enabled");
	}

	/**
	 * Gets the vote party give all players.
	 *
	 * @return the vote party give all players
	 */
	public boolean getVotePartyGiveAllPlayers() {
		return getData().getBoolean("VoteParty.GiveAllPlayers");
	}

	/**
	 * Gets the vote party rewards.
	 *
	 * @return the vote party rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotePartyRewards() {
		return (ArrayList<String>) getData().getList("VoteParty.Rewards");
	}

	/**
	 * Gets the vote party votes required.
	 *
	 * @return the vote party votes required
	 */
	public int getVotePartyVotesRequired() {
		return getData().getInt("VoteParty.VotesRequired");
	}

	/**
	 * Gets the votes required.
	 *
	 * @return the votes required
	 */
	public int getVotesRequired() {
		return getData().getInt("VotesRequired");
	}

	public boolean getMinVotesEnabled() {
		return getData().getBoolean("MinVotes.Enabled");
	}

	public int getMinVotesVotes() {
		return getData().getInt("MinVotes.Votes");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getMinVotesRewards() {
		return (ArrayList<String>) getData().getList("MinVotes.Rewards",
				new ArrayList<String>());
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	/**
	 * Save data.
	 */
	public void saveData() {
		FilesManager.getInstance().editFile(dFile, data);
	}

	public void setCumulativeRewards(int cumulative, ArrayList<String> rewards) {
		getData().set("Cumulative." + cumulative + ".Rewards", rewards);
		saveData();
	}

	/**
	 * Sets the up.
	 *
	 * @param p
	 *            the new up
	 */
	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "Rewards.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("Rewards.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED + "Could not create Rewards.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

}
