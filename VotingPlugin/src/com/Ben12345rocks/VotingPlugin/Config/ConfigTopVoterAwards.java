package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigTopVoterAwards.
 */
public class ConfigTopVoterAwards {

	/** The instance. */
	static ConfigTopVoterAwards instance = new ConfigTopVoterAwards();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigTopVoterAwards.
	 *
	 * @return single instance of ConfigTopVoterAwards
	 */
	public static ConfigTopVoterAwards getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config top voter awards.
	 */
	private ConfigTopVoterAwards() {
	}

	/**
	 * Instantiates a new config top voter awards.
	 *
	 * @param plugin the plugin
	 */
	public ConfigTopVoterAwards(Main plugin) {
		ConfigTopVoterAwards.plugin = plugin;
	}

	/**
	 * Gets the black list.
	 *
	 * @return the black list
	 */
	@SuppressWarnings("unchecked")
	public List<String> getBlackList() {
		return (List<String>) getData().getList("BlackList");
	}

	/**
	 * Gets the daily award rewards.
	 *
	 * @param pos the pos
	 * @return the daily award rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getDailyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"DailyAwards." + pos + ".Rewards");
	}

	/**
	 * Gets the daily awards enabled.
	 *
	 * @return the daily awards enabled
	 */
	public boolean getDailyAwardsEnabled() {
		return getData().getBoolean("EnableDailyAwards");
	}

	/**
	 * Gets the daily possible reward places.
	 *
	 * @return the daily possible reward places
	 */
	public Set<String> getDailyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("DailyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
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
	 * Gets the monthly award rewards.
	 *
	 * @param pos the pos
	 * @return the monthly award rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getMonthlyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"MonthlyAwards." + pos + ".Rewards");
	}

	/**
	 * Gets the monthly awards enabled.
	 *
	 * @return the monthly awards enabled
	 */
	public boolean getMonthlyAwardsEnabled() {
		return getData().getBoolean("EnableMonthlyAwards");
	}

	/**
	 * Gets the monthly possible reward places.
	 *
	 * @return the monthly possible reward places
	 */
	public Set<String> getMonthlyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("MonthlyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	/**
	 * Gets the weekly award rewards.
	 *
	 * @param pos the pos
	 * @return the weekly award rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getWeeklyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"WeeklyAwards." + pos + ".Rewards");
	}

	/**
	 * Gets the weekly awards enabled.
	 *
	 * @return the weekly awards enabled
	 */
	public boolean getWeeklyAwardsEnabled() {
		return getData().getBoolean("EnableWeeklyAwards");
	}

	/**
	 * Gets the weekly possible reward places.
	 *
	 * @return the weekly possible reward places
	 */
	public Set<String> getWeeklyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("WeeklyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
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
		Files.getInstance().editFile(dFile, data);
	}

	/**
	 * Sets the up.
	 *
	 * @param p the new up
	 */
	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "TopVoterAwards.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("TopVoterAwards.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
				.getLogger()
				.severe(ChatColor.RED
						+ "Could not create TopVoterAwards.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}
}
