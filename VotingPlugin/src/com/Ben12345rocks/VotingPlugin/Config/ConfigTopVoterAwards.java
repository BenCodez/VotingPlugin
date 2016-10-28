package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigTopVoterAwards.
 */
public class ConfigTopVoterAwards extends YMLFile {

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

	/**
	 * Instantiates a new config top voter awards.
	 */
	public ConfigTopVoterAwards() {
		super(new File(plugin.getDataFolder(), "TopVoterAwards.yml"));
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
	 * @param pos
	 *            the pos
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
	 * Gets the monthly award rewards.
	 *
	 * @param pos
	 *            the pos
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
	 * Gets the store top voters daily.
	 *
	 * @return the store top voters daily
	 */
	public boolean getStoreTopVotersDaily() {
		return getData().getBoolean("StoreTopVoters.Daily");
	}

	/**
	 * Gets the store top voters monthly.
	 *
	 * @return the store top voters monthly
	 */
	public boolean getStoreTopVotersMonthly() {
		return getData().getBoolean("StoreTopVoters.Monthly");
	}

	/**
	 * Gets the store top voters weekly.
	 *
	 * @return the store top voters weekly
	 */
	public boolean getStoreTopVotersWeekly() {
		return getData().getBoolean("StoreTopVoters.Weekly");
	}

	/**
	 * Gets the weekly award rewards.
	 *
	 * @param pos
	 *            the pos
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

	@Override
	public void onFileCreation() {
		plugin.saveResource("TopVoterAwards.yml", true);

	}
}
