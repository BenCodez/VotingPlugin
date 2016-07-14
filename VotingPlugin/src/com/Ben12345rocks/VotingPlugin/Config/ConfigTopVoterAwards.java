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

public class ConfigTopVoterAwards {

	static ConfigTopVoterAwards instance = new ConfigTopVoterAwards();

	static Main plugin = Main.plugin;

	public static ConfigTopVoterAwards getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigTopVoterAwards() {
	}

	public ConfigTopVoterAwards(Main plugin) {
		ConfigTopVoterAwards.plugin = plugin;
	}

	@SuppressWarnings("unchecked")
	public List<String> getBlackList() {
		return (List<String>) getData().getList("BlackList");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getDailyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"DailyAwards." + pos + ".Rewards");
	}

	public boolean getDailyAwardsEnabled() {
		return getData().getBoolean("EnableDailyAwards");
	}

	public Set<String> getDailyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("DailyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	public FileConfiguration getData() {
		return data;
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getMonthlyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"MonthlyAwards." + pos + ".Rewards");
	}

	public boolean getMonthlyAwardsEnabled() {
		return getData().getBoolean("EnableMonthlyAwards");
	}

	public Set<String> getMonthlyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("MonthlyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getWeeklyAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"WeeklyAwards." + pos + ".Rewards");
	}

	public boolean getWeeklyAwardsEnabled() {
		return getData().getBoolean("EnableWeeklyAwards");
	}

	public Set<String> getWeeklyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("WeeklyAwards").getKeys(
					false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		Files.getInstance().editFile(dFile, data);
	}

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
