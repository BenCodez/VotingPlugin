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
	public ArrayList<String> getAwardRewards(int pos) {
		return (ArrayList<String>) getData().getList(
				"Awards." + pos + ".Rewards");
	}

	@SuppressWarnings("unchecked")
	public List<String> getBlackList() {
		return (List<String>) getData().getList("BlackList");
	}

	public FileConfiguration getData() {
		return data;
	}

	public String getMessage(int place) {
		return getData().getString("Awards." + place + ".Message");
	}

	public Set<String> getPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("Awards").getKeys(false);
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
