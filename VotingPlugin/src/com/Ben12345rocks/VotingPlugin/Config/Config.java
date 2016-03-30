package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

public class Config {

	static Config instance = new Config();

	static Main plugin = Main.plugin;

	public static Config getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private Config() {
	}

	public Config(Main plugin) {
		Config.plugin = plugin;
	}

	public boolean allowUnJoined() {
		return getData().getBoolean("AllowUnjoined");
	}

	public boolean disableJson() {
		return true;
		// return getData().getBoolean("DisableJson");
	}

	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("BroadcastVote");
	}

	public FileConfiguration getData() {
		return data;
	}

	public boolean getDebugEnabled() {
		return getData().getBoolean("Debug");
	}

	public boolean getTopVoterAwardsDisabled() {
		return getData().getBoolean("DisableTopVoterAwards");
	}

	public void setTopVoterAwardsDisabled(boolean value) {
		getData().set("DisableTopVoterAwards", value);
		saveData();
	}

	public boolean getRemindVotesEnabled() {
		return getData().getBoolean("RemindVotes");
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save Config.yml!");
		}
	}

	public void setAllowUnJoined(boolean value) {
		getData().set("AllowUnjoined", value);
		saveData();
	}

	public void setBroadcastVoteEnabled(boolean value) {
		getData().set("BroadcastVote", value);
		saveData();
	}

	public void setDebugEnabled(boolean value) {
		getData().set("Debug", value);
		saveData();
	}

	public void setDisableJson(boolean value) {
		getData().set("DisableJson", value);
		saveData();
	}

	public void setRemindVotesEnabled(boolean value) {
		getData().set("RemindVotes", value);
		saveData();
	}

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "Config.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("Config.yml", true);
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create Config.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void setUpdateReminder(boolean value) {
		getData().set("UpdateReminder", value);
		saveData();
	}

	public boolean updateReminder() {
		return getData().getBoolean("UpdateReminder");
	}
}
