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

	private Config() {
	}

	static Config instance = new Config();

	public static Config getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public Config(Main plugin) {
		Config.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

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

	public FileConfiguration getData() {
		return data;
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save Config.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public boolean getDebugEnabled() {
		return getData().getBoolean("debug");
	}

	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("broadcastvote");
	}

	public boolean getRemindVotesEnabled() {
		return getData().getBoolean("remindvotes");
	}

	public boolean getBonusRewardEnabled() {
		return getData().getBoolean("allvotesbonus");
	}

	public void setDebugEnabled(boolean value) {
		getData().set("debug", value);
		saveData();
	}

	public void setBroadcastVoteEnabled(boolean value) {
		getData().set("broadcastvote", value);
		saveData();
	}

	public void setRemindVotesEnabled(boolean value) {
		getData().set("remindvotes", value);
		saveData();
	}

	public void setBonusRewardEnabled(boolean value) {
		getData().set("allvotesbonus", value);
		saveData();
	}

	public boolean sendBungeeVotes() {
		return getData().getBoolean("sendbungeevotes");
	}

	public boolean recieveBungeeVotes() {
		return getData().getBoolean("recievebungeevotes");
	}

	public int bungeePort() {
		return getData().getInt("bungeeport");
	}

	public boolean updateReminder() {
		return getData().getBoolean("updatereminder");
	}

	public boolean allowAllNames() {
		return getData().getBoolean("allowallnames");
	}
}
