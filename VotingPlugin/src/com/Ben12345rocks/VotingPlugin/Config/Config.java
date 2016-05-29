package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

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

	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("BroadcastVote");
	}

	public FileConfiguration getData() {
		return data;
	}

	public boolean getDebugEnabled() {
		return getData().getBoolean("Debug");
	}

	public boolean getRemindVotesEnabled() {
		return getData().getBoolean("RemindVotes");
	}

	public boolean getTopVoterAwardsDisabled() {
		return getData().getBoolean("DisableTopVoterAwards");
	}

	public boolean getVoteSoundEnabled() {
		return getData().getBoolean("VoteSound.Enabled");
	}

	public float getVoteSoundPitch() {
		return (float) getData().getDouble("VoteSound.Pitch");
	}

	public String getVoteSoundSound() {
		return getData().getString("VoteSound.Sound");
	}

	public float getVoteSoundVolume() {
		return (float) getData().getDouble("VoteSound.Volume");
	}

	public boolean getVoteURLDefault() {
		return getData().getBoolean("VoteURLDefault");
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		Files.getInstance().editFile(dFile, data);
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

	public void setRemindVotesEnabled(boolean value) {
		getData().set("RemindVotes", value);
		saveData();
	}

	public void setTopVoterAwardsDisabled(boolean value) {
		getData().set("DisableTopVoterAwards", value);
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
