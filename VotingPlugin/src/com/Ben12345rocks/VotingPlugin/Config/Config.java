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

	public boolean getAutoCreateVoteSites() {
		return getData().getBoolean("AutoCreateVoteSites");
	}

	public int getBackgroundTaskDelay() {
		int num = getData().getInt("BackgroundTaskDelay");
		if (num == 0) {
			num = 600;
		}
		return num;
	}
	
	public boolean getSendScoreboards() {
		return getData().getBoolean("SendScoreboards");
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

	public int getEffectData() {
		return getData().getInt("Effect.Data");
	}

	public String getEffectEffect() {
		return getData().getString("Effect.Effect");
	}

	public boolean getEffectEnabled() {
		return getData().getBoolean("Effect.Enabled");
	}

	public int getEffectParticles() {
		return getData().getInt("Effect.Particles");
	}

	public int getEffectRadius() {
		return getData().getInt("Effect.Radius");
	}

	public boolean getSoundEnabled() {
		return getData().getBoolean("Sound.Enabled");
	}

	public float getSoundPitch() {
		return (float) getData().getDouble("Sound.Pitch");
	}

	public String getSoundSound() {
		return getData().getString("Sound.Sound");
	}

	public float getSoundVolume() {
		return (float) getData().getDouble("Sound.Volume");
	}

	public boolean getTitleEnabled() {
		return getData().getBoolean("Title.Enabled");
	}

	public int getTitleFadeIn() {
		return getData().getInt("Title.FadeIn");
	}

	public int getTitleFadeOut() {
		return getData().getInt("Title.FadeOut");
	}

	public int getTitleShowTime() {
		return getData().getInt("Title.ShowTime");
	}

	public String getTitleSubTitle() {
		return getData().getString("Title.SubTitle");
	}

	public String getTitleSubTitleColor() {
		return getData().getString("Title.SubTitleColor");
	}

	public String getTitleTitle() {
		return getData().getString("Title.Title");
	}

	public String getTitleTitleColor() {
		return getData().getString("Title.TitleColor");
	}

	public boolean getTopVoterAwardsEnabled() {
		return getData().getBoolean("TopVoterAwards");
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

	public void setTopVoterAwardsEnabled(boolean value) {
		getData().set("TopVoterAwards", value);
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

}
