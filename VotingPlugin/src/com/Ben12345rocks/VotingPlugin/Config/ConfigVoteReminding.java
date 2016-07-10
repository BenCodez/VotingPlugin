package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

public class ConfigVoteReminding {

	static ConfigVoteReminding instance = new ConfigVoteReminding();

	static Main plugin = Main.plugin;

	public static ConfigVoteReminding getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigVoteReminding() {
	}

	public ConfigVoteReminding(Main plugin) {
		ConfigVoteReminding.plugin = plugin;
	}

	public int getChance() {
		return getData().getInt("Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsConsole() {
		try {
			return (ArrayList<String>) getData().getList("Commands.Console");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsPlayer() {
		try {
			return (ArrayList<String>) getData().getList("Commands.Player");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	public FileConfiguration getData() {
		return data;
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

	public boolean getEnabled() {
		return getData().getBoolean("Enabled");
	}

	public String getMessagesRemind() {
		String msg = getData().getString("Messages.Remind");
		if (msg != null) {
			return msg;
		} else {
			return "&cRemember to vote";
		}
	}

	public int getRemindDelay() {
		int num = getData().getInt("RemindDelay");
		if (num != 0) {
			return num;
		}
		return 30;
	}

	public boolean getRemindOnLogin() {
		return getData().getBoolean("RemindOnLogin");
	}

	public boolean getRemindOnlyOnce() {
		return getData().getBoolean("RemindOnlyOnce");
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

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		Files.getInstance().editFile(dFile, data);
	}

	public void set(String path, Object value) {
		getData().set(path, value);
		saveData();
	}

	public void setCommandsConsole(ArrayList<String> value) {
		set("Commands.Console", value);
	}

	public void setCommandsPlater(ArrayList<String> value) {
		set("Commands.Player", value);
	}

	public void setMessagesReward(String value) {
		set("Messages.Reward", value);
	}

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "VoteReminding.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("VoteReminding.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED
								+ "Could not create VoteReminding.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

}
