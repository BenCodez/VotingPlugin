package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.AdvancedCore.Util.Files.FilesManager;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class Config.
 */
public class Config {

	/** The instance. */
	static Config instance = new Config();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static Config getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config.
	 */
	private Config() {
	}

	/**
	 * Instantiates a new config.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public Config(Main plugin) {
		Config.plugin = plugin;
	}

	/**
	 * Allow un joined.
	 *
	 * @return true, if successful
	 */
	public boolean allowUnJoined() {
		return getData().getBoolean("AllowUnjoined");
	}

	/**
	 * Gets the auto create vote sites.
	 *
	 * @return the auto create vote sites
	 */
	public boolean getAutoCreateVoteSites() {
		return getData().getBoolean("AutoCreateVoteSites");
	}

	/**
	 * Gets the background task delay.
	 *
	 * @return the background task delay
	 */
	public int getBackgroundTaskDelay() {
		int num = getData().getInt("BackgroundTaskDelay");
		if (num == 0) {
			num = 600;
		}
		return num;
	}

	/**
	 * Gets the broad cast votes enabled.
	 *
	 * @return the broad cast votes enabled
	 */
	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("BroadcastVote");
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
	 * Gets the debug enabled.
	 *
	 * @return the debug enabled
	 */
	public boolean getDebugEnabled() {
		return getData().getBoolean("Debug");
	}

	/**
	 * Gets the debug info ingame.
	 *
	 * @return the debug info ingame
	 */
	public boolean getDebugInfoIngame() {
		return getData().getBoolean("DebugInfoIngame");
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getRewards() {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"Rewards");
			if (list != null) {
				return list;
			} else {
				return new ArrayList<String>();
			}
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the send scoreboards.
	 *
	 * @return the send scoreboards
	 */
	public boolean getSendScoreboards() {
		return getData().getBoolean("SendScoreboards");
	}

	/**
	 * Gets the vote URL default.
	 *
	 * @return the vote URL default
	 */
	public boolean getVoteURLDefault() {
		return getData().getBoolean("VoteURLDefault");
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

	/**
	 * Sets the allow un joined.
	 *
	 * @param value
	 *            the new allow un joined
	 */
	public void setAllowUnJoined(boolean value) {
		getData().set("AllowUnjoined", value);
		saveData();
	}

	/**
	 * Sets the broadcast vote enabled.
	 *
	 * @param value
	 *            the new broadcast vote enabled
	 */
	public void setBroadcastVoteEnabled(boolean value) {
		getData().set("BroadcastVote", value);
		saveData();
	}

	/**
	 * Sets the debug enabled.
	 *
	 * @param value
	 *            the new debug enabled
	 */
	public void setDebugEnabled(boolean value) {
		getData().set("Debug", value);
		saveData();
	}

	/**
	 * Sets the debug info ingame.
	 *
	 * @param value
	 *            the new debug info ingame
	 */
	public void setDebugInfoIngame(boolean value) {
		getData().set("DebugInfoIngame", value);
		saveData();
	}

	/**
	 * Sets the rewards.
	 *
	 * @param rewards
	 *            the new rewards
	 */
	public void setRewards(ArrayList<String> rewards) {
		getData().set("Rewards", rewards);
		saveData();
	}

	/**
	 * Sets the top voter awards enabled.
	 *
	 * @param value
	 *            the new top voter awards enabled
	 */
	public void setTopVoterAwardsEnabled(boolean value) {
		getData().set("TopVoterAwards", value);
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
