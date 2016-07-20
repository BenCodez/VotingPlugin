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

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigVoteReminding.
 */
public class ConfigVoteReminding {

	/** The instance. */
	static ConfigVoteReminding instance = new ConfigVoteReminding();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigVoteReminding.
	 *
	 * @return single instance of ConfigVoteReminding
	 */
	public static ConfigVoteReminding getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config vote reminding.
	 */
	private ConfigVoteReminding() {
	}

	/**
	 * Instantiates a new config vote reminding.
	 *
	 * @param plugin the plugin
	 */
	public ConfigVoteReminding(Main plugin) {
		ConfigVoteReminding.plugin = plugin;
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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public boolean getEnabled() {
		return getData().getBoolean("Enabled");
	}

	/**
	 * Gets the remind delay.
	 *
	 * @return the remind delay
	 */
	public int getRemindDelay() {
		int num = getData().getInt("RemindDelay");
		if (num != 0) {
			return num;
		}
		return 30;
	}

	/**
	 * Gets the remind on login.
	 *
	 * @return the remind on login
	 */
	public boolean getRemindOnLogin() {
		return getData().getBoolean("RemindOnLogin");
	}

	/**
	 * Gets the remind only once.
	 *
	 * @return the remind only once
	 */
	public boolean getRemindOnlyOnce() {
		return getData().getBoolean("RemindOnlyOnce");
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
	 * Sets the.
	 *
	 * @param path the path
	 * @param value the value
	 */
	public void set(String path, Object value) {
		getData().set(path, value);
		saveData();
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
