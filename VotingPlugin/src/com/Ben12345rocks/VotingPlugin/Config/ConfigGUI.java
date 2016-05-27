package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

public class ConfigGUI {

	static ConfigGUI instance = new ConfigGUI();

	static Main plugin = Main.plugin;

	public static ConfigGUI getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigGUI() {
	}

	public ConfigGUI(Main plugin) {
		ConfigGUI.plugin = plugin;
	}

	public int getVoteGUISlotAmount(String slot) {
		return data.getInt("GUI.VoteGUI." + slot + ".Item.Amount");
	}

	public String getVoteGUISlotCommand(String slot) {
		return data.getString("GUI.VoteGUI." + slot + ".Command");
	}

	public int getVoteGUISlotData(String slot) {
		return data.getInt("GUI.VoteGUI." + slot + ".Item.Data");
	}

	public int getVoteGUISlotID(String slot) {
		return data.getInt("GUI.VoteGUI." + slot + ".Item.ID");
	}

	public String getVoteGUISlotName(String slot) {
		return data.getString("GUI.VoteGUI." + slot + ".Item.Name");
	}

	public Set<String> getVoteGUISlots() {
		return data.getConfigurationSection("GUI.VoteGUI").getKeys(false);
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

		dFile = new File(p.getDataFolder(), "GUI.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("GUI.yml", true);
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create GUI.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

}
