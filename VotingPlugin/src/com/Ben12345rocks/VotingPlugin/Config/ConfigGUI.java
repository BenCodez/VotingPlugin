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

	public FileConfiguration getData() {
		return data;
	}

	public int getVoteGUISlotAmount(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Amount");
	}

	public String getVoteGUISlotCommand(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Command");
	}

	public int getVoteGUISlotData(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Data");
	}

	public int getVoteGUISlotID(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.ID");
	}

	public String getVoteGUISlotName(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Item.Name");
	}

	public Set<String> getVoteGUISlots() {
		return getData().getConfigurationSection("GUI.VoteGUI").getKeys(false);
	}

	public int getVoteURLAlreadyVotedItemAmount() {
		return getData().getInt("GUI.VoteURL.AlreadyVotedItem.Amount");
	}

	public int getVoteURLAlreadyVotedItemData() {
		return getData().getInt("GUI.VoteURL.AlreadyVotedItem.Data");
	}

	public int getVoteURLAlreadyVotedItemID() {
		return getData().getInt("GUI.VoteURL.AlreadyVotedItem.ID");
	}

	public int getVoteURLCanVoteItemAmount() {
		return getData().getInt("GUI.VoteURL.CanVoteItem.Amount");
	}

	public int getVoteURLCanVoteItemData() {
		return getData().getInt("GUI.VoteURL.CanVoteItem.Data");
	}

	public int getVoteURLCanVoteItemID() {
		return getData().getInt("GUI.VoteURL.CanVoteItem.ID");
	}

	public String getVoteURLNextVote() {
		return getData().getString("GUI.VoteURL.NextVote");
	}

	public String getVoteURLSeeURL() {
		return getData().getString("GUI.VoteURL.SeeURL");
	}

	public String getVoteURLSiteName() {
		return getData().getString("GUI.VoteURL.SiteName");
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
