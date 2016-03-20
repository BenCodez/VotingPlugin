package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class ConfigVoteSites {

	private ConfigVoteSites() {
	}

	static ConfigVoteSites instance = new ConfigVoteSites();

	public static ConfigVoteSites getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public ConfigVoteSites(Main plugin) {
		ConfigVoteSites.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "VoteSites.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("VoteSites.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED
								+ "Could not create VoteSites.yml!");
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
					.severe(ChatColor.RED + "Could not save VoteSites.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public Set<String> getVoteSitesName() {
		return getData().getConfigurationSection("VoteSites").getKeys(false);
	}

	public ArrayList<VoteSite> getVoteSites() {
		ArrayList<VoteSite> voteSites = new ArrayList<VoteSite>();
		for (String site : getVoteSitesName()) {
			voteSites.add(new VoteSite(site));
		}
		return voteSites;
	}

	public String getVoteSiteServiceSite(String voteSite) {
		return getData().getString("VoteSites." + voteSite + ".Site");
	}

	public String getVoteURL(String voteSite) {
		return getData().getString("VoteSites." + voteSite + ".VoteURL");
	}

	public int getVoteDelay(String voteSite) {
		return getData().getInt("VoteSites." + voteSite + ".votedelay");
	}

	public int getMoneyAmount(String voteSite) {
		return getData().getInt("VoteSites." + voteSite + ".Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands(String voteSite) {
		return (ArrayList<String>) getData().getList(
				"VoteSites." + voteSite + ".Commands.Console");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands(String voteSite) {
		return (ArrayList<String>) getData().getList(
				"VoteSites." + voteSite + ".Commands.Player");
	}

	public Set<String> getItems(String voteSite) {
		return getData().getConfigurationSection(
				"VoteSites." + voteSite + ".Items").getKeys(false);
	}

	public int getItemID(String voteSite, String item) {
		return getData().getInt(
				"VoteSites." + voteSite + ".Items." + item + ".id");
	}

	public int getItemData(String voteSite, String item) {
		return getData().getInt(
				"VoteSites." + voteSite + ".Items." + item + ".Data");
	}

	public int getItemAmount(String voteSite, String item) {
		return getData().getInt(
				"VoteSites." + voteSite + ".Items." + item + ".Amount");
	}

	public String getItemName(String voteSite, String item) {
		return getData().getString(
				"VoteSites." + voteSite + ".Items." + item + ".Name");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getItemLore(String voteSite, String item) {
		return (ArrayList<String>) getData().getList(
				"VoteSites." + voteSite + ".Items." + item + ".Lore");
	}

}
