package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

public class ConfigBonusReward {

	private ConfigBonusReward() {
	}

	static ConfigBonusReward instance = new ConfigBonusReward();

	public static ConfigBonusReward getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public ConfigBonusReward(Main plugin) {
		ConfigBonusReward.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "BonusReward.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("BonusReward.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
				.getLogger()
				.severe(ChatColor.RED
						+ "Could not create BonusReward.yml!");
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
			.severe(ChatColor.RED + "Could not save BonusReward.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public int getMoneyAmount() {
		return getData().getInt("AllVotesReward.Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands() {
		return (ArrayList<String>) getData().getList(
				"AllVotesReward.Commands.Console");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands() {
		return (ArrayList<String>) getData().getList(
				"AllVotesReward.Commands.Player");
	}

	public Set<String> getItems() {
		return getData().getConfigurationSection("AllVotesReward.Items")
				.getKeys(false);
	}

	public int getItemID(String item) {
		return getData().getInt("AllVotesReward.Items." + item + ".id");
	}

	public int getItemData(String item) {
		return getData().getInt("AllVotesReward.Items." + item + ".Data");
	}

	public int getItemAmount(String item) {
		return getData().getInt("AllVotesReward.Items." + item + ".Amount");
	}

	public String getItemName(String item) {
		return getData().getString("AllVotesReward.Items." + item + ".Name");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getItemLore(String item) {
		return (ArrayList<String>) getData().getList(
				"AllVotesReward.Items." + item + ".Lore");
	}

	public HashMap<String, Integer> getEnchantments(String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"AllVotesReward.Items." + item + ".Enchants")
					.getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant, getEnchantLevel(item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}
	}

	public int getEnchantLevel(String item, String enchant) {
		return getData().getInt(
				"AllVotesReward.Items." + item + ".Enchants." + enchant);
	}

	public int getChance(String item) {
		return getData().getInt("AllVotesReward.Items." + item + ".Chance");
	}

}
