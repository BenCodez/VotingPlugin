package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

public class ConfigBonusReward {

	static ConfigBonusReward instance = new ConfigBonusReward();

	static Main plugin = Main.plugin;

	public static ConfigBonusReward getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigBonusReward() {
	}

	public ConfigBonusReward(Main plugin) {
		ConfigBonusReward.plugin = plugin;
	}

	@SuppressWarnings("deprecation")
	public void addItem(String item, ItemStack itemStack) {
		int id = itemStack.getTypeId();
		int data = itemStack.getData().getData();
		int amount = itemStack.getAmount();

		String name = itemStack.getItemMeta().getDisplayName();
		List<String> lore = itemStack.getItemMeta().getLore();

		HashMap<Enchantment, Integer> enchants = new HashMap<Enchantment, Integer>(
				itemStack.getEnchantments());

		setItemId(item, id);
		setItemData(item, data);
		setItemAmount(item, amount);
		setItemName(item, name);
		setItemLore(item, lore);
		setItemEnchants(item, enchants);
	}

	public int getChanceRewardChance() {
		return getData().getInt("ChanceReward.Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardConsoleCommands() {
		return (ArrayList<String>) getData().getList(
				"ChanceReward.Commands.Console");
	}

	public int getChanceRewardEnchantLevel(String item, String enchant) {
		return getData().getInt(
				"ChanceReward.Items." + item + ".Enchants." + enchant);
	}

	public HashMap<String, Integer> getChanceRewardEnchantments(String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"ChanceReward.Items." + item + ".Enchants").getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant, getEnchantLevel(item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}
	}

	public int getChanceRewardItemAmount(String item) {
		return getData().getInt("ChanceReward.Items." + item + ".Amount");
	}

	public int getChanceRewardItemData(String item) {
		return getData().getInt("ChanceReward.Items." + item + ".Data");
	}

	public int getChanceRewardItemID(String item) {
		return getData().getInt("ChanceReward.Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardItemLore(String item) {
		return (ArrayList<String>) getData().getList(
				"ChanceReward.Items." + item + ".Lore");
	}

	public String getChanceRewardItemName(String item) {
		return getData().getString("ChanceReward.Items." + item + ".Name");
	}

	public Set<String> getChanceRewardItems() {
		return getData().getConfigurationSection("ChanceReward.Items").getKeys(
				false);
	}

	public int getChanceRewardMoneyAmount() {
		return getData().getInt("ChanceReward.Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardPlayerCommands() {
		return (ArrayList<String>) getData().getList(
				"ChanceReward.Commands.Player");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands() {
		return (ArrayList<String>) getData().getList("Commands.Console");
	}

	public FileConfiguration getData() {
		return data;
	}

	public int getEnchantLevel(String item, String enchant) {
		return getData().getInt("Items." + item + ".Enchants." + enchant);
	}

	public HashMap<String, Integer> getEnchantments(String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"Items." + item + ".Enchants").getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant, getEnchantLevel(item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}
	}

	public boolean getGiveBonusReward() {
		return getData().getBoolean("GiveBonusReward");
	}

	public int getItemAmount(String item) {
		return getData().getInt("Items." + item + ".Amount");
	}

	public int getItemData(String item) {
		return getData().getInt("Items." + item + ".Data");
	}

	public int getItemID(String item) {
		return getData().getInt("Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getItemLore(String item) {
		return (ArrayList<String>) getData().getList("Items." + item + ".Lore");
	}

	public String getItemName(String item) {
		return getData().getString("Items." + item + ".Name");
	}

	public Set<String> getItems() {
		return getData().getConfigurationSection("Items").getKeys(false);
	}

	public int getMoneyAmount() {
		return getData().getInt("Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands() {
		return (ArrayList<String>) getData().getList("Commands.Player");
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save BonusReward.yml!");
		}
	}

	public void setConsoleCommands(List<String> consoleCommands) {
		getData().set("Commands.Console", consoleCommands);
	}

	public void setGiveBonusReward(boolean value) {
		getData().set("GiveBonusReward", value);
		saveData();
	}

	public void setItemAmount(String item, int amount) {
		getData().set("Items." + item + ".Amount", amount);
		saveData();
	}

	public void setItemData(String item, int data) {
		getData().set("Items." + item + ".Data", data);
		saveData();
	}

	public void setItemEnchantLevel(String item, String enchant, int level) {
		getData().set("Items." + item + ".Enchants." + enchant, level);
		saveData();
	}

	public void setItemEnchants(String item,
			HashMap<Enchantment, Integer> enchants) {
		for (Enchantment enchant : enchants.keySet()) {
			setItemEnchantLevel(item, enchant.getName(), enchants.get(enchant));
		}
	}

	public void setItemId(String item, int id) {
		getData().set("Items." + item + ".ID", id);
		saveData();
	}

	public void setItemLore(String item, List<String> lore) {
		getData().set("Items." + item + ".Lore", lore);
		saveData();
	}

	public void setItemName(String item, String name) {
		getData().set("Items." + item + ".Name", name);
		saveData();
	}

	public void setMoney(int money) {
		getData().set("Money", money);
		saveData();
	}

	public void setPlayerCommands(List<String> playerCommands) {
		getData().set("Commands.Player", playerCommands);
		saveData();
	}

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

}
