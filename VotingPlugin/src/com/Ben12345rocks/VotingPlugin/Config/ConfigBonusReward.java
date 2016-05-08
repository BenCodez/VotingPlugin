package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
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
import com.Ben12345rocks.VotingPlugin.Files.Files;

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
	public void addExtraRewardItem(String reward, String item,
			ItemStack itemStack) {
		int id = itemStack.getTypeId();
		int data = itemStack.getData().getData();
		int amount = itemStack.getAmount();

		String name = itemStack.getItemMeta().getDisplayName();
		List<String> lore = itemStack.getItemMeta().getLore();

		HashMap<Enchantment, Integer> enchants = new HashMap<Enchantment, Integer>(
				itemStack.getEnchantments());

		setExtraRewardItemId(reward, item, id);
		setExtraRewardItemData(reward, item, data);
		setExtraRewardItemAmount(reward, item, amount);
		setExtraRewardItemName(reward, item, name);
		setExtraRewardItemLore(reward, item, lore);
		setExtraRewardItemEnchants(reward, item, enchants);
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

	public int getExtraRewardChance(String reward) {
		return getData().getInt("ExtraReward." + reward + ".Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getExtraRewardConsoleCommands(String reward) {
		return (ArrayList<String>) getData().getList(
				"ExtraReward.Commands." + reward + ".Console");
	}

	public int getExtraRewardEnchantLevel(String reward, String item,
			String enchant) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".Enchants."
						+ enchant);
	}

	public HashMap<String, Integer> getExtraRewardEnchantments(String reward,
			String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"ExtraReward." + reward + ".Items." + item + ".Enchants")
					.getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant,
						getExtraRewardEnchantLevel(reward, item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}
	}

	public int getExtraRewardItemAmount(String reward, String item) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".Amount");
	}

	public int getExtraRewardItemData(String reward, String item) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".Data");
	}

	public int getExtraRewardItemID(String reward, String item) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getExtraRewardItemLore(String reward, String item) {
		return (ArrayList<String>) getData().getList(
				"ExtraReward." + reward + ".Items." + item + ".Lore");
	}

	public String getExtraRewardItemName(String reward, String item) {
		return getData().getString(
				"ExtraReward." + reward + ".Items." + item + ".Name");
	}

	public Set<String> getExtraRewardItems(String reward) {
		try {
			return getData().getConfigurationSection(
					"ExtraReward." + reward + ".Items").getKeys(false);
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
			return new HashSet<String>();
		}
	}

	public int getExtraRewardMaxItemAmount(String reward, String item) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".MaxAmount");
	}

	public int getExtraRewardMaxMoneyAmount(String reward) {
		return getData().getInt("ExtraReward." + reward + ".MaxMoney");
	}

	public int getExtraRewardMinItemAmount(String reward, String item) {
		return getData().getInt(
				"ExtraReward." + reward + ".Items." + item + ".MinAmount");
	}

	public int getExtraRewardMinMoneyAmount(String reward) {
		return getData().getInt("ExtraReward." + reward + ".MinMoney");
	}

	public int getExtraRewardMoneyAmount(String reward) {
		return getData().getInt("ExtraReward." + reward + ".Money");
	}

	public String getExtraRewardPermission(String reward) {
		String perm = getData().getString(
				"ExtraReward." + reward + ".Permission");
		if (perm == null) {
			return null;
		}

		if (perm.equalsIgnoreCase("none")) {
			return null;
		}

		return "ExtraReward." + perm;
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getExtraRewardPlayerCommands(String reward) {
		return (ArrayList<String>) getData().getList(
				"ExtraReward." + reward + ".Commands.Player");
	}

	public Set<String> getExtraRewardRewards() {
		try {
			return getData().getConfigurationSection("ExtraReward").getKeys(
					false);
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
			return new HashSet<String>();
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
		try {
			return getData().getConfigurationSection("Items").getKeys(false);
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
			return new HashSet<String>();
		}
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
		Files.getInstance().editFile(dFile, data);
		/*
		 * try { data.save(dFile); } catch (IOException e) {
		 * Bukkit.getServer().getLogger() .severe(ChatColor.RED +
		 * "Could not save BonusReward.yml!"); }
		 */
	}

	public void setConsoleCommands(List<String> consoleCommands) {
		getData().set("Commands.Console", consoleCommands);
	}

	public void setExtraRewardChance(String reward, int chance) {
		getData().set("ExtraReward." + reward + ".Chance", chance);
	}

	public void setExtraRewardConsoleCommands(String reward,
			List<String> consoleCommands) {
		getData().set("ExtraReward." + reward + ".Commands.Console",
				consoleCommands);
	}

	public void setExtraRewardItemAmount(String reward, String item, int amount) {
		getData().set("ExtraReward." + reward + ".Items." + item + ".Amount",
				amount);
	}

	public void setExtraRewardItemData(String reward, String item, int data) {
		getData().set("ExtraReward." + reward + ".Items." + item + ".Data",
				data);
	}

	public void setExtraRewardItemEnchantLevel(String reward, String item,
			String enchant, int level) {
		getData().set(
				"ExtraReward." + reward + ".Items." + item + ".Enchants."
						+ enchant, level);
	}

	public void setExtraRewardItemEnchants(String reward, String item,
			HashMap<Enchantment, Integer> enchants) {
		for (Enchantment enchant : enchants.keySet()) {
			setExtraRewardItemEnchantLevel(reward, item, enchant.getName(),
					enchants.get(enchant));
		}
	}

	public void setExtraRewardItemId(String reward, String item, int id) {
		getData().set("ExtraReward." + reward + ".Items." + item + ".ID", id);
	}

	public void setExtraRewardItemLore(String reward, String item,
			List<String> lore) {
		getData().set("ExtraReward." + reward + ".Items." + item + ".Lore",
				lore);
	}

	public void setExtraRewardItemName(String reward, String item, String name) {
		getData().set("ExtraReward." + reward + ".Items." + item + ".Name",
				name);
	}

	public void setExtraRewardMoney(String reward, int money) {
		getData().set("ExtraReward." + reward + ".Money", money);
	}

	public void setExtraRewardPlayerCommands(String reward,
			List<String> playerCommands) {
		getData().set("ExtraReward." + reward + ".Commands.Player",
				playerCommands);
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
