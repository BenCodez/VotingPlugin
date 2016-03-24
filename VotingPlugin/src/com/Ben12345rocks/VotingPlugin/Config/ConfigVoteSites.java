package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
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

	// FileConfiguration data;
	// File dFile;

	public void setup(String siteName) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites", siteName + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				if (siteName.equalsIgnoreCase("Example")) {
					plugin.saveResource("VoteSites" + File.separator
							+ "Example.yml", true);
				}
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create VoteSites/"
								+ siteName + ".yml!");
			}
		}

		// data = YamlConfiguration.loadConfiguration(dFile);
	}

	public File getVoteSiteFile(String siteName) {
		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites", siteName + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create VoteSites/"
								+ siteName + ".yml!");

			}
		}
		return dFile;

	}

	public FileConfiguration getData(String siteName) {
		File dFile = getVoteSiteFile(siteName);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	public void set(String siteName, String path, Object value) {
		// String playerName = user.getPlayerName();
		File dFile = getVoteSiteFile(siteName);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		try {
			data.save(dFile);
		} catch (IOException e) {
			plugin.getLogger().severe(
					ChatColor.RED + "Could not save VoteSites/" + siteName
							+ ".yml!");
		}
	}

	public ArrayList<String> getVoteSitesFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator
				+ "VoteSite");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	public ArrayList<String> getVoteSitesNames() {
		ArrayList<String> siteNames = getVoteSitesFiles();
		if (siteNames == null) {
			return null;
		}
		for (int i = 0; i < siteNames.size(); i++) {
			siteNames.get(i).replace(".yml", "");
		}
		return siteNames;
	}

	public boolean getVoteSiteDisabled(String siteName) {
		return getData(siteName).getBoolean("Disabled");
	}

	public ArrayList<VoteSite> getVoteSitesLoad() {
		ArrayList<VoteSite> voteSites = new ArrayList<VoteSite>();
		ArrayList<String> voteSiteNames = getVoteSitesNames();
		if (voteSiteNames != null) {
			for (String site : voteSiteNames) {
				if (!site.equalsIgnoreCase("Example")
						|| !getVoteSiteDisabled(site)) {
					voteSites.add(new VoteSite(site));
				}
			}
		}
		return voteSites;
	}

	public ArrayList<VoteSite> getVoteSites() {
		return plugin.voteSites;
	}

	public String getServiceSite(String siteName) {
		return getData(siteName).getString("ServiceSite");
	}

	public String getVoteURL(String siteName) {
		return getData(siteName).getString("VoteURL");
	}

	public int getVoteDelay(String siteName) {
		return getData(siteName).getInt("VoteDelay");
	}

	public int getMoneyAmount(String siteName) {
		return getData(siteName).getInt("Money");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getItems(String siteName) {
		return getData(siteName).getConfigurationSection("Items")
				.getKeys(false);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getItemID(String siteName, String item) {
		return getData(siteName).getInt("Items." + item + ".ID");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getItemData(String siteName, String item) {
		return getData(siteName).getInt("Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Amount of items
	 */
	public int getItemAmount(String siteName, String item) {
		return getData(siteName).getInt("Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getItemName(String siteName, String item) {
		return getData(siteName).getString("Items." + item + ".Name");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getItemLore(String siteName, String item) {
		return (ArrayList<String>) getData(siteName).getList(
				"Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getEnchantments(String siteName, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData(siteName).getConfigurationSection(
					"Items." + item + ".Enchants").getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant,
						getEnchantLevel(siteName, item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}

	}

	/**
	 *
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getEnchantLevel(String siteName, String item, String enchant) {
		return getData(siteName).getInt(
				"Items." + item + ".Enchants." + enchant);
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName)
				.getList("Commands.Console");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList("Commands.Player");
	}

	public int getChanceRewardChance(String siteName) {
		return getData(siteName).getInt("ChanceReward.Chance");
	}

	public int getChanceRewardMoneyAmount(String siteName) {
		return getData(siteName).getInt("ChanceReward.Money");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getChanceRewardItems(String siteName) {
		return getData(siteName).getConfigurationSection("ChanceReward.Items")
				.getKeys(false);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getChanceRewardItemID(String siteName, String item) {
		return getData(siteName).getInt("ChanceReward.Items." + item + ".ID");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getChanceRewardItemData(String siteName, String item) {
		return getData(siteName).getInt("ChanceReward.Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Amount of items
	 */
	public int getChanceRewardItemAmount(String siteName, String item) {
		return getData(siteName).getInt(
				"ChanceReward.Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getChanceRewardItemName(String siteName, String item) {
		return getData(siteName).getString(
				"ChanceReward.Items." + item + ".Name");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getChanceRewardItemLore(String siteName,
			String item) {
		return (ArrayList<String>) getData(siteName).getList(
				"ChanceReward.Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getChanceRewardEnchantments(
			String siteName, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData(siteName).getConfigurationSection(
					"ChanceReward.Items." + item + ".Enchants").getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant,
						getEnchantLevel(siteName, item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}

	}

	/**
	 *
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getChanceRewardEnchantLevel(String siteName, String item,
			String enchant) {
		return getData(siteName).getInt(
				"ChanceReward.Items." + item + ".Enchants." + enchant);
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"ChanceReward.Commands.Console");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardPlayerCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"ChanceReward.Commands.Player");
	}

}
