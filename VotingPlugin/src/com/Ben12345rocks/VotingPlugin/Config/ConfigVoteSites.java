package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class ConfigVoteSites {

	static ConfigVoteSites instance = new ConfigVoteSites();

	static Main plugin = Main.plugin;

	public static ConfigVoteSites getInstance() {
		return instance;
	}

	private ConfigVoteSites() {
	}

	public ConfigVoteSites(Main plugin) {
		ConfigVoteSites.plugin = plugin;
	}

	// FileConfiguration data;
	// File dFile;

	@SuppressWarnings("deprecation")
	public void addItem(String siteName, String item, ItemStack itemStack) {
		int id = itemStack.getTypeId();
		int data = itemStack.getData().getData();
		int amount = itemStack.getAmount();

		String name = itemStack.getItemMeta().getDisplayName();
		List<String> lore = itemStack.getItemMeta().getLore();

		HashMap<Enchantment, Integer> enchants = new HashMap<Enchantment, Integer>(
				itemStack.getEnchantments());

		setItemId(siteName, item, id);
		setItemData(siteName, item, data);
		setItemAmount(siteName, item, amount);
		setItemName(siteName, item, name);
		setItemLore(siteName, item, lore);
		setItemEnchants(siteName, item, enchants);
	}

	public void generateVoteSite(String siteName) {
		setDisabled(siteName, true);
		setServiceSite(siteName, "Enter Service Site");
		setVoteURL(siteName, "VoteURL");
		setMoney(siteName, 0);
		setVoteDelay(siteName, 24);

		plugin.loadVoteSites();
		plugin.getLogger()
				.info("Created file VoteSites/"
						+ siteName
						+ ".yml! Loaded default values into file, remember to turn Disabled to false, else it won't be read by the plugin");
	}

	public int getChanceRewardChance(String siteName) {
		return getData(siteName).getInt("ChanceReward.Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"ChanceReward.Commands.Console");
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
	 * @return Item data value
	 */
	public int getChanceRewardItemData(String siteName, String item) {
		return getData(siteName).getInt("ChanceReward.Items." + item + ".Data");
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
	 * @return Name of item
	 */
	public String getChanceRewardItemName(String siteName, String item) {
		return getData(siteName).getString(
				"ChanceReward.Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getChanceRewardItems(String siteName) {
		return getData(siteName).getConfigurationSection("ChanceReward.Items")
				.getKeys(false);
	}

	public int getChanceRewardMoneyAmount(String siteName) {
		return getData(siteName).getInt("ChanceReward.Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChanceRewardPlayerCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"ChanceReward.Commands.Player");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName)
				.getList("Commands.Console");
	}

	public FileConfiguration getData(String siteName) {
		File dFile = getVoteSiteFile(siteName);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
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
	 * @return Amount of items
	 */
	public int getItemAmount(String siteName, String item) {
		return getData(siteName).getInt("Items." + item + ".Amount");
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
	 * @return Id of item
	 */
	public int getItemID(String siteName, String item) {
		return getData(siteName).getInt("Items." + item + ".ID");
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
	 * @return Name of item
	 */
	public String getItemName(String siteName, String item) {
		return getData(siteName).getString("Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getItems(String siteName) {
		return getData(siteName).getConfigurationSection("Items")
				.getKeys(false);
	}

	public int getMoneyAmount(String siteName) {
		return getData(siteName).getInt("Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList("Commands.Player");
	}

	public String getServiceSite(String siteName) {
		return getData(siteName).getString("ServiceSite");
	}

	public int getVoteDelay(String siteName) {
		return getData(siteName).getInt("VoteDelay");
	}

	public boolean getVoteSiteDisabled(String siteName) {
		return getData(siteName).getBoolean("Disabled");
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

	public ArrayList<VoteSite> getVoteSites() {
		if (plugin.voteSites != null) {
			return plugin.voteSites;
		} else {
			plugin.loadVoteSites();
			return plugin.voteSites;
		}
	}

	public ArrayList<String> getVoteSitesFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	public ArrayList<VoteSite> getVoteSitesLoad() {
		ArrayList<VoteSite> voteSites = new ArrayList<VoteSite>();
		ArrayList<String> voteSiteNames = getVoteSitesNames();
		if (voteSiteNames != null) {
			for (String site : voteSiteNames) {
				if (!site.equalsIgnoreCase("Example")
						&& !getVoteSiteDisabled(site)) {
					voteSites.add(new VoteSite(site));
				}
			}
		}
		return voteSites;
	}

	public ArrayList<String> getVoteSitesNames() {
		ArrayList<String> siteNames = getVoteSitesFiles();
		if (siteNames == null) {
			return null;
		}
		for (int i = 0; i < siteNames.size(); i++) {
			siteNames.set(i, siteNames.get(i).replace(".yml", ""));
		}
		for (int i = siteNames.size() - 1; i >= 0; i--) {
			// plugin.getLogger().info(siteNames.get(i));
			if (getVoteSiteDisabled(siteNames.get(i))) {
				// plugin.getLogger().info("Removed: " + siteNames.get(i));
				siteNames.remove(i);

			}

		}
		return siteNames;
	}

	public String getVoteURL(String siteName) {
		return getData(siteName).getString("VoteURL");
	}

	public boolean renameVoteSite(String siteName, String newName) {
		return getVoteSiteFile(siteName).renameTo(
				new File(plugin.getDataFolder() + File.separator + "VoteSites",
						newName + ".yml"));
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

	public void setConsoleCommands(String siteName, String item,
			List<String> consoleCommands) {
		set(siteName, "Commands.Console", consoleCommands);
	}

	public void setDisabled(String siteName, boolean disabled) {
		set(siteName, "Disabled", disabled);
	}

	public void setItemAmount(String siteName, String item, int amount) {
		set(siteName, "Items." + item + ".Amount", amount);
	}

	public void setItemData(String siteName, String item, int data) {
		set(siteName, "Items." + item + ".Data", data);
	}

	public void setItemEnchantLevel(String siteName, String item,
			String enchant, int level) {
		set(siteName, "Items." + item + ".Enchants." + enchant, level);
	}

	public void setItemEnchants(String siteName, String item,
			HashMap<Enchantment, Integer> enchants) {
		for (Enchantment enchant : enchants.keySet()) {
			setItemEnchantLevel(siteName, item, enchant.getName(),
					enchants.get(enchant));
		}
	}

	public void setItemId(String siteName, String item, int id) {
		set(siteName, "Items." + item + ".ID", id);
	}

	public void setItemLore(String siteName, String item, List<String> lore) {
		set(siteName, "Items." + item + ".Lore", lore);
	}

	public void setItemName(String siteName, String item, String name) {
		set(siteName, "Items." + item + ".Name", name);
	}

	public void setMoney(String siteName, int money) {
		set(siteName, "Money", money);
	}

	public void setPlayerCommands(String siteName, String item,
			List<String> playerCommands) {
		set(siteName, "Commands.Player", playerCommands);
	}

	public void setVoteDelay(String siteName, int voteDelay) {
		set(siteName, "VoteDelay", voteDelay);
	}

	public void setServiceSite(String siteName, String serviceSite) {
		set(siteName, "ServiceSite", serviceSite);
	}

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

	public void setVoteURL(String siteName, String url) {
		set(siteName, "VoteURL", url);
	}
}
