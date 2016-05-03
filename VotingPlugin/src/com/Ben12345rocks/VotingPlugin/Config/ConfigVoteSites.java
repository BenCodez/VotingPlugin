package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
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

	@SuppressWarnings("deprecation")
	public void addExtraRewardItem(String siteName, String reward, String item,
			ItemStack itemStack) {
		int id = itemStack.getTypeId();
		int data = itemStack.getData().getData();
		int amount = itemStack.getAmount();

		String name = itemStack.getItemMeta().getDisplayName();
		List<String> lore = itemStack.getItemMeta().getLore();

		HashMap<Enchantment, Integer> enchants = new HashMap<Enchantment, Integer>(
				itemStack.getEnchantments());

		setExtraRewardItemId(siteName, reward, item, id);
		setExtraRewardItemData(siteName, reward, item, data);
		setExtraRewardItemAmount(siteName, reward, item, amount);
		setExtraRewardItemName(siteName, reward, item, name);
		setExtraRewardItemLore(siteName, reward, item, lore);
		setExtraRewardItemEnchants(siteName, reward, item, enchants);
	}

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

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName)
				.getList("Commands.Console");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCumulativeRewardConsoleCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"CumulativeReward.Commands.Console");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getCumulativeRewardEnchantLevel(String siteName, String item,
			String enchant) {
		return getData(siteName).getInt(
				"CumulativeReward.Items." + item + ".Enchants." + enchant);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getCumulativeRewardEnchantments(
			String siteName, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData(siteName).getConfigurationSection(
					"CumulativeReward.Items." + item + ".Enchants").getKeys(
					false);
			for (String enchant : enchants) {
				enchantments
						.put(enchant,
								getCumulativeRewardEnchantLevel(siteName, item,
										enchant));
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
	public int getCumulativeRewardItemAmount(String siteName, String item) {
		return getData(siteName).getInt(
				"CumulativeReward.Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getCumulativeRewardItemData(String siteName, String item) {
		return getData(siteName).getInt(
				"CumulativeReward.Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getCumulativeRewardItemID(String siteName, String item) {
		return getData(siteName).getInt(
				"CumulativeReward.Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getCumulativeRewardItemLore(String siteName,
			String item) {
		return (ArrayList<String>) getData(siteName).getList(
				"CumulativeReward.Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getCumulativeRewardItemName(String siteName, String item) {
		return getData(siteName).getString(
				"CumulativeReward.Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getCumulativeRewardItems(String siteName) {
		try {
			return getData(siteName).getConfigurationSection(
					"CumulativeReward.Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getCumulativeRewardMoneyAmount(String siteName) {
		return getData(siteName).getInt("CumulativeReward.Money");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCumulativeRewardPlayerCommands(String siteName) {
		return (ArrayList<String>) getData(siteName).getList(
				"CumulativeReward.Commands.Player");
	}

	public int getCumulativeRewardVotesAmount(String siteName) {
		return getData(siteName).getInt("CumulativeReward.Votes");
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

	public int getExtraRewardChance(String siteName, String reward) {
		return getData(siteName).getInt("ExtraReward." + reward + ".Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getExtraRewardConsoleCommands(String siteName,
			String reward) {
		return (ArrayList<String>) getData(siteName).getList(
				"ExtraReward." + reward + ".Commands.Console");
	}

	public String getExtraRewardWorld(String siteName, String reward) {
		return getData(siteName).getString("ExtraReward." + reward + ".World");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getExtraRewardEnchantLevel(String siteName, String reward,
			String item, String enchant) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".Enchants."
						+ enchant);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getExtraRewardEnchantments(String siteName,
			String reward, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData(siteName).getConfigurationSection(
					"ExtraReward." + reward + ".Items." + item + ".Enchants")
					.getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(
						enchant,
						getExtraRewardEnchantLevel(siteName, reward, item,
								enchant));
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
	public int getExtraRewardItemAmount(String siteName, String reward,
			String item) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getExtraRewardItemData(String siteName, String reward,
			String item) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getExtraRewardItemID(String siteName, String reward, String item) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getExtraRewardItemLore(String siteName,
			String reward, String item) {
		return (ArrayList<String>) getData(siteName).getList(
				"ExtraReward." + reward + ".Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getExtraRewardItemName(String siteName, String reward,
			String item) {
		return getData(siteName).getString(
				"ExtraReward." + reward + ".Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getExtraRewardItems(String siteName, String reward) {
		try {
			return getData(siteName).getConfigurationSection(
					"ExtraReward." + reward + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getExtraRewardMaxItemAmount(String siteName, String reward,
			String item) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".MaxAmount");
	}

	public int getExtraRewardMaxMoney(String siteName, String reward) {
		return getData(siteName).getInt("ExtraReward." + reward + ".MaxMoney");
	}

	public int getExtraRewardMinItemAmount(String siteName, String reward,
			String item) {
		return getData(siteName).getInt(
				"ExtraReward." + reward + ".Items." + item + ".MinAmount");
	}

	public int getExtraRewardMinMoney(String siteName, String reward) {
		return getData(siteName).getInt("ExtraReward." + reward + ".MinMoney");
	}

	public int getExtraRewardMoneyAmount(String siteName, String reward) {
		return getData(siteName).getInt("ExtraReward." + reward + ".Money");
	}

	public String getExtraRewardPermission(String siteName, String reward) {
		String perm = getData(siteName).getString(
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
	public ArrayList<String> getExtraRewardPlayerCommands(String siteName,
			String reward) {
		return (ArrayList<String>) getData(siteName).getList(
				"ExtraReward." + reward + ".Commands.Player");
	}

	public Set<String> getExtraRewardRewards(String siteName) {
		try {
			return getData(siteName).getConfigurationSection("ExtraReward")
					.getKeys(false);
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
			return new HashSet<String>();
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
		try {
			return getData(siteName).getConfigurationSection("Items").getKeys(
					false);
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
			return new HashSet<String>();
		}
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

	public void setConsoleCommands(String siteName, List<String> consoleCommands) {
		set(siteName, "Commands.Console", consoleCommands);
	}

	public void setDisabled(String siteName, boolean disabled) {
		set(siteName, "Disabled", disabled);
	}

	public void setExtraRewardChance(String siteName, String reward, int chance) {
		set(siteName, "ExtraReward." + reward + ".Chance", chance);
	}

	public void setExtraRewardConsoleCommands(String siteName, String reward,
			List<String> consoleCommands) {
		set(siteName, "ExtraReward." + reward + ".Commands.Console",
				consoleCommands);
	}

	public void setExtraRewardItemAmount(String siteName, String reward,
			String item, int amount) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".Amount",
				amount);
	}

	public void setExtraRewardItemData(String siteName, String reward,
			String item, int data) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".Data",
				data);
	}

	public void setExtraRewardItemEnchantLevel(String siteName, String reward,
			String item, String enchant, int level) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".Enchants."
				+ enchant, level);
	}

	public void setExtraRewardItemEnchants(String siteName, String reward,
			String item, HashMap<Enchantment, Integer> enchants) {
		for (Enchantment enchant : enchants.keySet()) {
			setExtraRewardItemEnchantLevel(siteName, reward, item,
					enchant.getName(), enchants.get(enchant));
		}
	}

	public void setExtraRewardItemId(String siteName, String reward,
			String item, int id) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".ID", id);
	}

	public void setExtraRewardItemLore(String siteName, String reward,
			String item, List<String> lore) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".Lore",
				lore);
	}

	public void setExtraRewardItemName(String siteName, String reward,
			String item, String name) {
		set(siteName, "ExtraReward." + reward + ".Items." + item + ".Name",
				name);
	}

	public void setExtraRewardMoney(String siteName, String reward, int money) {
		set(siteName, "ExtraReward." + reward + ".Money", money);
	}

	public void setExtraRewardPlayerCommands(String siteName, String reward,
			List<String> playerCommands) {
		set(siteName, "ExtraReward." + reward + ".Commands.Player",
				playerCommands);
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

	public void setPlayerCommands(String siteName, List<String> playerCommands) {
		set(siteName, "Commands.Player", playerCommands);
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

	public void setVoteDelay(String siteName, int voteDelay) {
		set(siteName, "VoteDelay", voteDelay);
	}

	public void setVoteURL(String siteName, String url) {
		set(siteName, "VoteURL", url);
	}

}
