package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigGUI.
 */
public class ConfigGUI {

	/** The instance. */
	static ConfigGUI instance = new ConfigGUI();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigGUI.
	 *
	 * @return single instance of ConfigGUI
	 */
	public static ConfigGUI getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config GUI.
	 */
	private ConfigGUI() {
	}

	/**
	 * Instantiates a new config GUI.
	 *
	 * @param plugin the plugin
	 */
	public ConfigGUI(Main plugin) {
		ConfigGUI.plugin = plugin;
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
	 * Gets the vote GUI slot amount.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot amount
	 */
	public int getVoteGUISlotAmount(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Amount");
	}

	/**
	 * Gets the vote GUI slot command.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot command
	 */
	public String getVoteGUISlotCommand(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Command");
	}

	/**
	 * Gets the vote GUI slot data.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot data
	 */
	public int getVoteGUISlotData(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Data");
	}

	/**
	 * Gets the vote GUI slot durability.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot durability
	 */
	public int getVoteGUISlotDurability(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Durability");
	}

	/**
	 * Gets the vote GUI slot material.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot material
	 */
	public String getVoteGUISlotMaterial(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Item.Material");
	}

	/**
	 * Gets the vote GUI slot name.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot name
	 */
	public String getVoteGUISlotName(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Item.Name");
	}

	/**
	 * Gets the vote GUI slots.
	 *
	 * @return the vote GUI slots
	 */
	public Set<String> getVoteGUISlots() {
		try {
			return getData().getConfigurationSection("GUI.VoteGUI").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the vote GUI slot skull.
	 *
	 * @param player the player
	 * @param slot the slot
	 * @return the vote GUI slot skull
	 */
	public String getVoteGUISlotSkull(Player player, String slot) {
		String str = getData().getString("GUI.VoteGUI." + slot + ".Item.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote GUI slot slot.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot slot
	 */
	public int getVoteGUISlotSlot(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Slot");
	}

	/**
	 * Gets the vote GUI slot lore.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot lore
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVoteGUISlotLore(String slot) {

		ArrayList<String> list = (ArrayList<String>) getData().getList(
				"GUI.VoteGUI." + slot + ".Lore");
		if (list != null) {
			return list;
		}
		return new ArrayList<String>();
	}

	/**
	 * Gets the vote site item amount.
	 *
	 * @param site the site
	 * @return the vote site item amount
	 */
	public int getVoteSiteItemAmount(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Amount");
	}

	/**
	 * Gets the vote site item data.
	 *
	 * @param site the site
	 * @return the vote site item data
	 */
	public int getVoteSiteItemData(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Data");
	}

	/**
	 * Gets the vote site item durability.
	 *
	 * @param site the site
	 * @return the vote site item durability
	 */
	public int getVoteSiteItemDurability(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Item.Durability");
	}

	/**
	 * Gets the vote site item material.
	 *
	 * @param site the site
	 * @return the vote site item material
	 */
	public String getVoteSiteItemMaterial(String site) {
		String siteName = site.replace(".", "-");
		return getData().getString(
				"GUI.VoteReward." + siteName + ".Item.Material");
	}

	/**
	 * Gets the vote site item lore.
	 *
	 * @param site the site
	 * @return the vote site item lore
	 */
	@SuppressWarnings("unchecked")
	public List<String> getVoteSiteItemLore(String site) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList(
				"GUI.VoteReward." + siteName + ".Item.Lore");
	}

	/**
	 * Gets the vote site item name.
	 *
	 * @param site the site
	 * @return the vote site item name
	 */
	public String getVoteSiteItemName(String site) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Item.Name");
	}

	/**
	 * Gets the vote site items.
	 *
	 * @param site the site
	 * @return the vote site items
	 */
	public Set<String> getVoteSiteItems(String site) {
		String siteName = site.replace(".", "-");
		try {
			return getData().getConfigurationSection(
					"GUI.VoteReward." + siteName + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the vote site items amount.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items amount
	 */
	public int getVoteSiteItemsAmount(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Amount");
	}

	/**
	 * Gets the vote site items data.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items data
	 */
	public int getVoteSiteItemsData(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Data");
	}

	/**
	 * Gets the vote site items durability.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items durability
	 */
	public int getVoteSiteItemsDurability(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData()
				.getInt("GUI.VoteReward." + siteName + ".Items." + item
						+ ".Durability");
	}

	/**
	 * Gets the vote site items material.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items material
	 */
	public String getVoteSiteItemsMaterial(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getString(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Material");
		
	}

	/**
	 * Gets the vote site item skull.
	 *
	 * @param player the player
	 * @param siteName the site name
	 * @return the vote site item skull
	 */
	public String getVoteSiteItemSkull(Player player, String siteName) {
		String str = getData().getString(
				"GUI.VoteReward." + siteName + ".Item.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote site items lore.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items lore
	 */
	@SuppressWarnings("unchecked")
	public List<String> getVoteSiteItemsLore(String site, String item) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Lore");
	}

	/**
	 * Gets the vote site items name.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items name
	 */
	public String getVoteSiteItemsName(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getString(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Name");
	}

	/**
	 * Gets the vote site items skull.
	 *
	 * @param player the player
	 * @param siteName the site name
	 * @param item the item
	 * @return the vote site items skull
	 */
	public String getVoteSiteItemsSkull(Player player, String siteName,
			String item) {
		String str = getData().getString(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote site items slot.
	 *
	 * @param site the site
	 * @param item the item
	 * @return the vote site items slot
	 */
	public int getVoteSiteItemsSlot(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Slot");
	}

	/**
	 * Gets the vote URL already voted item amount.
	 *
	 * @return the vote URL already voted item amount
	 */
	public int getVoteURLAlreadyVotedItemAmount() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Amount");
		if (num != 0) {
			return num;
		} else {
			return 1;
		}
	}

	/**
	 * Gets the vote URL already voted item data.
	 *
	 * @return the vote URL already voted item data
	 */
	public int getVoteURLAlreadyVotedItemData() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Data");

		return num;

	}

	/**
	 * Gets the vote URL already voted item durability.
	 *
	 * @return the vote URL already voted item durability
	 */
	public int getVoteURLAlreadyVotedItemDurability() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Durability");

		return num;

	}

	/**
	 * Gets the vote URL already voted item material.
	 *
	 * @return the vote URL already voted item material
	 */
	public String getVoteURLAlreadyVotedItemMaterial() {
		String str = getData().getString(
				"GUI.VoteURL.AlreadyVotedItem.Material");
		if (str != null) {
			return str;
		}
		return "REDSTONE_BLOCK";

	}

	/**
	 * Gets the vote URL already voted item skull.
	 *
	 * @param player the player
	 * @return the vote URL already voted item skull
	 */
	public String getVoteURLAlreadyVotedItemSkull(Player player) {
		String str = getData().getString("GUI.VoteURL.AlreadyVotedItem.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote URL can vote item amount.
	 *
	 * @return the vote URL can vote item amount
	 */
	public int getVoteURLCanVoteItemAmount() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Amount");
		if (num != 0) {
			return num;
		} else {
			return 1;
		}
	}

	/**
	 * Gets the vote URL can vote item data.
	 *
	 * @return the vote URL can vote item data
	 */
	public int getVoteURLCanVoteItemData() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Data");

		return num;

	}

	/**
	 * Gets the vote URL can vote item durability.
	 *
	 * @return the vote URL can vote item durability
	 */
	public int getVoteURLCanVoteItemDurability() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Durability");
		return num;

	}

	/**
	 * Gets the vote URL can vote item material.
	 *
	 * @return the vote URL can vote item material
	 */
	public String getVoteURLCanVoteItemMaterial() {
		String str = getData().getString("GUI.VoteURL.CanVoteItem.Material");
		if (str != null) {
			return str;
		}
		return "EMERALD_BLOCK";
	}

	/**
	 * Gets the vote URL can vote item skull.
	 *
	 * @param player the player
	 * @return the vote URL can vote item skull
	 */
	public String getVoteURLCanVoteItemSkull(Player player) {
		String str = getData().getString("GUI.VoteURL.CanVoteItem.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;
	}

	/**
	 * Gets the vote URL next vote.
	 *
	 * @return the vote URL next vote
	 */
	public String getVoteURLNextVote() {
		String str = getData().getString("GUI.VoteURL.NextVote");
		if (str != null) {
			return str;
		} else {
			return "&cCan Vote In: %Info%";
		}
	}

	/**
	 * Gets the vote URL see URL.
	 *
	 * @return the vote URL see URL
	 */
	public String getVoteURLSeeURL() {
		String str = getData().getString("GUI.VoteURL.SeeURL");
		if (str != null) {
			return str;
		} else {
			return "&cClick to see URL";
		}
	}

	/**
	 * Gets the vote URL site name.
	 *
	 * @return the vote URL site name
	 */
	public String getVoteURLSiteName() {
		String str = getData().getString("GUI.VoteURL.SiteName");
		if (str != null) {
			return str;
		} else {
			return "&c%Name%";
		}
	}

	/**
	 * Gets the vote URL view all urls button enabled.
	 *
	 * @return the vote URL view all urls button enabled
	 */
	public boolean getVoteURLViewAllUrlsButtonEnabled() {
		return getData().getBoolean("GUI.VoteURL.ViewAllUrlsButtonEnabled");
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
	 * Sets the up.
	 *
	 * @param p the new up
	 */
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
