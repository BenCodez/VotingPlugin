package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigGUI.
 */
public class ConfigGUI extends YMLFile {

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

	/**
	 * Instantiates a new config GUI.
	 */
	public ConfigGUI() {
		super(new File(Main.plugin.getDataFolder(), "GUI.yml"));
	}

	public Set<String> getIdentifiers() {
		return getData().getConfigurationSection("Shop").getKeys(false);
	}

	public ConfigurationSection getIdentifierSection(String identifier) {
		return getData().getConfigurationSection("Shop." + identifier);
	}

	@Deprecated
	public String getIdentifierItemMaterial(String identifier) {
		return getData().getString("Shop." + identifier + ".Item.Material");
	}

	@Deprecated
	public String getIdentifierItemSkull(String identifier) {
		return getData().getString("Shop." + identifier + ".Item.Skull");
	}

	@Deprecated
	public int getIdentifierItemData(String identifier) {
		return getData().getInt("Shop." + identifier + ".Item.Data");
	}

	public int getIdentifierItemAmount(String identifier) {
		return getData().getInt("Shop." + identifier + ".Item.Amount");
	}

	@Deprecated
	public String getIdentifierItemName(String identifier) {
		return getData().getString("Shop." + identifier + ".Item.Name");
	}

	@SuppressWarnings("unchecked")
	@Deprecated
	public ArrayList<String> getIdentifierItemLore(String identifier) {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList("Shop." + identifier + ".Item.Lore");
			if (list != null) {
				return list;
			}
			return new ArrayList<String>();
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	public int getIdentifierCost(String identifier) {
		return getData().getInt("Shop." + identifier + ".Cost");
	}

	public int getIdentifierSlot(String identifier) {
		return getData().getInt("Shop." + identifier + ".Slot");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getIdentifierRewards(String identifier) {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList("Shop." + identifier + ".Rewards");
			if (list != null) {
				return list;
			}
			return new ArrayList<String>();
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	@Deprecated
	public int getIdentifierItemDurability(String identifier) {
		return getData().getInt("Shop." + identifier + ".Item.Durability");
	}

	public String getIdentifierFromSlot(int slot) {
		for (String identifier : getIdentifiers()) {
			if (getIdentifierSlot(identifier) == slot) {
				return identifier;
			}
		}
		return null;
	}

	/**
	 * Gets the vote GUI slot amount.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot amount
	 */
	@Deprecated
	public int getVoteGUISlotAmount(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Amount");
	}

	/**
	 * Gets the vote GUI slot command.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot command
	 */

	public String getVoteGUISlotCommand(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Command", "");
	}

	/**
	 * Gets the vote GUI slot data.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot data
	 */
	@Deprecated
	public int getVoteGUISlotData(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Data");
	}

	public ConfigurationSection getVoteGUISlotSection(String slot) {
		return getData().getConfigurationSection("GUI.VoteGUI." + slot + ".Item");
	}

	/**
	 * Gets the vote GUI slot durability.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot durability
	 */
	@Deprecated
	public int getVoteGUISlotDurability(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Durability");
	}

	/**
	 * Gets the vote GUI slot lore.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot lore
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVoteGUISlotLore(String slot) {

		ArrayList<String> list = (ArrayList<String>) getData().getList("GUI.VoteGUI." + slot + ".Lore");
		if (list != null) {
			return list;
		}
		return new ArrayList<String>();
	}

	/**
	 * Gets the vote GUI slot material.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot material
	 */
	@Deprecated
	public String getVoteGUISlotMaterial(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Item.Material");
	}

	/**
	 * Gets the vote GUI slot name.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot name
	 */
	@Deprecated
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
			return getData().getConfigurationSection("GUI.VoteGUI").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the vote GUI slot skull.
	 *
	 * @param player
	 *            the player
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot skull
	 */
	@Deprecated
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
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot slot
	 */
	public int getVoteGUISlotSlot(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Slot");
	}

	/**
	 * Gets the vote site item amount.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item amount
	 */
	@Deprecated
	public int getVoteSiteItemAmount(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Amount");
	}

	public ConfigurationSection getVoteSiteItemSection(String site) {
		String siteName = site.replace(".", "-");
		return getData().getConfigurationSection("GUI.VoteReward." + siteName);
	}

	/**
	 * Gets the vote site item data.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item data
	 */
	@Deprecated
	public int getVoteSiteItemData(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Data");
	}

	/**
	 * Gets the vote site item durability.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item durability
	 */
	@Deprecated
	public int getVoteSiteItemDurability(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Durability");
	}

	/**
	 * Gets the vote site item lore.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item lore
	 */
	@SuppressWarnings("unchecked")
	@Deprecated
	public List<String> getVoteSiteItemLore(String site) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList("GUI.VoteReward." + siteName + ".Item.Lore");
	}

	/**
	 * Gets the vote site item material.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item material
	 */
	@Deprecated
	public String getVoteSiteItemMaterial(String site) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Item.Material");
	}

	/**
	 * Gets the vote site item name.
	 *
	 * @param site
	 *            the site
	 * @return the vote site item name
	 */
	@Deprecated
	public String getVoteSiteItemName(String site) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Item.Name");
	}

	/**
	 * Gets the vote site items.
	 *
	 * @param site
	 *            the site
	 * @return the vote site items
	 */
	public Set<String> getVoteSiteItems(String site) {
		String siteName = site.replace(".", "-");
		try {
			return getData().getConfigurationSection("GUI.VoteReward." + siteName + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the vote site items amount.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items amount
	 */
	@Deprecated
	public int getVoteSiteItemsAmount(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Items." + item + ".Amount");
	}

	/**
	 * Gets the vote site items data.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items data
	 */
	@Deprecated
	public int getVoteSiteItemsData(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Items." + item + ".Data");
	}

	/**
	 * Gets the vote site items durability.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items durability
	 */
	@Deprecated
	public int getVoteSiteItemsDurability(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Items." + item + ".Durability");
	}

	/**
	 * Gets the vote site item skull.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 * @return the vote site item skull
	 */
	@Deprecated
	public String getVoteSiteItemSkull(Player player, String siteName) {
		String str = getData().getString("GUI.VoteReward." + siteName + ".Item.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote site items lore.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items lore
	 */
	@SuppressWarnings("unchecked")
	@Deprecated
	public List<String> getVoteSiteItemsLore(String site, String item) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList("GUI.VoteReward." + siteName + ".Items." + item + ".Lore");
	}

	public ConfigurationSection getVoteSiteItemsSection(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getConfigurationSection("GUI.VoteReward." + siteName + ".Items." + item);
	}

	/**
	 * Gets the vote site items material.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items material
	 */
	@Deprecated
	public String getVoteSiteItemsMaterial(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Items." + item + ".Material");

	}

	/**
	 * Gets the vote site items name.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items name
	 */
	@Deprecated
	public String getVoteSiteItemsName(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Items." + item + ".Name");
	}

	/**
	 * Gets the vote site items skull.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 * @param item
	 *            the item
	 * @return the vote site items skull
	 */
	@Deprecated
	public String getVoteSiteItemsSkull(Player player, String siteName, String item) {
		String str = getData().getString("GUI.VoteReward." + siteName + ".Items." + item + ".Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	/**
	 * Gets the vote site items slot.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items slot
	 */
	public int getVoteSiteItemsSlot(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Items." + item + ".Slot");
	}

	/**
	 * Gets the vote URL already voted item amount.
	 *
	 * @return the vote URL already voted item amount
	 */
	@Deprecated
	public int getVoteURLAlreadyVotedItemAmount() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Amount");
		if (num != 0) {
			return num;
		} else {
			return 1;
		}
	}

	public ConfigurationSection getVoteURLAlreadyVotedItemSection() {
		return getData().getConfigurationSection("GUI.VoteURL.AlreadyVotedItem");
	}

	/**
	 * Gets the vote URL already voted item data.
	 *
	 * @return the vote URL already voted item data
	 */
	@Deprecated
	public int getVoteURLAlreadyVotedItemData() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Data");

		return num;

	}

	/**
	 * Gets the vote URL already voted item durability.
	 *
	 * @return the vote URL already voted item durability
	 */
	@Deprecated
	public int getVoteURLAlreadyVotedItemDurability() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Durability");

		return num;

	}

	/**
	 * Gets the vote URL already voted item material.
	 *
	 * @return the vote URL already voted item material
	 */
	@Deprecated
	public String getVoteURLAlreadyVotedItemMaterial() {
		String str = getData().getString("GUI.VoteURL.AlreadyVotedItem.Material");
		if (str != null) {
			return str;
		}
		return "REDSTONE_BLOCK";

	}

	/**
	 * Gets the vote URL already voted item skull.
	 *
	 * @param player
	 *            the player
	 * @return the vote URL already voted item skull
	 */
	@Deprecated
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
	@Deprecated
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
	@Deprecated
	public int getVoteURLCanVoteItemData() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Data");

		return num;

	}

	public ConfigurationSection getVoteURLCanVoteItemSection() {
		return getData().getConfigurationSection("GUI.VoteURL.CanVoteItem");
	}

	/**
	 * Gets the vote URL can vote item durability.
	 *
	 * @return the vote URL can vote item durability
	 */
	@Deprecated
	public int getVoteURLCanVoteItemDurability() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Durability");
		return num;

	}

	/**
	 * Gets the vote URL can vote item material.
	 *
	 * @return the vote URL can vote item material
	 */
	@Deprecated
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
	 * @param player
	 *            the player
	 * @return the vote URL can vote item skull
	 */
	@Deprecated
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

	@Override
	public void onFileCreation() {
		plugin.saveResource("GUI.yml", true);
	}

}
