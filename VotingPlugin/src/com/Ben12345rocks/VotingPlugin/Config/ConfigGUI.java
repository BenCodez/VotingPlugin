package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
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

	public int getVoteGUISlotDurability(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.Durability");
	}

	public int getVoteGUISlotID(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Item.ID");
	}

	public String getVoteGUISlotName(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Item.Name");
	}

	public Set<String> getVoteGUISlots() {
		try {
			return getData().getConfigurationSection("GUI.VoteGUI").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public String getVoteGUISlotSkull(Player player, String slot) {
		String str = getData().getString("GUI.VoteGUI." + slot + ".Item.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	public int getVoteGUISlotSlot(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Slot");
	}

	public int getVoteSiteItemAmount(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Amount");
	}

	public int getVoteSiteItemData(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.Data");
	}

	public int getVoteSiteItemDurability(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Item.Durability");
	}

	public int getVoteSiteItemID(String site) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Item.ID");
	}

	@SuppressWarnings("unchecked")
	public List<String> getVoteSiteItemLore(String site) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList(
				"GUI.VoteReward." + siteName + ".Item.Lore");
	}

	public String getVoteSiteItemName(String site) {
		String siteName = site.replace(".", "-");
		return getData().getString("GUI.VoteReward." + siteName + ".Item.Name");
	}

	public Set<String> getVoteSiteItems(String site) {
		String siteName = site.replace(".", "-");
		try {
			return getData().getConfigurationSection(
					"GUI.VoteReward." + siteName + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getVoteSiteItemsAmount(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Amount");
	}

	public int getVoteSiteItemsData(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Data");
	}

	public int getVoteSiteItemsDurability(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData()
				.getInt("GUI.VoteReward." + siteName + ".Items." + item
						+ ".Durability");
	}

	public int getVoteSiteItemsID(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".ID");
	}

	public String getVoteSiteItemSkull(Player player, String siteName) {
		String str = getData().getString(
				"GUI.VoteReward." + siteName + ".Item.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	@SuppressWarnings("unchecked")
	public List<String> getVoteSiteItemsLore(String site, String item) {
		String siteName = site.replace(".", "-");
		return (List<String>) getData().getList(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Lore");
	}

	public String getVoteSiteItemsName(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getString(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Name");
	}

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

	public int getVoteSiteItemsSlot(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt(
				"GUI.VoteReward." + siteName + ".Items." + item + ".Slot");
	}

	public int getVoteURLAlreadyVotedItemAmount() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Amount");
		if (num != 0) {
			return num;
		} else {
			return 1;
		}
	}

	public int getVoteURLAlreadyVotedItemData() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Data");

		return num;

	}

	public int getVoteURLAlreadyVotedItemDurability() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.Durability");

		return num;

	}

	public int getVoteURLAlreadyVotedItemID() {
		int num = getData().getInt("GUI.VoteURL.AlreadyVotedItem.ID");
		if (num != 0) {
			return num;
		} else {
			return 152;
		}
	}

	public String getVoteURLAlreadyVotedItemSkull(Player player) {
		String str = getData().getString("GUI.VoteURL.AlreadyVotedItem.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;

	}

	public int getVoteURLCanVoteItemAmount() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Amount");
		if (num != 0) {
			return num;
		} else {
			return 1;
		}
	}

	public int getVoteURLCanVoteItemData() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Data");

		return num;

	}

	public int getVoteURLCanVoteItemDurability() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.Durability");
		return num;

	}

	public int getVoteURLCanVoteItemID() {
		int num = getData().getInt("GUI.VoteURL.CanVoteItem.ID");
		if (num != 0) {
			return num;
		} else {
			return 133;
		}
	}

	public String getVoteURLCanVoteItemSkull(Player player) {
		String str = getData().getString("GUI.VoteURL.CanVoteItem.Skull");
		if (str != null) {
			str = str.replace("%Player%", player.getName());
			return str;
		}
		return null;
	}

	public String getVoteURLNextVote() {
		String str = getData().getString("GUI.VoteURL.NextVote");
		if (str != null) {
			return str;
		} else {
			return "&cCan Vote In: %Info%";
		}
	}

	public String getVoteURLSeeURL() {
		String str = getData().getString("GUI.VoteURL.SeeURL");
		if (str != null) {
			return str;
		} else {
			return "&cClick to see URL";
		}
	}

	public String getVoteURLSiteName() {
		String str = getData().getString("GUI.VoteURL.SiteName");
		if (str != null) {
			return str;
		} else {
			return "&c%Name%";
		}
	}

	public boolean getVoteURLViewAllUrlsButtonEnabled() {
		return getData().getBoolean("GUI.VoteURL.ViewAllUrlsButtonEnabled");
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
