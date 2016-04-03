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
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class ConfigCumulativeAwards {

	static ConfigCumulativeAwards instance = new ConfigCumulativeAwards();

	static Main plugin = Main.plugin;

	public static ConfigCumulativeAwards getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigCumulativeAwards() {
	}

	public ConfigCumulativeAwards(Main plugin) {
		ConfigCumulativeAwards.plugin = plugin;
	}

	/**
	 *
	 * @param user
	 *            User to execute commands with
	 */
	public void doTopVoterAwardCommands(User user, String reward) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = getConsoleCommands(reward);

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = getPlayerCommands(reward);

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getConsoleCommands(String reward) {
		return (ArrayList<String>) getData().getList(
				"Awards." + reward + ".Commands.Console");
	}

	public FileConfiguration getData() {
		return data;
	}

	public int getRequiredVotes(String reward) {
		return getData().getInt("Awards." + reward + ".Votes");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<VoteSite> getRequiredVoteSites(String reward) {
		List<String> list = (List<String>) getData().getList(
				"Awards." + reward + ".VoteSites");
		if (list.contains("All")) {
			list.clear();
			for (VoteSite voteSite : plugin.voteSites) {
				list.add(voteSite.getSiteName());
			}
		}

		ArrayList<VoteSite> voteSites = new ArrayList<VoteSite>();
		for (String siteName : list) {
			VoteSite voteSite = new VoteSite(siteName);
			if (voteSite != null) {
				voteSites.add(voteSite);
			}
		}
		return voteSites;

	}

	/**
	 *
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getEnchantLevel(String reward, String item, String enchant) {
		return getData().getInt(
				"Awards." + reward + ".Items." + item + ".Enchants." + enchant);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getEnchantments(String reward, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"Awards." + reward + ".Items." + item + ".Enchants")
					.getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant,
						getEnchantLevel(reward, item, enchant));
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
	public int getItemAmount(String reward, String item) {
		return getData().getInt(
				"Awards." + reward + ".Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getItemData(String reward, String item) {
		return getData()
				.getInt("Awards." + reward + ".Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getItemID(String reward, String item) {
		return getData().getInt("Awards." + reward + ".Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getItemLore(String reward, String item) {
		return (ArrayList<String>) getData().getList(
				"Awards." + reward + ".Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getItemName(String reward, String item) {
		return getData().getString(
				"Awards." + reward + ".Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getItems(String reward) {
		try {
			return getData().getConfigurationSection(
					"Awards." + reward + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands(String reward) {
		return (ArrayList<String>) getData().getList(
				"Awards." + reward + ".Commands.Player");
	}

	public Set<String> getPossibleRewardrewards() {
		try {
			return getData().getConfigurationSection("Awards").getKeys(false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	@SuppressWarnings("deprecation")
	public ItemStack getTopVoterAwardItemStack(String reward, String item) {
		int id = getItemID(reward, item);
		int amount = getItemAmount(reward, item);
		int data = getItemData(reward, item);

		String itemName = getItemName(reward, item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = getItemLore(reward, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addlore(itemStack, lore);
		itemStack = Utils.getInstance().addEnchants(itemStack,
				getEnchantments(reward, item));
		return itemStack;
	}

	public int getTopVoterAwardMoney(String reward) {
		int money = getData().getInt("Awards." + reward + ".Money");
		return money;
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer()
					.getLogger()
					.severe(ChatColor.RED
							+ "Could not save TopVoterAwards.yml!");
		}
	}

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "TopVoterAwards.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("TopVoterAwards.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED
								+ "Could not create TopVoterAwards.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}
}
