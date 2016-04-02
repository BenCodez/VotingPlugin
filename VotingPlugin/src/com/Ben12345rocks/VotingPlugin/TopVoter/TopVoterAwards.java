package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
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

public class TopVoterAwards {

	static TopVoterAwards instance = new TopVoterAwards();

	static Main plugin = Main.plugin;

	public static TopVoterAwards getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private TopVoterAwards() {
	}

	public TopVoterAwards(Main plugin) {
		TopVoterAwards.plugin = plugin;
	}

	/**
	 *
	 * @param user
	 *            User to execute commands with
	 */
	public void doTopVoterAwardCommands(User user, int place) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = this.getConsoleCommands(place);

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
		ArrayList<String> playercmds = this.getPlayerCommands(place);

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
	public ArrayList<String> getConsoleCommands(int place) {
		return (ArrayList<String>) getData().getList(
				"Awards." + place + ".Commands.Console");
	}

	public FileConfiguration getData() {
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
	public int getEnchantLevel(int place, String item, String enchant) {
		return getData().getInt(
				"Awards." + place + ".Items." + item + ".Enchants." + enchant);
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getEnchantments(int place, String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = getData().getConfigurationSection(
					"Awards." + place + ".Items." + item + ".Enchants")
					.getKeys(false);
			for (String enchant : enchants) {
				enchantments
				.put(enchant, getEnchantLevel(place, item, enchant));
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
	public int getItemAmount(int place, String item) {
		return getData().getInt(
				"Awards." + place + ".Items." + item + ".Amount");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getItemData(int place, String item) {
		return getData().getInt("Awards." + place + ".Items." + item + ".Data");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getItemID(int place, String item) {
		return getData().getInt("Awards." + place + ".Items." + item + ".ID");
	}

	@SuppressWarnings("unchecked")
	/**
	 *
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getItemLore(int place, String item) {
		return (ArrayList<String>) getData().getList(
				"Awards." + place + ".Items." + item + ".Lore");
	}

	/**
	 *
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getItemName(int place, String item) {
		return getData().getString(
				"Awards." + place + ".Items." + item + ".Name");
	}

	/**
	 *
	 * @return Items of VoteSite
	 */
	public Set<String> getItems(int place) {
		return getData().getConfigurationSection("Awards." + place + ".Items")
				.getKeys(false);
	}

	public String getMessage(int place) {
		return getData().getString("Awards." + place + ".Message");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getPlayerCommands(int place) {
		return (ArrayList<String>) getData().getList(
				"Awards." + place + ".Commands.Player");
	}

	public Set<String> getPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("Awards").getKeys(false);
		} catch (Exception ex) {
			Set<String> noValues = new HashSet<String>();
			return noValues;
		}
	}

	@SuppressWarnings("deprecation")
	public ItemStack getTopVoterAwardItemStack(int place, String item) {
		int id = this.getItemID(place, item);
		int amount = this.getItemAmount(place, item);
		int data = this.getItemData(place, item);

		String itemName = this.getItemName(place, item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = this.getItemLore(place, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addlore(itemStack, lore);
		itemStack = Utils.getInstance().addEnchants(itemStack,
				this.getEnchantments(place, item));
		return itemStack;
	}

	public int getTopVoterAwardMoney(int place) {
		int money = getData().getInt("Awards." + place + ".Money");
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
