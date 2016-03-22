package com.Ben12345rocks.VotingPlugin;

import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserData.Data;
import com.Ben12345rocks.VotingPlugin.UserData.UUIDs;

public class Utils {

	private Utils() {
	}

	static Utils instance = new Utils();

	public static Utils getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public Utils(Main plugin) {
		Utils.plugin = plugin;
	}

	public String colorize(String format) {
		if (format == null) {
			return null;
		}
		return ChatColor.translateAlternateColorCodes('&', format);
	}

	public ItemStack nameItem(ItemStack item, String name) {
		if (name == null) {
			return item;
		}
		ItemMeta meta = item.getItemMeta();
		meta.setDisplayName(name);
		item.setItemMeta(meta);
		return item;

	}

	public ItemStack addlore(ItemStack item, List<String> lore) {
		if (lore == null) {
			return item;
		}
		if (item == null) {
			return null;
		}

		ItemMeta meta = item.getItemMeta();
		meta.setLore(lore);
		item.setItemMeta(meta);
		return item;
	}

	@SuppressWarnings("unused")
	public String[] convertArray(ArrayList<String> list) {
		if (list == null) {
			return null;
		}
		String[] string = new String[list.size()];
		for (int i = 0; i < list.size(); i++) {
			string[i] = list.get(i);
		}
		if (string == null) {
			return null;
		} else {
			return string;
		}
	}

	public ArrayList<String> colorize(ArrayList<String> list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.size(); i++) {
			list.set(i, colorize(list.get(i)));
		}
		return list;
	}

	public String getMonthString(int month) {
		if (month == 0) {
			month++;
		}
		return new DateFormatSymbols().getMonths()[month - 1];
	}

	@SuppressWarnings("unused")
	public ArrayList<String> convertArray(String[] list) {
		if (list == null) {
			return null;
		}
		ArrayList<String> newlist = new ArrayList<String>();
		for (int i = 0; i < list.length; i++) {
			newlist.add(list[i]);
		}
		if (newlist == null) {
			return null;
		} else {
			return newlist;
		}
	}

	@SuppressWarnings("deprecation")
	public String getUUID(String playerName) {
		if (playerName == null) {
			return null;
		}
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			return Bukkit.getOfflinePlayer(playerName).getUniqueId().toString();
		} else {
			return player.getUniqueId().toString();
		}
	}

	public String getPlayerName(String uuid) {
		if (uuid == null || uuid.equalsIgnoreCase("null")) {
			if (Config.getInstance().getDebugEnabled()) {
				plugin.getLogger().info("Null UUID");
			}
			return null;
		}

		String playerName = UUIDs.getInstance().getPlayerName(uuid);
		if (playerName != null) {
			return playerName;
		}

		User user = new User(new com.Ben12345rocks.VotingPlugin.Objects.UUID(
				uuid), false);
		playerName = Data.getInstance().getName(user);

		if (playerName != null) {
			return playerName;
		}

		Player player = Bukkit.getPlayer(UUID.fromString(uuid));
		if (player == null) {
			playerName = Bukkit.getOfflinePlayer(UUID.fromString(uuid))
					.getName();
			/*
			 * if (playerName == null) { playerName =
			 * MojangNameLookup.lookupName(UUID.fromString(uuid)); }
			 */
			return playerName;
		} else {
			return player.getName();
		}

	}

	@SuppressWarnings("deprecation")
	public boolean hasPermission(String playerName, String perm) {
		Player player = Bukkit.getPlayer(playerName);
		if (player != null) {
			return player.hasPermission(perm);
		}
		return false;
	}

	@SuppressWarnings("unused")
	public String[] setToArray(Set<String> set) {
		String[] array = new String[set.size()];
		int i = 0;
		for (String item : set) {
			array[i] = item;
			i++;
		}
		if (array == null) {
			return null;
		} else {
			return array;
		}
	}

	public String makeStringList(ArrayList<String> list) {
		String string = new String();
		if (list.size() > 1) {
			for (int i = 0; i < list.size(); i++) {
				if (i == 0) {
					string += list.get(i);
				} else {
					string += ", " + list.get(i);
				}
			}
		} else if (list.size() == 1) {
			string = list.get(0);
		}
		return string;
	}

	@SuppressWarnings("deprecation")
	public boolean isPlayerOnline(String playerName) {
		Player player = Bukkit.getPlayer(playerName);
		if (player != null) {
			return true;
		}
		return false;
	}

	public ArrayList<User> convertSet(Set<User> set) {
		ArrayList<User> list = new ArrayList<User>();
		for (User user : set) {
			list.add(user);
		}
		return list;
	}

	public ArrayList<String> convert(Set<String> set) {
		ArrayList<String> list = new ArrayList<String>();
		for (String st : set) {
			list.add(st);
		}
		return list;
	}

	public ArrayList<User> removeDoubleUsers(ArrayList<User> list) {
		Set<User> hs = new HashSet<User>();
		ArrayList<User> al = new ArrayList<User>();
		hs.addAll(list);
		al.clear();
		al.addAll(hs);

		return al;
	}

	public boolean isInt(String st) {
		try {
			@SuppressWarnings("unused")
			int num = Integer.parseInt(st);
			return true;

		} catch (NumberFormatException ex) {
			return false;
		}
	}

	public boolean hasPermission(Player player, String perm) {
		return player.hasPermission(plugin.getName() + "." + perm);
	}

	public boolean hasPermission(CommandSender sender, String perm) {
		return sender.hasPermission(plugin.getName() + "." + perm);
	}

	public ItemStack addEnchants(ItemStack item,
			HashMap<String, Integer> enchants) {
		if (enchants == null || enchants.size() == 0) {
			return item;
		}
		ItemMeta meta = item.getItemMeta();
		for (String enchant : enchants.keySet()) {
			meta.addEnchant(Enchantment.getByName(enchant),
					enchants.get(enchant), false);
		}
		item.setItemMeta(meta);
		return item;

	}

	@SuppressWarnings("deprecation")
	public int getMonthFromMili(long time) {
		Date date = new Date(time);
		return date.getMonth();
	}

	@SuppressWarnings("deprecation")
	public int getDayFromMili(long time) {
		Date date = new Date(time);
		return date.getDate();
	}

	@SuppressWarnings("deprecation")
	public int getYearFromMili(long time) {
		Date date = new Date(time);
		return date.getYear();
	}

	@SuppressWarnings("deprecation")
	public int getHourFromMili(long time) {
		Date date = new Date(time);
		return date.getHours();
	}

	@SuppressWarnings("deprecation")
	public int getMinutesFromMili(long time) {
		Date date = new Date(time);
		return date.getMinutes();
	}

	public String getVoteSiteName(String url) {
		Set<String> sites = ConfigVoteSites.getInstance().getVoteSitesName();
		for (String siteName : sites) {
			String URL = ConfigVoteSites.getInstance().getVoteSiteServiceSite(
					siteName);
			if (URL.equals(url)) {
				return siteName;
			}
		}
		return url;
	}
}
