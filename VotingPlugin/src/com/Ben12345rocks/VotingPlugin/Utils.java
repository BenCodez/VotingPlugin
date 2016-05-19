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
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.UUIDs;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class Utils {

	static Utils instance = new Utils();

	static Main plugin = Main.plugin;

	public static Utils getInstance() {
		return instance;
	}

	private Utils() {
	}

	public Utils(Main plugin) {
		Utils.plugin = plugin;
	}

	public ItemStack addEnchants(ItemStack item,
			HashMap<String, Integer> enchants) {
		if ((enchants == null) || (enchants.size() == 0)) {
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

	public ItemStack addLore(ItemStack item, List<String> lore) {
		if (lore == null) {
			return item;
		}
		if (item == null) {
			return null;
		}

		ItemMeta meta = item.getItemMeta();
		meta.setLore(colorize(lore));
		item.setItemMeta(meta);
		return item;
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

	public String[] colorize(String[] list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.length; i++) {
			list[i] = colorize(list[i]);
		}
		return list;
	}

	public List<String> colorize(List<String> list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.size(); i++) {
			list.set(i, colorize(list.get(i)));
		}
		return list;
	}

	public String colorize(String format) {
		if (format == null) {
			return null;
		}
		return ChatColor.translateAlternateColorCodes('&', format);
	}

	public ArrayList<String> convert(Set<String> set) {
		ArrayList<String> list = new ArrayList<String>();
		for (String st : set) {
			list.add(st);
		}
		return list;
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

	@SuppressWarnings("unused")
	public ArrayList<String> convertArray(String[] list) {
		if (list == null) {
			return null;
		}
		ArrayList<String> newlist = new ArrayList<String>();
		for (String element : list) {
			newlist.add(element);
		}
		if (newlist == null) {
			return null;
		} else {
			return newlist;
		}
	}

	public ArrayList<User> convertSet(Set<User> set) {
		if (set == null) {
			return null;
		}
		ArrayList<User> list = new ArrayList<User>();
		for (User user : set) {
			list.add(user);
		}
		return list;
	}

	@SuppressWarnings("deprecation")
	public int getDayFromMili(long time) {
		Date date = new Date(time);
		return date.getDate();
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

	@SuppressWarnings("deprecation")
	public int getMonthFromMili(long time) {
		Date date = new Date(time);
		return date.getMonth();
	}

	public String getMonthString(int month) {
		if (month == 0) {
			month++;
		}
		return new DateFormatSymbols().getMonths()[month - 1];
	}

	public String getPlayerName(String uuid) {
		if ((uuid == null) || uuid.equalsIgnoreCase("null")) {
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
			return playerName;
		} else {
			return player.getName();
		}

	}

	public List<Block> getRegionBlocks(World world, Location loc1, Location loc2) {
		List<Block> blocks = new ArrayList<Block>();

		for (double x = loc1.getX(); x <= loc2.getX(); x++) {
			for (double y = loc1.getY(); y <= loc2.getY(); y++) {
				for (double z = loc1.getZ(); z <= loc2.getZ(); z++) {
					Location loc = new Location(world, x, y, z);
					blocks.add(loc.getBlock());
				}
			}
		}

		return blocks;
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

	public String getVoteSiteName(String url) {
		ArrayList<String> sites = ConfigVoteSites.getInstance()
				.getVoteSitesNames();
		if (sites != null) {
			for (String siteName : sites) {
				String URL = ConfigVoteSites.getInstance().getServiceSite(
						siteName);
				if (URL.equals(url)) {
					return siteName;
				}
			}
		}
		return url;

	}

	@SuppressWarnings("deprecation")
	public int getYearFromMili(long time) {
		Date date = new Date(time);
		return date.getYear();
	}

	public boolean hasPermission(CommandSender sender, String perm) {
		return sender.hasPermission(plugin.getName() + "." + perm);
	}

	public boolean hasPermission(Player player, String perm) {
		return player.hasPermission(plugin.getName() + "." + perm);
	}

	public boolean hasPermission(String playerName, String perm) {
		if (playerName == null) {
			return false;
		}
		Player player = Bukkit.getPlayer(playerName);
		if (player != null) {
			return player.hasPermission(plugin.getName() + "." + perm);
		}
		return false;
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

	public boolean isPlayer(CommandSender sender) {
		if (sender instanceof Player) {
			return true;
		}
		return false;
	}

	public boolean isPlayerOnline(String playerName) {
		Player player = Bukkit.getPlayer(playerName);
		if (player != null) {
			return true;
		}
		return false;
	}

	public String makeString(int startIndex, String[] strs) {
		String str = new String();
		for (int i = startIndex; i < strs.length; i++) {
			if (i == startIndex) {
				str += strs[i];
			} else {
				str += " " + strs[i];
			}

		}
		return str;
	}

	public String makeStringList(ArrayList<String> list) {
		if (list == null) {
			return "";
		}
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

	public ItemStack nameItem(ItemStack item, String name) {
		if (name == null) {
			return item;
		}
		ItemMeta meta = item.getItemMeta();
		meta.setDisplayName(name);
		item.setItemMeta(meta);
		return item;

	}

	public ArrayList<User> removeDoubleUsers(ArrayList<User> list) {
		Set<User> hs = new HashSet<User>();
		ArrayList<User> al = new ArrayList<User>();
		hs.addAll(list);
		al.clear();
		al.addAll(hs);

		return al;
	}

	public ArrayList<String> replaceIgnoreCase(ArrayList<String> list,
			String toReplace, String replaceWith) {
		ArrayList<String> newList = new ArrayList<String>();
		for (String msg : list) {
			newList.add(replaceIgnoreCase(msg, toReplace, replaceWith));
		}
		return newList;
	}

	public String replaceIgnoreCase(String str, String toReplace,
			String replaceWith) {
		if (str == null) {
			return "";
		}
		if (toReplace == null || replaceWith == null) {
			return str;
		}
		return str.replaceAll("(?i)" + toReplace, replaceWith);
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

	public boolean startsWithIgnoreCase(String str1, String str2) {
		return str1.toLowerCase().startsWith(str2.toLowerCase());
	}
}
