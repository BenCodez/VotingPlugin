package com.Ben12345rocks.VotingPlugin;

import gyurix.api.TitleAPI;

import java.text.DateFormatSymbols;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;

import me.clip.placeholderapi.PlaceholderAPI;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.TextComponent;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.SkullMeta;

import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class Utils.
 */
public class Utils {

	/** The instance. */
	static Utils instance = new Utils();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Utils.
	 *
	 * @return single instance of Utils
	 */
	public static Utils getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new utils.
	 */
	private Utils() {
	}

	/**
	 * Instantiates a new utils.
	 *
	 * @param plugin the plugin
	 */
	public Utils(Main plugin) {
		Utils.plugin = plugin;
	}

	/**
	 * Adds the enchants.
	 *
	 * @param item the item
	 * @param enchants the enchants
	 * @return the item stack
	 */
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

	/**
	 * Adds the lore.
	 *
	 * @param item the item
	 * @param lore the lore
	 * @return the item stack
	 */
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

	/**
	 * Colorize.
	 *
	 * @param list the list
	 * @return the array list
	 */
	public ArrayList<String> colorize(ArrayList<String> list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.size(); i++) {
			list.set(i, colorize(list.get(i)));
		}
		return list;
	}

	/**
	 * Colorize.
	 *
	 * @param list the list
	 * @return the list
	 */
	public List<String> colorize(List<String> list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.size(); i++) {
			list.set(i, colorize(list.get(i)));
		}
		return list;
	}

	/**
	 * Colorize.
	 *
	 * @param format the format
	 * @return the string
	 */
	public String colorize(String format) {
		if (format == null) {
			return null;
		}
		return ChatColor.translateAlternateColorCodes('&', format);
	}

	/**
	 * Colorize.
	 *
	 * @param list the list
	 * @return the string[]
	 */
	public String[] colorize(String[] list) {
		if (list == null) {
			return null;
		}

		for (int i = 0; i < list.length; i++) {
			list[i] = colorize(list[i]);
		}
		return list;
	}

	/**
	 * Compto string.
	 *
	 * @param comps the comps
	 * @return the array list
	 */
	public ArrayList<String> comptoString(ArrayList<TextComponent> comps) {
		ArrayList<String> txt = new ArrayList<String>();
		for (TextComponent comp : comps) {
			txt.add(compToString(comp));
		}
		return txt;
	}

	/**
	 * Comp to string.
	 *
	 * @param comp the comp
	 * @return the string
	 */
	public String compToString(TextComponent comp) {
		return colorize(comp.toPlainText());
	}

	/**
	 * Convert.
	 *
	 * @param array the array
	 * @return the user[]
	 */
	public User[] convert(ArrayList<User> array) {
		if (array == null) {
			return null;
		}
		User[] list = new User[array.size()];
		for (int i = 0; i < array.size(); i++) {
			list[i] = array.get(i);
		}
		return list;
	}

	/**
	 * Convert.
	 *
	 * @param set the set
	 * @return the array list
	 */
	public ArrayList<String> convert(Set<String> set) {
		ArrayList<String> list = new ArrayList<String>();
		for (String st : set) {
			list.add(st);
		}
		return list;
	}

	/**
	 * Convert array.
	 *
	 * @param list the list
	 * @return the string[]
	 */
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

	/**
	 * Convert array.
	 *
	 * @param list the list
	 * @return the array list
	 */
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

	/**
	 * Convert set.
	 *
	 * @param set the set
	 * @return the array list
	 */
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

	/**
	 * Gets the day from mili.
	 *
	 * @param time the time
	 * @return the day from mili
	 */
	@SuppressWarnings("deprecation")
	public int getDayFromMili(long time) {
		Date date = new Date(time);
		return date.getDate();
	}

	/**
	 * Gets the hour from mili.
	 *
	 * @param time the time
	 * @return the hour from mili
	 */
	@SuppressWarnings("deprecation")
	public int getHourFromMili(long time) {
		Date date = new Date(time);
		return date.getHours();
	}

	/**
	 * Gets the minutes from mili.
	 *
	 * @param time the time
	 * @return the minutes from mili
	 */
	@SuppressWarnings("deprecation")
	public int getMinutesFromMili(long time) {
		Date date = new Date(time);
		return date.getMinutes();
	}

	/**
	 * Gets the month from mili.
	 *
	 * @param time the time
	 * @return the month from mili
	 */
	@SuppressWarnings("deprecation")
	public int getMonthFromMili(long time) {
		Date date = new Date(time);
		return date.getMonth();
	}

	/**
	 * Gets the month string.
	 *
	 * @param month the month
	 * @return the month string
	 */
	public String getMonthString(int month) {
		if (month == 0) {
			month++;
		}
		return new DateFormatSymbols().getMonths()[month - 1];
	}

	/**
	 * Gets the player name.
	 *
	 * @param uuid the uuid
	 * @return the player name
	 */
	public String getPlayerName(String uuid) {
		if ((uuid == null) || uuid.equalsIgnoreCase("null")) {

			plugin.debug("Null UUID");

			return null;
		}

		User user = new User(new com.Ben12345rocks.VotingPlugin.Objects.UUID(
				uuid), false);
		String playerName = Data.getInstance().getName(user);

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

	/**
	 * Gets the region blocks.
	 *
	 * @param world the world
	 * @param loc1 the loc 1
	 * @param loc2 the loc 2
	 * @return the region blocks
	 */
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

	/**
	 * Gets the uuid.
	 *
	 * @param playerName the player name
	 * @return the uuid
	 */
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

	/**
	 * Gets the vote site name.
	 *
	 * @param url the url
	 * @return the vote site name
	 */
	public String getVoteSiteName(String url) {
		ArrayList<String> sites = ConfigVoteSites.getInstance()
				.getVoteSitesNames();
		if (url == null) {
			return null;
		}
		if (sites != null) {
			for (String siteName : sites) {
				String URL = ConfigVoteSites.getInstance().getServiceSite(
						siteName);
				if (URL != null) {
					if (URL.equals(url)) {
						return siteName;
					}
				}
			}
		}
		return url;

	}

	/**
	 * Gets the year from mili.
	 *
	 * @param time the time
	 * @return the year from mili
	 */
	@SuppressWarnings("deprecation")
	public int getYearFromMili(long time) {
		Date date = new Date(time);
		return date.getYear();
	}

	/**
	 * Checks for permission.
	 *
	 * @param sender the sender
	 * @param perm the perm
	 * @return true, if successful
	 */
	public boolean hasPermission(CommandSender sender, String perm) {
		return sender.hasPermission(plugin.getName() + "." + perm);
	}

	/**
	 * Checks for permission.
	 *
	 * @param player the player
	 * @param perm the perm
	 * @return true, if successful
	 */
	public boolean hasPermission(Player player, String perm) {
		return player.hasPermission(plugin.getName() + "." + perm);
	}

	/**
	 * Checks for permission.
	 *
	 * @param playerName the player name
	 * @param perm the perm
	 * @return true, if successful
	 */
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

	/**
	 * Checks if is int.
	 *
	 * @param st the st
	 * @return true, if is int
	 */
	public boolean isInt(String st) {
		try {
			@SuppressWarnings("unused")
			int num = Integer.parseInt(st);
			return true;

		} catch (NumberFormatException ex) {
			return false;
		}
	}

	/**
	 * Checks if is player.
	 *
	 * @param sender the sender
	 * @return true, if is player
	 */
	public boolean isPlayer(CommandSender sender) {
		if (sender instanceof Player) {
			return true;
		}
		return false;
	}

	/**
	 * Checks if is player online.
	 *
	 * @param playerName the player name
	 * @return true, if is player online
	 */
	public boolean isPlayerOnline(String playerName) {
		Player player = Bukkit.getPlayer(playerName);
		if (player != null) {
			return true;
		}
		return false;
	}

	/**
	 * Make string.
	 *
	 * @param startIndex the start index
	 * @param strs the strs
	 * @return the string
	 */
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

	/**
	 * Make string list.
	 *
	 * @param list the list
	 * @return the string
	 */
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

	/**
	 * Name item.
	 *
	 * @param item the item
	 * @param name the name
	 * @return the item stack
	 */
	public ItemStack nameItem(ItemStack item, String name) {
		if (name == null) {
			return item;
		}
		ItemMeta meta = item.getItemMeta();
		meta.setDisplayName(name);
		item.setItemMeta(meta);
		return item;

	}

	/**
	 * Prints the map.
	 *
	 * @param topVoterMonthly the top voter monthly
	 */
	public void printMap(HashMap<User, Integer> topVoterMonthly) {
		for (Entry<User, Integer> entry : topVoterMonthly.entrySet()) {
			plugin.debug("Key : " + entry.getKey().getPlayerName()
					+ " Value : " + entry.getValue());
		}
	}

	/**
	 * Removes the double users.
	 *
	 * @param list the list
	 * @return the array list
	 */
	public ArrayList<User> removeDoubleUsers(ArrayList<User> list) {
		Set<User> hs = new HashSet<User>();
		ArrayList<User> al = new ArrayList<User>();
		hs.addAll(list);
		al.clear();
		al.addAll(hs);

		return al;
	}

	/**
	 * Removes the duplicates.
	 *
	 * @param list the list
	 * @return the array list
	 */
	public ArrayList<String> removeDuplicates(ArrayList<String> list) {
		Set<String> set = new HashSet<String>(list);
		return new ArrayList<String>(set);
	}

	/**
	 * Replace.
	 *
	 * @param list the list
	 * @param toReplace the to replace
	 * @param replaceWith the replace with
	 * @return the list
	 */
	public List<String> replace(List<String> list, String toReplace,
			String replaceWith) {
		if (list == null) {
			return null;
		}
		if (replaceWith == null || toReplace == null) {
			return list;
		}
		for (int i = 0; i < list.size(); i++) {
			list.set(i, list.get(i).replace(toReplace, replaceWith));
		}
		return list;
	}

	/**
	 * Replace ignore case.
	 *
	 * @param list the list
	 * @param toReplace the to replace
	 * @param replaceWith the replace with
	 * @return the array list
	 */
	public ArrayList<String> replaceIgnoreCase(ArrayList<String> list,
			String toReplace, String replaceWith) {
		ArrayList<String> newList = new ArrayList<String>();
		for (String msg : list) {
			newList.add(replaceIgnoreCase(msg, toReplace, replaceWith));
		}
		return newList;
	}

	/**
	 * Replace ignore case.
	 *
	 * @param str the str
	 * @param toReplace the to replace
	 * @param replaceWith the replace with
	 * @return the string
	 */
	public String replaceIgnoreCase(String str, String toReplace,
			String replaceWith) {
		if (str == null) {
			return "";
		}
		if ((toReplace == null) || (replaceWith == null)) {
			return str;
		}
		return str.replaceAll("(?i)" + toReplace, replaceWith);
	}

	/**
	 * Replace place holders.
	 *
	 * @param player the player
	 * @param text the text
	 * @return the string
	 */
	public String replacePlaceHolders(Player player, String text) {
		if (plugin.placeHolderAPIEnabled) {
			return PlaceholderAPI.setBracketPlaceholders(player, text);
		} else {
			return text;
		}
	}

	/**
	 * Send title.
	 *
	 * @param player the player
	 * @param title the title
	 * @param subTitle the sub title
	 * @param fadeIn the fade in
	 * @param showTime the show time
	 * @param fadeOut the fade out
	 */
	public void sendTitle(Player player, String title, String subTitle,
			int fadeIn, int showTime, int fadeOut) {
		if (plugin.spigotLibEnabled) {

			if (title == null) {
				title = "";
			}
			if (subTitle == null) {
				subTitle = "";
			}
			TitleAPI.set(colorize(title), colorize(subTitle), fadeIn, showTime,
					fadeOut, player);

		}
	}

	/**
	 * Sets the durabilty.
	 *
	 * @param item the item
	 * @param durability the durability
	 * @return the item stack
	 */
	public ItemStack setDurabilty(ItemStack item, int durability) {
		if (item == null) {
			return null;
		}
		if (durability > 0) {
			item.setDurability((short) durability);
		}
		return item;
	}

	/**
	 * Sets the skull owner.
	 *
	 * @param item the item
	 * @param playerName the player name
	 * @return the item stack
	 */
	public ItemStack setSkullOwner(ItemStack item, String playerName) {
		if (item == null) {
			return null;
		}
		if (playerName == null || playerName.equalsIgnoreCase("")) {
			return item;
		}
		ItemMeta meta = item.getItemMeta();
		((SkullMeta) meta).setOwner(playerName);
		return item;
	}

	/**
	 * Sets the to array.
	 *
	 * @param set the set
	 * @return the string[]
	 */
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

	/**
	 * Sort by values.
	 *
	 * @param unsortMap the unsort map
	 * @param order the order
	 * @return the hash map
	 */
	public HashMap<User, Integer> sortByValues(
			HashMap<User, Integer> unsortMap, final boolean order) {

		List<Entry<User, Integer>> list = new LinkedList<Entry<User, Integer>>(
				unsortMap.entrySet());

		// Sorting the list based on values
		Collections.sort(list, new Comparator<Entry<User, Integer>>() {
			@Override
			public int compare(Entry<User, Integer> o1, Entry<User, Integer> o2) {
				if (order) {
					return o1.getValue().compareTo(o2.getValue());
				} else {
					return o2.getValue().compareTo(o1.getValue());

				}
			}
		});

		// Maintaining insertion order with the help of LinkedList
		HashMap<User, Integer> sortedMap = new LinkedHashMap<User, Integer>();
		for (Entry<User, Integer> entry : list) {
			sortedMap.put(entry.getKey(), entry.getValue());
		}

		return sortedMap;
	}

	/**
	 * Starts with ignore case.
	 *
	 * @param str1 the str 1
	 * @param str2 the str 2
	 * @return true, if successful
	 */
	public boolean startsWithIgnoreCase(String str1, String str2) {
		return str1.toLowerCase().startsWith(str2.toLowerCase());
	}

	/**
	 * String to comp.
	 *
	 * @param string the string
	 * @return the text component
	 */
	public TextComponent stringToComp(String string) {
		TextComponent base = new TextComponent("");
		boolean previousLetter = false;
		ChatColor currentColor = null;
		boolean bold = false;
		boolean italic = false;
		boolean underline = false;
		boolean strike = false;
		boolean magic = false;
		String currentstring = "";
		for (int i = 0; i < string.length(); i++) {
			char c = string.charAt(i);
			if (c == '&') {
				if (string.charAt(i + 1) == 'l') {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentstring = "";
						currentColor = null;
						i++;
						previousLetter = false;
					} else {
						bold = true;
						i++;
					}
				} else if (string.charAt(i + 1) == 'k') {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentstring = "";
						currentColor = null;
						i++;
						previousLetter = false;
					} else {
						magic = true;
						i++;
					}
				} else if (string.charAt(i + 1) == 'm') {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentstring = "";
						currentColor = null;
						i++;
						previousLetter = false;
					} else {
						strike = true;
						i++;
					}
				} else if (string.charAt(i + 1) == 'n') {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentstring = "";
						currentColor = null;
						i++;
						previousLetter = false;
					} else {
						underline = true;
						i++;
					}
				} else if (string.charAt(i + 1) == 'o') {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentstring = "";
						currentColor = null;
						i++;
						previousLetter = false;
					} else {
						italic = true;
						i++;
					}
				} else if (string.charAt(i + 1) == 'r') {
					TextComponent newTC = new TextComponent(currentstring);
					if (currentColor != null) {
						newTC.setColor(currentColor);
					}
					newTC.setBold(bold);
					newTC.setItalic(italic);
					newTC.setUnderlined(underline);
					newTC.setStrikethrough(strike);
					newTC.setObfuscated(magic);
					base.addExtra(newTC);
					bold = false;
					italic = false;
					underline = false;
					strike = false;
					magic = false;
					currentstring = "";
					currentColor = null;
					i++;
					previousLetter = false;
				} else if (ChatColor.getByChar(string.charAt(i + 1)) != null) {
					if (previousLetter) {
						TextComponent newTC = new TextComponent(currentstring);
						if (currentColor != null) {
							newTC.setColor(currentColor);
						}
						newTC.setBold(bold);
						newTC.setItalic(italic);
						newTC.setUnderlined(underline);
						newTC.setStrikethrough(strike);
						newTC.setObfuscated(magic);
						base.addExtra(newTC);
						bold = false;
						italic = false;
						underline = false;
						strike = false;
						magic = false;
						currentColor = ChatColor
								.getByChar(string.charAt(i + 1));
						currentstring = "";
						i++;
						previousLetter = false;
					} else {
						currentColor = ChatColor
								.getByChar(string.charAt(i + 1));
						i++;
					}
				} else {
					previousLetter = true;
					currentstring = currentstring + c;
				}
			} else {
				previousLetter = true;
				currentstring = currentstring + c;
			}
		}
		TextComponent newTC = new TextComponent(currentstring);
		if (currentColor != null) {
			newTC.setColor(currentColor);
		}
		newTC.setBold(bold);
		newTC.setItalic(italic);
		newTC.setUnderlined(underline);
		newTC.setStrikethrough(strike);
		newTC.setObfuscated(magic);
		base.addExtra(newTC);
		return base;
	}
}
