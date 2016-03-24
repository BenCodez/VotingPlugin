package com.Ben12345rocks.VotingPlugin.UserData;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class Data {

	static Data instance = new Data();

	static Main plugin = Main.plugin;

	public static Data getInstance() {
		return instance;
	}

	private Data() {
	}

	public Data(Main plugin) {
		Data.plugin = plugin;
	}

	// FileConfiguration data;
	// File dFile;

	public void addSiteMonthTotal(User user, String voteSiteName) {
		setSiteMonthTotal(user, voteSiteName,
				getSiteMonthTotal(user, voteSiteName) + 1);
	}

	public void addTotal(User user, String voteSite) {
		set(user, user.getUUID() + ".Total." + voteSite,
				getTotal(user, voteSite) + 1);
	}

	public int getBonusOfflineVotes(User user) {
		return getData(user).getInt(user.getUUID() + ".BonusOfflineVotes");
	}

	public FileConfiguration getData(User user) {
		File dFile = getPlayerFile(user);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	public ArrayList<String> getFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator + "Data");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	public String getName(User user) {
		return getData(user).getString(user.getUUID() + ".Name");
	}

	public int getOfflineVotes(User user, String voteSite) {
		return getData(user).getInt(
				user.getUUID() + ".OfflineVotes." + voteSite);
	}

	public File getPlayerFile(User user) {
		String playerName = user.getPlayerName();
		String uuid = user.getUUID();
		File dFile = new File(plugin.getDataFolder() + File.separator + "Data",
				uuid + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				setName(user);
				if (Config.getInstance().getDebugEnabled()) {
					plugin.getLogger().info(
							"Created file: " + uuid + ".yml from player: "
									+ playerName);
				}
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED + "Could not create " + uuid
								+ ".yml! Name: " + playerName);

			}
		}
		return dFile;

	}

	@SuppressWarnings("unused")
	public ArrayList<String> getPlayerNames() {
		ArrayList<String> files = getFiles();
		ArrayList<String> names = new ArrayList<String>();
		for (String playerFile : files) {
			String uuid = playerFile.replace(".yml", "");
			String playerName = Utils.getInstance().getPlayerName(uuid);
			if (Config.getInstance().getDebugEnabled()) {
				plugin.getLogger().info(uuid + ": " + playerName);
			}
			if (playerName != null) {
				names.add(playerName);
			}
		}
		if (names == null) {
			return null;
		} else {
			return names;
		}
	}

	@SuppressWarnings("unused")
	public ArrayList<String> getPlayersUUIDs() {
		ArrayList<String> files = getFiles();
		ArrayList<String> uuids = new ArrayList<String>();
		if (files.size() > 0) {
			for (String playerFile : files) {
				String uuid = playerFile.replace(".yml", "");
				uuids.add(uuid);
			}
			if (uuids == null) {
				return null;
			} else {
				return uuids;
			}
		} else {
			return null;
		}
	}

	public int getSiteMonthTotal(User user, String voteSiteName) {
		return getData(user).getInt(
				user.getUUID() + ".TotalMonth." + voteSiteName);
	}

	public long getTime(User user, String voteSite) {
		String uuid = user.getUUID();
		return getData(user).getLong(uuid + ".LastVote.Miliseconds");
	}

	public long getTimeAll(User user, String value) {
		String uuid = user.getUUID();
		return getData(user).getLong(uuid + ".LastBonus.Miliseconds");
	}

	public int getTimeAllOLD(User user, String value) {
		return getData(user).getInt(user.getUUID() + ".LastBonus." + value);
	}

	public int getTimeOLD(User user, String voteSite, String value) {
		return getData(user).getInt(
				user.getUUID() + ".LastVote." + voteSite + "." + value);
	}

	public int getTotal(User user, String voteSite) {
		return getData(user).getInt(user.getUUID() + ".Total." + voteSite);
	}

	public Set<User> getUsers() {
		Set<User> users = new HashSet<User>();
		ArrayList<String> players = getPlayerNames();
		if (players != null) {

			for (String playerName : players) {
				User user = new User(playerName);
				users.add(user);
			}
			return users;
		} else {
			return null;
		}
	}

	public boolean hasJoinedBefore(User user) {
		try {
			return getPlayersUUIDs().contains(user.getUUID());
		} catch (Exception ex) {
			return false;
		}
	}

	public void saveData(User user) {
		File dFile = getPlayerFile(user);
		String playerName = user.getPlayerName();
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer()
					.getLogger()
					.severe(ChatColor.RED + "Could not save "
							+ Utils.getInstance().getUUID(playerName) + ".yml!");
		}

	}

	public void set(User user, String path, Object value) {
		// String playerName = user.getPlayerName();
		String uuid = user.getUUID();
		File dFile = getPlayerFile(user);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save " + uuid + ".yml!");
		}
	}

	public void setBonusOfflineVotes(User user, int amount) {
		set(user, user.getUUID() + ".BonusOfflineVotes", amount);
	}

	public void setName(User user) {
		set(user, user.getUUID() + ".Name", user.getPlayerName());
	}

	public void setOfflineVotes(User user, String voteSite, int amount) {
		set(user, user.getUUID() + ".OfflineVotes." + voteSite, amount);
	}

	public void setSiteMonthTotal(User user, String voteSiteName, int amount) {
		set(user, user.getUUID() + ".TotalMonth." + voteSiteName, amount);
	}

	public void setTime(String siteName, User user) {
		String uuid = user.getUUID();
		set(user, uuid + ".LastVote." + siteName + ".Miliseconds",
				System.currentTimeMillis());
	}

	public void setTimeAll(User user) {
		String uuid = user.getUUID();
		set(user, uuid + ".LastBonus.Miliseconds", System.currentTimeMillis());
	}

	@SuppressWarnings("deprecation")
	public void setTimeAllOLD(User user) {
		int day = new Date().getDate();
		int month = new Date().getMonth();
		int hour = new Date().getHours();
		int min = new Date().getMinutes();
		int year = new Date().getYear();
		month++;
		String uuid = user.getUUID();
		set(user, uuid + ".LastBonus.Month", month);
		set(user, uuid + ".LastBonus.Day", day);
		set(user, uuid + ".LastBonus.Hour", hour);
		set(user, uuid + ".LastBonus.Min", min);
		set(user, uuid + ".LastBonus.Year", year);
	}

	@SuppressWarnings("deprecation")
	public void setTimeOLD(String siteName, User user) {
		int day = new Date().getDate();
		int month = new Date().getMonth();
		int hour = new Date().getHours();
		int min = new Date().getMinutes();
		int year = new Date().getYear();
		month++;
		String uuid = user.getUUID();
		set(user, uuid + ".LastVote." + siteName + ".Month", month);
		set(user, uuid + ".LastVote." + siteName + ".Day", day);
		set(user, uuid + ".LastVote." + siteName + ".Hour", hour);
		set(user, uuid + ".LastVote." + siteName + ".Min", min);
		set(user, uuid + ".LastVote." + siteName + ".Year", year);
	}

	public void setTotal(User user, String voteSite, int amount) {
		set(user, user.getUUID() + ".Total." + voteSite, amount);
	}

	public void setup(User user) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		String playerName = user.getPlayerName();
		String uuid = user.getUUID();

		File dFile = new File(plugin.getDataFolder() + File.separator + "Data",
				uuid + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				setName(user);
				if (Config.getInstance().getDebugEnabled()) {
					plugin.getLogger().info(
							"Created file: " + uuid + ".yml from player: "
									+ playerName);
				}
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create " + uuid
								+ ".yml! Name: " + playerName);

			}
		}

		// FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
	}
}
