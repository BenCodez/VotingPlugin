package com.Ben12345rocks.VotingPlugin.Data;

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

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Util.Files.FilesManager;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class Data.
 */
@Deprecated
public class OldData {

	/** The instance. */
	static OldData instance = new OldData();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Data.
	 *
	 * @return single instance of Data
	 */
	public static OldData getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new data.
	 */
	private OldData() {
	}

	/**
	 * Instantiates a new data.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public OldData(Main plugin) {
		Data.plugin = plugin;
	}

	/**
	 * Adds the cumulative site.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 */
	public void addCumulativeSite(User user, String voteSite) {
		setCumulativeSite(user, voteSite, getCumulativeSite(user, voteSite) + 1);
	}

	/**
	 * Adds the total.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotal(User user, String voteSite) {
		set(user, "Total." + voteSite, getTotal(user, voteSite) + 1);
	}

	/**
	 * Gets the all sites offline.
	 *
	 * @param user
	 *            the user
	 * @return the all sites offline
	 */
	public int getAllSitesOffline(User user) {
		return getData(user).getInt("OtherRewards.AllSites");
	}

	/**
	 * Gets the cumulative site.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the cumulative site
	 */
	public int getCumulativeSite(User user, String voteSite) {
		return getData(user).getInt("Cumulative." + voteSite);
	}

	/**
	 * Gets the cumulative votes offline.
	 *
	 * @param user
	 *            the user
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative votes offline
	 */
	public int getCumulativeVotesOffline(User user, int cumulative) {
		return getData(user).getInt("OtherRewards.Cumulative." + cumulative);
	}

	/**
	 * Gets the data.
	 *
	 * @param user
	 *            the user
	 * @return the data
	 */
	public FileConfiguration getData(User user) {
		File dFile = getPlayerFile(user);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	/**
	 * Gets the files.
	 *
	 * @return the files
	 */
	public ArrayList<String> getFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator + "Data");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	/**
	 * Gets the first vote offline.
	 *
	 * @param user
	 *            the user
	 * @return the first vote offline
	 */
	public int getFirstVoteOffline(User user) {
		return getData(user).getInt("OtherRewards.FirstVote");
	}

	/**
	 * Gets the checks for gotten first reward.
	 *
	 * @param user
	 *            the user
	 * @return the checks for gotten first reward
	 */
	public boolean getHasGottenFirstReward(User user) {
		return getData(user).getBoolean("FirstVoteGotten");
	}

	/**
	 * Gets the name.
	 *
	 * @param user
	 *            the user
	 * @return the name
	 */
	public String getName(User user) {
		return getData(user).getString("Name");
	}

	/**
	 * Gets the offline reward.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @return the offline reward
	 */
	public int getOfflineReward(User user, Reward reward) {
		return getData(user).getInt("OfflineRewards." + reward.getRewardName());
	}

	/**
	 * Gets the offline votes site.
	 *
	 * @param user
	 *            the user
	 * @param siteName
	 *            the site name
	 * @return the offline votes site
	 */
	public int getOfflineVotesSite(User user, String siteName) {
		return getData(user).getInt("OfflineVotes." + siteName);
	}

	/**
	 * Gets the offline votes site world.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @param world
	 *            the world
	 * @return the offline votes site world
	 */
	public int getOfflineVotesSiteWorld(User user, String reward, String world) {
		if (world == null) {
			world = "AllTheWorlds";
		}
		return getData(user)
				.getInt("OfflineVotesWorld." + reward + "." + world);
	}

	/**
	 * Gets the player file.
	 *
	 * @param user
	 *            the user
	 * @return the player file
	 */
	public File getPlayerFile(User user) {
		String playerName = user.getPlayerName();
		String uuid = user.getUUID();
		// plugin.debug(playerName + ":" + uuid);
		// plugin.debug(plugin.toString());
		File dFile = new File(plugin.getDataFolder() + File.separator + "Data",
				uuid + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				setPlayerName(user);

				plugin.debug("Created file: " + uuid + ".yml from player: "
						+ playerName);

			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED + "Could not create " + uuid
								+ ".yml! Name: " + playerName);

			}
		}
		return dFile;

	}

	/**
	 * Gets the player names.
	 *
	 * @return the player names
	 */
	public ArrayList<String> getPlayerNames() {
		ArrayList<String> files = getFiles();
		ArrayList<String> names = new ArrayList<String>();
		if (files != null) {
			for (String playerFile : files) {
				String uuid = playerFile.replace(".yml", "");
				String playerName = Utils.getInstance().getPlayerName(uuid);
				if (playerName != null) {
					names.add(playerName);
				}
			}
			Set<String> namesSet = new HashSet<String>(names);
			names = Utils.getInstance().convert(namesSet);
			if (names == null) {
				return null;
			} else {
				return names;
			}
		}
		return null;
	}

	/**
	 * Gets the players UUI ds.
	 *
	 * @return the players UUI ds
	 */
	@SuppressWarnings("unused")
	public ArrayList<String> getPlayersUUIDs() {
		ArrayList<String> files = getFiles();
		if (files != null) {
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
			}
		}
		return null;

	}

	/**
	 * Gets the reminded.
	 *
	 * @param user
	 *            the user
	 * @return the reminded
	 */
	public boolean getReminded(User user) {
		return getData(user).getBoolean("Reminded");
	}

	/**
	 * Gets the timed reward.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @return the timed reward
	 */
	public long getTimedReward(User user, String reward) {
		return getData(user).getLong("Timed." + reward);
	}

	/**
	 * Gets the time site.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the time site
	 */
	public long getTimeSite(User user, String voteSite) {
		return getData(user).getLong("LastVote." + voteSite + ".Miliseconds");

	}

	/**
	 * Gets the top voter award offline.
	 *
	 * @param user
	 *            the user
	 * @return the top voter award offline
	 */
	public int getTopVoterAwardOffline(User user) {
		return getData(user).getInt(
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth());
	}

	/**
	 * Gets the top voter award offline daily.
	 *
	 * @param user
	 *            the user
	 * @param date
	 *            the date
	 * @return the top voter award offline daily
	 */
	public int getTopVoterAwardOfflineDaily(User user, int date) {
		return getData(user).getInt(
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth() + "." + date);
	}

	/**
	 * Gets the top voter award offline weekly.
	 *
	 * @param user
	 *            the user
	 * @param day
	 *            the day
	 * @return the top voter award offline weekly
	 */
	public int getTopVoterAwardOfflineWeekly(User user, int day) {
		return getData(user).getInt(
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth() + "." + day);
	}

	/**
	 * Gets the total.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the total
	 */
	public int getTotal(User user, String voteSite) {
		return getData(user).getInt("Total." + voteSite);
	}

	/**
	 * Gets the total daily.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the total daily
	 */
	public int getTotalDaily(User user, String voteSite) {
		return getData(user).getInt("TotalDay." + voteSite);
	}

	/**
	 * Gets the total week.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the total week
	 */
	public int getTotalWeek(User user, String voteSite) {
		return getData(user).getInt("TotalWeek." + voteSite);
	}

	/**
	 * Gets the users.
	 *
	 * @return the users
	 */
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
			return new HashSet<User>();
		}
	}

	/**
	 * Gets the voting points.
	 *
	 * @param user
	 *            the user
	 * @return the voting points
	 */
	public int getVotingPoints(User user) {
		return getData(user).getInt("Points");
	}

	/**
	 * Checks for joined before.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean hasJoinedBefore(User user) {
		try {
			return getPlayersUUIDs().contains(user.getUUID());
		} catch (Exception ex) {
			return false;
		}
	}

	/**
	 * Save data.
	 *
	 * @param user
	 *            the user
	 */
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

	/**
	 * Sets the.
	 *
	 * @param user
	 *            the user
	 * @param path
	 *            the path
	 * @param value
	 *            the value
	 */
	public void set(User user, String path, Object value) {
		File dFile = getPlayerFile(user);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		FilesManager.getInstance().editFile(dFile, data);
	}

	/**
	 * Sets the all sites offline.
	 *
	 * @param user
	 *            the user
	 * @param value
	 *            the value
	 */
	public void setAllSitesOffline(User user, int value) {
		set(user, "OtherRewards.AllSites", value);
	}

	/**
	 * Sets the cumuatlive votes offline.
	 *
	 * @param user
	 *            the user
	 * @param cumulative
	 *            the cumulative
	 * @param value
	 *            the value
	 */
	public void setCumuatliveVotesOffline(User user, int cumulative, int value) {
		set(user, "OtherRewards.Cumulative." + cumulative, value);
	}

	/**
	 * Sets the cumulative site.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setCumulativeSite(User user, String voteSite, int amount) {
		set(user, "Cumulative." + voteSite, amount);
	}

	/**
	 * Sets the first vote offline.
	 *
	 * @param user
	 *            the user
	 * @param value
	 *            the value
	 */
	public void setFirstVoteOffline(User user, int value) {
		set(user, "OtherRewards.FirstVote", value);
	}

	/**
	 * Sets the has gotten first reward.
	 *
	 * @param user
	 *            the user
	 * @param value
	 *            the value
	 */
	public void setHasGottenFirstReward(User user, boolean value) {
		set(user, "FirstVoteGotten", value);
	}

	/**
	 * Sets the offline reward.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setOfflineReward(User user, Reward reward, int value) {
		set(user, "OfflineRewards." + reward.getRewardName(), value);
	}

	/**
	 * Sets the offline votes site.
	 *
	 * @param user
	 *            the user
	 * @param siteName
	 *            the site name
	 * @param value
	 *            the value
	 */
	public void setOfflineVotesSite(User user, String siteName, int value) {
		set(user, "OfflineVotes." + siteName, value);
	}

	/**
	 * Sets the offline votes site world.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @param world
	 *            the world
	 * @param value
	 *            the value
	 */
	public void setOfflineVotesSiteWorld(User user, String reward,
			String world, int value) {
		if (world == null) {
			world = "AllTheWorlds";
		}
		set(user, "OfflineVotesWorld." + reward + "." + world, value);
	}

	/**
	 * Sets the player name.
	 *
	 * @param user
	 *            the new player name
	 */
	public void setPlayerName(User user) {
		set(user, "PlayerName", user.getPlayerName());
	}

	/**
	 * Sets the reminded.
	 *
	 * @param user
	 *            the user
	 * @param value
	 *            the value
	 */
	public void setReminded(User user, boolean value) {
		set(user, "Reminded", value);
	}

	/**
	 * Sets the time.
	 *
	 * @param siteName
	 *            the site name
	 * @param user
	 *            the user
	 */
	public void setTime(String siteName, User user) {
		set(user, "LastVote." + siteName + ".Miliseconds",
				System.currentTimeMillis());
	}

	/**
	 * Sets the timed reward.
	 *
	 * @param user
	 *            the user
	 * @param reward
	 *            the reward
	 * @param time
	 *            the time
	 */
	public void setTimedReward(User user, String reward, long time) {
		set(user, "Timed." + reward, time);
	}

	/*
	 * public int getVotesBonusReward(User user) { return
	 * getData(user).getInt("BonusVotes"); }
	 * 
	 * public void setVotesBonusReward(User user, int value) { set(user,
	 * "BonusVotes", value); }
	 */

	/**
	 * Sets the time mill.
	 *
	 * @param siteName
	 *            the site name
	 * @param user
	 *            the user
	 * @param mill
	 *            the mill
	 */
	public void setTimeMill(String siteName, User user, Long mill) {
		set(user, "LastVote." + siteName + ".Miliseconds", mill);
	}

	/**
	 * Sets the time site.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setTimeSite(User user, String voteSite, int value) {
		set(user, "LastVote." + voteSite + ".Miliseconds", value);

	}

	/**
	 * Sets the top voter award offline.
	 *
	 * @param user
	 *            the user
	 * @param place
	 *            the place
	 */
	public void setTopVoterAwardOffline(User user, int place) {
		set(user,
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth(), place);
	}

	/**
	 * Sets the top voter award offline daily.
	 *
	 * @param user
	 *            the user
	 * @param place
	 *            the place
	 */
	public void setTopVoterAwardOfflineDaily(User user, int place) {
		set(user,
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth() + "." + new Date().getDate(),
				place);
	}

	/**
	 * Sets the top voter award offline weekly.
	 *
	 * @param user
	 *            the user
	 * @param place
	 *            the place
	 */
	public void setTopVoterAwardOfflineWeekly(User user, int place) {
		set(user,
				"TopVoter." + new Date().getYear() + "."
						+ new Date().getMonth() + "." + new Date().getDay(),
				place);
	}

	/**
	 * Sets the total.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotal(User user, String voteSite, int amount) {
		set(user, "Total." + voteSite, amount);
	}

	/**
	 * Sets the total daily.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotalDaily(User user, String voteSite, int amount) {
		set(user, "TotalDay." + voteSite, amount);
	}

	/**
	 * Sets the total week.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotalWeek(User user, String voteSite, int amount) {
		set(user, "TotalWeek." + voteSite, amount);
	}

	/**
	 * Sets the up.
	 *
	 * @param user
	 *            the new up
	 */
	public void setup(User user) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		String uuid = user.getUUID();
		String playerName = user.getPlayerName();
		if (playerName == null) {
			Utils.getInstance().getPlayerName(uuid);
		}

		if (playerName == null) {
			return;
		}

		File dFile = new File(plugin.getDataFolder() + File.separator + "Data",
				uuid + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				setPlayerName(user);

				plugin.debug("Created file: " + uuid + ".yml from player: "
						+ playerName);

			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create " + uuid
								+ ".yml! Name: " + playerName);

			}
		}
	}

	/**
	 * Sets the voting points.
	 *
	 * @param user
	 *            the user
	 * @param value
	 *            the value
	 */
	public void setVotingPoints(User user, int value) {
		set(user, "Points", value);
	}
}