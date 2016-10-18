package com.Ben12345rocks.VotingPlugin.Data;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.AdvancedCore.Util.Files.FilesManager;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;

// TODO: Auto-generated Javadoc
/**
 * The Class ServerData.
 */
public class ServerDataOld {

	/** The instance. */
	static ServerDataOld instance = new ServerDataOld();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ServerData.
	 *
	 * @return single instance of ServerData
	 */
	public static ServerDataOld getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new server data.
	 */
	private ServerDataOld() {
	}

	/**
	 * Instantiates a new server data.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public ServerDataOld(Main plugin) {
		ServerDataOld.plugin = plugin;
	}

	public void convert() {
		setup(plugin);
		for (String sign : getSigns()) {
			ServerData.getInstance().addSign(getSignLocation(sign),
					getSignData(sign), getSignPosition(sign));
			ServerData.getInstance().setPrevDay(getPrevDay());
			ServerData.getInstance().setPrevMonth(getPrevMonth());
			ServerData.getInstance().setPrevWeekDay(getPrevWeekDay());
		}
	}

	/**
	 * Adds the sign.
	 *
	 * @param location
	 *            the location
	 * @param data
	 *            the data
	 * @param position
	 *            the position
	 */
	public void addSign(Location location, String data, int position) {

		int count = nextSignNumber();

		getData().set("Signs." + count + ".World",
				location.getWorld().getName());
		getData().set("Signs." + count + ".X", location.getBlockX());
		getData().set("Signs." + count + ".Y", location.getBlockY());
		getData().set("Signs." + count + ".Z", location.getBlockZ());
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		saveData();
		plugin.signs.add(new SignHandler("" + count,
				getSignLocation("" + count), getSignData("" + count),
				getSignPosition("" + count)));
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
	 * Gets the prev day.
	 *
	 * @return the prev day
	 */
	public int getPrevDay() {
		return getData().getInt("PrevDay");
	}

	/**
	 * Gets the prev month.
	 *
	 * @return the prev month
	 */
	public int getPrevMonth() {
		return getData().getInt("PrevMonth");
	}

	/**
	 * Gets the prev week day.
	 *
	 * @return the prev week day
	 */
	public int getPrevWeekDay() {
		return getData().getInt("PrevWeek");
	}

	/**
	 * Gets the sign data.
	 *
	 * @param sign
	 *            the sign
	 * @return the sign data
	 */
	public String getSignData(String sign) {
		return getData().getString("Signs." + sign + ".Data");
	}

	/**
	 * Gets the sign location.
	 *
	 * @param sign
	 *            the sign
	 * @return the sign location
	 */
	public Location getSignLocation(String sign) {
		return new Location(Bukkit.getWorld(getData().getString(
				"Signs." + sign + ".World")), getData().getDouble(
				"Signs." + sign + ".X"), getData().getDouble(
				"Signs." + sign + ".Y"), getData().getDouble(
				"Signs." + sign + ".Z"));
	}

	/**
	 * Gets the sign position.
	 *
	 * @param sign
	 *            the sign
	 * @return the sign position
	 */
	public int getSignPosition(String sign) {
		return getData().getInt("Signs." + sign + ".Position");
	}

	/**
	 * Gets the signs.
	 *
	 * @return the signs
	 */
	public Set<String> getSigns() {
		try {
			return getData().getConfigurationSection("Signs").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Next sign number.
	 *
	 * @return the int
	 */
	public int nextSignNumber() {
		Set<String> signs = getSigns();

		if (signs != null) {
			for (int i = 0; i < 100000; i++) {
				if (!signs.contains(Integer.toString(i))) {
					return i;
				}
			}
		}
		return 0;
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	/**
	 * Removes the sign.
	 *
	 * @param sign
	 *            the sign
	 */
	public void removeSign(String sign) {
		getData().set("Signs." + sign + ".World", null);
		getData().set("Signs." + sign + ".X", null);
		getData().set("Signs." + sign + ".Y", null);
		getData().set("Signs." + sign + ".Z", null);
		getData().set("Signs." + sign + ".Data", null);
		getData().set("Signs." + sign + ".Position", null);
		getData().set("Signs." + sign, null);
		saveData();
	}

	/**
	 * Save data.
	 */
	public synchronized void saveData() {
		FilesManager.getInstance().editFile(dFile, data);
	}

	/**
	 * Sets the plugin version.
	 */
	public void setPluginVersion() {
		getData().set("PluginVersion", plugin.getDescription().getVersion());
		saveData();
	}

	/**
	 * Sets the prev day.
	 *
	 * @param day
	 *            the new prev day
	 */
	public void setPrevDay(int day) {
		getData().set("PrevDay", day);
		saveData();
	}

	/**
	 * Sets the prev month.
	 *
	 * @param value
	 *            the new prev month
	 */
	public void setPrevMonth(int value) {
		getData().set("PrevMonth", value);
		saveData();
	}

	/**
	 * Sets the prev week day.
	 *
	 * @param week
	 *            the new prev week day
	 */
	public void setPrevWeekDay(int week) {
		getData().set("PrevWeek", week);
		saveData();
	}

	/**
	 * Sets the sign.
	 *
	 * @param count
	 *            the count
	 * @param location
	 *            the location
	 * @param data
	 *            the data
	 * @param position
	 *            the position
	 */
	public void setSign(String count, Location location, String data,
			int position) {

		getData().set("Signs." + count + ".World",
				location.getWorld().getName());
		int x = (int) location.getX();
		int z = (int) location.getZ();
		getData().set("Signs." + count + ".X", x);
		getData().set("Signs." + count + ".Y", location.getY());
		getData().set("Signs." + count + ".Z", z);
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		saveData();
	}

	/**
	 * Sets the up.
	 *
	 * @param p
	 *            the new up
	 */
	@SuppressWarnings("deprecation")
	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "ServerData.yml");

		boolean genFile = false;

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				genFile = true;
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED
								+ "Could not create ServerData.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
		data.options().header("DO NOT EDIT THIS FILE MANUALLY");
		if (genFile) {
			java.util.TimeZone tz = java.util.TimeZone.getTimeZone(ConfigFormat
					.getInstance().getTimeZone());
			java.util.Calendar c = java.util.Calendar.getInstance(tz);
			setPrevMonth(c.getTime().getMonth());
			int day = c.getTime().getDay();
			ServerDataOld.getInstance().setPrevDay(day);
			ServerDataOld.getInstance().setPrevWeekDay(c.getTime().getDate());
		}
		saveData();
	}

	/**
	 * Sets the version.
	 */
	public void setVersion() {
		getData().set("Version", Bukkit.getVersion());
		saveData();
	}

	/**
	 * Update values.
	 */
	public void updateValues() {
		setVersion();
		setPluginVersion();
	}
}
