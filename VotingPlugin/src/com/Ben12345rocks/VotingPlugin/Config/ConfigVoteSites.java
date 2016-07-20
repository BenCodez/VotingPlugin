package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Files.Files;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigVoteSites.
 */
public class ConfigVoteSites {

	/** The instance. */
	static ConfigVoteSites instance = new ConfigVoteSites();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigVoteSites.
	 *
	 * @return single instance of ConfigVoteSites
	 */
	public static ConfigVoteSites getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new config vote sites.
	 */
	private ConfigVoteSites() {
	}

	/**
	 * Instantiates a new config vote sites.
	 *
	 * @param plugin the plugin
	 */
	public ConfigVoteSites(Main plugin) {
		ConfigVoteSites.plugin = plugin;
	}

	/**
	 * Generate vote site.
	 *
	 * @param siteName the site name
	 */
	public void generateVoteSite(String siteName) {
		plugin.getLogger().warning(
				"VoteSite " + siteName
				+ " doe not exist, generaterating one...");
		setEnabled(siteName, false);
		setServiceSite(siteName, "Enter Service Site");
		setVoteURL(siteName, "VoteURL");
		setVoteDelay(siteName, 24);
		setRewards(siteName, new ArrayList<String>());

		plugin.loadVoteSites();
		plugin.getLogger()
		.info("Created file VoteSites/"
				+ siteName
				+ ".yml! Loaded default values into file, remember to turn Enabled to true, else it won't be read by the plugin");
	}

	/**
	 * Gets the cumulative rewards.
	 *
	 * @param siteName the site name
	 * @return the cumulative rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCumulativeRewards(String siteName) {
		try {
			return (ArrayList<String>) getData(siteName).getList(
					"Cumulative.Rewards");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the cumulative reward votes amount.
	 *
	 * @param siteName the site name
	 * @return the cumulative reward votes amount
	 */
	public int getCumulativeRewardVotesAmount(String siteName) {
		return getData(siteName).getInt("Cumulative.Votes");
	}

	/**
	 * Gets the data.
	 *
	 * @param siteName the site name
	 * @return the data
	 */
	public FileConfiguration getData(String siteName) {
		File dFile = getVoteSiteFile(siteName);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	/**
	 * Gets the priority.
	 *
	 * @param siteName the site name
	 * @return the priority
	 */
	public int getPriority(String siteName) {
		return getData(siteName).getInt("Priority");
	}

	/**
	 * Gets the rewards.
	 *
	 * @param siteName the site name
	 * @return the rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getRewards(String siteName) {
		try {
			return (ArrayList<String>) getData(siteName).getList("Rewards");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the service site.
	 *
	 * @param siteName the site name
	 * @return the service site
	 */
	public String getServiceSite(String siteName) {
		return getData(siteName).getString("ServiceSite");
	}

	/**
	 * Gets the vote delay.
	 *
	 * @param siteName the site name
	 * @return the vote delay
	 */
	public int getVoteDelay(String siteName) {
		return getData(siteName).getInt("VoteDelay");
	}

	/**
	 * Gets the vote site enabled.
	 *
	 * @param siteName the site name
	 * @return the vote site enabled
	 */
	public boolean getVoteSiteEnabled(String siteName) {
		return getData(siteName).getBoolean("Enabled");
	}

	/**
	 * Gets the vote site file.
	 *
	 * @param siteName the site name
	 * @return the vote site file
	 */
	public File getVoteSiteFile(String siteName) {
		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites", siteName + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create VoteSites/"
								+ siteName + ".yml!");

			}
		}
		return dFile;

	}

	/**
	 * Gets the vote sites.
	 *
	 * @return the vote sites
	 */
	public ArrayList<VoteSite> getVoteSites() {
		if (plugin.voteSites != null) {
			return plugin.voteSites;
		} else {
			plugin.loadVoteSites();
			return plugin.voteSites;
		}
	}

	/**
	 * Gets the vote sites files.
	 *
	 * @return the vote sites files
	 */
	public ArrayList<String> getVoteSitesFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	/**
	 * Gets the vote sites load.
	 *
	 * @return the vote sites load
	 */
	public ArrayList<VoteSite> getVoteSitesLoad() {
		ArrayList<VoteSite> voteSites = new ArrayList<VoteSite>();
		ArrayList<String> voteSiteNames = getVoteSitesNames();
		if (voteSiteNames != null) {
			for (String site : voteSiteNames) {
				if (!site.equalsIgnoreCase("Example")
						&& getVoteSiteEnabled(site)
						&& !site.equalsIgnoreCase("null")) {
					if (!siteCheck(site)) {
						plugin.getLogger().warning(
								"Failed to load site " + site + ", see above");
					} else {
						voteSites.add(new VoteSite(site));
					}
				}
			}
		}

		Collections.sort(voteSites, new Comparator<VoteSite>() {
			@Override
			public int compare(VoteSite v1, VoteSite v2) {
				int v1P = getPriority(v1.getSiteName());
				int v2P = getPriority(v2.getSiteName());

				if (v1P < v2P) {
					return 1;
				}
				if (v1P > v2P) {
					return -1;
				}

				return 0;
			}
		});

		return voteSites;
	}

	/**
	 * Gets the vote sites names.
	 *
	 * @return the vote sites names
	 */
	public ArrayList<String> getVoteSitesNames() {
		ArrayList<String> siteNames = getVoteSitesFiles();
		if (siteNames == null) {
			return null;
		}
		for (int i = 0; i < siteNames.size(); i++) {
			siteNames.set(i, siteNames.get(i).replace(".yml", ""));
		}
		for (int i = siteNames.size() - 1; i >= 0; i--) {
			// plugin.getLogger().info(siteNames.get(i));
			if (!getVoteSiteEnabled(siteNames.get(i))
					|| siteNames.get(i).equalsIgnoreCase("Example")
					|| siteNames.get(i).equalsIgnoreCase("null")
					|| !siteCheck(siteNames.get(i))) {
				// plugin.getLogger().info("Removed: " + siteNames.get(i));
				siteNames.remove(i);

			}

		}

		return siteNames;
	}

	/**
	 * Gets the vote URL.
	 *
	 * @param siteName the site name
	 * @return the vote URL
	 */
	public String getVoteURL(String siteName) {
		return getData(siteName).getString("VoteURL");
	}

	/**
	 * Checks if is service site good.
	 *
	 * @param siteName the site name
	 * @return true, if is service site good
	 */
	public boolean isServiceSiteGood(String siteName) {
		if (getServiceSite(siteName) == null) {
			return false;
		} else if (getServiceSite(siteName).equalsIgnoreCase("")) {
			return false;
		}
		return true;
	}

	/**
	 * Checks if is vote URL good.
	 *
	 * @param siteName the site name
	 * @return true, if is vote URL good
	 */
	public boolean isVoteURLGood(String siteName) {
		if (getVoteURL(siteName) == null) {
			return false;
		} else if (getVoteURL(siteName).equalsIgnoreCase("")) {
			return false;
		}
		return true;
	}

	/**
	 * Rename vote site.
	 *
	 * @param siteName the site name
	 * @param newName the new name
	 * @return true, if successful
	 */
	public boolean renameVoteSite(String siteName, String newName) {
		return getVoteSiteFile(siteName).renameTo(
				new File(plugin.getDataFolder() + File.separator + "VoteSites",
						newName + ".yml"));
	}

	/**
	 * Sets the.
	 *
	 * @param siteName the site name
	 * @param path the path
	 * @param value the value
	 */
	public void set(String siteName, String path, Object value) {
		// String playerName = user.getPlayerName();
		File dFile = getVoteSiteFile(siteName);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		Files.getInstance().editFile(dFile, data);
	}

	/**
	 * Sets the cumulative rewards.
	 *
	 * @param siteName the site name
	 * @param value the value
	 */
	public void setCumulativeRewards(String siteName, ArrayList<String> value) {
		set(siteName, "Cumulative.Rewards", value);
	}

	/**
	 * Sets the enabled.
	 *
	 * @param siteName the site name
	 * @param disabled the disabled
	 */
	public void setEnabled(String siteName, boolean disabled) {
		set(siteName, "Enabled", disabled);
	}

	/**
	 * Sets the priority.
	 *
	 * @param siteName the site name
	 * @param value the value
	 */
	public void setPriority(String siteName, int value) {
		set(siteName, "Priority", value);
	}

	/**
	 * Sets the rewards.
	 *
	 * @param siteName the site name
	 * @param value the value
	 */
	public void setRewards(String siteName, ArrayList<String> value) {
		set(siteName, "Rewards", value);
	}

	/**
	 * Sets the service site.
	 *
	 * @param siteName the site name
	 * @param serviceSite the service site
	 */
	public void setServiceSite(String siteName, String serviceSite) {
		set(siteName, "ServiceSite", serviceSite);
	}

	/**
	 * Sets the up.
	 *
	 * @param siteName the new up
	 */
	public void setup(String siteName) {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "VoteSites", siteName + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
				if (siteName.equalsIgnoreCase("ExampleVoteSite")) {
					plugin.saveResource("VoteSites" + File.separator
							+ "ExampleVoteSite.yml", true);
				}
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create VoteSites/"
								+ siteName + ".yml!");
			}
		}
	}

	/**
	 * Sets the vote delay.
	 *
	 * @param siteName the site name
	 * @param voteDelay the vote delay
	 */
	public void setVoteDelay(String siteName, int voteDelay) {
		set(siteName, "VoteDelay", voteDelay);
	}

	/**
	 * Sets the vote URL.
	 *
	 * @param siteName the site name
	 * @param url the url
	 */
	public void setVoteURL(String siteName, String url) {
		set(siteName, "VoteURL", url);
	}

	/**
	 * Site check.
	 *
	 * @param siteName the site name
	 * @return true, if successful
	 */
	public boolean siteCheck(String siteName) {
		boolean pass = true;
		if (!isServiceSiteGood(siteName)) {
			plugin.getLogger().warning(
					"Issue with ServiceSite in site " + siteName
					+ ", votes may not work properly");
			pass = false;
		}
		if (!isVoteURLGood(siteName)) {
			plugin.getLogger()
			.warning("Issue with VoteURL in site " + siteName);
		}
		return pass;
	}

}
