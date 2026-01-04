package com.bencodez.votingplugin.config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.file.YMLFile;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigVoteSites.
 */
public class ConfigVoteSites extends YMLFile {

	private VotingPluginMain plugin;

	public ConfigVoteSites(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "VoteSites.yml"));
		setIgnoreCase(plugin.getConfigFile().isCaseInsensitiveYMLFiles());
		this.plugin = plugin;
	}

	/**
	 * Generate vote site.
	 *
	 * @param siteName the site name
	 */
	public void generateVoteSite(String siteName) {
		if (plugin.getConfigFile().isAutoCreateVoteSites()) {
			String org = siteName;
			siteName = siteName.replace(".", "_").replace(" ", "_");
			plugin.getLogger().warning("VoteSite " + siteName + " does not exist with the servicesite '" + org
					+ "', creating one, set AutoCreateVoteSites to false to prevent this");
			setEnabled(siteName, true);
			setServiceSite(siteName, org);
			setVoteURL(siteName, "VoteURL");
			setVoteDelay(siteName, 24);
			set(siteName, "DisplayItem.Material", "STONE");
			set(siteName, "DisplayItem.Amount", 1);
			set(siteName, "Rewards.Messages.Player", "&aThanks for voting on %ServiceSite%!");

			plugin.loadVoteSites();

			plugin.addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteSites." + siteName + ".Rewards") {

				@Override
				public void createSection(String key) {
					plugin.getConfigVoteSites().createSection(key);
				}

				@Override
				public ConfigurationSection getFileData() {
					return plugin.getConfigVoteSites().getData();
				}

				@Override
				public void save() {
					plugin.getConfigVoteSites().saveData();
				}

				@Override
				public void setData(String path, Object value) {
					plugin.getConfigVoteSites().setValue(path, value);
				}
			});

			plugin.addDirectlyDefinedRewards(
					new DirectlyDefinedReward("VoteSites." + siteName + ".CoolDownEndRewards") {

						@Override
						public void createSection(String key) {
							plugin.getConfigVoteSites().createSection(key);
						}

						@Override
						public ConfigurationSection getFileData() {
							return plugin.getConfigVoteSites().getData();
						}

						@Override
						public void save() {
							plugin.getConfigVoteSites().saveData();
						}

						@Override
						public void setData(String path, Object value) {
							plugin.getConfigVoteSites().setValue(path, value);
						}
					});

			for (Player p : Bukkit.getOnlinePlayers()) {
				if (p.hasPermission("VotingPlugin.Admin.GenerateServiceSite") || p.isOp()) {
					p.sendMessage(MessageAPI.colorize("&cGenerating votesite for service site " + siteName
							+ ", please check console for details"));
				}
			}
		}
	}

	/**
	 * Gets the data.
	 *
	 * @param siteName the site name
	 * @return the data
	 */
	public ConfigurationSection getData(String siteName) {
		if (!getData().isConfigurationSection("VoteSites." + siteName)) {
			plugin.getLogger().warning("VoteSites." + siteName + " is not a configuration section");
		}
		return getData().getConfigurationSection("VoteSites." + siteName);
	}

	public String getDisplayName(String site) {
		return getData(site).getString("Name");
	}

	public String getEverySiteRewardPath() {
		return "EverySiteReward";
	}

	public ConfigurationSection getItem(String site) {
		if (getData(site).isConfigurationSection("DisplayItem")) {
			return getData(site).getConfigurationSection("DisplayItem");
		}
		return getData(site).getConfigurationSection("Item");
	}

	public String getPermissionToView(String siteName) {
		return getData(siteName).getString("PermissionToView", "");
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
	public String getRewardsPath(String siteName) {
		return "VoteSites." + siteName + ".Rewards";
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
	public double getVoteDelay(String siteName) {
		return getData(siteName).getDouble("VoteDelay", 24);
	}

	public int getVoteDelayDailyHour(String siteName) {
		return getData(siteName).getInt("VoteDelayDailyHour", 0);
	}

	public double getVoteDelayMin(String siteName) {
		return getData(siteName).getDouble("VoteDelayMin", 0);
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
		File dFile = new File(plugin.getDataFolder() + File.separator + "VoteSites", siteName + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
			} catch (IOException e) {
				plugin.getLogger().severe(ChatColor.RED + "Could not create VoteSites/" + siteName + ".yml!");

			}
		}
		return dFile;

	}

	public boolean getVoteSiteGiveOffline(String site) {
		return getData(site).getBoolean("ForceOffline", getData(site).getBoolean("GiveOffline"));
	}

	public boolean getVoteSiteHidden(String siteName) {
		return getData(siteName).getBoolean("Hidden");
	}

	public boolean getVoteSiteIgnoreCanVote(String siteName) {
		return getData(siteName).getBoolean("IgnoreCanVote");
	}

	public boolean getVoteSiteResetVoteDelayDaily(String siteName) {
		return getData(siteName).getBoolean("VoteDelayDaily");
	}

	/**
	 * Gets the vote sites load.
	 *
	 * @return the vote sites load
	 */
	public ArrayList<VoteSite> getVoteSitesLoad() {
		ArrayList<VoteSite> voteSites = new ArrayList<>();
		ArrayList<String> voteSiteNames = getVoteSitesNames(true);
		if (voteSiteNames != null) {
			for (String site : voteSiteNames) {
				if (getVoteSiteEnabled(site) && !site.equalsIgnoreCase("null")) {
					if (!siteCheck(site)) {
						plugin.getLogger().warning("Failed to load site " + site + ", see above");
					} else {
						VoteSite voteSite = new VoteSite(plugin, site);
						plugin.debug(voteSite.loadingDebug());
						voteSites.add(voteSite);
					}
				}
			}
		}

		Collections.sort(voteSites, new Comparator<VoteSite>() {
			@Override
			public int compare(VoteSite v1, VoteSite v2) {
				int v1P = v1.getPriority();
				int v2P = v2.getPriority();

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

	public ArrayList<String> getVoteSitesNames(boolean checkEnabled) {
		ArrayList<String> siteNames = new ArrayList<>();
		if (getData().isConfigurationSection("VoteSites")) {
			siteNames = ArrayUtils.convert(getData().getConfigurationSection("VoteSites").getKeys(false));
		}

		for (int i = siteNames.size() - 1; i >= 0; i--) {
			// plugin.getLogger().info(siteNames.get(i));
			if (!getData().isConfigurationSection("VoteSites." + siteNames.get(i))) {
				plugin.getLogger()
						.warning("VoteSites." + siteNames.get(i) + " is not a configuration section, please remove");
				siteNames.remove(i);
			} else if (siteNames.get(i).equalsIgnoreCase("null")
					|| (!getVoteSiteEnabled(siteNames.get(i)) && checkEnabled) || !siteCheck(siteNames.get(i))) {
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
		return getData(siteName).getString("VoteURL", "");
	}

	public boolean getWaitUntilVoteDelay(String siteName) {
		return getData(siteName).getBoolean("WaitUntilVoteDelay", false);
	}

	/**
	 * Checks if is service site good.
	 *
	 * @param siteName the site name
	 * @return true, if is service site good
	 */
	public boolean isServiceSiteGood(String siteName) {
		if (getServiceSite(siteName) == null || getServiceSite(siteName).equals("")) {
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
		if (getVoteURL(siteName) == null || getVoteURL(siteName).equals("")) {
			return false;
		}
		return true;
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("VoteSites.yml", true);

	}

	/**
	 * Rename vote site.
	 *
	 * @param siteName the site name
	 * @param newName  the new name
	 * @return true, if successful
	 */
	public boolean renameVoteSite(String siteName, String newName) {
		return getVoteSiteFile(siteName)
				.renameTo(new File(plugin.getDataFolder() + File.separator + "VoteSites", newName + ".yml"));
	}

	/**
	 * Sets the.
	 *
	 * @param siteName the site name
	 * @param path     the path
	 * @param value    the value
	 */
	public void set(String siteName, String path, Object value) {
		// String playerName = user.getPlayerName();
		ConfigurationSection data = getData(siteName);
		if (data == null) {
			getData().createSection("VoteSites." + siteName);
			data = getData(siteName);
		}
		data.set(path, value);
		saveData();
	}

	/**
	 * Sets the cumulative rewards.
	 *
	 * @param siteName the site name
	 * @param value    the value
	 */
	public void setCumulativeRewards(String siteName, ArrayList<String> value) {
		set(siteName, "Cumulative.Rewards", value);
	}

	public void setCumulativeVotes(String siteName, int value) {
		set(siteName, "Cumulative.Votes", value);
	}

	public void setDisplayName(String siteName, String value) {
		set(siteName, "Name", value);
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

	public void setForceOffline(String siteName, boolean value) {
		set(siteName, "ForceOffline", value);

	}

	/**
	 * Sets the priority.
	 *
	 * @param siteName the site name
	 * @param value    the value
	 */
	public void setPriority(String siteName, int value) {
		set(siteName, "Priority", value);
	}

	/**
	 * Sets the rewards.
	 *
	 * @param siteName the site name
	 * @param value    the value
	 */
	public void setRewards(String siteName, ArrayList<String> value) {
		set(siteName, "Rewards", value);
	}

	/**
	 * Sets the service site.
	 *
	 * @param siteName    the site name
	 * @param serviceSite the service site
	 */
	public void setServiceSite(String siteName, String serviceSite) {
		set(siteName, "ServiceSite", serviceSite);
	}

	/**
	 * Sets the vote delay.
	 *
	 * @param siteName  the site name
	 * @param voteDelay the vote delay
	 */
	public void setVoteDelay(String siteName, double voteDelay) {
		set(siteName, "VoteDelay", voteDelay);
	}

	public void setVoteDelayDaily(String siteName, boolean value) {
		set(siteName, "VoteDelayDaily", value);
	}

	/**
	 * Sets the vote URL.
	 *
	 * @param siteName the site name
	 * @param url      the url
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
			plugin.getLogger().warning("Issue with ServiceSite in site " + siteName + ", votes may not work properly");
			pass = false;
		}
		if (!isVoteURLGood(siteName)) {
			plugin.getLogger().warning("Issue with VoteURL in site " + siteName);
		}
		return pass;
	}

}
