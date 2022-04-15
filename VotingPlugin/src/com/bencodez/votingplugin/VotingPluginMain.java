package com.bencodez.votingplugin;

import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.security.CodeSource;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.bukkit.permissions.Permission;
import org.bukkit.permissions.PermissionDefault;
import org.bukkit.plugin.PluginManager;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.javascript.JavascriptPlaceholderRequest;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardEditData;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardPlaceholderHandle;
import com.bencodez.advancedcore.api.rewards.injected.RewardInject;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectInt;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectValidator;
import com.bencodez.advancedcore.api.skull.SkullHandler;
import com.bencodez.advancedcore.api.updater.Updater;
import com.bencodez.advancedcore.logger.Logger;
import com.bencodez.advancedcore.nms.NMSManager;
import com.bencodez.votingplugin.broadcast.BroadcastHandler;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.commands.executers.CommandAdminVote;
import com.bencodez.votingplugin.commands.executers.CommandVote;
import com.bencodez.votingplugin.commands.gui.AdminGUI;
import com.bencodez.votingplugin.commands.tabcompleter.AdminVoteTabCompleter;
import com.bencodez.votingplugin.commands.tabcompleter.VoteTabCompleter;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.listeners.BlockBreak;
import com.bencodez.votingplugin.listeners.PlayerInteract;
import com.bencodez.votingplugin.listeners.PlayerJoinEvent;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;
import com.bencodez.votingplugin.listeners.SignChange;
import com.bencodez.votingplugin.listeners.VotiferEvent;
import com.bencodez.votingplugin.listeners.VotingPluginUpdateEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.placeholders.MVdWPlaceholders;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterHandler;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.updater.CheckUpdate;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteparty.VoteParty;
import com.bencodez.votingplugin.votereminding.VoteReminding;

import lombok.Getter;
import lombok.Setter;

/**
 * The Class Main.
 */
public class VotingPluginMain extends AdvancedCorePlugin {

	@Getter
	public static VotingPluginMain plugin;

	@Getter
	@Setter
	private ArrayList<CommandHandler> adminVoteCommand;

	@Getter
	private LinkedHashMap<java.util.UUID, ArrayList<String>> advancedTab = new LinkedHashMap<java.util.UUID, ArrayList<String>>();

	@Getter
	private BroadcastHandler broadcastHandler;

	@Getter
	private BungeeHandler bungeeHandler;

	@Getter
	private BungeeSettings bungeeSettings;

	@Getter
	private CheckUpdate checkUpdate;

	@Getter
	private CommandLoader commandLoader;

	@Getter
	private Config configFile;

	@Getter
	private ConfigVoteSites configVoteSites;

	@Getter
	private CoolDownCheck coolDownCheck;

	@Getter
	private GUI gui;

	@Getter
	private LinkedHashMap<TopVoterPlayer, Integer> lastMonthTopVoter;

	@Getter
	private MVdWPlaceholders mvdwPlaceholders;

	@Getter
	private PlaceHolders placeholders;

	@Getter
	private String profile = "";

	@Getter
	private String buildNumber = "NOTSET";

	@Getter
	private ServerData serverData;

	@Getter
	@Setter
	private Signs signs;

	@Getter
	private SpecialRewards specialRewards;

	@Getter
	private SpecialRewardsConfig specialRewardsConfig;

	@Getter
	private String time = "";

	@Getter
	@Setter
	private LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> topVoter;

	@Getter
	private TopVoterHandler topVoterHandler;

	@Getter
	@Setter
	private boolean update = true;

	@Getter
	@Setter
	private Updater updater;

	@Getter
	private boolean updateStarted = false;

	@Getter
	@Setter
	private ArrayList<CommandHandler> voteCommand;

	@Getter
	private Logger voteLog;

	@Getter
	private VoteParty voteParty;

	@Getter
	private VoteReminding voteReminding;

	@Getter
	private List<VoteSite> voteSites;

	@Getter
	@Setter
	private LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday;

	private boolean votifierLoaded = true;

	@Getter
	private boolean ymlError = false;

	@Getter
	private Timer voteTimer = new Timer();

	@Getter
	private UserManager votingPluginUserManager;

	private void addDirectlyDefinedRewards(DirectlyDefinedReward directlyDefinedReward) {
		RewardHandler.getInstance().addDirectlyDefined(directlyDefinedReward);
	}

	public void basicBungeeUpdate() {
		for (Player player : Bukkit.getOnlinePlayers()) {
			VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
			user.clearCache();
			user.offVote();
			user.checkOfflineRewards();
		}
	}

	/**
	 * Check votifier.
	 */
	private void checkVotifier() {
		try {
			Class.forName("com.vexsoftware.votifier.model.VotifierEvent");
		} catch (ClassNotFoundException e) {
			if (!bungeeSettings.isUseBungeecoord()) {
				plugin.getLogger()
						.warning("No VotifierEvent found, install Votifier, NuVotifier, or another Votifier plugin");
			} else {
				plugin.debug("No VotifierEvent found, but usebungeecoord enabled");
			}
			votifierLoaded = false;
		}
	}

	private void checkYMLError() {
		if (configFile.isFailedToRead() || configVoteSites.isFailedToRead() || specialRewardsConfig.isFailedToRead()
				|| bungeeSettings.isFailedToRead() || gui.isFailedToRead()) {
			ymlError = true;
		} else {
			ymlError = false;
		}

		if (ymlError) {
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().severe("Failed to load a file, check startup log");
				}
			}, 10);
		}
	}

	public ArrayList<TopVoterPlayer> convertSet(Set<TopVoterPlayer> set) {
		return new ArrayList<TopVoterPlayer>(set);
	}

	@Override
	public FileConfiguration getConfig() {
		return configFile.getData();
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getTopVoter(TopVoter top) {
		LinkedHashMap<TopVoterPlayer, Integer> top1 = topVoter.get(top);
		if (top1 == null) {
			top1 = new LinkedHashMap<TopVoterPlayer, Integer>();
		}
		return top1;
	}

	/**
	 * Gets the user.
	 *
	 * @param uuid the uuid
	 * @return the user
	 */
	public VotingPluginUser getUser(UUID uuid) {
		return UserManager.getInstance().getVotingPluginUser(uuid);
	}

	private YamlConfiguration getVersionFile() {
		try {
			CodeSource src = this.getClass().getProtectionDomain().getCodeSource();
			if (src != null) {
				URL jar = src.getLocation();
				ZipInputStream zip = null;
				zip = new ZipInputStream(jar.openStream());
				while (true) {
					ZipEntry e = zip.getNextEntry();
					if (e != null) {
						String name = e.getName();
						if (name.equals("votingpluginversion.yml")) {
							Reader defConfigStream = new InputStreamReader(zip);
							if (defConfigStream != null) {
								YamlConfiguration defConfig = YamlConfiguration.loadConfiguration(defConfigStream);
								defConfigStream.close();
								return defConfig;
							}
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public VoteSite getVoteSite(String site, boolean checkEnabled) {
		String siteName = getVoteSiteName(checkEnabled, site);
		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return voteSite;
			}
		}
		if (configFile.isAutoCreateVoteSites() && !configVoteSites.getVoteSitesNames(false).contains(siteName)) {
			configVoteSites.generateVoteSite(siteName);
			return new VoteSite(plugin, siteName.replace(".", "_"));
		}
		return null;

	}

	public String getVoteSiteName(boolean checkEnabled, String... urls) {
		ArrayList<String> sites = getConfigVoteSites().getVoteSitesNames(checkEnabled);
		for (String url : urls) {
			if (url == null) {
				return null;
			}
			if (sites != null) {
				for (String siteName : sites) {
					String URL = getConfigVoteSites().getServiceSite(siteName);
					if (URL != null) {
						if (URL.equalsIgnoreCase(url)) {
							return siteName;
						}
					}
					if (siteName.equalsIgnoreCase(url)) {
						return siteName;
					}

				}
			}
			return url;
		}
		for (String url : urls) {
			return url;
		}
		return "";

	}

	public String getVoteSiteServiceSite(String name) {
		ArrayList<String> sites = getConfigVoteSites().getVoteSitesNames(true);
		if (name == null) {
			return null;
		}
		if (sites != null) {
			for (String siteName : sites) {
				String URL = getConfigVoteSites().getServiceSite(siteName);
				if (URL != null) {
					if (URL.equalsIgnoreCase(name)) {
						return URL;
					}
					if (name.equalsIgnoreCase(siteName)) {
						return URL;
					}
				}
			}
		}
		return name;

	}

	public boolean hasVoteSite(String site) {
		String siteName = getVoteSiteName(false, site);
		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return true;
			}
		}
		return false;
	}

	public boolean isVoteSite(String voteSite) {
		for (VoteSite site : getVoteSites()) {
			if (site.getKey().equalsIgnoreCase(voteSite)) {
				return true;
			}
		}
		return false;
	}

	public void loadDirectlyDefined() {
		RewardHandler.getInstance().getDirectlyDefinedRewards().clear();
		// AllSites reward
		addDirectlyDefinedRewards(new DirectlyDefinedReward("AllSites") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		// FirstVote
		addDirectlyDefinedRewards(new DirectlyDefinedReward("FirstVote") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		addDirectlyDefinedRewards(new DirectlyDefinedReward("FirstVoteToday") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteReminding.Rewards") {

			@Override
			public void createSection(String key) {
				getConfigFile().saveData();
			}

			@Override
			public ConfigurationSection getFileData() {
				return getConfigFile().getData();
			}

			@Override
			public void save() {
				getConfigFile().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getConfigFile().setValue(path, value);
			}
		});

		// vote cooldown ended
		addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteCoolDownEndedReward") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		// any site rewards
		addDirectlyDefinedRewards(new DirectlyDefinedReward("AnySiteRewards") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		addDirectlyDefinedRewards(new DirectlyDefinedReward("EverySiteReward") {

			@Override
			public void createSection(String key) {
				getConfigVoteSites().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getConfigVoteSites().getData();
			}

			@Override
			public void save() {
				getConfigVoteSites().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getConfigVoteSites().setValue(path, value);
			}
		});

		// login rewards
		addDirectlyDefinedRewards(new DirectlyDefinedReward("LoginRewards") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		// logout rewards
		addDirectlyDefinedRewards(new DirectlyDefinedReward("LogoutRewards") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		// VoteParty
		addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteParty.Rewards") {

			@Override
			public void createSection(String key) {
				getSpecialRewardsConfig().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getSpecialRewardsConfig().getData();
			}

			@Override
			public void save() {
				getSpecialRewardsConfig().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getSpecialRewardsConfig().setValue(path, value);
			}
		});

		// Cumulative rewards
		for (String num : getSpecialRewardsConfig().getCumulativeVotes()) {
			addDirectlyDefinedRewards(new DirectlyDefinedReward("Cumulative." + num + ".Rewards") {

				@Override
				public void createSection(String key) {
					getSpecialRewardsConfig().createSection(key);
				}

				@Override
				public ConfigurationSection getFileData() {
					return getSpecialRewardsConfig().getData();
				}

				@Override
				public void save() {
					getSpecialRewardsConfig().saveData();
				}

				@Override
				public void setData(String path, Object value) {
					getSpecialRewardsConfig().setValue(path, value);
				}
			});
		}

		// Milestones rewards
		for (String num : getSpecialRewardsConfig().getMilestoneVotes()) {
			addDirectlyDefinedRewards(new DirectlyDefinedReward("MileStones." + num + ".Rewards") {

				@Override
				public void createSection(String key) {
					getSpecialRewardsConfig().createSection(key);
				}

				@Override
				public ConfigurationSection getFileData() {
					return getSpecialRewardsConfig().getData();
				}

				@Override
				public void save() {
					getSpecialRewardsConfig().saveData();
				}

				@Override
				public void setData(String path, Object value) {
					getSpecialRewardsConfig().setValue(path, value);
				}
			});
		}

		// VoteSites
		for (VoteSite site : plugin.getVoteSites()) {
			addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteSites." + site.getKey() + ".Rewards") {

				@Override
				public void createSection(String key) {
					getConfigVoteSites().createSection(key);
				}

				@Override
				public ConfigurationSection getFileData() {
					return getConfigVoteSites().getData();
				}

				@Override
				public void save() {
					getConfigVoteSites().saveData();
				}

				@Override
				public void setData(String path, Object value) {
					getConfigVoteSites().setValue(path, value);
				}
			});
		}

		// vote streaks
		String[] types = new String[] { "Day", "Week", "Month" };
		for (String type : types) {
			for (String str : plugin.getSpecialRewardsConfig().getVoteStreakVotes(type)) {
				addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteStreak." + type + "." + str + ".Rewards") {

					@Override
					public void createSection(String key) {
						getSpecialRewardsConfig().createSection(key);
					}

					@Override
					public ConfigurationSection getFileData() {
						return getSpecialRewardsConfig().getData();
					}

					@Override
					public void save() {
						getSpecialRewardsConfig().saveData();
					}

					@Override
					public void setData(String path, Object value) {
						getSpecialRewardsConfig().setValue(path, value);
					}
				});
			}
		}

		for (String path : plugin.getSpecialRewardsConfig().getMonthlyPossibleRewardPlaces()) {
			addDirectlyDefinedRewards(
					new DirectlyDefinedReward(plugin.getSpecialRewardsConfig().getMonthlyAwardRewardsPath(path)) {

						@Override
						public void createSection(String key) {
							getSpecialRewardsConfig().createSection(key);
						}

						@Override
						public ConfigurationSection getFileData() {
							return getSpecialRewardsConfig().getData();
						}

						@Override
						public void save() {
							getSpecialRewardsConfig().saveData();
						}

						@Override
						public void setData(String path, Object value) {
							getSpecialRewardsConfig().setValue(path, value);
						}
					});
		}

		for (String path : plugin.getSpecialRewardsConfig().getWeeklyPossibleRewardPlaces()) {
			addDirectlyDefinedRewards(
					new DirectlyDefinedReward(plugin.getSpecialRewardsConfig().getWeeklyAwardRewardsPath(path)) {

						@Override
						public void createSection(String key) {
							getSpecialRewardsConfig().createSection(key);
						}

						@Override
						public ConfigurationSection getFileData() {
							return getSpecialRewardsConfig().getData();
						}

						@Override
						public void save() {
							getSpecialRewardsConfig().saveData();
						}

						@Override
						public void setData(String path, Object value) {
							getSpecialRewardsConfig().setValue(path, value);
						}
					});
		}

		for (String path : plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces()) {
			addDirectlyDefinedRewards(
					new DirectlyDefinedReward(plugin.getSpecialRewardsConfig().getDailyAwardRewardsPath(path)) {

						@Override
						public void createSection(String key) {
							getSpecialRewardsConfig().createSection(key);
						}

						@Override
						public ConfigurationSection getFileData() {
							return getSpecialRewardsConfig().getData();
						}

						@Override
						public void save() {
							getSpecialRewardsConfig().saveData();
						}

						@Override
						public void setData(String path, Object value) {
							getSpecialRewardsConfig().setValue(path, value);
						}
					});
		}

		for (String identifier : plugin.getGui().getChestShopIdentifiers()) {
			addDirectlyDefinedRewards(new DirectlyDefinedReward("CHEST.Shop." + identifier + ".Rewards") {

				@Override
				public void createSection(String key) {
					getGui().createSection(key);
				}

				@Override
				public ConfigurationSection getFileData() {
					return getGui().getData();
				}

				@Override
				public void save() {
					getGui().saveData();
				}

				@Override
				public void setData(String path, Object value) {
					getGui().setValue(path, value);
				}
			});
		}

		addDirectlyDefinedRewards(new DirectlyDefinedReward("BungeeVotePartyRewards") {

			@Override
			public void createSection(String key) {
				getBungeeSettings().createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return getBungeeSettings().getData();
			}

			@Override
			public void save() {
				getBungeeSettings().saveData();
			}

			@Override
			public void setData(String path, Object value) {
				getBungeeSettings().setValue(path, value);
			}
		});
	}

	private void loadTimer() {
		Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				getTimer().schedule(new TimerTask() {

					@Override
					public void run() {
						if (plugin != null) {
							update();
						} else {
							cancel();
						}
					}
				}, 1000, 1000 * 60 * configFile.getDelayBetweenUpdates());

				getTimer().schedule(new TimerTask() {

					@Override
					public void run() {
						if (plugin != null && configFile.isExtraBackgroundUpdate()) {
							basicBungeeUpdate();
						} else {
							cancel();
						}
					}
				}, 1000, 1000 * 30);

			}
		}, 40L);

	}

	private void loadVersionFile() {
		YamlConfiguration conf = getVersionFile();
		if (conf != null) {
			time = conf.getString("time", "");
			profile = conf.getString("profile", "");
			buildNumber = conf.getString("buildnumber", "NOTSET");
		}
	}

	/**
	 * Load vote sites.
	 */
	public void loadVoteSites() {
		configVoteSites.setup();
		voteSites = Collections.synchronizedList(new ArrayList<VoteSite>());
		voteSites.addAll(configVoteSites.getVoteSitesLoad());

		if (voteSites.size() == 0) {
			plugin.getLogger().warning("Detected no voting sites, this may mean something isn't properly setup");
		}

		plugin.debug("Loaded VoteSites");

	}

	/**
	 * Log vote.
	 *
	 * @param date       the date
	 * @param playerName the player name
	 * @param voteSite   the vote site
	 */
	public void logVote(LocalDateTime date, String playerName, String voteSite) {
		if (configFile.isLogVotesToFile()) {
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
			String str = formatter.format(date);
			voteLog.logToFile(str + ": " + playerName + " voted on " + voteSite);
		}
	}

	/**
	 * Metrics.
	 */
	private void metrics() {
		new VotingPluginMetrics().load(this);
	}

	@Override
	public void onPostLoad() {
		loadVersionFile();
		getOptions().setServer(bungeeSettings.getServer());
		if (bungeeSettings.isUseBungeecoord()) {
			bungeeHandler = new BungeeHandler(this);
			bungeeHandler.load();

			if (getOptions().getServer().equalsIgnoreCase("PleaseSet")) {
				getLogger()
						.warning("Bungeecoord is true and server name is not set, bungeecoord features may not work");
			}

		}

		registerCommands();
		checkVotifier();
		registerEvents();
		loadDirectlyDefined();
		checkUpdate = new CheckUpdate(this);
		checkUpdate.startUp();
		voteReminding = new VoteReminding(this);
		voteReminding.loadRemindChecking();
		specialRewards = new SpecialRewards(this);
		signs = new Signs(this);

		coolDownCheck.checkEnabled();
		coolDownCheck.load();

		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				signs.loadSigns();
			}
		});

		topVoterHandler = new TopVoterHandler(this);
		lastMonthTopVoter = new LinkedHashMap<TopVoterPlayer, Integer>();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				topVoterHandler.loadLastMonth();
				debug("Loaded last month top voters");
			}
		});
		topVoter = new LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>>();
		for (TopVoter top : TopVoter.values()) {
			topVoter.put(top, new LinkedHashMap<TopVoterPlayer, Integer>());
		}
		voteToday = new LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>>();
		voteLog = new Logger(plugin, new File(plugin.getDataFolder() + File.separator + "Log", "votelog.txt"));

		new AdminGUI(this).loadHook();

		// vote party
		voteParty = new VoteParty(this);
		voteParty.register();

		topVoterHandler.register();

		metrics();

		// javascript api
		getJavascriptEngineRequests().add(new JavascriptPlaceholderRequest("User") {

			@Override
			public Object getObject(OfflinePlayer player) {
				return getVotingPluginUserManager().getVotingPluginUser(player);
			}
		});
		getJavascriptEngine().put("VotingPluginHooks", VotingPluginHooks.getInstance());

		loadTimer();

		// placeholderapi loading
		placeholders = new PlaceHolders(this);
		placeholders.load();

		if (Bukkit.getPluginManager().isPluginEnabled("MVdWPlaceholderAPI")) {
			mvdwPlaceholders = new MVdWPlaceholders(this);
			mvdwPlaceholders.loadMVdWPlaceholders();
		}

		// Add rewards
		RewardHandler.getInstance().addInjectedReward(new RewardInjectInt("Points", 0) {

			@Override
			public String onRewardRequest(Reward reward, com.bencodez.advancedcore.api.user.AdvancedCoreUser user,
					int num, HashMap<String, String> placeholders) {
				VotingPluginUser vpUser = UserManager.getInstance().getVotingPluginUser(user);
				user.dontCache();
				vpUser.addPoints(num);
				return null;
			}
		}.synchronize().addEditButton(
				new EditGUIButton(new ItemBuilder(Material.PAPER), new EditGUIValueNumber("Points", null) {

					@Override
					public void setValue(Player player, Number value) {
						RewardEditData reward = (RewardEditData) getInv().getData("Reward");
						reward.setValue("Points", value.intValue());
					}
				}.addLore("Give player voting points"))).validator(new RewardInjectValidator() {

					@Override
					public void onValidate(Reward reward, RewardInject inject, ConfigurationSection data) {
						if (data.getInt(inject.getPath(), -1) == 0) {
							warning(reward, inject, "Points can not be 0");
						}
					}
				}));

		RewardHandler.getInstance().addInjectedReward(new RewardInjectConfigurationSection("VoteBossBar") {

			@Override
			public String onRewardRequested(Reward arg0, com.bencodez.advancedcore.api.user.AdvancedCoreUser user,
					ConfigurationSection section, HashMap<String, String> placeholders) {
				if (section.getBoolean("Enabled")) {
					user.sendBossBar(
							StringParser.getInstance().replacePlaceHolder(section.getString("Message", ""),
									placeholders),
							section.getString("Color", "BLUE"), section.getString("Style", "SOLID"),
							(double) UserManager.getInstance().getVotingPluginUser(user).getSitesVotedOn()
									/ plugin.getVoteSites().size(),
							section.getInt("Delay", 30));
				}
				return null;
			}
		});

		for (final TopVoter top : TopVoter.values()) {
			RewardHandler.getInstance().addPlaceholder(new RewardPlaceholderHandle("Total_" + top.toString()) {

				@Override
				public String getValue(Reward reward, com.bencodez.advancedcore.api.user.AdvancedCoreUser user) {
					VotingPluginUser vUser = UserManager.getInstance().getVotingPluginUser(user);
					return "" + vUser.getTotal(top);
				}
			});
		}

		if (plugin.getConfigFile().isFormatAlternateBroadcastEnabled()) {
			broadcastHandler = new BroadcastHandler(plugin, plugin.getConfigFile().getFormatAlternateBroadcastDelay());
			plugin.debug("Using alternate broadcast method");
		}

		plugin.getLogger().info("Enabled VotingPlugin " + plugin.getDescription().getVersion());
		if (getProfile().contains("dev")) {
			plugin.getLogger().info(
					"Using dev build, this is not a stable build, use at your own risk. Build number: " + buildNumber);
		}

		boolean hasRewards = RewardHandler.getInstance().hasRewards(getConfigVoteSites().getData(),
				getConfigVoteSites().getEverySiteRewardPath());

		boolean issues = true;
		ArrayList<String> services = serverData.getServiceSites();
		for (VoteSite site : getVoteSites()) {
			if (!site.hasRewards() && !hasRewards) {
				issues = false;
				plugin.getLogger().warning("No rewards detected for the site: " + site.getKey()
						+ ". See https://github.com/BenCodez/VotingPlugin/wiki/Rewards");
			}

			boolean contains = false;
			for (String service : services) {
				if (service.equalsIgnoreCase(site.getServiceSite())) {
					contains = true;
				}
			}
			if (!contains) {
				issues = false;
				plugin.getLogger().warning("No vote has been recieved from " + site.getServiceSite()
						+ ", may be an invalid service site. Please read: https://github.com/BenCodez/VotingPlugin/wiki/Votifier-Troubleshooting");
			}
		}

		if (!issues) {
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().warning(
							"Detected an issue with voting sites, check the server startup log for more details: https://github.com/BenCodez/VotingPlugin/wiki/Votifier-Troubleshooting");
				}
			}, 30l);
		}

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.plugin.java.JavaPlugin#onEnable()
	 */
	@Override
	public void onPreLoad() {
		plugin = this;

		// disable plugin for older versions below 1.12

		if (NMSManager.getInstance().isVersion("1.7", "1.8", "1.9", "1.10", "1.11")) {
			plugin.getLogger().severe("Detected running " + Bukkit.getVersion()
					+ ", this version is not supported on this build, read the plugin page. Disabling...");
			if (!configFile.isOverrideVersionDisable()) {
				Bukkit.getPluginManager().disablePlugin(this);
				return;
			} else {
				plugin.getLogger().warning("Overriding version disable, beware of using this! This may cause issues!");
			}
		}

		setupFiles();

		loadVoteSites();

		votingPluginUserManager = UserManager.getInstance();
		votingPluginUserManager.addCachingKeys();

		setJenkinsSite("bencodez.com");
		updateAdvancedCoreHook();

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.plugin.java.JavaPlugin#onDisable()
	 */
	@Override
	public void onUnLoad() {
		getSigns().storeSigns();
		HandlerList.unregisterAll(plugin);
		if (bungeeSettings.isUseBungeecoord()) {
			try {
				getBungeeHandler().close();
			} catch (Exception e) {
				debug(e);
			}
		}
		plugin = null;
	}

	/**
	 * Register commands.
	 */
	private void registerCommands() {
		commandLoader = new CommandLoader(this);
		commandLoader.loadCommands();
		commandLoader.loadAliases();

		// /vote, /v
		getCommand("vote").setExecutor(new CommandVote(this));
		getCommand("vote").setTabCompleter(new VoteTabCompleter());
		// getCommand("v").setExecutor(new CommandVote(this));
		// getCommand("v").setTabCompleter(new VoteTabCompleter());

		// /adminvote, /av
		getCommand("adminvote").setExecutor(new CommandAdminVote(this));
		getCommand("adminvote").setTabCompleter(new AdminVoteTabCompleter());
		getCommand("adminvote").setPermission("VotingPlugin.Commands.AdminVote");
		getCommand("av").setExecutor(new CommandAdminVote(this));
		getCommand("av").setTabCompleter(new AdminVoteTabCompleter());
		getCommand("av").setPermission("VotingPlugin.Commands.AdminVote");

		Permission perm = Bukkit.getPluginManager().getPermission("VotingPlugin.Player");
		if (perm != null) {
			if (configFile.getGiveDefaultPermission()) {
				perm.setDefault(PermissionDefault.TRUE);
				getLogger().info("Giving VotingPlugin.Player permission by default, can be disabled in the config");
			} else {
				perm.setDefault(PermissionDefault.OP);
			}
		}

		plugin.debug("Loaded Commands");

	}

	/**
	 * Register events.
	 */
	private void registerEvents() {
		PluginManager pm = getServer().getPluginManager();

		pm.registerEvents(new PlayerJoinEvent(this), this);
		if (votifierLoaded) {
			pm.registerEvents(new VotiferEvent(this), this);
		}
		pm.registerEvents(new PlayerVoteListener(this), this);
		pm.registerEvents(new SignChange(this), this);
		pm.registerEvents(new BlockBreak(this), this);
		if (!plugin.getConfigFile().isDisableInteractEvent()) {
			pm.registerEvents(new PlayerInteract(this), this);
		}

		pm.registerEvents(new VotingPluginUpdateEvent(this), this);
		/*
		 * if (!NMSManager.getInstance().isVersion("1.12")) { pm.registerEvents(new
		 * PlayerCommandSendListener(this), this); }
		 */
		coolDownCheck = new CoolDownCheck(this);
		pm.registerEvents(coolDownCheck, this);

		plugin.debug("Loaded Events");

	}

	/**
	 * Reload.
	 */
	@Override
	public void reload() {
		reloadPlugin(false);
	}

	public void reloadAll() {
		reloadPlugin(true);
	}

	private void reloadPlugin(boolean userStorage) {
		setUpdate(true);

		configFile.reloadData();
		configFile.loadValues();

		configVoteSites.reloadData();

		specialRewardsConfig.reloadData();

		gui.reloadData();

		bungeeSettings.reloadData();
		checkYMLError();

		if (broadcastHandler != null) {
			broadcastHandler.schelude(getConfigFile().getFormatAlternateBroadcastDelay());
		}

		updateAdvancedCoreHook();
		plugin.loadVoteSites();
		loadDirectlyDefined();
		reloadAdvancedCore(userStorage);
		getOptions().setServer(bungeeSettings.getServer());
		if (userStorage) {
			placeholders.load();
		}

		voteReminding.loadRemindChecking();
		coolDownCheck.checkEnabled();

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				update();
			}
		});
	}

	private void setupFiles() {
		configFile = new Config(this);
		configFile.setup();

		configVoteSites = new ConfigVoteSites(this);
		configVoteSites.setup();

		specialRewardsConfig = new SpecialRewardsConfig(this);
		specialRewardsConfig.setup();

		bungeeSettings = new BungeeSettings(this);
		bungeeSettings.setup();

		gui = new GUI(this);
		gui.setup();

		serverData = new ServerData(this);

		checkYMLError();

		plugin.debug("Loaded Files");
	}

	public void update() {
		if (update || configFile.isAlwaysUpdate()) {
			if (!updateStarted && plugin != null) {
				if (!configFile.isUpdateWithPlayersOnlineOnly() || Bukkit.getOnlinePlayers().size() != 0) {
					updateStarted = true;
					update = false;

					synchronized (plugin) {
						if (plugin != null && plugin.isEnabled()) {
							getUserManager().getDataManager().clearCacheBasic();

							plugin.debug("Starting background task, current cached users: "
									+ plugin.getUserManager().getDataManager().getUserDataCache().keySet().size());

							int dataLoadLimit = getConfigFile().getPlayerDataLoadLimit();
							try {
								boolean extraBackgroundUpdate = configFile.isExtraBackgroundUpdate();
								long startTime = System.currentTimeMillis();

								LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday = new LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>>();
								LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> tempTopVoter = new LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>>();

								ArrayList<TopVoter> topVotersToCheck = new ArrayList<TopVoter>();
								for (TopVoter top : TopVoter.values()) {
									if (plugin == null) {
										return;
									}
									if (plugin.getConfigFile().getLoadTopVoter(top)) {
										topVotersToCheck.add(top);
										tempTopVoter.put(top, new LinkedHashMap<TopVoterPlayer, Integer>());
									}
								}
								boolean topVoterIgnorePermissionUse = plugin.getConfigFile()
										.getTopVoterIgnorePermission();
								ArrayList<String> blackList = plugin.getConfigFile().getBlackList();

								ArrayList<String> uuids = UserManager.getInstance().getAllUUIDs();
								int currentDay = LocalDateTime.now().getDayOfMonth();
								int currentDataLoad = 0;
								for (String uuid : uuids) {
									if (uuid != null && !uuid.isEmpty()) {
										VotingPluginUser user = UserManager.getInstance()
												.getVotingPluginUser(UUID.fromString(uuid), false);
										user.dontCache();
										user.tempCache();
										user.getUserData().updateCacheWithTemp();
										if (!user.isBanned() && !blackList.contains(user.getPlayerName())) {

											if (!topVoterIgnorePermissionUse || !user.isTopVoterIgnore()) {
												for (TopVoter top : topVotersToCheck) {
													int total = user.getTotal(top);
													if (total > 0) {
														tempTopVoter.get(top).put(user.getTopVoterPlayer(), total);
													}
												}
											}

											HashMap<VoteSite, LocalDateTime> times = new HashMap<VoteSite, LocalDateTime>();
											for (Entry<VoteSite, Long> entry : user.getLastVotes().entrySet()) {
												if (entry.getKey().isEnabled() && !entry.getKey().isHidden()) {
													long time = entry.getValue();
													if ((currentDay == MiscUtils.getInstance().getDayFromMili(time))
															&& (LocalDateTime.now().getMonthValue() == MiscUtils
																	.getInstance().getMonthFromMili(time))
															&& (LocalDateTime.now().getYear() == MiscUtils.getInstance()
																	.getYearFromMili(time))) {

														times.put(entry.getKey(), LocalDateTime.ofInstant(
																Instant.ofEpochMilli(time), ZoneId.systemDefault()));
													}
												}
											}
											if (times.keySet().size() > 0) {
												voteToday.put(user.getTopVoterPlayer(), times);
											}
										}
										if (!extraBackgroundUpdate) {
											if (user.isOnline()) {
												user.offVote();
											}
										}
										plugin.getPlaceholders().onUpdate(user);
										user.clearTempCache();
										user = null;
										if (dataLoadLimit > 0) {
											currentDataLoad++;
											if (currentDataLoad >= dataLoadLimit) {
												currentDataLoad -= dataLoadLimit;
												Thread.sleep(1000);
											}
										}
									}
								}
								update = false;
								long time1 = ((System.currentTimeMillis() - startTime) / 1000);
								plugin.debug("Finished loading player data in " + time1 + " seconds, " + uuids.size()
										+ " users, " + plugin.getStorageType().toString() + ", data load limit: "
										+ dataLoadLimit);
								time1 = System.currentTimeMillis();

								topVoterHandler.updateTopVoters(tempTopVoter);
								placeholders.onUpdate();
								setVoteToday(voteToday);
								serverData.updateValues();
								getSigns().updateSigns();

								tempTopVoter = null;

								time1 = ((System.currentTimeMillis() - time1) / 1000);
								long totalTime = ((System.currentTimeMillis() - startTime) / 1000);
								plugin.debug("Background task finished. Final processing took " + time1
										+ " seconds. Total time: " + totalTime + " seconds");
								plugin.extraDebug("Current cached users: "
										+ plugin.getUserManager().getDataManager().getUserDataCache().keySet().size());
								checkFirstTimeLoaded();
							} catch (Exception ex) {
								if (plugin != null) {
									plugin.getLogger().info("Looks like something went wrong");
								}
								ex.printStackTrace();
							}
						}
					}

					updateStarted = false;
				}
			}
		}
	}

	private boolean firstTimeLoaded = false;

	public void checkFirstTimeLoaded() {
		if (!firstTimeLoaded) {
			if (getGui().isChestVoteTopUseSkull()) {
				int maxToLoad = 200;
				for (TopVoter top : topVoter.keySet()) {
					int num = 1;
					Set<TopVoterPlayer> players = topVoter.get(top).keySet();
					for (TopVoterPlayer p : players) {
						if (num <= maxToLoad) {
							SkullHandler.getInstance().loadSkull(p.getPlayerName());
						}
						num++;
					}
				}
			}
		}
		firstTimeLoaded = true;
	}

	public void updateAdvancedCoreHook() {
		getJavascriptEngine().put("VotingPlugin", this);
		allowDownloadingFromSpigot(15358);
		setConfigData(configFile.getData());
		if (bungeeSettings.isUseBungeecoord()) {
			getOptions().setPerServerRewards(getBungeeSettings().isPerServerRewards());
		}
	}

}
