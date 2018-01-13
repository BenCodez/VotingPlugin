package com.Ben12345rocks.VotingPlugin;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.bukkit.permissions.Permission;
import org.bukkit.permissions.PermissionDefault;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.java.JavaPlugin;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Objects.UserStorage;
import com.Ben12345rocks.AdvancedCore.Thread.Thread;
import com.Ben12345rocks.AdvancedCore.Util.Javascript.JavascriptPlaceholderRequest;
import com.Ben12345rocks.AdvancedCore.Util.Logger.Logger;
import com.Ben12345rocks.AdvancedCore.Util.Metrics.BStatsMetrics;
import com.Ben12345rocks.AdvancedCore.Util.Metrics.MCStatsMetrics;
import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.AdvancedCore.mysql.MySQL;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Commands.GUI.AdminGUI;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AdminVoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.BlockBreak;
import com.Ben12345rocks.VotingPlugin.Events.PlayerInteract;
import com.Ben12345rocks.VotingPlugin.Events.PlayerJoinEvent;
import com.Ben12345rocks.VotingPlugin.Events.SignChange;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Events.VotingPluginUpdateEvent;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.Signs.Signs;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.Util.Updater.CheckUpdate;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.Ben12345rocks.VotingPlugin.VoteReminding.VoteReminding;

/**
 * The Class Main.
 */
public class Main extends JavaPlugin {

	/** The config. */
	public static Config config;

	/** The config vote sites. */
	public static ConfigVoteSites configVoteSites;

	/** The plugin. */
	public static Main plugin;

	/** The top voter monthly. */
	public LinkedHashMap<User, Integer> topVoterAllTime;

	/** The top voter monthly. */
	public LinkedHashMap<User, Integer> topVoterMonthly;

	/** The top voter weekly. */
	public LinkedHashMap<User, Integer> topVoterWeekly;

	/** The top voter daily. */
	public LinkedHashMap<User, Integer> topVoterDaily;

	/** The updater. */
	public Updater updater;

	/** The vote command. */
	public ArrayList<CommandHandler> voteCommand;

	/** The admin vote command. */
	public ArrayList<CommandHandler> adminVoteCommand;

	/** The vote sites. */
	private List<VoteSite> voteSites;

	/** The vote today. */
	public LinkedHashMap<User, HashMap<VoteSite, LocalDateTime>> voteToday;

	/** The signs. */
	public ArrayList<SignHandler> signs;

	/** The vote log. */
	public Logger voteLog;

	private boolean update = true;

	private boolean updateStarted = false;

	/**
	 * Check votifier.
	 */
	public void checkVotifier() {
		try {
			Class.forName("com.vexsoftware.votifier.model.VotifierEvent");
		} catch (ClassNotFoundException e) {
			plugin.getLogger()
					.warning("No VotifierEvent found, install Votifier, NuVotifier, or another Votifier plugin");
		}
	}

	public void convertDataStorage(UserStorage from, UserStorage to) {
		if (from == null || to == null) {
			throw new RuntimeException("Invalid Storage Method");
		}
		UserStorage cur = AdvancedCoreHook.getInstance().getStorageType();
		AdvancedCoreHook.getInstance().setStorageType(from);
		loadMySQL();
		HashMap<User, HashMap<String, String>> data = new HashMap<User, HashMap<String, String>>();
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			try {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));

				HashMap<String, String> values = new HashMap<String, String>();
				for (String key : user.getData().getKeys()) {
					values.put(key, user.getData().getString(key));
				}
				data.put(user, values);
			} catch (Exception e) {
				AdvancedCoreHook.getInstance().debug(e);
				plugin.getLogger().warning("Exception occoured for '" + uuid + "': " + e.getMessage()
						+ ", turn debug on to see full stack traces");
			}
		}

		AdvancedCoreHook.getInstance().setStorageType(to);
		loadMySQL();

		for (Entry<User, HashMap<String, String>> entry : data.entrySet()) {
			try {
				for (Entry<String, String> values : entry.getValue().entrySet()) {
					entry.getKey().getData().setString(values.getKey(), values.getValue());
				}
			} catch (Exception e) {
				e.printStackTrace();
				plugin.getLogger().warning("Exception occoured for '" + entry.getKey().getUUID() + "': "
						+ e.getMessage() + ", turn debug on to see full stack traces");
			}
		}
		AdvancedCoreHook.getInstance().setStorageType(cur);
	}

	public ArrayList<User> convertSet(Set<User> set) {
		return new ArrayList<User>(set);
	}

	/**
	 * Debug.
	 *
	 * @param message
	 *            the message
	 */
	public void debug(String message) {
		AdvancedCoreHook.getInstance().debug(plugin, message);
	}

	/**
	 * Gets the user.
	 *
	 * @param uuid
	 *            the uuid
	 * @return the user
	 */
	public User getUser(UUID uuid) {
		return UserManager.getInstance().getVotingPluginUser(uuid);
	}

	public UserManager getUserManager() {
		return UserManager.getInstance();
	}

	public VoteParty getVoteParty() {
		return VoteParty.getInstance();
	}

	/**
	 * Gets the vote site.
	 *
	 * @param site
	 *            the site name
	 * @return the vote site
	 */
	public VoteSite getVoteSite(String site) {
		String siteName = getVoteSiteName(site);
		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return voteSite;
			}
		}
		if (Config.getInstance().getAutoCreateVoteSites() && !configVoteSites.getVoteSitesNames().contains(siteName)) {
			configVoteSites.generateVoteSite(siteName);
			return new VoteSite(siteName.replace(".", "_"));
		}
		return null;

	}

	/**
	 * Gets the vote site name.
	 *
	 * @param url
	 *            the url
	 * @return the vote site name
	 */
	public String getVoteSiteName(String url) {
		ArrayList<String> sites = ConfigVoteSites.getInstance().getVoteSitesNames();
		if (url == null) {
			return null;
		}
		if (sites != null) {
			for (String siteName : sites) {
				String URL = ConfigVoteSites.getInstance().getServiceSite(siteName);
				if (URL != null) {
					if (URL.equalsIgnoreCase(url)) {
						return siteName;
					}
				}
			}
			for (String siteName : sites) {
				if (siteName.equalsIgnoreCase(url)) {
					return siteName;
				}
			}
		}
		return url;

	}

	public List<VoteSite> getVoteSites() {
		return voteSites;
	}

	public boolean hasVoteSite(String site) {
		String siteName = getVoteSiteName(site);
		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return true;
			}
		}
		return false;
	}

	private void loadMySQL() {
		if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
			Thread.getInstance().run(new Runnable() {

				@Override
				public void run() {
					AdvancedCoreHook.getInstance()
							.setMysql(new MySQL("VotingPlugin_Users", Config.getInstance().getMySql()));
				}
			});

		}
	}

	private void loadTimer() {
		AdvancedCoreHook.getInstance().getTimer().schedule(new TimerTask() {

			@Override
			public void run() {
				update();
			}
		}, 1000, 1000 * 60 * Config.getInstance().getDelayBetweenUpdates());
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
	 * @param date
	 *            the date
	 * @param playerName
	 *            the player name
	 * @param voteSite
	 *            the vote site
	 */
	public void logVote(Date date, String playerName, String voteSite) {
		if (Config.getInstance().getLogVotesToFile()) {
			String str = new SimpleDateFormat("EEE, d MMM yyyy HH:mm").format(date);
			voteLog.logToFile(str + ": " + playerName + " voted on " + voteSite);
		}
	}

	/**
	 * Metrics.
	 */
	private void metrics() {
		try {
			MCStatsMetrics metrics = new MCStatsMetrics(this);
			metrics.start();
			plugin.debug("Loaded Metrics");
		} catch (IOException e) {
			plugin.getLogger().info("Can't submit metrics stats");
		}

		BStatsMetrics metrics = new BStatsMetrics(this);

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_firstvote") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
						Config.getInstance().getFirstVoteRewardsPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_everysite") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(ConfigVoteSites.getInstance().getData(),
						ConfigVoteSites.getInstance().getEverySiteRewardPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_allsites") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
						Config.getInstance().getAllSitesRewardPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_cumulative") {

			@Override
			public String getValue() {
				if (Config.getInstance().getCumulativeVotes().size() == 0) {
					return "False";
				} else {
					for (String cum : Config.getInstance().getCumulativeVotes()) {
						if (Config.getInstance().getCumulativeRewardEnabled(Integer.parseInt(cum))) {
							return "True";
						}
					}
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_voteparty") {

			@Override
			public String getValue() {
				if (!Config.getInstance().getVotePartyEnabled()) {
					return "False";
				} else {
					return "True";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_milestone") {

			@Override
			public String getValue() {
				if (Config.getInstance().getMilestoneVotes().size() == 0) {
					return "False";
				} else {
					for (String milestone : Config.getInstance().getMilestoneVotes()) {
						if (Config.getInstance().getMilestoneRewardEnabled(Integer.parseInt(milestone))) {
							return "True";
						}
					}
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_anysitereward") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
						Config.getInstance().getAnySiteRewardsPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakday") {

			@Override
			public String getValue() {
				for (String s : Config.getInstance().getVoteStreakVotes("Day")) {

					if (Config.getInstance().getVoteStreakRewardEnabled("Day", s)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getVoteStreakRewardsPath("Day", s))) {
						return "True";
					}

				}
				return "False";
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakweek") {

			@Override
			public String getValue() {
				for (String s : Config.getInstance().getVoteStreakVotes("Week")) {

					if (Config.getInstance().getVoteStreakRewardEnabled("Week", s)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getVoteStreakRewardsPath("Week", s))) {
						return "True";
					}
				}

				return "False";
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakmonth") {

			@Override
			public String getValue() {
				for (String s : Config.getInstance().getVoteStreakVotes("Month")) {

					if (Config.getInstance().getVoteStreakRewardEnabled("Month", s)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getVoteStreakRewardsPath("Month", s))) {
						return "True";
					}
				}

				return "False";
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofsites") {

			@Override
			public String getValue() {
				return "" + plugin.voteSites.size();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofrewards") {

			@Override
			public String getValue() {
				return "" + RewardHandler.getInstance().getRewards().size();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("autocreatevotesites") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getAutoCreateVoteSites();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("sendscoreboards") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getSendScoreboards();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofuser") {

			@Override
			public String getValue() {
				int total = UserManager.getInstance().getAllUUIDs().size();
				int num = total / 100;
				num = num * 100;
				int num2 = (total + 100) / 100;
				num2 = num2 * 100;
				return "" + num + "-" + num2;
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("data_storage") {

			@Override
			public String getValue() {
				return Config.getInstance().getDataStorage();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("DisableCheckOnWorldChange") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getDisableCheckOnWorldChange();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("votereminding_enabled") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getVoteRemindingEnabled();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Today") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUIToday();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_TopVoter") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUITopVoter();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Last") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUILast();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Next") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUINext();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Total") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUITotal();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Vote") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUIVote();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Best") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUIBest();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("UseGUI_Streak") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getCommandsUseGUIStreak();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Monthly") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getLoadTopVoterMonthly();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Weekly") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getLoadTopVoterWeekly();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Daily") {

			@Override
			public String getValue() {
				return "" + Config.getInstance().getLoadTopVoterDaily();
			}
		});
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.plugin.java.JavaPlugin#onDisable()
	 */
	@Override
	public void onDisable() {
		new Timer().schedule(new TimerTask() {

			@Override
			public void run() {
				Signs.getInstance().storeSigns();
			}
		}, 0);
		HandlerList.unregisterAll(plugin);
		plugin = null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.plugin.java.JavaPlugin#onEnable()
	 */
	@Override
	public void onEnable() {
		plugin = this;

		setupFiles();
		loadVoteSites();
		updateAdvancedCoreHook();
		AdvancedCoreHook.getInstance().loadHook(this);
		if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
			debug("UseBatchUpdates: " + AdvancedCoreHook.getInstance().getMysql().isUseBatchUpdates());
		}
		registerCommands();
		registerEvents();
		checkVotifier();

		CheckUpdate.getInstance().startUp();

		VoteReminding.getInstance().loadRemindChecking();

		plugin.signs = new ArrayList<SignHandler>();

		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				Signs.getInstance().loadSigns();
			}
		});

		topVoterMonthly = new LinkedHashMap<User, Integer>();
		topVoterWeekly = new LinkedHashMap<User, Integer>();
		topVoterDaily = new LinkedHashMap<User, Integer>();
		voteToday = new LinkedHashMap<User, HashMap<VoteSite, LocalDateTime>>();
		topVoterAllTime = new LinkedHashMap<User, Integer>();

		voteLog = new Logger(plugin, new File(plugin.getDataFolder() + File.separator + "Log", "votelog.txt"));

		AdminGUI.getInstance().loadHook();

		if (Config.getInstance().getVotePartyEnabled()) {
			VoteParty.getInstance().check();
		}
		VoteParty.getInstance().register();

		TopVoterHandler.getInstance().register();

		metrics();

		AdvancedCoreHook.getInstance().getJavascriptEngineRequests().add(new JavascriptPlaceholderRequest("User") {

			@Override
			public Object getObject(OfflinePlayer player) {
				return getUserManager().getVotingPluginUser(player);
			}
		});

		loadTimer();

		plugin.getLogger().info("Enabled VotingPlgin " + plugin.getDescription().getVersion());

		for (VoteSite site : getVoteSites()) {
			if (!site.hasRewards()) {
				plugin.getLogger().warning("No rewards detected for the site: " + site.getKey()
						+ ". See https://github.com/Ben12345rocks/AdvancedCore/wiki/Rewards on how to add rewards");
			}
			if (!ServerData.getInstance().getServiceSites().contains(site.getServiceSite())) {
				plugin.getLogger().warning("No vote has been recieved from " + site.getServiceSite()
						+ ", may be an invalid service site. Vote on the site and look in console for a service site, if you get nothing then there is an issue with votifier");
			}
		}

	}

	public LinkedHashMap<User, Integer> getTopVoterAllTime() {
		return topVoterAllTime;
	}

	public LinkedHashMap<User, Integer> getTopVoterMonthly() {
		return topVoterMonthly;
	}

	public LinkedHashMap<User, Integer> getTopVoterWeekly() {
		return topVoterWeekly;
	}

	public LinkedHashMap<User, Integer> getTopVoterDaily() {
		return topVoterDaily;
	}

	public Updater getUpdater() {
		return updater;
	}

	public ArrayList<CommandHandler> getVoteCommand() {
		return voteCommand;
	}

	public ArrayList<CommandHandler> getAdminVoteCommand() {
		return adminVoteCommand;
	}

	public LinkedHashMap<User, HashMap<VoteSite, LocalDateTime>> getVoteToday() {
		return voteToday;
	}

	public ArrayList<SignHandler> getSigns() {
		return signs;
	}

	public Logger getVoteLog() {
		return voteLog;
	}

	public boolean isUpdate() {
		return update;
	}

	public boolean isUpdateStarted() {
		return updateStarted;
	}

	/**
	 * Register commands.
	 */
	private void registerCommands() {
		CommandLoader.getInstance().loadCommands();
		CommandLoader.getInstance().loadAliases();

		// /vote, /v
		getCommand("vote").setExecutor(new CommandVote(this));
		getCommand("vote").setTabCompleter(new VoteTabCompleter());
		// getCommand("v").setExecutor(new CommandVote(this));
		// getCommand("v").setTabCompleter(new VoteTabCompleter());

		// /adminvote, /av
		getCommand("adminvote").setExecutor(new CommandAdminVote(this));
		getCommand("adminvote").setTabCompleter(new AdminVoteTabCompleter());
		getCommand("av").setExecutor(new CommandAdminVote(this));
		getCommand("av").setTabCompleter(new AdminVoteTabCompleter());

		Permission perm = Bukkit.getPluginManager().getPermission("VotingPlugin.Player");
		if (perm != null) {
			if (Config.getInstance().getGiveDefaultPermission()) {
				perm.setDefault(PermissionDefault.TRUE);
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
		pm.registerEvents(new VotiferEvent(this), this);

		pm.registerEvents(new SignChange(this), this);

		pm.registerEvents(new BlockBreak(this), this);

		pm.registerEvents(new PlayerInteract(this), this);

		pm.registerEvents(new VotingPluginUpdateEvent(this), this);

		plugin.debug("Loaded Events");

	}

	/**
	 * Reload.
	 */
	public void reload() {
		setUpdate(true);
		config.reloadData();
		configVoteSites.reloadData();
		updateAdvancedCoreHook();
		plugin.loadVoteSites();
		AdvancedCoreHook.getInstance().reload();
		loadTimer();

	}

	public void setUpdate(boolean update) {
		this.update = update;
	}

	/**
	 * Setup files.
	 */
	public void setupFiles() {
		try {
			config = Config.getInstance();
			config.setup();
		} catch (Exception e) {
			e.printStackTrace();
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().severe("Failed to load Config.yml");
					e.printStackTrace();
				}
			}, 10);
		}
		try {
			configVoteSites = ConfigVoteSites.getInstance();
			configVoteSites.setup();
		} catch (Exception e) {
			e.printStackTrace();
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().severe("Failed to load VoteSites.yml");
					e.printStackTrace();
				}

			}, 10);
		}

		plugin.debug("Loaded Files");

	}

	/**
	 * Update.
	 */
	public void update() {
		if (update && plugin != null && !updateStarted) {
			updateStarted = true;
			update = false;

			synchronized (plugin) {
				if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
					if (AdvancedCoreHook.getInstance().getMysql() == null) {
						plugin.debug("MySQL not loaded yet");
						return;
					} else if (Config.getInstance().getClearCacheOnUpdate()) {
						AdvancedCoreHook.getInstance().getMysql().clearCache();
					} else {
						AdvancedCoreHook.getInstance().getMysql().clearCacheBasic();
					}
				}

				plugin.debug("Starting background task");
				long time = System.currentTimeMillis();
				try {
					ArrayList<String> uuids = UserManager.getInstance().getAllUUIDs();
					ArrayList<User> users = new ArrayList<User>();
					for (String uuid : uuids) {
						User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
						users.add(user);
					}
					update = false;
					long time1 = ((System.currentTimeMillis() - time) / 1000);
					plugin.debug("Finished loading player data in " + time1 + " seconds, " + users.size() + " users");
					TopVoterHandler.getInstance().updateTopVoters(users);
					Commands.getInstance().updateVoteToday(users);
					ServerData.getInstance().updateValues();
					Signs.getInstance().updateSigns();

					for (Player player : Bukkit.getOnlinePlayers()) {
						UserManager.getInstance().getVotingPluginUser(player).offVote();
					}
					time1 = ((System.currentTimeMillis() - time) / 1000);
					plugin.debug("Background task finished in " + time1 + " seconds");
				} catch (Exception ex) {
					ex.printStackTrace();
					plugin.getLogger().info("Looks like something went wrong.");
				}
			}

			updateStarted = false;
		}
	}

	public void updateAdvancedCoreHook() {
		AdvancedCoreHook.getInstance().setAutoDownload(Config.getInstance().getAutoDownload());
		AdvancedCoreHook.getInstance().getJavascriptEngine().put("VotingPlugin", this);
		AdvancedCoreHook.getInstance().allowDownloadingFromSpigot(15358);
		AdvancedCoreHook.getInstance().setExtraDebug(Config.getInstance().getExtraDebug());
		AdvancedCoreHook.getInstance().setStorageType(UserStorage.value(Config.getInstance().getDataStorage()));
		loadMySQL();

		AdvancedCoreHook.getInstance()
				.setDisableCheckOnWorldChange(Config.getInstance().getDisableCheckOnWorldChange());
		AdvancedCoreHook.getInstance().setDebug(Config.getInstance().getDebugEnabled());
		AdvancedCoreHook.getInstance().setDebugIngame(Config.getInstance().getDebugInfoIngame());
		AdvancedCoreHook.getInstance().setDefaultRequestMethod(Config.getInstance().getRequestAPIDefaultMethod());
		AdvancedCoreHook.getInstance().setDisabledRequestMethods(Config.getInstance().getRequestAPIDisabledMethods());
		AdvancedCoreHook.getInstance().setFormatNoPerms(Config.getInstance().getFormatNoPerms());
		AdvancedCoreHook.getInstance().setFormatNotNumber(Config.getInstance().getFormatNotNumber());
		AdvancedCoreHook.getInstance().setHelpLine(Config.getInstance().getFormatHelpLine());
		AdvancedCoreHook.getInstance().setLogDebugToFile(Config.getInstance().getLogDebugToFile());
		AdvancedCoreHook.getInstance().setSendScoreboards(Config.getInstance().getSendScoreboards());
		AdvancedCoreHook.getInstance().setAlternateUUIDLookUp(Config.getInstance().getAlternateUUIDLookup());
		AdvancedCoreHook.getInstance().setAutoKillInvs(Config.getInstance().getAutoKillInvs());
		AdvancedCoreHook.getInstance().setPrevPageTxt(Config.getInstance().getFormatPrevPage());
		AdvancedCoreHook.getInstance().setNextPageTxt(Config.getInstance().getFormatNextPage());

		AdvancedCoreHook.getInstance().setPurgeOldData(Config.getInstance().getPurgeOldData());
		AdvancedCoreHook.getInstance().setPurgeMinimumDays(Config.getInstance().getPurgeMin());
		AdvancedCoreHook.getInstance().setCheckNameMojang(Config.getInstance().getCheckNameMojang());
	}

}
