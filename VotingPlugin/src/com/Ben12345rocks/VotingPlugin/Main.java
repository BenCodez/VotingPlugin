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
import com.Ben12345rocks.AdvancedCore.CommandAPI.CommandHandler;
import com.Ben12345rocks.AdvancedCore.NMSManager.NMSManager;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.UserManager.UUID;
import com.Ben12345rocks.AdvancedCore.UserManager.UserStorage;
import com.Ben12345rocks.AdvancedCore.Util.Javascript.JavascriptPlaceholderRequest;
import com.Ben12345rocks.AdvancedCore.Util.Logger.Logger;
import com.Ben12345rocks.AdvancedCore.Util.Metrics.BStatsMetrics;
import com.Ben12345rocks.AdvancedCore.Util.Metrics.MCStatsMetrics;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
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
import com.Ben12345rocks.VotingPlugin.Listeners.BlockBreak;
import com.Ben12345rocks.VotingPlugin.Listeners.PlayerInteract;
import com.Ben12345rocks.VotingPlugin.Listeners.PlayerJoinEvent;
import com.Ben12345rocks.VotingPlugin.Listeners.PlayerVoteListener;
import com.Ben12345rocks.VotingPlugin.Listeners.SignChange;
import com.Ben12345rocks.VotingPlugin.Listeners.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Listeners.VotingPluginUpdateEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.Signs.SignHandler;
import com.Ben12345rocks.VotingPlugin.Signs.Signs;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;
import com.Ben12345rocks.VotingPlugin.Updater.CheckUpdate;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.Util.PlaceHolders.PlaceHolders;
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

	private LinkedHashMap<TopVoter, LinkedHashMap<User, Integer>> topVoter;

	/**
	 * @return the topVoter
	 */
	public LinkedHashMap<TopVoter, LinkedHashMap<User, Integer>> getTopVoter() {
		return topVoter;
	}

	public LinkedHashMap<User, Integer> getTopVoter(TopVoter top) {
		return topVoter.get(top);
	}

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
		AdvancedCoreHook.getInstance().getOptions().setStorageType(from);
		if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)
				&& AdvancedCoreHook.getInstance().getMysql() != null) {
			AdvancedCoreHook.getInstance().getMysql().clearCache();
		}
		ArrayList<String> uuids = new ArrayList<String>(UserManager.getInstance().getAllUUIDs());

		while (uuids.size() > 0) {
			HashMap<User, HashMap<String, String>> data = new HashMap<User, HashMap<String, String>>();
			AdvancedCoreHook.getInstance().getOptions().setStorageType(from);
			// AdvancedCoreHook.getInstance().setStorageType(to);

			if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)
					&& AdvancedCoreHook.getInstance().getMysql() != null) {
				AdvancedCoreHook.getInstance().getMysql().clearCache();
			}

			ArrayList<String> converted = new ArrayList<String>();
			int i = 0;
			while (i < 250 && i < uuids.size()) {
				String uuid = uuids.get(i);
				try {
					User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));

					HashMap<String, String> values = new HashMap<String, String>();
					for (String key : user.getData().getKeys()) {
						String value = user.getData().getValue(key);
						if (value != null && !value.isEmpty() && !value.equalsIgnoreCase("null")) {
							values.put(key, value);
						}
					}
					i++;
					converted.add(uuid);
					data.put(user, values);
					debug("[Convert] Added " + uuid);
				} catch (Exception e) {
					AdvancedCoreHook.getInstance().debug(e);
					plugin.getLogger().warning("Exception occoured for '" + uuid + "': " + e.getMessage()
							+ ", turn debug on to see full stack traces");
				}
			}

			try {
				wait(Config.getInstance().getConvertDelay());
			} catch (Exception e) {
			}

			uuids.removeAll(converted);

			plugin.getLogger()
					.info("Finished getting data from " + from.toString() + " Converting " + data.size() + " users");

			AdvancedCoreHook.getInstance().getOptions().setStorageType(to);
			if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)
					&& AdvancedCoreHook.getInstance().getMysql() != null) {
				AdvancedCoreHook.getInstance().getMysql().clearCache();
			}

			writeConvertData(data);
		}

		AdvancedCoreHook.getInstance().getOptions().setStorageType(cur);
		AdvancedCoreHook.getInstance().reload();

		plugin.getLogger().info("Finished convertting");
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

	public ArrayList<CommandHandler> getAdminVoteCommand() {
		return adminVoteCommand;
	}

	public ArrayList<SignHandler> getSigns() {
		return signs;
	}

	/*
	 * public LinkedHashMap<User, Integer> getTopVoterAllTime() { return
	 * topVoterAllTime; }
	 * 
	 * public LinkedHashMap<User, Integer> getTopVoterDaily() { return
	 * topVoterDaily; }
	 * 
	 * public LinkedHashMap<User, Integer> getTopVoterMonthly() { return
	 * topVoterMonthly; }
	 * 
	 * public LinkedHashMap<User, Integer> getTopVoterWeekly() { return
	 * topVoterWeekly; }
	 */

	public Updater getUpdater() {
		return updater;
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

	public ArrayList<CommandHandler> getVoteCommand() {
		return voteCommand;
	}

	public Logger getVoteLog() {
		return voteLog;
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

	public String getVoteSiteServiceSite(String name) {
		ArrayList<String> sites = ConfigVoteSites.getInstance().getVoteSitesNames();
		if (name == null) {
			return null;
		}
		if (sites != null) {
			for (String siteName : sites) {
				String URL = ConfigVoteSites.getInstance().getServiceSite(siteName);
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

	public List<VoteSite> getVoteSites() {
		return voteSites;
	}

	public LinkedHashMap<User, HashMap<VoteSite, LocalDateTime>> getVoteToday() {
		return voteToday;
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

	public boolean isUpdate() {
		return update;
	}

	public boolean isUpdateStarted() {
		return updateStarted;
	}

	private void loadTimer() {
		Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				AdvancedCoreHook.getInstance().getTimer().schedule(new TimerTask() {

					@Override
					public void run() {
						update();
					}
				}, 1000, 1000 * 60 * Config.getInstance().getDelayBetweenUpdates());
			}
		}, 40L);

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
				return Config.getInstance().getDataStorage().toUpperCase();
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
		AdvancedCoreHook.getInstance().setJenkinsSite("ben12345rocks.com");
		updateAdvancedCoreHook();
		AdvancedCoreHook.getInstance().loadHook(this);
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

		topVoter = new LinkedHashMap<TopVoter, LinkedHashMap<User, Integer>>();
		for (TopVoter top : TopVoter.values()) {
			topVoter.put(top, new LinkedHashMap<User, Integer>());
		}
		voteToday = new LinkedHashMap<User, HashMap<VoteSite, LocalDateTime>>();

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

		PlaceHolders.getInstance().load();

		// set columns
		if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL)) {
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("TopVoterIgnore", "VARCHAR(5)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("CheckWorld", "VARCHAR(5)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("Reminded", "VARCHAR(5)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("DisableBroadcast", "VARCHAR(5)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("LastOnline", "VARCHAR(20)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("PlayerName", "VARCHAR(20)");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("DailyTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("WeeklyTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("DayVoteStreak", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("BestDayVoteStreak", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("WeekVoteStreak", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("BestWeekVoteStreak", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("VotePartyVotes", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("MonthVoteStreak", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("Points", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("HighestDailyTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("MileStoneTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("AllTimeTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("HighestMonthlyTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("MilestoneCount", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("MonthTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("HighestWeeklyTotal", "INT DEFAULT '0'");
			AdvancedCoreHook.getInstance().getMysql().alterColumnType("LastMonthTotal", "INT DEFAULT '0'");

		}

		plugin.getLogger().info("Enabled VotingPlugin " + plugin.getDescription().getVersion());

		boolean hasRewards = RewardHandler.getInstance().hasRewards(ConfigVoteSites.getInstance().getData(),
				ConfigVoteSites.getInstance().getEverySiteRewardPath());

		boolean noIssues = true;
		ArrayList<String> services = ServerData.getInstance().getServiceSites();
		for (VoteSite site : getVoteSites()) {
			if (!site.hasRewards() && !hasRewards) {
				noIssues = false;
				plugin.getLogger().warning("No rewards detected for the site: " + site.getKey()
						+ ". See https://github.com/Ben12345rocks/AdvancedCore/wiki/Rewards on how to add rewards");
			}

			boolean contains = false;
			for (String service : services) {
				if (service.equalsIgnoreCase(site.getServiceSite())) {
					contains = true;
				}
			}
			if (!contains) {
				noIssues = false;
				plugin.getLogger().warning("No vote has been recieved from " + site.getServiceSite()
						+ ", may be an invalid service site. Vote on the site and look in console for a service site, if you get nothing then there is an issue with votifier");
			}
		}

		if (!noIssues) {
			Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().warning(
							"Detected an issue with voting sites, check the plugin startup log for more details");
				}
			}, 30l);
		}

		if (NMSManager.getInstance().isVersion("1.7", "1.8", "1.9", "1.10", "1.11", "1.12")) {
			plugin.getLogger().warning("Detected running " + NMSManager.getInstance().getVersion()
					+ ", this version is not supported on this build");
		}
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
		pm.registerEvents(new PlayerVoteListener(this), this);

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
		config.loadValues();
		configVoteSites.reloadData();
		updateAdvancedCoreHook();
		plugin.loadVoteSites();
		AdvancedCoreHook.getInstance().setConfigData(Config.getInstance().getData());
		AdvancedCoreHook.getInstance().reload();
		PlaceHolders.getInstance().load();
		// loadTimer();

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
			config.loadValues();
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
						if (uuid != null && !uuid.isEmpty()) {
							User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
							users.add(user);
							// AdvancedCoreHook.getInstance().extraDebug("Loading " + uuid);
							// java.lang.Thread.sleep(5000);
						}
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
		AdvancedCoreHook.getInstance().getJavascriptEngine().put("VotingPlugin", this);
		AdvancedCoreHook.getInstance().allowDownloadingFromSpigot(15358);
		AdvancedCoreHook.getInstance().setConfigData(Config.getInstance().getData());
	}

	private void writeConvertData(HashMap<User, HashMap<String, String>> data) {
		boolean checkInt = AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.MYSQL);
		for (Entry<User, HashMap<String, String>> entry : data.entrySet()) {
			try {
				for (Entry<String, String> values : entry.getValue().entrySet()) {
					String value = values.getValue();
					if (value != null && !value.equalsIgnoreCase("null")) {
						if (checkInt) {
							if (StringUtils.getInstance().isInt(value)) {
								entry.getKey().getData().setInt(values.getKey(), Integer.parseInt(value));
							} else {
								entry.getKey().getData().setString(values.getKey(), value);
							}
						} else {
							entry.getKey().getData().setString(values.getKey(), value);
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
				plugin.getLogger().warning("Exception occoured for '" + entry.getKey().getUUID() + "': "
						+ e.getMessage() + ", turn debug on to see full stack traces");
			}
		}
	}

}
