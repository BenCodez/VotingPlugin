package com.bencodez.votingplugin;

import java.io.File;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
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
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardEditData;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.rewards.RewardPlaceholderHandle;
import com.bencodez.advancedcore.api.rewards.injected.RewardInject;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectInt;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectValidator;
import com.bencodez.advancedcore.api.rewards.injectedrequirement.RequirementInjectConfigurationSection;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.simpleapi.file.YMLConfig;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.simpleapi.updater.Updater;
import com.bencodez.votingplugin.broadcast.BroadcastHandler;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask;
import com.bencodez.votingplugin.backendproxy.BackendProxyRewardRegistrar;
import com.bencodez.votingplugin.broadcast.BroadcastSettings;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.commands.executers.CommandAdminVote;
import com.bencodez.votingplugin.commands.executers.CommandVote;
import com.bencodez.votingplugin.commands.gui.AdminGUI;
import com.bencodez.votingplugin.commands.tabcompleter.AdminVoteTabCompleter;
import com.bencodez.votingplugin.commands.tabcompleter.AliasCommandFilterListener;
import com.bencodez.votingplugin.commands.tabcompleter.VoteTabCompleter;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.config.ShopFile;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.config.VotingPluginConfigHealth;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.discord.DiscordHandler;
import com.bencodez.votingplugin.listeners.BlockBreak;
import com.bencodez.votingplugin.integration.votifier.VotifierIntegration;
import com.bencodez.votingplugin.listeners.PlayerInteract;
import com.bencodez.votingplugin.listeners.PlayerJoinEvent;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;
import com.bencodez.votingplugin.listeners.SignChange;
import com.bencodez.votingplugin.listeners.VotiferEvent;
import com.bencodez.votingplugin.listeners.VotingPluginUpdateEvent;
import com.bencodez.votingplugin.placeholders.MVdWPlaceholders;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.VotingPluginExpansion;
import com.bencodez.votingplugin.presets.VoteSitePresetSetupHandler;
import com.bencodez.votingplugin.rewards.VotingPluginRewardRegistrar;
import com.bencodez.votingplugin.servicesites.ServiceSiteHandler;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.specialrewards.NameMCLikeCheckerTask;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.specialrewards.SpecialRewardsRewardRegistrar;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneRewardRegistrar;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesManager;
import com.bencodez.votingplugin.specialrewards.voteparty.VoteParty;
import com.bencodez.votingplugin.specialrewards.voteparty.VotePartyRewardRegistrar;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakRewardRegistrar;
import com.bencodez.votingplugin.test.VoteTester;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterHandler;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.topvoter.TopVoterState;
import com.bencodez.votingplugin.topvoter.TopVoterRewardRegistrar;
import com.bencodez.votingplugin.updater.CheckUpdate;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.version.VotingPluginVersionInfo;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogManager;
import com.bencodez.votingplugin.votelog.listeners.PlayerPostVoteLoggerListener;
import com.bencodez.votingplugin.votelog.listeners.PlayerSpecialRewardLoggerListener;
import com.bencodez.votingplugin.votelog.listeners.VoteMilestoneVoteLogListener;
import com.bencodez.votingplugin.votelog.listeners.VoteShopPurchaseLoggerListener;
import com.bencodez.votingplugin.votereminding.VoteRemindersLegacyMigrator;
import com.bencodez.votingplugin.votereminding.VoteRemindersListener;
import com.bencodez.votingplugin.votereminding.VoteReminderRewardRegistrar;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager;
import com.bencodez.votingplugin.votereminding.store.UserDataVoteReminderCooldownStore;
import com.bencodez.votingplugin.voteshop.VoteShopManager;
import com.bencodez.votingplugin.voteshop.VoteShopRewardRegistrar;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;
import com.bencodez.votingplugin.votesites.VoteSiteRewardRegistrar;
import com.bencodez.votingplugin.webhook.VotingPluginWebhooks;
import com.bencodez.votingplugin.webhook.VotingPluginWebhookManager;
import com.bencodez.votingplugin.webhook.WebhookRewardEntry;
import com.bencodez.votingplugin.webhook.WebhookRewardParser;

import lombok.Getter;
import lombok.Setter;
public class VotingPluginMain extends AdvancedCorePlugin {

	@Getter
	public static VotingPluginMain plugin;

	@Getter
	@Setter
	private ArrayList<CommandHandler> adminVoteCommand;

	@Getter
	private LinkedHashMap<java.util.UUID, ArrayList<String>> advancedTab = new LinkedHashMap<>();

	@Getter
	private BroadcastHandler broadcastHandler;

	@Getter
	private BackendProxyHandler backendProxyHandler;

	@Getter
	private BungeeSettings bungeeSettings;

	@Getter
	private CheckUpdate checkUpdate;

	@Getter
	private CommandLoader commandLoader;

	@Getter
	private NameMCLikeCheckerTask nameMCLikeCheckerTask;

	@Getter
	private Config configFile;

	@Getter
	private ConfigVoteSites configVoteSites;

	@Getter
	private CoolDownCheck coolDownCheck;

	@Getter
	private GUI gui;

	@Getter
	private ShopFile shopFile;

	@Getter
	private MVdWPlaceholders mvdwPlaceholders;

	@Getter
	private PlaceHolders placeholders;

	@Getter
	private VoteTester voteTester;



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
	private TopVoterHandler topVoterHandler;

	@Getter
	private TopVoterState topVoterState;


	@Getter
	@Setter
	private Updater updater;


	@Getter
	@Setter
	private VoteShopManager voteShopManager;

	@Getter
	@Setter
	private ArrayList<CommandHandler> voteCommand;

	@Getter
	private VoteParty voteParty;

	@Getter
	private VoteRemindersManager voteRemindersManager;

	@Getter
	private VoteSiteManager voteSiteManager;



	@Getter
	private ScheduledExecutorService voteTimer;

	@Getter
	private UserManager votingPluginUserManager;

	@Getter
	private TimeQueueHandler timeQueueHandler;

	@Getter
	private ServiceSiteHandler serviceSiteHandler;



	@Getter
	@Setter
	private VoteSitePresetSetupHandler presetHandler;

	@Getter
	private DiscordHandler discordHandler;

	private VotingPluginBackgroundTask backgroundTask;
	private VotingPluginVersionInfo versionInfo;
	private VotingPluginConfigHealth configHealth;
	private VotifierIntegration votifierIntegration;
	private VoteLogManager voteLogManager;
	private VotingPluginWebhookManager webhookManager;

	public boolean isUpdate() {
		return backgroundTask != null && backgroundTask.isRequested();
	}

	public void setUpdate(boolean update) {
		if (backgroundTask == null) {
			backgroundTask = new VotingPluginBackgroundTask(this);
		}
		backgroundTask.setRequested(update);
	}

	public boolean isUpdateStarted() {
		return backgroundTask != null && backgroundTask.isRunning();
	}

	public long getLastBackgroundTaskTimeTaken() {
		return backgroundTask == null ? -1 : backgroundTask.getLastRunTimeSeconds();
	}

	public void addDirectlyDefinedRewards(DirectlyDefinedReward directlyDefinedReward) {
		getRewardHandler().addDirectlyDefined(directlyDefinedReward);
	}


	public void basicBungeeUpdate() {
		for (Player player : Bukkit.getOnlinePlayers()) {
			VotingPluginUser user = getVotingPluginUserManager().getVotingPluginUser(player);
			user.cache();
			user.offVote();
			user.checkOfflineRewards();
		}
	}



	/**
	 * Check votifier.
	 */
	private void checkVotifier() {
		if (votifierIntegration == null) {
			votifierIntegration = new VotifierIntegration(this);
		}
		votifierIntegration.detect();
	}


	private void checkYMLError() {
		if (configHealth == null) {
			configHealth = new VotingPluginConfigHealth(this);
		}
		configHealth.check();
	}


	public String getProfile() {
		return versionInfo == null ? "" : versionInfo.getProfile();
	}

	public String getBuildNumber() {
		return versionInfo == null ? "NOTSET" : versionInfo.getBuildNumber();
	}

	public String getTime() {
		return versionInfo == null ? "" : versionInfo.getTime();
	}

	public boolean isYmlError() {
		return configHealth != null && configHealth.hasYmlError();
	}

	public boolean isVotifierLoaded() {
		return votifierIntegration == null || votifierIntegration.isLoaded();
	}

	public VoteLogMysqlTable getVoteLogMysqlTable() {
		return voteLogManager == null ? null : voteLogManager.getVoteLogMysqlTable();
	}

	public VotingPluginWebhooks getWebhooks() {
		return webhookManager == null ? null : webhookManager.getWebhooks();
	}

	public void loadVoteShopManager() {
		voteShopManager = new VoteShopManager(this);
	}



	@Override
	public FileConfiguration getConfig() {
		return configFile.getData();
	}

	public LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> getTopVoter() {
		return topVoterState.getTopVoters();
	}

	public void setTopVoter(LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> topVoter) {
		topVoterState.setTopVoters(topVoter);
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getTopVoter(TopVoter top) {
		return topVoterState.getTopVoters(top);
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getLastMonthTopVoter() {
		return topVoterState.getLastMonthTopVoters();
	}

	public LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> getPreviousMonthsTopVoters() {
		return topVoterState.getPreviousMonthsTopVoters();
	}

	public void setPreviousMonthsTopVoters(
			LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previousMonthsTopVoters) {
		topVoterState.setPreviousMonthsTopVoters(previousMonthsTopVoters);
	}

	public LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> getVoteToday() {
		return topVoterState.getVoteToday();
	}

	public void setVoteToday(LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday) {
		topVoterState.setVoteToday(voteToday);
	}

	/**
	 * Gets the user.
	 *
	 * @param uuid the uuid
	 * @return the user
	 */
	@Deprecated
	public VotingPluginUser getUser(UUID uuid) {
		return getVotingPluginUserManager().getVotingPluginUser(uuid);
	}



	@Deprecated
	public VoteSite getVoteSite(String site, boolean checkEnabled) {
		return voteSiteManager.getVoteSite(site, checkEnabled);
	}

	@Deprecated
	public String getVoteSiteName(boolean checkEnabled, String... urls) {
		return voteSiteManager.getVoteSiteName(checkEnabled, urls);
	}

	@Deprecated
	public ArrayList<VoteSite> getVoteSitesEnabled() {
		return voteSiteManager.getVoteSitesEnabled();
	}

	@Deprecated
	public String getVoteSiteServiceSite(String name) {
		return voteSiteManager.getVoteSiteServiceSite(name);
	}

	@Deprecated
	public boolean hasVoteSite(String site) {
		return voteSiteManager.hasVoteSite(site);
	}

	@Deprecated
	public boolean isVoteSite(String voteSite) {
		return voteSiteManager.isVoteSite(voteSite);
	}

	private void loadBungeeHandler() {
		backendProxyHandler = new BackendProxyHandler(this);
		backendProxyHandler.load();

		if (getOptions().getServer().equalsIgnoreCase("PleaseSet")) {
			getLogger().warning("Bungeecoord is true and server name is not set, bungeecoord features may not work");
		}
	}

	@Getter
	private VoteStreakHandler voteStreakHandler;

	/**
	 * Registers a directly editable rewards path from SpecialRewards.yml.
	 *
	 * @param rewardPath full configuration path to the rewards section
	 */
	public void loadDirectlyDefined() {
		getRewardHandler().getDirectlyDefinedRewards().clear();

		VoteReminderRewardRegistrar.register(this);
		SpecialRewardsRewardRegistrar.register(this);
		VoteSiteRewardRegistrar.register(this);
		VotePartyRewardRegistrar.register(this);
		VoteMilestoneRewardRegistrar.register(this);
		VoteStreakRewardRegistrar.register(this);
		TopVoterRewardRegistrar.register(this);
		VoteShopRewardRegistrar.register(this);
		BackendProxyRewardRegistrar.register(this);

		getRewardHandler().checkDirectlyDefined();
	}

	private void loadTimer() {
		plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				getTimer().scheduleWithFixedDelay(new Runnable() {

					@Override
					public void run() {
						if (plugin != null) {
							update();
						}
					}
				}, 1000 * 60 * 4,
						ParsedDuration.parse(configFile.getDelayBetweenUpdates(), TimeUnit.MINUTES).getMillis(),
						TimeUnit.MILLISECONDS);

				if (configFile.isExtraBackgroundUpdate()) {
					getTimer().scheduleWithFixedDelay(new Runnable() {

						@Override
						public void run() {
							if (plugin != null && configFile.isExtraBackgroundUpdate()) {
								basicBungeeUpdate();
							}
						}
					}, 1000, 1000 * 30, TimeUnit.MILLISECONDS);
				}
			}
		}, 2);

	}

	private void loadVersionFile() {
		versionInfo = new VotingPluginVersionInfo(this);
		versionInfo.load();
	}


	public void loadVoteSites() {
		configVoteSites.setup();
		voteSiteManager = new VoteSiteManager(this);
		voteSiteManager.loadVoteSites();

		plugin.debug("Loaded VoteSites");
	}

	private void loadVoteTimer() {
		voteTimer = Executors.newSingleThreadScheduledExecutor();
	}

	@Deprecated
	public List<VoteSite> getVoteSites() {
		return voteSiteManager.getVoteSites();
	}

	/**
	 * Metrics.
	 */
	private void metrics() {
		new VotingPluginMetrics().load(plugin);
	}

	@Override
	public void onPostLoad() {
		// auto conversion for Shop.yml
		if (plugin.getShopFile().isJustCreated()) {
			if (!plugin.getGui().isJustCreated() && !getServerData().isVoteShopConverted()) {
				plugin.getLogger().warning("Converting VoteShop configuration to Shop.yml from GUI.yml");
				plugin.getShopFile().convertFromGUIFile();
			}
			getServerData().setShopConverted(true);
		} else if (!getServerData().isVoteShopConverted()) {
			getServerData().setShopConverted(true);
		}

		// vote reminder migration
		VoteRemindersLegacyMigrator.migrateIfNeeded(this, new File(getDataFolder(), "Config.yml"),
				getConfigFile().getData());

		voteRemindersManager = new VoteRemindersManager(this, new UserDataVoteReminderCooldownStore(this));
		voteRemindersManager.reload();

		loadVersionFile();
		getOptions().setServer(bungeeSettings.getServer());

		// only purges if enabled in config
		getVotingPluginUserManager().purgeOldPlayersNoData();

		voteTester = new VoteTester(plugin);

		loadVoteTimer();

		if (bungeeSettings.isUseBungeecoord()) {
			loadBungeeHandler();
		}

		if (!bungeeSettings.isUseBungeecoord() || !bungeeSettings.isGloblalDataEnabled()) {
			this.timeQueueHandler = new TimeQueueHandler(this);
		}

		if (Bukkit.getPluginManager().getPlugin("PlaceholderAPI") != null) {
			if (getConfigFile().isLoadInteralExpansion()) {
				new VotingPluginExpansion(this).register();
				getLogger().info("Loading PlaceholderAPI expansion");
			}
		}
		
		voteMilestonesManager = new VoteMilestonesManager(this);
		
		voteStreakHandler = new VoteStreakHandler(this);
		voteStreakHandler.reload();

		registerCommands();
		checkVotifier();
		registerEvents();

		loadVoteBroadcast();

		loadVoteShopManager();

		loadDirectlyDefined();
		checkUpdate = new CheckUpdate(this);
		checkUpdate.startUp();
		specialRewards = new SpecialRewards(this);
		signs = new Signs(this);

		coolDownCheck.checkEnabled();
		coolDownCheck.load();

		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				signs.loadSigns();
			}
		});

		topVoterState = new TopVoterState();
		topVoterHandler = new TopVoterHandler(this);

		new AdminGUI(this).loadHook();

		// vote party
		voteParty = new VoteParty(this);
		voteParty.register();

		topVoterHandler.register();

		

		plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				metrics();
			}
		});

		// javascript api
		getJavascriptEngineRequests().add(new JavascriptPlaceholderRequest("User") {

			@Override
			public Object getObject(OfflinePlayer player) {
				return getVotingPluginUserManager().getVotingPluginUser(player);
			}
		});
		getJavascriptEngine().put("VotingPluginHooks", VotingPluginHooks.getInstance());
		getJavascriptEngine().put("VotingPlugin", this);

		loadTimer();

		// placeholderapi loading
		placeholders = new PlaceHolders(this);
		getServerData().updatePlaceholders();
		placeholders.load();

		if (Bukkit.getPluginManager().isPluginEnabled("MVdWPlaceholderAPI")) {
			mvdwPlaceholders = new MVdWPlaceholders(this);
			mvdwPlaceholders.loadMVdWPlaceholders();
		}

		if (Bukkit.getPluginManager().isPluginEnabled("DiscordSRV") && configFile.isDiscordSRVEnabled()) {
			discordHandler = new DiscordHandler(this);
			discordHandler.load();
			debug("DiscordSRV enabled, loading DiscordSRV handler");
		}

		// load vote logging if enabled
		loadVoteLoggingMySQL();

		webhookManager = new VotingPluginWebhookManager(this);
		webhookManager.load();

		if (getSpecialRewardsConfig().isNameMCLikeRewardEnabled()) {
			nameMCLikeCheckerTask = new NameMCLikeCheckerTask(this);

			long interval = Math.max(1, getSpecialRewardsConfig().getNameMCLikeRewardCheckIntervalMinutes()) * 60L
					* 20L;
			nameMCLikeCheckerTask.runTaskTimerAsynchronously(this, 20L, interval);
		}

		VotingPluginRewardRegistrar.register(this);

		plugin.getLogger().info("Enabled VotingPlugin " + plugin.getDescription().getVersion());
		if (plugin.getDescription().getVersion().contains("SNAPSHOT")) {
			plugin.getLogger().info(
					"Using dev build, this is not a stable build, use at your own risk. Build number: " + getBuildNumber());
		}

		boolean hasRewards = getRewardHandler().hasRewards(getConfigVoteSites().getData(),
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
			if (!contains && !getConfigFile().isDisableNoServiceSiteMessage()) {
				issues = false;
				plugin.getLogger().warning("No vote has been received from " + site.getServiceSite()
						+ ", may be an invalid service site. Please read: https://github.com/BenCodez/VotingPlugin/wiki/Votifier-Troubleshooting");
			}
		}

		if (!issues) {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getLogger().warning(
							"Detected an issue with voting sites, check the server startup log for more details: https://github.com/BenCodez/VotingPlugin/wiki/Votifier-Troubleshooting");
				}
			}, 5);

			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					serviceSiteHandler = new ServiceSiteHandler(plugin);
				}
			}, 10);
		}

	}

	private void migrateVoteBroadcast(Config configFile) {
		ConfigurationSection cfg = configFile.getData();
		// If new section exists, do nothing
		if (cfg.isConfigurationSection("VoteBroadcast")) {
			return;
		}

		// Create VoteBroadcast section
		org.bukkit.configuration.ConfigurationSection vb = cfg.createSection("VoteBroadcast");

		// Detect old AlternateBroadcast
		boolean altEnabled = cfg.getBoolean("Format.AlternateBroadcast.Enabled", false);
		int altDelay = cfg.getInt("Format.AlternateBroadcast.Delay", 30);
		String altMsg = cfg.getString("Format.AlternateBroadcast.Broadcast",
				"&6[Vote] &a%numberofplayers% voted recently! /vote");

		// Old single vote message
		String oldBroadcastMsg = cfg.getString("Format.BroadcastMsg",
				"&6[Vote] &aThanks &e%player% &afor voting on &e%SiteName%");

		// Map old -> new
		if (altEnabled) {
			vb.set("Type", "INTERVAL_SUMMARY_GLOBAL");
			vb.set("Duration", altDelay + "m");
			vb.set("MaxSitesListed", 0);

			org.bukkit.configuration.ConfigurationSection fmt = vb.createSection("Format");

			// Header uses the old interval broadcast line (make it clearer + include new
			// placeholders)
			fmt.set("Header", altMsg.replace("%numberofplayers%", "%numberofplayers%").replace("%players%", "%players%")
					.replace("%numberofsites%", "%numberofsites%").replace("%sites%", "%sites%"));

			// Default: list entries like "Player (N)" (the handler feeds that as item text)
			fmt.set("ListLine", "&7 - &6%site%");
			fmt.set("BroadcastMsg", "&6[Vote] &aThanks &e%player% &afor voting on &e%site%&a!");

		} else {
			vb.set("Type", "EVERY_VOTE");
			vb.set("Duration", "2m");
			vb.set("MaxSitesListed", 0);

			org.bukkit.configuration.ConfigurationSection fmt = vb.createSection("Format");
			fmt.set("BroadcastMsg", oldBroadcastMsg.replace("%SiteName%", "%site%")); // convert placeholder name
			fmt.set("Header", "&6[Vote] &aThanks &e%player% &afor voting on &e%sites_count% &asites:");
			fmt.set("ListLine", "&7 - &e%site%");
		}
		plugin.getLogger().info("Migrated vote broadcast settings to new format.");
		configFile.saveData();
	}

	@Override
	public void onPreLoad() {
		plugin = this;

		setupFiles();

		loadVoteSites();

		votingPluginUserManager = new UserManager(this);
		votingPluginUserManager.addCachingKeys();

		updateAdvancedCoreHook();

		addUserStartup(new UserStartup() {

			@Override
			public void onStartUp(AdvancedCoreUser user) {

			}

			@Override
			public void onStart() {

			}

			@Override
			public void onFinish() {
				topVoterHandler.loadLastMonth();

				topVoterHandler.loadPreviousMonthTopVoters();

				setUpdate(true);
				update();
			}
		});

	}

	@Override
	public void onUnLoad() {
		if (getBackendProxyHandler() != null) {
			try {
				getBackendProxyHandler().close();
			} catch (Exception e) {
				debug(e);
			}
		}
		if (webhookManager != null) {
			webhookManager.shutdown();
		}
		voteTimer.shutdown();
		try {
			voteTimer.awaitTermination(1, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		voteTimer.shutdownNow();
		if (timeQueueHandler != null) {
			timeQueueHandler.save();
		}

		if (voteRemindersManager != null) {
			voteRemindersManager.shutdown();
			voteRemindersManager = null;
		}

		if (coolDownCheck != null) {
			coolDownCheck.shutdown();
			coolDownCheck = null;
		}

		getSigns().storeSigns();
		HandlerList.unregisterAll(plugin);
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
			if (configFile.isGiveDefaultPermission()) {
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
		if (isVotifierLoaded()) {
			pm.registerEvents(new VotiferEvent(this), this);
		}
		pm.registerEvents(new PlayerVoteListener(this), this);
		pm.registerEvents(new PlayerPostVoteLoggerListener(this), this);
		pm.registerEvents(new PlayerSpecialRewardLoggerListener(this), this);
		pm.registerEvents(new VoteShopPurchaseLoggerListener(this), this);
		pm.registerEvents(new VoteMilestoneVoteLogListener(this), this);
		pm.registerEvents(new SignChange(this), this);
		pm.registerEvents(new BlockBreak(this), this);

		pm.registerEvents(new VoteRemindersListener(this), this);

		if (!plugin.getConfigFile().isDisableInteractEvent()) {
			pm.registerEvents(new PlayerInteract(this), this);
		}

		if (timeQueueHandler != null) {
			pm.registerEvents(timeQueueHandler, plugin);
		}

		pm.registerEvents(new VotingPluginUpdateEvent(this), this);
		/*
		 * if (!NMSManager.getInstance().isVersion("1.12")) { pm.registerEvents(new
		 * PlayerCommandSendListener(this), this); }
		 */
		coolDownCheck = new CoolDownCheck(this);
		pm.registerEvents(coolDownCheck, this);

		pm.registerEvents(new AliasCommandFilterListener(this), this);

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
		configFile.reloadData();
		configFile.loadValues();

		configVoteSites.reloadData();

		specialRewardsConfig.reloadData();

		voteMilestonesManager.reload();

		gui.reloadData();
		shopFile.reloadData();

		bungeeSettings.reloadData();
		updateAdvancedCoreHook();

		reloadAdvancedCore(userStorage);

		if (bungeeSettings.isUseBungeecoord()) {
			if (getBackendProxyHandler() == null) {
				loadBungeeHandler();
			} else {
				getBackendProxyHandler().reloadPresenceReporting();
			}
			if (userStorage) {
				getBackendProxyHandler().loadGlobalMysql();
			}
		} else if (getBackendProxyHandler() != null) {
			getBackendProxyHandler().disablePresenceReporting();
		}
		checkYMLError();

		plugin.loadVoteSites();

		getOptions().setServer(bungeeSettings.getServer());
		if (userStorage) {
			placeholders.load();
			placeholders.reload();
		}

		if (voteRemindersManager != null) {
			voteRemindersManager.reload();
		}

		if (webhookManager != null) { webhookManager.reload(); }

		coolDownCheck.checkEnabled();

		getVoteStreakHandler().reload();

		loadVoteBroadcast();

		voteShopManager.reload();

		loadDirectlyDefined();

		setUpdate(true);
	}

	private void loadVoteBroadcast() {
		ConfigurationSection sec = getConfigFile().getData().getConfigurationSection("VoteBroadcast");
		BroadcastSettings settings = BroadcastSettings.load(sec);

		if (broadcastHandler == null) {
			// Backend servers only: create once
			broadcastHandler = new BroadcastHandler(this, settings, ZoneId.systemDefault());
		} else {
			// Reload-safe: just update settings + reschedule interval if needed
			broadcastHandler.setSettings(settings);
		}
	}

	private void setupFiles() {
		configFile = new Config(this);
		configFile.setup();
		configFile.setIgnoreCase(plugin.getConfigFile().isCaseInsensitiveYMLFiles());
		configFile.reloadData();

		migrateVoteBroadcast(configFile);

		configVoteSites = new ConfigVoteSites(this);
		configVoteSites.setup();

		specialRewardsConfig = new SpecialRewardsConfig(this);
		specialRewardsConfig.setup();

		bungeeSettings = new BungeeSettings(this);
		bungeeSettings.setup();

		serverData = new ServerData(this);

		gui = new GUI(this);
		gui.setup();

		shopFile = new ShopFile(this);
		shopFile.setup();

		checkYMLError();

		plugin.debug("Loaded Files");
	}

	public synchronized void update() {
		if (backgroundTask == null) {
			backgroundTask = new VotingPluginBackgroundTask(this);
		}
		backgroundTask.run();
	}



	@Getter
	private VoteMilestonesManager voteMilestonesManager;


	public void loadVoteLoggingMySQL() {
		if (voteLogManager == null) {
			voteLogManager = new VoteLogManager(this);
		}
		voteLogManager.load();
	}


	public void updateAdvancedCoreHook() {
		getJavascriptEngine().put("VotingPlugin", this);
		allowDownloadingFromSpigot(15358);
		setConfigData(new YMLConfig(this, null) {
			@Override
			public ConfigurationSection getData() {
				return configFile.getData();
			}

			@Override
			public void createSection(String key) {
				configFile.createSection(key);
			}

			@Override
			public void saveData() {
				configFile.saveData();
			}

			@Override
			public void setValue(String path, Object value) {
				configFile.setValue(path, value);
			}
		});
		if (bungeeSettings.isUseBungeecoord()) {
			getOptions().setPerServerRewards(getBungeeSettings().isPerServerRewards());
		}
	}

}
