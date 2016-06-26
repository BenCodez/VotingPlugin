package com.Ben12345rocks.VotingPlugin;

import java.io.IOException;
import java.util.ArrayList;

import net.milkbowl.vault.economy.Economy;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;

import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteGUI;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteHelp;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteInfo;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteLast;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteNext;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteToday;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteTop;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVoteTotal;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AdminVoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteInfoTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteLastTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteNextTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteTotalTabCompleter;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBungeeVoting;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Data.UUIDs;
import com.Ben12345rocks.VotingPlugin.Events.BlockBreak;
import com.Ben12345rocks.VotingPlugin.Events.PlayerJoinEvent;
import com.Ben12345rocks.VotingPlugin.Events.SignChange;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Files.Files;
import com.Ben12345rocks.VotingPlugin.Metrics.Metrics;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.Signs.Signs;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.Updater.CheckUpdate;
import com.Ben12345rocks.VotingPlugin.Updater.Updater;

public class Main extends JavaPlugin {

	public static Config config;

	public static ConfigOtherRewards configBonusReward;

	public static ConfigGUI configGUI;

	public static ConfigFormat configFormat;

	public static ConfigVoteSites configVoteSites;

	public static Economy econ = null;

	public static Main plugin;

	public String[] topVoter;

	public Updater updater;

	public ArrayList<CommandHandler> voteCommand;

	public ArrayList<CommandHandler> adminVoteCommand;

	public ArrayList<VoteSite> voteSites;

	public String[] voteToday;

	public boolean placeHolderAPIEnabled;

	public ArrayList<Reward> rewards;

	public void checkPlaceHolderAPI() {
		if (Bukkit.getPluginManager().getPlugin("PlaceHolderAPI") != null) {
			placeHolderAPIEnabled = true;
		} else {
			placeHolderAPIEnabled = false;
		}
	}

	private void checkVotifier() {
		if (getServer().getPluginManager().getPlugin("Votifier") == null) {
			plugin.getLogger()
					.warning("Votifier not found, votes may not work");
		}
	}

	public User getUser(String playerName) {
		return new User(playerName);
	}

	public User getUser(UUID uuid) {
		return new User(uuid);
	}

	public VoteSite getVoteSite(String siteName) {
		for (VoteSite voteSite : voteSites) {
			if (voteSite.getSiteName().equalsIgnoreCase(siteName)) {
				return voteSite;
			}
		}
		if (!config.getDisableAutoCreateVoteSites()) {
			return new VoteSite(siteName);
		} else {
			return null;
		}
	}

	public void loadReminders() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(plugin,
				new Runnable() {

					@Override
					public void run() {
						for (Player player : Bukkit.getOnlinePlayers()) {
							if (player != null) {
								User user = new User(player);
								if (user.canVoteAll() && !user.reminded()) {

									user.loginMessage();
								}
							}
						}
					}
				}, 50, 60 * 20);
		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded Reminders");
		}
	}

	public void loadRewards() {
		for (String reward : ConfigRewards.getInstance().getRewardNames()) {
			rewards.add(new Reward(reward));
		}
		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded rewards");
		}
	}

	public void loadVoteSites() {
		configVoteSites.setup("Example");
		voteSites = configVoteSites.getVoteSitesLoad();
		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded VoteSites");
		}
	}

	private void metrics() {
		try {
			Metrics metrics = new Metrics(this);
			metrics.start();
			if (config.getDebugEnabled()) {
				plugin.getLogger().info("Loaded Metrics");
			}
		} catch (IOException e) {
			plugin.getLogger().info("Can't submit metrics stats");
		}
	}

	@Override
	public void onDisable() {
		plugin = null;
	}

	@Override
	public void onEnable() {
		plugin = this;
		Files.getInstance().loadFileEditngThread();
		setupFiles();
		registerCommands();
		registerEvents();
		setupEconomy();
		checkVotifier();
		metrics();

		CheckUpdate.getInstance().startUp();

		loadVoteSites();
		loadRewards();

		if (Config.getInstance().getRemindVotesEnabled()) {
			loadReminders();
		}

		topVoter = new String[1];
		voteToday = new String[1];
		startTimer();
		plugin.getLogger().info(
				"Enabled VotingPlgin " + plugin.getDescription().getVersion());
	}

	private void registerCommands() {
		CommandLoader.getInstance().loadCommands();

		// /vote, /v
		getCommand("vote").setExecutor(new CommandVote(this));
		getCommand("vote").setTabCompleter(new VoteTabCompleter());
		getCommand("v").setExecutor(new CommandVote(this));
		getCommand("v").setTabCompleter(new VoteTabCompleter());

		// /adminvote, /av
		getCommand("adminvote").setExecutor(new CommandAdminVote(this));
		getCommand("adminvote").setTabCompleter(new AdminVoteTabCompleter());
		getCommand("av").setExecutor(new CommandAdminVote(this));
		getCommand("av").setTabCompleter(new AdminVoteTabCompleter());

		// /votegui, /vgui
		getCommand("votegui").setExecutor(new CommandVoteGUI(this));

		// /votehelp, /vhelp
		getCommand("votehelp").setExecutor(new CommandVoteHelp(this));

		// /voteinfo, /vinfo
		getCommand("voteinfo").setExecutor(new CommandVoteInfo(this));
		getCommand("voteinfo").setTabCompleter(new VoteInfoTabCompleter());

		// /votelast, /vlast
		getCommand("votelast").setExecutor(new CommandVoteLast(this));
		getCommand("votelast").setTabCompleter(new VoteLastTabCompleter());

		// /votenext, /vnext
		getCommand("votenext").setExecutor(new CommandVoteNext(this));
		getCommand("votenext").setTabCompleter(new VoteNextTabCompleter());

		// /votetoday, /vtoday
		getCommand("votetoday").setExecutor(new CommandVoteToday(this));

		// /votetop, /vtop
		getCommand("votetop").setExecutor(new CommandVoteTop(this));

		// /votetotal, /vtotal
		getCommand("votetotal").setExecutor(new CommandVoteTotal(this));
		getCommand("votetotal").setTabCompleter(new VoteTotalTabCompleter());

		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded Commands");
		}

	}

	private void registerEvents() {
		PluginManager pm = getServer().getPluginManager();

		pm.registerEvents(new PlayerJoinEvent(this), this);
		pm.registerEvents(new VotiferEvent(this), this);

		pm.registerEvents(new SignChange(this), this);

		pm.registerEvents(new BlockBreak(this), this);

		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded Events");
		}
	}

	public void reload() {
		config.reloadData();
		configGUI.reloadData();
		configFormat.reloadData();
		plugin.loadVoteSites();
		configBonusReward.reloadData();
		plugin.setupFiles();
		plugin.update();
		ServerData.getInstance().reloadData();
	}

	private boolean setupEconomy() {
		if (getServer().getPluginManager().getPlugin("Vault") == null) {
			return false;
		}
		RegisteredServiceProvider<Economy> rsp = getServer()
				.getServicesManager().getRegistration(Economy.class);
		if (rsp == null) {
			return false;
		}
		econ = rsp.getProvider();
		return econ != null;
	}

	public void setupFiles() {
		config = Config.getInstance();
		configVoteSites = ConfigVoteSites.getInstance();
		configFormat = ConfigFormat.getInstance();
		configBonusReward = ConfigOtherRewards.getInstance();
		configGUI = ConfigGUI.getInstance();

		config.setup(this);
		configFormat.setup(this);
		configBonusReward.setup(this);
		configGUI.setup(plugin);

		ConfigBungeeVoting.getInstance().setup(plugin);

		ServerData.getInstance().setup(plugin);

		ConfigTopVoterAwards.getInstance().setup(plugin);

		UUIDs.getInstance().setup(plugin);
		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded Files");
		}
	}

	public void startTimer() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(plugin,
				new Runnable() {

					@Override
					public void run() {
						update();
					}
				}, 50, 600 * 20);
		if (config.getDebugEnabled()) {
			plugin.getLogger().info(
					"Loaded Timer for VoteTop, Updater, and VoteToday");
		}
	}

	public void update() {
		try {
			topVoter = TopVoter.getInstance().topVoters();
			updater = new Updater(this, 15358, false);
			voteToday = Commands.getInstance().voteToday();
			TopVoter.getInstance().checkTopVoterAward();
			Signs.getInstance().refreshSigns();
			for (Player player : Bukkit.getOnlinePlayers()) {
				new User(player).offVoteWorld(player.getWorld().getName());
			}
			if (config.getDebugEnabled()) {
				plugin.getLogger().info("Background task ran");
			}
		} catch (Exception ex) {
			plugin.getLogger()
					.info("Looks like there are no data files or something went wrong.");
			ex.printStackTrace();
		}
	}

}
