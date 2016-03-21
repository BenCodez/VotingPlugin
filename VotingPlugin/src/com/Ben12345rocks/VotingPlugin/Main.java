package com.Ben12345rocks.VotingPlugin;

import java.io.IOException;

import net.milkbowl.vault.economy.Economy;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;

import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AdminVoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.VoteTabCompleter;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Events.PlayerJoinEvent;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Metrics.Metrics;
import com.Ben12345rocks.VotingPlugin.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.Updater.CheckUpdate;
import com.Ben12345rocks.VotingPlugin.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.UserData.UUIDs;

public class Main extends JavaPlugin {

	public static Main plugin;

	public static Economy econ = null;

	public Updater updater;

	public String[] topVoter;

	public String[] voteToday;

	public static Config config;

	public static ConfigVoteSites configVoteSites;

	public static ConfigFormat configFormat;

	public static ConfigBonusReward configBonusReward;

	public void onEnable() {
		plugin = this;
		setupFiles();
		registerCommands();
		registerEvents();
		setupEconomy();
		checkVotifier();
		metrics();

		CheckUpdate.getInstance().startUp();

		loadBungee();

		if (Config.getInstance().getRemindVotesEnabled()) {
			loadReminders();
		}

		topVoter = new String[1];
		voteToday = new String[1];
		startTimer();
	}

	public void onDisable() {
		plugin = null;
	}

	public void setupFiles() {
		config = Config.getInstance();

		configVoteSites = ConfigVoteSites.getInstance();

		configFormat = ConfigFormat.getInstance();

		configBonusReward = ConfigBonusReward.getInstance();

		config.setup(this);
		configVoteSites.setup(this);
		configFormat.setup(this);
		configBonusReward.setup(this);

		UUIDs.getInstance().setup(plugin);
	}

	private void registerCommands() {
		this.getCommand("vote").setExecutor(new CommandVote(this));
		this.getCommand("vote").setTabCompleter(new VoteTabCompleter());

		this.getCommand("adminvote").setExecutor(new CommandAdminVote(this));
		this.getCommand("adminvote").setTabCompleter(
				new AdminVoteTabCompleter());
		
		this.getCommand("v").setExecutor(new CommandVote(this));
		this.getCommand("v").setTabCompleter(new VoteTabCompleter());

		this.getCommand("av").setExecutor(new CommandAdminVote(this));
		this.getCommand("av").setTabCompleter(
				new AdminVoteTabCompleter());
	}

	private void registerEvents() {
		PluginManager pm = getServer().getPluginManager();

		pm.registerEvents(new PlayerJoinEvent(this), this);
		pm.registerEvents(new VotiferEvent(this), this);
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

	private void checkVotifier() {
		if (getServer().getPluginManager().getPlugin("Votifier") == null) {
			plugin.getLogger().warning(
					"Votifier not found, votes will not work!");
		}
	}

	private void metrics() {
		try {
			Metrics metrics = new Metrics(this);
			metrics.start();
		} catch (IOException e) {
			plugin.getLogger().info("Can't submit metrics stats");
		}
	}

	public void loadBungee() {
		BungeeVote.getInstance().registerBungeeVoting();
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
	}

	public void startTimer() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(plugin,
				new Runnable() {

					@Override
					public void run() {
						updateTopUpdater();
					}
				}, 50, 600 * 20);
	}

	public void updateTopUpdater() {
		try {
			topVoter = TopVoter.getInstance().topVoters();
			updater = new Updater(this, 15358, false);
			voteToday = Commands.getInstance().voteToday();
		} catch (Exception ex) {
			plugin.getLogger()
					.info("Looks like there are no data files or something went wrong. If this is your first time installing this plugin ignore this");
			ex.printStackTrace();
		}
	}

	public User getUser(String playerName) {
		return new User(playerName);
	}

	public User getUser(UUID uuid) {
		return new User(uuid);
	}

}
