package com.Ben12345rocks.VotingPlugin;

import java.io.IOException;
import java.util.ArrayList;

import net.milkbowl.vault.economy.Economy;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;

import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
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
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBungeeVoting;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
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
import com.Ben12345rocks.VotingPlugin.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.Signs.Signs;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.Updater.CheckUpdate;
import com.Ben12345rocks.VotingPlugin.Updater.Updater;

public class Main extends JavaPlugin {

	public static Config config;

	public static ConfigBonusReward configBonusReward;

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
		return new VoteSite(siteName);
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
		if (config.getDebugEnabled()) {
			plugin.getLogger().info("Loaded Reminders");
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
		loadBungee();

		if (Config.getInstance().getRemindVotesEnabled()) {
			loadReminders();
		}

		topVoter = new String[1];
		voteToday = new String[1];
		startTimer();
		plugin.getLogger().info(
				"Enabled VotingPlgin " + plugin.getDescription().getVersion());
	}

	private void loadVoteCommand() {
		voteCommand = new ArrayList<CommandHandler>();
		voteCommand.add(new CommandHandler(new String[] { "Help" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().help(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Info" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().infoSelf(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Info", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().infoOther(sender, args[1]);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Last", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().lastOther(sender, args[1]);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Last" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().lastSelf(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Next", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().nextOther(sender, args[1]);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Next" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().nextSelf(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "GUI" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteGUI(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Today", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().today(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Today" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender, 1);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Top", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().today(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Top" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender, 1);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Total", "All" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalAll(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Total", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalOther(sender, args[1]);

			}
		});

		voteCommand.add(new CommandHandler(new String[] { "Total" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalSelf(sender);

			}
		});

		voteCommand.add(new CommandHandler(new String[] {}) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().voteURLs(sender);

			}
		});
	}

	private void loadAdminVoteCommand() {
		adminVoteCommand = new ArrayList<CommandHandler>();
		adminVoteCommand.add(new CommandHandler(new String[] {}) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});
		adminVoteCommand.add(new CommandHandler(new String[] { "Help" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "?" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Reload" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().reload(sender);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Version" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().version(sender);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Sites" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().sites(sender);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Sites",
				"sitename" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().site(sender, args[1]);

			}
		});

		adminVoteCommand.add(new CommandHandler(
				new String[] { "UUID", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().uuid(sender, args[1]);

			}
		});

		adminVoteCommand
				.add(new CommandHandler(new String[] { "Reset", "Top" }) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						CommandAdminVote.getInstance().resetTop(sender);

					}
				});

		adminVoteCommand.add(new CommandHandler(new String[] { "Vote",
				"sitename", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().globalVote(sender, args[1],
						args[2]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BungeeVote",
				"sitename", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().bungeeVote(sender, args[1],
						args[2]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "ServerVote",
				"sitename", "player" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().serverVote(sender, args[1],
						args[2]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSites",
				"sitename", "Create" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().createVoteSite(sender, args[1]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddItem", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardItem(sender,
						args[2]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"SetMoney", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[2])) {
					CommandAdminVote.getInstance().setBonusRewardMoney(sender,
							Integer.parseInt(args[2]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[2] + ", number expected"));
				}

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"SetGiveBonusReward", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setGiveBonusReward(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetDebug", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDebug(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetBroadcastVote", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigBroadcastVote(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetUpdateReminder", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigUpdateReminder(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetAllowUnjoined", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigAllowUnjoined(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetDisableTopVoterAwards", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDisableTopVoterAwards(
						sender, Boolean.parseBoolean(args[2]));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "ServerData",
				"SetPrevMonth", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[2])) {
					CommandAdminVote.getInstance().setServerDataPrevMonth(
							sender, Integer.parseInt(args[2]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[2] + ", number expected"));
				}

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddCommandPlayer", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardCommandPlayer(
						sender, Utils.getInstance().makeString(2, args));
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddCommandConsole", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardCommandConsole(
						sender, Utils.getInstance().makeString(2, args));
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddExtraRewardItem", "string", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardExtraRewardItem(
						sender, args[2], args[3]);
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"SetExtraRewardMoney", "string", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance()
							.setBonusRewardExtraRewardMoney(sender, args[2],
									Integer.parseInt(args[3]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"SetExtraRewardChance", "string", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance()
							.setBonusRewardExtraRewardChance(sender, args[2],
									Integer.parseInt(args[3]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "SetTotal",
				"player", "sitename", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().setTotal(sender, args[1],
							args[2], Integer.parseInt(args[3]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "AddItem", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addVoteSiteItem(sender, args[1],
						args[3]);
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetMoney", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().setVoteSiteMoney(sender,
							args[1], Integer.parseInt(args[3]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetServiceSite", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteServiceSite(sender,
						args[1], args[3]);
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetVoteURL", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteVoteURL(sender,
						args[1], args[3]);
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetDisabled", "boolean" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteDsiabled(sender,
						args[1], Boolean.parseBoolean(args[3]));
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetPriority", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().setVoteSitePriority(sender,
							args[1], Integer.parseInt(args[3]));
				} else {
					sender.sendMessage("&c" + args[3] + " is not an int");
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetVoteDelay", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().setVoteSiteVoteDelay(sender,
							args[1], Integer.parseInt(args[3]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "AddCommandPlayer", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().addVoteSiteCommandPlayer(
							sender, args[1],
							Utils.getInstance().makeString(3, args));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "AddCommandConsole", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance().addVoteSiteCommandConsole(
							sender, args[1],
							Utils.getInstance().makeString(3, args));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddExtraRewardCommandPlayer", "string", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
						.addBonusRewardExtraRewardCommandPlayer(sender,
								args[2],
								Utils.getInstance().makeString(3, args));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "BonusReward",
				"AddExtraRewardCommandConsole", "string", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
						.addBonusRewardExtraRewardCommandConsole(sender,
								args[2],
								Utils.getInstance().makeString(3, args));

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetExtraRewardChance", "string", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[3])) {
					CommandAdminVote.getInstance()
							.setVoteSiteExtraRewardChance(sender, args[1],
									args[3], Integer.parseInt(args[4]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[3] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetExtraRewardItem", "string", "string" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().addVoteSiteExtraRewardItem(
						sender, args[1], args[3], args[4]);

			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "SetExtraRewardMoney", "string", "number" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[4])) {
					CommandAdminVote.getInstance()
							.setVoteSiteExtraRewardMoney(sender, args[1],
									args[3], Integer.parseInt(args[4]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[4] + ", number expected"));
				}
			}
		});

		adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite",
				"sitename", "AddExtraRewardCommandPlayer", "string", "list" }) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
						.addVoteSiteExtraRewardCommandPlayer(sender, args[1],
								args[3],
								Utils.getInstance().makeString(4, args));

			}
		});

		adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "sitename",
						"AddExtraRewardCommandConsole", "string", "list" }) {

					@Override
					public void execute(CommandSender sender, String[] args) {

						CommandAdminVote
								.getInstance()
								.addVoteSiteExtraRewardCommandConsole(sender,
										args[1], args[3],
										Utils.getInstance().makeString(4, args));

					}
				});

	}

	private void registerCommands() {
		loadVoteCommand();
		loadAdminVoteCommand();

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
		plugin.updateTopUpdater();
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
		configBonusReward = ConfigBonusReward.getInstance();
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
						updateTopUpdater();
					}
				}, 50, 600 * 20);
		if (config.getDebugEnabled()) {
			plugin.getLogger().info(
					"Loaded Timer for VoteTop, Updater, and VoteToday");
		}
	}

	public void updateTopUpdater() {
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
				plugin.getLogger().info(
						"Updated VoteTop, Updater, and VoteToday");
			}
		} catch (Exception ex) {
			plugin.getLogger()
					.info("Looks like there are no data files or something went wrong.");
			ex.printStackTrace();
		}
	}

}
