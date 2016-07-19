package com.Ben12345rocks.VotingPlugin.Commands;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAliases;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AliasesTabCompleter;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Report.Report;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandLoader.
 */
public class CommandLoader {

	/** The other reward. */
	static ConfigOtherRewards otherReward = ConfigOtherRewards.getInstance();

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

	/** The instance. */
	static CommandLoader instance = new CommandLoader();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of CommandLoader.
	 *
	 * @return single instance of CommandLoader
	 */
	public static CommandLoader getInstance() {
		return instance;
	}

	/** The commands. */
	private HashMap<String, CommandHandler> commands;

	/**
	 * Instantiates a new command loader.
	 */
	private CommandLoader() {
	}

	/**
	 * Instantiates a new command loader.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandLoader(Main plugin) {
		CommandLoader.plugin = plugin;
	}

	/**
	 * Gets the commands.
	 *
	 * @return the commands
	 */
	public HashMap<String, CommandHandler> getCommands() {
		return commands;
	}

	/**
	 * Load admin vote command.
	 */
	private void loadAdminVoteCommand() {
		plugin.adminVoteCommand = new ArrayList<CommandHandler>();
		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"TriggerPlayerVoteEvent", "(player)", "(sitename)" },
				"VotingPlugin.Commands.AdminVote.TriggerPlayerVoteEvent",
				"Trigger vote event, used for testing") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin
						.getVoteSite(args[2]), new User(args[1]));
				plugin.getServer().getPluginManager().callEvent(voteEvent);

			}
		});
		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Help&?" },
				"VotingPlugin.Commands.AdminVote.Help", "See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender, 1);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Help&?",
				"(number)" }, "VotingPlugin.Commands.AdminVote.Help",
				"See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender,
						Integer.parseInt(args[1]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Report" },
				"VotingPlugin.Commands.AdminVote.Report",
				"Create a zip file to send for debuging") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Report.getInstance().create();
				sender.sendMessage("Created Zip File!");

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Perms" },
				"VotingPlugin.Commands.AdminVote.Perms", "List perms") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().permList(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Reload" },
				"VotingPlugin.Commands.AdminVote.Reload", "Reload plugin") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().reload(sender);

			}
		});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "Version" },
						"VotingPlugin.Commands.AdminVote.Version",
						"List version info") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						CommandAdminVote.getInstance().version(sender);

					}
				});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Sites" },
				"VotingPlugin.Commands.AdminVote.Sites", "List VoteSites") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().sites(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Sites",
				"(sitename)" }, "VotingPlugin.Commands.AdminVote.Sites.Site",
				"View Site Info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().site(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Rewards" },
				"VotingPlugin.Commands.AdminVote.Rewards", "List VoteSites") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().rewards(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"Rewards", "(reward)" },
				"VotingPlugin.Commands.AdminVote.Rewards.Reward",
				"View Site Info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().reward(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "UUID",
				"(player)" }, "VotingPlugin.Commands.AdminVote.UUID",
				"View UUID of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().uuid(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Reset",
				"Totals" }, "VotingPlugin.Commands.AdminVote.Reset.Total",
				"Reset totals for all players") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().resetTotals(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Reset",
				"Totals", "(player)" },
				"VotingPlugin.Commands.AdminVote.Reset.Total.Player",
				"Reset total for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().resetPlayerTotals(sender,
						args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Vote",
				"(player)", "(sitename)" },
				"VotingPlugin.Commands.AdminVote.Vote", "Trigger manual vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().Vote(sender, args[1], args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "Create" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Create VoteSite") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().createVoteSite(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetDebug", "(boolean)" },
				"VotingPlugin.Commands.AdminVote.Config.Edit",
				"Set Debug on or off") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDebug(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"ServerData", "SetPrevMonth", "(number)" },
				"VotingPlugin.Commands.AdminVote.ServerDta.Edit",
				"Edit PrevMonth, ADVANCED USERS ONLY") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setServerDataPrevMonth(sender,
						Integer.parseInt(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"SetTotal", "(player)", "(sitename)", "(number)" },
				"VotingPlugin.Commands.AdminVote.Set.Total",
				"Set Total votes of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setTotal(sender, args[1],
						args[2], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "SetServiceSite", "(string)" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Set VoteSite SerivceSite") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteServiceSite(sender,
						args[1], args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "SetVoteURL", "(string)" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Set VoteSite VoteURL") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteVoteURL(sender,
						args[1], args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "SetPriority", "(number)" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Set VoteSite Priority") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSitePriority(sender,
						args[1], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "SetVoteDelay", "(number)" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Set VoteSite VoteDelay") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteVoteDelay(sender,
						args[1], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "UpdateCheck" },
				"VotingPlugin.Commands.AdminVote.UpdateCheck",
				"Check for update") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

							@Override
							public void run() {
								sender.sendMessage(Utils.getInstance()
										.colorize("&cChecking for update..."));
								CommandAdminVote.getInstance().checkUpdate(
										sender);
							}
						});

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "SetEnabled", "(boolean)" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit",
				"Set VoteSite Enabled") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteEnabled(sender,
						args[1], Boolean.parseBoolean(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "(sitename)", "Check" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Check",
				"Check to see if VoteSite is valid") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().checkVoteSite(sender, args[1]);

			}
		});

	}

	/**
	 * Load aliases.
	 */
	public void loadAliases() {
		commands = new HashMap<String, CommandHandler>();
		for (CommandHandler cmdHandle : plugin.voteCommand) {
			if (cmdHandle.getArgs().length > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");
				for (String arg : args) {
					try {
						plugin.getCommand("vote" + arg).setExecutor(
								new CommandAliases(cmdHandle));

						plugin.getCommand("vote" + arg).setTabCompleter(
								new AliasesTabCompleter()
										.setCMDHandle(cmdHandle));
					} catch (Exception ex) {
						plugin.debug("Failed to load command and tab completer for /vote"
								+ arg);
					}
				}

			}
		}

		for (CommandHandler cmdHandle : plugin.adminVoteCommand) {
			if (cmdHandle.getArgs().length > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");
				for (String arg : args) {
					try {
						plugin.getCommand("adminvote" + arg).setExecutor(
								new CommandAliases(cmdHandle));

						plugin.getCommand("adminvote" + arg).setTabCompleter(
								new AliasesTabCompleter()
										.setCMDHandle(cmdHandle));
					} catch (Exception ex) {
						plugin.debug("Failed to load command and tab completer for /adminvote"
								+ arg);

					}
				}

			}
		}
	}

	/**
	 * Load commands.
	 */
	public void loadCommands() {
		loadAdminVoteCommand();
		loadVoteCommand();
	}

	/**
	 * Load vote command.
	 */
	private void loadVoteCommand() {
		plugin.voteCommand = new ArrayList<CommandHandler>();
		plugin.voteCommand.add(new CommandHandler(new String[] { "Help&?" },
				"VotingPlugin.Commands.Vote.Help", "View this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().help(sender);

			}
		});
		plugin.voteCommand.add(new CommandHandler(new String[] { "URL" },
				"VotingPlugin.Commands.Vote.URL", "Open VoteURL GUI") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteURL(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Reward" },
				"VotingPlugin.Commands.Vote.Reward", "Open VoteReward GUI") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteReward(sender, "");

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Reward",
				"(SiteName)" }, "VotingPlugin.Commands.Vote.Reward",
				"Open VoteURL GUI for VoteSIte") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteReward(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Info" },
				"VotingPlugin.Commands.Vote.Info", "See player info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().infoSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Info",
				"(player)" }, "VotingPlugin.Commands.Vote.Info.Other",
				"See other players info") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().infoOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last",
				"(player)" }, "VotingPlugin.Commands.Vote.Last.Other",
				"See other players last votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().lastOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last" },
				"VotingPlugin.Commands.Vote.Last", "See your last votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().lastSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next",
				"(player)" }, "VotingPlugin.Commands.Vote.Next.Other",
				"See other players next votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().nextOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Points",
				"(player)" }, "VotingPlugin.Commands.Vote.Points.Other",
				"View pints of other player") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance()
						.pointsOther(sender, new User(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Points", },
				"VotingPlugin.Commands.Vote.Points", "View your points") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				if (sender instanceof Player) {
					CommandVote.getInstance().pointsSelf(
							new User((Player) sender));
				} else {
					sender.sendMessage("Must be a player to use this!");
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next" },
				"VotingPlugin.Commands.Vote.Next", "See your next votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().nextSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "GUI" },
				"VotingPlugin.Commands.Vote.GUI", "Open VoteGUI") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteGUI(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top" },
				"VotingPlugin.Commands.Vote.Top", "Open list of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().topVoterMonthly(sender, 1);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top",
				"(number)" }, "VotingPlugin.Commands.Vote.Top",
				"Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterMonthly(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top",
				"(number)", "Monthly" },
				"VotingPlugin.Commands.Vote.Top.Monthly",
				"Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterMonthly(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top",
				"(number)", "Weekly" },
				"VotingPlugin.Commands.Vote.Top.Weekly",
				"Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterWeekly(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top",
				"(number)", "Daily" }, "VotingPlugin.Commands.Vote.Top.Daily",
				"Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Utils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterDaily(sender,
							Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Party" },
				"VotingPlugin.Commands.Vote.Party",
				"View current amount of votes and how many more needed") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VoteParty.getInstance().commandVoteParty(sender);
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today",
				"(number)" }, "VotingPlugin.Commands.Vote.Today",
				"Open page of who Voted Today") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender,
						Integer.parseInt(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today" },
				"VotingPlugin.Commands.Vote.Today",
				"View who list of who voted today") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender, 1);

			}
		});

		plugin.voteCommand.add(new CommandHandler(
				new String[] { "Total", "All" },
				"VotingPlugin.Commands.Vote.Total.All",
				"View server total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalAll(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total",
				"(player)" }, "VotingPlugin.Commands.Vote.Total.Other",
				"View other players total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total" },
				"VotingPlugin.Commands.Vote.Total", "View your total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] {}, "",
				"See voting URLs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getVoteURLDefault()) {
					CommandVote.getInstance().voteURLs(sender);
				} else {
					CommandVote.getInstance().voteURL(sender);
				}

			}
		});
	}

	/**
	 * Sets the commands.
	 *
	 * @param commands
	 *            the commands
	 */
	public void setCommands(HashMap<String, CommandHandler> commands) {
		this.commands = commands;
	}
}
