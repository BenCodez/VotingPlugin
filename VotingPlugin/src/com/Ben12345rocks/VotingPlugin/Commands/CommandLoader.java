package com.Ben12345rocks.VotingPlugin.Commands;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Report.Report;

public class CommandLoader {

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static CommandLoader instance = new CommandLoader();

	static Main plugin = Main.plugin;

	public static CommandLoader getInstance() {
		return instance;
	}

	private CommandLoader() {
	}

	public CommandLoader(Main plugin) {
		CommandLoader.plugin = plugin;
	}

	private void loadAdminVoteCommand() {
		plugin.adminVoteCommand = new ArrayList<CommandHandler>();
		plugin.adminVoteCommand.add(new CommandHandler(new String[] {},
				"VotingPlugin.Commands.AdminVote.Help") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});
		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Help" },
				"VotingPlugin.Commands.AdminVote.Help") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});
		
		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Report" },
				"VotingPlugin.Commands.AdminVote.Report") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Report.getInstance().create();

			}
		});
		
		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Perms" },
				"VotingPlugin.Commands.AdminVote.Perms") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().permList(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "?" },
				"VotingPlugin.Commands.AdminVote.Help") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Reload" },
				"VotingPlugin.Commands.AdminVote.Reload") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().reload(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Version" },
				"VotingPlugin.Commands.AdminVote.Version") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().version(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "Sites" },
				"VotingPlugin.Commands.AdminVote.Sites") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().sites(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Sites",
		"sitename" }, "VotingPlugin.Commands.AdminVote.Sites.Site") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().site(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "UUID",
		"player" }, "VotingPlugin.Commands.AdminVote.UUID") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().uuid(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Reset",
		"Top" }, "VotingPlugin.Commands.AdminVote.Reset.Top") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().resetTop(sender);

			}
		});

		plugin.adminVoteCommand
		.add(new CommandHandler(new String[] { "Vote", "player",
		"sitename" }, "VotingPlugin.Commands.AdminVote.Vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().globalVote(sender,
						args[1], args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BungeeVote", "player", "sitename" },
				"VotingPlugin.Commands.AdminVote.BungeeVote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().bungeeVote(sender, args[1],
						args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"ServerVote", "player", "sitename" },
				"VotingPlugin.Commands.AdminVote.ServerVote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().serverVote(sender, args[1],
						args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSites", "sitename", "Create" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().createVoteSite(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "AddItem", "string" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardItem(sender,
						args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "SetMoney", "number" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setBonusRewardMoney(sender,
						Integer.parseInt(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "SetGiveBonusReward", "boolean" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setGiveBonusReward(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetDebug", "boolean" },
				"VotingPlugin.Commands.AdminVote.Config.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDebug(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetBroadcastVote", "boolean" },
				"VotingPlugin.Commands.AdminVote.Config.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigBroadcastVote(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetUpdateReminder", "boolean" },
				"VotingPlugin.Commands.AdminVote.Config.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigUpdateReminder(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetAllowUnjoined", "boolean" },
				"VotingPlugin.Commands.AdminVote.Config.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigAllowUnjoined(sender,
						Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config",
				"SetDisableTopVoterAwards", "boolean" },
				"VotingPlugin.Commands.AdminVote.Config.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDisableTopVoterAwards(
						sender, Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"ServerData", "SetPrevMonth", "number" },
				"VotingPlugin.Commands.AdminVote.ServerDta.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setServerDataPrevMonth(sender,
						Integer.parseInt(args[2]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "AddCommandPlayer", "list" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardCommandPlayer(
						sender, Utils.getInstance().makeString(2, args));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "AddCommandConsole", "list" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardCommandConsole(
						sender, Utils.getInstance().makeString(2, args));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "AddExtraRewardItem", "string", "string" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addBonusRewardExtraRewardItem(
						sender, args[2], args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "SetExtraRewardMoney", "string", "number" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setBonusRewardExtraRewardMoney(
						sender, args[2], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"BonusReward", "SetExtraRewardChance", "string", "number" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setBonusRewardExtraRewardChance(
						sender, args[2], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"SetTotal", "player", "sitename", "number" },
				"VotingPlugin.Commands.AdminVote.Set.Total") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setTotal(sender, args[1],
						args[2], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "AddItem", "string" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().addVoteSiteItem(sender, args[1],
						args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetMoney", "number" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteMoney(sender,
						args[1], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetServiceSite", "string" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteServiceSite(sender,
						args[1], args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetVoteURL", "string" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteVoteURL(sender,
						args[1], args[3]);
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetDisabled", "boolean" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setVoteSiteDsiabled(sender,
						args[1], Boolean.parseBoolean(args[3]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetPriority", "number" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSitePriority(sender,
						args[1], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetVoteDelay", "number" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteVoteDelay(sender,
						args[1], Integer.parseInt(args[3]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "AddCommandPlayer", "list" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().addVoteSiteCommandPlayer(sender,
						args[1], Utils.getInstance().makeString(3, args));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "AddCommandConsole", "list" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().addVoteSiteCommandConsole(
						sender, args[1],
						Utils.getInstance().makeString(3, args));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "BonusReward", "AddExtraRewardCommandPlayer",
						"string", "list" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
				.addBonusRewardExtraRewardCommandPlayer(sender,
						args[2],
						Utils.getInstance().makeString(3, args));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(
				new String[] { "BonusReward", "AddExtraRewardCommandConsole",
						"string", "list" },
				"VotingPlugin.Commands.AdminVote.BonusReward.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
				.addBonusRewardExtraRewardCommandConsole(sender,
						args[2],
						Utils.getInstance().makeString(3, args));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetExtraRewardChance", "string",
		"number" }, "VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteExtraRewardChance(
						sender, args[1], args[3], Integer.parseInt(args[4]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetExtraRewardItem", "string",
		"string" }, "VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().addVoteSiteExtraRewardItem(
						sender, args[1], args[3], args[4]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "SetExtraRewardMoney", "string",
		"number" }, "VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().setVoteSiteExtraRewardMoney(
						sender, args[1], args[3], Integer.parseInt(args[4]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "AddExtraRewardCommandPlayer",
				"string", "list" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
				.addVoteSiteExtraRewardCommandPlayer(sender, args[1],
						args[3],
						Utils.getInstance().makeString(4, args));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] {
				"VoteSite", "sitename", "AddExtraRewardCommandConsole",
				"string", "list" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance()
				.addVoteSiteExtraRewardCommandConsole(sender, args[1],
						args[3],
						Utils.getInstance().makeString(4, args));

			}
		});

	}

	public void loadCommands() {
		loadAdminVoteCommand();
		loadVoteCommand();
	}

	private void loadVoteCommand() {
		plugin.voteCommand = new ArrayList<CommandHandler>();
		plugin.voteCommand.add(new CommandHandler(new String[] { "Help" },
				"VotingPlugin.Commands.Vote.Help") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().help(sender);

			}
		});
		plugin.voteCommand.add(new CommandHandler(new String[] { "URL" },
				"VotingPlugin.Commands.Vote.Help") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteURL(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Info" },
				"VotingPlugin.Commands.Vote.Info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().infoSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Info",
		"player" }, "VotingPlugin.Commands.Vote.Info.Other") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().infoOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last",
		"player" }, "VotingPlugin.Commands.Vote.Last.Other") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().lastOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last" },
				"VotingPlugin.Commands.Vote.Last") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().lastSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next",
		"player" }, "VotingPlugin.Commands.Vote.Next.Other") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().nextOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next" },
				"VotingPlugin.Commands.Vote.Next") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().nextSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "GUI" },
				"VotingPlugin.Commands.Vote.GUI") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().voteGUI(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today",
		"number" }, "VotingPlugin.Commands.Vote.Today") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender,
						Integer.parseInt(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today" },
				"VotingPlugin.Commands.Vote.Today") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender, 1);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top",
		"number" }, "VotingPlugin.Commands.Vote.Top") {

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

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top" },
				"VotingPlugin.Commands.Vote.Top") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().today(sender, 1);

			}
		});

		plugin.voteCommand.add(new CommandHandler(
				new String[] { "Total", "All" },
				"VotingPlugin.Commands.Vote.Total.All") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalAll(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total",
		"player" }, "VotingPlugin.Commands.Vote.Total.Other") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalOther(sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total" },
				"VotingPlugin.Commands.Vote.Total") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalSelf(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] {}, "") {

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
}
