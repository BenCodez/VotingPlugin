package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

public class CommandAdminVote implements CommandExecutor {

	private static CommandAdminVote instance = new CommandAdminVote();

	public static CommandAdminVote getInstance() {
		return instance;
	}

	ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	Config config = Config.getInstance();

	ConfigFormat format = ConfigFormat.getInstance();

	private Main plugin;

	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	private CommandAdminVote() {
	}

	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
	}

	public void addBonusRewardCommandConsole(CommandSender sender, String cmd) {

		List<String> cmds = ConfigBonusReward.getInstance()
				.getConsoleCommands();
		cmds.add(cmd);
		ConfigBonusReward.getInstance().setConsoleCommands(cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded console command &c&l" + cmd));

	}

	public void addBonusRewardCommandPlayer(CommandSender sender, String cmd) {

		List<String> cmds = ConfigBonusReward.getInstance().getPlayerCommands();
		cmds.add(cmd);
		ConfigBonusReward.getInstance().setPlayerCommands(cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded player command &c&l" + cmd));

	}

	public void addBonusRewardExtraRewardCommandConsole(CommandSender sender,
			String reward, String cmd) {

		List<String> cmds = ConfigBonusReward.getInstance()
				.getExtraRewardConsoleCommands(reward);
		cmds.add(cmd);
		ConfigBonusReward.getInstance().setExtraRewardConsoleCommands(reward,
				cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded extra reward console command &c&l" + cmd));

	}

	public void addBonusRewardExtraRewardCommandPlayer(CommandSender sender,
			String reward, String cmd) {

		List<String> cmds = ConfigBonusReward.getInstance()
				.getExtraRewardPlayerCommands(reward);
		cmds.add(cmd);
		ConfigBonusReward.getInstance().setExtraRewardPlayerCommands(reward,
				cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded extra reward player command &c&l" + cmd));

	}

	public void addBonusRewardExtraRewardItem(CommandSender sender,
			String reward, String item) {

		if (Utils.getInstance().isPlayer(sender)) {
			Player player = (Player) sender;
			if (player.getInventory().getItemInMainHand() != null) {

				sender.sendMessage(Utils.getInstance().colorize(
						"&cTrying to add item..."));
				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

							@Override
							public void run() {
								ConfigBonusReward.getInstance()
										.addExtraRewardItem(
												reward,

												item,
												player.getInventory()
														.getItemInMainHand());
								sender.sendMessage(Utils.getInstance()
										.colorize(
												"&cAdded extra reward item &c&l"
														+ item));

							}
						});

			} else {
				sender.sendMessage(Utils.getInstance().colorize(
						"&cHold an item"));
			}
		} else {
			sender.sendMessage("You must be a player to do this!");
		}

	}

	public void addBonusRewardItem(CommandSender sender, String item) {

		if (Utils.getInstance().isPlayer(sender)) {
			Player player = (Player) sender;
			if (player.getInventory().getItemInMainHand() != null) {

				sender.sendMessage(Utils.getInstance().colorize(
						"&cTrying to add item..."));
				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

							@Override
							public void run() {
								ConfigBonusReward.getInstance()
										.addItem(

												item,
												player.getInventory()
														.getItemInMainHand());
								sender.sendMessage(Utils.getInstance()
										.colorize("&cAdded item &c&l" + item));

							}
						});

			} else {
				sender.sendMessage(Utils.getInstance().colorize(
						"&cHold an item"));
			}
		} else {
			sender.sendMessage("You must be a player to do this!");
		}

	}

	public void addVoteSiteCommandConsole(CommandSender sender,
			String voteSite, String cmd) {

		List<String> cmds = ConfigVoteSites.getInstance().getConsoleCommands(
				voteSite);
		cmds.add(cmd);
		ConfigVoteSites.getInstance().setConsoleCommands(voteSite, cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded console command &c&l" + cmd + "&c on &c&l" + voteSite));

	}

	public void addVoteSiteCommandPlayer(CommandSender sender, String voteSite,
			String cmd) {

		List<String> cmds = ConfigVoteSites.getInstance().getPlayerCommands(
				voteSite);
		cmds.add(cmd);
		ConfigVoteSites.getInstance().setPlayerCommands(voteSite, cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded player command &c&l" + cmd + "&c on &c&l" + voteSite));

	}

	public void addVoteSiteExtraRewardCommandConsole(CommandSender sender,
			String voteSite, String reward, String cmd) {

		List<String> cmds = ConfigVoteSites.getInstance()
				.getExtraRewardConsoleCommands(voteSite, reward);
		cmds.add(cmd);
		ConfigVoteSites.getInstance().setExtraRewardConsoleCommands(voteSite,
				reward, cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded extra reward console command &c&l" + cmd
						+ "&c on &c&l" + voteSite));

	}

	public void addVoteSiteExtraRewardCommandPlayer(CommandSender sender,
			String voteSite, String reward, String cmd) {

		List<String> cmds = ConfigVoteSites.getInstance()
				.getExtraRewardPlayerCommands(voteSite, reward);
		cmds.add(cmd);
		ConfigVoteSites.getInstance().setExtraRewardPlayerCommands(voteSite,
				reward, cmds);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cAdded extra reward player command &c&l" + cmd + "&c on &c&l"
						+ voteSite));

	}

	public void addVoteSiteExtraRewardItem(CommandSender sender,
			String voteSite, String reward, String item) {

		if (Utils.getInstance().isPlayer(sender)) {
			Player player = (Player) sender;
			if (player.getInventory().getItemInMainHand() != null) {

				sender.sendMessage(Utils.getInstance().colorize(
						"&cTrying to add item..."));
				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

							@Override
							public void run() {
								ConfigVoteSites.getInstance()
										.addExtraRewardItem(
												voteSite,
												reward,
												item,
												player.getInventory()
														.getItemInMainHand());
								sender.sendMessage(Utils.getInstance()
										.colorize(
												"&cAdded extra reward item &c&l"
														+ item + " &cto "
														+ voteSite));

							}
						});

			} else {
				sender.sendMessage(Utils.getInstance().colorize(
						"&cHold an item"));
			}
		} else {
			sender.sendMessage("You must be a player to do this!");
		}

	}

	public void addVoteSiteItem(CommandSender sender, String voteSite,
			String item) {

		if (Utils.getInstance().isPlayer(sender)) {
			Player player = (Player) sender;
			if (player.getInventory().getItemInMainHand() != null) {

				sender.sendMessage(Utils.getInstance().colorize(
						"&cTrying to add item..."));
				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

							@Override
							public void run() {
								ConfigVoteSites.getInstance().addItem(
										voteSite,
										item,
										player.getInventory()
												.getItemInMainHand());
								sender.sendMessage(Utils.getInstance()
										.colorize(
												"&cAdded item &c&l" + item
														+ " &cto " + voteSite));

							}
						});

			} else {
				sender.sendMessage(Utils.getInstance().colorize(
						"&cHold an item"));
			}
		} else {
			sender.sendMessage("You must be a player to do this!");
		}

	}

	public void bungeeVote(CommandSender sender, String voteSite,
			String playerName) {

		BungeeVote.getInstance().sendBungeeVote(voteSite, playerName);

	}

	public void createVoteSite(CommandSender sender, String voteSite) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cCreating VoteSite..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				ConfigVoteSites.getInstance().generateVoteSite(voteSite);
				sender.sendMessage(Utils.getInstance().colorize(
						"&cCreated VoteSite: &c&l" + voteSite));
			}
		});

	}

	public void globalVote(CommandSender sender, String voteSite,
			String playerName) {

		VotiferEvent.playerVote(voteSite, playerName);

		BungeeVote.getInstance().sendBungeeVote(voteSite, playerName);

	}

	public void help(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().adminHelpTextColored());

	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /adminvote help!");

		return true;
	}

	public void reload(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				sender.sendMessage(ChatColor.RED + "Reloading "
						+ plugin.getName() + "...");
				plugin.reload();
				sender.sendMessage(ChatColor.RED + plugin.getName()
						+ " reloaded!");
			}
		});

	}

	public void resetTop(CommandSender sender) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting top voter..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTopVoter();
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting top voter"));
				plugin.updateTopUpdater();
			}
		});

	}

	public void serverVote(CommandSender sender, String voteSite,
			String playerName) {

		VotiferEvent.playerVote(voteSite, playerName);

	}

	public void setBonusRewardExtraRewardChance(CommandSender sender,
			String reward, int chance) {

		ConfigBonusReward.getInstance().setExtraRewardChance(reward, chance);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet chance to &c&l" + chance));

	}

	public void setBonusRewardExtraRewardMoney(CommandSender sender,
			String reward, int money) {

		ConfigBonusReward.getInstance().setExtraRewardMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet extra reward money to &c&l" + money));

	}

	public void setBonusRewardMoney(CommandSender sender, int money) {

		ConfigBonusReward.getInstance().setMoney(money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet money to &c&l" + money));

	}

	public void setConfigAllowUnjoined(CommandSender sender, boolean value) {

		Config.getInstance().setAllowUnJoined(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet AllowUnjoined to &c&l" + value));

	}

	public void setConfigBroadcastVote(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet BroadcastVote to &c&l" + value));

	}

	public void setConfigDebug(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet Debug to &c&l" + value));

	}

	public void setConfigDisableTopVoterAwards(CommandSender sender,
			boolean value) {

		Config.getInstance().setTopVoterAwardsDisabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet DisableTopVoterAwards to &c&l" + value));

	}

	public void setConfigUpdateReminder(CommandSender sender, boolean value) {

		Config.getInstance().setUpdateReminder(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet UpdateReminder to &c&l" + value));

	}

	public void setGiveBonusReward(CommandSender sender, boolean value) {

		ConfigBonusReward.getInstance().setGiveBonusReward(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet GiveBonusReward to &c&l" + value));

	}

	public void setServerDataPrevMonth(CommandSender sender, int month) {

		ServerData.getInstance().setPrevMonth(month);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet PreMonth to &c&l" + month));

	}

	public void setTotal(CommandSender sender, String playerName,
			String voteSite, int amount) {

		Data.getInstance().setTotal(new User(playerName), voteSite, amount);
		sender.sendMessage(ChatColor.GREEN + playerName + " total votes for "
				+ voteSite + " has been set to " + amount);
		plugin.updateTopUpdater();

	}

	public void setVoteSiteDsiabled(CommandSender sender, String voteSite,
			boolean disabled) {

		ConfigVoteSites.getInstance().setDisabled(voteSite, disabled);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet priority to &c&l" + disabled + "&c on &c&l" + voteSite));

	}

	public void setVoteSiteExtraRewardChance(CommandSender sender,
			String siteName, String reward, int chance) {

		ConfigVoteSites.getInstance().setExtraRewardChance(siteName, reward,
				chance);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet chance to &c&l" + chance));

	}

	public void setVoteSiteExtraRewardMoney(CommandSender sender,
			String voteSite, String reward, int money) {

		ConfigVoteSites.getInstance().setExtraRewardMoney(voteSite, reward,
				money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet extra reward money to &c&l" + money + "&c on &c&l"
						+ voteSite));

	}

	public void setVoteSiteMoney(CommandSender sender, String voteSite,
			int money) {

		ConfigVoteSites.getInstance().setMoney(voteSite, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet money to &c&l" + money + "&c on &c&l" + voteSite));

	}

	public void setVoteSitePriority(CommandSender sender, String voteSite,
			int value) {

		ConfigVoteSites.getInstance().setPriority(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

	}

	public void setVoteSiteServiceSite(CommandSender sender, String voteSite,
			String serviceSite) {

		ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l"
						+ voteSite));

	}

	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite,
			int delay) {

		ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

	}

	public void setVoteSiteVoteURL(CommandSender sender, String voteSite,
			String url) {

		ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));

	}

	public void site(CommandSender sender, String site) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Sites.Site")) {
			sender.sendMessage(Commands.getInstance().voteCommandSiteInfo(site));
		} else {

		}
	}

	public void sites(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandSites());

	}

	public void uuid(CommandSender sender, String playerName) {

		sender.sendMessage(ChatColor.GREEN + "UUID of player "
				+ ChatColor.DARK_GREEN + playerName + ChatColor.GREEN + " is: "
				+ Utils.getInstance().getUUID(playerName));

	}

	public void version(CommandSender sender) {
		if (sender instanceof Player) {

			Player player = (Player) sender;
			player.performCommand("bukkit:version " + plugin.getName());

		} else {
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					"bukkit:version " + plugin.getName());
		}
	}

}
