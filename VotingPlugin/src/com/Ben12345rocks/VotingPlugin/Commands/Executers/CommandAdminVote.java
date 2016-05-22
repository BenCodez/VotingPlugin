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
import com.Ben12345rocks.VotingPlugin.Messages.Messages;
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
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			List<String> cmds = ConfigBonusReward.getInstance()
					.getConsoleCommands();
			cmds.add(cmd);
			ConfigBonusReward.getInstance().setConsoleCommands(cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded console command &c&l" + cmd));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addBonusRewardCommandPlayer(CommandSender sender, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			List<String> cmds = ConfigBonusReward.getInstance()
					.getPlayerCommands();
			cmds.add(cmd);
			ConfigBonusReward.getInstance().setPlayerCommands(cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded player command &c&l" + cmd));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addBonusRewardExtraRewardCommandConsole(CommandSender sender,
			String reward, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			List<String> cmds = ConfigBonusReward.getInstance()
					.getExtraRewardConsoleCommands(reward);
			cmds.add(cmd);
			ConfigBonusReward.getInstance().setExtraRewardConsoleCommands(
					reward, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded extra reward console command &c&l" + cmd));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addBonusRewardExtraRewardCommandPlayer(CommandSender sender,
			String reward, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			List<String> cmds = ConfigBonusReward.getInstance()
					.getExtraRewardPlayerCommands(reward);
			cmds.add(cmd);
			ConfigBonusReward.getInstance().setExtraRewardPlayerCommands(
					reward, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded extra reward player command &c&l" + cmd));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addBonusRewardExtraRewardItem(CommandSender sender,
			String reward, String item) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			if (Utils.getInstance().isPlayer(sender)) {
				Player player = (Player) sender;
				if (player.getInventory().getItemInMainHand() != null) {

					sender.sendMessage(Utils.getInstance().colorize(
							"&cTrying to add item..."));
					Bukkit.getScheduler().runTaskAsynchronously(plugin,
							new Runnable() {

						@Override
						public void run() {
							ConfigBonusReward
							.getInstance()
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
				sender.sendMessage(Messages.getInstance().mustBePlayer());
			}
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addBonusRewardItem(CommandSender sender, String item) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			if (Utils.getInstance().isPlayer(sender)) {
				Player player = (Player) sender;
				if (player.getInventory().getItemInMainHand() != null) {

					sender.sendMessage(Utils.getInstance().colorize(
							"&cTrying to add item..."));
					Bukkit.getScheduler().runTaskAsynchronously(plugin,
							new Runnable() {

						@Override
						public void run() {
							ConfigBonusReward.getInstance().addItem(

									item,
									player.getInventory()
									.getItemInMainHand());
							sender.sendMessage(Utils.getInstance()
									.colorize(
											"&cAdded item &c&l" + item));

						}
					});

				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cHold an item"));
				}
			} else {
				sender.sendMessage(Messages.getInstance().mustBePlayer());
			}
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteCommandConsole(CommandSender sender,
			String voteSite, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			List<String> cmds = ConfigVoteSites.getInstance()
					.getConsoleCommands(voteSite);
			cmds.add(cmd);
			ConfigVoteSites.getInstance().setConsoleCommands(voteSite, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded console command &c&l" + cmd + "&c on &c&l"
							+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteCommandPlayer(CommandSender sender, String voteSite,
			String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			List<String> cmds = ConfigVoteSites.getInstance()
					.getPlayerCommands(voteSite);
			cmds.add(cmd);
			ConfigVoteSites.getInstance().setPlayerCommands(voteSite, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded player command &c&l" + cmd + "&c on &c&l"
							+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteExtraRewardCommandConsole(CommandSender sender,
			String voteSite, String reward, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			List<String> cmds = ConfigVoteSites.getInstance()
					.getExtraRewardConsoleCommands(voteSite, reward);
			cmds.add(cmd);
			ConfigVoteSites.getInstance().setExtraRewardConsoleCommands(
					voteSite, reward, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded extra reward console command &c&l" + cmd
					+ "&c on &c&l" + voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteExtraRewardCommandPlayer(CommandSender sender,
			String voteSite, String reward, String cmd) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			List<String> cmds = ConfigVoteSites.getInstance()
					.getExtraRewardPlayerCommands(voteSite, reward);
			cmds.add(cmd);
			ConfigVoteSites.getInstance().setExtraRewardPlayerCommands(
					voteSite, reward, cmds);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cAdded extra reward player command &c&l" + cmd
					+ "&c on &c&l" + voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteExtraRewardItem(CommandSender sender,
			String voteSite, String reward, String item) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			if (Utils.getInstance().isPlayer(sender)) {
				Player player = (Player) sender;
				if (player.getInventory().getItemInMainHand() != null) {

					sender.sendMessage(Utils.getInstance().colorize(
							"&cTrying to add item..."));
					Bukkit.getScheduler().runTaskAsynchronously(plugin,
							new Runnable() {

						@Override
						public void run() {
							ConfigVoteSites
							.getInstance()
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
				sender.sendMessage(Messages.getInstance().mustBePlayer());
			}
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void addVoteSiteItem(CommandSender sender, String voteSite,
			String item) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
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
											+ " &cto "
											+ voteSite));

						}
					});

				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cHold an item"));
				}
			} else {
				sender.sendMessage(Messages.getInstance().mustBePlayer());
			}
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void bungeeVote(CommandSender sender, String voteSite,
			String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BungeeVote")) {
			BungeeVote.getInstance().sendBungeeVote(voteSite, playerName);
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void createVoteSite(CommandSender sender, String voteSite) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Create")) {
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
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void globalVote(CommandSender sender, String voteSite,
			String playerName) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.AdminVote.Vote")) {

			VotiferEvent.playerVote(voteSite, playerName);

			BungeeVote.getInstance().sendBungeeVote(voteSite, playerName);
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void help(CommandSender sender) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.AdminVote.Help")) {
			sender.sendMessage(Commands.getInstance().adminHelpTextColored());

		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
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
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Reload")) {

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

		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void resetTop(CommandSender sender) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Reset.Top")) {
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
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void serverVote(CommandSender sender, String voteSite,
			String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.ServerVote")) {
			VotiferEvent.playerVote(voteSite, playerName);
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setBonusRewardExtraRewardChance(CommandSender sender,
			String reward, int chance) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			ConfigBonusReward.getInstance()
			.setExtraRewardChance(reward, chance);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet chance to &c&l" + chance));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setBonusRewardExtraRewardMoney(CommandSender sender,
			String reward, int money) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			ConfigBonusReward.getInstance().setExtraRewardMoney(reward, money);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet extra reward money to &c&l" + money));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setBonusRewardMoney(CommandSender sender, int money) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			ConfigBonusReward.getInstance().setMoney(money);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet money to &c&l" + money));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setConfigAllowUnjoined(CommandSender sender, boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Config.Edit")) {
			Config.getInstance().setAllowUnJoined(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet AllowUnjoined to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setConfigBroadcastVote(CommandSender sender, boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Config.Edit")) {
			Config.getInstance().setDebugEnabled(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet BroadcastVote to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setConfigDebug(CommandSender sender, boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Config.Edit")) {
			Config.getInstance().setDebugEnabled(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet Debug to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setConfigDisableTopVoterAwards(CommandSender sender,
			boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Config.Edit")) {
			Config.getInstance().setTopVoterAwardsDisabled(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet DisableTopVoterAwards to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setConfigUpdateReminder(CommandSender sender, boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Config.Edit")) {
			Config.getInstance().setUpdateReminder(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet UpdateReminder to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setGiveBonusReward(CommandSender sender, boolean value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.BonusReward.Edit")) {
			ConfigBonusReward.getInstance().setGiveBonusReward(value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet GiveBonusReward to &c&l" + value));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setServerDataPrevMonth(CommandSender sender, int month) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.ServerData.Edit")) {
			ServerData.getInstance().setPrevMonth(month);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet PreMonth to &c&l" + month));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setTotal(CommandSender sender, String playerName,
			String voteSite, int amount) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Set.Total")) {
			Data.getInstance().setTotal(new User(playerName), voteSite, amount);
			sender.sendMessage(ChatColor.GREEN + playerName
					+ " total votes for " + voteSite + " has been set to "
					+ amount);
			plugin.updateTopUpdater();
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteDsiabled(CommandSender sender, String voteSite,
			boolean disabled) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setDisabled(voteSite, disabled);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet priority to &c&l" + disabled + "&c on &c&l"
							+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteExtraRewardChance(CommandSender sender,
			String siteName, String reward, int chance) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setExtraRewardChance(siteName,
					reward, chance);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet chance to &c&l" + chance));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteExtraRewardMoney(CommandSender sender,
			String voteSite, String reward, int money) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setExtraRewardMoney(voteSite, reward,
					money);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet extra reward money to &c&l" + money + "&c on &c&l"
							+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteMoney(CommandSender sender, String voteSite,
			int money) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setMoney(voteSite, money);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet money to &c&l" + money + "&c on &c&l" + voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSitePriority(CommandSender sender, String voteSite,
			int value) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setPriority(voteSite, value);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteServiceSite(CommandSender sender, String voteSite,
			String serviceSite) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l"
							+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite,
			int delay) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
			sender.sendMessage(Utils.getInstance()
					.colorize(
							"&cSet VoteDelay to &c&l" + delay + "&c on &c&l"
									+ voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void setVoteSiteVoteURL(CommandSender sender, String voteSite,
			String url) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.Edit")) {
			ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
			sender.sendMessage(Utils.getInstance().colorize(
					"&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void site(CommandSender sender, String site) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Sites.Site")) {
			sender.sendMessage(Commands.getInstance().voteCommandSiteInfo(site));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void sites(CommandSender sender) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.Sites")) {
			sender.sendMessage(Commands.getInstance().voteCommandSites());
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void uuid(CommandSender sender, String playerName) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.AdminVote.UUID")) {
			sender.sendMessage(ChatColor.GREEN + "UUID of player "
					+ ChatColor.DARK_GREEN + playerName + ChatColor.GREEN
					+ " is: " + Utils.getInstance().getUUID(playerName));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void version(CommandSender sender) {
		if (sender instanceof Player) {
			if (Utils.getInstance().hasPermission(sender,
					"Commands.AdminVote.Version")) {
				Player player = (Player) sender;
				player.performCommand("bukkit:version " + plugin.getName());
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					"bukkit:version " + plugin.getName());
		}
	}

}
