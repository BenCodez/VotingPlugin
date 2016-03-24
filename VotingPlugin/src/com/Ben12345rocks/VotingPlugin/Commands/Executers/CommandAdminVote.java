package com.Ben12345rocks.VotingPlugin.Commands.Executers;

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
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserData.Data;

public class CommandAdminVote implements CommandExecutor {

	ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	Config config = Config.getInstance();

	ConfigFormat format = ConfigFormat.getInstance();

	private Main plugin;

	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
	}

	public void addVoteSiteItem(CommandSender sender, String voteSite,
			String item) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.VoteSite.AddItem")) {
			if (Utils.getInstance().isPlayer(sender)) {
				Player player = (Player) sender;
				if (player.getInventory().getItemInMainHand() != null) {

					sender.sendMessage("&cTrying to add item...");
					Bukkit.getScheduler().runTaskAsynchronously(plugin,
							new Runnable() {

						@Override
						public void run() {
							ConfigVoteSites.getInstance().addItem(
									voteSite,
									item,
									player.getInventory()
									.getItemInMainHand());
							sender.sendMessage("&cAdded item &c&l"
									+ item + " &cto " + voteSite);

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

	public void globalVote(CommandSender sender, String voteSite,
			String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.AdminVote.GlobalVote")) {
			VotiferEvent.playerVote(voteSite, playerName);
			BungeeVote.getInstance().sendBungeeVote(voteSite, playerName);
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void help(CommandSender sender) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.AdminVote.Help")) {
			Utils.getInstance().sendMessageComponent(sender,
					Commands.getInstance().adminVoteHelp());
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (args.length == 0) {
			help(sender);
		}

		if (args.length == 1) {
			if (args[0].equalsIgnoreCase("help")
					|| args[0].equalsIgnoreCase("?")) {
				help(sender);
				return true;
			}
			if (args[0].equalsIgnoreCase("reload")) {
				reload(sender);
				return true;
			}
			if (args[0].equalsIgnoreCase("version")) {
				version(sender);
				return true;
			}

			if (args[0].equalsIgnoreCase("sites")) {
				sites(sender);
				return true;
			}

		}

		if (args.length == 2) {
			if (args[0].equalsIgnoreCase("sites")) {
				site(sender, args[1]);
				return true;
			}
			if (args[0].equalsIgnoreCase("uuid")) {
				uuid(sender, args[1]);
				return true;
			}

			if (args[0].equalsIgnoreCase("reset")) {
				if (args[1].equalsIgnoreCase("top")) {
					resetTop(sender);
				}
				return true;
			}

		}

		if (args.length == 3) {
			if (args[0].equalsIgnoreCase("vote")) {
				vote(sender, args[1], args[2]);
				return true;
			}
			if (args[0].equalsIgnoreCase("bungeevote")) {
				bungeeVote(sender, args[1], args[2]);
				return true;
			}
			if (args[0].equalsIgnoreCase("globalvote")) {
				globalVote(sender, args[1], args[2]);
				return true;
			}

		}

		if (args.length == 4) {
			if (args[0].equalsIgnoreCase("settotal")) {
				if (Utils.getInstance().isInt(args[4])) {
					setTotal(sender, args[1], args[2],
							Integer.parseInt(args[2]));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cError on " + args[4] + ", number expected"));
				}
				return true;
			}
			if (args[0].equalsIgnoreCase("VoteSite")) {
				if (args[2].equalsIgnoreCase("AddItem")) {
					addVoteSiteItem(sender, args[1], args[3]);
					return true;
				}
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
			config.reloadData();
			format.reloadData();
			plugin.loadVoteSites();
			bonusReward.reloadData();
			sender.sendMessage(ChatColor.RED + plugin.getName() + " reloaded!");
			plugin.updateTopUpdater();
			plugin.setupFiles();
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
					resetTopVoter();
					sender.sendMessage(Utils.getInstance().colorize(
							"&cDone resseting top voter"));
					plugin.updateTopUpdater();
				}
			});
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void resetTopVoter() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotal(voteSite, 0);
			}
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

	public void vote(CommandSender sender, String voteSite, String playerName) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.AdminVote.Vote")) {
			VotiferEvent.playerVote(voteSite, playerName);
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}
}
