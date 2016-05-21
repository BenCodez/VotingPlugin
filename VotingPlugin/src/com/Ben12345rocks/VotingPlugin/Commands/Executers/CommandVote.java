package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

public class CommandVote implements CommandExecutor {

	private static CommandVote instance = new CommandVote();

	private static Main plugin;

	public static CommandVote getInstance() {
		return instance;
	}

	private CommandVote() {
	}

	public CommandVote(Main plugin) {
		CommandVote.plugin = plugin;
	}

	public void help(CommandSender sender) {
		if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Help")
				|| Utils.getInstance().hasPermission(sender, "Player")) {
			sender.sendMessage(Commands.getInstance().voteHelpTextColored());
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void infoOther(CommandSender sender, String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.Vote.Info.Other")) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					sender.sendMessage(Utils.getInstance().colorize(
							"&cGetting player info..."));
					sender.sendMessage(Commands.getInstance().playerInfo(
							new User(playerName)));
				}
			});
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void infoSelf(CommandSender sender) {
		if (sender instanceof Player) {
			if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Info")
					|| Utils.getInstance().hasPermission(sender, "Player")) {
				Bukkit.getScheduler().runTaskAsynchronously(plugin,
						new Runnable() {

					@Override
					public void run() {
						sender.sendMessage(Utils.getInstance()
								.colorize("&cGetting info..."));
						sender.sendMessage(Commands.getInstance()
								.playerInfo(new User(sender.getName())));
					}
				});
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			sender.sendMessage(Messages.getInstance().mustBePlayer());
		}
	}

	public void lastOther(CommandSender sender, String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.Vote.Last.Other")) {
			User user = new User(playerName);
			sender.sendMessage(Commands.getInstance().voteCommandLast(user));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void lastSelf(CommandSender sender) {
		if (sender instanceof Player) {
			if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Last")
					|| Utils.getInstance().hasPermission(sender, "Player")) {
				String playerName = sender.getName();
				User user = new User(playerName);
				sender.sendMessage(Commands.getInstance().voteCommandLast(user));
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			sender.sendMessage(Messages.getInstance().mustBePlayer());
		}
	}

	public void nextOther(CommandSender sender, String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.Vote.Next.Other")) {
			User user = new User(playerName);
			sender.sendMessage(Commands.getInstance().voteCommandNext(user));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void nextSelf(CommandSender sender) {
		if (sender instanceof Player) {
			if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Next")
					|| Utils.getInstance().hasPermission(sender, "Player")) {
				String playerName = sender.getName();
				User user = new User(playerName);
				sender.sendMessage(Commands.getInstance().voteCommandNext(user));
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			sender.sendMessage(Messages.getInstance().mustBePlayer());
		}
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		for (CommandHandler commandHandler : plugin.voteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		/*
		 * if (args.length == 0) { voteURLs(sender); return true; }
		 *
		 * if (args.length == 1) { if (args[0].equalsIgnoreCase("help") ||
		 * args[0].equalsIgnoreCase("?")) { help(sender); return true; } if
		 * (args[0].equalsIgnoreCase("total")) { totalSelf(sender); return true;
		 * } if (args[0].equalsIgnoreCase("last")) { lastSelf(sender); return
		 * true; } if (args[0].equalsIgnoreCase("next")) { nextSelf(sender);
		 * return true; }
		 *
		 * if (args[0].equalsIgnoreCase("top")) { topVoter(sender, 1); return
		 * true; }
		 *
		 * if (args[0].equalsIgnoreCase("info")) { infoSelf(sender); return
		 * true; }
		 *
		 * if (args[0].equalsIgnoreCase("today")) { today(sender, 1); return
		 * true; }
		 *
		 * if (args[0].equalsIgnoreCase("gui")) { voteGUI(sender); return true;
		 * }
		 *
		 * }
		 *
		 * if (args.length == 2) { if (args[0].equalsIgnoreCase("info")) {
		 * infoOther(sender, args[1]); return true; } if
		 * (args[0].equalsIgnoreCase("total")) { if
		 * (args[1].equalsIgnoreCase("all")) { totalAll(sender); } else {
		 * totalOther(sender, args[1]); } return true; } if
		 * (args[0].equalsIgnoreCase("last")) { lastOther(sender, args[1]);
		 * return true; } if (args[0].equalsIgnoreCase("next")) {
		 * nextOther(sender, args[1]); return true; } if
		 * (args[0].equalsIgnoreCase("top")) { if
		 * (Utils.getInstance().isInt(args[1])) { topVoter(sender,
		 * Integer.parseInt(args[1])); } else {
		 * sender.sendMessage(Utils.getInstance().colorize( "&cError on " +
		 * args[1] + ", number expected")); } return true; }
		 *
		 * if (args[0].equalsIgnoreCase("today")) { if
		 * (Utils.getInstance().isInt(args[1])) { today(sender,
		 * Integer.parseInt(args[1])); } else {
		 * sender.sendMessage(Utils.getInstance().colorize( "&cError on " +
		 * args[1] + ", number expected")); }
		 *
		 * return true; } }
		 */

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /vote help!");
		return true;
	}

	public void today(CommandSender sender, int page) {
		if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Today")) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
				@Override
				public void run() {

					sender.sendMessage(Commands.getInstance().commandVoteToday(
							page));
				}
			});

		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void topVoter(CommandSender sender, int page) {
		if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Top")
				|| Utils.getInstance().hasPermission(sender, "Player")) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					sender.sendMessage(TopVoter.getInstance().topVoter(page));
				}
			});
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void totalAll(CommandSender sender) {
		if (Utils.getInstance()
				.hasPermission(sender, "Commands.Vote.Total.All")) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					sender.sendMessage(Commands.getInstance()
							.voteCommandTotalAll());
				}
			});

		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void totalOther(CommandSender sender, String playerName) {
		if (Utils.getInstance().hasPermission(sender,
				"Commands.Vote.Total.Other")) {
			User user = new User(playerName);
			sender.sendMessage(Commands.getInstance().voteCommandTotal(user));
		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

	public void totalSelf(CommandSender sender) {
		if (sender instanceof Player) {
			if (Utils.getInstance()
					.hasPermission(sender, "Commands.Vote.Total")
					|| Utils.getInstance().hasPermission(sender, "Player")) {
				String playerName = sender.getName();
				User user = new User(playerName);
				sender.sendMessage(Commands.getInstance()
						.voteCommandTotal(user));
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			sender.sendMessage(Messages.getInstance().mustBePlayer());
		}
	}

	public void voteGUI(CommandSender sender) {

		if (sender instanceof Player) {
			if (Utils.getInstance().hasPermission(sender, "Commands.Vote.GUI")
					|| Utils.getInstance().hasPermission(sender, "Player")) {
				Commands.getInstance().openVoteGUI((Player) sender);
			} else {
				sender.sendMessage(Messages.getInstance().noPerms());
			}
		} else {
			sender.sendMessage("Must be a player to do this!");
		}
	}

	public void voteTopSite(CommandSender sender, String siteName) {

	}

	public void voteURLs(CommandSender sender) {
		sender.sendMessage(Commands.getInstance().voteURLs());
	}

}
