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
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendJson(Commands.getInstance().voteHelpText(sender));
		} else {
			sender.sendMessage(Utils.getInstance().convertArray(
					Utils.getInstance().comptoString(
							Commands.getInstance().voteHelpText(sender))));
		}

	}

	public void infoOther(CommandSender sender, String playerName) {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(Utils.getInstance().colorize(
							"&cGetting player info..."));
					user.sendMessage(Commands.getInstance().playerInfo(
							new User(playerName)));
				} else {
					sender.sendMessage(Utils.getInstance().colorize(
							Commands.getInstance().playerInfo(
									new User(playerName))));
				}
			}
		});

	}

	public void infoSelf(CommandSender sender) {
		if (sender instanceof Player) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					User user = new User((Player) sender);
					user.sendMessage(Utils.getInstance().colorize(
							"&cGetting info..."));
					user.sendMessage(Commands.getInstance().playerInfo(
							new User(sender.getName())));
				}
			});

		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	public void lastOther(CommandSender sender, String playerName) {

		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendMessage(Commands.getInstance().voteCommandLast(
					new User(playerName)));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					Commands.getInstance()
							.voteCommandLast(new User(playerName))));
		}

	}

	public void lastSelf(CommandSender sender) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendMessage(Commands.getInstance().voteCommandLast(user));

		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	public void nextOther(CommandSender sender, String playerName) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendMessage(Commands.getInstance().voteCommandNext(
					new User(playerName)));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					Commands.getInstance()
							.voteCommandNext(new User(playerName))));
		}

	}

	public void nextSelf(CommandSender sender) {
		if (sender instanceof Player) {
			String playerName = sender.getName();
			User user = new User(playerName);
			user.sendMessage(Commands.getInstance().voteCommandNext(user));

		} else {
			sender.sendMessage("You must be a player to do this!");
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

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /vote help!");
		return true;
	}

	public void today(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(Commands.getInstance().commandVoteToday(
							page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendVoteTodayScoreBoard(
									(Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(Commands.getInstance().commandVoteToday(
							page));
				}

			}
		});

	}

	public void topVoterDaily(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(TopVoter.getInstance().topVoterDaily(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendTopVoterDailyScoreBoard(
									(Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoter.getInstance().topVoterDaily(
							page));
				}

			}
		});

	}

	public void topVoterMonthly(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(TopVoter.getInstance().topVoterMonthly(
							page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance()
									.sendTopVoterMonthlyScoreBoard(
											(Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoter.getInstance().topVoterMonthly(
							page));
				}

			}
		});

	}

	public void topVoterWeekly(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(TopVoter.getInstance()
							.topVoterWeekly(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance()
									.sendTopVoterWeeklyScoreBoard(
											(Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoter.getInstance().topVoterWeekly(
							page));
				}

			}
		});

	}

	public void totalAll(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = new User((Player) sender);
					user.sendMessage(Commands.getInstance()
							.voteCommandTotalAll());
					Commands.getInstance().sendVoteTotalAllScoreBoard(
							(Player) sender);
				} else {
					sender.sendMessage(Commands.getInstance()
							.voteCommandTotalAll());
				}

			}
		});

	}

	public void totalOther(CommandSender sender, String playerName) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendMessage(Commands.getInstance().voteCommandTotal(
					new User(playerName)));
			Commands.getInstance().sendVoteTotalScoreBoard((Player) sender,
					new User(playerName));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					Commands.getInstance().voteCommandTotal(
							new User(playerName))));
		}

	}

	public void totalSelf(CommandSender sender) {
		if (sender instanceof Player) {
			String playerName = sender.getName();
			User user = new User(playerName);
			user.sendMessage(Commands.getInstance().voteCommandTotal(user));
			Commands.getInstance().sendVoteTotalScoreBoard((Player) sender,
					user);
		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	public void voteGUI(CommandSender sender) {
		if (sender instanceof Player) {
			Commands.getInstance().openVoteGUI((Player) sender);
		} else {
			sender.sendMessage("Must be a player to do this!");
		}
	}

	public void voteReward(CommandSender sender, String siteName) {
		if (sender instanceof Player) {
			Commands.getInstance().voteReward((Player) sender, siteName);
		} else {
			sender.sendMessage("Must be a player to do this!");
		}
	}

	public void voteURL(CommandSender sender) {
		if (sender instanceof Player) {
			Commands.getInstance().voteURL((Player) sender);
		} else {
			sender.sendMessage("Must be a player to do this!");
		}
	}

	public void voteURLs(CommandSender sender) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendMessage(Commands.getInstance().voteURLs());
		} else {
			sender.sendMessage(Commands.getInstance().voteURLs());
		}
	}

}
