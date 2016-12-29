package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandVote.
 */
public class CommandVote implements CommandExecutor {

	/** The instance. */
	private static CommandVote instance = new CommandVote();

	/** The plugin. */
	private static Main plugin;

	/**
	 * Gets the single instance of CommandVote.
	 *
	 * @return single instance of CommandVote
	 */
	public static CommandVote getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new command vote.
	 */
	private CommandVote() {
	}

	/**
	 * Instantiates a new command vote.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandVote(Main plugin) {
		CommandVote.plugin = plugin;
	}

	/**
	 * Help.
	 *
	 * @param sender
	 *            the sender
	 */
	public void help(CommandSender sender) {
		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendJson(Commands.getInstance().voteHelpText(sender));
		} else {
			sender.sendMessage(ArrayUtils.getInstance()
					.convert(ArrayUtils.getInstance().comptoString(Commands.getInstance().voteHelpText(sender))));
		}

	}

	/**
	 * Info other.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void infoOther(CommandSender sender, String playerName) {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(StringUtils.getInstance().colorize("&cGetting player info..."));
					user.sendMessage(Commands.getInstance()
							.playerInfo(UserManager.getInstance().getVotingPluginUser(playerName)));
				} else {
					sender.sendMessage(ArrayUtils.getInstance().colorize(Commands.getInstance()
							.playerInfo(UserManager.getInstance().getVotingPluginUser(playerName))));
				}
			}
		});

	}

	/**
	 * Info self.
	 *
	 * @param sender
	 *            the sender
	 */
	public void infoSelf(CommandSender sender) {
		if (sender instanceof Player) {
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(StringUtils.getInstance().colorize("&cGetting info..."));
					user.sendMessage(Commands.getInstance().playerInfo(user));
				}
			});

		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	/**
	 * Last other.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void lastOther(CommandSender sender, String playerName) {

		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendMessage(
					Commands.getInstance().voteCommandLast(UserManager.getInstance().getVotingPluginUser(playerName)));
		} else {
			sender.sendMessage(ArrayUtils.getInstance().colorize(
					Commands.getInstance().voteCommandLast(UserManager.getInstance().getVotingPluginUser(playerName))));
		}

	}

	/**
	 * Last self.
	 *
	 * @param sender
	 *            the sender
	 */
	public void lastSelf(CommandSender sender) {
		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendMessage(Commands.getInstance().voteCommandLast(user));
		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	/**
	 * Next other.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void nextOther(CommandSender sender, String playerName) {
		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendMessage(
					Commands.getInstance().voteCommandNext(UserManager.getInstance().getVotingPluginUser(playerName)));

		} else {
			sender.sendMessage(ArrayUtils.getInstance().colorize(
					Commands.getInstance().voteCommandNext(UserManager.getInstance().getVotingPluginUser(playerName))));
		}

	}

	/**
	 * Next self.
	 *
	 * @param sender
	 *            the sender
	 */
	public void nextSelf(CommandSender sender) {
		if (sender instanceof Player) {
			String playerName = sender.getName();
			User user = UserManager.getInstance().getVotingPluginUser(playerName);
			user.sendMessage(Commands.getInstance().voteCommandNext(user));
		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.command.CommandExecutor#onCommand(org.bukkit.command.
	 * CommandSender , org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {

		for (CommandHandler commandHandler : plugin.voteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED + "No valid arguments, see /vote help!");
		return true;
	}

	/**
	 * Points other.
	 *
	 * @param sender
	 *            the sender
	 * @param user
	 *            the user
	 */
	public void pointsOther(CommandSender sender, User user) {
		String msg = Config.getInstance().getFormatCommandVotePoints().replace("%Player%", user.getPlayerName())
				.replace("%Points%", "" + user.getPoints());
		if (sender instanceof Player) {
			UserManager.getInstance().getVotingPluginUser((Player) sender).sendMessage(msg);
		} else {
			sender.sendMessage(StringUtils.getInstance().colorize(msg));
		}

	}

	/**
	 * Points self.
	 *
	 * @param user
	 *            the user
	 */
	public void pointsSelf(User user) {
		String msg = Config.getInstance().getFormatCommandVotePoints().replace("%Player%", user.getPlayerName())
				.replace("%Points%", "" + user.getPoints());
		user.sendMessage(msg);
	}

	/**
	 * Today.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void today(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(Commands.getInstance().commandVoteToday(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendVoteTodayScoreBoard((Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(Commands.getInstance().commandVoteToday(page));
				}

			}
		});

	}

	/**
	 * Top voter daily.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void topVoterDaily(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(TopVoterHandler.getInstance().topVoterDaily(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendTopVoterDailyScoreBoard((Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoterHandler.getInstance().topVoterDaily(page));
				}

			}
		});

	}

	/**
	 * Top voter monthly.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void topVoterMonthly(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(TopVoterHandler.getInstance().topVoterMonthly(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendTopVoterMonthlyScoreBoard((Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoterHandler.getInstance().topVoterMonthly(page));
				}

			}
		});

	}

	/**
	 * Top voter weekly.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void topVoterWeekly(CommandSender sender, int page) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(TopVoterHandler.getInstance().topVoterWeekly(page));
					Bukkit.getScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Commands.getInstance().sendTopVoterWeeklyScoreBoard((Player) sender, page);
						}
					});
				} else {
					sender.sendMessage(TopVoterHandler.getInstance().topVoterWeekly(page));
				}

			}
		});

	}

	/**
	 * Total all.
	 *
	 * @param sender
	 *            the sender
	 */
	public void totalAll(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				if (sender instanceof Player) {
					User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
					user.sendMessage(Commands.getInstance().voteCommandTotalAll());

				} else {
					sender.sendMessage(Commands.getInstance().voteCommandTotalAll());
				}

			}
		});

	}

	/**
	 * Total other.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void totalOther(CommandSender sender, String playerName) {
		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendMessage(
					Commands.getInstance().voteCommandTotal(UserManager.getInstance().getVotingPluginUser(playerName)));
		} else {
			sender.sendMessage(ArrayUtils.getInstance().colorize(Commands.getInstance()
					.voteCommandTotal(UserManager.getInstance().getVotingPluginUser(playerName))));
		}

	}

	/**
	 * Total self.
	 *
	 * @param sender
	 *            the sender
	 */
	public void totalSelf(CommandSender sender) {
		if (sender instanceof Player) {
			String playerName = sender.getName();
			User user = UserManager.getInstance().getVotingPluginUser(playerName);
			user.sendMessage(Commands.getInstance().voteCommandTotal(user));
		} else {
			sender.sendMessage("You must be a player to do this!");
		}
	}

	/**
	 * Vote UR ls.
	 *
	 * @param sender
	 *            the sender
	 */
	public void voteURLs(CommandSender sender) {
		if (sender instanceof Player) {
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendMessage(Commands.getInstance().voteURLs());
		} else {
			sender.sendMessage(Commands.getInstance().voteURLs());
		}
	}

}
