package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandAdminVote.
 */
public class CommandAdminVote implements CommandExecutor {

	/** The instance. */
	private static CommandAdminVote instance = new CommandAdminVote();

	/**
	 * Gets the single instance of CommandAdminVote.
	 *
	 * @return single instance of CommandAdminVote
	 */
	public static CommandAdminVote getInstance() {
		return instance;
	}

	/** The config. */
	Config config = Config.getInstance();

	/** The plugin. */
	private Main plugin = Main.plugin;

	/** The vote sites. */
	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	/**
	 * Instantiates a new command admin vote.
	 */
	private CommandAdminVote() {
	}

	/**
	 * Instantiates a new command admin vote.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
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
		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED + "No valid arguments, see /adminvote help!");

		return true;
	}

}
