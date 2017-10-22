package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Main;

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

}
