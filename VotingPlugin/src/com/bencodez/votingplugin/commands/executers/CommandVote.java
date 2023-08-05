package com.bencodez.votingplugin.commands.executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandVote.
 */
public class CommandVote implements CommandExecutor {

	/** The instance. */
	private static CommandVote instance = new CommandVote();

	/** The plugin. */
	private static VotingPluginMain plugin;

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
	 * @param plugin the plugin
	 */
	public CommandVote(VotingPluginMain plugin) {
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

		for (CommandHandler commandHandler : plugin.getVoteCommand()) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(StringParser.getInstance().colorize(plugin.getConfigFile().getFormatInvalidCommandVote()));
		return true;
	}

}
