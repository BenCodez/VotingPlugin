package com.bencodez.votingplugin.commands.executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandAdminVote.
 */
public class CommandAdminVote implements CommandExecutor {

	private VotingPluginMain plugin;

	public CommandAdminVote(VotingPluginMain plugin) {
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
		for (CommandHandler commandHandler : plugin.getAdminVoteCommand()) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(MessageAPI.colorize(plugin.getConfigFile().getFormatInvalidCommandAdminVote()));

		return true;
	}

}
