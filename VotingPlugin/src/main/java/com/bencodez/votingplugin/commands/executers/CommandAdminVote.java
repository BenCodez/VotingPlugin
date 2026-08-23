package com.bencodez.votingplugin.commands.executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

public class CommandAdminVote implements CommandExecutor {

	private VotingPluginMain plugin;

	public CommandAdminVote(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

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
