package com.bencodez.votingplugin.commands.executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

public class CommandVote implements CommandExecutor {

	private static CommandVote instance = new CommandVote();

	private static VotingPluginMain plugin;

	public static CommandVote getInstance() {
		return instance;
	}

	private CommandVote() {
	}

	public CommandVote(VotingPluginMain plugin) {
		CommandVote.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {

		for (CommandHandler commandHandler : plugin.getVoteCommand()) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(MessageAPI.colorize(plugin.getConfigFile().getFormatInvalidCommandVote()));
		return true;
	}

}
