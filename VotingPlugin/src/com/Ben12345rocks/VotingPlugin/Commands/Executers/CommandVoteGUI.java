package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;

public class CommandVoteGUI implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteGUI(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		CommandVote.getInstance().voteGUI(sender);
		return true;
	}

}
