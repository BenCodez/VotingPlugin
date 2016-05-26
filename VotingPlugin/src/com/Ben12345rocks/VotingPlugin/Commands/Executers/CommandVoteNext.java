package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;

public class CommandVoteNext implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteNext(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (args.length == 0) {
			CommandVote.getInstance().nextSelf(sender);
			return true;
		}

		if (args.length == 1) {
			CommandVote.getInstance().nextOther(sender, args[0]);
			return true;

		}

		// invalid command
		sender.sendMessage("Invalid Command, see /vote help");

		return true;
	}

}
