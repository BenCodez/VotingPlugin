package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;

public class CommandVoteTotal implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteTotal(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (args.length == 0) {
			CommandVote.getInstance().totalSelf(sender);
			return true;
		}

		if (args.length == 1) {
			if (args[0].equalsIgnoreCase("all")) {
				CommandVote.getInstance().totalAll(sender);
			} else {
				CommandVote.getInstance().totalOther(sender, args[1]);
			}
			return true;

		}

		// invalid command
		sender.sendMessage("Invalid Command, see /vote help");

		return true;
	}

}
