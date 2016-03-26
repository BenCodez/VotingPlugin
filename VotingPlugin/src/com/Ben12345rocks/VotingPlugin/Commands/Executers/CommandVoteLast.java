package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;

public class CommandVoteLast implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteLast(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (args.length == 0) {
			CommandVote.getInstance().lastSelf(sender);
			return true;
		}

		if (args.length == 1) {
			CommandVote.getInstance().lastOther(sender, args[0]);
			return true;

		}

		// invalid command
		sender.sendMessage(Messages.getInstance().invalidCommand());

		return true;
	}

}
