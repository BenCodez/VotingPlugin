package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;

public class CommandVoteHelp implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteHelp(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		CommandVote.getInstance().help(sender);
		return true;
	}

	public void help(CommandSender sender) {
		if (Utils.getInstance().hasPermission(sender, "Commands.Vote.Help")
				|| Utils.getInstance().hasPermission(sender, "Player")) {
			sender.sendMessage(Commands.getInstance().voteHelp());

		} else {
			sender.sendMessage(Messages.getInstance().noPerms());
		}
	}

}
