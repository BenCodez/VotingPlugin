package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;

public class CommandVoteTop implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteTop(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (args.length == 0) {
			CommandVote.getInstance().topVoter(sender, 1);
			return true;
		}

		if (args.length == 1) {

			if (Utils.getInstance().isInt(args[0])) {
				CommandVote.getInstance().topVoter(sender,
						Integer.parseInt(args[0]));
			} else {
				sender.sendMessage(Utils.getInstance().colorize(
						"&cError on " + args[0] + ", number expected"));
			}
			return true;

		}

		// invalid command
		sender.sendMessage(Messages.getInstance().invalidCommand());

		return true;
	}

}
