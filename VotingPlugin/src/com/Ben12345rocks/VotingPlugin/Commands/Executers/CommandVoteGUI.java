package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;

public class CommandVoteGUI implements CommandExecutor {

	@SuppressWarnings("unused")
	private Main plugin;

	public CommandVoteGUI(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (!Utils.getInstance().isPlayer(sender)) {
			sender.sendMessage("You must be a player to use this command");
			return true;
		}
		Player player = (Player) sender;

		if (!Utils.getInstance().hasPermission(player, "Commands.Vote.GUI")) {
			player.sendMessage("&cNo Permissions to use this command");
			return true;
		}

		if (args.length == 1) {
			if (args[0].equalsIgnoreCase("votesites")) {
			//	openVoteSitesListGUI(player, 1);
				return true;
			}
		}

		if (args.length == 2) {
			if (args[0].equalsIgnoreCase("votesites")) {
				if (Utils.getInstance().isInt(args[1])) {
				//	openVoteSitesListGUI(player, Integer.parseInt(args[1]));
				} else {
					player.sendMessage("&c" + args[1] + " is not an integer");
				}
				return true;
			}
		}

		// invalid command
		sender.sendMessage(Messages.getInstance().invalidCommand());

		return true;
	}

	

}
