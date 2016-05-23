package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;

public class VoteTabCompleter implements TabCompleter {
	Main plugin = Main.plugin;

	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] args) {

		if (cmd.getName().equalsIgnoreCase("vote")
				|| cmd.getName().equalsIgnoreCase("v")) {

			ArrayList<String> tab = new ArrayList<String>();

			if (args.length == 1) {

				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.voteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 1) {

						if (cmdArgs[0].equalsIgnoreCase("player")) {
							for (Object playerOb : Bukkit.getOnlinePlayers()
									.toArray()) {
								Player player = (Player) playerOb;
								cmds.add(player.getName());
							}
						} else if (cmdArgs[0].equalsIgnoreCase("sitename")) {
							cmds.addAll(ConfigVoteSites.getInstance()
									.getVoteSitesNames());
						} else if (cmdArgs[1].equalsIgnoreCase("boolean")) {
							cmds.add("True");
							cmds.add("False");
						} else {
							cmds.add(cmdArgs[0]);
						}

					}
				}

				cmds = Utils.getInstance().removeDuplicates(cmds);

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[0])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 2) {

				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.voteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 1) {
						if (commandHandler.argsMatch(args[0], 0)) {
							if (cmdArgs[1].equalsIgnoreCase("player")) {
								for (Object playerOb : Bukkit
										.getOnlinePlayers().toArray()) {
									Player player = (Player) playerOb;
									cmds.add(player.getName());
								}
							} else if (cmdArgs[1].equalsIgnoreCase("sitename")) {
								cmds.addAll(ConfigVoteSites.getInstance()
										.getVoteSitesNames());
							} else if (cmdArgs[1].equalsIgnoreCase("boolean")) {
								cmds.add("True");
								cmds.add("False");
							} else {
								cmds.add(cmdArgs[1]);
							}
						}

					}
				}

				cmds = Utils.getInstance().removeDuplicates(cmds);

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[1])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			}

		}

		return null;
	}

}
