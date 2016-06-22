package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collections;
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

public class AdminVoteTabCompleter implements TabCompleter {
	Main plugin = Main.plugin;

	public ArrayList<String> getAdminTabCompleteOptions(CommandSender sender,
			String[] args, int argNum) {
		ArrayList<String> cmds = new ArrayList<String>();
		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (sender.hasPermission(commandHandler.getPerm())) {
				String[] cmdArgs = commandHandler.getArgs();
				if (cmdArgs.length > argNum) {
					boolean argsMatch = true;
					for (int i = 0; i < argNum; i++) {
						if (args.length >= i) {
							if (!commandHandler.argsMatch(args[i], i)) {
								argsMatch = false;
							}
						}
					}

					if (argsMatch) {
						if (cmdArgs[argNum].equalsIgnoreCase("player")) {
							for (Object playerOb : Bukkit.getOnlinePlayers()
									.toArray()) {
								Player player = (Player) playerOb;
								if (!cmds.contains(player.getName())) {
									cmds.add(player.getName());
								}
							}
						} else if (cmdArgs[argNum].equalsIgnoreCase("sitename")) {
							for (String siteName : ConfigVoteSites
									.getInstance().getVoteSitesNames()) {
								if (!cmds.contains(siteName)) {
									cmds.add(siteName);
								}
							}
						} else if (cmdArgs[argNum].equalsIgnoreCase("boolean")) {
							if (!cmds.contains("True")) {
								cmds.add("True");
							}
							if (!cmds.contains("False")) {
								cmds.add("False");
							}
						} else if (cmdArgs[argNum].equalsIgnoreCase("number")) {

						} else if (!cmds.contains(cmdArgs[argNum])) {
							cmds.add(cmdArgs[argNum]);
						}
					}
				}

			}
		}

		Collections.sort(cmds, String.CASE_INSENSITIVE_ORDER);

		return cmds;
	}

	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] args) {

		if (cmd.getName().equalsIgnoreCase("adminvote")
				|| cmd.getName().equalsIgnoreCase("av")) {

			List<String> tab = new ArrayList<String>();

			if (args.length == 1) {

				ArrayList<String> cmds = new ArrayList<String>();

				cmds.addAll(getAdminTabCompleteOptions(sender, args, 0));

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[0])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 2) {

				ArrayList<String> cmds = new ArrayList<String>();

				cmds.addAll(getAdminTabCompleteOptions(sender, args, 1));

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[1])) {
						tab.add(cmds.get(i));
					}
				}
				return tab;

			} else if (args.length == 3) {
				ArrayList<String> cmds = new ArrayList<String>();

				cmds.addAll(getAdminTabCompleteOptions(sender, args, 2));

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[2])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 4) {
				ArrayList<String> cmds = new ArrayList<String>();

				cmds.addAll(getAdminTabCompleteOptions(sender, args, 3));

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[3])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			}

		}

		return null;
	}

}
