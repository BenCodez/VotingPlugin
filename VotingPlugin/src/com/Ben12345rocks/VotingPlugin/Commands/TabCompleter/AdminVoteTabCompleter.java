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

public class AdminVoteTabCompleter implements TabCompleter {
	Main plugin = Main.plugin;

	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] args) {

		if (cmd.getName().equalsIgnoreCase("adminvote")
				|| cmd.getName().equalsIgnoreCase("av")) {

			List<String> tab = new ArrayList<String>();

			if (args.length == 1) {

				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.adminVoteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 0) {

						if (cmdArgs[0].equalsIgnoreCase("player")) {
							for (Object playerOb : Bukkit.getOnlinePlayers()
									.toArray()) {
								Player player = (Player) playerOb;
								if (!cmds.contains(player.getName())) {
									cmds.add(player.getName());
								}
							}
						} else if (cmdArgs[0].equalsIgnoreCase("sitename")) {
							for (String siteName : ConfigVoteSites
									.getInstance().getVoteSitesNames()) {
								if (!cmds.contains(siteName)) {
									cmds.add(siteName);
								}
							}
						} else if (cmdArgs[0].equalsIgnoreCase("boolean")) {
							if (!cmds.contains("True")) {
								cmds.add("True");
							}
							if (!cmds.contains("False")) {
								cmds.add("False");
							}
						} else {
							if (!cmds.contains(cmdArgs[0])) {
								cmds.add(cmdArgs[0]);
							}
						}

					}
				}

				// cmds = Utils.getInstance().removeDuplicates(cmds);

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[0])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 2) {

				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.adminVoteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 1) {
						if (commandHandler.argsMatch(args[0], 0)) {
							if (cmdArgs[1].equalsIgnoreCase("player")) {
								for (Object playerOb : Bukkit
										.getOnlinePlayers().toArray()) {
									Player player = (Player) playerOb;
									if (!cmds.contains(player.getName())) {
										cmds.add(player.getName());
									}
								}
							} else if (cmdArgs[1].equalsIgnoreCase("sitename")) {
								for (String siteName : ConfigVoteSites
										.getInstance().getVoteSitesNames()) {
									if (!cmds.contains(siteName)) {
										cmds.add(siteName);
									}
								}
							} else if (cmdArgs[1].equalsIgnoreCase("boolean")) {
								if (!cmds.contains("True")) {
									cmds.add("True");
								}
								if (!cmds.contains("False")) {
									cmds.add("False");
								}
							} else {
								if (!cmds.contains(cmdArgs[1])) {
									cmds.add(cmdArgs[1]);
								}
							}
						}

					}
				}

				// cmds = Utils.getInstance().removeDuplicates(cmds);

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[1])) {
						tab.add(cmds.get(i));
					}
				}
				return tab;

			} else if (args.length == 3) {
				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.adminVoteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 2) {
						if (commandHandler.argsMatch(args[0], 0)) {
							if (commandHandler.argsMatch(args[1], 1)) {
								if (cmdArgs[2].equalsIgnoreCase("player")) {
									for (Object playerOb : Bukkit
											.getOnlinePlayers().toArray()) {
										Player player = (Player) playerOb;
										if (!cmds.contains(player.getName())) {
											cmds.add(player.getName());
										}
									}
								} else if (cmdArgs[2]
										.equalsIgnoreCase("sitename")) {
									for (String siteName : ConfigVoteSites
											.getInstance().getVoteSitesNames()) {
										if (!cmds.contains(siteName)) {
											cmds.add(siteName);
										}
									}
								} else if (cmdArgs[2]
										.equalsIgnoreCase("boolean")) {
									if (!cmds.contains("True")) {
										cmds.add("True");
									}
									if (!cmds.contains("False")) {
										cmds.add("False");
									}
								} else {
									if (!cmds.contains(cmdArgs[2])) {
										cmds.add(cmdArgs[2]);
									}
								}
							}
						}

					}
				}
				// cmds = Utils.getInstance().removeDuplicates(cmds);

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[2])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 4) {
				ArrayList<String> cmds = new ArrayList<String>();

				for (CommandHandler commandHandler : plugin.adminVoteCommand) {

					String[] cmdArgs = commandHandler.getArgs();
					if (cmdArgs.length > 3) {
						if (commandHandler.argsMatch(args[0], 0)) {
							if (commandHandler.argsMatch(args[1], 1)) {
								if (commandHandler.argsMatch(args[2], 2)) {
									if (cmdArgs[3].equalsIgnoreCase("player")) {
										for (Object playerOb : Bukkit
												.getOnlinePlayers().toArray()) {
											Player player = (Player) playerOb;
											if (!cmds
													.contains(player.getName())) {
												cmds.add(player.getName());
											}
										}
									} else if (cmdArgs[3]
											.equalsIgnoreCase("sitename")) {
										for (String siteName : ConfigVoteSites
												.getInstance()
												.getVoteSitesNames()) {
											if (!cmds.contains(siteName)) {
												cmds.add(siteName);
											}
										}
									} else if (cmdArgs[3]
											.equalsIgnoreCase("boolean")) {
										if (!cmds.contains("True")) {
											cmds.add("True");
										}
										if (!cmds.contains("False")) {
											cmds.add("False");
										}
									} else {
										if (!cmds.contains(cmdArgs[3])) {
											cmds.add(cmdArgs[3]);
										}
									}
								}
							}
						}

					}
				}

				// cmds = Utils.getInstance().removeDuplicates(cmds);

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
