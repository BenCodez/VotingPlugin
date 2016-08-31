package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

/**
 * The Class AdminVoteTabCompleter.
 */
public class AdminVoteTabCompleter implements TabCompleter {

	/** The plugin. */
	Main plugin = Main.plugin;

	/**
	 * Gets the admin tab complete options.
	 *
	 * @param sender
	 *            the sender
	 * @param args
	 *            the args
	 * @param argNum
	 *            the arg num
	 * @return the admin tab complete options
	 */
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
						String[] cmdArgsList = cmdArgs[argNum].split("&");
						for (String arg : cmdArgsList) {
							if (arg.equalsIgnoreCase("(player)")) {
								for (Object playerOb : Bukkit
										.getOnlinePlayers().toArray()) {
									Player player = (Player) playerOb;
									if (!cmds.contains(player.getName())) {
										cmds.add(player.getName());
									}
								}
							} else if (arg.equalsIgnoreCase("(sitename)")) {
								for (String siteName : ConfigVoteSites
										.getInstance().getVoteSitesNames()) {
									if (!cmds.contains(siteName)) {
										cmds.add(siteName);
									}
								}
							} else if (arg.equalsIgnoreCase("(reward)")) {
								for (String reward : ConfigRewards
										.getInstance().getRewardNames()) {
									if (!cmds.contains(reward)) {
										cmds.add(reward);
									}
								}
							} else if (arg.equalsIgnoreCase("(boolean)")) {
								if (!cmds.contains("True")) {
									cmds.add("True");
								}
								if (!cmds.contains("False")) {
									cmds.add("False");
								}
							} else if (arg.equalsIgnoreCase("(number)")) {

							} else if (!cmds.contains(arg)) {
								cmds.add(arg);
							}
						}

					}
				}

			}
		}

		Collections.sort(cmds, String.CASE_INSENSITIVE_ORDER);

		return cmds;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.bukkit.command.TabCompleter#onTabComplete(org.bukkit.command.
	 * CommandSender, org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] args) {

		if (cmd.getName().equalsIgnoreCase("adminvote")
				|| cmd.getName().equalsIgnoreCase("av")) {

			List<String> tab = new ArrayList<String>();

			Set<String> cmds = new HashSet<String>();

			for (CommandHandler cmdHandle : plugin.adminVoteCommand) {
				cmds.addAll(cmdHandle.getAdminTabCompleteOptions(sender, args,
						args.length - 1));
			}

			for (String str : cmds) {
				if (Utils.getInstance().startsWithIgnoreCase(str,
						args[args.length - 1])) {
					tab.add(str);
				}
			}

			Collections.sort(tab, String.CASE_INSENSITIVE_ORDER);
			return tab;

		}

		return null;
	}

}
