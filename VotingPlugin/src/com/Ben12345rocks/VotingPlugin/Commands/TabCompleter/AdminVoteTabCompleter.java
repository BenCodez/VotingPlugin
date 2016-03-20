package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

public class AdminVoteTabCompleter implements TabCompleter {

	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] args) {

		if (cmd.getName().equalsIgnoreCase("adminvote")
				|| cmd.getName().equalsIgnoreCase("av")) {

			List<String> tab = new ArrayList<String>();

			if (args.length == 1) {

				List<String> cmds = new ArrayList<String>();

				cmds.add("vote");
				cmds.add("settotal");
				cmds.add("reload");
				cmds.add("uuid");
				cmds.add("bungeevote");
				cmds.add("reset");
				cmds.add("sites");
				cmds.add("version");
				cmds.add("help");

				for (int i = 0; i < cmds.size(); i++) {
					if (cmds.get(i).startsWith(args[0])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 2) {

				List<String> cmds = new ArrayList<String>();

				if (args[0].equalsIgnoreCase("reset")) {
					cmds.add("top");
				}

				if (args[0].equalsIgnoreCase("vote")
						|| args[0].equalsIgnoreCase("settotal")
						|| args[0].equalsIgnoreCase("uuid")
						|| args[0].equalsIgnoreCase("bungeevote")
						|| args[0].equalsIgnoreCase("reset")) {
					for (Object playerOb : Bukkit.getOnlinePlayers().toArray()) {
						Player player = (Player) playerOb;
						cmds.add(player.getName());
					}
				}

				if (args[0].equalsIgnoreCase("sites")) {
					cmds.addAll(ConfigVoteSites.getInstance()
							.getVoteSitesName());
				}

				for (int i = 0; i < cmds.size(); i++) {
					if (cmds.get(i).startsWith(args[1])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 3) {
				if (args[0].equalsIgnoreCase("vote")
						|| args[0].equalsIgnoreCase("settotal")
						|| args[0].equalsIgnoreCase("bungeevote")) {

					List<String> cmds = new ArrayList<String>();

					for (String siteName : ConfigVoteSites.getInstance()
							.getVoteSitesName()) {
						cmds.add(siteName);
					}

					for (int i = 0; i < cmds.size(); i++) {
						if (cmds.get(i).startsWith(args[2])) {
							tab.add(cmds.get(i));
						}
					}

					return tab;
				}
			}

		}

		return null;
	}

}
