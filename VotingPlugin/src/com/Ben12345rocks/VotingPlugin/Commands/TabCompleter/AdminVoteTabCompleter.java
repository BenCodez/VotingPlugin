package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Utils;
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

				cmds.add("Vote");
				cmds.add("SetTotal");
				cmds.add("Reload");
				cmds.add("UUID");
				cmds.add("BungeeVote");
				cmds.add("GlobalVote");
				cmds.add("Reset");
				cmds.add("Sites");
				cmds.add("Version");
				cmds.add("Help");
				cmds.add("VoteSite");

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[0])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 2) {

				List<String> cmds = new ArrayList<String>();

				if (args[0].equalsIgnoreCase("reset")) {
					cmds.add("Top");
				}

				if (args[0].equalsIgnoreCase("vote")
						|| args[0].equalsIgnoreCase("settotal")
						|| args[0].equalsIgnoreCase("uuid")
						|| args[0].equalsIgnoreCase("bungeevote")
						|| args[0].equalsIgnoreCase("reset")
						|| args[0].equalsIgnoreCase("globalvote")) {
					for (Object playerOb : Bukkit.getOnlinePlayers().toArray()) {
						Player player = (Player) playerOb;
						cmds.add(player.getName());
					}
				}

				if (args[0].equalsIgnoreCase("sites")
						|| args[0].equalsIgnoreCase("votesite")) {
					cmds.addAll(ConfigVoteSites.getInstance()
							.getVoteSitesNames());
				}

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[1])) {
						tab.add(cmds.get(i));
					}
				}
				return tab;

			} else if (args.length == 3) {
				List<String> cmds = new ArrayList<String>();
				if (args[0].equalsIgnoreCase("vote")
						|| args[0].equalsIgnoreCase("settotal")
						|| args[0].equalsIgnoreCase("bungeevote")
						|| args[0].equalsIgnoreCase("reset")
						|| args[0].equalsIgnoreCase("globalvote")) {

					for (String siteName : ConfigVoteSites.getInstance()
							.getVoteSitesNames()) {
						cmds.add(siteName);
					}
				}
				if (args[0].equalsIgnoreCase("VoteSite")) {
					cmds.add("AddItem");
				}

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[2])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			}

		}

		return null;
	}

}
