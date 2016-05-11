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
				cmds.add("ServerVote");
				cmds.add("Reset");
				cmds.add("Sites");
				cmds.add("Version");
				cmds.add("Help");
				cmds.add("VoteSite");
				cmds.add("BonusReward");
				cmds.add("Config");
				cmds.add("ServerData");

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
						|| args[0].equalsIgnoreCase("servervote")) {
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
				if (args[0].equalsIgnoreCase("BonusReward")) {
					cmds.add("AddItem");
					cmds.add("SetMoney");
					cmds.add("SetGiveBonusReward");
					cmds.add("AddCommandPlayer");
					cmds.add("AddCommandConsole");
					cmds.add("SetExtraRewardChance");
					cmds.add("AddExtraRewardItem");
					cmds.add("SetExtraRewardMoney");
					cmds.add("AddExtraRewardCommandPlayer");
					cmds.add("AddExtraRewardCommandConsole");
				}

				if (args[0].equalsIgnoreCase("Config")) {
					cmds.add("SetDebug");
					cmds.add("SetBroadcastVote");
					cmds.add("SetUpdateReminder");
					cmds.add("SetAllowUnjoined");
					cmds.add("SetDisableTopVoterAwards");
				}

				if (args[0].equalsIgnoreCase("ServerData")) {
					cmds.add("SetPrevMonth");
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
						|| args[0].equalsIgnoreCase("servervote")) {

					for (String siteName : ConfigVoteSites.getInstance()
							.getVoteSitesNames()) {
						cmds.add(siteName);
					}
				}
				if (args[0].equalsIgnoreCase("VoteSite")) {
					cmds.add("AddItem");
					cmds.add("SetMoney");
					cmds.add("SetServiceSite");
					cmds.add("SetVoteURL");
					cmds.add("SetExtraRewardChance");
					cmds.add("SetDisabled");
					cmds.add("SetPriority");
					cmds.add("SetVoteDelay");
					cmds.add("AddCommandPlayer");
					cmds.add("AddCommandConsole");
					cmds.add("Create");
					cmds.add("AddExtraRewardItem");
					cmds.add("SetExtraRewardMoney");
					cmds.add("AddExtraRewardCommandPlayer");
					cmds.add("AddExtraRewardCommandConsole");
				}
				if (args[0].equalsIgnoreCase("Config")) {
					if (args[1].equalsIgnoreCase("SetDebug")) {
						cmds.add("True");
						cmds.add("False");
					}
					if (args[1].equalsIgnoreCase("SetBroadcastVote")) {
						cmds.add("True");
						cmds.add("False");
					}
					if (args[1].equalsIgnoreCase("SetUpdateReminder")) {
						cmds.add("True");
						cmds.add("False");
					}
					if (args[1].equalsIgnoreCase("SetAllowUnjoined")) {
						cmds.add("True");
						cmds.add("False");
					}
					if (args[1].equalsIgnoreCase("SetDisableTopVoterAwards")) {
						cmds.add("True");
						cmds.add("False");
					}
					/*
					 * if (args[1].equalsIgnoreCase("SetDisableJson")) {
					 * cmds.add("True"); cmds.add("False"); }
					 */
				}
				if (args[0].equalsIgnoreCase("BonusReward")) {
					if (args[1].equalsIgnoreCase("SetGiveBonusReward")) {
						cmds.add("True");
						cmds.add("False");
					}
				}

				for (int i = 0; i < cmds.size(); i++) {
					if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
							args[2])) {
						tab.add(cmds.get(i));
					}
				}

				return tab;

			} else if (args.length == 4) {
				List<String> cmds = new ArrayList<String>();

				if (args[0].equalsIgnoreCase("VoteSite")) {
					if (args[2].equalsIgnoreCase("SetDisabled")) {
						cmds.add("True");
						cmds.add("False");
					}

				}

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
