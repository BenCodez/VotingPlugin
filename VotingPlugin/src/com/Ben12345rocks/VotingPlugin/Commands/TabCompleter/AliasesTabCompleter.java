package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.player.PlayerChatTabCompleteEvent;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;

public class AliasesTabCompleter implements TabCompleter {
	Main plugin = Main.plugin;

	public CommandHandler cmdHandle;

	public ArrayList<String> getTabCompleteOptions(CommandSender sender,
			String[] args, int argNum, CommandHandler cmdHandle) {
		ArrayList<String> cmds = new ArrayList<String>();
		if (sender.hasPermission(cmdHandle.getPerm())) {
			String[] cmdArgs = cmdHandle.getArgs();
			if (cmdArgs.length > argNum) {
				boolean argsMatch = true;
				for (int i = 0; i < argNum; i++) {
					if (args.length >= i) {
						if (!cmdHandle.argsMatch(args[i], i)) {
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
						for (String siteName : ConfigVoteSites.getInstance()
								.getVoteSitesNames()) {
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

		Collections.sort(cmds, String.CASE_INSENSITIVE_ORDER);

		return cmds;
	}

	@EventHandler
	public void onChatTab(PlayerChatTabCompleteEvent event) {
		Player sender = event.getPlayer();
		Collection<String> msg = event.getTabCompletions();
		String[] msgs = event.getChatMessage().split(" ");
		String cmd = msgs[0];
		CommandHandler cmdHandle = null;
		for (String command : CommandLoader.getInstance().getCommands()
				.keySet()) {
			if (CommandLoader.getInstance().getCommands().get(command)
					.equals(cmd)) {
				cmdHandle = CommandLoader.getInstance().getCommands()
						.get(command);
			}
		}
		if (cmdHandle == null) {
			return;
		}
		ArrayList<String> msgArray = new ArrayList<String>();
		for (int i = 1; i < msgs.length; i++) {
			msgArray.add(msgs[i]);
		}
		String[] args = Utils.getInstance().convertArray(msgArray);

		ArrayList<String> tab = new ArrayList<String>();

		ArrayList<String> cmds = new ArrayList<String>();

		cmds.addAll(getTabCompleteOptions(sender, args, args.length + 1,
				cmdHandle));

		for (int i = 0; i < cmds.size(); i++) {
			if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
					args[args.length - 1])) {
				tab.add(cmds.get(i));
			}
		}

		msg.addAll(tab);

	}

	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] argsIn) {
		ArrayList<String> msgArray = new ArrayList<String>();
		msgArray.add(cmdHandle.getArgs()[0]);
		for (int i = 1; i < argsIn.length; i++) {
			msgArray.add(argsIn[i]);
		}
		String[] args = Utils.getInstance().convertArray(msgArray);

		ArrayList<String> tab = new ArrayList<String>();

		ArrayList<String> cmds = new ArrayList<String>();

		cmds.addAll(getTabCompleteOptions(sender, args, args.length, cmdHandle));

		for (int i = 0; i < cmds.size(); i++) {
			if (Utils.getInstance().startsWithIgnoreCase(cmds.get(i),
					args[argsIn.length])) {
				tab.add(cmds.get(i));
			}
		}

		return tab;
	}
	
	public AliasesTabCompleter setCMDHandle(CommandHandler cmd) {
		this.cmdHandle = cmd;
		return this;
	}

}
