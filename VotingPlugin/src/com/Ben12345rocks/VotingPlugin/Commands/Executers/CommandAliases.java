package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.ArrayList;

import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;

public class CommandAliases implements CommandExecutor {

	private Main plugin = Main.plugin;

	private CommandHandler cmdHandle;

	public CommandAliases(CommandHandler cmdHandle) {
		this.cmdHandle = cmdHandle;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		ArrayList<String> argsNew = new ArrayList<String>();
		argsNew.add(cmdHandle.getArgs()[0]);
		for (String arg : args) {
			argsNew.add(arg);
		}
		plugin.debug("Attempting cmd...");
		plugin.debug("Inputed args: "
				+ Utils.getInstance().makeStringList(argsNew));
		// plugin.debug(Utils.getInstance().makeStringList(
		// Utils.getInstance().convertArray(cmdHandle.getArgs())));

		ArrayList<CommandHandler> cmdHandlers = new ArrayList<CommandHandler>();
		cmdHandlers.addAll(plugin.voteCommand);
		cmdHandlers.addAll(plugin.adminVoteCommand);
		for (CommandHandler cmdHandle : cmdHandlers) {
			if (cmdHandle.getArgs().length > 0) {
				for (String arg : cmdHandle.getArgs()[0].split("&")) {
					if (cmd.getName().equalsIgnoreCase("vote" + arg)
							|| cmd.getName()
									.equalsIgnoreCase("adminvote" + arg)) {
						
						argsNew.set(0, arg);
						if (cmdHandle.runCommand(sender, Utils.getInstance()
								.convertArray(argsNew))) {
							plugin.debug("cmd found, ran cmd");
							return true;
						}
					}
				}
			}
		}

		/*
		 * for (String arg : cmdHandle.getArgs()[0].split("&")) { argsNew.set(0,
		 * arg); if (cmdHandle.runCommand(sender,
		 * Utils.getInstance().convertArray(argsNew))) {
		 * plugin.debug("cmd found, ran cmd"); return true; } }
		 */

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /vote help!");
		return true;
	}
}
