package com.bencodez.votingplugin.commands.executers;

import java.util.ArrayList;

import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandAliases.
 */
public class CommandAliases implements CommandExecutor {

	private boolean adminCommand;

	/** The cmd handle. */
	private CommandHandler cmdHandle;

	/** The plugin. */
	private VotingPluginMain plugin = VotingPluginMain.plugin;

	public CommandAliases(CommandHandler cmdHandle, boolean adminCommand) {
		this.cmdHandle = cmdHandle;
		this.adminCommand = adminCommand;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.command.CommandExecutor#onCommand(org.bukkit.command.
	 * CommandSender , org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {

		ArrayList<String> argsNew = new ArrayList<String>();
		argsNew.add(cmdHandle.getArgs()[0]);
		for (String arg : args) {
			argsNew.add(arg);
		}
		plugin.debug("Attempting cmd...");
		plugin.debug("Inputed args: " + ArrayUtils.getInstance().makeStringList(argsNew));

		ArrayList<CommandHandler> handles = new ArrayList<CommandHandler>();
		if (adminCommand) {
			handles.addAll(plugin.getAdminVoteCommand());
		} else {
			handles.addAll(plugin.getVoteCommand());
		}

		for (CommandHandler cmdHandle : handles) {
			if (cmdHandle.getArgs().length > args.length) {
				for (String arg : cmdHandle.getArgs()[0].split("&")) {
					if (cmd.getName().equalsIgnoreCase("vote" + arg)
							|| cmd.getName().equalsIgnoreCase("adminvote" + arg)) {
						argsNew.set(0, arg);

						boolean argsMatch = true;
						for (int i = 0; i < argsNew.size(); i++) {
							if (i < cmdHandle.getArgs().length) {
								if (!cmdHandle.argsMatch(argsNew.get(i), i)) {
									argsMatch = false;
								}
							}

						}

						if (argsMatch) {
							if (cmdHandle.runCommand(sender, ArrayUtils.getInstance().convert(argsNew))) {
								plugin.debug("cmd found, ran cmd");
								return true;
							}
						}
					}
				}
			}
		}

		/*
		 * for (String arg : cmdHandle.getArgs()[0].split("&")) { argsNew.set(0, arg);
		 * if (cmdHandle.runCommand(sender, Utils.getInstance().convertArray(argsNew)))
		 * { plugin.debug( "cmd found, ran cmd"); return true; } }
		 */

		// invalid command
		if (adminCommand) {
			sender.sendMessage(ChatColor.RED + "No valid arguments, see /adminvote help!");
		} else {
			sender.sendMessage(ChatColor.RED + "No valid arguments, see /vote help!");
		}
		return true;
	}
}
