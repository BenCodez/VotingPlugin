package com.bencodez.votingplugin.commands.tabcompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

/**
 * The Class AliasesTabCompleter.
 */
public class AliasesTabCompleter implements TabCompleter {

	private boolean adminCommand;

	/** The cmd handle. */
	public CommandHandler cmdHandle;

	/** The plugin. */
	VotingPluginMain plugin = VotingPluginMain.plugin;

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.command.TabCompleter#onTabComplete(org.bukkit.command.
	 * CommandSender, org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd, String alias, String[] argsIn) {
		ArrayList<String> msgArray = new ArrayList<String>();
		msgArray.add("");
		for (String arg : argsIn) {
			msgArray.add(arg);
		}

		String[] args = ArrayUtils.convert(msgArray);

		ArrayList<String> tab = new ArrayList<String>();

		Set<String> cmds = new HashSet<String>();

		ArrayList<CommandHandler> cmdHandlers = new ArrayList<CommandHandler>();

		if (adminCommand) {
			cmdHandlers.addAll(plugin.getAdminVoteCommand());
		} else {
			cmdHandlers.addAll(plugin.getVoteCommand());
		}
		ConcurrentHashMap<String, ArrayList<String>> tabCompletes = TabCompleteHandler.getInstance()
				.getTabCompleteOptions();
		for (CommandHandler cmdHandle : cmdHandlers) {
			if (cmdHandle.getArgs().length >= argsIn.length) {
				for (String arg : cmdHandle.getArgs()[0].split("&")) {
					if (cmd.getName().equalsIgnoreCase("vote" + arg)
							|| cmd.getName().equalsIgnoreCase("adminvote" + arg)) {
						// plugin.debug("Found cmd... attempting to get tab
						// complete");
						args[0] = arg;
						boolean argsMatch = true;
						for (int i = 0; i < argsIn.length; i++) {
							if (args.length >= i) {
								if (!cmdHandle.argsMatch(args[i], i)) {
									argsMatch = false;
								}
							}
						}

						if (argsMatch) {

							cmds.addAll(cmdHandle.getTabCompleteOptions(sender, args, argsIn.length, tabCompletes));
						}

					}
				}
			}
		}

		for (String str : cmds) {
			if (MessageAPI.startsWithIgnoreCase(str, args[args.length - 1])) {
				tab.add(str);
			}
		}

		Collections.sort(tab, String.CASE_INSENSITIVE_ORDER);

		return tab;
	}

	public AliasesTabCompleter setCMDHandle(CommandHandler cmd, boolean adminCommand) {
		cmdHandle = cmd;
		this.adminCommand = adminCommand;
		return this;
	}

}
