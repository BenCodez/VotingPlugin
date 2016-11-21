package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;

/**
 * The Class AliasesTabCompleter.
 */
public class AliasesTabCompleter implements TabCompleter {

	/** The plugin. */
	Main plugin = Main.plugin;

	/** The cmd handle. */
	public CommandHandler cmdHandle;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.bukkit.command.TabCompleter#onTabComplete(org.bukkit.command.
	 * CommandSender, org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd,
			String alias, String[] argsIn) {
		ArrayList<String> msgArray = new ArrayList<String>();
		msgArray.add("");
		for (String arg : argsIn) {
			msgArray.add(arg);
		}

		String[] args = ArrayUtils.getInstance().convert(msgArray);

		ArrayList<String> tab = new ArrayList<String>();

		Set<String> cmds = new HashSet<String>();

		ArrayList<CommandHandler> cmdHandlers = new ArrayList<CommandHandler>();
		cmdHandlers.addAll(plugin.voteCommand);
		cmdHandlers.addAll(plugin.adminVoteCommand);
		for (CommandHandler cmdHandle : cmdHandlers) {
			if (cmdHandle.getArgs().length >= argsIn.length) {
				for (String arg : cmdHandle.getArgs()[0].split("&")) {
					if (cmd.getName().equalsIgnoreCase("vote" + arg)
							|| cmd.getName()
									.equalsIgnoreCase("adminvote" + arg)) {
						// plugin.debug("Found cmd... attempting to get tab complete");
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

							cmds.addAll(cmdHandle.getTabCompleteOptions(sender,
									args, argsIn.length));
						}

					}
				}
			}
		}

		for (String str : cmds) {
			if (StringUtils.getInstance().startsWithIgnoreCase(str,
					args[args.length - 1])) {
				tab.add(str);
			}
		}

		Collections.sort(tab, String.CASE_INSENSITIVE_ORDER);

		return tab;
	}

	/**
	 * Sets the CMD handle.
	 *
	 * @param cmd
	 *            the cmd
	 * @return the aliases tab completer
	 */
	public AliasesTabCompleter setCMDHandle(CommandHandler cmd) {
		cmdHandle = cmd;
		return this;
	}

}
