package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteTabCompleter.
 */
public class VoteTabCompleter implements TabCompleter {

	/** The plugin. */
	Main plugin = Main.plugin;

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

		if (cmd.getName().equalsIgnoreCase("vote")
				|| cmd.getName().equalsIgnoreCase("v")) {

			ArrayList<String> tab = new ArrayList<String>();

			Set<String> cmds = new HashSet<String>();

			for (CommandHandler cmdHandle : plugin.voteCommand) {
				cmds.addAll(cmdHandle.getTabCompleteOptions(sender, args,
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
