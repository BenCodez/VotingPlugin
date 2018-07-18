package com.Ben12345rocks.VotingPlugin.Commands.TabCompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;

import com.Ben12345rocks.AdvancedCore.Objects.TabCompleteHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteTabCompleter.
 */
public class VoteTabCompleter implements TabCompleter {
	private Main main = ServiceLocator.getService(Main.class);

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.command.TabCompleter#onTabComplete(org.bukkit.command.
	 * CommandSender, org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public List<String> onTabComplete(CommandSender sender, Command cmd, String alias, String[] args) {

		ArrayList<String> tab = new ArrayList<String>();

		Set<String> cmds = new HashSet<String>();

		cmds.addAll(TabCompleteHandler.getInstance().getTabCompleteOptions(main.voteCommand, sender, args,
				args.length - 1));

		for (String str : cmds) {
			if (StringUtils.getInstance().startsWithIgnoreCase(str, args[args.length - 1])) {
				tab.add(str);
			}
		}

		Collections.sort(tab, String.CASE_INSENSITIVE_ORDER);

		return tab;
	}

}
