package com.bencodez.votingplugin.commands.tabcompleter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;

import com.bencodez.advancedcore.api.command.AdvancedCoreTabCompleteHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteTabCompleter.
 */
public class VoteTabCompleter implements TabCompleter {

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
	public List<String> onTabComplete(CommandSender sender, Command cmd, String alias, String[] args) {

		ArrayList<String> tab = new ArrayList<>();

		Set<String> cmds = new HashSet<>();

		cmds.addAll(AdvancedCoreTabCompleteHandler.getInstance().getTabCompleteOptions(plugin.getVoteCommand(), sender,
				args, args.length - 1));

		for (String str : cmds) {
			if (MessageAPI.startsWithIgnoreCase(str, args[args.length - 1])) {
				tab.add(str);
			}
		}

		Collections.sort(tab, String.CASE_INSENSITIVE_ORDER);

		return tab;
	}

}
