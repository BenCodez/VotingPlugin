package com.bencodez.votingplugin.commands.tabcompleter;

import java.util.Locale;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerCommandSendEvent;

import com.bencodez.votingplugin.VotingPluginMain;

public class AliasCommandFilterListener implements Listener {

	private final VotingPluginMain plugin;

	public AliasCommandFilterListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@EventHandler
	public void onPlayerCommandSend(PlayerCommandSendEvent event) {
		// Only hide when aliases feature is disabled
		if (plugin.getConfigFile().isLoadCommandAliases()) {
			return;
		}

		event.getCommands().removeIf(cmd -> isAliasCommand(cmd));
	}

	private boolean isAliasCommand(String cmd) {
		String name = cmd.toLowerCase(Locale.ROOT);

		// Keep the main commands
		if (name.equals("vote") || name.equals("adminvote")) {
			return false;
		}

		// Any other command starting with vote/adminvote (votehelp, vhelp, avhelp,
		// etc.)
		return name.startsWith("vote") || name.startsWith("adminvote");
	}
}
