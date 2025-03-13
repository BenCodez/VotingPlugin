package com.bencodez.votingplugin.proxy.velocity;

import com.bencodez.votingplugin.proxy.VotingPluginProxyCommand;
import com.velocitypowered.api.command.CommandSource;
import com.velocitypowered.api.command.SimpleCommand;

import net.kyori.adventure.text.Component;

public class VotingPluginVelocityCommand implements SimpleCommand {
	private VotingPluginVelocity plugin;

	public VotingPluginVelocityCommand(VotingPluginVelocity plugin) {
		this.plugin = plugin;
	}

	@Override
	public void execute(final Invocation invocation) {
		CommandSource source = invocation.source();
		// Get the arguments after the command alias
		String[] args = invocation.arguments();

		String result = new VotingPluginProxyCommand(plugin.getVotingPluginProxy()).execute(args);
		if (result != null) {
			source.sendMessage(Component.text(result.replace("&", "§")));
		}

	}

	@Override
	public boolean hasPermission(final Invocation invocation) {
		return invocation.source().hasPermission("votingpluginbungee.admin");
	}

	public boolean isInt(String st) {
		if (st == null) {
			return false;
		}
		try {
			@SuppressWarnings("unused")
			int num = Integer.parseInt(st);
			return true;

		} catch (NumberFormatException ex) {
			return false;
		}
	}
}
