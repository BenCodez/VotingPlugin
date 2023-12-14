package com.bencodez.votingplugin.bungee.velocity;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.velocitypowered.api.command.CommandSource;
import com.velocitypowered.api.command.SimpleCommand;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;

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

		if (args.length > 0) {
			if (args[0].equalsIgnoreCase("reload")) {
				plugin.reload(false);
				source.sendMessage(Component.text("Reloading VotingPluginBungee").color(NamedTextColor.AQUA));
			}
			if (args[0].equalsIgnoreCase("reloadmysql")) {
				plugin.reload(true);
				source.sendMessage(Component.text("Reloading VotingPluginBungee MySQL").color(NamedTextColor.AQUA));
			}
			if (args[0].equalsIgnoreCase("vote")) {
				if (args.length >= 2) {
					String user = args[1];
					String site = args[2];
					plugin.vote(user, site, false, true, 0, null, null);
					source.sendMessage(Component.text("Sending vote").color(NamedTextColor.AQUA));
				}
			}
			if (args[0].equalsIgnoreCase("forcetimechange")) {
				if (args.length >= 2) {
					plugin.getBungeeTimeChecker().forceChanged(TimeType.getTimeType(args[1]));
					source.sendMessage(Component.text("Triggering time change: " + args[1]).color(NamedTextColor.AQUA));
				}
			}
			if (args[0].equalsIgnoreCase("status")) {
				if (plugin.getMethod().equals(BungeeMethod.SOCKETS)) {
					source.sendMessage(
							Component.text("Checking status, waiting for response, check console, method: sockets")
									.color(NamedTextColor.AQUA));
					plugin.status();
				} else if (plugin.getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
					source.sendMessage(Component
							.text("Checking status, waiting for response, check console, method: pluginmessaging")
							.color(NamedTextColor.AQUA));
					plugin.status();
					plugin.sendServerNameMessage();
				} else if (plugin.getMethod().equals(BungeeMethod.REDIS)) {
					source.sendMessage(
							Component.text("Checking status, waiting for response, check console, method: redis")
									.color(NamedTextColor.AQUA));
					plugin.status();
					plugin.sendServerNameMessage();
				} else {
					source.sendMessage(Component.text("Not using socket/pluginmessage method, command unavailable")
							.color(NamedTextColor.AQUA));
				}
			}
			if (args[0].equalsIgnoreCase("multiproxystatus")) {
				plugin.sendMultiProxyServerMessage("Status");

				source.sendMessage(Component.text("Sending status message").color(NamedTextColor.AQUA));
			}
			if (args[0].equalsIgnoreCase("help")) {
				String[] msg = new String[7];
				msg[0] = "votingpluginbungee reload - Reload plugin";
				msg[1] = "votingpluginbungee reloadmysql - Reload plugin including mysql";
				msg[2] = "votingpluginbungee vote (player) (servicesite) - Send bungee vote";
				msg[3] = "votingpluginbungee status - Check socket connection status";
				msg[4] = "&avotingpluginbungee forcetimechange (timetype) - Force a time change";
				msg[5] = "&avotingpluginbungee voteparty force - Force a vote party";
				msg[6] = "&avotingpluginbungee voteparty setvotecount (number) - Set current vote party votes";
				for (String m : msg) {
					source.sendMessage(Component.text(m).color(NamedTextColor.AQUA));
				}
			}
			if (args[0].equalsIgnoreCase("voteparty")) {
				if (args.length > 1) {
					if (args[1].equalsIgnoreCase("Force")) {
						plugin.setCurrentVotePartyVotes(plugin.getCurrentVotePartyVotesRequired());
						plugin.checkVoteParty();
						source.sendMessage(Component.text("Vote party forced").color(NamedTextColor.AQUA));
					}
					if (args.length > 2) {
						if (args[1].equalsIgnoreCase("SetVoteCount")) {
							if (isInt(args[2])) {
								plugin.setCurrentVotePartyVotes(Integer.parseInt(args[2]));
								source.sendMessage(Component.text("Set current vote party votes to " + args[2])
										.color(NamedTextColor.AQUA));
							}
						}
					}
				}
			}
		}
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

	@Override
	public boolean hasPermission(final Invocation invocation) {
		return invocation.source().hasPermission("votingpluginbungee.admin");
	}
}
