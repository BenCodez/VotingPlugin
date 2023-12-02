package com.bencodez.votingplugin.bungee;

import com.bencodez.advancedcore.api.time.TimeType;

import net.md_5.bungee.api.CommandSender;
import net.md_5.bungee.api.chat.TextComponent;
import net.md_5.bungee.api.plugin.Command;

public class VotingPluginBungeeCommand extends Command {
	private VotingPluginBungee bungee;

	public VotingPluginBungeeCommand(VotingPluginBungee bungee) {
		super("votingpluginbungee", "votingplugin.admin");
		this.bungee = bungee;
	}

	@Override
	public void execute(CommandSender sender, String[] args) {
		if (sender.hasPermission("votingplugin.admin")) {
			if (args.length > 0) {
				if (args[0].equalsIgnoreCase("reload")) {
					bungee.reload(false);
					sender.sendMessage(new TextComponent("Reloading VotingPluginBungee"));
				}
				if (args[0].equalsIgnoreCase("reloadmysql")) {
					bungee.reload(true);
					sender.sendMessage(new TextComponent("Reloading VotingPluginBungee with MySQL"));
				}
				if (args[0].equalsIgnoreCase("vote")) {
					if (args.length >= 2) {
						String user = args[1];
						String site = args[2];
						bungee.vote(user, site, false, true, 0, null, null);
						sender.sendMessage(new TextComponent("Sending vote"));
					}
				}
				if (args[0].equalsIgnoreCase("forcetimechange")) {
					if (args.length >= 2) {
						bungee.getBungeeTimeChecker().forceChanged(TimeType.getTimeType(args[1]));
						sender.sendMessage(new TextComponent("Triggering time change: " + args[1]));
					}
				}
				if (args[0].equalsIgnoreCase("status")) {
					if (bungee.getMethod().equals(BungeeMethod.SOCKETS)) {
						sender.sendMessage(new TextComponent(
								"&aChecking status, waiting for response, check console, method: sockets"));
						bungee.status(sender);
					} else if (bungee.getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
						sender.sendMessage(new TextComponent(
								"&aChecking status, waiting for response, check console, method: plugin messaging"));
						bungee.status(sender);
						bungee.sendServerNameMessage();
					} else {
						sender.sendMessage(
								new TextComponent("&aNot using socket/pluginmessage method, command unavailable"));
					}
				}
				if (args[0].equalsIgnoreCase("multiproxystatus")) {
					bungee.sendMultiProxyServerMessage("Status");
					sender.sendMessage(new TextComponent("&aSending status message"));
				}
				if (args[0].equalsIgnoreCase("help")) {
					TextComponent[] msg = new TextComponent[7];
					msg[0] = new TextComponent("&avotingpluginbungee reload - Reload plugin");
					msg[1] = new TextComponent("&avotingpluginbungee reloadmysql - Reload plugin including mysql");
					msg[2] = new TextComponent("&avotingpluginbungee vote (player) (servicesite) - Send bungee vote");
					msg[3] = new TextComponent("&avotingpluginbungee status - Check socket connection status");
					msg[4] = new TextComponent("&avotingpluginbungee forcetimechange (timetype) - Force a time change");
					msg[5] = new TextComponent("&avotingpluginbungee voteparty force - Force a vote party");
					msg[6] = new TextComponent(
							"&avotingpluginbungee voteparty setvotecount (number) - Set current vote party votes");
					sender.sendMessage(msg);
				}
				if (args[0].equalsIgnoreCase("voteparty")) {
					if (args.length > 1) {
						if (args[1].equalsIgnoreCase("Force")) {
							bungee.setCurrentVotePartyVotes(bungee.getCurrentVotePartyVotesRequired());
							bungee.checkVoteParty();
							sender.sendMessage(new TextComponent("Vote party forced"));
						}
						if (args.length > 2) {
							if (args[1].equalsIgnoreCase("SetVoteCount")) {
								if (isInt(args[2])) {
									bungee.setCurrentVotePartyVotes(Integer.parseInt(args[2]));
									sender.sendMessage(new TextComponent("Set current vote party votes to " + args[2]));
								}
							}
						}
					}
				}
			}
		} else {
			sender.sendMessage(new TextComponent("&cYou do not have permission to do this!"));
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
}
