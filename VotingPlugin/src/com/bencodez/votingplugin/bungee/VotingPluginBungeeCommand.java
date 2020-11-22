package com.bencodez.votingplugin.bungee;

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
					bungee.reload();
					sender.sendMessage(new TextComponent("Reloading VotingPluginBungee"));
				}
				if (args[0].equalsIgnoreCase("vote")) {
					if (args.length >= 2) {
						String user = args[1];
						String site = args[2];
						bungee.vote(user, site, false);
						sender.sendMessage(new TextComponent("Sending vote"));
					}
				}
				if (args[0].equalsIgnoreCase("status")) {
					if (bungee.getMethod().equals(BungeeMethod.SOCKETS)) {
						sender.sendMessage(new TextComponent(
								"Checking status, waiting for response, check console, only for socket method"));
						bungee.status(sender);
					} else {
						sender.sendMessage(new TextComponent("Not using socket method, command unavailable"));
					}
				}
				if (args[0].equalsIgnoreCase("help")) {
					TextComponent[] msg = new TextComponent[3];
					msg[0] = new TextComponent("&avotingpluginbungee reload - Reload plugin");
					msg[1] = new TextComponent("&avotingpluginbungee vote (player) (servicesite) - Send bungee vote");
					msg[2] = new TextComponent("&avotingplugin status - Check socket connection status");
					sender.sendMessage(msg);
				}
			}
		} else {
			sender.sendMessage(new TextComponent("You do not have permission to do this!"));
		}
	}
}
