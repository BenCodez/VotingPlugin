package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;

import net.md_5.bungee.api.ProxyServer;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.event.EventHandler;

public class PluginMessage implements Listener {

	BungeeMain plugin;

	public PluginMessage(BungeeMain bungeeMain) {
		plugin = bungeeMain;
	}

	@EventHandler
	public void onQueryReceive(PluginMessageEvent event) {
		if (event.getTag().equalsIgnoreCase("VotingPlugin")) {
			ProxyServer.getInstance().getLogger().info("Got a vote");
			DataInputStream di = new DataInputStream(new ByteArrayInputStream(
					event.getData()));
			try {
				String input = di.readUTF();
				if (input.split("/").length > 2) {
					plugin.sendVoteToServers(input.split("/")[0],
							input.split("/")[1] + input.split("/")[2]);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
	}

}
