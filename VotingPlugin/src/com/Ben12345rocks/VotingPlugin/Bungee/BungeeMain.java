package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import net.md_5.bungee.api.ProxyServer;
import net.md_5.bungee.api.config.ServerInfo;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.event.EventHandler;

public class BungeeMain extends Plugin implements Listener {

	public static BungeeMain plugin;

	public void onEnable() {
		plugin = this;
		ProxyServer.getInstance().registerChannel("VotingPlugin");
		ProxyServer.getInstance().getPluginManager()
				.registerListener(plugin, this);
	}

	public void sendVoteToServers(String playerName, String siteName) {
		for (String serverName : ProxyServer.getInstance().getServers()
				.keySet()) {
			ServerInfo server = ProxyServer.getInstance().getServerInfo(
					serverName);
			ByteArrayOutputStream stream = new ByteArrayOutputStream();
			DataOutputStream out = new DataOutputStream(stream);
			try {
				out.writeUTF(playerName + "/" + siteName);
			} catch (IOException e) {
				ProxyServer.getInstance().getLogger()
						.severe("An I/O error occurred!");
			}
			server.sendData("VotingPlugin", stream.toByteArray());
		}
	}

	@EventHandler
	public void onQueryReceive(PluginMessageEvent event) {
		if (event.getTag().equalsIgnoreCase("VotingPlugin")) {
			DataInputStream di = new DataInputStream(new ByteArrayInputStream(
					event.getData()));
			try {
				String input = di.readUTF();
				if (input.split("/").length > 1) {
					sendVoteToServers(input.split("/")[0], input.split("/")[1]);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
	}
}
