package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import net.md_5.bungee.api.ProxyServer;
import net.md_5.bungee.api.config.ServerInfo;
import net.md_5.bungee.api.plugin.Plugin;

public class BungeeMain extends Plugin {

	public static BungeeMain plugin;

	public void onEnable() {
		plugin = this;
		ProxyServer.getInstance().registerChannel("VotingPlugin");
		ProxyServer.getInstance().getPluginManager()
				.registerListener(plugin, new PluginMessage(this));
	}

	public void sendVoteToServers(String playerName, String siteName) {
		ProxyServer.getInstance().getLogger().info("Sending votes");
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
			ProxyServer.getInstance().getLogger()
					.info("Sending vote to " + serverName);
		}
	}

	
}
