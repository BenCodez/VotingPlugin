package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.bukkit.craftbukkit.libs.joptsimple.util.InetAddressConverter;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBungeeVoting;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;

public class BungeeVote {

	class ReadThread extends Thread {
		@Override
		public void run() {
			Socket client;
			while (true) {
				try {
					// init the client
					client = sock.accept();
					// Read the data
					DataInputStream dis = new DataInputStream(
							client.getInputStream());
					String data = dis.readUTF();

					String[] lines = data.split("/");
					String cmd = lines[0];
					if (cmd.equalsIgnoreCase("vote")) {
						recievedBungeeVote(lines[1], lines[2]);
					}

				} catch (Exception e) {
					plugin.getLogger().warning(
							"Exception caught while recieving vote!");
					e.printStackTrace();
				}
			}
		}
	}

	static BungeeVote instance = new BungeeVote();

	static Main plugin = Main.plugin;

	static ServerSocket sock;

	public static BungeeVote getInstance() {
		return instance;
	}

	private BungeeVote() {
	}

	public BungeeVote(Main plugin) {
		BungeeVote.plugin = plugin;
	}

	public void recievedBungeeVote(String playerName, String voteSite) {
		if (ConfigBungeeVoting.getInstance().recieveBungeeVotes()) {
			plugin.getLogger().info("Bungee Vote Recieved!");
			VotiferEvent.playerVote(playerName, voteSite);
		}
	}

	public void registerBungeeVoting() {

		if (ConfigBungeeVoting.getInstance().recieveBungeeVotes()) {
			try {
				if (!ConfigBungeeVoting.getInstance().useAdvanced()) {
					sock = new ServerSocket(ConfigBungeeVoting.getInstance()
							.getRecievePort());
					ReadThread read = new ReadThread();
					read.start();
					plugin.getLogger().info(
							"Bungee voting registered! Server will recieve votes on port '"
									+ ConfigBungeeVoting.getInstance()
											.getRecievePort() + "'!");
				} else {
					sock = new ServerSocket(ConfigBungeeVoting.getInstance()
							.getAdvancedRecievePort(), 0,
							new InetAddressConverter()
									.convert(ConfigBungeeVoting.getInstance()
											.getAdvancedRecieveIP()));

					ReadThread read = new ReadThread();
					read.start();
					plugin.getLogger().info(
							"Bungee voting registered! Server will recieve votes on port '"
									+ ConfigBungeeVoting.getInstance()
											.getAdvancedRecievePort() + "'!");
				}
			} catch (IOException e) {
				plugin.getLogger().warning("Bungee voting failed to load!");
				e.printStackTrace();
			}
		}
	}

	public void sendBungeeVote(String playerName, String voteSite) {
		if (ConfigBungeeVoting.getInstance().sendBungeeVotes()) {

			Socket client;
			try {
				if (!ConfigBungeeVoting.getInstance().useAdvanced()) {
					for (int port : ConfigBungeeVoting.getInstance()
							.getSendPorts()) {
						String data = "vote/" + playerName + "/" + voteSite;

						client = new Socket("localhost", port);

						DataOutputStream ds = new DataOutputStream(
								client.getOutputStream());
						ds.writeUTF(data);
						ds.close();
						client.close();
						if (Config.getInstance().getDebugEnabled()) {
							plugin.getLogger().info(
									"Sent bungee vote on port: " + port);
						}
					}
				} else {
					for (String server : ConfigBungeeVoting.getInstance()
							.getAdvancedSendServers()) {
						String ip = ConfigBungeeVoting.getInstance()
								.getAdvancedSendIP(server);
						int port = ConfigBungeeVoting.getInstance()
								.getAdvancedSendPort(server);
						String data = "vote/" + playerName + "/" + voteSite;

						client = new Socket(ip, port);

						DataOutputStream ds = new DataOutputStream(
								client.getOutputStream());
						ds.writeUTF(data);
						ds.close();
						client.close();
						if (Config.getInstance().getDebugEnabled()) {
							plugin.getLogger().info(
									"Sent bungee vote to " + server);
						}
					}
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}
}
