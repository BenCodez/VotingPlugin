package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;

public class BungeeVote {

	static ServerSocket sock;

	private BungeeVote() {
	}

	static BungeeVote instance = new BungeeVote();

	public static BungeeVote getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public BungeeVote(Main plugin) {
		BungeeVote.plugin = plugin;
	}

	public void registerBungeeVoting() {
		if (Config.getInstance().recieveBungeeVotes()) {
			try {
				sock = new ServerSocket(Config.getInstance().bungeePort());
				ReadThread read = new ReadThread();
				read.start();
				plugin.getLogger().info(
						"Bungee voting registered! Server will recieve votes on port '"
								+ Config.getInstance().bungeePort() + "'!");
			} catch (IOException e) {
				plugin.getLogger().warning("Bungee voting failed to load!");
				e.printStackTrace();
			}
		}
	}

	public void sendBungeeVote(String playerName, String voteSite) {
		if (Config.getInstance().sendBungeeVotes()) {
			Socket client;
			try {
				String data = "vote/" + playerName + "/" + voteSite;

				client = new Socket("localhost", Config.getInstance()
						.bungeePort());

				DataOutputStream ds = new DataOutputStream(
						client.getOutputStream());
				ds.writeUTF(data);
				ds.close();
				client.close();
			} catch (UnknownHostException e1) {
				e1.printStackTrace();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
	}

	public void recievedBungeeVote(String playerName, String voteSite) {
		if (Config.getInstance().recieveBungeeVotes()) {
			plugin.getLogger().info("Bungee Vote Recieved!");
			VotiferEvent.playerVote(playerName, voteSite);
		}
	}

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
}
