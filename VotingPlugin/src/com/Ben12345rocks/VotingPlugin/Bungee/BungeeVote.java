package com.Ben12345rocks.VotingPlugin.Bungee;

import java.net.ServerSocket;
import java.util.Date;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.plugin.messaging.PluginMessageListener;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBungeeVoting;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.google.common.io.ByteArrayDataInput;
import com.google.common.io.ByteArrayDataOutput;
import com.google.common.io.ByteStreams;

public class BungeeVote implements PluginMessageListener {

	/*class ReadThread extends Thread {
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
	}*/

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

	public void recievedBungeeVote(String playerName, String voteSite, String time) {
		if (ConfigBungeeVoting.getInstance().recieveBungeeVotes()) {
			plugin.getLogger().info("Bungee Vote Recieved!");
			
			Long mill = Long.getLong(time);
			
			VotiferEvent.playerVote(playerName, voteSite, mill);
		}
	}

	public void registerBungeeVoting() {

		/*
		 * if (ConfigBungeeVoting.getInstance().recieveBungeeVotes()) { try { if
		 * (!ConfigBungeeVoting.getInstance().useAdvanced()) { sock = new
		 * ServerSocket(ConfigBungeeVoting.getInstance() .getRecievePort());
		 * ReadThread read = new ReadThread(); read.start();
		 * plugin.getLogger().info(
		 * "Bungee voting registered! Server will recieve votes on port '" +
		 * ConfigBungeeVoting.getInstance() .getRecievePort() + "'!"); } else {
		 * sock = new ServerSocket(ConfigBungeeVoting.getInstance()
		 * .getAdvancedRecievePort(), 0, new InetAddressConverter()
		 * .convert(ConfigBungeeVoting.getInstance() .getAdvancedRecieveIP()));
		 * 
		 * ReadThread read = new ReadThread(); read.start();
		 * plugin.getLogger().info(
		 * "Bungee voting registered! Server will recieve votes on port '" +
		 * ConfigBungeeVoting.getInstance() .getAdvancedRecievePort() + "'!"); }
		 * } catch (IOException e) {
		 * plugin.getLogger().warning("Bungee voting failed to load!");
		 * e.printStackTrace(); } }
		 */

		if (ConfigBungeeVoting.getInstance().recieveBungeeVotes()) {
			Bukkit.getServer()
					.getMessenger()
					.registerIncomingPluginChannel(plugin, "VotingPlugin", this);
		}

		if (ConfigBungeeVoting.getInstance().sendBungeeVotes()) {
			Bukkit.getServer().getMessenger()
					.registerOutgoingPluginChannel(plugin, "VotingPlugin");
		}

	}

	public void sendBungeeVote(String playerName, String voteSite) {
		if (ConfigBungeeVoting.getInstance().sendBungeeVotes()) {

			/*
			 * Socket client; try { if
			 * (!ConfigBungeeVoting.getInstance().useAdvanced()) { for (int port
			 * : ConfigBungeeVoting.getInstance() .getSendPorts()) { String data
			 * = "vote/" + playerName + "/" + voteSite;
			 * 
			 * client = new Socket("localhost", port);
			 * 
			 * DataOutputStream ds = new DataOutputStream(
			 * client.getOutputStream()); ds.writeUTF(data); ds.close();
			 * client.close(); if (Config.getInstance().getDebugEnabled()) {
			 * plugin.getLogger().info( "Sent bungee vote on port: " + port); }
			 * } } else { for (String server : ConfigBungeeVoting.getInstance()
			 * .getAdvancedSendServers()) { String ip =
			 * ConfigBungeeVoting.getInstance() .getAdvancedSendIP(server); int
			 * port = ConfigBungeeVoting.getInstance()
			 * .getAdvancedSendPort(server); String data = "vote/" + playerName
			 * + "/" + voteSite;
			 * 
			 * client = new Socket(ip, port);
			 * 
			 * DataOutputStream ds = new DataOutputStream(
			 * client.getOutputStream()); ds.writeUTF(data); ds.close();
			 * client.close(); if (Config.getInstance().getDebugEnabled()) {
			 * plugin.getLogger().info( "Sent bungee vote to " + server); } } }
			 * } catch (Exception ex) { ex.printStackTrace(); }
			 */
			ByteArrayDataOutput out = ByteStreams.newDataOutput();
			out.writeUTF(playerName + "/" + voteSite + "/"
					+ new Date().getTime());
			Bukkit.getServer().sendPluginMessage(plugin, "VotingPlugin",
					out.toByteArray());
			plugin.getLogger().info("Sent bungee vote");
		}
	}

	@Override
	public void onPluginMessageReceived(String channel, Player p, byte[] msg) {
		if (!channel.equals("VotingPlugin")) {
			return;
		}
		ByteArrayDataInput in = ByteStreams.newDataInput(msg);
		String input = in.readUTF();
		if (input.split("/").length > 2) {
			recievedBungeeVote(input.split("/")[0], input.split("/")[1], input.split("/")[2]);
		}

	}
}
