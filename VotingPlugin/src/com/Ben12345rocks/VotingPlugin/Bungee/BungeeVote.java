package com.Ben12345rocks.VotingPlugin.Bungee;

import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;

import javax.xml.bind.DatatypeConverter;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBungeeVoting;
import com.vexsoftware.votifier.crypto.RSA;
import com.vexsoftware.votifier.model.Vote;

public class BungeeVote {

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

	public void sendVote(Vote vote) throws NoSuchAlgorithmException,
			InvalidKeySpecException {
		if (ConfigBungeeVoting.getInstance().getEnabled()) {
			for (String server : ConfigBungeeVoting.getInstance().getServers()) {
				byte[] encodedPublicKey = DatatypeConverter
						.parseBase64Binary(ConfigBungeeVoting.getInstance()
								.getServerKey(server));
				KeyFactory keyFactory = KeyFactory.getInstance("RSA");
				X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(
						encodedPublicKey);
				PublicKey publicKey = keyFactory.generatePublic(publicKeySpec);
				String serverIP = ConfigBungeeVoting.getInstance().getServerIP(
						server);
				int serverPort = ConfigBungeeVoting.getInstance()
						.getServerPort(serverIP);
				String serviceSite = ConfigBungeeVoting.getInstance()
						.getServerServiceSite(serverIP);
				if (serverIP.length() != 0) {
					try {
						if (serviceSite.length() > 0) {
							vote.setServiceName(serviceSite);
						}
						String VoteString = "VOTE\n" + vote.getServiceName()
								+ "\n" + vote.getUsername() + "\n"
								+ vote.getAddress() + "\n"
								+ vote.getTimeStamp() + "\n";
						SocketAddress sockAddr = new InetSocketAddress(
								serverIP, serverPort);
						Socket socket = new Socket();
						socket.connect(sockAddr, 1000);
						OutputStream socketOutputStream = socket
								.getOutputStream();
						socketOutputStream.write(RSA.encrypt(
								VoteString.getBytes(), publicKey));
						socketOutputStream.close();
						socket.close();
						if (Config.getInstance().getDebugEnabled()) {
							plugin.getLogger().info(
									"Sending vote to " + server + ": "
											+ vote.toString());
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		}
	}
}