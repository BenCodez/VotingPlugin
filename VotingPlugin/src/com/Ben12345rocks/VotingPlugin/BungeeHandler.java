package com.Ben12345rocks.VotingPlugin;

import java.io.File;
import java.util.UUID;

import com.Ben12345rocks.AdvancedCore.Util.Encryption.EncryptionHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.ClientHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketReceiver;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import lombok.Getter;

public class BungeeHandler {
	private static BungeeHandler instance = new BungeeHandler();

	public static BungeeHandler getInstance() {
		return instance;
	}

	@Getter
	private SocketHandler socketHandler;

	@Getter
	private ClientHandler clientHandler;

	private EncryptionHandler encryptionHandler;

	public void close() {
		socketHandler.closeConnection();
		clientHandler.stopConnection();
	}

	public void load() {
		Main.plugin.debug("Loading bungee handles");

		encryptionHandler = new EncryptionHandler(new File(Main.plugin.getDataFolder(), "secretkey.key"));

		clientHandler = new ClientHandler(Config.getInstance().getBungeeServerHost(),
				Config.getInstance().getBungeeServerPort(), encryptionHandler, Config.getInstance().isBungeeDebug());

		socketHandler = new SocketHandler(Main.plugin.getVersion(), Config.getInstance().getSpigotServerHost(),
				Config.getInstance().getSpigotServerPort(), encryptionHandler, Config.getInstance().isBungeeDebug());

		socketHandler.add(new SocketReceiver() {

			@Override
			public void onReceive(String[] data) {
				if (data.length > 3) {
					if (data[0].equalsIgnoreCase("bungeevote")) {
						String uuid = data[1];
						User user = null;
						if (!uuid.isEmpty()) {
							user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
						} else {
							user = UserManager.getInstance().getVotingPluginUser(data[2]);
						}

						user.clearCache();

						user.bungeeVote(data[3]);

					}
				}
			}
		});

		socketHandler.add(new SocketReceiver() {

			@Override
			public void onReceive(String[] data) {
				if (data.length > 3) {
					if (data[0].equalsIgnoreCase("bungeevoteonline")) {
						String uuid = data[1];
						User user = null;
						if (!uuid.isEmpty()) {
							user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
						} else {
							user = UserManager.getInstance().getVotingPluginUser(data[2]);
						}

						user.clearCache();

						user.bungeeVoteOnline(data[3]);

					}
				}
			}
		});

		socketHandler.add(new SocketReceiver() {

			@Override
			public void onReceive(String[] data) {
				if (data.length > 2) {
					if (data[0].equalsIgnoreCase("BungeeBroadcast")) {

						VoteSite site = Main.plugin.getVoteSite(data[1]);
						String p = data[3];
						User user = UserManager.getInstance().getVotingPluginUser(p);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							Main.plugin.getLogger().warning("No votesite for " + data[1]);
						}
						Main.plugin.setUpdate(true);
					}
				}
			}
		});

		socketHandler.add(new SocketReceiver() {

			@Override
			public void onReceive(String[] data) {
				if (data.length > 0) {
					if (data[0].equalsIgnoreCase("Status")) {
						Main.plugin.getLogger().info("Received status command, sending status back");
						sendData("StatusOkay", Main.plugin.getOptions().getServer());
					}
				}
			}
		});

	}

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
