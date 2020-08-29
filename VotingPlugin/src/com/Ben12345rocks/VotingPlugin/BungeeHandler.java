package com.Ben12345rocks.VotingPlugin;

import java.io.File;
import java.util.ArrayList;
import java.util.UUID;

import com.Ben12345rocks.AdvancedCore.Util.Encryption.EncryptionHandler;
import com.Ben12345rocks.AdvancedCore.Util.PluginMessage.PluginMessage;
import com.Ben12345rocks.AdvancedCore.Util.PluginMessage.PluginMessageHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.ClientHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketReceiver;
import com.Ben12345rocks.VotingPlugin.Config.BungeeSettings;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.bungee.BungeeMethod;

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

	@Getter
	private BungeeMethod method;

	public void close() {
		socketHandler.closeConnection();
		clientHandler.stopConnection();
	}

	public void load() {
		Main.plugin.debug("Loading bungee handler");

		method = BungeeMethod.getByName(BungeeSettings.getInstance().getBungeeMethod());

		Main.plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			Main.plugin.registerBungeeChannels();

			PluginMessage.getInstance().add(new PluginMessageHandler("Vote") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String player = args.get(0);
					String uuid = args.get(1);
					String service = args.get(2);
					long time = Long.parseLong(args.get(3));
					Main.plugin.debug("pluginmessaging vote received from " + player + " on " + service);
					User user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					user.clearCache();

					user.bungeeVotePluginMessaging(service, time);

					if (Boolean.valueOf(args.get(4)) || BungeeSettings.getInstance().isBungeeBroadcastAlways()) {
						VoteSite site = Main.plugin.getVoteSite(service);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							Main.plugin.getLogger().warning("No votesite for " + service);
						}
					}

					if (args.size() > 4 && Boolean.valueOf(args.get(5))) {
						ServerData.getInstance().addServiceSite(service);
					}

				}
			});

			PluginMessage.getInstance().add(new PluginMessageHandler("VoteOnline") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String player = args.get(0);
					String uuid = args.get(1);
					String service = args.get(2);
					long time = Long.parseLong(args.get(3));
					Main.plugin.debug("pluginmessaging voteonline received from " + player + " on " + service);
					User user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					user.clearCache();

					user.bungeeVotePluginMessaging(service, time);

					if (Boolean.valueOf(args.get(4)) || BungeeSettings.getInstance().isBungeeBroadcastAlways()) {
						VoteSite site = Main.plugin.getVoteSite(service);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							Main.plugin.getLogger().warning("No votesite for " + service);
						}
					}

					if (args.size() > 4 && Boolean.valueOf(args.get(5))) {
						ServerData.getInstance().addServiceSite(service);
					}
				}
			});

			PluginMessage.getInstance().add(new PluginMessageHandler("VoteUpdate") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String player = args.get(0);
					Main.plugin.debug("pluginmessaging voteupdate received for " + player);
					User user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(player));
					user.clearCache();

					user.offVote();

					Main.plugin.setUpdate(true);
				}
			});

			PluginMessage.getInstance().add(new PluginMessageHandler("VoteBroadcast") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String uuid = args.get(0);
					String service = args.get(1);
					User user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					VoteSite site = Main.plugin.getVoteSite(service);
					if (site != null) {
						site.broadcastVote(user, false);
					} else {
						Main.plugin.getLogger().warning("No votesite for " + service);
					}
				}
			});

		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(Main.plugin.getDataFolder(), "secretkey.key"));

			clientHandler = new ClientHandler(BungeeSettings.getInstance().getBungeeServerHost(),
					BungeeSettings.getInstance().getBungeeServerPort(), encryptionHandler,
					BungeeSettings.getInstance().isBungeeDebug());

			socketHandler = new SocketHandler(Main.plugin.getVersion(),
					BungeeSettings.getInstance().getSpigotServerHost(),
					BungeeSettings.getInstance().getSpigotServerPort(), encryptionHandler,
					BungeeSettings.getInstance().isBungeeDebug());

			socketHandler.add(new SocketReceiver("bungeevote") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 3) {
						Main.plugin.extraDebug("BungeeVote from " + data[2] + ", processing");
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
			});

			socketHandler.add(new SocketReceiver("bungeevoteonline") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 3) {
						Main.plugin.extraDebug("BungeeVoteOnline from " + data[2] + ", processing");
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
			});

			socketHandler.add(new SocketReceiver("BungeeBroadcast") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 2) {
						VoteSite site = Main.plugin.getVoteSite(data[1]);
						String p = data[3];
						User user = UserManager.getInstance().getVotingPluginUser(p);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							Main.plugin.getLogger().warning("No votesite for " + data[1]);
						}
					}
				}
			});

			socketHandler.add(new SocketReceiver("Status") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 0) {
						Main.plugin.getLogger().info("Received status command, sending status back");
						sendData("StatusOkay", Main.plugin.getOptions().getServer());

					}
				}
			});

			socketHandler.add(new SocketReceiver("BungeeUpdate") {

				@Override
				public void onReceive(String[] data) {
					Main.plugin.setUpdate(true);
				}
			});

			if (Main.plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
				Main.plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
			}

		}

	}

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
