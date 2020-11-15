package com.bencodez.votingplugin;

import java.io.File;
import java.util.ArrayList;
import java.util.UUID;

import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.bungeeapi.pluginmessage.PluginMessage;
import com.bencodez.advancedcore.bungeeapi.pluginmessage.PluginMessageHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

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
		VotingPluginMain.plugin.debug("Loading bungee handler");

		method = BungeeMethod.getByName(BungeeSettings.getInstance().getBungeeMethod());

		VotingPluginMain.plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			VotingPluginMain.plugin.registerBungeeChannels();

			PluginMessage.getInstance().add(new PluginMessageHandler("Vote") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String player = args.get(0);
					String uuid = args.get(1);
					String service = args.get(2);
					long time = Long.parseLong(args.get(3));
					VotingPluginMain.plugin.debug("pluginmessaging vote received from " + player + " on " + service);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));

					boolean wasOnline = Boolean.valueOf(args.get(4));

					String text = args.get(6);

					user.clearCache();

					user.bungeeVotePluginMessaging(service, time, text);

					if (!BungeeSettings.getInstance().isBungeeBroadcast()) {
						if (wasOnline || BungeeSettings.getInstance().isBungeeBroadcastAlways()) {
							VoteSite site = VotingPluginMain.plugin.getVoteSite(service);
							if (site != null) {
								site.broadcastVote(user, false);
							} else {
								VotingPluginMain.plugin.getLogger().warning("No votesite for " + service);
							}
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
					String text = args.get(6);
					VotingPluginMain.plugin.debug("pluginmessaging voteonline received from " + player + " on " + service);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					user.clearCache();

					user.bungeeVotePluginMessaging(service, time, text);

					if (!BungeeSettings.getInstance().isBungeeBroadcast()) {
						if (Boolean.valueOf(args.get(4)) || BungeeSettings.getInstance().isBungeeBroadcastAlways()) {
							VoteSite site = VotingPluginMain.plugin.getVoteSite(service);
							if (site != null) {
								site.broadcastVote(user, false);
							} else {
								VotingPluginMain.plugin.getLogger().warning("No votesite for " + service);
							}
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
					VotingPluginMain.plugin.debug("pluginmessaging voteupdate received for " + player);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(player));
					user.clearCache();

					user.offVote();

					VotingPluginMain.plugin.setUpdate(true);
				}
			});

			PluginMessage.getInstance().add(new PluginMessageHandler("VoteBroadcast") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String uuid = args.get(0);
					String service = args.get(1);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					VoteSite site = VotingPluginMain.plugin.getVoteSite(service);
					if (site != null) {
						site.broadcastVote(user, false);
					} else {
						VotingPluginMain.plugin.getLogger().warning("No votesite for " + service);
					}
				}
			});

		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(VotingPluginMain.plugin.getDataFolder(), "secretkey.key"));

			clientHandler = new ClientHandler(BungeeSettings.getInstance().getBungeeServerHost(),
					BungeeSettings.getInstance().getBungeeServerPort(), encryptionHandler,
					BungeeSettings.getInstance().isBungeeDebug());

			socketHandler = new SocketHandler(VotingPluginMain.plugin.getVersion(),
					BungeeSettings.getInstance().getSpigotServerHost(),
					BungeeSettings.getInstance().getSpigotServerPort(), encryptionHandler,
					BungeeSettings.getInstance().isBungeeDebug());

			socketHandler.add(new SocketReceiver("bungeevote") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 3) {
						VotingPluginMain.plugin.extraDebug("BungeeVote from " + data[2] + ", processing");
						String uuid = data[1];
						VotingPluginUser user = null;
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
						VotingPluginMain.plugin.extraDebug("BungeeVoteOnline from " + data[2] + ", processing");
						String uuid = data[1];
						VotingPluginUser user = null;
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
						VoteSite site = VotingPluginMain.plugin.getVoteSite(data[1]);
						String p = data[3];
						VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(p);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							VotingPluginMain.plugin.getLogger().warning("No votesite for " + data[1]);
						}
					}
				}
			});

			socketHandler.add(new SocketReceiver("Status") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 0) {
						VotingPluginMain.plugin.getLogger().info("Received status command, sending status back");
						sendData("StatusOkay", VotingPluginMain.plugin.getOptions().getServer());

					}
				}
			});

			socketHandler.add(new SocketReceiver("BungeeUpdate") {

				@Override
				public void onReceive(String[] data) {
					VotingPluginMain.plugin.setUpdate(true);
				}
			});

			if (VotingPluginMain.plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
				VotingPluginMain.plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
			}

		}

	}

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
