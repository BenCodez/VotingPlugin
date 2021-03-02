package com.bencodez.votingplugin;

import java.io.File;
import java.util.ArrayList;
import java.util.UUID;

import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.bungeeapi.pluginmessage.PluginMessageHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.votingplugin.bungee.BungeeMessageData;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.bungee.BungeeVersion;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

public class BungeeHandler {
	@Getter
	private ClientHandler clientHandler;

	private EncryptionHandler encryptionHandler;

	@Getter
	private BungeeMethod method;

	private VotingPluginMain plugin;

	@Getter
	private SocketHandler socketHandler;

	public BungeeHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void close() {
		socketHandler.closeConnection();
		clientHandler.stopConnection();
	}

	public void load() {
		plugin.debug("Loading bungee handler");

		method = BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod());

		plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		if (method.equals(BungeeMethod.MYSQL)) {
			plugin.registerBungeeChannels("vp:vp");
		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			plugin.registerBungeeChannels("vp:vp");

			plugin.getPluginMessaging().add(new PluginMessageHandler("Vote") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					if (args.size() > 8) {
						int bungeeVersion = Integer.parseInt(args.get(8));
						if (bungeeVersion != BungeeVersion.getPluginMessageVersion()) {
							plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
							return;
						}

						String player = args.get(0);
						String uuid = args.get(1);
						String service = args.get(2);
						long time = Long.parseLong(args.get(3));
						plugin.debug("pluginmessaging vote received from " + player + " on " + service);
						VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid),
								player);

						boolean wasOnline = Boolean.valueOf(args.get(4));

						BungeeMessageData text = new BungeeMessageData(args.get(6));

						boolean setTotals = Boolean.valueOf(args.get(7));

						user.clearCache();

						user.bungeeVotePluginMessaging(service, time, text, !setTotals);

						if (!plugin.getBungeeSettings().isBungeeBroadcast()) {
							if (wasOnline || plugin.getBungeeSettings().isBungeeBroadcastAlways()) {
								VoteSite site = plugin.getVoteSite(service);
								if (site != null) {
									site.broadcastVote(user, false);
								} else {
									plugin.getLogger().warning("No votesite for " + service);
								}
							}
						}

						if (Boolean.valueOf(args.get(5))) {
							plugin.getServerData().addServiceSite(service);
						}
					} else {
						plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
					}

				}
			});

			plugin.getPluginMessaging().add(new PluginMessageHandler("VoteOnline") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					if (args.size() > 8) {
						int bungeeVersion = Integer.parseInt(args.get(8));
						if (bungeeVersion != BungeeVersion.getPluginMessageVersion()) {
							plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
							return;
						}
						String player = args.get(0);
						String uuid = args.get(1);
						String service = args.get(2);
						long time = Long.parseLong(args.get(3));
						BungeeMessageData text = new BungeeMessageData(args.get(6));
						plugin.debug("pluginmessaging voteonline received from " + player + " on " + service);
						VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid),
								player);
						user.clearCache();

						boolean setTotals = Boolean.valueOf(args.get(7));

						user.bungeeVotePluginMessaging(service, time, text, !setTotals);

						if (!plugin.getBungeeSettings().isBungeeBroadcast()) {
							if (Boolean.valueOf(args.get(4)) || plugin.getBungeeSettings().isBungeeBroadcastAlways()) {
								VoteSite site = plugin.getVoteSite(service);
								if (site != null) {
									site.broadcastVote(user, false);
								} else {
									plugin.getLogger().warning("No votesite for " + service);
								}
							}
						}

						if (Boolean.valueOf(args.get(5))) {
							plugin.getServerData().addServiceSite(service);
						}
					} else {
						plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
					}
				}
			});

			plugin.getPluginMessaging().add(new PluginMessageHandler("VoteUpdate") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String player = args.get(0);
					plugin.debug("pluginmessaging voteupdate received for " + player);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(player));
					user.clearCache();

					user.offVote();

					plugin.setUpdate(true);
				}
			});

			plugin.getPluginMessaging().add(new PluginMessageHandler("VoteBroadcast") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String uuid = args.get(0);
					String service = args.get(1);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
					VoteSite site = plugin.getVoteSite(service);
					if (site != null) {
						site.broadcastVote(user, false);
					} else {
						plugin.getLogger().warning("No votesite for " + service);
					}
				}
			});

			plugin.getPluginMessaging().add(new PluginMessageHandler("Status") {
				@Override
				public void onRecieve(String subChannel, ArrayList<String> args) {
					String server = args.get(0);
					plugin.getPluginMessaging().sendPluginMessage("statusokay", server);

				}
			});

		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(plugin.getDataFolder(), "secretkey.key"));

			clientHandler = new ClientHandler(plugin.getBungeeSettings().getBungeeServerHost(),
					plugin.getBungeeSettings().getBungeeServerPort(), encryptionHandler,
					plugin.getBungeeSettings().isBungeeDebug());

			socketHandler = new SocketHandler(plugin.getVersion(), plugin.getBungeeSettings().getSpigotServerHost(),
					plugin.getBungeeSettings().getSpigotServerPort(), encryptionHandler,
					plugin.getBungeeSettings().isBungeeDebug());

			socketHandler.add(new SocketReceiver("bungeevote") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 5) {
						plugin.extraDebug("BungeeVote from " + data[2] + ", processing");
						String uuid = data[1];
						VotingPluginUser user = null;
						if (!uuid.isEmpty()) {
							user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
						} else {
							user = UserManager.getInstance().getVotingPluginUser(data[2]);
						}

						user.clearCache();

						user.bungeeVote(data[3], new BungeeMessageData(data[4]), !Boolean.valueOf(data[5]));
					}
				}
			});

			socketHandler.add(new SocketReceiver("bungeevoteonline") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 5) {
						plugin.extraDebug("BungeeVoteOnline from " + data[2] + ", processing");
						String uuid = data[1];
						VotingPluginUser user = null;
						if (!uuid.isEmpty()) {
							user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
						} else {
							user = UserManager.getInstance().getVotingPluginUser(data[2]);
						}

						user.clearCache();

						user.bungeeVoteOnline(data[3], new BungeeMessageData(data[4]), !Boolean.valueOf(data[5]));
					}
				}
			});

			socketHandler.add(new SocketReceiver("BungeeBroadcast") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 2) {
						VoteSite site = plugin.getVoteSite(data[1]);
						String p = data[3];
						VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(p);
						if (site != null) {
							site.broadcastVote(user, false);
						} else {
							plugin.getLogger().warning("No votesite for " + data[1]);
						}
					}
				}
			});

			socketHandler.add(new SocketReceiver("Status") {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 0) {
						plugin.getLogger().info("Received status command, sending status back");
						sendData("StatusOkay", plugin.getOptions().getServer());

					}
				}
			});

			socketHandler.add(new SocketReceiver("BungeeUpdate") {

				@Override
				public void onReceive(String[] data) {
					plugin.setUpdate(true);
				}
			});

			if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
				plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
			}

		}

	}

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
