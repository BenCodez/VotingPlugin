package com.Ben12345rocks.VotingPlugin;

import java.util.UUID;

import org.bukkit.Bukkit;

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

	public void close() {
		socketHandler.closeConnection();
		clientHandler.stopConnection();
	}

	public void load() {
		Bukkit.getScheduler().runTask(Main.plugin, new Runnable() {

			@Override
			public void run() {
				clientHandler = new ClientHandler(Config.getInstance().getBungeeServerHost(),
						Config.getInstance().getBungeeServerPort());

				socketHandler.add(new SocketReceiver() {

					@Override
					public void onReceive(String[] data) {
						if (data.length > 2) {
							if (data[0].equalsIgnoreCase("bungeevote")) {
								String uuid = data[1];
								User user = null;
								if (!uuid.isEmpty()) {
									Main.plugin.getMysql().clearCache(uuid);
									user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
								} else {
									user = UserManager.getInstance().getVotingPluginUser(data[2]);
									user.clearCache();
								}

								user.bungeeVote();

							}
						}
					}
				});

				socketHandler.add(new SocketReceiver() {

					@Override
					public void onReceive(String[] data) {
						if (data.length > 2) {
							if (data[0].equalsIgnoreCase("Broadcast")) {

								VoteSite site = Main.plugin.getVoteSite(data[1]);
								String p = data[2];
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

			}
		});

		socketHandler = new SocketHandler(Main.plugin.getVersion(), Config.getInstance().getSpigotServerHost(),
				Config.getInstance().getSpigotServerPort());

	}

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
