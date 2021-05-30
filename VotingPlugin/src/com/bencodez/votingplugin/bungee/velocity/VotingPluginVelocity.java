package com.bencodez.votingplugin.bungee.velocity;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.spongepowered.configurate.ConfigurationNode;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.api.user.userstorage.sql.Column;
import com.bencodez.advancedcore.api.user.userstorage.sql.DataType;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.votingplugin.bungee.BungeeMessageData;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.bungee.BungeeVersion;
import com.bencodez.votingplugin.bungee.OfflineBungeeVote;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.velocitypowered.api.event.Subscribe;
import com.velocitypowered.api.event.connection.PluginMessageEvent;
import com.velocitypowered.api.event.player.ServerConnectedEvent;
import com.velocitypowered.api.event.proxy.ProxyInitializeEvent;
import com.velocitypowered.api.event.proxy.ProxyShutdownEvent;
import com.velocitypowered.api.plugin.Dependency;
import com.velocitypowered.api.plugin.Plugin;
import com.velocitypowered.api.plugin.annotation.DataDirectory;
import com.velocitypowered.api.proxy.Player;
import com.velocitypowered.api.proxy.ProxyServer;
import com.velocitypowered.api.proxy.server.RegisteredServer;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.velocity.event.VotifierEvent;

import lombok.Getter;

@Plugin(id = "votingplugin", name = "VotingPlugin", version = "1.0", url = "https://www.spigotmc.org/resources/votingplugin.15358/", description = "VotingPlugin Velocity Version", authors = {
		"BenCodez" }, dependencies = { @Dependency(id = "nuvotifier", optional = true) })
public class VotingPluginVelocity {

	private final ProxyServer server;
	private final Logger logger;
	private final Path dataDirectory;

	@Getter
	private Config config;
	private VotingPluginChannelIdentifier ident = new VotingPluginChannelIdentifier();
	@Getter
	private BungeeMethod method;
	@Getter
	private BungeeMySQL mysql;

	private HashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new HashMap<String, ArrayList<OfflineBungeeVote>>();

	private HashMap<RegisteredServer, ArrayList<OfflineBungeeVote>> cachedVotes = new HashMap<RegisteredServer, ArrayList<OfflineBungeeVote>>();

	private HashMap<String, ClientHandler> clientHandles;

	private EncryptionHandler encryptionHandler;

	private NonVotedPlayersCache nonVotedPlayersCache;

	private SocketHandler socketHandler;

	private VoteCache voteCacheFile;

	public void checkCachedVotes(RegisteredServer serverToCheck) {
		if (!serverToCheck.getPlayersConnected().isEmpty()) {
			if (cachedVotes.containsKey(serverToCheck)
					&& !config.getBlockedServers().contains(serverToCheck.getServerInfo().getName())) {
				ArrayList<OfflineBungeeVote> c = cachedVotes.get(serverToCheck);
				ArrayList<OfflineBungeeVote> newSet = new ArrayList<OfflineBungeeVote>();
				if (!c.isEmpty()) {
					for (OfflineBungeeVote cache : c) {
						boolean toSend = true;
						if (getConfig().getWaitForUserOnline()) {
							Player p = server.getPlayer(UUID.fromString(cache.getUuid())).get();
							if (p == null || !p.isActive()) {
								toSend = false;
							} else if (p != null && p.isActive() && !p.getCurrentServer().get().getServerInfo()
									.getName().equals(serverToCheck.getServerInfo().getName())) {
								toSend = false;
							}

						}
						if (toSend) {
							sendPluginMessageServer(serverToCheck, "Vote", cache.getPlayerName(), cache.getUuid(),
									cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
									"" + cache.isRealVote(), cache.getText(), "" + getConfig().getBungeeManageTotals(),
									"" + BungeeVersion.getPluginMessageVersion());
						} else {
							debug("Not sending vote because user isn't on server " + serverToCheck + ": "
									+ cache.toString());
							newSet.add(cache);
						}
					}
					cachedVotes.put(serverToCheck, newSet);
				}
			}
		}

	}

	public void checkOnlineVotes(Player player, String uuid, RegisteredServer serverToCheck) {
		if (player != null && player.isActive() && cachedOnlineVotes.containsKey(uuid)) {
			ArrayList<OfflineBungeeVote> c = cachedOnlineVotes.get(uuid);
			if (!c.isEmpty()) {
				if (serverToCheck == null) {
					serverToCheck = player.getCurrentServer().get().getServer();
				}
				if (!config.getBlockedServers().contains(serverToCheck.getServerInfo().getName())) {
					for (OfflineBungeeVote cache : c) {
						sendPluginMessageServer(serverToCheck, "VoteOnline", cache.getPlayerName(), cache.getUuid(),
								cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
								"" + cache.isRealVote(), cache.getText(), "" + getConfig().getBungeeManageTotals(),
								"" + BungeeVersion.getPluginMessageVersion());
					}
					cachedOnlineVotes.put(uuid, new ArrayList<OfflineBungeeVote>());
				}
			}
		}
	}

	@Inject
	public VotingPluginVelocity(ProxyServer server, Logger logger, @DataDirectory Path dataDirectory) {
		this.server = server;
		this.logger = logger;
		this.dataDirectory = dataDirectory;
	}

	@Subscribe
	public void onProxyInitialization(ProxyInitializeEvent event) {
		File configFile = new File(dataDirectory.toFile(), "VotingPlugin" + File.separator + "bungeeconfig.yml");
		config = new Config(configFile);
		server.getChannelRegistrar().register(ident);
		method = BungeeMethod.getByName(config.getBungeeMethod());
		mysql = new BungeeMySQL("VotingPlugin_Users", config);

		if (method.equals(BungeeMethod.MYSQL)) {
			// this.getProxy().registerChannel("vp:vp");

		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			voteCacheFile = new VoteCache(
					new File(dataDirectory.toFile(), "VotingPlugin" + File.separator + "votecache.yml"));
			nonVotedPlayersCache = new NonVotedPlayersCache(
					new File(dataDirectory.toFile(), "VotingPlugin" + File.separator + "nonvotedplayerscache.yml"),
					this);

			try {
				for (String serverToCheck : voteCacheFile.getServers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<OfflineBungeeVote>();
					for (String num : voteCacheFile.getServerVotes(serverToCheck)) {
						ConfigurationNode data = voteCacheFile.getServerVotes(serverToCheck, num);

						vote.add(new OfflineBungeeVote(data.node("Name").getString(), data.node("UUID").getString(),
								data.node("Service").getString(), data.node("Time").getLong(),
								data.node("Real").getBoolean(), data.node("TEXT").getString()));
					}
					cachedVotes.put(server.getServer(serverToCheck).get(), vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : voteCacheFile.getPlayers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<OfflineBungeeVote>();
					for (String num : voteCacheFile.getOnlineVotes(player)) {
						ConfigurationNode data = voteCacheFile.getOnlineVotes(player, num);
						vote.add(new OfflineBungeeVote(data.node("Name").getString(), data.node("UUID").getString(),
								data.node("Service").getString(), data.node("Time").getLong(),
								data.node("Real").getBoolean(), data.node("TEXT").getString()));
					}
					cachedOnlineVotes.put(player, vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			voteCacheFile.clearData();

			server.getScheduler().buildTask(this, () -> {
				for (RegisteredServer server : cachedVotes.keySet()) {
					checkCachedVotes(server);
				}

				for (String player : cachedOnlineVotes.keySet()) {
					checkOnlineVotes(server.getPlayer(UUID.fromString(player)).get(), player, null);
				}
			}).delay(15L, TimeUnit.SECONDS).repeat(30l, TimeUnit.SECONDS).schedule();

			server.getScheduler().buildTask(this, () -> {
				if (nonVotedPlayersCache != null) {
					debug("Checking nonvotedplayerscache.yml...");
					nonVotedPlayersCache.check();
				}
			}).delay(1L, TimeUnit.MINUTES).repeat(60l, TimeUnit.MINUTES).schedule();
		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(dataDirectory.toFile(), "secretkey.key"));

			socketHandler = new SocketHandler(
					server.getPluginManager().getPlugin("votingplugin").get().getDescription().getVersion().get(),
					config.getBungeeHost(), config.getBungeePort(), encryptionHandler, config.getDebug());

			socketHandler.add(new SocketReceiver() {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 1) {
						if (data.length > 2) {
							if (data[0].equalsIgnoreCase("Broadcast")) {
								sendServerMessage(data);
							}
						}
					}

				}
			});

			socketHandler.add(new SocketReceiver() {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 1) {
						if (data[0].equalsIgnoreCase("StatusOkay")) {
							String server = data[1];
							logger.info("Voting communicaton okay with " + server);
						}
					}

				}
			});

			clientHandles = new HashMap<String, ClientHandler>();
			List<String> l = config.getBlockedServers();
			for (String s : config.getSpigotServers()) {
				if (!l.contains(s)) {
					ConfigurationNode d = config.getSpigotServerConfiguration(s);
					clientHandles.put(s, new ClientHandler(d.node("Host").getString(""), d.node("Port").getInt(1298),
							encryptionHandler, config.getDebug()));
				}
			}
		}

		logger.info("VotingPlugin velocity loaded, method: " + method.toString());
	}

	@Subscribe
	public void onProxyDisable(ProxyShutdownEvent event) {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			for (Entry<RegisteredServer, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
				RegisteredServer server = entry.getKey();
				int num = 0;
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheFile.addVote(server.getServerInfo().getName(), num, voteData);
					num++;
				}
			}
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
				String name = entry.getKey();
				int num = 0;
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheFile.addVoteOnline(name, num, voteData);
					num++;
				}
			}
			nonVotedPlayersCache.save();
		}
	}

	@Subscribe
	public void playerServerConnected(ServerConnectedEvent event) {
		checkCachedVotes(event.getServer());
		checkOnlineVotes(event.getPlayer(), event.getPlayer().getUniqueId().toString(), event.getServer());
	}

	@Subscribe
	public void onPluginMessagingReceived(PluginMessageEvent event) {
		if (event.getIdentifier().equals("vp:vp")) {
			ByteArrayInputStream instream = new ByteArrayInputStream(event.getData());
			DataInputStream in = new DataInputStream(instream);
			try {
				ByteArrayOutputStream outstream = new ByteArrayOutputStream();
				DataOutputStream out = new DataOutputStream(outstream);
				String subchannel = in.readUTF();
				int size = in.readInt();

				// check for status message returns
				if (subchannel.equalsIgnoreCase("statusokay")) {
					String server = in.readUTF();
					logger.info("Status okay for " + server);
					return;
				} else if (subchannel.equalsIgnoreCase("login")) {
					String player = in.readUTF();
					Player p = server.getPlayer(player).get();
					checkCachedVotes(p.getCurrentServer().get().getServer());
					checkOnlineVotes(p, p.getUniqueId().toString(), p.getCurrentServer().get().getServer());
					return;
				} else {

					// reforward message
					out.writeUTF(subchannel);
					out.writeInt(size);
					for (int i = 0; i < size; i++) {
						out.writeUTF(in.readUTF());
					}
					for (RegisteredServer send : server.getAllServers()) {

						if (send.getPlayersConnected().size() > 0) {
							send.sendPluginMessage(ident, outstream.toByteArray());
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	public String getUUID(String playerName) {
		Player p = server.getPlayer(playerName).get();
		if (p != null && p.isActive()) {
			return p.getUniqueId().toString();
		}
		if (mysql != null) {
			String str = mysql.getUUID(playerName);
			if (str != null) {
				return str;
			}
		}
		if (nonVotedPlayersCache != null) {
			return nonVotedPlayersCache.playerExists(playerName);
		}
		return "";
	}

	public UUID fetchUUID(String playerName) throws Exception {
		// Get response from Mojang API
		URL url = new URL("https://api.mojang.com/users/profiles/minecraft/" + playerName);
		HttpURLConnection connection = (HttpURLConnection) url.openConnection();
		connection.connect();

		if (connection.getResponseCode() == 400) {
			logger.info("There is no player with the name \"" + playerName + "\"!");
			return null;
		}

		InputStream inputStream = connection.getInputStream();
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

		// Parse JSON response and get UUID
		JsonElement element = new JsonParser().parse(bufferedReader);
		JsonObject object = element.getAsJsonObject();
		String uuidAsString = object.get("id").getAsString();

		// Return UUID
		return parseUUIDFromString(uuidAsString);
	}

	private UUID parseUUIDFromString(String uuidAsString) {
		String[] parts = { "0x" + uuidAsString.substring(0, 8), "0x" + uuidAsString.substring(8, 12),
				"0x" + uuidAsString.substring(12, 16), "0x" + uuidAsString.substring(16, 20),
				"0x" + uuidAsString.substring(20, 32) };

		long mostSigBits = Long.decode(parts[0]).longValue();
		mostSigBits <<= 16;
		mostSigBits |= Long.decode(parts[1]).longValue();
		mostSigBits <<= 16;
		mostSigBits |= Long.decode(parts[2]).longValue();

		long leastSigBits = Long.decode(parts[3]).longValue();
		leastSigBits <<= 48;
		leastSigBits |= Long.decode(parts[4]).longValue();

		return new UUID(mostSigBits, leastSigBits);
	}

	public void debug(String msg) {
		if (config.getDebug()) {
			logger.info("Debug: " + msg);
		}
	}

	private int getValue(ArrayList<Column> cols, String column) {
		for (Column d : cols) {
			if (d.getName().equalsIgnoreCase(column)) {

				Object value = d.getValue();
				int num = 0;
				if (value instanceof Integer) {
					try {
						num = (int) value;
					} catch (ClassCastException | NullPointerException ex) {
					}
				} else if (value instanceof String) {
					try {
						num = Integer.parseInt((String) value);
					} catch (Exception e) {
					}
				}
				return num;
			}
		}
		return 0;
	}

	private int mysqlUpdate(ArrayList<Column> cols, String uuid, String column, int toAdd) {
		int num = getValue(cols, column) + toAdd;
		debug("Setting " + column + " to " + num + " for " + uuid);
		mysql.update(uuid, column, num, DataType.INTEGER);
		return num;
	}

	@Subscribe
	public void onVotifierEvent(VotifierEvent event) {
		Vote vote = event.getVote();
		vote(vote.getUsername(), vote.getServiceName(), true);
	}

	public String getProperName(String uuid, String currentName) {
		Player p = server.getPlayer(UUID.fromString(uuid)).get();
		if (p != null && p.isActive()) {
			return p.getUsername();
		}
		return currentName;
	}

	public void sendPluginMessageServer(RegisteredServer s, String channel, String... messageData) {
		ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(byteOutStream);
		try {
			out.writeUTF(channel);
			out.writeInt(messageData.length);
			for (String message : messageData) {
				out.writeUTF(message);
			}
			if (s.getPlayersConnected().size() > 0) {
				s.sendPluginMessage(ident, byteOutStream.toByteArray());
			}
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		debug("Sending plugin message " + server + " " + channel + " "
				+ ArrayUtils.getInstance().makeStringList(ArrayUtils.getInstance().convert(messageData)));

	}

	public void vote(String player, String service, boolean realVote) {
		try {
			if (player == null || player.isEmpty()) {
				logger.info("No name from vote on " + service);
				return;
			}

			String uuid = getUUID(player);
			if (uuid.isEmpty()) {
				if (config.getAllowUnJoined()) {
					UUID u = null;
					try {
						u = fetchUUID(player);
					} catch (Exception e) {
						if (getConfig().getDebug()) {
							e.printStackTrace();
						}
					}
					if (u == null) {
						debug("Failed to get uuid for " + player);
						return;
					}
					uuid = u.toString();
				} else {
					logger.info("Ignoring vote from " + player);
					return;
				}
			}

			player = getProperName(uuid, player);

			BungeeMessageData text = null;

			if (getConfig().getBungeeManageTotals()) {

				if (!mysql.getUuids().contains(uuid)) {
					mysql.update(uuid, "PlayerName", player, DataType.STRING);
				}

				ArrayList<Column> data = mysql.getExactQuery(new Column("uuid", uuid, DataType.STRING));

				text = new BungeeMessageData(mysqlUpdate(data, uuid, "AllTimeTotal", 1),
						mysqlUpdate(data, uuid, "MonthTotal", 1), mysqlUpdate(data, uuid, "WeeklyTotal", 1),
						mysqlUpdate(data, uuid, "DailyTotal", 1),
						mysqlUpdate(data, uuid, "Points", getConfig().getPointsOnVote()),
						mysqlUpdate(data, uuid, "MilestoneCount", 1));
			} else {
				text = new BungeeMessageData(0, 0, 0, 0, 0, 0);
			}

			/*
			 * String text = mysqlUpdate(data, uuid, "AllTimeTotal", 1) + "//" +
			 * mysqlUpdate(data, uuid, "MonthTotal", 1) + "//" + mysqlUpdate(data, uuid,
			 * "WeeklyTotal", 1) + "//" + mysqlUpdate(data, uuid, "DailyTotal", 1) + "//" +
			 * mysqlUpdate(data, uuid, "Points", 1) + "//" + mysqlUpdate(data, uuid,
			 * "MilestoneCount", 1);
			 */

			long time = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();

			if (method.equals(BungeeMethod.PLUGINMESSAGING)) {

				if (config.getSendVotesToAllServers()) {
					for (RegisteredServer s : server.getAllServers()) {
						if (!config.getBlockedServers().contains(s.getServerInfo().getName())) {
							boolean forceCache = false;
							Player p = server.getPlayer(UUID.fromString(uuid)).get();
							if ((p == null || !p.isActive()) && getConfig().getWaitForUserOnline()) {
								forceCache = true;
								debug("Forcing vote to cache");
							}
							if (s.getPlayersConnected().isEmpty() || forceCache) {
								// cache
								if (!cachedVotes.containsKey(s)) {
									cachedVotes.put(s, new ArrayList<OfflineBungeeVote>());
								}
								ArrayList<OfflineBungeeVote> list = cachedVotes.get(s);
								list.add(new OfflineBungeeVote(player, uuid, service, time, realVote, text.toString()));
								cachedVotes.put(s, list);

								debug("Caching vote for " + player + " on " + service + " for " + s);

							} else {
								// send
								sendPluginMessageServer(s, "Vote", player, uuid, service, "" + time,
										Boolean.TRUE.toString(), "" + realVote, text.toString(),
										"" + getConfig().getBungeeManageTotals(),
										"" + BungeeVersion.getPluginMessageVersion());
							}
						}
					}
				} else {
					Player p = server.getPlayer(UUID.fromString(uuid)).get();
					if (p != null && p.isActive() && !config.getBlockedServers()
							.contains(p.getCurrentServer().get().getServerInfo().getName())) {
						sendPluginMessageServer(p.getCurrentServer().get().getServer(), "VoteOnline", player, uuid,
								service, "" + time, Boolean.TRUE.toString(), "" + realVote, text.toString(),
								"" + getConfig().getBungeeManageTotals(), "" + BungeeVersion.getPluginMessageVersion());
					} else {
						if (!cachedOnlineVotes.containsKey(uuid)) {
							cachedOnlineVotes.put(uuid, new ArrayList<OfflineBungeeVote>());
						}
						ArrayList<OfflineBungeeVote> list = cachedOnlineVotes.get(uuid);
						if (list == null) {
							list = new ArrayList<OfflineBungeeVote>();
						}
						list.add(new OfflineBungeeVote(player, uuid, service, time, realVote, text.toString()));
						cachedOnlineVotes.put(uuid, list);
						debug("Caching online vote for " + player + " on " + service);
					}
				}
			} else if (method.equals(BungeeMethod.SOCKETS)) {
				sendSocketVote(player, service, text);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void sendSocketVote(String name, String service, BungeeMessageData text) {
		String uuid = getUUID(name);

		if (config.getSendVotesToAllServers()) {
			sendServerMessage("bungeevote", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
			if (config.getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
		} else {
			// online server only
			Player p = server.getPlayer(name).get();

			String server = "";
			if (p != null && p.isActive()) {
				server = p.getCurrentServer().get().getServerInfo().getName();
			} else {
				server = config.getFallBack();
			}
			if (config.getBlockedServers().contains(server)) {
				server = config.getFallBack();
			}

			sendServerMessageServer(server, "bungeevoteonline", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
			if (config.getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
			sendServerMessage("BungeeUpdate");
		}

	}

	public void sendServerMessage(String... messageData) {
		for (ClientHandler h : clientHandles.values()) {
			h.sendMessage(messageData);
		}
	}

	public void sendServerMessageServer(String server, String... messageData) {
		if (clientHandles.containsKey(server)) {
			clientHandles.get(server).sendMessage(messageData);
		}
	}
}
