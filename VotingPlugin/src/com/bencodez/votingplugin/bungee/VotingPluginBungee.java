package com.bencodez.votingplugin.bungee;

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
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.api.user.userstorage.sql.Column;
import com.bencodez.advancedcore.api.user.userstorage.sql.DataType;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import lombok.Getter;
import net.md_5.bungee.api.CommandSender;
import net.md_5.bungee.api.config.ServerInfo;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.connection.Server;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.event.PostLoginEvent;
import net.md_5.bungee.api.event.ServerConnectedEvent;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.event.EventHandler;

public class VotingPluginBungee extends Plugin implements net.md_5.bungee.api.plugin.Listener {

	private HashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new HashMap<String, ArrayList<OfflineBungeeVote>>();

	private HashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new HashMap<String, ArrayList<OfflineBungeeVote>>();

	private HashMap<String, ClientHandler> clientHandles;

	@Getter
	private Config config;

	private EncryptionHandler encryptionHandler;

	@Getter
	private BungeeMethod method;

	@Getter
	private BungeeMySQL mysql;

	private SocketHandler socketHandler;

	private VoteCache voteCacheFile;

	private NonVotedPlayersCache nonVotedPlayersCache;

	public void checkCachedVotes(String server) {
		if (getProxy().getServerInfo(server) != null) {
			if (!getProxy().getServerInfo(server).getPlayers().isEmpty()) {
				if (cachedVotes.containsKey(server) && !config.getBlockedServers().contains(server)) {
					ArrayList<OfflineBungeeVote> c = cachedVotes.get(server);
					ArrayList<OfflineBungeeVote> newSet = new ArrayList<OfflineBungeeVote>();
					if (!c.isEmpty()) {
						for (OfflineBungeeVote cache : c) {
							boolean toSend = true;
							if (getConfig().getWaitForUserOnline()) {
								ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(cache.getUuid()));
								if (p == null || !p.isConnected()) {
									toSend = false;
								} else if (p != null && p.isConnected()
										&& !p.getServer().getInfo().getName().equals(server)) {
									toSend = false;
								}

							}
							if (toSend) {
								sendPluginMessageServer(server, "Vote", cache.getPlayerName(), cache.getUuid(),
										cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
										"" + cache.isRealVote(), cache.getText());
							} else {
								debug("Not sending vote because user isn't on server " + server + ": "
										+ cache.toString());
								newSet.add(cache);
							}
						}
						cachedVotes.put(server, newSet);
					}
				}
			}
		}

	}

	public void checkOnlineVotes(ProxiedPlayer player, String uuid, String server) {
		if (player != null && player.isConnected() && cachedOnlineVotes.containsKey(uuid)) {
			ArrayList<OfflineBungeeVote> c = cachedOnlineVotes.get(uuid);
			if (!c.isEmpty()) {
				if (server == null) {
					server = getProxy().getPlayer(UUID.fromString(uuid)).getServer().getInfo().getName();
				}
				if (!config.getBlockedServers().contains(server)) {
					for (OfflineBungeeVote cache : c) {
						sendPluginMessageServer(server, "VoteOnline", cache.getPlayerName(), cache.getUuid(),
								cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
								"" + cache.isRealVote(), cache.getText());
					}
					cachedOnlineVotes.put(uuid, new ArrayList<OfflineBungeeVote>());
				}
			}
		}
	}

	public void debug(String msg) {
		if (config.getDebug()) {
			getLogger().info("Debug: " + msg);
		}
	}

	public UUID fetchUUID(String playerName) throws Exception {
		// Get response from Mojang API
		URL url = new URL("https://api.mojang.com/users/profiles/minecraft/" + playerName);
		HttpURLConnection connection = (HttpURLConnection) url.openConnection();
		connection.connect();

		if (connection.getResponseCode() == 400) {
			getProxy().getLogger().info("There is no player with the name \"" + playerName + "\"!");
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

	public String getProperName(String uuid, String currentName) {
		ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
		if (p != null && p.isConnected()) {
			return p.getName();
		}
		return currentName;
	}

	public String getUUID(String playerName) {
		ProxiedPlayer p = getProxy().getPlayer(playerName);
		if (p != null && p.isConnected()) {
			return p.getUniqueId().toString();
		}
		String str = mysql.getUUID(playerName);
		if (str != null) {
			return str;
		}
		if (nonVotedPlayersCache != null) {
			return nonVotedPlayersCache.playerExists(playerName);
		}
		return "";
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

	@Override
	public void onDisable() {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
				String server = entry.getKey();
				int num = 0;
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheFile.addVote(server, num, voteData);
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

	@Override
	public void onEnable() {
		getProxy().getPluginManager().registerListener(this, this);
		config = new Config(this);
		config.load();

		mysql = new BungeeMySQL("VotingPlugin_Users", config.getData());

		// column types
		getMysql().alterColumnType("TopVoterIgnore", "VARCHAR(5)");
		getMysql().alterColumnType("CheckWorld", "VARCHAR(5)");
		getMysql().alterColumnType("Reminded", "VARCHAR(5)");
		getMysql().alterColumnType("DisableBroadcast", "VARCHAR(5)");
		getMysql().alterColumnType("LastOnline", "VARCHAR(20)");
		getMysql().alterColumnType("PlayerName", "VARCHAR(30)");
		getMysql().alterColumnType("DailyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("WeeklyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("DayVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("BestDayVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("WeekVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("BestWeekVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("VotePartyVotes", "INT DEFAULT '0'");
		getMysql().alterColumnType("MonthVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("Points", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestDailyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("AllTimeTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestMonthlyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("MilestoneCount", "INT DEFAULT '0'");
		getMysql().alterColumnType("MonthTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestWeeklyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("LastMonthTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("OfflineRewards", "MEDIUMTEXT");

		getProxy().getPluginManager().registerCommand(this, new VotingPluginBungeeCommand(this));

		method = BungeeMethod.getByName(config.getBungeeMethod());
		if (method == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}

		this.getProxy().registerChannel("vp:vp");

		if (method.equals(BungeeMethod.MYSQL)) {
			// this.getProxy().registerChannel("vp:vp");

		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			voteCacheFile = new VoteCache(this);
			voteCacheFile.load();

			nonVotedPlayersCache = new NonVotedPlayersCache(this);
			nonVotedPlayersCache.load();

			try {
				for (String server : voteCacheFile.getServers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<OfflineBungeeVote>();
					for (String num : voteCacheFile.getServerVotes(server)) {
						Configuration data = voteCacheFile.getServerVotes(server, num);
						vote.add(new OfflineBungeeVote(data.getString("Name"), data.getString("UUID"),
								data.getString("Service"), data.getLong("Time"), data.getBoolean("Real"),
								data.getString("Text")));
					}
					cachedVotes.put(server, vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : voteCacheFile.getPlayers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<OfflineBungeeVote>();
					for (String num : voteCacheFile.getServerVotes(player)) {
						Configuration data = voteCacheFile.getOnlineVotes(player, num);
						vote.add(new OfflineBungeeVote(data.getString("Name"), data.getString("UUID"),
								data.getString("Service"), data.getLong("Time"), data.getBoolean("Real"),
								data.getString("Text")));
					}
					cachedOnlineVotes.put(player, vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			voteCacheFile.clearData();

			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {
					for (String server : cachedVotes.keySet()) {
						checkCachedVotes(server);
					}

					for (String player : cachedOnlineVotes.keySet()) {
						checkOnlineVotes(getProxy().getPlayer(UUID.fromString(player)), player, null);
					}
				}
			}, 15l, 30l, TimeUnit.SECONDS);

			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {
					if (nonVotedPlayersCache != null) {
						debug("Checking nonvotedplayers.yml...");
						nonVotedPlayersCache.check();
					}
				}
			}, 1l, 60l, TimeUnit.MINUTES);
		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(getDataFolder(), "secretkey.key"));

			socketHandler = new SocketHandler(getDescription().getVersion(), config.getBungeeHost(),
					config.getBungeePort(), encryptionHandler, config.getDebug());

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
							getLogger().info("Voting communicaton okay with " + server);
						}
					}

				}
			});

			clientHandles = new HashMap<String, ClientHandler>();
			List<String> l = config.getBlockedServers();
			for (String s : config.getSpigotServers()) {
				if (!l.contains(s)) {
					Configuration d = config.getSpigotServerConfiguration(s);
					clientHandles.put(s, new ClientHandler(d.getString("Host", ""), d.getInt("Port", 1298),
							encryptionHandler, config.getDebug()));
				}
			}
		}

		BStatsMetricsBungee metrics = new BStatsMetricsBungee(this, 9453);

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("bungee_method", () -> getConfig().getBungeeMethod().toString()));

		metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("sendtoallservers",
				() -> "" + getConfig().getSendVotesToAllServers()));

		getLogger().info("VotingPlugin loaded, using method: " + method.toString());
	}

	@EventHandler
	public void onPluginMessage(PluginMessageEvent ev) {
		if (!ev.getTag().equals("vp:vp".toLowerCase())) {
			return;
		}

		ev.setCancelled(true);

		if (!(ev.getSender() instanceof Server))
			return;

		ByteArrayInputStream instream = new ByteArrayInputStream(ev.getData());
		DataInputStream in = new DataInputStream(instream);
		try {
			ByteArrayOutputStream outstream = new ByteArrayOutputStream();
			DataOutputStream out = new DataOutputStream(outstream);
			String subchannel = in.readUTF();
			int size = in.readInt();

			// check for status message returns
			if (subchannel.equalsIgnoreCase("statusokay")) {
				String server = in.readUTF();
				getLogger().info("Status okay for " + server);
				return;
			} else {
				// reforward message
				out.writeUTF(subchannel);
				out.writeInt(size);
				for (int i = 0; i < size; i++) {
					out.writeUTF(in.readUTF());
				}
				for (String send : getProxy().getServers().keySet()) {
					if (getProxy().getServers().get(send).getPlayers().size() > 0) {
						getProxy().getServers().get(send).sendData("vp:vp".toLowerCase(), outstream.toByteArray());
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@EventHandler
	public void onLogin(PostLoginEvent event) {
		if (nonVotedPlayersCache != null) {
			nonVotedPlayersCache.addPlayer(event.getPlayer());
		}
	}

	@EventHandler
	public void onServerConnected(ServerConnectedEvent event) {
		final String server = event.getServer().getInfo().getName();
		final String uuid = event.getPlayer().getUniqueId().toString();
		getProxy().getScheduler().schedule(this, new Runnable() {

			@Override
			public void run() {
				checkCachedVotes(server);
				checkOnlineVotes(event.getPlayer(), uuid, server);
			}

		}, 2, TimeUnit.SECONDS);
	}

	@EventHandler
	public void onVote(VotifierEvent event) {
		Vote vote = event.getVote();
		getLogger().info("Vote received " + vote.getUsername() + " from service site " + vote.getServiceName());

		vote(vote.getUsername(), vote.getServiceName(), true);

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

	public void reload() {
		config.load();
	}

	public void sendPluginMessageServer(String server, String channel, String... messageData) {
		ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(byteOutStream);
		try {
			out.writeUTF(channel);
			out.writeInt(messageData.length);
			for (String message : messageData) {
				out.writeUTF(message);
			}
			if (getProxy().getServers().get(server).getPlayers().size() > 0) {
				getProxy().getServers().get(server).sendData("vp:vp".toLowerCase(), byteOutStream.toByteArray(), false);
			}
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		debug("Sending plugin message " + server + " " + channel + " "
				+ ArrayUtils.getInstance().makeStringList(ArrayUtils.getInstance().convert(messageData)));

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

	public void sendSocketVote(String name, String service, BungeeMessageData text) {
		String uuid = getUUID(name);

		if (config.getSendVotesToAllServers()) {
			sendServerMessage("bungeevote", uuid, name, service, text.toString());
			if (config.getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
		} else {
			// online server only
			ProxiedPlayer p = getProxy().getPlayer(name);

			String server = "";
			if (p != null && p.isConnected()) {
				server = p.getServer().getInfo().getName();
			} else {
				server = config.getFallBack();
			}
			if (config.getBlockedServers().contains(server)) {
				server = config.getFallBack();
			}

			sendServerMessageServer(server, "bungeevoteonline", uuid, name, service, text.toString());
			if (config.getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
			sendServerMessage("BungeeUpdate");
		}

	}

	public void status(CommandSender sender) {
		if (method.equals(BungeeMethod.SOCKETS)) {
			sendServerMessage("status");
		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			for (String s : getProxy().getServers().keySet()) {
				if (!config.getBlockedServers().contains(s)) {
					ServerInfo info = getProxy().getServerInfo(s);
					if (info.getPlayers().isEmpty()) {
						getLogger().info("No players on server " + s + " to send test status message");
					} else {
						// send
						getLogger().info("Sending request for status message on " + s);
						sendPluginMessageServer(s, "Status", s);
					}
				} else {
					getLogger().info("Ignoring blocked server " + s);
				}
			}
		}
	}

	public void vote(String player, String service, boolean realVote) {
		if (player == null || player.isEmpty()) {
			getLogger().info("No name from vote on " + service);
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
				getLogger().info("Ignoring vote from " + player);
				return;
			}
		}

		player = getProperName(uuid, player);

		if (!mysql.getUuids().contains(uuid)) {
			mysql.update(uuid, "PlayerName", player, DataType.STRING);
		}

		ArrayList<Column> data = mysql.getExactQuery(new Column("uuid", uuid, DataType.STRING));

		BungeeMessageData text = new BungeeMessageData(mysqlUpdate(data, uuid, "AllTimeTotal", 1),
				mysqlUpdate(data, uuid, "MonthTotal", 1), mysqlUpdate(data, uuid, "WeeklyTotal", 1),
				mysqlUpdate(data, uuid, "DailyTotal", 1), mysqlUpdate(data, uuid, "Points", 1),
				mysqlUpdate(data, uuid, "MilestoneCount", 1));

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
				for (String s : getProxy().getServers().keySet()) {
					if (!config.getBlockedServers().contains(s)) {
						ServerInfo info = getProxy().getServerInfo(s);
						boolean forceCache = false;
						ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
						if ((p == null || !p.isConnected()) && getConfig().getWaitForUserOnline()) {
							forceCache = true;
							debug("Forcing vote to cache");
						}
						if (info.getPlayers().isEmpty() || forceCache) {
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
									Boolean.TRUE.toString(), "" + realVote, text.toString());
						}
					}
				}
			} else {
				ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
				if (p != null && p.isConnected()
						&& !config.getBlockedServers().contains(p.getServer().getInfo().getName())) {
					sendPluginMessageServer(p.getServer().getInfo().getName(), "VoteOnline", player, uuid, service,
							"" + time, Boolean.TRUE.toString(), "" + realVote, text.toString());
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

	}

}
