package com.bencodez.votingplugin.bungee.proxy;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.usercache.value.DataValue;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueBoolean;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueInt;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueString;
import com.bencodez.advancedcore.api.user.userstorage.Column;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.mysql.ProxyMySQL;
import com.bencodez.advancedcore.bungeeapi.redis.RedisHandler;
import com.bencodez.advancedcore.bungeeapi.redis.RedisListener;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.advancedcore.bungeeapi.time.BungeeTimeChecker;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.json.JsonParser;
import com.bencodez.votingplugin.bungee.BungeeMessageData;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.bungee.BungeeVersion;
import com.bencodez.votingplugin.bungee.OfflineBungeeVote;
import com.bencodez.votingplugin.bungee.global.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.bungee.global.multiproxy.MultiProxyMethod;
import com.bencodez.votingplugin.bungee.global.multiproxy.MultiProxyServerSocketConfiguration;
import com.bencodez.votingplugin.bungee.global.multiproxy.MultiProxyServerSocketConfigurationBungee;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import lombok.Getter;
import lombok.Setter;

public abstract class VotingPluginProxy {
	@Getter
	private int votePartyVotes = 0;

	@Getter
	@Setter
	private int currentVotePartyVotesRequired = 0;

	@Getter
	@Setter
	private ProxyMySQL proxyMySQL;

	private EncryptionHandler encryptionHandler;

	@Getter
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	@Getter
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new ConcurrentHashMap<>();

	private HashMap<String, ClientHandler> clientHandles;

	private SocketHandler socketHandler;

	@Getter
	@Setter
	private boolean votifierEnabled = true;

	@Getter
	private ConcurrentHashMap<UUID, String> uuidPlayerNameCache = new ConcurrentHashMap<>();

	@Getter
	@Setter
	private GlobalDataHandlerProxy globalDataHandler;

	@Getter
	private RedisHandler redisHandler;

	private boolean enabled;

	@Getter
	private MultiProxyHandler multiProxyHandler;

	@Getter
	private BungeeTimeChecker bungeeTimeChecker;

	@Getter
	@Setter
	private BungeeMethod method;

	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	public VotingPluginProxy() {
		enabled = true;

		bungeeTimeChecker = new BungeeTimeChecker(getConfig().getTimeZone(), getConfig().getTimeHourOffSet()) {

			@Override
			public void debug(String text) {
				debug2(text);
			}

			@Override
			public long getLastUpdated() {
				return getVoteCacheLastUpdated();
			}

			@Override
			public int getPrevDay() {
				return getVoteCachePrevDay();
			}

			@Override
			public String getPrevMonth() {
				return getVoteCachePrevMonth();
			}

			@Override
			public int getPrevWeek() {
				return getVoteCachePrevWeek();
			}

			@Override
			public void info(String text) {
				log(text);
			}

			@Override
			public boolean isEnabled() {
				return enabled;
			}

			@Override
			public boolean isIgnoreTime() {
				return isVoteCacheIgnoreTime();
			}

			@Override
			public void setIgnoreTime(boolean ignore) {
				setVoteCacheVoteCacheIgnoreTime(ignore);
			}

			@Override
			public void setLastUpdated() {
				setVoteCacheLastUpdated();
			}

			@Override
			public void setPrevDay(int day) {
				setVoteCachePrevDay(day);
			}

			@Override
			public void setPrevMonth(String text) {
				setVoteCachePrevMonth(text);
			}

			@Override
			public void setPrevWeek(int week) {
				setVoteCachePrevWeek(week);
			}

			@Override
			public void timeChanged(TimeType type, boolean fake, boolean pre, boolean post) {
				if (getConfig().getVoteCacheTime() > 0) {
					checkVoteCacheTime();
				}
				if (!getConfig().getGlobalDataEnabled()) {
					warn("Global data not enabled, ignoring time change event");
					return;
				}
				for (String s : getAllAvailableServers()) {
					if (getGlobalDataHandler().getGlobalMysql().containsKey(s)) {
						String lastOnlineStr = getGlobalDataHandler().getString(s, "LastOnline");
						long lastOnline = 0;
						try {
							lastOnline = Long.valueOf(lastOnlineStr);
						} catch (NumberFormatException e) {
							// e.printStackTrace();
						}

						if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastOnline < 1000
								* 60 * 60 * 12) {
							// server has been online within the 12 hours
							HashMap<String, DataValue> dataToSet = new HashMap<>();
							dataToSet.put("LastUpdated", new DataValueString(
									"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
							dataToSet.put("FinishedProcessing", new DataValueBoolean(false));
							dataToSet.put(type.toString(), new DataValueBoolean(true));
							getGlobalDataHandler().setData(s, dataToSet);
							if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
								sendPluginMessageServer(s, "BungeeTimeChange", "");
							} else if (method.equals(BungeeMethod.SOCKETS)) {
								sendServerMessage(s, "BungeeTimeChange");
							}
						} else {
							warn("Server " + s + " hasn't been online recently");
						}
					} else {
						warn("Server " + s + " global data handler disabled?");
					}

				}
				globalDataHandler.onTimeChange(type);
			}

			@Override
			public void warning(String text) {
				warn(text);
			}
		};

		loadMultiProxySupport();

	}

	public void addCurrentVotePartyVotes(int amount) {
		votePartyVotes += amount;
		setVoteCacheVotePartyCurrentVotes(votePartyVotes);
		debug("Current vote party total: " + votePartyVotes);
	}

	public abstract void addNonVotedPlayer(String uuid, String playerName);

	public void addVoteParty() {
		if (getConfig().getVotePartyEnabled()) {
			addCurrentVotePartyVotes(1);

			checkVoteParty();
		}
	}

	public abstract void broadcast(String message);

	public synchronized void checkCachedVotes(String server) {
		if (isServerValid(server)) {
			if (isSomeoneOnlineServer(server)) {
				if (cachedVotes.containsKey(server) && !getConfig().getBlockedServers().contains(server)) {
					ArrayList<OfflineBungeeVote> c = cachedVotes.get(server);
					ArrayList<OfflineBungeeVote> newSet = new ArrayList<>();
					if (!c.isEmpty()) {
						int num = 1;
						int numberOfVotes = c.size();
						for (OfflineBungeeVote cache : c) {
							boolean toSend = true;
							if (getConfig().getWaitForUserOnline()) {
								if (!isPlayerOnline(cache.getPlayerName())) {
									toSend = false;
								} else if (isPlayerOnline(cache.getPlayerName())
										&& !getCurrentPlayerServer(cache.getPlayerName()).equals(server)) {
									toSend = false;
								}

							}
							if (toSend) {
								sendMessageServer(server, "Vote", cache.getPlayerName(), cache.getUuid(),
										cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
										"" + cache.isRealVote(), cache.getText(),
										"" + getConfig().getBungeeManageTotals(),
										"" + BungeeVersion.getPluginMessageVersion(), "" + getConfig().getBroadcast(),
										"" + num, "" + numberOfVotes);
								num++;
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

	public synchronized void checkOnlineVotes(String player, String uuid, String server) {
		if (isPlayerOnline(player) && cachedOnlineVotes.containsKey(uuid)) {
			ArrayList<OfflineBungeeVote> c = cachedOnlineVotes.get(uuid);
			if (!c.isEmpty()) {
				if (server == null) {
					server = getCurrentPlayerServer(player);
				}
				if (!getConfig().getBlockedServers().contains(server)) {
					int num = 1;
					int numberOfVotes = c.size();
					for (OfflineBungeeVote cache : c) {
						sendMessageServer(server, "VoteOnline", cache.getPlayerName(), cache.getUuid(),
								cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
								"" + cache.isRealVote(), cache.getText(), "" + getConfig().getBungeeManageTotals(),
								"" + BungeeVersion.getPluginMessageVersion(), "" + getConfig().getBroadcast(), "" + num,
								"" + numberOfVotes);
						num++;
					}
					cachedOnlineVotes.put(uuid, new ArrayList<>());
					if (getConfig().getMultiProxySupport() && getConfig().getMultiProxyOneGlobalReward()) {
						multiProxyHandler.sendMultiProxyServerMessage("ClearVote", player, uuid);
					}
				}
			}
		}
	}

	public void checkVoteCacheTime() {
		long cTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (int i = votes.size() - 1; i >= 0; i--) {
				if (cTime - votes.get(i).getTime() > getConfig().getVoteCacheTime() * 24 * 60 * 60 * 1000) {
					votes.remove(i);
				}
			}
		}

		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (int i = votes.size() - 1; i >= 0; i--) {
				if (cTime - votes.get(i).getTime() > getConfig().getVoteCacheTime() * 24 * 60 * 60 * 1000) {
					votes.remove(i);
				}
			}
		}
	}

	public void checkVoteParty() {
		if (getConfig().getVotePartyEnabled()) {
			if (votePartyVotes >= currentVotePartyVotesRequired) {
				debug("Vote party reached");
				addCurrentVotePartyVotes(-currentVotePartyVotesRequired);

				currentVotePartyVotesRequired += getConfig().getVotePartyIncreaseVotesRequired();
				setVoteCacheVotePartyIncreaseVotesRequired(
						getVoteCacheVotePartyIncreaseVotesRequired() + getConfig().getVotePartyIncreaseVotesRequired());

				if (!getConfig().getVotePartyBroadcast().isEmpty()) {
					broadcast(getConfig().getVotePartyBroadcast());
				}

				for (String command : getConfig().getVotePartyBungeeCommands()) {
					runConsoleCommand(command);
				}

				/*
				 * if (method.equals(BungeeMethod.PLUGINMESSAGING)) { if
				 * (getConfig().getVotePartySendToAllServers()) { for (Entry<String, ServerInfo>
				 * entry : getProxy().getServers().entrySet()) { //sendVoteParty(entry.getKey(),
				 * entry.getValue()); } } else { for (String server :
				 * getConfig().getVotePartyServersToSend()) { ServerInfo serverInfo =
				 * getProxy().getServerInfo(server); if (serverInfo != null) {
				 * //sendVoteParty(server, serverInfo); } } } }
				 */
			}
			saveVoteCacheFile();
		}
	}

	public abstract void debug(String str);

	private void debug2(String message) {
		debug(message);
	}

	public UUID fetchUUID(String playerName) throws Exception {
		if (playerName == null || playerName.equalsIgnoreCase("null")) {
			return null;
		}
		// Get response from Mojang API
		URL url = new URL("https://api.mojang.com/users/profiles/minecraft/" + playerName);
		HttpURLConnection connection = (HttpURLConnection) url.openConnection();
		connection.connect();

		if (connection.getResponseCode() == 400) {
			log("There is no player with the name \"" + playerName + "\"!");
			return null;
		}

		InputStream inputStream = connection.getInputStream();
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

		// Parse JSON response and get UUID
		JsonElement element = JsonParser.parseReader(bufferedReader);
		JsonObject object = element.getAsJsonObject();
		String uuidAsString = object.get("id").getAsString();

		// Return UUID
		return parseUUIDFromString(uuidAsString);
	}

	public abstract Set<String> getAllAvailableServers();

	public abstract VotingPluginProxyConfig getConfig();

	public abstract String getCurrentPlayerServer(String player);

	public abstract File getDataFolderPlugin();

	public String getMonthTotalsWithDatePath() {
		LocalDateTime cTime = getBungeeTimeChecker().getTime();
		return getMonthTotalsWithDatePath(cTime);
	}

	public String getMonthTotalsWithDatePath(LocalDateTime cTime) {
		return "MonthTotal-" + cTime.getMonth().toString() + "-" + cTime.getYear();
	}

	public abstract String getProperName(String uuid, String playerName);

	public abstract String getUUID(String playerName);

	private int getValue(ArrayList<Column> cols, String column, int toAdd) {
		for (Column d : cols) {
			if (d.getName().equalsIgnoreCase(column)) {

				DataValue value = d.getValue();
				int num = 0;
				if (value.isInt()) {
					num = value.getInt();
				} else if (value.isString()) {
					try {
						num = Integer.parseInt(value.getString());
					} catch (Exception e) {
					}
				}
				return num + toAdd;
			}
		}
		return toAdd;
	}

	public abstract String getPluginVersion();

	public abstract int getVoteCacheCurrentVotePartyVotes();

	public abstract long getVoteCacheLastUpdated();

	public abstract int getVoteCachePrevDay();

	public abstract String getVoteCachePrevMonth();

	public abstract int getVoteCachePrevWeek();

	public abstract int getVoteCacheVotePartyIncreaseVotesRequired();

	public abstract boolean isPlayerOnline(String playerName);

	public abstract boolean isServerValid(String server);

	public abstract boolean isSomeoneOnlineServer(String server);

	public abstract boolean isVoteCacheIgnoreTime();

	public void load() {
		uuidPlayerNameCache = getProxyMySQL().getRowsUUIDNameQuery();

		bungeeTimeChecker.setTimeChangeFailSafeBypass(getConfig().getTimeChangeFailSafeBypass());
		bungeeTimeChecker.loadTimer();

		method = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (getMethod() == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}

		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {

		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(new File(getDataFolderPlugin(), "secretkey.key"));

			socketHandler = new SocketHandler(getPluginVersion(), getConfig().getBungeeHost(),
					getConfig().getBungeePort(), encryptionHandler, getConfig().getDebug()) {

				@Override
				public void log(String str) {
					logInfo(str);
				}
			};

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
							log("Voting communicaton okay with " + server);
						}
					}

				}
			});

			clientHandles = new HashMap<>();
			List<String> l = getConfig().getBlockedServers();
			for (String s : getConfig().getSpigotServers()) {
				if (!l.contains(s)) {
					Map<String, Object> d = getConfig().getSpigotServerConfiguration(s);
					String host = "";
					if (d.containsKey("Host")) {
						host = (String) d.get("Host");
					}
					int port = 1298;
					if (d.containsKey("Port")) {
						port = (int) d.get("Port");
					}
					clientHandles.put(s, new ClientHandler(host, port, encryptionHandler, getConfig().getDebug()));
				}
			}
		} else if (method.equals(BungeeMethod.REDIS)) {
			redisHandler = new RedisHandler(getConfig().getRedisHost(), getConfig().getRedisPort(),
					getConfig().getRedisUsername(), getConfig().getRedisPassword()) {

				@Override
				public void debug(String message) {
					debug2(message);
				}

				@Override
				protected void onMessage(String channel, String[] message) {
					if (message.length > 0) {
						if (message[0].equalsIgnoreCase("statusokay")) {
							String server = message[1];
							log("Status okay for " + server);
						} else if (message[0].equalsIgnoreCase("TimeChangeFinished")) {

						} else if (message[0].equalsIgnoreCase("login")) {
							String player = message[1];
							String uuid = message[2];
							String server = "";
							if (message.length > 3) {
								server = message[3];
							}
							debug("Login: " + player + "/" + uuid + " " + server);
							addNonVotedPlayer(uuid, player);

							login(player, uuid, server);
						}
					}
				}
			};
			runAsync(new Runnable() {

				@Override
				public void run() {
					redisHandler.loadListener(
							new RedisListener(redisHandler, getConfig().getRedisPrefix() + "VotingPlugin"));
				}
			});

		}
		currentVotePartyVotesRequired = getConfig().getVotePartyVotesRequired()
				+ getVoteCacheVotePartyIncreaseVotesRequired();
		votePartyVotes = getVoteCacheCurrentVotePartyVotes();

		loadMultiProxySupport();
	}

	public void loadMultiProxySupport() {
		if (multiProxyHandler != null) {
			multiProxyHandler.close();
		}
		multiProxyHandler = new MultiProxyHandler() {

			@Override
			public void addNonVotedPlayerCache(String uuid, String player) {
				addNonVotedPlayer(uuid, player);
			}

			@Override
			public void clearVote(String string) {
				cachedOnlineVotes.remove(string);
			}

			@Override
			public boolean getDebug() {
				return getConfig().getDebug();
			}

			@Override
			public EncryptionHandler getEncryptionHandler() {
				return encryptionHandler;
			}

			@Override
			public MultiProxyMethod getMultiProxyMethod() {
				return MultiProxyMethod.getByName(getConfig().getMultiProxyMethod());
			}

			@Override
			public String getMultiProxyPassword() {
				return getConfig().getMultiProxyRedisPassword();
			}

			@Override
			public String getMultiProxyRedisHost() {
				return getConfig().getMultiProxyRedisHost();
			}

			@Override
			public int getMultiProxyRedisPort() {
				return getConfig().getMultiProxyRedisPort();
			}

			@Override
			public boolean getMultiProxyRedisUseExistingConnection() {
				return getConfig().getMultiProxyRedisUseExistingConnection();
			}

			@Override
			public String getMultiProxyServerName() {
				return getConfig().getProxyServerName();
			}

			@Override
			public Collection<String> getMultiProxyServers() {
				return getConfig().getMultiProxyServers();
			}

			@Override
			public MultiProxyServerSocketConfiguration getMultiProxyServersConfiguration(String s) {
				return new MultiProxyServerSocketConfigurationBungee(s,
						getConfig().getMultiProxyServersConfiguration(s));
			}

			@Override
			public String getMultiProxySocketHostHost() {
				return getConfig().getMultiProxySocketHostHost();
			}

			@Override
			public int getMultiProxySocketHostPort() {
				return getConfig().getMultiProxySocketHostPort();
			}

			@Override
			public boolean getMultiProxySupportEnabled() {
				return getConfig().getMultiProxySupport();
			}

			@Override
			public String getMultiProxyUsername() {
				return getConfig().getMultiProxyRedisUsername();
			}

			@Override
			public File getPluginDataFolder() {
				return getDataFolderPlugin();
			}

			@Override
			public boolean getPrimaryServer() {
				return getConfig().getPrimaryServer();
			}

			@Override
			public List<String> getProxyServers() {
				return getConfig().getProxyServers();
			}

			@Override
			public RedisHandler getRedisHandler() {
				return redisHandler;
			}

			@Override
			public String getVersion() {
				return getPluginVersion();
			}

			@Override
			public void logInfo(String msg) {
				log(msg);
			}

			@Override
			public void runAsnc(Runnable runnable) {
				runAsync(runnable);
			}

			@Override
			public void setEncryptionHandler(EncryptionHandler encryptionHandler1) {
				encryptionHandler = encryptionHandler1;
			}

			@Override
			public void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
					BungeeMessageData text, String uuid) {
				vote(player, service, realVote, timeQueue, queueTime, text, uuid);
			}
		};
		multiProxyHandler.loadMultiProxySupport();
	}

	public abstract void log(String message);

	public void login(String playerName, String uuid, String serverName) {
		addNonVotedPlayer(uuid, playerName);
		if (isPlayerOnline(playerName)) {
			if (getConfig().getGlobalDataEnabled()) {
				if (getGlobalDataHandler().isTimeChangedHappened()) {
					getGlobalDataHandler().checkForFinishedTimeChanges();
				}
			}

			checkCachedVotes(serverName);
			checkOnlineVotes(playerName, uuid, serverName);
			multiProxyHandler.login(uuid, playerName);
		}
	}

	private void logInfo(String msg) {
		log(msg);
	}

	public abstract void logSevere(String message);

	public void onDisable() {

		if (getProxyMySQL() != null) {
			getProxyMySQL().shutdown();
		}
		if (multiProxyHandler != null) {
			multiProxyHandler.close();
		}

		enabled = false;
	}

	public void onPluginMessageReceived(DataInputStream in) throws IOException {
		ByteArrayOutputStream outstream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(outstream);
		String subchannel = in.readUTF();
		int size = in.readInt();

		debug("Received plugin message, processing...");

		// check for status message returns
		if (subchannel.equalsIgnoreCase("statusokay")) {
			String server = in.readUTF();
			log("Status okay for " + server);
			return;
		} else if (subchannel.equalsIgnoreCase("TimeChangeFinished")) {
			// not used currently
		} else if (subchannel.equalsIgnoreCase("login")) {
			String player = in.readUTF();
			String uuid = in.readUTF();
			String server = "";
			if (size > 2) {
				server = in.readUTF();
			}
			debug("Login: " + player + "/" + uuid + " " + server);
			login(player, uuid, server);
			return;
		} else if (subchannel.equalsIgnoreCase("VoteUpdate")) {
			// reforward message for VoteUpdate
			out.writeUTF(subchannel);
			out.writeInt(size);
			for (int i = 0; i < size; i++) {
				out.writeUTF(in.readUTF());
			}
			for (String send : getAllAvailableServers()) {
				if (isSomeoneOnlineServer(send)) {
					sendPluginMessageData(send, getConfig().getPluginMessageChannel().toLowerCase(), outstream.toByteArray(), false);
				}
			}
		} else {
			debug("Ignoring plugin message: " + subchannel);
		}
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

	public void processQueue() {
		while (getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getTimeChangeQueue().remove();
			vote(vote.getName(), vote.getService(), true, false, vote.getTime(), null, null);
		}
	}

	public void reload() {
		method = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (getMethod() == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}

		setCurrentVotePartyVotesRequired(
				getConfig().getVotePartyVotesRequired() + getVoteCacheVotePartyIncreaseVotesRequired());

		loadMultiProxySupport();
	}

	public abstract void runAsync(Runnable run);

	public abstract void runConsoleCommand(String command);

	public abstract void saveVoteCacheFile();

	public abstract void reloadCore(boolean mysql);

	public void sendMessageServer(String server, String channel, String... messageData) {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			sendPluginMessageServer(server, channel, messageData);
		} else if (method.equals(BungeeMethod.REDIS)) {
			sendRedisMessageServer(server, channel, messageData);
		}
	}

	public abstract void sendPluginMessageData(String server, String channel, byte[] data, boolean queue);

	public void sendPluginMessageServer(String server, String channel, String... messageData) {
		ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(byteOutStream);
		try {
			out.writeUTF(channel);
			out.writeInt(messageData.length);
			for (String message : messageData) {
				out.writeUTF(message);
			}
			if (isSomeoneOnlineServer(server)) {
				sendPluginMessageData(server, getConfig().getPluginMessageChannel().toLowerCase(), byteOutStream.toByteArray(), false);
			}
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		debug("Sending plugin message " + server + " " + channel + " "
				+ ArrayUtils.makeStringList(ArrayUtils.convert(messageData)));
	}

	public void sendRedisMessageServer(String server, String channel, String... messageData) {
		ArrayList<String> list = new ArrayList<>();
		list.add(channel);
		list.addAll(ArrayUtils.convert(messageData));
		redisHandler.sendMessage(getConfig().getRedisPrefix() + "VotingPlugin_" + server, ArrayUtils.convert(list));
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

	public void sendServerNameMessage() {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			for (String s : getAllAvailableServers()) {
				sendPluginMessageServer(s, "ServerName", s);
			}
		}

	}

	public void sendSocketVote(String name, String service, BungeeMessageData text) {
		String uuid = getUUID(name);

		if (getConfig().getSendVotesToAllServers()) {
			sendServerMessage("bungeevote", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
			if (getConfig().getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
		} else {
			// online server only

			String server = "";
			if (isPlayerOnline(name)) {
				server = getCurrentPlayerServer(name);
			} else {
				server = getConfig().getFallBack();
			}
			if (getConfig().getBlockedServers().contains(server)) {
				server = getConfig().getFallBack();
			}

			sendServerMessageServer(server, "bungeevoteonline", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
			if (getConfig().getBroadcast()) {
				sendServerMessage("BungeeBroadcast", service, uuid, name);
			}
			sendServerMessage("BungeeUpdate");
		}

	}

	public void sendVoteParty(String server) {
		if (isSomeoneOnlineServer(server)) {
			if (method.equals(BungeeMethod.PLUGINMESSAGING) || method.equals(BungeeMethod.REDIS)) {
				sendMessageServer(server, "VotePartyBungee");
			} else if (method.equals(BungeeMethod.SOCKETS)) {
				sendServerMessageServer(server, "VotePartyBungee");
			}
		}
	}

	public void setCurrentVotePartyVotes(int amount) {
		votePartyVotes = amount;
		setVoteCacheVotePartyCurrentVotes(amount);
		debug("Current vote party total: " + votePartyVotes);
	}

	public abstract void setVoteCacheLastUpdated();

	public abstract void setVoteCachePrevDay(int day);

	public abstract void setVoteCachePrevMonth(String text);

	public abstract void setVoteCachePrevWeek(int week);

	public abstract void setVoteCacheVoteCacheIgnoreTime(boolean ignore);

	public abstract void setVoteCacheVotePartyCurrentVotes(int votes);

	public abstract void setVoteCacheVotePartyIncreaseVotesRequired(int votes);

	public void status() {
		if (method.equals(BungeeMethod.SOCKETS)) {
			sendServerMessage("status");
		} else if (method.equals(BungeeMethod.PLUGINMESSAGING) || method.equals(BungeeMethod.REDIS)) {
			for (String s : getAllAvailableServers()) {
				if (!isSomeoneOnlineServer(s)) {
					log("No players on server " + s
							+ " to send test status message, please retest with someone online");
				} else {
					// send
					log("Sending request for status message on " + s);
					sendMessageServer(s, "Status", s);
				}
			}
		}
	}

	public synchronized void vote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			BungeeMessageData text, String uuid) {
		try {
			if (player == null || player.isEmpty()) {
				log("No name from vote on " + service);
				return;
			}
			if (getConfig().getGlobalDataEnabled()) {
				if (getGlobalDataHandler().isTimeChangedHappened()) {
					getGlobalDataHandler().checkForFinishedTimeChanges();
					if (timeQueue && getGlobalDataHandler().isTimeChangedHappened()) {
						timeChangeQueue.add(new VoteTimeQueue(player, service,
								LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
						log("Cachcing vote from " + player + "/" + service
								+ " because time change is happening right now");
						return;
					}
				}
			}

			if (uuid == null || uuid.isEmpty()) {
				uuid = getUUID(player);
			}

			if (uuid.isEmpty()) {
				if (player.startsWith(getConfig().getBedrockPlayerPrefix())) {
					log("Ignoring vote since unable to get UUID of bedrock player");
					return;
				}
				if (!getConfig().getAllowUnJoined()) {
					log("Ignoring vote from " + player + " since player hasn't joined before");
					return;
				}
				if (!getConfig().getUUIDLookup()) {
					log("Failed to get uuid for " + player);
					return;
				}
				debug("Fetching UUID online, since allowunjoined is enabled");
				UUID u = null;
				try {
					if (getConfig().getOnlineMode()) {
						u = fetchUUID(player);
					}
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
			}

			player = getProperName(uuid, player);

			addVoteParty();
			if (getConfig().getPrimaryServer() || !getConfig().getMultiProxySupport()) {
				if (getConfig().getBungeeManageTotals()) {

					if (getProxyMySQL() == null) {
						logSevere("Mysql is not loaded correctly, stopping vote processing");
						return;
					}

					// one time query to insert player
					if (!getProxyMySQL().getUuids().contains(uuid)) {
						getProxyMySQL().update(uuid, "PlayerName", new DataValueString(player));
					}

					ArrayList<Column> data = getProxyMySQL()
							.getExactQuery(new Column("uuid", new DataValueString(uuid)));

					int allTimeTotal = getValue(data, "AllTimeTotal", 1);
					int monthTotal = getValue(data, "MonthTotal", 1);
					int dateMonthTotal = -1;
					if (getConfig().getStoreMonthTotalsWithDate()) {
						if (getConfig().getUseMonthDateTotalsAsPrimaryTotal()) {
							dateMonthTotal = getValue(data, getMonthTotalsWithDatePath(), 1);
						} else {
							dateMonthTotal = monthTotal;
						}
					}
					int weeklyTotal = getValue(data, "WeeklyTotal", 1);
					int dailyTotal = getValue(data, "DailyTotal", 1);
					int points = getValue(data, "Points", getConfig().getPointsOnVote());
					int milestoneCount = getValue(data, "MilestoneCount", 1);

					int maxVotes = getConfig().getMaxAmountOfVotesPerDay();
					if (maxVotes > 0) {
						LocalDateTime cTime = getBungeeTimeChecker().getTime();
						int days = cTime.getDayOfMonth();
						if (monthTotal > days * maxVotes) {
							monthTotal = days * maxVotes;
						}
					}

					if (getConfig().getLimitVotePoints() > 0) {
						if (points > getConfig().getLimitVotePoints()) {
							points = getConfig().getLimitVotePoints();
						}
					}
					text = new BungeeMessageData(allTimeTotal, monthTotal, weeklyTotal, dailyTotal, points,
							milestoneCount, votePartyVotes, currentVotePartyVotesRequired, dateMonthTotal);
					ArrayList<Column> update = new ArrayList<>();
					update.add(new Column("AllTimeTotal", new DataValueInt(allTimeTotal)));
					update.add(new Column("MonthTotal", new DataValueInt(monthTotal)));
					if (getConfig().getStoreMonthTotalsWithDate()) {
						update.add(new Column(getMonthTotalsWithDatePath(), new DataValueInt(dateMonthTotal)));
					}
					update.add(new Column("WeeklyTotal", new DataValueInt(weeklyTotal)));
					update.add(new Column("DailyTotal", new DataValueInt(dailyTotal)));
					update.add(new Column("Points", new DataValueInt(points)));
					update.add(new Column("MilestoneCount", new DataValueInt(milestoneCount)));
					debug("Setting totals " + text.toString());
					getProxyMySQL().update(uuid, update);
				} else {
					text = new BungeeMessageData(0, 0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
				}
			}

			long time = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
			if (queueTime != 0) {
				time = queueTime;
			}

			if (method.equals(BungeeMethod.PLUGINMESSAGING) || method.equals(BungeeMethod.REDIS)) {

				if (getConfig().getSendVotesToAllServers()) {
					for (String s : getAllAvailableServers()) {
						boolean forceCache = false;
						if (!isPlayerOnline(player) && getConfig().getWaitForUserOnline()) {
							forceCache = true;
							debug("Forcing vote to cache");
						}
						if (getConfig().getBroadcast()) {
							sendMessageServer(s, "VoteBroadcast", uuid, player, service);
						}
						if (!isSomeoneOnlineServer(s) || forceCache) {
							// cache
							if (!cachedVotes.containsKey(s)) {
								cachedVotes.put(s, new ArrayList<>());
							}
							ArrayList<OfflineBungeeVote> list = cachedVotes.get(s);
							list.add(new OfflineBungeeVote(player, uuid, service, time, realVote, text.toString()));
							cachedVotes.put(s, list);

							debug("Caching vote for " + player + " on " + service + " for " + s);

						} else {
							// send
							sendMessageServer(s, "Vote", player, uuid, service, "" + time, Boolean.TRUE.toString(),
									"" + realVote, text.toString(), "" + getConfig().getBungeeManageTotals(),
									"" + BungeeVersion.getPluginMessageVersion(), "" + getConfig().getBroadcast(), "1",
									"1");
						}

					}
				} else {
					if (isPlayerOnline(player) && getAllAvailableServers().contains(getCurrentPlayerServer(player))) {
						sendMessageServer(getCurrentPlayerServer(player), "VoteOnline", player, uuid, service,
								"" + time, Boolean.TRUE.toString(), "" + realVote, text.toString(),
								"" + getConfig().getBungeeManageTotals(), "" + BungeeVersion.getPluginMessageVersion(),
								"" + getConfig().getBroadcast(), "1", "1");
						if (getConfig().getMultiProxySupport() && getConfig().getMultiProxyOneGlobalReward()) {
							multiProxyHandler.sendMultiProxyServerMessage("ClearVote", player, uuid);
						}
					} else {
						if (!cachedOnlineVotes.containsKey(uuid)) {
							cachedOnlineVotes.put(uuid, new ArrayList<>());
						}
						ArrayList<OfflineBungeeVote> list = cachedOnlineVotes.get(uuid);
						if (list == null) {
							list = new ArrayList<>();
						}
						list.add(new OfflineBungeeVote(player, uuid, service, time, realVote, text.toString()));
						cachedOnlineVotes.put(uuid, list);
						debug("Caching online vote for " + player + " on " + service);
					}
					for (String s : getAllAvailableServers()) {
						if (getConfig().getBroadcast()) {
							sendMessageServer(s, "VoteBroadcast", uuid, player, service);
						}
						sendMessageServer(s, "VoteUpdate", uuid, "" + votePartyVotes,
								"" + currentVotePartyVotesRequired, text.toString());

					}
				}
			} else if (method.equals(BungeeMethod.SOCKETS)) {
				sendSocketVote(player, service, text);
			}
			if (getConfig().getMultiProxySupport() && getConfig().getPrimaryServer()) {
				if (!getConfig().getMultiProxyOneGlobalReward()) {
					debug("Seending global proxy vote message");
					multiProxyHandler.sendMultiProxyServerMessage("Vote", uuid, player, service, "" + votePartyVotes,
							"" + currentVotePartyVotesRequired, "" + time, "" + realVote, text.toString());
				} else {
					// check if reward should've already been given
					if (!(isPlayerOnline(player)
							&& !getConfig().getBlockedServers().contains(getCurrentPlayerServer(player)))) {
						debug("Sending global proxy voteonline message");
						multiProxyHandler.sendMultiProxyServerMessage("VoteOnline", uuid, player, service,
								"" + votePartyVotes, "" + currentVotePartyVotesRequired, "" + time, "" + realVote,
								text.toString());
					} else {
						debug("Not sending global proxy message for voteonline, player already got reward");
					}
				}

			}
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public abstract void warn(String message);
}
