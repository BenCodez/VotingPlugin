package com.bencodez.votingplugin.proxy;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.sql.SQLException;
import java.time.Instant;
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
import java.util.regex.Pattern;

import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.time.BungeeTimeChecker;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.json.JsonParser;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler.MessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.simpleapi.servercomm.mysql.ProxyMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyMethod;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfiguration;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfigurationBungee;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import lombok.Getter;
import lombok.Setter;

public abstract class VotingPluginProxy {
	@Getter
	@Setter
	private int votePartyVotes = 0;

	@Getter
	@Setter
	private int currentVotePartyVotesRequired = 0;

	@Getter
	@Setter
	private ProxyMysqlUserTable proxyMySQL;

	private EncryptionHandler encryptionHandler;

	@Getter
	@Setter
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	@Getter
	@Setter
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
	@Setter
	private MultiProxyHandler multiProxyHandler;

	@Getter
	private BungeeTimeChecker bungeeTimeChecker;

	@Getter
	@Setter
	private BungeeMethod method;

	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	@Getter
	private MqttHandler mqttHandler;

	@Getter
	private GlobalMessageProxyHandler globalMessageProxyHandler;

	@Getter
	private ProxyMessenger proxyMysqlMessenger;

	public void resetMilestoneCountInVotes() {
		// Iterate through cached online votes
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				String updatedText = updateMilestoneCount(vote.getText());
				vote.setText(updatedText);
			}
		}

		// Iterate through cached votes
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				String updatedText = updateMilestoneCount(vote.getText());
				vote.setText(updatedText);
			}
		}
	}

	private String updateMilestoneCount(String text) {
		BungeeMessageData data = new BungeeMessageData(text);
		data.setMilestoneCount(0); // Reset milestone count to 0
		return data.toString();
	}

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
				if (type.equals(TimeType.MONTH) && getConfig().getResetMilestonesMonthly()) {
					debug("Resetting milestones for month change");
					resetMilestoneCountInVotes();
				}

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
								globalMessageProxyHandler.sendMessage(server, "Vote", cache.getPlayerName(),
										cache.getUuid(), cache.getService(), "" + cache.getTime(),
										Boolean.FALSE.toString(), "" + cache.isRealVote(), cache.getText(),
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
						globalMessageProxyHandler.sendMessage(server, "VoteOnline", cache.getPlayerName(),
								cache.getUuid(), cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
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
				if (votes.get(i).getTime() + (getConfig().getVoteCacheTime() * 24 * 60 * 60 * 1000) < cTime) {
					debug("Removing vote from cache: " + votes.get(i).toString());
					votes.remove(i);
				}
			}
		}

		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (int i = votes.size() - 1; i >= 0; i--) {
				if (votes.get(i).getTime() + (getConfig().getVoteCacheTime() * 24 * 60 * 60 * 1000) < cTime) {
					debug("Removing vote from cache: " + votes.get(i).toString());
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

				if (getConfig().getVotePartySendToAllServers()) {
					for (String server : getAllAvailableServers()) {
						sendVoteParty(server);
					}
				} else {
					for (String server : getConfig().getVotePartyServersToSend()) {
						sendVoteParty(server);
					}
				}

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

		if (method.equals(BungeeMethod.MYSQL)) {
			try {
				proxyMysqlMessenger = new ProxyMessenger("VotingPlugin",
						getProxyMySQL().getMysql().getConnectionManager().getDataSource(), msg -> {
							debug("Got from " + msg.sourceServerId + ": " + msg.payload);
							String[] data = msg.payload.split(Pattern.quote("%l%"));

							globalMessageProxyHandler.onMessage(data[0], ArrayUtils.convertAndRemoveFirst(data));

						});
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			if (getConfig().getPluginMessageEncryption()) {
				encryptionHandler = new EncryptionHandler("VotingPlugin",
						new File(getDataFolderPlugin(), "secretkey.key"));
			}
		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler("VotingPlugin", new File(getDataFolderPlugin(), "secretkey.key"));

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
						globalMessageProxyHandler.onMessage(data[0].toLowerCase(),
								ArrayUtils.convertAndRemoveFirst(data));
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
						if (message.length > 0) {
							globalMessageProxyHandler.onMessage(message[0].toLowerCase(),
									ArrayUtils.convertAndRemoveFirst(message));
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

		} else if (method.equals(BungeeMethod.MQTT)) {
			// use MqttHander

			try {
				mqttHandler = new MqttHandler(new MqttServerComm(getConfig().getMqttClientID(),
						getConfig().getMqttBrokerURL(), getConfig().getMqttUsername(), getConfig().getMqttPassword()),
						2);
				mqttHandler.subscribe(getConfig().getMqttPrefix() + "votingplugin/servers/proxy", new MessageHandler() {

					@Override
					public void onMessage(String topic, String payload) {
						debug("Received mqtt message: " + topic + " - " + payload);
						String[] message = payload.split(":");

						if (message.length > 0) {
							globalMessageProxyHandler.onMessage(message[0].toLowerCase(),
									ArrayUtils.convertAndRemoveFirst(message));
						}
					}
				});
			} catch (MqttException e) {
				e.printStackTrace();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
		currentVotePartyVotesRequired =

				getConfig().getVotePartyVotesRequired() + getVoteCacheVotePartyIncreaseVotesRequired();
		votePartyVotes = getVoteCacheCurrentVotePartyVotes();

		globalMessageProxyHandler = new GlobalMessageProxyHandler() {

			@Override
			public void sendMessage(String server, String channel, String... messageData) {
				switch (method) {
				case MQTT:
					sendMqttMessageServer(server, channel, messageData);
					break;
				case MYSQL:
					// sendPluginMessageServer(server, channel, messageData);

					try {
						proxyMysqlMessenger.sendToBackend(server, channel + "%l%" + String.join("%l%", messageData));
					} catch (SQLException e) {
						e.printStackTrace();
					}

					break;
				case PLUGINMESSAGING:
					sendPluginMessageServer(server, channel, messageData);
					break;
				case REDIS:
					sendRedisMessageServer(server, channel, messageData);
					break;
				case SOCKETS:
					ArrayList<String> data = new ArrayList<>();
					data.add(channel);
					data.addAll(ArrayUtils.convert(messageData));
					sendServerMessageServer(server, ArrayUtils.convert(data));
					break;
				default:
					break;
				}

			}
		};

		globalMessageProxyHandler.addListener(new GlobalMessageListener("login") {

			@Override
			public void onReceive(ArrayList<String> message) {
				if (message.size() < 2) {
					logSevere("Invalid login message received: " + message);
					return;
				}
				String player = message.get(0);
				String uuid = message.get(1);
				String server = "";
				if (message.size() > 2) {
					server = message.get(2);
				}
				debug("Login: " + player + "/" + uuid + " " + server);

				login(player, uuid, server);
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener("statusokay") {

			@Override
			public void onReceive(ArrayList<String> message) {
				String server = message.get(0);
				log("Status okay for " + server);
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener("voteupdate") {

			@Override
			public void onReceive(ArrayList<String> message) {
				for (String send : getAllAvailableServers()) {
					globalMessageProxyHandler.sendMessage(send, "VoteUpdate", ArrayUtils.convert(message));
				}
			}
		});

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
		if (getConfig().getOnlineMode()) { // no need to cache in offline mode
			addNonVotedPlayer(uuid, playerName);
		}
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

		if (getProxyMysqlMessenger() != null) {
			getProxyMysqlMessenger().shutdown();
		}

		if (getProxyMySQL() != null) {
			getProxyMySQL().shutdown();
		}
		if (multiProxyHandler != null) {
			multiProxyHandler.close();
		}

		if (socketHandler != null) {
			socketHandler.closeConnection();
		}

		if (redisHandler != null) {
			redisHandler.close();
		}

		bungeeTimeChecker.shutdown();

		if (getGlobalDataHandler() != null) {
			getGlobalDataHandler().shutdown();
		}

		enabled = false;
	}

	public void onPluginMessageReceived(DataInputStream in) {
		runAsync(new Runnable() {

			@Override
			public void run() {
				try {
					String subchannel = "";
					if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
						subchannel = encryptionHandler.decrypt(in.readUTF());
					} else {
						subchannel = in.readUTF();
					}
					int size = in.readInt();

					debug("Received plugin message, processing..." + subchannel + " " + size);

					String data = "";
					if (size > 0) {
						if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
							data = encryptionHandler.decrypt(in.readUTF());
						} else {
							data = in.readUTF();
						}
					}
					String[] list = data.split("/a/");

					if (list.length > 0) {
						globalMessageProxyHandler.onMessage(subchannel.toLowerCase(), ArrayUtils.convert(list));
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});

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

	/*
	 * public void sendMessageServer(String server, String channel, String...
	 * messageData) { if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
	 * sendPluginMessageServer(server, channel, messageData); } else if
	 * (method.equals(BungeeMethod.REDIS)) { sendRedisMessageServer(server, channel,
	 * messageData); } else if (method.equals(BungeeMethod.MQTT)) {
	 * sendMqttMessageServer(server, channel, messageData); } }
	 */

	public abstract void sendPluginMessageData(String server, String channel, byte[] data, boolean queue);

	public void sendPluginMessageServer(String server, String channel, String... messageData) {
		ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(byteOutStream);
		try {
			if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
				out.writeUTF(encryptionHandler.encrypt(channel));
			} else {
				out.writeUTF(channel);
			}
			out.writeInt(messageData.length);

			String data = "";
			for (String message : messageData) {
				data += message + "/a/";
			}
			if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
				out.writeUTF(encryptionHandler.encrypt(data));
			} else {
				out.writeUTF(data);
			}
			if (isSomeoneOnlineServer(server)) {
				sendPluginMessageData(server, getConfig().getPluginMessageChannel().toLowerCase(),
						byteOutStream.toByteArray(), false);
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

	public void sendMqttMessageServer(String server, String channel, String... messageData) {
		ArrayList<String> list = new ArrayList<>();
		list.add(channel);
		list.addAll(ArrayUtils.convert(messageData));

		try {
			mqttHandler.publish(getConfig().getMqttPrefix() + "votingplugin/servers/" + server, String.join(":", list));
		} catch (Exception e) {
			e.printStackTrace();
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

	public void sendServerNameMessage() {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			for (String s : getAllAvailableServers()) {
				sendPluginMessageServer(s, "ServerName", s);
			}
		}

	}

	public void sendVoteParty(String server) {
		if (isSomeoneOnlineServer(server)) {
			globalMessageProxyHandler.sendMessage(server, "VotePartyBungee", "");
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
		for (String s : getAllAvailableServers()) {
			if (!isSomeoneOnlineServer(s)) {
				log("No players on server " + s + " to send test status message, please retest with someone online");
			} else {
				// send
				log("Sending request for status message on " + s);
				globalMessageProxyHandler.sendMessage(s, "Status", s);
			}
		}
	}

	public String getWaitUntilDelaySiteFromService(String service) {
		for (String site : getConfig().getWaitUntilVoteDelaySites()) {
			if (getConfig().getWaitUntilVoteDelayService(site).equalsIgnoreCase(service)) {
				return site;
			}
		}
		return "";
	}

	private long getLastVotesTime(String uuid, ArrayList<Column> cols, String site, String service) {
		long mostRecentTime = 0;

		// Check cached online votes
		if (cachedOnlineVotes.containsKey(uuid)) {
			ArrayList<OfflineBungeeVote> onlineVotes = cachedOnlineVotes.get(uuid);
			for (OfflineBungeeVote vote : onlineVotes) {
				if (vote.getService().equalsIgnoreCase(service)) {
					mostRecentTime = Math.max(mostRecentTime, vote.getTime());
				}
			}
		}

		// Check cached votes
		for (ArrayList<OfflineBungeeVote> votes : cachedVotes.values()) {
			for (OfflineBungeeVote vote : votes) {
				if (vote.getUuid().equals(uuid) && vote.getService().equalsIgnoreCase(service)) {
					mostRecentTime = Math.max(mostRecentTime, vote.getTime());
				}
			}
		}

		for (Column d : cols) {
			if (d.getName().equalsIgnoreCase("LastVotes")) {

				DataValue value = d.getValue();
				String[] list = value.getString().split("%line%");
				for (String str : list) {
					String[] data = str.split("//");
					if (data[0].equalsIgnoreCase(site)) {
						mostRecentTime = Math.max(mostRecentTime, Long.valueOf(data[1]));
					}
				}

			}
		}
		return mostRecentTime;
	}

	public boolean checkVoteDelay(String uuid, String service, ArrayList<Column> data) {
		String site = getWaitUntilDelaySiteFromService(service);
		if (site.isEmpty()) {
			debug("No service site set for " + service + ", skipping vote delay check");
			return true;
		}

		int voteDelay = getConfig().getWaitUntilVoteDelayVoteDelay(site);
		int voteDelayMin = getConfig().getWaitUntilVoteDelayVoteDelayMin(site);

		long lastVote = getLastVotesTime(uuid, data, site, service);
		if (lastVote == 0) {
			debug("No last vote time found for " + uuid + "/" + service + ", skipping vote delay check");
			return true;
		}

		try {
			LocalDateTime now = getBungeeTimeChecker().getTime();
			LocalDateTime lastVoteTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(lastVote), ZoneId.systemDefault())
					.plusHours(getConfig().getTimeHourOffSet());

			if (!getConfig().getWaitUntilVoteDelayVoteDelayDaily(site)) {
				if (voteDelay == 0 && voteDelayMin == 0) {
					debug("Vote delay is 0 for " + site + ", skipping vote delay check");
					return true;
				}

				LocalDateTime nextvote = lastVoteTime.plusHours((long) voteDelay).plusMinutes((long) voteDelayMin);

				return now.isAfter(nextvote);
			}
			LocalDateTime resetTime = lastVoteTime.withHour(getConfig().getWaitUntilVoteDelayVoteDelayHour(site))
					.withMinute(0).withSecond(0);
			LocalDateTime resetTimeTomorrow = resetTime.plusHours(24);

			if (lastVoteTime.isBefore(resetTime)) {
				if (now.isAfter(resetTime)) {
					debug("Vote delay is met for " + uuid + "/" + service + ", vote can be processed");
					return true;
				}
			} else {
				if (now.isAfter(resetTimeTomorrow)) {
					debug("Vote delay is met for " + uuid + "/" + service + ", vote can be processed");
					return true;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		debug("Vote delay is not met for " + uuid + "/" + service + ", skipping vote");
		return false;
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
				if (uuid.isEmpty() && !getConfig().getBedrockPlayerPrefix().isEmpty()
						&& !player.startsWith(getConfig().getBedrockPlayerPrefix())) {
					String uuid1 = getUUID(getConfig().getBedrockPlayerPrefix() + player);
					if (!uuid1.isEmpty()) {
						debug("Detected bedrock player without prefix, adjusting...");
						player = getConfig().getBedrockPlayerPrefix() + player;
						uuid = uuid1;
					}
				}
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

					if (!checkVoteDelay(uuid, service, data)) {
						log("Vote delay is not met for " + player + "/" + service + ", skipping vote");
						return;
					}

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

			if (getConfig().getSendVotesToAllServers()) {
				for (String s : getAllAvailableServers()) {
					boolean forceCache = false;
					if (!isPlayerOnline(player) && getConfig().getWaitForUserOnline()) {
						forceCache = true;
						debug("Forcing vote to cache");
					}
					if (getConfig().getBroadcast()) {
						globalMessageProxyHandler.sendMessage(s, "VoteBroadcast", uuid, player, service);
					}
					if ((!isSomeoneOnlineServer(s) && method.requiresPlayerOnline()) || forceCache) {
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
						globalMessageProxyHandler.sendMessage(s, "Vote", player, uuid, service, "" + time,
								Boolean.TRUE.toString(), "" + realVote, text.toString(),
								"" + getConfig().getBungeeManageTotals(), "" + BungeeVersion.getPluginMessageVersion(),
								"" + getConfig().getBroadcast(), "1", "1");
					}

				}
			} else {
				if (isPlayerOnline(player) && getAllAvailableServers().contains(getCurrentPlayerServer(player))) {
					globalMessageProxyHandler.sendMessage(getCurrentPlayerServer(player), "VoteOnline", player, uuid,
							service, "" + time, Boolean.TRUE.toString(), "" + realVote, text.toString(),
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
						globalMessageProxyHandler.sendMessage(s, "VoteBroadcast", uuid, player, service);
					}
					globalMessageProxyHandler.sendMessage(s, "VoteUpdate", uuid, "" + votePartyVotes,
							"" + currentVotePartyVotesRequired, text.toString(), service, "" + time);

				}
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
