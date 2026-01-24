// File: com/bencodez/votingplugin/proxy/VotingPluginProxy.java
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
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.time.BungeeTimeChecker;
import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.json.JsonParser;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
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
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage;
import com.bencodez.votingplugin.proxy.cache.nonvoted.NonVotedPlayersCache;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyMethod;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfiguration;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfigurationBungee;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogStatus;
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
	private MqttHandler mqttHandler;

	@Getter
	private GlobalMessageProxyHandler globalMessageProxyHandler;

	@Getter
	private MySqlMessenger proxyMysqlMessenger;

	@Getter
	private VoteCacheHandler voteCacheHandler;

	@Getter
	private NonVotedPlayersCache nonVotedPlayersCache;

	public VotingPluginProxy() {
		enabled = true;

		bungeeTimeChecker = new BungeeTimeChecker(getConfig().getTimeZone(), getConfig().getTimeHourOffSet(),
				getConfig().getTimeWeekOffSet()) {

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
					getVoteCacheHandler().checkVoteCacheTime(getConfig().getVoteCacheTime());
				}
				if (!getConfig().getGlobalDataEnabled()) {
					warn("Global data not enabled, ignoring time change event");
					return;
				}
				int delay = 1;
				for (String s : getAllAvailableServers()) {
					if (getGlobalDataHandler().getGlobalMysql().containsKey(s)) {
						String lastOnlineStr = getGlobalDataHandler().getString(s, "LastOnline");
						long lastOnline = 0;
						try {
							lastOnline = Long.valueOf(lastOnlineStr);
						} catch (NumberFormatException e) {
							// ignore
						}

						if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastOnline < 1000
								* 60 * 60 * 12) {
							HashMap<String, DataValue> dataToSet = new HashMap<>();
							dataToSet.put("LastUpdated", new DataValueString(
									"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()));
							dataToSet.put("FinishedProcessing", new DataValueBoolean(false));
							dataToSet.put(type.toString(), new DataValueBoolean(true));
							getGlobalDataHandler().setData(s, dataToSet);

							globalMessageProxyHandler.sendMessage(s, delay, VotingPluginWire.bungeeTimeChange());
							delay++;
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

	public void addNonVotedPlayer(String uuid, String playerName) {
		nonVotedPlayersCache.addPlayer(uuid, playerName);
	}

	public void addVoteParty() {
		if (getConfig().getVotePartyEnabled()) {
			addCurrentVotePartyVotes(1);
			checkVoteParty();
		}
	}

	public abstract void broadcast(String message);

	public synchronized void checkCachedVotes(String server) {
		int delay = 1;
		if (isServerValid(server)) {
			if (isSomeoneOnlineServer(server)) {
				if (getVoteCacheHandler().hasVotes(server) && !getConfig().getBlockedServers().contains(server)) {
					ArrayList<OfflineBungeeVote> c = getVoteCacheHandler().getVotes(server);
					ArrayList<OfflineBungeeVote> removed = new ArrayList<>();
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
								globalMessageProxyHandler.sendMessage(server, delay,
										VotingPluginWire.vote(cache.getPlayerName(), cache.getUuid(),
												cache.getService(), cache.getTime(), false, cache.isRealVote(),
												cache.getText(), getConfig().getBungeeManageTotals(),
												getConfig().getBroadcast(), num, numberOfVotes));
								delay++;
								num++;
								removed.add(cache);
							} else {
								debug("Not sending vote because user isn't on server " + server + ": "
										+ cache.toString());
							}
						}
						getVoteCacheHandler().removeServerVotes(server, removed);
					} else {
						debug("No cached votes for server: " + server);
					}
				} else {
					debug("No cached votes for server: " + server);
				}
			}
		} else {
			debug("Server not valid: " + server);
		}
	}

	public synchronized void checkOnlineVotes(String player, String uuid, String server) {
		int delay = 1;
		if (isPlayerOnline(player) && getVoteCacheHandler().hasOnlineVotes(uuid)) {
			ArrayList<OfflineBungeeVote> c = getVoteCacheHandler().getOnlineVotes(uuid);
			if (!c.isEmpty()) {
				if (server == null) {
					server = getCurrentPlayerServer(player);
				}
				if (!getConfig().getBlockedServers().contains(server)) {
					int num = 1;
					int numberOfVotes = c.size();
					for (OfflineBungeeVote cache : c) {
						globalMessageProxyHandler.sendMessage(server, delay,
								VotingPluginWire.voteOnline(cache.getPlayerName(), cache.getUuid(), cache.getService(),
										cache.getTime(), false, cache.isRealVote(), cache.getText(),
										getConfig().getBungeeManageTotals(), getConfig().getBroadcast(), num,
										numberOfVotes));
						delay++;
						num++;
					}
					getVoteCacheHandler().removeOnlineVotes(uuid);

					// multiproxy: envelope-only
					if (getConfig().getMultiProxySupport() && getConfig().getMultiProxyOneGlobalReward()) {
						multiProxyHandler.sendClearVote(uuid, player);
					}
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
		URL url = new URL("https://api.mojang.com/users/profiles/minecraft/" + playerName);
		HttpURLConnection connection = (HttpURLConnection) url.openConnection();
		connection.connect();

		if (connection.getResponseCode() == 400) {
			log("There is no player with the name \"" + playerName + "\"!");
			return null;
		}

		InputStream inputStream = connection.getInputStream();
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

		JsonElement element = JsonParser.parseReader(bufferedReader);
		JsonObject object = element.getAsJsonObject();
		String uuidAsString = object.get("id").getAsString();

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
				if (value == null) {
					return toAdd;
				}
				if (value.isInt()) {
					num = value.getInt();
				} else if (value.isString()) {
					try {
						num = Integer.parseInt(value.getString());
					} catch (Exception e) {
						// ignore
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

	public abstract MysqlConfig getVoteCacheMySQLConfig();

	public abstract MysqlConfig getNonVotedCacheMySQLConfig();

	public abstract MysqlConfig getVoteLoggingMySQLConfig();

	public void load(IVoteCache jsonStorage, INonVotedPlayersStorage nonVotedCacheJson) {
		uuidPlayerNameCache = getProxyMySQL().getRowsUUIDNameQuery();

		bungeeTimeChecker.setTimeChangeFailSafeBypass(getConfig().getTimeChangeFailSafeBypass());
		bungeeTimeChecker.loadTimer();

		voteCacheHandler = new VoteCacheHandler(getVoteCacheMySQLConfig(), getConfig().getVoteCacheUseMySQL(),
				getConfig().getVoteCacheUseMainMySQL(), getProxyMySQL().getMysql(), getConfig().getDebug(),
				jsonStorage) {

			@Override
			public void logInfo1(String msg) {
				logInfo(msg);
			}

			@Override
			public void logSevere1(String msg) {
				logSevere(msg);
			}

			@Override
			public void debug1(Exception e) {
				if (getConfig().getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void debug1(String msg) {
				if (getConfig().getDebug()) {
					debug(msg);
				}
			}

			@Override
			public void debug1(Throwable e) {
				if (getConfig().getDebug()) {
					e.printStackTrace();
				}
			}
		};
		voteCacheHandler.load();

		nonVotedPlayersCache = new NonVotedPlayersCache(getNonVotedCacheMySQLConfig(),
				getConfig().getNonVotedCacheUseMySQL(), getConfig().getNonVotedCacheUseMainMySQL(),
				getProxyMySQL().getMysql(), nonVotedCacheJson, getConfig().getDebug()) {

			@Override
			public boolean userExists(String uuid) {
				return getProxyMySQL().containsKeyQuery(uuid);
			}

			@Override
			public void logInfo1(String msg) {
				logInfo(msg);
			}

			@Override
			public void logSevere1(String msg) {
				logSevere(msg);
			}

			@Override
			public void debug1(Exception e) {
				if (getConfig().getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void debug1(String msg) {
				if (getConfig().getDebug()) {
					debug(msg);
				}
			}

			@Override
			public Set<String> getAllUUIDs() {
				return getProxyMySQL().getUuids();
			}
		};

		method = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (getMethod() == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}

		if (method.equals(BungeeMethod.MYSQL)) {
			try {
				proxyMysqlMessenger = new MySqlMessenger("VotingPlugin",
						getProxyMySQL().getMysql().getConnectionManager().getDataSource(), MySqlMessenger.Mode.PROXY,
						null, // no serverId in PROXY mode
						msg -> {
							if (getConfig().getDebug()) {
								debug("Got from " + msg.source + ": " + msg.envelope.getSubChannel() + " "
										+ msg.envelope.getFields());
							}
							globalMessageProxyHandler.onMessage(msg.envelope);
						});
			} catch (SQLException e) {
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
				public void onReceiveEnvelope(JsonEnvelope envelope) {
					globalMessageProxyHandler.onMessage(envelope);
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
					getConfig().getRedisUsername(), getConfig().getRedisPassword(), getConfig().getRedisDbIndex()) {

				@Override
				public void debug(String message) {
					debug2(message);
				}
			};

			runAsync(() -> {
				RedisListener listener = redisHandler.createEnvelopeListener(
						getConfig().getRedisPrefix() + "VotingPlugin",
						(ch, env) -> globalMessageProxyHandler.onMessage(env));
				redisHandler.loadListener(listener);
			});

		} else if (method.equals(BungeeMethod.MQTT)) {
			try {
				mqttHandler = new MqttHandler(new MqttServerComm(getConfig().getMqttClientID(),
						getConfig().getMqttBrokerURL(), getConfig().getMqttUsername(), getConfig().getMqttPassword()),
						2);

				mqttHandler.subscribeEnvelopes(getConfig().getMqttPrefix() + "votingplugin/servers/proxy",
						(topic, env) -> globalMessageProxyHandler.onMessage(env));

			} catch (MqttException e) {
				e.printStackTrace();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		currentVotePartyVotesRequired = getConfig().getVotePartyVotesRequired()
				+ getVoteCacheVotePartyIncreaseVotesRequired();
		votePartyVotes = getVoteCacheCurrentVotePartyVotes();

		globalMessageProxyHandler = new GlobalMessageProxyHandler() {
			@Override
			public void sendMessage(String server, int delay, JsonEnvelope envelope) {
				switch (method) {
				case MQTT:
					sendMqttEnvelopeServer(server, envelope);
					break;
				case MYSQL:
					try {
						proxyMysqlMessenger.sendToBackend(server, envelope);
					} catch (SQLException e) {
						e.printStackTrace();
					}
					break;
				case PLUGINMESSAGING:
					sendPluginMessageServer(server, delay, envelope);
					break;
				case REDIS:
					sendRedisEnvelopeServer(server, envelope);
					break;
				case SOCKETS:
					sendSocketEnvelopeServer(server, envelope);
					break;
				default:
					break;
				}
			}
		};

		globalMessageProxyHandler.addListener(new GlobalMessageListener("login") {
			@Override
			public void onReceive(JsonEnvelope message) {
				Map<String, String> f = message.getFields();
				String player = f.getOrDefault("player", "");
				String uuid = f.getOrDefault("uuid", "");
				String server = f.getOrDefault("server", "");

				if (player.isEmpty() || uuid.isEmpty()) {
					logSevere("Invalid login envelope received: " + message.getFields());
					return;
				}

				debug("Login: " + player + "/" + uuid + " " + server);
				login(player, uuid, server);
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener("statusokay") {
			@Override
			public void onReceive(JsonEnvelope message) {
				String server = message.getFields().getOrDefault("server", "");
				log("Status okay for " + server);
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener("voteupdate") {
			@Override
			public void onReceive(JsonEnvelope message) {
				int delay = 1;
				for (String send : getAllAvailableServers()) {
					globalMessageProxyHandler.sendMessage(send, delay, message);
					delay++;
				}
			}
		});

		loadMultiProxySupport();
		loadVoteLoggingMySQL();

		debug("VotingPluginProxy loaded, ONLINEMODE: " + getConfig().getOnlineMode());
	}

	private VoteLogMysqlTable voteLogMysqlTable;

	public void loadVoteLoggingMySQL() {
		if (getConfig().getVoteLoggingEnabled()) {
			if (getConfig().getVoteLoggingUseMainMySQL()) {
				voteLogMysqlTable = new VoteLogMysqlTable("votingplugin_votelog", getProxyMySQL().getMysql(),
						getVoteLoggingMySQLConfig(), getConfig().getDebug()) {

					@Override
					public void logSevere(String string) {
						VotingPluginProxy.this.logSevere(string);
					}

					@Override
					public void logInfo(String string) {
						VotingPluginProxy.this.logInfo(string);
					}

					@Override
					public void debug(Throwable e) {
						if (getConfig().getDebug()) {
							e.printStackTrace();
						}
					}

					@Override
					public String getServerName() {
						return "Proxy";
					}
				};
			} else {
				voteLogMysqlTable = new VoteLogMysqlTable("votingplugin_votelog", getVoteLoggingMySQLConfig(),
						getConfig().getDebug()) {

					@Override
					public void logSevere(String string) {
						VotingPluginProxy.this.logSevere(string);
					}

					@Override
					public void logInfo(String string) {
						VotingPluginProxy.this.logInfo(string);
					}

					@Override
					public void debug(Throwable e) {
						if (getConfig().getDebug()) {
							e.printStackTrace();
						}
					}

					@Override
					public String getServerName() {
						return "Proxy";
					}
				};
			}

			if (getConfig().getVoteLoggingPurgeDays() > 0) {
				loadTaskTimer(() -> voteLogMysqlTable.purgeOlderThanDays(getConfig().getVoteLoggingPurgeDays(), 100),
						60, 60 * 60);
			}

			debug("Vote logging MySQL enabled");
		} else {
			debug("Vote logging MySQL disabled");
		}
	}

	public abstract void loadTaskTimer(Runnable runnable, long delaySeconds, long repeatSeconds);

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
			public void clearVote(String uuid) {
				getVoteCacheHandler().removeOnlineVotes(uuid);
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
			public int getMultiProxyRedisDbIndex() {
				return getConfig().getMultiProxyRedisDbIndex();
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
					VoteTotalsSnapshot text, String uuid) {
				vote(player, service, realVote, timeQueue, queueTime, text, uuid);
			}
		};
		multiProxyHandler.loadMultiProxySupport();
	}

	public abstract void log(String message);

	public void login(String playerName, String uuid, String serverName) {
		if (!getConfig().getOnlineMode()) {
			uuid = getUUID(playerName);
		}

		try {
			if (uuid != null && !uuid.isEmpty() && !uuid.equalsIgnoreCase("null")) {
				uuid = UUID.fromString(uuid.trim()).toString();
			}
		} catch (Exception ignored) {
			// ignore
		}

		if (getConfig().getOnlineMode()) {
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
		getVoteCacheHandler().saveVoteCache();

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
		runAsync(() -> {
			try {
				final String headerSub;
				if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
					headerSub = encryptionHandler.decrypt(in.readUTF());
				} else {
					headerSub = in.readUTF();
				}

				int size = in.readInt(); // sanity only

				if (getConfig().getDebug()) {
					debug("Received plugin message header=" + headerSub + " size=" + size);
				}

				String payload = "";
				if (size > 0) {
					if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
						payload = encryptionHandler.decrypt(in.readUTF());
					} else {
						payload = in.readUTF();
					}
				}

				JsonEnvelope envelope = JsonEnvelopeCodec.decode(payload);

				if (!headerSub.equalsIgnoreCase(envelope.getSubChannel())) {
					if (getConfig().getDebug()) {
						warn("PluginMessage subChannel mismatch: header=" + headerSub + " env="
								+ envelope.getSubChannel());
					}
					return;
				}

				globalMessageProxyHandler.onMessage(envelope);
			} catch (Exception e) {
				e.printStackTrace();
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
		while (getVoteCacheHandler().getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getVoteCacheHandler().getTimeChangeQueue().remove();
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

	public abstract void sendPluginMessageData(String server, String channel, byte[] data, boolean queue);

	private static final int PLUGIN_MESSAGE_HARD_LIMIT = 32767;
	private static final int PLUGIN_MESSAGE_SOFT_LIMIT = 30000;

	public void sendPluginMessageServer(String server, int delay, JsonEnvelope envelope) {
		getScheduler().schedule(() -> {
			final String subChannel = envelope.getSubChannel();
			final String payload = JsonEnvelopeCodec.encode(envelope);

			final byte[] subChannelBytes = subChannel.getBytes(java.nio.charset.StandardCharsets.UTF_8);
			final byte[] payloadBytes = payload.getBytes(java.nio.charset.StandardCharsets.UTF_8);

			// Estimate bytes written:
			// - writeUTF adds 2-byte length prefix + UTF-8 bytes
			// - writeInt is 4 bytes
			int estimatedSize = 2 + subChannelBytes.length + // subChannel UTF (len prefix + bytes)
					4 + // payload length int
					2 + payloadBytes.length; // payload UTF (len prefix + bytes)

			if (estimatedSize > PLUGIN_MESSAGE_SOFT_LIMIT) {
				debug("[PluginMessage] Payload nearing limit (" + estimatedSize + " bytes) server=" + server
						+ " subChannel=" + subChannel + " — consider Redis instead");
			}

			if (estimatedSize > PLUGIN_MESSAGE_HARD_LIMIT) {
				debug("[PluginMessage] Payload TOO LARGE (" + estimatedSize + " bytes, max=" + PLUGIN_MESSAGE_HARD_LIMIT
						+ ") server=" + server + " subChannel=" + subChannel + " — NOT sent");
				return;
			}

			ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
			DataOutputStream out = new DataOutputStream(byteOutStream);

			try {
				if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
					out.writeUTF(encryptionHandler.encrypt(subChannel));
				} else {
					out.writeUTF(subChannel);
				}

				// sanity only: MUST be bytes, not chars
				out.writeInt(payloadBytes.length);

				if (getConfig().getPluginMessageEncryption() && encryptionHandler != null) {
					out.writeUTF(encryptionHandler.encrypt(payload));
				} else {
					out.writeUTF(payload);
				}

				if (isSomeoneOnlineServer(server)) {
					sendPluginMessageData(server, getConfig().getPluginMessageChannel().toLowerCase(),
							byteOutStream.toByteArray(), false);
				}

				out.close();
			} catch (Exception e) {
				e.printStackTrace();
			}

			if (getConfig().getDebug()) {
				debug("Sending plugin envelope (" + estimatedSize + " bytes) " + server + " " + subChannel + " "
						+ envelope.getFields());
			}
		}, delay * 5L, TimeUnit.MILLISECONDS);
	}

	public void sendRedisEnvelopeServer(String server, JsonEnvelope envelope) {
		redisHandler.publishEnvelope(getConfig().getRedisPrefix() + "VotingPlugin_" + server, envelope);
	}

	public void sendMqttEnvelopeServer(String server, JsonEnvelope envelope) {
		try {
			mqttHandler.publishEnvelope(getConfig().getMqttPrefix() + "votingplugin/servers/" + server, envelope);
		} catch (Exception e) {
			if (getConfig().getDebug()) {
				e.printStackTrace();
			}
		}
	}

	public void sendSocketEnvelopeServer(String server, JsonEnvelope envelope) {
		if (clientHandles == null) {
			return;
		}
		if (clientHandles.containsKey(server)) {
			clientHandles.get(server).sendEnvelope(envelope);
		}
	}

	public void sendServerNameMessage() {
		for (String s : getAllAvailableServers()) {
			sendPluginMessageServer(s, 1, VotingPluginWire.serverName(s));
		}
	}

	public void sendVoteParty(String server) {
		if (isSomeoneOnlineServer(server)) {
			globalMessageProxyHandler.sendMessage(server, 1, VotingPluginWire.votePartyBungee());
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
				log("Sending request for status message on " + s);
				globalMessageProxyHandler.sendMessage(s, 1, VotingPluginWire.status(s));
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

		if (getVoteCacheHandler().hasOnlineVotes(uuid)) {
			ArrayList<OfflineBungeeVote> onlineVotes = getVoteCacheHandler().getOnlineVotes(uuid);
			for (OfflineBungeeVote vote : onlineVotes) {
				if (vote.getService().equalsIgnoreCase(service)) {
					mostRecentTime = Math.max(mostRecentTime, vote.getTime());
				}
			}
		}

		for (String server : getAllAvailableServers()) {
			for (OfflineBungeeVote vote : getVoteCacheHandler().getVotes(server)) {
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
			VoteTotalsSnapshot text, String uuid) {
		try {
			if (player == null || player.isEmpty()) {
				log("No name from vote on " + service);
				return;
			}
			if (getConfig().getGlobalDataEnabled()) {
				if (getGlobalDataHandler().isTimeChangedHappened()) {
					getGlobalDataHandler().checkForFinishedTimeChanges();
					if (timeQueue && getGlobalDataHandler().isTimeChangedHappened()) {
						getVoteCacheHandler().getTimeChangeQueue().add(new VoteTimeQueue(player, service,
								LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
						log("Cachcing vote from " + player + "/" + service
								+ " because time change is happening right now");
						return;
					}
				}
			}

			if (!getConfig().getOnlineMode()) {
				uuid = getUUID(player);
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

			try {
				if (uuid != null && !uuid.isEmpty() && !uuid.equalsIgnoreCase("null")) {
					uuid = UUID.fromString(uuid.trim()).toString();
				}
			} catch (Exception ignored) {
				// ignore
			}

			player = getProperName(uuid, player);

			UUID voteId = UUID.randomUUID();

			addVoteParty();
			if (getConfig().getPrimaryServer() || !getConfig().getMultiProxySupport()) {
				if (getConfig().getBungeeManageTotals()) {

					if (getProxyMySQL() == null) {
						logSevere("Mysql is not loaded correctly, stopping vote processing");
						return;
					}

					if (!getProxyMySQL().containsKeyQuery(uuid)) {
						getProxyMySQL().update(uuid, "PlayerName", new DataValueString(player));
						getProxyMySQL().getUuids().add(uuid);
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
					text = new VoteTotalsSnapshot(allTimeTotal, monthTotal, weeklyTotal, dailyTotal, points,
							votePartyVotes, currentVotePartyVotesRequired, dateMonthTotal, voteId);

					ArrayList<Column> update = new ArrayList<>();
					update.add(new Column("AllTimeTotal", new DataValueInt(allTimeTotal)));
					update.add(new Column("MonthTotal", new DataValueInt(monthTotal)));
					if (getConfig().getStoreMonthTotalsWithDate()) {
						update.add(new Column(getMonthTotalsWithDatePath(), new DataValueInt(dateMonthTotal)));
					}
					update.add(new Column("WeeklyTotal", new DataValueInt(weeklyTotal)));
					update.add(new Column("DailyTotal", new DataValueInt(dailyTotal)));
					update.add(new Column("Points", new DataValueInt(points)));

					debug("Setting totals " + text.toString());
					getProxyMySQL().update(uuid, update);
				} else {
					text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0,
							voteId);
				}
			}

			long time = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
			if (queueTime != 0) {
				time = queueTime;
			}

			VoteLogStatus voteStatus = VoteLogStatus.IMMEDIATE;

			if (getConfig().getSendVotesToAllServers()) {
				for (String s : getAllAvailableServers()) {
					boolean forceCache = false;
					if (!isPlayerOnline(player) && getConfig().getWaitForUserOnline()) {
						forceCache = true;
						debug("Forcing vote to cache");
					}
					if (getConfig().getBroadcast()) {
						globalMessageProxyHandler.sendMessage(s, 1,
								VotingPluginWire.voteBroadcast(uuid, player, service));
					}
					if ((!isSomeoneOnlineServer(s) && method.requiresPlayerOnline()) || forceCache) {
						voteStatus = VoteLogStatus.CACHED;
						getVoteCacheHandler().addServerVote(s,
								new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString()));

						debug("Caching vote for " + player + " on " + service + " for " + s);

					} else {
						globalMessageProxyHandler.sendMessage(s, 2,
								VotingPluginWire.vote(player, uuid, service, time, true, realVote, text.toString(),
										getConfig().getBungeeManageTotals(), getConfig().getBroadcast(), 1, 1));
					}
				}
			} else {
				if (isPlayerOnline(player) && getAllAvailableServers().contains(getCurrentPlayerServer(player))) {
					globalMessageProxyHandler.sendMessage(getCurrentPlayerServer(player), 1,
							VotingPluginWire.voteOnline(player, uuid, service, time, true, realVote, text.toString(),
									getConfig().getBungeeManageTotals(), getConfig().getBroadcast(), 1, 1));

					// multiproxy: envelope-only clear vote
					if (getConfig().getMultiProxySupport() && getConfig().getMultiProxyOneGlobalReward()) {
						multiProxyHandler.sendClearVote(uuid, player);
					}
				} else {
					voteStatus = VoteLogStatus.CACHED;
					getVoteCacheHandler().addOnlineVote(uuid,
							new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString()));
					debug("Caching online vote for " + player + " on " + service);
				}
				int delay = 2;
				for (String s : getAllAvailableServers()) {
					if (getConfig().getBroadcast()) {
						globalMessageProxyHandler.sendMessage(s, delay,
								VotingPluginWire.voteBroadcast(uuid, player, service));
					}
					globalMessageProxyHandler.sendMessage(s, delay + 1, VotingPluginWire.voteUpdate(uuid,
							votePartyVotes, currentVotePartyVotesRequired, service, time, text.toString()));
					delay += 2;
				}
			}

			if (voteLogMysqlTable != null && getConfig().getVoteLoggingEnabled()) {
				voteLogMysqlTable.logVote(voteId, voteStatus, service, uuid, player, time,
						getVoteCacheHandler().getProxyCachedTotal(uuid));
			}

			// === UPDATED (multiproxy): send JsonEnvelope only ===
			if (getConfig().getMultiProxySupport() && getConfig().getPrimaryServer()) {
				if (!getConfig().getMultiProxyOneGlobalReward()) {
					debug("Sending global proxy vote envelope");
					multiProxyHandler.sendMultiProxyEnvelope(VotingPluginWire.vote(player, uuid, service, time, false,
							realVote, text == null ? "" : text.toString(), false, false, 1, 1));
				} else {
					if (!(isPlayerOnline(player)
							&& !getConfig().getBlockedServers().contains(getCurrentPlayerServer(player)))) {
						debug("Sending global proxy voteonline envelope");
						multiProxyHandler.sendMultiProxyEnvelope(VotingPluginWire.voteOnline(player, uuid, service,
								time, false, realVote, text == null ? "" : text.toString(), false, false, 1, 1));
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

	public abstract ScheduledExecutorService getScheduler();
}
