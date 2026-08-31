package com.bencodez.votingplugin.proxy;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.SQLException;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import javax.net.ssl.SSLParameters;

import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
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
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.votingplugin.backendproxy.http.HttpEnrollmentAuthority;
import com.bencodez.votingplugin.backendproxy.http.HttpProxyTransportServer;
import com.bencodez.votingplugin.backendproxy.http.HttpTlsIdentity;
import com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage;
import com.bencodez.votingplugin.proxy.cache.nonvoted.NonVotedPlayersCache;
import com.bencodez.votingplugin.proxy.control.ControlConnector;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyMethod;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfiguration;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyServerSocketConfigurationBungee;
import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker;
import com.bencodez.votingplugin.proxy.presence.PlayerPresence;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogStatus;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import redis.clients.jedis.DefaultJedisClientConfig;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;

import lombok.Getter;
import lombok.Setter;

public abstract class VotingPluginProxy {
	private static final long PRESENCE_HANDOFF_TIMEOUT_MILLIS = TimeUnit.MINUTES.toMillis(2);
	private static final long PRESENCE_STARTUP_RESYNC_DELAY_SECONDS = 5L;
	private static final long PRESENCE_MAINTENANCE_INTERVAL_SECONDS = 30L;
	private static final long PRESENCE_BACKEND_TIMEOUT_MILLIS = TimeUnit.SECONDS.toMillis(90);
	private static final long CONTROL_ENROLLMENT_MIN_INTERVAL_NANOS = TimeUnit.SECONDS.toNanos(10);

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
	private HttpProxyTransportServer httpTransportServer;
	private HttpEnrollmentAuthority httpEnrollmentAuthority;

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
	private JedisPool redisPublisherPool;
	private volatile long redisPublisherRetryAfter;
	private boolean timeVoteRetryScheduled;
	private boolean timeVoteDeliveryRetryScheduled;
	private boolean cachedVoteDeliveryRetryScheduled;

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
	@Setter
	private MySqlMessenger proxyMysqlMessenger;

	@Getter
	private VoteCacheHandler voteCacheHandler;

	@Getter
	private NonVotedPlayersCache nonVotedPlayersCache;

	@Getter
	private final BackendPlayerPresenceTracker backendPlayerPresenceTracker = new BackendPlayerPresenceTracker();
	private final Map<UUID, PendingPresenceHandoff> pendingPresenceHandoffs = new HashMap<>();
	private final Set<String> pendingBackendRecoverySnapshots = ConcurrentHashMap.newKeySet();
	private final Map<String, Long> controlEnrollmentNextAllowed = new ConcurrentHashMap<>();
	private final Map<UUID, PendingCommunicationTest> pendingCommunicationTests = new ConcurrentHashMap<>();
	private volatile ControlConnector controlConnector;
	private volatile HostedControlManager hostedControlManager;
	private final Object controlLifecycleLock = new Object();
	private final AtomicLong controlServicesGeneration = new AtomicLong();
	private final ExecutorService controlLifecycleExecutor = Executors.newSingleThreadExecutor(task -> {
		Thread thread = new Thread(task, "votingplugin-control-lifecycle");
		thread.setDaemon(true);
		return thread;
	});

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

	public void onTimeChangedFailed(String srv, TimeType type) {
		getGlobalDataHandler().setBoolean(srv, type.toString(), false);
		getGlobalDataHandler().setBoolean(srv, "FinishedProcessing", true);
		getGlobalDataHandler().setBoolean(srv, "Processing", false);
	}

	public void onTimeChangedFinished(TimeType type) {
		if (type.equals(TimeType.MONTH)) {
			getProxyMySQL().copyColumnData(TopVoter.Monthly.getColumnName(), "LastMonthTotal");
		}
		getProxyMySQL().wipeColumnData(TopVoter.of(type).getColumnName(), DataType.INTEGER);

		if (!getConfig().getGlobalDataEnabled()) {
			return;
		}
		for (String s : getAllAvailableServers()) {
			getGlobalDataHandler().setBoolean(s, "ForceUpdate", true);
			getGlobalMessageProxyHandler().sendMessage(s, 1, VotingPluginWire.bungeeTimeChange());
		}
		processQueue();
	}

	/**
	 * Load MySQL + global data handler.
	 */
	public void loadMysql(MysqlConfig mysqlConfig, MysqlConfig globalDataMysqlConfig) {
		if (mysqlConfig.getHostName().isEmpty() || mysqlConfig.getDatabase().isEmpty()) {
			logSevere("MySQL is not configured correctly. " + "Missing host/database. host=" + mysqlConfig.getHostName()
					+ " db=" + mysqlConfig.getDatabase());
			setProxyMySQL(null);
			return;
		}

		setProxyMySQL(new ProxyMysqlUserTable("VotingPlugin_Users", mysqlConfig, getConfig().getDebug()) {

			@Override
			public void debug(SQLException e) {
				if (getConfig().getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void logSevere(String string) {
				VotingPluginProxy.this.logSevere(string);
			}

			@Override
			public void logInfo(String string) {
				VotingPluginProxy.this.logInfo(string);
			}

			@Override
			public void debug(Throwable t) {
				if (getConfig().getDebug()) {
					t.printStackTrace();
				}
			}

			@Override
			public void debug(String str) {
				debug2(str);
			}
		});

		ArrayList<String> servers = new ArrayList<String>(getAllAvailableServers());

		if (getConfig().getGlobalDataEnabled()) {
			if (getConfig().getGlobalDataUseMainMySQL()) {
				setGlobalDataHandler(new GlobalDataHandlerProxy(
						new GlobalMySQL("VotingPlugin_GlobalData", getProxyMySQL().getMysql()) {

							@Override
							public void debugEx(Exception e) {
								if (getConfig().getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debugLog(String text) {
								debug(text);
							}

							@Override
							public void info(String text) {
								logInfo(text);
							}

							@Override
							public void logSevere(String text) {
								VotingPluginProxy.this.logSevere(text);
							}

							@Override
							public void warning(String text) {
								warn(text);
							}
						}, servers) {

					@Override
					public void onTimeChangedFailed(String srv, TimeType type) {
						VotingPluginProxy.this.onTimeChangedFailed(srv, type);
					}

					@Override
					public void onTimeChangedFinished(TimeType type) {
						VotingPluginProxy.this.onTimeChangedFinished(type);
					}
				});
			} else {
				setGlobalDataHandler(
						new GlobalDataHandlerProxy(new GlobalMySQL("VotingPlugin_GlobalData", globalDataMysqlConfig) {

							@Override
							public void debugEx(Exception e) {
								if (getConfig().getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debugLog(String text) {
								debug(text);
							}

							@Override
							public void info(String text) {
								logInfo(text);
							}

							@Override
							public void logSevere(String text) {
								VotingPluginProxy.this.logSevere(text);
							}

							@Override
							public void warning(String text) {
								warn(text);
							}
						}, servers) {

							@Override
							public void onTimeChangedFailed(String srv, TimeType type) {
								VotingPluginProxy.this.onTimeChangedFailed(srv, type);
							}

							@Override
							public void onTimeChangedFinished(TimeType type) {
								VotingPluginProxy.this.onTimeChangedFinished(type);
							}
						});
			}

			// update global schema columns (unchanged from original)
			getGlobalDataHandler().getGlobalMysql().alterColumnType("IgnoreTime", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("MONTH", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("WEEK", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("DAY", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("FinishedProcessing", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("Processing", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("ForceUpdate", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("LastUpdated", "MEDIUMTEXT");
		}

		// column types (unchanged from original)
		getProxyMySQL().alterColumnType("TopVoterIgnore", "VARCHAR(5)");
		getProxyMySQL().alterColumnType("CheckWorld", "VARCHAR(5)");
		getProxyMySQL().alterColumnType("Reminded", "VARCHAR(5)");
		getProxyMySQL().alterColumnType("DisableBroadcast", "VARCHAR(5)");
		getProxyMySQL().alterColumnType("LastOnline", "VARCHAR(20)");
		getProxyMySQL().alterColumnType("PlayerName", "VARCHAR(30)");
		getProxyMySQL().alterColumnType("DailyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("WeeklyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("DayVoteStreak", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("BestDayVoteStreak", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("WeekVoteStreak", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("BestWeekVoteStreak", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("VotePartyVotes", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("MonthVoteStreak", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("Points", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("HighestDailyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("AllTimeTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("HighestMonthlyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("MonthTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("HighestWeeklyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("LastMonthTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("LastWeeklyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("LastDailyTotal", "INT DEFAULT '0'");
		getProxyMySQL().alterColumnType("OfflineRewards", "MEDIUMTEXT");
		getProxyMySQL().alterColumnType("DayVoteStreakLastUpdate", "MEDIUMTEXT");

		if (getConfig().getStoreMonthTotalsWithDate()) {
			getProxyMySQL().alterColumnType(getMonthTotalsWithDatePath(LocalDateTime.now()), "INT DEFAULT '0'");
			getProxyMySQL().alterColumnType(getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(1)),
					"INT DEFAULT '0'");
			getProxyMySQL().alterColumnType(getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(2)),
					"INT DEFAULT '0'");
		}
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

	private Set<String> sendProxyBroadcast(Set<String> targets, String uuid, String player, String service, long time,
			String text, boolean wasOnline) {
		Set<String> forwarded = new LinkedHashSet<>();
		for (String targetServer : targets) {
			JsonEnvelope envelope = VotingPluginWire.voteBroadcast(uuid, player, service, time, text, wasOnline);
			if (sendProxyBroadcastEnvelopeNow(targetServer, envelope)) {
				forwarded.add(targetServer);
			}
		}
		return forwarded;
	}

	/**
	 * Sends a standalone proxy broadcast through the selected transport and reports
	 * whether that transport accepted the message.
	 *
	 * @param server target backend server
	 * @param envelope standalone broadcast envelope
	 * @return true only when the transport accepted the message
	 */
	protected boolean sendProxyBroadcastEnvelopeNow(String server, JsonEnvelope envelope) {
		switch (method) {
		case MQTT:
			return sendMqttEnvelopeServer(server, envelope);
		case MYSQL:
			if (proxyMysqlMessenger == null) {
				return false;
			}
			try {
				proxyMysqlMessenger.sendToBackend(server, envelope);
				return true;
			} catch (SQLException e) {
				debug(e.getMessage());
				return false;
			}
		case PLUGINMESSAGING:
			return sendPluginMessageServerNow(server, envelope);
		case REDIS:
			return sendRedisEnvelopeServer(server, envelope, true);
		case SOCKETS:
			// Standalone broadcasts use the same initialized client as normal
			// envelopes. This preserves the socket connection and its delivery
			// acknowledgement instead of creating a second short-lived socket.
			return sendSocketEnvelope(server, envelope);
		case HTTP:
			return sendHttpEnvelope(server, envelope);
		default:
			return false;
		}
	}

	/**
	 * Sends a reward-bearing vote envelope and reports whether the selected
	 * transport accepted it. Legacy transports retain their existing asynchronous
	 * semantics; HTTP exposes its bounded-queue result so a vote is never discarded
	 * when the queue is full.
	 */
	protected boolean sendVoteEnvelopeAccepted(String server, int delay, JsonEnvelope envelope) {
		if (method == BungeeMethod.HTTP) {
			return sendHttpEnvelope(server, envelope);
		}
		GlobalMessageProxyHandler handler = globalMessageProxyHandler;
		if (handler == null) {
			return false;
		}
		handler.sendMessage(server, delay, envelope);
		return true;
	}

	public synchronized void checkCachedVotes(String server) {
		int delay = 1;
		if (isServerValid(server)) {
			if (isSomeoneOnlineServerForVoteRouting(server)) {
				if (getVoteCacheHandler().hasVotes(server) && !getConfig().getBlockedServers().contains(server)) {
					ArrayList<OfflineBungeeVote> c = getVoteCacheHandler().getVotes(server);
					ArrayList<OfflineBungeeVote> removed = new ArrayList<>();
					if (!c.isEmpty()) {
						int num = 1;
						int numberOfVotes = c.size();
						for (OfflineBungeeVote cache : c) {
							if (cache.isDeliveryStateDirty() && !persistServerVoteDelivery(server, cache)) {
								continue;
							}
							if (cache.isProxyBroadcastHandled() && cache.needsBroadcastOn(server)) {
								Set<String> forwarded = sendProxyBroadcast(Collections.singleton(server),
										cache.getUuid(), cache.getPlayerName(), cache.getService(), cache.getTime(),
										cache.getText(), false);
								if (cache.getBroadcastForwardedServers().addAll(forwarded)) {
									cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
									if (!persistServerVoteDelivery(server, cache)) {
										continue;
									}
								}
							}

							boolean toSend = true;
							if (getConfig().getWaitForUserOnline()) {
								if (!isPlayerOnlineForVoteRouting(cache.getPlayerName())) {
									toSend = false;
								} else if (isPlayerOnlineForVoteRouting(cache.getPlayerName())
										&& !getCurrentPlayerServerForVoteRouting(cache.getPlayerName()).equals(server)) {
									toSend = false;
								}
							}
							if (toSend) {
								boolean broadcastHere = cache.needsBroadcastOn(server);
								if (!cache.isProxyBroadcastHandled() && broadcastHere
										&& getConfig().getProxyBroadcastEnabled()) {
									boolean playerOnline = isPlayerOnlineForVoteRouting(cache.getPlayerName());
									String playerServer = playerOnline ? getCurrentPlayerServerForVoteRouting(cache.getPlayerName())
											: null;

									Set<String> targets = proxyBroadcastDecider.resolveTargets(playerOnline,
											playerServer);
									broadcastHere = proxyBroadcastDecider.shouldBroadcast(server, targets);
								}

								if (!sendVoteEnvelopeAccepted(server, delay,
										VotingPluginWire.vote(cache.getPlayerName(), cache.getUuid(),
												cache.getService(), cache.getTime(), false, cache.isRealVote(),
												cache.getText(), cache.getVoteId(), getConfig().getBungeeManageTotals(),
												broadcastHere, num, numberOfVotes))) {
									debug("Retaining cached vote because the transport rejected delivery for " + server);
									continue;
								}
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
		if (isPlayerOnlineForVoteRouting(player) && getVoteCacheHandler().hasOnlineVotes(uuid)) {
			ArrayList<OfflineBungeeVote> c = getVoteCacheHandler().getOnlineVotes(uuid);
			if (!c.isEmpty()) {
				if (server == null) {
					server = getCurrentPlayerServerForVoteRouting(player);
				}
				if (!getConfig().getBlockedServers().contains(server)) {
					int num = 1;
					int numberOfVotes = (int) c.stream().filter(vote -> !vote.isRewardDelivered()).count();
					boolean deliveredReward = false;
					ArrayList<OfflineBungeeVote> retained = new ArrayList<>();
					for (OfflineBungeeVote cache : c) {
						if (cache.isProxyBroadcastHandled()) {
							Set<String> pendingTargets = new LinkedHashSet<>(cache.getBroadcastTargets());
							pendingTargets.removeAll(cache.getBroadcastForwardedServers());
							List<String> blockedServers = getConfig().getBlockedServers();
							if (blockedServers != null) {
								pendingTargets.removeAll(blockedServers);
							}
							cache.getBroadcastForwardedServers().addAll(sendProxyBroadcast(pendingTargets,
									cache.getUuid(), cache.getPlayerName(), cache.getService(), cache.getTime(),
									cache.getText(), false));
							cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
						}
						boolean broadcastHere = cache.needsBroadcastOn(server);
						if (!cache.isProxyBroadcastHandled() && broadcastHere
								&& getConfig().getProxyBroadcastEnabled()) {
							String playerServer = (server != null) ? server : getCurrentPlayerServerForVoteRouting(player);

							Set<String> targets = proxyBroadcastDecider.resolveTargets(true, playerServer);
							broadcastHere = proxyBroadcastDecider.shouldBroadcast(server, targets);
						}

						if (!cache.isRewardDelivered()) {
							if (!sendVoteEnvelopeAccepted(server, delay,
									VotingPluginWire.voteOnline(cache.getPlayerName(), cache.getUuid(), cache.getService(),
											cache.getTime(), false, cache.isRealVote(), cache.getText(), cache.getVoteId(),
											getConfig().getBungeeManageTotals(), broadcastHere, num, numberOfVotes))) {
								debug("Retaining online vote because the transport rejected delivery for " + server);
								retained.add(cache);
								continue;
							}
							// The normal envelope is also a valid broadcast delivery for the
							// current target. Record it so a previously pending standalone
							// retry cannot announce the same vote again later.
							if (cache.isProxyBroadcastHandled() && broadcastHere) {
								cache.getBroadcastForwardedServers().add(server);
								cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
							}
							cache.setRewardDelivered(true);
							deliveredReward = true;
							delay++;
							num++;
						}

						if (cache.isProxyBroadcastHandled() && !cache.isProxyBroadcastComplete()) {
							retained.add(cache);
						}
					}
					getVoteCacheHandler().removeOnlineVotes(uuid);
					for (OfflineBungeeVote pending : retained) {
						getVoteCacheHandler().addOnlineVote(uuid, pending);
					}

					// multiproxy: envelope-only
					if (deliveredReward && getConfig().getMultiProxySupport()
							&& getConfig().getMultiProxyOneGlobalReward()) {
						multiProxyHandler.sendClearVote(uuid, player);
					}
				}
			}
		}
	}

	/**
	 * Retries voter-keyed standalone broadcasts when any player makes a target
	 * backend available as a plugin-message carrier.
	 *
	 * @param server backend server that gained a carrier
	 */
	protected synchronized void retryPendingOnlineBroadcasts(String server) {
		List<String> blockedServers = getConfig().getBlockedServers();
		if (server == null || (blockedServers != null && blockedServers.contains(server))) {
			return;
		}
		for (String cachedUuid : getVoteCacheHandler().getOnlineVoteUUIDs()) {
			for (OfflineBungeeVote cache : new ArrayList<>(getVoteCacheHandler().getOnlineVotes(cachedUuid))) {
				if (cache.isDeliveryStateDirty() && !persistOnlineVoteDelivery(cachedUuid, cache)) {
					continue;
				}
				if (!cache.isProxyBroadcastHandled() || !cache.needsBroadcastOn(server)) {
					continue;
				}
				Set<String> forwarded = sendProxyBroadcast(Collections.singleton(server), cache.getUuid(),
						cache.getPlayerName(), cache.getService(), cache.getTime(), cache.getText(), false);
				if (cache.getBroadcastForwardedServers().addAll(forwarded)) {
					cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
					if (cache.isRewardDelivered() && cache.isProxyBroadcastComplete()) {
						getVoteCacheHandler().removeOnlineVote(cachedUuid, cache);
					} else {
						persistOnlineVoteDelivery(cachedUuid, cache);
					}
				}
			}
		}
	}

	protected synchronized void retryPendingTimeBroadcasts(String server) {
		List<String> blockedServers = getConfig().getBlockedServers();
		if (server == null || (blockedServers != null && blockedServers.contains(server))) {
			return;
		}
		if (getVoteCacheHandler().getTimeChangeQueue() == null) {
			return;
		}
		for (VoteTimeQueue vote : new ArrayList<>(getVoteCacheHandler().getTimeChangeQueue())) {
			if (vote.isDeliveryStateDirty() && !persistTimeVoteDelivery(vote)) {
				continue;
			}
			if (!vote.isProxyBroadcastHandled() || vote.getUuid().isEmpty() || !vote.getBroadcastTargets().contains(server)
					|| vote.getBroadcastForwardedServers().contains(server)) {
				continue;
			}
			Set<String> forwarded = sendProxyBroadcast(Collections.singleton(server), vote.getUuid(), vote.getName(),
					vote.getService(), vote.getTime(), vote.getTotals(), false);
			if (vote.getBroadcastForwardedServers().addAll(forwarded)) {
				persistTimeVoteDelivery(vote);
			}
		}
	}

	/**
	 * Periodically retries every pending voter-keyed standalone broadcast. This is
	 * required for broker transports whose recovery does not produce a player-login
	 * carrier event.
	 */
	public synchronized void retryPendingOnlineBroadcasts() {
		for (String cachedUuid : new LinkedHashSet<>(getVoteCacheHandler().getOnlineVoteUUIDs())) {
			for (OfflineBungeeVote cache : new ArrayList<>(getVoteCacheHandler().getOnlineVotes(cachedUuid))) {
				if (cache.isDeliveryStateDirty() && !persistOnlineVoteDelivery(cachedUuid, cache)) {
					continue;
				}
				if (!cache.isProxyBroadcastHandled() || cache.isProxyBroadcastComplete()) {
					continue;
				}
				Set<String> pendingTargets = new LinkedHashSet<>(cache.getBroadcastTargets());
				pendingTargets.removeAll(cache.getBroadcastForwardedServers());
				List<String> blockedServers = getConfig().getBlockedServers();
				if (blockedServers != null) {
					pendingTargets.removeAll(blockedServers);
				}
				Set<String> forwarded = sendProxyBroadcast(pendingTargets, cache.getUuid(), cache.getPlayerName(),
						cache.getService(), cache.getTime(), cache.getText(), false);
				if (cache.getBroadcastForwardedServers().addAll(forwarded)) {
					cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
					if (cache.isRewardDelivered() && cache.isProxyBroadcastComplete()) {
						getVoteCacheHandler().removeOnlineVote(cachedUuid, cache);
					} else {
						persistOnlineVoteDelivery(cachedUuid, cache);
					}
				}
			}
		}
		retryPendingTimeBroadcasts();
	}

	public synchronized void retryPendingTimeBroadcasts() {
		if (getVoteCacheHandler().getTimeChangeQueue() == null) {
			return;
		}
		for (VoteTimeQueue vote : new ArrayList<>(getVoteCacheHandler().getTimeChangeQueue())) {
			if (vote.isDeliveryStateDirty() && !persistTimeVoteDelivery(vote)) {
				continue;
			}
			if (!vote.isProxyBroadcastHandled() || vote.getUuid().isEmpty()) {
				continue;
			}
			Set<String> pendingTargets = new LinkedHashSet<>(vote.getBroadcastTargets());
			pendingTargets.removeAll(vote.getBroadcastForwardedServers());
			List<String> blockedServers = getConfig().getBlockedServers();
			if (blockedServers != null) {
				pendingTargets.removeAll(blockedServers);
			}
			Set<String> forwarded = sendProxyBroadcast(pendingTargets, vote.getUuid(), vote.getName(), vote.getService(),
					vote.getTime(), vote.getTotals(), false);
			if (vote.getBroadcastForwardedServers().addAll(forwarded)) {
				persistTimeVoteDelivery(vote);
			}
		}
	}

	protected synchronized boolean persistTimeVoteDelivery(VoteTimeQueue vote) {
		if (getVoteCacheHandler().updateTimeVote(vote)) {
			vote.setDeliveryStateDirty(false);
			return true;
		}
		vote.setDeliveryStateDirty(true);
		scheduleTimeVoteDeliveryRetry();
		return false;
	}

	private void scheduleTimeVoteDeliveryRetry() {
		if (timeVoteDeliveryRetryScheduled || getScheduler() == null) {
			return;
		}
		timeVoteDeliveryRetryScheduled = true;
		try {
			getScheduler().schedule(() -> {
				synchronized (VotingPluginProxy.this) {
					timeVoteDeliveryRetryScheduled = false;
				}
				retryPendingTimeBroadcasts();
			}, 5, TimeUnit.SECONDS);
		} catch (RuntimeException e) {
			timeVoteDeliveryRetryScheduled = false;
			debug("Unable to schedule timed broadcast state retry: " + e.getMessage());
		}
	}

	protected synchronized boolean persistServerVoteDelivery(String server, OfflineBungeeVote vote) {
		if (getVoteCacheHandler().updateServerVote(server, vote)) {
			vote.setDeliveryStateDirty(false);
			return true;
		}
		vote.setDeliveryStateDirty(true);
		scheduleCachedVoteDeliveryRetry();
		return false;
	}

	protected synchronized boolean persistOnlineVoteDelivery(String uuid, OfflineBungeeVote vote) {
		if (getVoteCacheHandler().updateOnlineVote(uuid, vote)) {
			vote.setDeliveryStateDirty(false);
			return true;
		}
		vote.setDeliveryStateDirty(true);
		scheduleCachedVoteDeliveryRetry();
		return false;
	}

	private void scheduleCachedVoteDeliveryRetry() {
		if (cachedVoteDeliveryRetryScheduled || getScheduler() == null) {
			return;
		}
		cachedVoteDeliveryRetryScheduled = true;
		try {
			getScheduler().schedule(() -> {
				synchronized (VotingPluginProxy.this) {
					cachedVoteDeliveryRetryScheduled = false;
				}
				retryCachedVoteDeliveryPersistence();
			}, 5, TimeUnit.SECONDS);
		} catch (RuntimeException e) {
			cachedVoteDeliveryRetryScheduled = false;
			debug("Unable to schedule cached broadcast state retry: " + e.getMessage());
		}
	}

	private synchronized void retryCachedVoteDeliveryPersistence() {
		for (String server : getVoteCacheHandler().getCachedVotesServers()) {
			for (OfflineBungeeVote vote : new ArrayList<>(getVoteCacheHandler().getVotes(server))) {
				if (vote.isDeliveryStateDirty()) {
					persistServerVoteDelivery(server, vote);
				}
			}
		}
		for (String uuid : new LinkedHashSet<>(getVoteCacheHandler().getOnlineVoteUUIDs())) {
			for (OfflineBungeeVote vote : new ArrayList<>(getVoteCacheHandler().getOnlineVotes(uuid))) {
				if (vote.isDeliveryStateDirty()) {
					persistOnlineVoteDelivery(uuid, vote);
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

	/**
	 * HTTP client used for Mojang API requests.
	 */
	private final HttpClient httpClient = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build();

	/**
	 * Fetches a player's UUID from the Mojang API.
	 *
	 * @param playerName player name
	 * @return player UUID, or {@code null} if not found
	 * @throws IOException          if the request fails
	 * @throws InterruptedException if interrupted while waiting for the response
	 */
	public UUID fetchUUID(String playerName) throws IOException, InterruptedException {
		if (playerName == null || playerName.equalsIgnoreCase("null")) {
			return null;
		}

		HttpRequest request = HttpRequest.newBuilder()
				.uri(URI.create("https://api.mojang.com/users/profiles/minecraft/" + playerName)).GET()
				.timeout(Duration.ofSeconds(5)).build();

		HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

		if (response.statusCode() == 400 || response.statusCode() == 404) {
			log("There is no player with the name \"" + playerName + "\"!");
			return null;
		}

		if (response.statusCode() < 200 || response.statusCode() >= 300) {
			throw new IOException("Failed to fetch UUID for " + playerName + ", HTTP " + response.statusCode());
		}

		JsonElement element = JsonParser.parseString(response.body());
		if (element == null || !element.isJsonObject()) {
			return null;
		}

		JsonObject object = element.getAsJsonObject();
		if (!object.has("id") || object.get("id").isJsonNull()) {
			return null;
		}

		String uuidAsString = object.get("id").getAsString();
		return parseUUIDFromString(uuidAsString);
	}

	public abstract Set<String> getAllAvailableServers();

	/** Complete platform server set before whitelist/blocked routing filters. */
	public abstract Set<String> getAllConfiguredServers();

	public abstract VotingPluginProxyConfig getConfig();

	public abstract String getCurrentPlayerServer(String player);

	/**
	 * Resolves a player's server for vote routing. A dedicated voting proxy has no
	 * local players, so it uses the backend presence tracker instead.
	 */
	protected String getCurrentPlayerServerForVoteRouting(String player) {
		if (isDedicatedVotingProxyEnabled()) {
			return backendPlayerPresenceTracker.getPlayer(player).map(presence -> presence.getServer()).orElse(null);
		}
		return getCurrentPlayerServer(player);
	}

	/**
	 * Dedicated routing is intentionally unavailable on plugin messaging: that
	 * transport is attached to a player-facing proxy and does not carry backend
	 * presence snapshots.
	 */
	protected boolean isDedicatedVotingProxyEnabled() {
		return getConfig().getDedicatedVotingProxy() && method != null && method.supportsBackendPresence();
	}

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

	private VoteTotalsSnapshot getProjectedRolloverTotals(ArrayList<Column> data, String player) {
		List<TimeType> timeChanges = getGlobalDataHandler().getTimeChanges();
		boolean resetMonth = timeChanges.contains(TimeType.MONTH);
		boolean resetWeek = timeChanges.contains(TimeType.WEEK);
		boolean resetDay = timeChanges.contains(TimeType.DAY);
		int acceptedQueuedVotes = 0;
		int acceptedGlobalQueuedVotes = 0;
		for (VoteTimeQueue queued : getVoteCacheHandler().getTimeChangeQueue()) {
			if (!queued.isProcessed()) {
				acceptedGlobalQueuedVotes++;
			}
			if (!queued.isProcessed() && queued.getName() != null && queued.getName().equalsIgnoreCase(player)) {
				acceptedQueuedVotes++;
			}
		}
		int voteIncrement = acceptedQueuedVotes + 1;

		int allTimeTotal = getValue(data, "AllTimeTotal", voteIncrement);
		int monthTotal = resetMonth ? voteIncrement : getValue(data, "MonthTotal", voteIncrement);
		int weeklyTotal = resetWeek ? voteIncrement : getValue(data, "WeeklyTotal", voteIncrement);
		int dailyTotal = resetDay ? voteIncrement : getValue(data, "DailyTotal", voteIncrement);
		int points = getValue(data, "Points", voteIncrement * getConfig().getPointsOnVote());

		int maxVotes = getConfig().getMaxAmountOfVotesPerDay();
		if (maxVotes > 0) {
			int days = getBungeeTimeChecker().getTime().getDayOfMonth();
			if (monthTotal > days * maxVotes) {
				monthTotal = days * maxVotes;
			}
		}
		if (getConfig().getLimitVotePoints() > 0 && points > getConfig().getLimitVotePoints()) {
			points = getConfig().getLimitVotePoints();
		}

		int dateMonthTotal = -1;
		if (getConfig().getStoreMonthTotalsWithDate()) {
			if (getConfig().getUseMonthDateTotalsAsPrimaryTotal()) {
				dateMonthTotal = resetMonth ? voteIncrement
						: getValue(data, getMonthTotalsWithDatePath(), voteIncrement);
			} else {
				dateMonthTotal = monthTotal;
			}
		}

		int[] projectedVoteParty = getProjectedVotePartyState(acceptedGlobalQueuedVotes + 1);
		return new VoteTotalsSnapshot(allTimeTotal, monthTotal, weeklyTotal, dailyTotal, points,
				projectedVoteParty[0], projectedVoteParty[1], dateMonthTotal);
	}

	protected boolean canForwardStandaloneBroadcast(boolean managesTotals) {
		return managesTotals;
	}

	protected int[] getProjectedVotePartyState(int acceptedVotes) {
		int current = votePartyVotes;
		int required = currentVotePartyVotesRequired;
		if (!getConfig().getVotePartyEnabled()) {
			return new int[] { current, required };
		}

		int increase = getConfig().getVotePartyIncreaseVotesRequired();
		for (int i = 0; i < acceptedVotes; i++) {
			current++;
			if (current >= required) {
				current -= required;
				required += increase;
			}
		}
		return new int[] { current, required };
	}

	public abstract String getPluginVersion();

	public abstract int getVoteCacheCurrentVotePartyVotes();

	public abstract long getVoteCacheLastUpdated();

	public abstract int getVoteCachePrevDay();

	public abstract String getVoteCachePrevMonth();

	public abstract int getVoteCachePrevWeek();

	public abstract int getVoteCacheVotePartyIncreaseVotesRequired();

	public abstract boolean isPlayerOnline(String playerName);

	/**
	 * Checks online state for vote routing, using backend presence only when this
	 * proxy is explicitly configured as the dedicated voting proxy.
	 */
	protected boolean isPlayerOnlineForVoteRouting(String playerName) {
		return isDedicatedVotingProxyEnabled() ? backendPlayerPresenceTracker.getPlayer(playerName).isPresent()
				: isPlayerOnline(playerName);
	}

	public abstract boolean isServerValid(String server);

	public abstract boolean isSomeoneOnlineServer(String server);

	protected boolean isSomeoneOnlineServerForVoteRouting(String server) {
		if (!isDedicatedVotingProxyEnabled()) {
			return isSomeoneOnlineServer(server);
		}
		com.bencodez.votingplugin.proxy.presence.BackendPresenceStatus status = backendPlayerPresenceTracker
				.getBackendStatus(server);
		return status != null && status.isAvailable() && status.getPlayerCount() > 0;
	}

	public abstract boolean isVoteCacheIgnoreTime();

	public abstract MysqlConfig getVoteCacheMySQLConfig();

	public abstract MysqlConfig getNonVotedCacheMySQLConfig();

	public abstract MysqlConfig getVoteLoggingMySQLConfig();

	/**
	 * Shutdown MySQL-related resources safely.
	 */
	public void shutdownMySql() {
		if (getProxyMysqlMessenger() != null) {
			getProxyMysqlMessenger().shutdown();
			setProxyMysqlMessenger(null);
		}

		if (getProxyMySQL() != null) {
			getProxyMySQL().shutdown();
			setProxyMySQL(null);
		}
	}

	public void load(IVoteCache jsonStorage, INonVotedPlayersStorage nonVotedCacheJson) {
		method = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (getMethod() == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}
		warnUnsupportedDedicatedVotingProxyMode();
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

			rebuildSocketClients();
		} else if (method.equals(BungeeMethod.HTTP)) {
			startHttpTransport();
		} else if (method.equals(BungeeMethod.REDIS)) {
			redisHandler = new RedisHandler(getConfig().getRedisHost(), getConfig().getRedisPort(),
					getConfig().getRedisUsername(), getConfig().getRedisPassword(), getConfig().getRedisDbIndex(),
					getConfig().getRedisSsl()) {

				@Override
				public void debug(String message) {
					debug2(message);
				}
			};
			redisPublisherPool = new JedisPool(new HostAndPort(getConfig().getRedisHost(), getConfig().getRedisPort()),
					buildRedisClientConfig(getConfig()));

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
					sendSocketEnvelope(server, envelope);
					break;
				case HTTP:
					sendHttpEnvelope(server, envelope);
					break;
				default:
					break;
				}
			}
		};

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_LOGIN) {
			@Override
			public void onReceive(JsonEnvelope message) {
				handleLoginMessage(message);
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_LOGOUT) {
			@Override
			public void onReceive(JsonEnvelope message) {
				if (!method.supportsBackendPresence()) {
					return;
				}
				VotingPluginWire.PlayerPresenceEvent event = VotingPluginWire.readPlayerPresenceEvent(message);
				if (!isPresenceServerValid(event.server, VotingPluginWire.SUB_LOGOUT)
						|| !isPresenceGenerationValid(event.backendIncarnationId, event.backendStartedAt,
								event.presenceTimestamp,
								VotingPluginWire.SUB_LOGOUT)) {
					return;
				}
				if (!backendPlayerPresenceTracker.playerOffline(event.uuid, event.server, event.connectionId,
						event.backendIncarnationId, event.backendStartedAt, event.presenceTimestamp,
						System.currentTimeMillis())) {
					debug("Ignored invalid or stale logout envelope: " + message.getFields());
				}
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_BACKEND_STARTED) {
			@Override
			public void onReceive(JsonEnvelope message) {
				if (!method.supportsBackendPresence()) {
					return;
				}
				String server = message.getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
				UUID backendIncarnationId = VotingPluginWire.readBackendIncarnationId(message);
				long backendStartedAt = VotingPluginWire.readBackendStartedAt(message);
				long presenceTimestamp = VotingPluginWire.readPresenceTimestamp(message);
				if (isPresenceServerValid(server, VotingPluginWire.SUB_BACKEND_STARTED)
						&& isPresenceGenerationValid(backendIncarnationId, backendStartedAt, presenceTimestamp,
								VotingPluginWire.SUB_BACKEND_STARTED)) {
					if (backendPlayerPresenceTracker.backendStarted(server, backendIncarnationId, backendStartedAt,
							presenceTimestamp, System.currentTimeMillis())) {
						discardPendingPresenceHandoffs(server);
						pendingBackendRecoverySnapshots.add(presenceServerKey(server));
						requestBackendPresenceSnapshot(server);
					}
				}
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_BACKEND_STOPPED) {
			@Override
			public void onReceive(JsonEnvelope message) {
				if (!method.supportsBackendPresence()) {
					return;
				}
				String server = message.getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
				UUID backendIncarnationId = VotingPluginWire.readBackendIncarnationId(message);
				long backendStartedAt = VotingPluginWire.readBackendStartedAt(message);
				long presenceTimestamp = VotingPluginWire.readPresenceTimestamp(message);
				if (isPresenceServerValid(server, VotingPluginWire.SUB_BACKEND_STOPPED)
						&& isPresenceGenerationValid(backendIncarnationId, backendStartedAt, presenceTimestamp,
								VotingPluginWire.SUB_BACKEND_STOPPED)) {
					if (backendPlayerPresenceTracker.backendStopped(server, backendIncarnationId, backendStartedAt,
							presenceTimestamp, System.currentTimeMillis())) {
						discardPendingPresenceHandoffs(server);
						pendingBackendRecoverySnapshots.remove(presenceServerKey(server));
					}
				}
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_BACKEND_HEARTBEAT) {
			@Override
			public void onReceive(JsonEnvelope message) {
				if (!method.supportsBackendPresence()) {
					return;
				}
				String server = message.getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
				UUID backendIncarnationId = VotingPluginWire.readBackendIncarnationId(message);
				long backendStartedAt = VotingPluginWire.readBackendStartedAt(message);
				long presenceTimestamp = VotingPluginWire.readPresenceTimestamp(message);
				if (isPresenceServerValid(server, VotingPluginWire.SUB_BACKEND_HEARTBEAT)
						&& isPresenceGenerationValid(backendIncarnationId, backendStartedAt, presenceTimestamp,
								VotingPluginWire.SUB_BACKEND_HEARTBEAT)) {
					backendPlayerPresenceTracker.heartbeat(server, backendIncarnationId, backendStartedAt,
							presenceTimestamp, System.currentTimeMillis());
				}
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_PRESENCE_SNAPSHOT) {
			@Override
			public void onReceive(JsonEnvelope message) {
				if (!method.supportsBackendPresence()) {
					return;
				}
				String server = message.getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
				if (!isPresenceServerValid(server, VotingPluginWire.SUB_PRESENCE_SNAPSHOT)) {
					return;
				}
				VotingPluginWire.PresenceSnapshot snapshot = VotingPluginWire.readPresenceSnapshot(message);
				long now = System.currentTimeMillis();
				boolean accepted = snapshot.valid
						&& isPresenceGenerationValid(snapshot.backendIncarnationId, snapshot.backendStartedAt,
								snapshot.presenceTimestamp,
								VotingPluginWire.SUB_PRESENCE_SNAPSHOT)
						&& backendPlayerPresenceTracker.applySnapshotChunk(snapshot.server,
								snapshot.requestId, snapshot.chunkIndex, snapshot.chunkCount, snapshot.players,
								snapshot.backendIncarnationId, snapshot.backendStartedAt,
								snapshot.presenceTimestamp, now);
				if (!accepted) {
					debug("Ignored invalid or unexpected presence snapshot from " + snapshot.server);
					if (backendPlayerPresenceTracker.getPendingSnapshotRequestId(snapshot.server, now) == null) {
						discardPendingPresenceHandoffs(snapshot.requestId);
					}
				} else if (backendPlayerPresenceTracker.getPendingSnapshotRequestId(snapshot.server, now) == null) {
					pendingBackendRecoverySnapshots.remove(presenceServerKey(snapshot.server));
					Set<UUID> handoffPlayers = completePendingPresenceHandoffs(snapshot.requestId, snapshot.server,
							snapshot.backendIncarnationId, snapshot.backendStartedAt, now);
					processDedicatedSnapshotLogins(snapshot.server, handoffPlayers);
				}
			}
		});

		globalMessageProxyHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_STATUS_OKAY) {
			@Override
			public void onReceive(JsonEnvelope message) {
				handleStatusOkay(message);
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

		proxyBroadcastDecider = new ProxyBroadcastDecider(() -> getConfig(), () -> getAllAvailableServers(),
				s -> isServerValid(s),
				s -> getConfig().getBlockedServers() != null && getConfig().getBlockedServers().contains(s));

		loadMultiProxySupport();
		loadVoteLoggingMySQL();
		if (method.supportsBackendPresence()) {
			scheduleBackendPresenceStartupResync();
			loadTaskTimer(this::maintainBackendPresence, PRESENCE_MAINTENANCE_INTERVAL_SECONDS,
					PRESENCE_MAINTENANCE_INTERVAL_SECONDS);
		}
		startControlServices();

		debug("VotingPluginProxy loaded, ONLINEMODE: " + getConfig().getOnlineMode());
	}

	private void startControlServices() {
		synchronized (controlLifecycleLock) {
			ControlConnector predecessor = controlConnector;
			if (predecessor != null && predecessor.deferReplacementUntilSafe(this::restartControlServicesAsync)) {
				log("[Control] service restart deferred until the current result is acknowledged");
				return;
			}
			stopControlServicesLocked(true);
			startControlServicesLocked();
		}
	}

	/** Keeps potentially long hosted-Control handoffs off proxy command/event threads. */
	private void restartControlServicesAsync() {
		final long generation = controlServicesGeneration.incrementAndGet();
		try {
			controlLifecycleExecutor.execute(() -> {
				try {
					synchronized (controlLifecycleLock) {
						if (!enabled || generation != controlServicesGeneration.get()) return;
						ControlConnector predecessor = controlConnector;
						if (predecessor != null
								&& predecessor.deferReplacementUntilSafe(this::restartControlServicesAsync)) {
							log("[Control] service restart deferred until the current result is acknowledged");
							return;
						}
						stopControlServicesLocked(true);
						startControlServicesLocked();
					}
				} catch (RuntimeException failure) {
					if (generation == controlServicesGeneration.get()) {
						logSevere("[Control] asynchronous service restart failed: " + failure.getMessage());
					}
				}
			});
		} catch (RuntimeException failure) {
			logSevere("[Control] services were not restarted because async scheduling failed");
		}
	}

	/** Rebuilds a recovery connector from current settings after its durable result is acknowledged. */
	public final void restartControlServicesAfterRecovery() {
		restartControlServicesAsync();
	}

	private void stopControlServices(boolean waitForHosted) {
		synchronized (controlLifecycleLock) {
			stopControlServicesLocked(waitForHosted);
		}
	}

	private void startControlServicesLocked() {
		if (getConfig().getControlHostedEnabled()) {
			try {
				hostedControlManager = HostedControlManager.create(this);
				if (hostedControlManager != null) hostedControlManager.start();
			} catch (IOException | IllegalArgumentException e) {
				hostedControlManager = null;
				logSevere("[Control Host] configuration or automatic enrollment is invalid; VotingPlugin remains unaffected");
			}
		}
		try {
			controlConnector = ControlConnector.create(this);
			if (controlConnector != null) controlConnector.start();
		} catch (IOException | IllegalArgumentException e) {
			controlConnector = null;
			logSevere("[Control] connector configuration or credential is invalid; voting remains unaffected");
		}
	}

	private void stopControlServicesLocked(boolean waitForHosted) {
		ControlConnector connector = controlConnector;
		if (connector != null) {
			try {
				connector.close();
				if (controlConnector == connector) controlConnector = null;
			} catch (RuntimeException failure) {
				if (waitForHosted) throw failure;
				if (controlConnector == connector) controlConnector = null;
				logSevere("[Control] connector did not stop cleanly; proxy cleanup will continue");
			}
		}
		HostedControlManager manager = hostedControlManager;
		if (manager != null) {
			try {
				if (waitForHosted) {
					manager.closeAndWait();
				} else {
					manager.close();
				}
				if (hostedControlManager == manager) hostedControlManager = null;
			} catch (RuntimeException failure) {
				if (waitForHosted) throw failure;
				if (hostedControlManager == manager) hostedControlManager = null;
				logSevere("[Control Host] manager did not stop cleanly; proxy cleanup will continue");
			}
		}
	}

	public String getControlConnectorStatus() {
		ControlConnector connector = controlConnector;
		return connector == null ? "DISABLED" : connector.status().name();
	}

	public String getHostedControlStatus() {
		HostedControlManager manager = hostedControlManager;
		return manager == null ? "DISABLED" : manager.status().name();
	}

	/**
	 * Handles both the original login notification and extended presence logins.
	 * Kept protected so transport-policy behavior can be regression tested without
	 * initializing a live proxy transport.
	 *
	 * @param message login envelope
	 */
	protected void handleLoginMessage(JsonEnvelope message) {
		VotingPluginWire.PlayerPresenceEvent event = VotingPluginWire.readPlayerPresenceEvent(message);
		String player = event.player;
		String uuid = event.uuid;
		String server = event.server;

		if (player.isEmpty() || uuid.isEmpty()) {
			logSevere("Invalid login envelope received: " + message.getFields());
			return;
		}
		boolean legacy = event.connectionId == null && event.backendIncarnationId == null
				&& event.backendStartedAt == 0L && event.presenceTimestamp == 0L;
		boolean accepted = false;
		String deliveryServer = server;
		if (legacy) {
			if (method == BungeeMethod.PLUGINMESSAGING) {
				String proxyServer = getCurrentPlayerServer(player);
				accepted = isLegacyLoginDestinationAuthoritative(player, uuid, proxyServer);
				if (accepted) {
					deliveryServer = proxyServer;
				}
			} else if (method != null && method.supportsBackendPresence()
					&& isPresenceServerValid(server, VotingPluginWire.SUB_LOGIN)) {
				accepted = isLegacyLoginDestinationAuthoritative(player, uuid, server);
			}
		} else if (method != null && method.supportsBackendPresence() && event.connectionId != null
				&& isPresenceServerValid(server, VotingPluginWire.SUB_LOGIN)
				&& isPresenceGenerationValid(event.backendIncarnationId, event.backendStartedAt,
						event.presenceTimestamp, VotingPluginWire.SUB_LOGIN)) {
			BackendPlayerPresenceTracker.PlayerOnlineResult result = backendPlayerPresenceTracker.playerOnlineResult(
					player, uuid, server, event.connectionId,
					event.backendIncarnationId, event.backendStartedAt, event.presenceTimestamp,
					System.currentTimeMillis());
			accepted = result.isAccepted();
			if (result.isConflictingPresence()) {
				requestBackendPresenceSnapshot(server,
						new PendingPresenceHandoff(player, uuid, server, event.connectionId,
								event.backendIncarnationId, event.backendStartedAt,
								result.getConflictSequence(), System.currentTimeMillis()));
			}
		}

		debug("Login: " + player + "/" + uuid + " " + server);
		if (accepted) {
			discardPendingPresenceHandoff(uuid);
			login(player, uuid, deliveryServer);
		} else {
			debug("Ignored invalid or stale login envelope: " + message.getFields());
		}
	}

	/**
	 * Validates a legacy login against an authority independent of the envelope.
	 * Player-facing proxies use their native live route and UUID. A dedicated
	 * voting proxy has no native player session, so it requires an exact modern
	 * presence match for the claimed destination.
	 */
	private boolean isLegacyLoginDestinationAuthoritative(String player, String uuid, String server) {
		if (server == null || server.isBlank()) {
			return false;
		}

		UUID claimedUuid;
		try {
			claimedUuid = UUID.fromString(uuid.trim());
		} catch (RuntimeException e) {
			return false;
		}

		if (isDedicatedVotingProxyEnabled()) {
			PlayerPresence presence = backendPlayerPresenceTracker.getPlayer(player).orElse(null);
			return presence != null && presence.getServer().equalsIgnoreCase(server)
					&& (!getConfig().getOnlineMode() || presence.getUuid().equals(claimedUuid));
		}

		if (!isPlayerOnline(player)) {
			return false;
		}
		String proxyServer = getCurrentPlayerServer(player);
		if (proxyServer == null || !proxyServer.equalsIgnoreCase(server)) {
			return false;
		}
		if (!getConfig().getOnlineMode()) {
			return true;
		}

		String authoritativeUuid = getUUID(player);
		if (authoritativeUuid == null || authoritativeUuid.isBlank()) {
			return false;
		}
		try {
			return claimedUuid.equals(UUID.fromString(authoritativeUuid.trim()));
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	private VoteLogMysqlTable voteLogMysqlTable;

	@Getter
	private ProxyBroadcastDecider proxyBroadcastDecider;

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
				getVoteCacheHandler().clearOnlineVoteRewards(uuid);
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
			public boolean getMultiProxyRedisSsl() {
				return getConfig().getMultiProxyRedisSsl();
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

	/**
	 * Requests a complete player-presence snapshot from one backend server.
	 *
	 * @param server configured backend server name
	 * @return new or already-active request identifier, or null when the server is
	 *         invalid or is inside the snapshot-request cooldown
	 */
	public UUID requestBackendPresenceSnapshot(String server) {
		return requestBackendPresenceSnapshot(server, null);
	}

	private UUID requestBackendPresenceSnapshot(String server, PendingPresenceHandoff handoff) {
		return requestBackendPresenceSnapshot(server, handoff, System.currentTimeMillis(), false);
	}

	private UUID requestBackendPresenceSnapshot(String server, PendingPresenceHandoff handoff, long now,
			boolean handoffAlreadyQueued) {
		if (method == null || !method.supportsBackendPresence() || globalMessageProxyHandler == null
				|| !isPresenceServerValid(server, VotingPluginWire.SUB_PRESENCE_SNAPSHOT_REQUEST)) {
			return null;
		}
		long backendStartedAt = backendPlayerPresenceTracker.getBackendStartedAt(server);
		UUID backendIncarnationId = backendPlayerPresenceTracker.getBackendIncarnationId(server);
		if (backendStartedAt <= 0L || backendIncarnationId == null) {
			return null;
		}
		if (handoff != null && (!server.equalsIgnoreCase(handoff.server)
				|| !backendIncarnationId.equals(handoff.backendIncarnationId)
				|| backendStartedAt != handoff.backendStartedAt)) {
			return null;
		}
		if (handoff != null && (handoffAlreadyQueued ? !isPendingPresenceHandoff(handoff, now)
				: !queuePendingPresenceHandoff(handoff, now))) {
			return null;
		}
		UUID requestId = handoff == null
				? backendPlayerPresenceTracker.beginSnapshot(server, UUID.randomUUID(), backendIncarnationId,
						backendStartedAt, now)
				: backendPlayerPresenceTracker.beginSnapshotForDestinationClaim(server, UUID.randomUUID(),
						backendIncarnationId, backendStartedAt, handoff.playerUuid, handoff.conflictSequence, now);
		boolean created = requestId != null;
		if (!created) {
			requestId = handoff == null ? backendPlayerPresenceTracker.getPendingSnapshotRequestId(server, now)
					: backendPlayerPresenceTracker.getPendingSnapshotRequestIdForDestinationClaim(server,
							handoff.playerUuid, handoff.conflictSequence, now);
		}
		if (requestId == null) {
			if (handoff != null && !backendPlayerPresenceTracker.isCurrentDestinationClaim(handoff.playerUuid,
					handoff.server, handoff.conflictSequence)) {
				discardPendingPresenceHandoff(handoff);
			}
			// A handoff stays unassigned while the destination is inside its snapshot
			// cooldown. Presence maintenance will attach it to the next allowed snapshot.
			return null;
		}
		if (handoff != null) {
			assignPendingPresenceHandoff(handoff, requestId, now);
		}
		if (created) {
			JsonEnvelope request = VotingPluginWire.presenceSnapshotRequest(server, requestId, backendIncarnationId,
					backendStartedAt, now);
			globalMessageProxyHandler.sendMessage(server, 1, request);
		}
		return requestId;
	}

	private boolean queuePendingPresenceHandoff(PendingPresenceHandoff handoff, long now) {
		if (!isPresenceHandoffValid(handoff, now)) {
			return false;
		}
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
			PendingPresenceHandoff current = pendingPresenceHandoffs.get(handoff.playerUuid);
			if (current != null && current.conflictSequence > handoff.conflictSequence) {
				return false;
			}
			handoff.requestId = null;
			pendingPresenceHandoffs.put(handoff.playerUuid, handoff);
			return true;
		}
	}

	private boolean isPendingPresenceHandoff(PendingPresenceHandoff handoff, long now) {
		if (!isPresenceHandoffValid(handoff, now)) {
			return false;
		}
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
			return pendingPresenceHandoffs.get(handoff.playerUuid) == handoff;
		}
	}

	private void assignPendingPresenceHandoff(PendingPresenceHandoff handoff, UUID requestId, long now) {
		if (requestId == null || !isPresenceHandoffValid(handoff, now)) {
			return;
		}
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
			if (pendingPresenceHandoffs.get(handoff.playerUuid) == handoff) {
				handoff.requestId = requestId;
			}
		}
	}

	private boolean isPresenceHandoffValid(PendingPresenceHandoff handoff, long now) {
		return handoff != null && handoff.playerUuid != null && handoff.connectionId != null
				&& handoff.conflictSequence > 0L
				&& now >= handoff.createdAt && now - handoff.createdAt <= PRESENCE_HANDOFF_TIMEOUT_MILLIS;
	}

	private Set<UUID> completePendingPresenceHandoffs(UUID requestId, String server, UUID backendIncarnationId,
			long backendStartedAt, long now) {
		List<PendingPresenceHandoff> completed = new ArrayList<>();
		Set<UUID> completedPlayers = new LinkedHashSet<>();
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
			pendingPresenceHandoffs.entrySet().removeIf(entry -> {
				PendingPresenceHandoff handoff = entry.getValue();
				if (!requestId.equals(handoff.requestId)) {
					return false;
				}
				if (handoff.server.equalsIgnoreCase(server)
						&& handoff.backendIncarnationId.equals(backendIncarnationId)
						&& handoff.backendStartedAt == backendStartedAt) {
					completed.add(handoff);
				}
				return true;
			});
		}
		for (PendingPresenceHandoff handoff : completed) {
			PlayerPresence presence = backendPlayerPresenceTracker.getPlayer(handoff.playerUuid).orElse(null);
			if (presence != null && presence.getServer().equalsIgnoreCase(handoff.server)
					&& presence.getConnectionId().equals(handoff.connectionId)) {
				login(handoff.playerName, handoff.uuid, handoff.server);
				completedPlayers.add(handoff.playerUuid);
			}
			releaseDestinationClaim(handoff);
		}
		return completedPlayers;
	}

	/**
	 * Drains voter-keyed cached rewards when a complete recovery snapshot first
	 * confirms a player on a dedicated voting proxy. Cross-backend handoffs are
	 * already processed by their token-bound completion path and are excluded to
	 * avoid a second login callback.
	 */
	protected void processDedicatedSnapshotLogins(String server, Set<UUID> handoffPlayers) {
		if (!isDedicatedVotingProxyEnabled() || server == null || server.isBlank()) {
			return;
		}
		Set<UUID> excluded = handoffPlayers == null ? Collections.emptySet() : handoffPlayers;
		for (PlayerPresence presence : backendPlayerPresenceTracker.getOnlinePlayers()) {
			if (presence.getServer().equalsIgnoreCase(server) && !excluded.contains(presence.getUuid())) {
				login(presence.getPlayerName(), presence.getUuid().toString(), presence.getServer());
			}
		}
	}

	private void discardPendingPresenceHandoff(String uuid) {
		try {
			UUID playerUuid = UUID.fromString(uuid.trim());
			PendingPresenceHandoff removed;
			synchronized (pendingPresenceHandoffs) {
				removed = pendingPresenceHandoffs.remove(playerUuid);
			}
			releaseDestinationClaim(removed);
		} catch (Exception ignored) {
			// Invalid identities are rejected by the presence tracker.
		}
	}

	private void discardPendingPresenceHandoff(PendingPresenceHandoff handoff) {
		boolean removed = false;
		synchronized (pendingPresenceHandoffs) {
			if (handoff != null && pendingPresenceHandoffs.get(handoff.playerUuid) == handoff) {
				pendingPresenceHandoffs.remove(handoff.playerUuid);
				removed = true;
			}
		}
		if (removed) {
			releaseDestinationClaim(handoff);
		}
	}

	private void discardPendingPresenceHandoffs(String server) {
		synchronized (pendingPresenceHandoffs) {
			pendingPresenceHandoffs.entrySet().removeIf(entry -> {
				if (!entry.getValue().server.equalsIgnoreCase(server)) {
					return false;
				}
				releaseDestinationClaim(entry.getValue());
				return true;
			});
		}
	}

	private void discardPendingPresenceHandoffs(UUID requestId) {
		if (requestId == null) {
			return;
		}
		synchronized (pendingPresenceHandoffs) {
			pendingPresenceHandoffs.entrySet().removeIf(entry -> {
				if (!requestId.equals(entry.getValue().requestId)) {
					return false;
				}
				releaseDestinationClaim(entry.getValue());
				return true;
			});
		}
	}

	private void prunePendingPresenceHandoffs(long now) {
		pendingPresenceHandoffs.entrySet().removeIf(entry -> {
			PendingPresenceHandoff handoff = entry.getValue();
			if (now >= handoff.createdAt && now - handoff.createdAt <= PRESENCE_HANDOFF_TIMEOUT_MILLIS) {
				return false;
			}
			releaseDestinationClaim(handoff);
			return true;
		});
	}

	private void releaseDestinationClaim(PendingPresenceHandoff handoff) {
		if (handoff != null) {
			backendPlayerPresenceTracker.releaseDestinationClaim(handoff.playerUuid, handoff.server,
					handoff.conflictSequence);
		}
	}

	protected void retryPendingPresenceHandoffs(long now) {
		List<PendingPresenceHandoff> retry = new ArrayList<>();
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
			for (PendingPresenceHandoff handoff : pendingPresenceHandoffs.values()) {
				UUID activeRequestId = backendPlayerPresenceTracker.getPendingSnapshotRequestId(handoff.server, now);
				if (handoff.requestId != null && !handoff.requestId.equals(activeRequestId)) {
					handoff.requestId = null;
				}
				if (handoff.requestId == null) {
					retry.add(handoff);
				}
			}
		}
		for (PendingPresenceHandoff handoff : retry) {
			requestBackendPresenceSnapshot(handoff.server, handoff, now, true);
		}
	}

	protected int getPendingPresenceHandoffCount() {
		synchronized (pendingPresenceHandoffs) {
			return pendingPresenceHandoffs.size();
		}
	}

	protected void scheduleBackendPresenceStartupResync() {
		ScheduledExecutorService scheduler = getScheduler();
		if (method == null || !method.supportsBackendPresence() || scheduler == null) {
			return;
		}
		scheduler.schedule(this::requestBackendPresenceStartupResync,
				PRESENCE_STARTUP_RESYNC_DELAY_SECONDS, TimeUnit.SECONDS);
	}

	protected void requestBackendPresenceStartupResync() {
		if (!enabled || method == null || !method.supportsBackendPresence() || globalMessageProxyHandler == null) {
			return;
		}
		long requestedAt = System.currentTimeMillis();
		int delay = 1;
		for (String server : getAllAvailableServers()) {
			if (!isPresenceServerValid(server, VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST)) {
				continue;
			}
			globalMessageProxyHandler.sendMessage(server, delay++,
					VotingPluginWire.presenceResyncRequest(server, UUID.randomUUID(), requestedAt));
		}
	}

	private void maintainBackendPresence() {
		if (!enabled || method == null || !method.supportsBackendPresence()) {
			return;
		}
		expireBackendPresence(PRESENCE_BACKEND_TIMEOUT_MILLIS);
		for (String server : getAllAvailableServers()) {
			if (pendingBackendRecoverySnapshots.contains(presenceServerKey(server))) {
				requestBackendPresenceSnapshot(server);
			}
		}
		retryPendingPresenceHandoffs(System.currentTimeMillis());
	}

	private String presenceServerKey(String server) {
		return server == null ? "" : server.trim().toLowerCase(java.util.Locale.ROOT);
	}

	private boolean isPresenceServerValid(String server, String subChannel) {
		// The presence protocol's trust boundary is the configured backend set. The
		// selected transport must only be accessible to backend servers trusted not to
		// impersonate one another.
		if (server == null || server.isBlank() || !isServerValid(server)) {
			debug("Ignored " + subChannel + " presence envelope for an unconfigured server");
			return false;
		}
		return true;
	}

	private boolean isPresenceGenerationValid(UUID backendIncarnationId, long backendStartedAt,
			long presenceTimestamp, String subChannel) {
		if (backendIncarnationId == null || backendStartedAt <= 0L || presenceTimestamp < backendStartedAt) {
			debug("Ignored " + subChannel + " presence envelope with an invalid backend generation");
			return false;
		}
		return true;
	}

	/**
	 * Removes presence owned by backends that have stopped reporting heartbeats.
	 * Scheduling and timeout configuration are intentionally left to dedicated
	 * proxy mode.
	 *
	 * @param timeoutMillis maximum backend silence before expiry
	 * @return expired backend server names
	 */
	public Set<String> expireBackendPresence(long timeoutMillis) {
		if (method == null || !method.supportsBackendPresence()) {
			return Collections.emptySet();
		}
		long now = System.currentTimeMillis();
		Set<String> expired = backendPlayerPresenceTracker.expireBackends(now, timeoutMillis);
		for (String server : expired) {
			discardPendingPresenceHandoffs(server);
			// Keep recovery pending while this generation is unavailable. If the same
			// backend process resumes, its heartbeat can mark it available again and the
			// maintenance task will request a fresh snapshot of players who stayed online.
			pendingBackendRecoverySnapshots.add(presenceServerKey(server));
		}
		synchronized (pendingPresenceHandoffs) {
			prunePendingPresenceHandoffs(now);
		}
		return expired;
	}

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
		if (isPlayerOnlineForVoteRouting(playerName)) {
			if (getConfig().getGlobalDataEnabled()) {
				if (getGlobalDataHandler().isTimeChangedHappened()) {
					getGlobalDataHandler().checkForFinishedTimeChanges();
				}
			}

			checkCachedVotes(serverName);
			retryPendingOnlineBroadcasts(serverName);
			retryPendingTimeBroadcasts(serverName);
			checkOnlineVotes(playerName, uuid, serverName);
			multiProxyHandler.login(uuid, playerName);
		}
	}

	private void logInfo(String msg) {
		log(msg);
	}

	public abstract void logSevere(String message);

	public void onDisable() {
		onDisable(false);
	}

	/** Full runtime replacement waits for hosted workers; final proxy stop remains non-blocking. */
	public void onDisable(boolean waitForHosted) {
		if (waitForHosted) {
			prepareForRuntimeReplacement();
		} else {
			controlServicesGeneration.incrementAndGet();
			controlLifecycleExecutor.shutdownNow();
			stopControlServices(false);
		}
		completeRuntimeReplacementShutdown();
	}

	/** Fail-closed gate that must complete before a replacement proxy runtime is created. */
	public void prepareForRuntimeReplacement() {
		controlServicesGeneration.incrementAndGet();
		synchronized (controlLifecycleLock) {
			ControlConnector connector = controlConnector;
			if (connector != null && !connector.reserveRuntimeReplacement()) {
				throw new IllegalStateException("Control result must be acknowledged before proxy runtime replacement");
			}
			controlLifecycleExecutor.shutdown();
			stopControlServicesLocked(true);
		}
	}

	/** Best-effort remainder of runtime teardown after the Control overlap gate has succeeded. */
	public void completeRuntimeReplacementShutdown() {
		cancelCommunicationTests("Proxy runtime stopped before the backend replied");
		runCleanup("vote cache", () -> getVoteCacheHandler().saveVoteCache());
		runCleanup("proxy MySQL messenger", () -> {
			if (getProxyMysqlMessenger() != null) getProxyMysqlMessenger().shutdown();
		});
		runCleanup("proxy MySQL", () -> {
			if (getProxyMySQL() != null) getProxyMySQL().shutdown();
		});
		runCleanup("multi-proxy handler", () -> {
			if (multiProxyHandler != null) multiProxyHandler.close();
		});
		runCleanup("socket listener", () -> {
			if (socketHandler != null) socketHandler.closeConnection();
		});
		runCleanup("socket clients", this::closeSocketClients);
		runCleanup("HTTP transport", this::closeHttpTransport);
		runCleanup("Redis subscriber", () -> {
			if (redisHandler != null) redisHandler.close();
		});
		runCleanup("Redis publisher", () -> {
			JedisPool pool = redisPublisherPool;
			try {
				if (pool != null) pool.close();
			} finally {
				if (redisPublisherPool == pool) redisPublisherPool = null;
			}
		});
		runCleanup("MQTT transport", () -> {
			if (mqttHandler != null) mqttHandler.disconnect();
		});
		runCleanup("time checker", () -> bungeeTimeChecker.shutdown());
		runCleanup("global data", () -> {
			if (getGlobalDataHandler() != null) getGlobalDataHandler().shutdown();
		});
		enabled = false;
	}

	private void runCleanup(String service, CleanupAction cleanup) {
		try {
			cleanup.run();
		} catch (Exception failure) {
			logSevere("Unable to stop " + service + "; remaining proxy cleanup will continue");
		}
	}

	@FunctionalInterface
	private interface CleanupAction { void run() throws Exception; }

	public void onPluginMessageReceived(DataInputStream in) {
		onPluginMessageReceived(in, null);
	}

	/** Receives a plugin message bound to the backend server connection that sent it. */
	public void onPluginMessageReceived(DataInputStream in, String sourceServer) {
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

				if (VotingPluginWire.SUB_CONTROL_ENROLLMENT_REQUEST.equals(envelope.getSubChannel())) {
					handleControlEnrollmentRequest(sourceServer, envelope);
					return;
				}

				globalMessageProxyHandler.onMessage(envelope);
			} catch (Exception e) {
				e.printStackTrace();
			}
		});
	}

	private void handleControlEnrollmentRequest(String sourceServer, JsonEnvelope envelope) {
		VotingPluginWire.ControlEnrollmentRequest request = VotingPluginWire.readControlEnrollmentRequest(envelope);
		if (!request.valid || sourceServer == null || sourceServer.isBlank()) return;
		if (!sourceServer.equals(request.nodeId)) {
			sendPluginMessageServer(sourceServer, 0,
					VotingPluginWire.controlEnrollmentResult(sourceServer, request.requestId, false));
			return;
		}
		long now = System.nanoTime();
		AtomicBoolean allowed = new AtomicBoolean();
		controlEnrollmentNextAllowed.compute(sourceServer, (ignored, nextAllowed) -> {
			if (nextAllowed == null || now >= nextAllowed) {
				allowed.set(true);
				return now + CONTROL_ENROLLMENT_MIN_INTERVAL_NANOS;
			}
			return nextAllowed;
		});
		if (!allowed.get()) return;
		HostedControlManager manager = hostedControlManager;
		if (manager == null) {
			sendPluginMessageServer(sourceServer, 0,
					VotingPluginWire.controlEnrollmentResult(sourceServer, request.requestId, false));
			return;
		}
		manager.installNodeVerifier(sourceServer, request.verifier, request.endpoint).whenComplete((installed, failure) -> {
			boolean success = failure == null && Boolean.TRUE.equals(installed);
			sendPluginMessageServer(sourceServer, 0,
					VotingPluginWire.controlEnrollmentResult(sourceServer, request.requestId, success));
			if (success) log("[Control] automatically enrolled backend node " + sourceServer);
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

	public synchronized void processQueue() {
		while (getVoteCacheHandler().getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getVoteCacheHandler().getTimeChangeQueue().element();
			if (!vote.isProcessed()) {
				VoteTotalsSnapshot queuedTotals = vote.getTotals() == null || vote.getTotals().isEmpty() ? null
						: VoteTotalsSnapshot.parseStorage(vote.getTotals());
				QueuedVoteResult result = vote(vote.getName(), vote.getService(), true, false, vote.getTime(), queuedTotals,
						vote.getUuid(), vote);
				if (result == QueuedVoteResult.RETRY) {
					scheduleTimeVoteRetry();
					return;
				}
				if (result == QueuedVoteResult.TERMINAL) {
					warn("Removing terminal rollover vote " + vote.getVoteId() + " for " + vote.getName() + "/"
							+ ServiceSiteValidator.sanitizeForLog(vote.getService()));
				}
			}
			if (!getVoteCacheHandler().removeTimeVote(vote)) {
				scheduleTimeVoteRetry();
				return;
			}
		}
	}

	private void scheduleTimeVoteRetry() {
		if (timeVoteRetryScheduled || getScheduler() == null) {
			return;
		}
		timeVoteRetryScheduled = true;
		try {
			getScheduler().schedule(() -> {
				synchronized (VotingPluginProxy.this) {
					timeVoteRetryScheduled = false;
				}
				processQueue();
			}, 5, TimeUnit.SECONDS);
		} catch (RuntimeException e) {
			timeVoteRetryScheduled = false;
			debug("Unable to schedule rollover vote retry: " + e.getMessage());
		}
	}

	public void reload() {
		reloadRuntime(true);
	}

	/** Applies a Control-originated configuration reload without stopping its connector or hosted service. */
	public void reloadFromControl() {
		reloadRuntime(false);
	}

	private void reloadRuntime(boolean restartControlServices) {
		method = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (getMethod() == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}
		warnUnsupportedDedicatedVotingProxyMode();
		if (!restartControlServices && method == BungeeMethod.SOCKETS) {
			rebuildSocketClients();
		}

		setCurrentVotePartyVotesRequired(
				getConfig().getVotePartyVotesRequired() + getVoteCacheVotePartyIncreaseVotesRequired());
		if (restartControlServices) {
			loadMultiProxySupport();
			restartControlServicesAsync();
		}
	}

	private synchronized void rebuildSocketClients() {
		HashMap<String, ClientHandler> rebuilt = new HashMap<>();
		try {
			List<String> blocked = getConfig().getBlockedServers();
			for (String server : getConfig().getSpigotServers()) {
				if (blocked.contains(server)) continue;
				Map<String, Object> data = getConfig().getSpigotServerConfiguration(server);
				String host = data.containsKey("Host") ? (String) data.get("Host") : "";
				int port = data.containsKey("Port") ? (int) data.get("Port") : 1298;
				rebuilt.put(server, new ClientHandler(host, port, encryptionHandler, getConfig().getDebug()));
			}
		} catch (RuntimeException failure) {
			stopSocketClients(rebuilt);
			throw failure;
		}
		HashMap<String, ClientHandler> previous = clientHandles;
		clientHandles = rebuilt;
		stopSocketClients(previous);
	}

	private synchronized boolean sendSocketEnvelope(String server, JsonEnvelope envelope) {
		ClientHandler socketClient = clientHandles == null ? null : clientHandles.get(server);
		if (socketClient == null) return false;
		try {
			socketClient.sendEnvelope(envelope);
			return true;
		} catch (RuntimeException e) {
			debug(e.getMessage());
			return false;
		}
	}

	private synchronized boolean sendHttpEnvelope(String server, JsonEnvelope envelope) {
		HttpProxyTransportServer transport = httpTransportServer;
		return transport != null && transport.send(server, envelope);
	}

	private void startHttpTransport() {
		try {
			URI endpoint = URI.create(getConfig().getHttpPublicEndpoint());
			if (!"https".equalsIgnoreCase(endpoint.getScheme()) || endpoint.getHost() == null
					|| endpoint.getPort() == 0 || endpoint.getPort() > 65535
					|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
					|| (endpoint.getPath() != null && !endpoint.getPath().isEmpty() && !"/".equals(endpoint.getPath()))) {
				throw new IllegalArgumentException("HTTP.PublicEndpoint must be an HTTPS origin");
			}
			File directory = new File(getDataFolderPlugin(), "http");
			HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.toPath(), endpoint.getHost());
			httpEnrollmentAuthority = new HttpEnrollmentAuthority(identity, directory.toPath());
			httpTransportServer = new HttpProxyTransportServer(
					new InetSocketAddress(getConfig().getHttpHost(), getConfig().getHttpPort()), identity,
					httpEnrollmentAuthority, received -> {
						GlobalMessageProxyHandler handler = globalMessageProxyHandler;
						if (handler == null) throw new IllegalStateException("HTTP message router is not ready");
						handler.onMessage(received.envelope());
					});
			httpTransportServer.start();
			logInfo("HTTP transport listening securely on " + getConfig().getHttpHost() + ":"
					+ httpTransportServer.port() + "; use /votingpluginbungee httpcode <server> for each backend");
		} catch (Exception failure) {
			closeHttpTransport();
			throw new IllegalStateException("HTTP transport could not start securely", failure);
		}
	}

	private synchronized void closeHttpTransport() {
		HttpProxyTransportServer transport = httpTransportServer;
		httpTransportServer = null;
		httpEnrollmentAuthority = null;
		if (transport != null) transport.close();
	}

	public String createHttpConnectionCode(String serverId) {
		HttpEnrollmentAuthority authority = httpEnrollmentAuthority;
		if (method != BungeeMethod.HTTP || authority == null) {
			throw new IllegalStateException("The HTTP transport is not running");
		}
		return authority.createConnectionCode(serverId, URI.create(getConfig().getHttpPublicEndpoint()), Duration.ofMinutes(15))
				.encode();
	}

	public void revokeHttpBackend(String serverId) {
		HttpEnrollmentAuthority authority = httpEnrollmentAuthority;
		if (method != BungeeMethod.HTTP || authority == null) throw new IllegalStateException("The HTTP transport is not running");
		authority.revoke(HttpTlsIdentity.canonicalServerId(serverId));
	}

	private synchronized void closeSocketClients() {
		HashMap<String, ClientHandler> clients = clientHandles;
		clientHandles = null;
		stopSocketClients(clients);
	}

	static void stopSocketClients(Map<String, ClientHandler> clients) {
		if (clients == null) return;
		for (ClientHandler client : clients.values()) {
			if (client == null) continue;
			try {
				client.stopConnection();
			} catch (RuntimeException ignored) {
				// Best effort: one broken client must not prevent the remaining sockets from closing.
			}
		}
	}

	private void warnUnsupportedDedicatedVotingProxyMode() {
		if (getConfig().getDedicatedVotingProxy() && (method == null || !method.supportsBackendPresence())) {
			logSevere("DedicatedVotingProxy requires MYSQL, REDIS, MQTT, SOCKETS, or HTTP; PLUGINMESSAGING is disabled for "
					+ "dedicated-proxy routing. Falling back to normal proxy routing.");
		}
	}

	public abstract void runAsync(Runnable run);

	/** Platform name used only for the transport-neutral Control discovery contract. */
	public abstract String getProxyPlatform();

	public abstract void runConsoleCommand(String command);

	public abstract void saveVoteCacheFile();

	public abstract void reloadCore(boolean mysql);

	/** Strict Control reload path; failures propagate so the caller can restore its backup. */
	public abstract void reloadControlConfiguration() throws Exception;

	public abstract boolean sendPluginMessageData(String server, String channel, byte[] data, boolean queue);

	private static final int PLUGIN_MESSAGE_HARD_LIMIT = 32767;
	private static final int PLUGIN_MESSAGE_SOFT_LIMIT = 30000;

	public void sendPluginMessageServer(String server, int delay, JsonEnvelope envelope) {
		getScheduler().schedule(() -> sendPluginMessageServerNow(server, envelope), delay * 5L, TimeUnit.MILLISECONDS);
	}

	/**
	 * Sends a plugin-message envelope immediately and reports whether the proxy
	 * accepted it for delivery.
	 *
	 * @param server target backend server
	 * @param envelope envelope to send
	 * @return true when the proxy accepted the message for delivery
	 */
	protected boolean sendPluginMessageServerNow(String server, JsonEnvelope envelope) {
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
			return false;
		}

		try (ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
				DataOutputStream out = new DataOutputStream(byteOutStream)) {
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
			out.flush();

			boolean sent = sendPluginMessageData(server, getConfig().getPluginMessageChannel().toLowerCase(),
					byteOutStream.toByteArray(), false);
			if (getConfig().getDebug()) {
				debug((sent ? "Sent" : "Could not send") + " plugin envelope (" + estimatedSize + " bytes) " + server
						+ " " + subChannel + " " + envelope.getFields());
			}
			return sent;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	static DefaultJedisClientConfig buildRedisClientConfig(VotingPluginProxyConfig configSource) {
		DefaultJedisClientConfig.Builder config = DefaultJedisClientConfig.builder()
				.database(configSource.getRedisDbIndex()).ssl(configSource.getRedisSsl()).connectionTimeoutMillis(2000)
				.socketTimeoutMillis(2000);
		if (configSource.getRedisSsl()) {
			SSLParameters sslParameters = new SSLParameters();
			sslParameters.setEndpointIdentificationAlgorithm("HTTPS");
			config.sslParameters(sslParameters);
		}
		if (configSource.getRedisUsername() != null && !configSource.getRedisUsername().isEmpty()) {
			config.user(configSource.getRedisUsername());
		}
		if (configSource.getRedisPassword() != null && !configSource.getRedisPassword().isEmpty()) {
			config.password(configSource.getRedisPassword());
		}
		return config.build();
	}

	public boolean sendRedisEnvelopeServer(String server, JsonEnvelope envelope) {
		return sendRedisEnvelopeServer(server, envelope, false);
	}

	private boolean sendRedisEnvelopeServer(String server, JsonEnvelope envelope, boolean useRetryCooldown) {
		JedisPool publisherPool = redisPublisherPool;
		if (publisherPool == null || (useRetryCooldown && System.currentTimeMillis() < redisPublisherRetryAfter)) {
			return false;
		}

		try (Jedis jedis = publisherPool.getResource()) {
			String channel = getConfig().getRedisPrefix() + "VotingPlugin_" + server;
			long subscribers = jedis.publish(channel,
					JsonEnvelopeCodec.encode(VotingPluginWire.withRedisDeliveryId(envelope)));
			redisPublisherRetryAfter = 0L;
			return subscribers > 0;
		} catch (Exception e) {
			if (useRetryCooldown) {
				// Standalone broadcasts remain queued, so their retries can be throttled safely.
				redisPublisherRetryAfter = System.currentTimeMillis() + 2000L;
			}
			debug(e.getMessage());
			return false;
		}
	}

	public boolean sendMqttEnvelopeServer(String server, JsonEnvelope envelope) {
		if (mqttHandler == null) {
			return false;
		}
		try {
			mqttHandler.publishEnvelope(getConfig().getMqttPrefix() + "votingplugin/servers/" + server, envelope);
			return true;
		} catch (Exception e) {
			if (getConfig().getDebug()) {
				e.printStackTrace();
			}
			return false;
		}
	}

	public boolean sendSocketEnvelopeServer(String server, JsonEnvelope envelope) {
		Map<String, Object> configuration = getConfig().getSpigotServerConfiguration(server);
		if (configuration == null) {
			return false;
		}
		String host = configuration.get("Host") instanceof String ? (String) configuration.get("Host") : "";
		int port = configuration.get("Port") instanceof Number ? ((Number) configuration.get("Port")).intValue() : 1298;
		if (host.isEmpty()) {
			return false;
		}

		String payload = JsonEnvelopeCodec.encode(envelope);
		String encoded = encryptionHandler != null ? encryptionHandler.encrypt(payload) : payload;
		try (Socket socket = new Socket()) {
			socket.connect(new InetSocketAddress(host, port), 2000);
			try (DataOutputStream output = new DataOutputStream(socket.getOutputStream())) {
				output.writeUTF(encoded);
				output.flush();
			}
			return true;
		} catch (Exception e) {
			debug(e.getMessage());
			return false;
		}
	}

	public void sendServerNameMessage() {
		for (String s : getAllAvailableServers()) {
			sendPluginMessageServer(s, 1, VotingPluginWire.serverName(s));
		}
	}

	public void sendVoteParty(String server) {
		if (isSomeoneOnlineServerForVoteRouting(server)) {
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
			if (!isSomeoneOnlineServerForVoteRouting(s)) {
				log("No players on server " + s + " to send test status message, please retest with someone online");
			} else {
				log("Sending request for status message on " + s);
				globalMessageProxyHandler.sendMessage(s, 1, VotingPluginWire.status(s));
			}
		}
	}

	/** Runs a correlated, non-vote round trip over the active backend transport. */
	public CompletableFuture<CommunicationTestResult> testBackendCommunication(String requestedServer,
			long timeoutMillis) {
		String server = requestedServer == null ? "" : requestedServer.trim();
		BungeeMethod activeMethod = method;
		if (server.isEmpty() || !getAllAvailableServers().contains(server)) {
			return CompletableFuture.completedFuture(CommunicationTestResult.failure(server, activeMethod,
					"UNKNOWN_BACKEND", "The backend is not configured on this proxy"));
		}
		if (activeMethod == null || globalMessageProxyHandler == null) {
			return CompletableFuture.completedFuture(CommunicationTestResult.failure(server, activeMethod,
					"TRANSPORT_UNAVAILABLE", "The proxy communication transport is not running"));
		}
		if (activeMethod == BungeeMethod.PLUGINMESSAGING && !isSomeoneOnlineServerForVoteRouting(server)) {
			return CompletableFuture.completedFuture(CommunicationTestResult.failure(server, activeMethod,
					"PLAYER_REQUIRED", "Plugin messaging requires an online player on the selected backend"));
		}
		ScheduledExecutorService scheduler = getScheduler();
		if (scheduler == null) {
			return CompletableFuture.completedFuture(CommunicationTestResult.failure(server, activeMethod,
					"TRANSPORT_UNAVAILABLE", "The proxy scheduler is not running"));
		}
		long boundedTimeout = Math.max(500L, Math.min(timeoutMillis, 30000L));
		UUID requestId = UUID.randomUUID();
		CompletableFuture<CommunicationTestResult> result = new CompletableFuture<>();
		PendingCommunicationTest pending = new PendingCommunicationTest(server, activeMethod, System.nanoTime(), result);
		pendingCommunicationTests.put(requestId, pending);
		result.whenComplete((ignored, failure) -> pendingCommunicationTests.remove(requestId, pending));
		try {
			if (!sendCommunicationTestEnvelopeNow(server, VotingPluginWire.status(server, requestId))) {
				result.complete(CommunicationTestResult.failure(server, activeMethod, "TRANSPORT_UNAVAILABLE",
						"The active transport could not accept the communication test"));
				return result;
			}
			scheduler.schedule(() -> result.complete(CommunicationTestResult.failure(server, activeMethod,
					"TIMEOUT", "No correlated reply arrived before the timeout")), boundedTimeout, TimeUnit.MILLISECONDS);
		} catch (RuntimeException failure) {
			result.complete(CommunicationTestResult.failure(server, activeMethod, "SEND_FAILED",
					"The proxy could not send the communication test"));
		}
		return result;
	}

	/** Sends a diagnostic immediately and reports whether the active transport accepted it. */
	protected boolean sendCommunicationTestEnvelopeNow(String server, JsonEnvelope envelope) {
		return sendProxyBroadcastEnvelopeNow(server, envelope);
	}

	protected void handleStatusOkay(JsonEnvelope message) {
		String server = message.getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
		String request = message.getFields().getOrDefault(VotingPluginWire.K_REQUEST_ID, "");
		if (request.isEmpty()) {
			log("Status okay for " + server);
			return;
		}
		UUID requestId;
		try {
			requestId = UUID.fromString(request);
		} catch (IllegalArgumentException ignored) {
			debug("Ignored status reply with an invalid request ID from " + server);
			return;
		}
		PendingCommunicationTest pending = pendingCommunicationTests.get(requestId);
		if (pending == null || !pending.server().equals(server)) {
			debug("Ignored unexpected status reply from " + server);
			return;
		}
		long roundTripMillis = Math.max(0L,
				TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - pending.startedAtNanos()));
		pending.result().complete(CommunicationTestResult.success(server, pending.method(), roundTripMillis));
	}

	private void cancelCommunicationTests(String message) {
		pendingCommunicationTests.forEach((requestId, pending) -> pending.result().complete(
				CommunicationTestResult.failure(pending.server(), pending.method(), "TRANSPORT_STOPPED", message)));
		pendingCommunicationTests.clear();
	}

	public record CommunicationTestResult(boolean success, String code, String message, String server,
			String method, long roundTripMillis) {
		private static CommunicationTestResult success(String server, BungeeMethod method, long roundTripMillis) {
			return new CommunicationTestResult(true, "OK", "Backend replied over the active transport", server,
					method == null ? "" : method.name(), roundTripMillis);
		}

		private static CommunicationTestResult failure(String server, BungeeMethod method, String code, String message) {
			return new CommunicationTestResult(false, code, message, server,
					method == null ? "" : method.name(), -1L);
		}
	}

	private record PendingCommunicationTest(String server, BungeeMethod method, long startedAtNanos,
			CompletableFuture<CommunicationTestResult> result) { }

	private void sendVoteDelayRejected(String player, String uuid, String service, boolean playerOnline,
			String playerServer) {
		if (!playerOnline || playerServer == null || !getAllAvailableServers().contains(playerServer)) {
			debug("Not sending vote delay rejection for " + player + " because the player is offline");
			return;
		}

		globalMessageProxyHandler.sendMessage(playerServer, 1,
				VotingPluginWire.voteDelayRejected(player, uuid, service, true));
	}

	public String getWaitUntilDelaySiteFromService(String service) {
		for (String site : getConfig().getWaitUntilVoteDelaySites()) {
			if (getConfig().getWaitUntilVoteDelayService(site).equalsIgnoreCase(service)) {
				return site;
			}
		}
		return "";
	}

	private long getLastVotesTime(String uuid, ArrayList<Column> cols, String site, String service, String player,
			boolean includeTimeChangeQueue) {
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

		if (includeTimeChangeQueue && player != null) {
			for (VoteTimeQueue queuedVote : getVoteCacheHandler().getTimeChangeQueue()) {
				if (queuedVote.getName().equalsIgnoreCase(player)
						&& queuedVote.getService().equalsIgnoreCase(service)) {
					mostRecentTime = Math.max(mostRecentTime, queuedVote.getTime());
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
		return checkVoteDelay(uuid, null, service, data, false);
	}

	/**
	 * Checks the configured vote delay, optionally including accepted votes waiting
	 * for a GlobalData time change to finish.
	 *
	 * @param uuid player UUID
	 * @param player player name used by the time-change queue
	 * @param service vote service
	 * @param data current player data
	 * @param includeTimeChangeQueue whether queued votes reserve their delay slot
	 * @return true when the vote may be accepted
	 */
	public boolean checkVoteDelay(String uuid, String player, String service, ArrayList<Column> data,
			boolean includeTimeChangeQueue) {
		String site = getWaitUntilDelaySiteFromService(service);
		if (site.isEmpty()) {
			debug("No service site set for " + service + ", skipping vote delay check");
			return true;
		}

		int voteDelay = getConfig().getWaitUntilVoteDelayVoteDelay(site);
		int voteDelayMin = getConfig().getWaitUntilVoteDelayVoteDelayMin(site);

		long lastVote = getLastVotesTime(uuid, data, site, service, player, includeTimeChangeQueue);
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
		vote(player, service, realVote, timeQueue, queueTime, text, uuid, null);
	}

	private enum QueuedVoteResult {
		SUCCESS, RETRY, TERMINAL
	}

	private synchronized QueuedVoteResult vote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid, VoteTimeQueue queuedVote) {
		try {
			if (!ServiceSiteValidator.isValid(service)) {
				warn("Rejected vote with invalid service site '" + ServiceSiteValidator.sanitizeForLog(service) + "'");
				return QueuedVoteResult.TERMINAL;
			}
			if (!MinecraftUsernameValidator.isValid(player, getConfig().getBedrockPlayerPrefix())) {
				warn("Rejected vote with invalid Minecraft username '"
						+ MinecraftUsernameValidator.sanitizeForLog(player) + "' from service '"
						+ MinecraftUsernameValidator.sanitizeForLog(service) + "'");
				return QueuedVoteResult.TERMINAL;
			}

			UUID voteId = queuedVote == null ? null : queuedVote.getVoteId();
			if (voteId == null) {
				voteId = UUID.randomUUID();
			}

			// UUID resolution
			if (!getConfig().getOnlineMode()) {
				uuid = getUUID(player);
			}

			if (uuid == null || uuid.isEmpty()) {
				uuid = getUUID(player);

				// Bedrock prefix auto-detect
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
					return QueuedVoteResult.TERMINAL;
				}
				if (!getConfig().getAllowUnJoined()) {
					log("Ignoring vote from " + player + " since player hasn't joined before");
					return QueuedVoteResult.TERMINAL;
				}
				if (!getConfig().getUUIDLookup()) {
					log("Failed to get uuid for " + player);
					return QueuedVoteResult.TERMINAL;
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
					return QueuedVoteResult.TERMINAL;
				}
				uuid = u.toString();
			}

			// Normalize UUID string if possible
			try {
				if (uuid != null && !uuid.isEmpty() && !uuid.equalsIgnoreCase("null")) {
					uuid = UUID.fromString(uuid.trim()).toString();
				}
			} catch (Exception ignored) {
				// ignore
			}

			player = getProperName(uuid, player);

			// Cache online state/server once (IMPORTANT for broadcast logic correctness)
			final boolean playerOnline = isPlayerOnlineForVoteRouting(player);
			final String playerServer = playerOnline ? getCurrentPlayerServerForVoteRouting(player) : null;
			long time = queueTime != 0 ? queueTime
					: LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();

			Set<String> broadcastTargets = queuedVote == null ? new LinkedHashSet<>()
					: new LinkedHashSet<>(queuedVote.getBroadcastTargets());
			Set<String> broadcastForwardedServers = queuedVote == null ? new LinkedHashSet<>()
					: new LinkedHashSet<>(queuedVote.getBroadcastForwardedServers());
			boolean proxyBroadcastHandled = queuedVote != null && queuedVote.isProxyBroadcastHandled();
			boolean processesTotals = getConfig().getPrimaryServer() || !getConfig().getMultiProxySupport();
			boolean managesTotals = processesTotals && getConfig().getBungeeManageTotals();
			boolean canValidateStandaloneBroadcast = canForwardStandaloneBroadcast(managesTotals);
			ArrayList<Column> data = null;
			boolean queueForTimeChange = false;

			// A completion callback can wipe totals and replay older queued votes. Run it
			// before loading this vote's database snapshot so the calculations below use
			// the post-rollover state.
			if (getConfig().getGlobalDataEnabled() && getGlobalDataHandler().isTimeChangedHappened()) {
				getGlobalDataHandler().checkForFinishedTimeChanges();
				queueForTimeChange = timeQueue && getGlobalDataHandler().isTimeChangedHappened();
			}

			// Validate the vote before any immediate announcement. This keeps duplicate
			// votes rejected by the delay check out of the GlobalData rollover queue and
			// prevents announcing a vote that will not be processed.
			if (managesTotals) {
				if (getProxyMySQL() == null) {
					logSevere("Mysql is not loaded correctly, stopping vote processing");
					return QueuedVoteResult.RETRY;
				}

				if (!getProxyMySQL().containsKeyQuery(uuid)) {
					getProxyMySQL().update(uuid, "PlayerName", new DataValueString(player));
					getProxyMySQL().getUuids().add(uuid);
				}

				data = getProxyMySQL().getExactQuery(new Column("uuid", new DataValueString(uuid)));
				if (!checkVoteDelay(uuid, player, service, data, queuedVote == null)) {
					log("Vote delay is not met for " + player + "/" + service + ", skipping vote");
					sendVoteDelayRejected(player, uuid, service, playerOnline, playerServer);
					return QueuedVoteResult.TERMINAL;
				}
			}

			// Forward an accepted offline broadcast before the still-active GlobalData
			// change queues the reward/totals work. The queued delivery state prevents
			// replaying broadcasts that already reached a backend.
			if (queueForTimeChange) {
				VoteTotalsSnapshot projectedTotals = managesTotals ? getProjectedRolloverTotals(data, player) : text;
				if (canValidateStandaloneBroadcast && proxyBroadcastDecider.usesImmediateForwarding(playerOnline)) {
					broadcastTargets.addAll(proxyBroadcastDecider.resolveTargets(false, null));
					proxyBroadcastHandled = true;
				}
				VoteTimeQueue delayedVote = new VoteTimeQueue(voteId, player, service, time,
						proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers,
						projectedTotals == null ? "" : projectedTotals.toString(), false, uuid);
				if (!getVoteCacheHandler().addTimeVoteToCache(delayedVote)) {
					logSevere("Unable to persist queued rollover vote for " + player + "/" + service
							+ "; skipping proxy broadcast");
					return QueuedVoteResult.RETRY;
				}
				if (proxyBroadcastHandled) {
					for (String target : broadcastTargets) {
						Set<String> forwarded = sendProxyBroadcast(Collections.singleton(target), uuid, player,
								service, time, projectedTotals == null ? "" : projectedTotals.toString(), false);
						if (delayedVote.getBroadcastForwardedServers().addAll(forwarded)) {
							broadcastForwardedServers.addAll(forwarded);
							persistTimeVoteDelivery(delayedVote);
						}
					}
				}
				log("Caching vote from " + player + "/" + service
						+ " because time change is happening right now");
				return QueuedVoteResult.SUCCESS;
			}

			addVoteParty();

			// Totals processing (primary server OR no multiproxy)
			if (processesTotals) {
				if (managesTotals) {
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

					if (getConfig().getLimitVotePoints() > 0 && points > getConfig().getLimitVotePoints()) {
						points = getConfig().getLimitVotePoints();
					}

					text = new VoteTotalsSnapshot(allTimeTotal, monthTotal, weeklyTotal, dailyTotal, points,
							votePartyVotes, currentVotePartyVotesRequired, dateMonthTotal);

					ArrayList<Column> update = new ArrayList<>();
					update.add(new Column("AllTimeTotal", new DataValueInt(allTimeTotal)));
					update.add(new Column("MonthTotal", new DataValueInt(monthTotal)));
					if (getConfig().getStoreMonthTotalsWithDate()) {
						update.add(new Column(getMonthTotalsWithDatePath(), new DataValueInt(dateMonthTotal)));
					}
					update.add(new Column("WeeklyTotal", new DataValueInt(weeklyTotal)));
					update.add(new Column("DailyTotal", new DataValueInt(dailyTotal)));
					update.add(new Column("Points", new DataValueInt(points)));

					debug("Setting totals " + text.toString() + ", voteId=" + voteId + " for " + player + "/"
							+ service);
					getProxyMySQL().update(uuid, update);
				} else {
					text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
				}
			}
			if (text == null) {
				text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
			}

			VoteLogStatus voteStatus = VoteLogStatus.IMMEDIATE;
			boolean standaloneProxyBroadcast = canValidateStandaloneBroadcast && (proxyBroadcastHandled
					|| proxyBroadcastDecider.usesImmediateForwarding(playerOnline));
			Set<String> proxyBroadcastTargets = Collections.emptySet();
			if (standaloneProxyBroadcast) {
				// A handled queued broadcast was necessarily sampled while the player was
				// offline. Retry only targets that did not previously accept delivery.
				proxyBroadcastTargets = proxyBroadcastHandled ? new LinkedHashSet<>(broadcastTargets)
						: proxyBroadcastDecider.resolveTargets(false, null);
				Set<String> remainingTargets = new LinkedHashSet<>(proxyBroadcastTargets);
				remainingTargets.removeAll(broadcastForwardedServers);
				broadcastForwardedServers.addAll(sendProxyBroadcast(remainingTargets, uuid, player, service, time,
						text == null ? "" : text.toString(), false));
			}

			// ===========================
			// Send vote(s) to backend(s)
			// ===========================
			if (getConfig().getSendVotesToAllServers()) {
				for (String s : getAllAvailableServers()) {

					boolean forceCache = getConfig().getWaitForUserOnline()
							&& (!playerOnline || playerServer == null || !playerServer.equalsIgnoreCase(s));

					if (forceCache) {
						debug("Forcing vote to cache for server " + s);
					}

					if ((!isSomeoneOnlineServerForVoteRouting(s) && method.requiresPlayerOnline()) || forceCache) {
						voteStatus = VoteLogStatus.CACHED;
						boolean broadcastForwarded = standaloneProxyBroadcast
								&& broadcastForwardedServers.containsAll(proxyBroadcastTargets);
						getVoteCacheHandler().addServerVote(s,
								new OfflineBungeeVote(voteId, player, uuid, service, time, realVote,
										text.toString(), broadcastForwarded, standaloneProxyBroadcast,
										proxyBroadcastTargets, broadcastForwardedServers, false));
						debug("Caching vote for " + player + " on " + service + " for " + s);
					} else {
						boolean broadcastHere = !broadcastForwardedServers.contains(s);
						if (broadcastHere && getConfig().getProxyBroadcastEnabled()) {
							Set<String> targets = standaloneProxyBroadcast ? proxyBroadcastTargets
									: proxyBroadcastDecider.resolveTargets(playerOnline, playerServer);
							broadcastHere = proxyBroadcastDecider.shouldBroadcast(s, targets);
						}

						if (!sendVoteEnvelopeAccepted(s, 2,
								VotingPluginWire.vote(player, uuid, service, time, true, realVote, text.toString(),
										voteId, getConfig().getBungeeManageTotals(), broadcastHere, 1, 1))) {
							voteStatus = VoteLogStatus.CACHED;
							boolean broadcastForwarded = standaloneProxyBroadcast
									&& broadcastForwardedServers.containsAll(proxyBroadcastTargets);
							getVoteCacheHandler().addServerVote(s,
									new OfflineBungeeVote(voteId, player, uuid, service, time, realVote,
											text.toString(), broadcastForwarded, standaloneProxyBroadcast,
											proxyBroadcastTargets, broadcastForwardedServers, false));
							debug("Caching vote after the transport rejected delivery for " + s);
						}
					}
				}
			} else {
				// Single-server mode: online goes to player server; otherwise queue as "online
				// vote"
				if (playerOnline && playerServer != null && getAllAvailableServers().contains(playerServer)) {
					String server = playerServer;

					boolean broadcastHere = !broadcastForwardedServers.contains(server);
					if (broadcastHere && getConfig().getProxyBroadcastEnabled()) {
						Set<String> targets = standaloneProxyBroadcast ? proxyBroadcastTargets
								: proxyBroadcastDecider.resolveTargets(true, playerServer);
						broadcastHere = proxyBroadcastDecider.shouldBroadcast(server, targets);
					}

					boolean rewardAccepted = sendVoteEnvelopeAccepted(server, 1,
							VotingPluginWire.voteOnline(player, uuid, service, time, true, realVote, text.toString(),
									voteId, getConfig().getBungeeManageTotals(), broadcastHere, 1, 1));
					if (!rewardAccepted) {
						voteStatus = VoteLogStatus.CACHED;
						boolean broadcastForwarded = standaloneProxyBroadcast
								&& broadcastForwardedServers.containsAll(proxyBroadcastTargets);
						getVoteCacheHandler().addOnlineVote(uuid,
								new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString(),
										broadcastForwarded, standaloneProxyBroadcast, proxyBroadcastTargets,
										broadcastForwardedServers, false));
						debug("Caching online vote after the transport rejected delivery for " + server);
					}

					if (rewardAccepted && canValidateStandaloneBroadcast && getConfig().getProxyBroadcastEnabled()
							&& !standaloneProxyBroadcast) {
						Set<String> targets = proxyBroadcastDecider.resolveTargets(true, playerServer);

						int bDelay = 2;
						for (String targetServer : targets) {
							// avoid double-broadcast on the same server that already got the voteOnline
							if (targetServer.equalsIgnoreCase(server)) {
								continue;
							}
							if (getConfig().getBlockedServers().contains(targetServer)) {
								continue;
							}

							globalMessageProxyHandler.sendMessage(targetServer, bDelay,
									VotingPluginWire.voteBroadcast(uuid, player, service, time,
											text == null ? "" : text.toString(), true));
							bDelay++;
						}
					}

					// multiproxy: envelope-only clear vote
					if (rewardAccepted && getConfig().getMultiProxySupport() && getConfig().getMultiProxyOneGlobalReward()) {
						multiProxyHandler.sendClearVote(uuid, player);
					}
				} else {
					voteStatus = VoteLogStatus.CACHED;
					boolean broadcastForwarded = standaloneProxyBroadcast
							&& broadcastForwardedServers.containsAll(proxyBroadcastTargets);
					getVoteCacheHandler().addOnlineVote(uuid,
							new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString(),
									broadcastForwarded, standaloneProxyBroadcast, proxyBroadcastTargets,
									broadcastForwardedServers, false));
					debug("Caching online vote for " + player + " on " + service);
				}

				int delay = 2;
				for (String s : getAllAvailableServers()) {
					globalMessageProxyHandler.sendMessage(s, delay + 1, VotingPluginWire.voteUpdate(uuid,
							votePartyVotes, currentVotePartyVotesRequired, service, time, text.toString()));
					delay += 2;
				}
			}

			// Vote logging
			if (voteLogMysqlTable != null && getConfig().getVoteLoggingEnabled()) {
				voteLogMysqlTable.logVote(voteId, voteStatus, service, uuid, player, time,
						getVoteCacheHandler().getProxyCachedTotal(uuid));
			}

			// ===========================
			// Multiproxy forwarding
			// ===========================
			if (getConfig().getMultiProxySupport() && getConfig().getPrimaryServer()) {
				if (!getConfig().getMultiProxyOneGlobalReward()) {
					debug("Sending global proxy vote envelope");
					multiProxyHandler.sendMultiProxyEnvelope(VotingPluginWire.vote(player, uuid, service, time, false,
							realVote, text == null ? "" : text.toString(), voteId, false, false, 1, 1));
				} else {
					// Only send to other proxies if the player DID NOT already receive reward on a
					// backend
					boolean shouldSend = true;
					if (playerOnline && playerServer != null) {
						if (!getConfig().getBlockedServers().contains(playerServer)) {
							shouldSend = false;
						}
					}

					if (shouldSend) {
						debug("Sending global proxy voteonline envelope");
						multiProxyHandler
								.sendMultiProxyEnvelope(VotingPluginWire.voteOnline(player, uuid, service, time, false,
										realVote, text == null ? "" : text.toString(), voteId, false, false, 1, 1));
					} else {
						debug("Not sending global proxy message for voteonline, player already got reward");
					}
				}
			}
			if (queuedVote != null) {
				queuedVote.setProcessed(true);
				if (!getVoteCacheHandler().updateTimeVote(queuedVote)) {
					warn("Unable to persist completed rollover vote " + queuedVote.getVoteId()
							+ "; attempting durable removal immediately");
				}
			}
			return QueuedVoteResult.SUCCESS;
		} catch (Exception e) {
			e.printStackTrace();
			return QueuedVoteResult.RETRY;
		}
	}

	private static final class PendingPresenceHandoff {
		private UUID requestId;
		private final UUID playerUuid;
		private final String playerName;
		private final String uuid;
		private final String server;
		private final UUID connectionId;
		private final UUID backendIncarnationId;
		private final long backendStartedAt;
		private final long conflictSequence;
		private final long createdAt;

		private PendingPresenceHandoff(String playerName, String uuid, String server, UUID connectionId,
				UUID backendIncarnationId, long backendStartedAt, long conflictSequence, long createdAt) {
			this.playerUuid = parsePlayerUuid(uuid);
			this.playerName = playerName;
			this.uuid = uuid;
			this.server = server;
			this.connectionId = connectionId;
			this.backendIncarnationId = backendIncarnationId;
			this.backendStartedAt = backendStartedAt;
			this.conflictSequence = conflictSequence;
			this.createdAt = createdAt;
		}

		private static UUID parsePlayerUuid(String uuid) {
			try {
				return UUID.fromString(uuid.trim());
			} catch (Exception ignored) {
				return null;
			}
		}
	}

	public abstract void warn(String message);

	public abstract ScheduledExecutorService getScheduler();
}
