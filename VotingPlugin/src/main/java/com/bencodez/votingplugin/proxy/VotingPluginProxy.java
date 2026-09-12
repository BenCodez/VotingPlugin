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
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Base64;
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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Queue;
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
import java.util.concurrent.atomic.AtomicReference;

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
import com.bencodez.simpleapi.servercomm.http.HttpEnrollmentAuthority;
import com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer;
import com.bencodez.simpleapi.servercomm.http.HttpTlsIdentity;
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
import com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
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
	public static final class VoteRetryException extends IllegalStateException {
		private static final long serialVersionUID = 1L;

		private VoteRetryException() {
			super("Vote processing could not be made durable; retry is required");
		}
	}
	private static final long PRESENCE_HANDOFF_TIMEOUT_MILLIS = TimeUnit.MINUTES.toMillis(2);
	private static final long PRESENCE_STARTUP_RESYNC_DELAY_SECONDS = 5L;
	private static final int MAX_LIVE_VOTE_RETRIES = 1024;
	private static final int MAX_MULTI_PROXY_VOTE_ATTEMPTS = 12;
	private static final int MAX_MULTI_PROXY_VOTE_RETRIES = 1024;
	private static final int MAX_COMPLETED_MULTI_PROXY_VOTES = 4096;
	private static final String FORWARDED_QUEUE_TOTALS_PREFIX = "\u0000VP-FWD:";
	private static final int FINAL_SHUTDOWN_PERSISTENCE_ATTEMPTS = 3;
	private static final String REWARD_JOURNAL_TARGET_PREFIX = "__vp_reward_target__:";
	private final Map<UUID, LiveVoteRetryState> liveVoteRetries = new LinkedHashMap<>();
	private final Map<UUID, MultiProxyVoteRetry> multiProxyVoteRetries = new LinkedHashMap<>();
	private final LinkedHashMap<UUID, Boolean> completedMultiProxyVotes = new LinkedHashMap<>();
	// Set only after all replacement gates have succeeded. Vote entry points are
	// synchronized, so no new side-effecting vote can race the handoff window.
	private boolean runtimeReplacementPrepared;

	private static final class LiveVoteRetryState {
		private String requestIdentity;
		private VoteTotalsSnapshot totals;
		private ArrayList<Column> totalsInput;
		private boolean votePartyApplied;
		private boolean totalsApplied;
		private final Set<String> broadcastForwardedServers = new LinkedHashSet<>();
		private final Set<String> deliveredRewardServers = new LinkedHashSet<>();
		private final Map<String, OfflineBungeeVote> rewardStates = new LinkedHashMap<>();
		private Set<String> rewardServers;
		private boolean rewardJournalsDurable;
		private boolean multiProxyForwardingHandled;
		private OfflineBungeeVote standaloneBroadcastState;
		private OfflineBungeeVote rewardJournalOwner;
		private OfflineBungeeVote pendingOnlineRewardState;
		private VoteTimeQueue queuedVote;
		private String player;
		private String service;
		private String uuid;
		private long time;
		private boolean realVote;

	}

	private final class MultiProxyVoteRetry implements Runnable {
		private enum Phase {
			EXECUTE,
			PERSIST_DEFERRED_RECEIPT,
			PERSIST_COMPLETION
		}

		private final String player;
		private final String service;
		private final boolean realVote;
		private final boolean timeQueue;
		private final long queueTime;
		private final VoteTotalsSnapshot totals;
		private final String uuid;
		private final UUID voteId;
		private final String origin;
		private int attempts;
		private boolean scheduled;
		private Phase phase = Phase.EXECUTE;

		private MultiProxyVoteRetry(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
				VoteTotalsSnapshot totals, String uuid, UUID voteId, String origin) {
			this.player = player;
			this.service = service;
			this.realVote = realVote;
			this.timeQueue = timeQueue;
			this.queueTime = queueTime;
			this.totals = totals;
			this.uuid = uuid;
			this.voteId = voteId;
			this.origin = origin == null ? "" : origin;
		}

		@Override
		public void run() {
			attemptMultiProxyVote(this);
		}
	}
	private static final long PRESENCE_MAINTENANCE_INTERVAL_SECONDS = 30L;
	private static final long PRESENCE_BACKEND_TIMEOUT_MILLIS = TimeUnit.SECONDS.toMillis(90);
	private static final long CONTROL_ENROLLMENT_MIN_INTERVAL_NANOS = TimeUnit.SECONDS.toNanos(10);
	private static final int MAX_PENDING_VOTE_PARTY_REWARDS = 1024;
	private static final long HTTP_TRANSPORT_RECONCILIATION_DELAY_MILLIS = 100L;
	// Acks run before SimpleAPI removes an entry. Keep one bounded, single-flight
	// poll armed while a replacement is deferred so state cleared after an ack (or
	// from the durable cache) cannot strand the old HTTP runtime indefinitely.
	private static final long HTTP_TRANSPORT_RECONCILIATION_POLL_MILLIS = 1_000L;
	private static final Map<Path, PreparedHttpTransport> PREPARED_HTTP_TRANSPORTS = new ConcurrentHashMap<>();

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
	private String liveHttpHost;
	private String liveHttpPublicEndpoint;
	private int liveHttpPort;

	private static final class PreparedHttpTransport {
		private final HttpProxyTransportServer server;
		private final HttpEnrollmentAuthority authority;
		private final AtomicReference<VotingPluginProxy> owner;
		private final String host;
		private final int port;
		private final String publicEndpoint;

		private PreparedHttpTransport(HttpProxyTransportServer server, HttpEnrollmentAuthority authority,
				AtomicReference<VotingPluginProxy> owner, String host, int port, String publicEndpoint) {
			this.server = server;
			this.authority = authority;
			this.owner = owner;
			this.host = host;
			this.port = port;
			this.publicEndpoint = publicEndpoint;
		}

		private boolean matches(VotingPluginProxyConfig config) {
			return java.util.Objects.equals(host, config.getHttpHost()) && port == config.getHttpPort()
					&& java.util.Objects.equals(publicEndpoint, config.getHttpPublicEndpoint());
		}

		private void close() {
			owner.set(null);
			server.close();
		}
	}

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
	private boolean votePartyDeliveryRetryScheduled;
	private boolean deferredHttpTransportReconciliation;
	private boolean httpTransportReconciliationScheduled;
	private boolean httpTransportReconciliationRunning;
	private long httpTransportReconciliationGeneration;
	private long votePartyProxyCommandAttemptSequence;
	private long votePartyProxyCommandInFlight;
	private volatile CompletableFuture<Void> votePartyProxyCommandExecution;
	private boolean votePartyProxyCommandCompletedUnpersisted;

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
		return sendProxyBroadcast(targets, uuid, player, service, time, text, wasOnline,
				(OfflineBungeeVote) null);
	}

	private Set<String> sendProxyBroadcast(Set<String> targets, String uuid, String player, String service, long time,
			String text, boolean wasOnline, OfflineBungeeVote cachedVote) {
		Set<String> forwarded = new LinkedHashSet<>();
		for (String targetServer : targets) {
			JsonEnvelope envelope = VotingPluginWire.voteBroadcast(uuid, player, service, time, text, wasOnline);
			boolean accepted = method == BungeeMethod.HTTP
					? sendHttpBroadcastEnvelopeWithRecovery(targetServer, envelope, cachedVote)
					: sendProxyBroadcastEnvelopeNow(targetServer, envelope);
			if (accepted) {
				forwarded.add(targetServer);
			}
		}
		return forwarded;
	}

	private Set<String> sendProxyBroadcast(Set<String> targets, String uuid, String player, String service, long time,
			String text, boolean wasOnline, VoteTimeQueue cachedVote) {
		Set<String> forwarded = new LinkedHashSet<>();
		for (String targetServer : targets) {
			// A timed vote can survive a proxy restart. Persist its HTTP delivery ID before
			// publication so a crash after the transport accepts it replays with the same
			// ID rather than creating a second backend broadcast.
			if (method == BungeeMethod.HTTP && !prepareTimedHttpBroadcastDelivery(targetServer, cachedVote)) {
				continue;
			}
			JsonEnvelope envelope = VotingPluginWire.voteBroadcast(uuid, player, service, time, text, wasOnline);
			boolean accepted = method == BungeeMethod.HTTP
					? sendHttpBroadcastEnvelopeWithRecovery(targetServer, envelope, cachedVote)
					: sendProxyBroadcastEnvelopeNow(targetServer, envelope);
			if (accepted) {
				forwarded.add(targetServer);
				if (method == BungeeMethod.HTTP) {
					cachedVote.getBroadcastForwardedServers().add(targetServer);
					// Persist completion before preparing another target. Otherwise persisting
					// that target's ID could leave this accepted target looking pending after a
					// crash, and it would be replayed under a newly generated ID.
					if (!persistTimeVoteDelivery(cachedVote)) break;
				} else if (cachedVote.getHttpBroadcastDeliveryId(targetServer) != null) {
					cachedVote.setHttpBroadcastDeliveryId(targetServer, null);
					cachedVote.setDeliveryStateDirty(true);
				}
			}
		}
		return forwarded;
	}

	private boolean prepareTimedHttpBroadcastDelivery(String server, VoteTimeQueue vote) {
		if (vote.getHttpBroadcastDeliveryId(server) != null) return true;
		vote.setHttpBroadcastDeliveryId(server, UUID.randomUUID().toString());
		vote.setDeliveryStateDirty(true);
		return persistTimeVoteDelivery(vote);
	}

	protected boolean sendHttpBroadcastEnvelopeWithRecovery(String server, JsonEnvelope envelope,
			OfflineBungeeVote cachedVote) {
		String persistedId = cachedVote == null ? null : cachedVote.getHttpBroadcastDeliveryId(server);
		String stableId = persistedId != null ? persistedId
				: stableCachedHttpDeliveryId("broadcast", server, envelope, cachedVote);
		try {
			boolean accepted = stableId == null ? sendProxyBroadcastEnvelopeNow(server, envelope)
					: sendHttpEnvelope(server, stableId, envelope);
			if (accepted && persistedId != null) {
				cachedVote.setHttpBroadcastDeliveryId(server, null);
				cachedVote.setDeliveryStateDirty(true);
			}
			return accepted;
		} catch (HttpProxyTransportServer.DeliveryRetryException failure) {
			if (cachedVote != null) {
				cachedVote.setHttpBroadcastDeliveryId(server, failure.deliveryId());
				cachedVote.setDeliveryStateDirty(true);
				// Let the caller persist the recovered ID before retrying. Retrying here
				// would create a crash window after acceptance but before durable cache state.
				return false;
			}
			try {
				boolean accepted = sendHttpEnvelope(server, failure.deliveryId(), envelope);
				return accepted;
			} catch (RuntimeException retryFailure) {
				debug("Unable to recover HTTP standalone delivery " + failure.deliveryId() + ": "
						+ retryFailure.getMessage());
				return false;
			}
		} catch (RuntimeException failure) {
			debug("Unable to send HTTP standalone delivery: " + failure.getMessage());
			return false;
		}
	}

	private boolean sendHttpBroadcastEnvelopeWithRecovery(String server, JsonEnvelope envelope,
			VoteTimeQueue cachedVote) {
		String stableId = cachedVote.getHttpBroadcastDeliveryId(server);
		if (stableId == null) {
			debug("Skipping HTTP timed broadcast without a persisted delivery ID for " + server);
			return false;
		}
		try {
			boolean accepted = sendHttpEnvelope(server, stableId, envelope);
			if (accepted) {
				cachedVote.setHttpBroadcastDeliveryId(server, null);
				cachedVote.setDeliveryStateDirty(true);
			}
			return accepted;
		} catch (RuntimeException failure) {
			debug("Unable to send HTTP timed broadcast delivery: " + failure.getMessage());
			return false;
		}
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
		return sendVoteEnvelopeAccepted(server, delay, envelope, null);
	}

	protected boolean sendVoteEnvelopeAccepted(String server, int delay, JsonEnvelope envelope,
			OfflineBungeeVote cachedVote) {
		if (method == BungeeMethod.HTTP) {
			return sendHttpEnvelopeWithRecovery(server, envelope, cachedVote);
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
										cache.getText(), false, cache);
								boolean broadcastChanged = cache.getBroadcastForwardedServers().addAll(forwarded);
								if (broadcastChanged) {
									cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
								}
								if ((broadcastChanged || cache.isDeliveryStateDirty())
										&& !persistServerVoteDelivery(server, cache)) continue;
							}
							if (cache.isRewardDelivered()) {
								if (cache.isProxyBroadcastHandled() && !cache.isProxyBroadcastComplete()) continue;
								removed.add(cache);
								continue;
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
								boolean broadcastHere = !cache.isProxyBroadcastHandled() && cache.needsBroadcastOn(server);
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
												broadcastHere, num, numberOfVotes), cache)) {
									debug("Retaining cached vote because the transport rejected delivery for " + server);
									persistServerVoteDelivery(server, cache);
									continue;
								}
								delay++;
								num++;
								cache.setRewardDelivered(true);
								if (!persistServerVoteDelivery(server, cache)) {
									continue;
								}
								if (cache.isProxyBroadcastHandled() && !cache.isProxyBroadcastComplete()) {
									continue;
								}
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
					for (OfflineBungeeVote cache : new ArrayList<>(c)) {
						if (isIncompleteRewardJournalOwner(cache)) {
							if (!materializeRewardJournalOwner(uuid, cache)) continue;
							if (!cache.isProxyBroadcastHandled() || cache.isProxyBroadcastComplete()) {
								if (!getVoteCacheHandler().tryRemoveOnlineVote(uuid, cache)) {
									scheduleCachedVoteDeliveryRetry();
								}
							}
							continue;
						}
						if (cache.isDeliveryStateDirty() && !persistOnlineVoteDelivery(uuid, cache)) {
							continue;
						}
						if (cache.isProxyBroadcastHandled()) {
							Set<String> pendingTargets = new LinkedHashSet<>(cache.getBroadcastTargets());
							pendingTargets.removeAll(cache.getBroadcastForwardedServers());
							List<String> blockedServers = getConfig().getBlockedServers();
							if (blockedServers != null) {
								pendingTargets.removeAll(blockedServers);
							}
							boolean broadcastChanged = cache.getBroadcastForwardedServers().addAll(sendProxyBroadcast(pendingTargets,
									cache.getUuid(), cache.getPlayerName(), cache.getService(), cache.getTime(),
									cache.getText(), false, cache));
							cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
							if ((broadcastChanged || cache.isDeliveryStateDirty())
									&& !persistOnlineVoteDelivery(uuid, cache)) continue;
						}
						boolean broadcastHere = !cache.isProxyBroadcastHandled() && cache.needsBroadcastOn(server);
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
											getConfig().getBungeeManageTotals(), broadcastHere, num, numberOfVotes), cache)) {
								debug("Retaining online vote because the transport rejected delivery for " + server);
								persistOnlineVoteDelivery(uuid, cache);
								continue;
							}
							cache.setRewardDelivered(true);
							if (!persistOnlineVoteDelivery(uuid, cache)) continue;
							deliveredReward = true;
							delay++;
							num++;
						}

						if (!cache.isProxyBroadcastHandled() || cache.isProxyBroadcastComplete()) {
							getVoteCacheHandler().removeOnlineVote(uuid, cache);
						}
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
				if (isIncompleteRewardJournalOwner(cache) && !materializeRewardJournalOwner(cachedUuid, cache)) {
					continue;
				}
				if (retryCompletedRewardJournalOwner(cachedUuid, cache)) {
					continue;
				}
				if (cache.isDeliveryStateDirty() && !persistOnlineVoteDelivery(cachedUuid, cache)) {
					continue;
				}
				if (!cache.isProxyBroadcastHandled() || !cache.needsBroadcastOn(server)) {
					continue;
				}
				Set<String> forwarded = sendProxyBroadcast(Collections.singleton(server), cache.getUuid(),
					cache.getPlayerName(), cache.getService(), cache.getTime(), cache.getText(), false, cache);
				if (cache.getBroadcastForwardedServers().addAll(forwarded)) {
					cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
					if (cache.isRewardDelivered() && cache.isProxyBroadcastComplete()) {
						getVoteCacheHandler().removeOnlineVote(cachedUuid, cache);
					} else {
						persistOnlineVoteDelivery(cachedUuid, cache);
					}
				} else if (cache.isDeliveryStateDirty()) {
					persistOnlineVoteDelivery(cachedUuid, cache);
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
					vote.getService(), vote.getTime(), vote.getTotals(), false, vote);
			if (vote.getBroadcastForwardedServers().addAll(forwarded) || vote.isDeliveryStateDirty()) {
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
		if (!getVoteCacheHandler().retryPendingVotePersistence()) {
			scheduleCachedVoteDeliveryRetry();
		}
		for (String cachedUuid : new LinkedHashSet<>(getVoteCacheHandler().getOnlineVoteUUIDs())) {
			for (OfflineBungeeVote cache : new ArrayList<>(getVoteCacheHandler().getOnlineVotes(cachedUuid))) {
				if (isIncompleteRewardJournalOwner(cache) && !materializeRewardJournalOwner(cachedUuid, cache)) {
					continue;
				}
				if (retryCompletedRewardJournalOwner(cachedUuid, cache)) {
					continue;
				}
				if (cache.isDeliveryStateDirty() && !persistOnlineVoteDelivery(cachedUuid, cache)) {
					continue;
				}
				if (cache.isProxyBroadcastHandled() && cache.isRewardDelivered()
						&& cache.isProxyBroadcastComplete()) {
					getVoteCacheHandler().removeOnlineVote(cachedUuid, cache);
					continue;
				}
				if (!cache.isProxyBroadcastHandled()) {
					continue;
				}
				Set<String> pendingTargets = new LinkedHashSet<>(cache.getBroadcastTargets());
				pendingTargets.removeAll(cache.getBroadcastForwardedServers());
				List<String> blockedServers = getConfig().getBlockedServers();
				if (blockedServers != null) {
					pendingTargets.removeAll(blockedServers);
				}
				Set<String> forwarded = sendProxyBroadcast(pendingTargets, cache.getUuid(), cache.getPlayerName(),
					cache.getService(), cache.getTime(), cache.getText(), false, cache);
				if (cache.getBroadcastForwardedServers().addAll(forwarded)) {
					cache.setBroadcastForwarded(cache.isProxyBroadcastComplete());
					if (cache.isRewardDelivered() && cache.isProxyBroadcastComplete()) {
						getVoteCacheHandler().removeOnlineVote(cachedUuid, cache);
					} else {
						persistOnlineVoteDelivery(cachedUuid, cache);
					}
				} else if (cache.isDeliveryStateDirty()) {
					persistOnlineVoteDelivery(cachedUuid, cache);
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
					vote.getTime(), vote.getTotals(), false, vote);
			if (vote.getBroadcastForwardedServers().addAll(forwarded) || vote.isDeliveryStateDirty()) {
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
		if (!getVoteCacheHandler().retryPendingVotePersistence()) {
			scheduleCachedVoteDeliveryRetry();
		}
		for (String server : getVoteCacheHandler().getCachedVotesServers()) {
			for (OfflineBungeeVote vote : new ArrayList<>(getVoteCacheHandler().getVotes(server))) {
				if (vote.isDeliveryStateDirty()) {
					persistServerVoteDelivery(server, vote);
				}
			}
		}
		for (String uuid : new LinkedHashSet<>(getVoteCacheHandler().getOnlineVoteUUIDs())) {
			for (OfflineBungeeVote vote : new ArrayList<>(getVoteCacheHandler().getOnlineVotes(uuid))) {
				if (retryCompletedRewardJournalOwner(uuid, vote)) continue;
				if (vote.isDeliveryStateDirty()) {
					persistOnlineVoteDelivery(uuid, vote);
				}
			}
		}
	}

	public synchronized void checkVoteParty() {
		if (!getConfig().getVotePartyEnabled()) return;
		if (votePartyVotes < currentVotePartyVotesRequired) {
			saveVoteCacheFile();
			return;
		}
		if (!retryPendingVotePartyProxyEffects()) {
			persistRetainedVotePartyThreshold();
			return;
		}
		PendingVotePartyProxyEffects stagedProxyEffects = PendingVotePartyProxyEffects.empty();
		if (method == BungeeMethod.HTTP) {
			try {
				stagedProxyEffects = new PendingVotePartyProxyEffects(getConfig().getVotePartyBroadcast(),
						getConfig().getVotePartyBungeeCommands());
			} catch (IllegalArgumentException oversized) {
				logSevere("HTTP vote-party proxy effects exceed the durable backlog limit; retaining the vote-party threshold");
				persistRetainedVotePartyThreshold();
				return;
			}
		}
		Collection<String> targets = getConfig().getVotePartySendToAllServers()
				? getAllAvailableServers() : getConfig().getVotePartyServersToSend();
		Map<String, String> onlineTargets = onlineVotePartyTargets(targets);
		if (method == BungeeMethod.HTTP && !canQueueVotePartyRewards(onlineTargets)) {
			try {
				saveVotePartyStateDurably();
			} catch (IOException failure) {
				throw new IllegalStateException("Unable to retain the full HTTP vote-party backlog", failure);
			}
			return;
		}

		Map<String, String> stagedRewards = new LinkedHashMap<>();
		if (method == BungeeMethod.HTTP) {
			for (String canonicalServer : onlineTargets.keySet()) {
				String deliveryId = UUID.randomUUID().toString();
				setVoteCachePendingVotePartyReward(canonicalServer, deliveryId, true);
				stagedRewards.put(canonicalServer, deliveryId);
			}
		}
		int previousVotes = votePartyVotes;
		int previousRequired = currentVotePartyVotesRequired;
		int previousIncrease = getVoteCacheVotePartyIncreaseVotesRequired();
		PendingVotePartyProxyEffects previousProxyEffects = method == BungeeMethod.HTTP
				? getVoteCachePendingVotePartyProxyEffects() : PendingVotePartyProxyEffects.empty();
		if (method == BungeeMethod.HTTP) setVoteCachePendingVotePartyProxyEffects(stagedProxyEffects);
		debug("Vote party reached");
		addCurrentVotePartyVotes(-currentVotePartyVotesRequired);
		currentVotePartyVotesRequired += getConfig().getVotePartyIncreaseVotesRequired();
		setVoteCacheVotePartyIncreaseVotesRequired(
				previousIncrease + getConfig().getVotePartyIncreaseVotesRequired());
		try {
			if (method == BungeeMethod.HTTP) saveVotePartyStateDurably();
			else saveVoteCacheFile();
		} catch (IOException | RuntimeException failure) {
			votePartyVotes = previousVotes;
			setVoteCacheVotePartyCurrentVotes(previousVotes);
			currentVotePartyVotesRequired = previousRequired;
			setVoteCacheVotePartyIncreaseVotesRequired(previousIncrease);
			for (Map.Entry<String, String> staged : stagedRewards.entrySet())
				setVoteCachePendingVotePartyReward(staged.getKey(), staged.getValue(), false);
			if (method == BungeeMethod.HTTP) setVoteCachePendingVotePartyProxyEffects(previousProxyEffects);
			throw failure instanceof RuntimeException runtime ? runtime
					: new IllegalStateException("Unable to persist HTTP vote-party rewards", failure);
		}

		if (method == BungeeMethod.HTTP) {
			if (retryPendingVotePartyProxyEffects()) retryPendingVotePartyRewards();
		} else {
			if (!getConfig().getVotePartyBroadcast().isEmpty()) broadcast(getConfig().getVotePartyBroadcast());
			for (String command : getConfig().getVotePartyBungeeCommands()) runConsoleCommand(command);
			for (String server : targets) sendVoteParty(server);
		}
	}

	private void persistRetainedVotePartyThreshold() {
		try {
			saveVotePartyStateDurably();
		} catch (IOException failure) {
			throw new IllegalStateException("Unable to retain the HTTP vote-party threshold", failure);
		}
	}

	private Map<String, String> onlineVotePartyTargets(Collection<String> targets) {
		Map<String, String> online = new LinkedHashMap<>();
		for (String server : targets) if (isSomeoneOnlineServerForVoteRouting(server))
			online.putIfAbsent(server.toLowerCase(Locale.ROOT), server);
		return online;
	}

	private boolean canQueueVotePartyRewards(Map<String, String> targets) {
		for (String server : targets.keySet()) {
			Collection<String> pending = getVoteCachePendingVotePartyRewardIds(server);
			if (pending != null && pending.size() >= MAX_PENDING_VOTE_PARTY_REWARDS) {
				logSevere("HTTP vote-party reward backlog is full for " + targets.get(server)
						+ "; retaining the vote-party threshold");
				return false;
			}
		}
		return true;
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

	public abstract Collection<String> getVoteCachePendingVotePartyServers();

	public abstract Collection<String> getVoteCachePendingVotePartyRewardIds(String server);

	public abstract PendingVotePartyProxyEffects getVoteCachePendingVotePartyProxyEffects();

	public abstract PendingVotePartyProxyEffects getVoteCacheQuarantinedVotePartyProxyEffects();

	public abstract void saveVotePartyStateDurably() throws IOException;

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
		method = retainHttpForPendingDeliveries(method);

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
					sendGenericHttpEnvelope(server, envelope);
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
		// Open the listener last: backend callbacks can immediately reach routing,
		// presence, vote-log, multi-proxy, and Control-adjacent runtime helpers.
		if (method.equals(BungeeMethod.HTTP)) {
			startHttpTransport();
		}
		scheduleVotePartyDeliveryRetry();

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

			@Override
			public void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
					VoteTotalsSnapshot text, String uuid, UUID voteId) {
				receiveMultiProxyVote(player, service, realVote, timeQueue, queueTime, text, uuid, voteId);
			}

			@Override
			public void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
					VoteTotalsSnapshot text, String uuid, UUID voteId, String origin) {
				receiveMultiProxyVote(player, service, realVote, timeQueue, queueTime, text, uuid, voteId, origin);
			}

			@Override
			public void onMultiProxyVoteAcknowledged(UUID voteId, String recipient) {
				handleMultiProxyVoteAcknowledgement(voteId, recipient);
			}

			@Override
			public void onMultiProxyVoteRetirementAcknowledged(UUID voteId, String recipient) {
				handleMultiProxyVoteRetirementAcknowledgement(voteId, recipient);
			}

			@Override
			public void onMultiProxyVoteRetirementRequested(UUID voteId, String origin) {
				handleMultiProxyVoteRetirementRequest(voteId, origin);
			}
		};
		multiProxyHandler.loadMultiProxySupport();
	}

	/** Receives and locally retries a forwarded vote under its wire-stable identity. */
	protected synchronized void receiveMultiProxyVote(String player, String service, boolean realVote,
			boolean timeQueue, long queueTime, VoteTotalsSnapshot totals, String uuid, UUID wireVoteId) {
		receiveMultiProxyVote(player, service, realVote, timeQueue, queueTime, totals, uuid, wireVoteId, "");
	}

	/** Receives a reliable envelope and retains its sender only for a later ACK. */
	protected synchronized void receiveMultiProxyVote(String player, String service, boolean realVote,
			boolean timeQueue, long queueTime, VoteTotalsSnapshot totals, String uuid, UUID wireVoteId, String origin) {
		UUID voteId = wireVoteId == null ? UUID.randomUUID() : wireVoteId;
		if (completedMultiProxyVotes.containsKey(voteId)) {
			acknowledgeCompletedMultiProxyVote(voteId, origin);
			return;
		}
		if (getVoteCacheHandler().hasMultiProxyVoteCompletion(voteId)) {
			rememberCompletedMultiProxyVote(voteId);
			acknowledgeCompletedMultiProxyVote(voteId, origin);
			return;
		}
		MultiProxyVoteRetry retry = multiProxyVoteRetries.get(voteId);
		if (retry == null) {
			if (multiProxyVoteRetries.size() >= MAX_MULTI_PROXY_VOTE_RETRIES) {
				// Do not drop a sender delivery when the in-memory retry fence is full.
				// The ordinary timed-vote cache is bounded by durable storage rather than
				// this process heap and is loaded again after a restart. It also lets the
				// normal queue processor drain the spill as retry slots become available.
				if (retainForwardedVoteOverflow(player, service, realVote, totals, uuid, voteId, queueTime, origin)) {
					scheduleTimeVoteRetry();
					return;
				}
				logSevere("Unable to durably retain forwarded multi-proxy vote while the bounded retry queue is full");
				return;
			}
			retry = new MultiProxyVoteRetry(player, service, realVote, timeQueue, queueTime, totals, uuid, voteId, origin);
			multiProxyVoteRetries.put(voteId, retry);
		}
		if (!retry.scheduled) attemptMultiProxyVote(retry);
	}

	/** Durably spills an over-capacity forwarded vote into the normal replay queue. */
	private boolean retainForwardedVoteOverflow(String player, String service, boolean realVote,
			VoteTotalsSnapshot totals, String uuid, UUID voteId, long queueTime, String origin) {
		if (voteId == null || player == null || service == null || uuid == null) return false;
		for (VoteTimeQueue queued : getVoteCacheHandler().getTimeChangeQueue()) {
			if (voteId.equals(queued.getVoteId())) return true;
		}
		long time = queueTime == 0L ? System.currentTimeMillis() : queueTime;
		VoteTimeQueue queued = new VoteTimeQueue(voteId, player, service, time, false,
				Collections.emptySet(), Collections.emptySet(), totals == null ? "" : totals.toString(), false, uuid);
		queued.setRealVote(realVote);
		queued.setMultiProxyOrigin(origin == null ? "" : origin);
		return getVoteCacheHandler().addTimeVoteToCache(queued);
	}

	private static String[] decodeForwardedQueueTotals(String encoded) {
		if (encoded == null || !encoded.startsWith(FORWARDED_QUEUE_TOTALS_PREFIX)) {
			return new String[] { "true", encoded == null ? "" : encoded };
		}
		int flagIndex = FORWARDED_QUEUE_TOTALS_PREFIX.length();
		int separator = flagIndex + 1;
		if (encoded.length() <= separator || (encoded.charAt(flagIndex) != '0' && encoded.charAt(flagIndex) != '1')
				|| encoded.charAt(separator) != ':') {
			return new String[] { "true", encoded };
		}
		char realVote = encoded.charAt(FORWARDED_QUEUE_TOTALS_PREFIX.length());
		try {
			String decoded = new String(Base64.getUrlDecoder().decode(encoded.substring(separator + 1)), StandardCharsets.UTF_8);
			return new String[] { realVote == '1' ? "true" : "false", decoded };
		} catch (IllegalArgumentException invalidEncoding) {
			return new String[] { "true", encoded };
		}
	}

	private synchronized void attemptMultiProxyVote(MultiProxyVoteRetry retry) {
		attemptMultiProxyVote(retry, true);
	}

	private synchronized void attemptMultiProxyVote(MultiProxyVoteRetry retry, boolean allowSchedule) {
		if (multiProxyVoteRetries.get(retry.voteId) != retry) return;
		retry.scheduled = false;
		if (!enabled) return;
		if (retry.phase == MultiProxyVoteRetry.Phase.PERSIST_DEFERRED_RECEIPT) {
			if (persistDeferredMultiProxyReceipt(retry)) {
				multiProxyVoteRetries.remove(retry.voteId);
			} else {
				scheduleMultiProxyVoteRetry(retry, allowSchedule, "deferred-receipt persistence");
			}
			return;
		}
		if (retry.phase == MultiProxyVoteRetry.Phase.PERSIST_COMPLETION) {
			if (getVoteCacheHandler().hasMultiProxyVoteCompletion(retry.voteId)
					|| getVoteCacheHandler().markMultiProxyVoteCompletedDurably(retry.voteId)) {
				completeMultiProxyVote(retry);
			} else {
				scheduleMultiProxyVoteRetry(retry, allowSchedule, "completion-record persistence");
			}
			return;
		}
		try {
			vote(retry.player, retry.service, retry.realVote, retry.timeQueue, retry.queueTime, retry.totals,
					retry.uuid, retry.voteId);
			if (retry.timeQueue && findUnprocessedQueuedVote(retry.voteId) != null) {
				retry.phase = MultiProxyVoteRetry.Phase.PERSIST_DEFERRED_RECEIPT;
				retry.attempts = 0;
				if (persistDeferredMultiProxyReceipt(retry)) multiProxyVoteRetries.remove(retry.voteId);
				else scheduleMultiProxyVoteRetry(retry, allowSchedule, "deferred-receipt persistence");
				return;
			}
			retry.phase = MultiProxyVoteRetry.Phase.PERSIST_COMPLETION;
			retry.attempts = 0;
			if (getVoteCacheHandler().markMultiProxyVoteCompletedDurably(retry.voteId)) {
				completeMultiProxyVote(retry);
			} else {
				scheduleMultiProxyVoteRetry(retry, allowSchedule, "completion-record persistence");
			}
		} catch (VoteRetryException retryable) {
			scheduleMultiProxyVoteRetry(retry, allowSchedule, "durable-storage");
		}
	}

	/** Binds a deferred receiver queue to its origin before dropping its live retry fence. */
	private boolean persistDeferredMultiProxyReceipt(MultiProxyVoteRetry retry) {
		VoteTimeQueue queued = findUnprocessedQueuedVote(retry.voteId);
		if (queued == null) return false;
		queued.setMultiProxyOrigin(retry.origin);
		queued.setRealVote(retry.realVote);
		queued.setDeliveryStateDirty(true);
		return persistTimeVoteDelivery(queued);
	}

	private VoteTimeQueue findUnprocessedQueuedVote(UUID voteId) {
		if (voteId == null) return null;
		for (VoteTimeQueue queued : getVoteCacheHandler().getTimeChangeQueue()) {
			if (voteId.equals(queued.getVoteId()) && !queued.isProcessed()) return queued;
		}
		return null;
	}

	private void completeMultiProxyVote(MultiProxyVoteRetry retry) {
		multiProxyVoteRetries.remove(retry.voteId);
		rememberCompletedMultiProxyVote(retry.voteId);
		acknowledgeCompletedMultiProxyVote(retry.voteId, retry.origin);
	}

	private void acknowledgeCompletedMultiProxyVote(UUID voteId, String origin) {
		if (origin == null || origin.isBlank() || multiProxyHandler == null) return;
		multiProxyHandler.acknowledgeMultiProxyVote(voteId, origin);
	}

	private synchronized void handleMultiProxyVoteRetirementRequest(UUID voteId, String origin) {
		if (voteId == null || origin == null || origin.isBlank() || multiProxyHandler == null) return;
		for (VoteTimeQueue queued : getVoteCacheHandler().getTimeChangeQueue()) {
			if (!voteId.equals(queued.getVoteId())) continue;
			// A completion tombstone can be the only durable proof while deletion of
			// the receiver's processed queue row is retrying. Retire that row first,
			// otherwise deleting the tombstone would let the local queue replay it.
			if (!queued.isProcessed() || !origin.equalsIgnoreCase(queued.getMultiProxyOrigin())
					|| !getVoteCacheHandler().removeTimeVote(queued)) return;
			break;
		}
		if (!getVoteCacheHandler().removeMultiProxyVoteCompletion(voteId)) return;
		removeCompletedMultiProxyVote(voteId);
		multiProxyHandler.acknowledgeMultiProxyVoteRetirement(voteId, origin);
	}

	private void scheduleMultiProxyVoteRetry(MultiProxyVoteRetry retry, boolean allowSchedule, String reason) {
		retry.attempts++;
		ScheduledExecutorService scheduler = getScheduler();
		if (!allowSchedule || retry.attempts >= MAX_MULTI_PROXY_VOTE_ATTEMPTS
				|| scheduler == null || scheduler.isShutdown()) {
			if (!allowSchedule) return;
			logSevere("Forwarded multi-proxy vote remains fenced after bounded " + reason + " retries for "
					+ MinecraftUsernameValidator.sanitizeForLog(retry.player));
			return;
		}
		retry.scheduled = true;
		try {
			scheduler.schedule(retry, 5, TimeUnit.SECONDS);
		} catch (RuntimeException schedulingFailure) {
			retry.scheduled = false;
			logSevere("Unable to schedule a forwarded multi-proxy vote retry; its live side-effect fence was retained");
		}
	}

	private void rememberCompletedMultiProxyVote(UUID voteId) {
		completedMultiProxyVotes.put(voteId, Boolean.TRUE);
		while (completedMultiProxyVotes.size() > MAX_COMPLETED_MULTI_PROXY_VOTES) {
			UUID oldest = completedMultiProxyVotes.keySet().iterator().next();
			removeCompletedMultiProxyVote(oldest);
		}
	}

	private void removeCompletedMultiProxyVote(UUID voteId) {
		completedMultiProxyVotes.remove(voteId);
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
		invalidateDeferredHttpTransportReconciliation();
		if (waitForHosted) {
			prepareForRuntimeReplacement();
		} else {
			boolean liveRetriesSettled;
			try {
				liveRetriesSettled = settleMultiProxyAndLiveVotesForFinalShutdown();
			} catch (RuntimeException failure) {
				liveRetriesSettled = false;
			}
			if (!liveRetriesSettled) {
				logSevere("Unable to durably quarantine live vote retries after bounded final-shutdown persistence attempts; operator reconciliation may be required after restart");
			}
			if (!quarantineInFlightVotePartyProxyCommandForReplacement()) {
				awaitInFlightVotePartyProxyCommand();
				if (!quarantineInFlightVotePartyProxyCommandForReplacement()) {
					logSevere("Unable to durably quarantine an in-flight HTTP vote-party command after the bounded final-shutdown wait; operator reconciliation may be required after restart");
				}
			}
			cancelPreparedHttpTransportChange();
			controlServicesGeneration.incrementAndGet();
			controlLifecycleExecutor.shutdownNow();
			stopControlServices(false);
		}
		completeRuntimeReplacementShutdown();
	}

	/** Settles consumed multi-proxy envelopes together with any live phase fences they own. */
	protected synchronized boolean settleMultiProxyAndLiveVotesForFinalShutdown() {
		for (int attempt = 0; attempt < FINAL_SHUTDOWN_PERSISTENCE_ATTEMPTS; attempt++) {
			for (MultiProxyVoteRetry retry : new ArrayList<>(multiProxyVoteRetries.values())) {
				retry.scheduled = false;
				attemptMultiProxyVote(retry, false);
			}
			Set<UUID> activeForwardedVotes = new LinkedHashSet<>();
			for (UUID voteId : multiProxyVoteRetries.keySet()) {
				if (liveVoteRetries.containsKey(voteId)) activeForwardedVotes.add(voteId);
			}
			boolean liveSettled = settleLiveVoteRetriesForFinalShutdown();
			for (UUID voteId : activeForwardedVotes) {
				if (!liveVoteRetries.containsKey(voteId)) {
					MultiProxyVoteRetry retry = multiProxyVoteRetries.get(voteId);
					if (retry != null) {
						retry.phase = MultiProxyVoteRetry.Phase.PERSIST_COMPLETION;
						retry.attempts = 0;
						attemptMultiProxyVote(retry, false);
					}
				}
			}
			if (liveSettled && multiProxyVoteRetries.isEmpty()) return true;
		}
		// Platform disable callers always continue teardown. A receiver whose vote
		// side effects completed but whose separate tombstone could not be written
		// must therefore be represented by the ordinary durable queue, not retained
		// only by this process's retry map.
		if (liveVoteRetries.isEmpty() && quarantineCompletedMultiProxyRetriesForShutdown()) return true;
		return liveVoteRetries.isEmpty() && multiProxyVoteRetries.isEmpty();
	}

	private boolean quarantineCompletedMultiProxyRetriesForShutdown() {
		VoteCacheHandler cache = getVoteCacheHandler();
		for (MultiProxyVoteRetry retry : new ArrayList<>(multiProxyVoteRetries.values())) {
			if (retry.phase != MultiProxyVoteRetry.Phase.PERSIST_COMPLETION
					|| !retainCompletedMultiProxyRetry(cache, retry)) return false;
			multiProxyVoteRetries.remove(retry.voteId);
		}
		return true;
	}

	/** Persists a completed receiver phase without acknowledging until its tombstone succeeds. */
	private boolean retainCompletedMultiProxyRetry(VoteCacheHandler cache, MultiProxyVoteRetry retry) {
		if (retry.voteId == null) return false;
		for (VoteTimeQueue queued : cache.getTimeChangeQueue()) {
			if (!retry.voteId.equals(queued.getVoteId())) continue;
			// An unprocessed row owns deferred vote effects. Do not convert it into a
			// completion fence: its origin is persisted by PERSIST_DEFERRED_RECEIPT.
			if (!queued.isProcessed()) return false;
			queued.setProcessed(true);
			queued.setRealVote(retry.realVote);
			queued.setMultiProxyOrigin(retry.origin);
			queued.setMultiProxyCompletionPending(true);
			queued.setDeliveryStateDirty(true);
			return cache.updateTimeVote(queued);
		}
		VoteTimeQueue quarantine = new VoteTimeQueue(retry.voteId, retry.player, retry.service,
				retry.queueTime == 0L ? System.currentTimeMillis() : retry.queueTime, false,
				Collections.emptySet(), Collections.emptySet(), retry.totals == null ? "" : retry.totals.toString(), true,
				retry.uuid);
		quarantine.setRealVote(retry.realVote);
		quarantine.setMultiProxyOrigin(retry.origin);
		quarantine.setMultiProxyCompletionPending(true);
		return cache.addTimeVoteToCache(quarantine);
	}

	/** Moves every in-memory live retry into the ordinary durable vote outbox before final teardown. */
	protected synchronized boolean settleLiveVoteRetriesForFinalShutdown() {
		if (liveVoteRetries.isEmpty()) return true;
		VoteCacheHandler cache = getVoteCacheHandler();
		boolean retained = true;
		for (LiveVoteRetryState retry : liveVoteRetries.values()) {
			if (retry.queuedVote != null) {
				Queue<VoteTimeQueue> queuedVotes = cache.getTimeChangeQueue();
				if (queuedVotes == null || !queuedVotes.contains(retry.queuedVote)) {
					retained &= cache.addTimeVoteToCache(retry.queuedVote);
				}
			}
			Set<OfflineBungeeVote> onlineStates = Collections.newSetFromMap(new java.util.IdentityHashMap<>());
			if (retry.rewardJournalOwner != null) onlineStates.add(retry.rewardJournalOwner);
			if (retry.standaloneBroadcastState != null) onlineStates.add(retry.standaloneBroadcastState);
			if (retry.pendingOnlineRewardState != null) onlineStates.add(retry.pendingOnlineRewardState);
			for (OfflineBungeeVote state : onlineStates) {
				retained &= cache.retainOnlineVoteForPersistenceRetry(state.getUuid(), state);
			}
			for (Map.Entry<String, OfflineBungeeVote> entry : retry.rewardStates.entrySet()) {
				String server = entry.getKey();
				for (String configured : getAllConfiguredServers()) {
					if (configured.equalsIgnoreCase(server)) {
						server = configured;
						break;
					}
				}
				retained &= cache.retainServerVoteForPersistenceRetry(server, entry.getValue());
			}
		}
		for (int attempt = 0; attempt < FINAL_SHUTDOWN_PERSISTENCE_ATTEMPTS; attempt++) {
			if (!retained || !cache.retryPendingVotePersistence()) continue;
			for (Map.Entry<UUID, LiveVoteRetryState> entry : new ArrayList<>(liveVoteRetries.entrySet())) {
				LiveVoteRetryState retry = entry.getValue();
				QueuedVoteResult result = vote(retry.player, retry.service, retry.realVote, false, retry.time,
						retry.totals, retry.uuid, retry.queuedVote, entry.getKey());
				if (result == QueuedVoteResult.TERMINAL) liveVoteRetries.remove(entry.getKey());
			}
			if (liveVoteRetries.isEmpty()) return true;
		}
		return false;
	}

	/** Fail-closed gate that must complete before a replacement proxy runtime is created. */
	public synchronized void prepareForRuntimeReplacement() {
		// Live retry markers fence side effects (including vote-party and totals) that
		// have already run but whose durable outbox write has not. They are owned by
		// this runtime only, so replacing it would let the scheduled listener retry
		// start with an empty map and reapply those effects. Keep this runtime alive
		// until the retry settles instead of dropping or replaying an uncertain vote.
		if (!liveVoteRetries.isEmpty() || !multiProxyVoteRetries.isEmpty()) {
			throw new IllegalStateException(
					"Live vote retries or forwarded multi-proxy vote retries must settle before proxy runtime replacement");
		}
		if (!quarantineInFlightVotePartyProxyCommandForReplacement()) {
			throw new IllegalStateException("In-flight vote-party command must be durably quarantined before proxy runtime replacement");
		}
		controlServicesGeneration.incrementAndGet();
		synchronized (controlLifecycleLock) {
			ControlConnector connector = controlConnector;
			if (connector != null && !connector.reserveRuntimeReplacement()) {
				throw new IllegalStateException("Control result must be acknowledged before proxy runtime replacement");
			}
			controlLifecycleExecutor.shutdown();
			stopControlServicesLocked(true);
		}
		runtimeReplacementPrepared = true;
	}

	/** Best-effort remainder of runtime teardown after the Control overlap gate has succeeded. */
	public void completeRuntimeReplacementShutdown() {
		enabled = false;
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
			if (vote.isMultiProxyCompletionPending()) {
				if (!getVoteCacheHandler().hasMultiProxyVoteCompletion(vote.getVoteId())
						&& !getVoteCacheHandler().markMultiProxyVoteCompletedDurably(vote.getVoteId())) {
					scheduleTimeVoteRetry();
					return;
				}
				acknowledgeCompletedMultiProxyVote(vote.getVoteId(), vote.getMultiProxyOrigin());
				if (!getVoteCacheHandler().removeTimeVote(vote)) {
					scheduleTimeVoteRetry();
					return;
				}
				continue;
			}
			if (!vote.isProcessed() && getVoteCacheHandler().hasTimeVoteCompletion(vote)) {
				vote.setProcessed(true);
				vote.setDeliveryStateDirty(true);
			}
			if (vote.isProcessed() && vote.hasPendingHttpBroadcastDeliveryIds()) {
				// The reward/totals work is already complete. Only retry the durable
				// standalone broadcasts; removing this row would lose their stable IDs.
				retryPendingTimeBroadcasts();
				if (vote.hasPendingHttpBroadcastDeliveryIds()) {
					scheduleTimeVoteRetry();
					return;
				}
			}
			if (vote.isProcessed() && vote.isMultiProxyForwardingRequired()
					&& !vote.isMultiProxyForwardingHandled()) {
				if (!retryDurableMultiProxyOutbox(vote)) {
					scheduleTimeVoteRetry();
					return;
				}
			}
			if (vote.isProcessed() && vote.isDeliveryStateDirty() && !persistTimeVoteDelivery(vote)) {
				scheduleTimeVoteRetry();
				return;
			}
			// A direct listener retry can still be queued after its ACK outbox completes.
			// Keep that in-memory fence until the listener consumes it; queued-vote
			// processing removes its own fence in vote() when no listener retry exists.
			if (vote.isProcessed() && !vote.isMultiProxyForwardingRequired()) {
				liveVoteRetries.remove(vote.getVoteId());
			}
			if (!vote.isProcessed()) {
				String[] forwardedTotals = decodeForwardedQueueTotals(vote.getTotals());
				boolean queuedRealVote = vote.isRealVote();
				// Existing NUL-prefixed emergency rows predate the explicit realVote
				// column. Decode them once for compatibility, while new rows preserve
				// PostgreSQL-safe plain totals.
				if (vote.getTotals() != null && vote.getTotals().startsWith(FORWARDED_QUEUE_TOTALS_PREFIX)) {
					queuedRealVote = Boolean.parseBoolean(forwardedTotals[0]);
					vote.setTotals(forwardedTotals[1]);
					vote.setRealVote(queuedRealVote);
					vote.setDeliveryStateDirty(true);
				}
				VoteTotalsSnapshot queuedTotals = forwardedTotals[1].isEmpty() ? null
						: VoteTotalsSnapshot.parseStorage(forwardedTotals[1]);
				QueuedVoteResult result = vote(vote.getName(), vote.getService(), queuedRealVote, false, vote.getTime(), queuedTotals,
						vote.getUuid(), vote);
				if (result == QueuedVoteResult.RETRY) {
					scheduleTimeVoteRetry();
					return;
				}
				if ((result == QueuedVoteResult.SUCCESS || result == QueuedVoteResult.TERMINAL)
						&& !vote.getMultiProxyOrigin().isBlank()) {
					if (!getVoteCacheHandler().markMultiProxyVoteCompletedDurably(vote.getVoteId())) {
						scheduleTimeVoteRetry();
						return;
					}
					acknowledgeCompletedMultiProxyVote(vote.getVoteId(), vote.getMultiProxyOrigin());
				}
				if (result == QueuedVoteResult.TERMINAL) {
					warn("Removing terminal rollover vote " + vote.getVoteId() + " for " + vote.getName() + "/"
							+ ServiceSiteValidator.sanitizeForLog(vote.getService()));
				}
				if (!getVoteCacheHandler().getTimeChangeQueue().contains(vote)) {
					getVoteCacheHandler().clearTimeVoteCompletion(vote);
					continue;
				}
			}
			if (!getVoteCacheHandler().removeTimeVote(vote)) {
				scheduleTimeVoteRetry();
				return;
			}
			getVoteCacheHandler().clearTimeVoteCompletion(vote);
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
		// A manual reload supersedes any nested deferred reload scheduled by an old
		// runtime. The generation check prevents that task from rebuilding after
		// shutdown or racing this explicit replacement.
		invalidateDeferredHttpTransportReconciliation();
		BungeeMethod configuredMethod = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (configuredMethod == null) configuredMethod = BungeeMethod.PLUGINMESSAGING;
		method = retainHttpForPendingDeliveries(configuredMethod);
		scheduleDeferredHttpTransportReconciliation();
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

	private synchronized BungeeMethod retainHttpForPendingDeliveries(BungeeMethod configuredMethod) {
		if (configuredMethod == BungeeMethod.HTTP && !hasChangedLiveHttpConfiguration()) {
			deferredHttpTransportReconciliation = false;
			return configuredMethod;
		}
		HttpProxyTransportServer transport = httpTransportServer;
		if (transport != null && httpTransportHasPendingDeliveries(transport)) {
			deferredHttpTransportReconciliation = true;
			logSevere("Retaining HTTP transport until durable deliveries are acknowledged");
			return BungeeMethod.HTTP;
		}
		if (transport == null) {
			try {
				if (httpQueueHasPersistedDeliveries(
						getDataFolderPlugin().toPath().resolve("http").resolve("outgoing-v1"))) {
					deferredHttpTransportReconciliation = true;
					logSevere("Retaining HTTP transport until persisted deliveries are acknowledged");
					return BungeeMethod.HTTP;
				}
			} catch (IOException unreadableQueue) {
				// An unreadable durable queue is not proof that it is empty. Reopen HTTP so
				// its normal bounded loader can validate or recover the state.
				deferredHttpTransportReconciliation = true;
				logSevere("Retaining HTTP transport because its persisted delivery queue could not be inspected");
				return BungeeMethod.HTTP;
			}
		}
		if (hasPendingCachedHttpDeliveries()) {
			deferredHttpTransportReconciliation = true;
			logSevere("Retaining HTTP transport until cached deliveries are acknowledged");
			return BungeeMethod.HTTP;
		}
		Collection<String> servers = getVoteCachePendingVotePartyServers();
		if (servers != null) {
			for (String server : servers) {
				Collection<String> rewards = getVoteCachePendingVotePartyRewardIds(server);
				if (rewards != null && !rewards.isEmpty()) {
					deferredHttpTransportReconciliation = true;
					logSevere("Retaining HTTP transport until pending vote-party rewards are acknowledged");
					return BungeeMethod.HTTP;
				}
			}
		}
		deferredHttpTransportReconciliation = false;
		return configuredMethod;
	}

	private boolean hasChangedLiveHttpConfiguration() {
		return httpTransportServer != null && (!java.util.Objects.equals(liveHttpHost, getConfig().getHttpHost())
				|| liveHttpPort != getConfig().getHttpPort()
				|| !java.util.Objects.equals(liveHttpPublicEndpoint, getConfig().getHttpPublicEndpoint()));
	}

	/** Uses SimpleAPI #80 when deployed while remaining safe with an older published snapshot. */
	protected boolean httpTransportHasPendingDeliveries(HttpProxyTransportServer transport) {
		try {
			return Boolean.TRUE.equals(transport.getClass().getMethod("hasPendingDeliveries").invoke(transport));
		} catch (NoSuchMethodException unavailable) {
			// Without an authoritative query, the live durable server cannot be proven empty.
			return true;
		} catch (ReflectiveOperationException | SecurityException failure) {
			logSevere("Retaining HTTP transport because its live delivery queue could not be inspected");
			return true;
		}
	}

	/** Invokes the additive SimpleAPI disk probe, with an equivalent bounded compatibility fallback. */
	protected boolean httpQueueHasPersistedDeliveries(java.nio.file.Path outgoingDirectory) throws IOException {
		try {
			Object result = HttpProxyTransportServer.class.getMethod("hasPersistedDeliveries", java.nio.file.Path.class)
					.invoke(null, outgoingDirectory);
			return Boolean.TRUE.equals(result);
		} catch (NoSuchMethodException unavailable) {
			return inspectPersistedHttpDeliveries(outgoingDirectory);
		} catch (java.lang.reflect.InvocationTargetException failure) {
			Throwable cause = failure.getCause();
			if (cause instanceof IOException io) throw io;
			throw new IOException("HTTP outgoing queue inspection failed", cause);
		} catch (ReflectiveOperationException | SecurityException failure) {
			throw new IOException("HTTP outgoing queue inspection failed", failure);
		}
	}

	private boolean inspectPersistedHttpDeliveries(java.nio.file.Path outgoingDirectory) throws IOException {
		java.nio.file.Path root = outgoingDirectory.toAbsolutePath().normalize();
		if (!java.nio.file.Files.exists(root, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return false;
		if (java.nio.file.Files.isSymbolicLink(root)
				|| !java.nio.file.Files.isDirectory(root, java.nio.file.LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP outgoing queue directory is invalid");
		int backendCount = 0;
		try (java.nio.file.DirectoryStream<java.nio.file.Path> backends = java.nio.file.Files.newDirectoryStream(root)) {
			for (java.nio.file.Path backend : backends) {
				if (java.nio.file.Files.isSymbolicLink(backend)
						|| !java.nio.file.Files.isDirectory(backend, java.nio.file.LinkOption.NOFOLLOW_LINKS))
					throw new IOException("HTTP outgoing queue contains an invalid entry");
				if (++backendCount > 128) throw new IOException("HTTP outgoing queue exceeds its backend bound");
				try (java.nio.file.DirectoryStream<java.nio.file.Path> entries =
						java.nio.file.Files.newDirectoryStream(backend)) {
					if (entries.iterator().hasNext()) return true;
				}
			}
		}
		return false;
	}

	private boolean hasPendingCachedHttpDeliveries() {
		VoteCacheHandler cache = getVoteCacheHandler();
		if (cache == null) return false;
		String[] cachedServers = cache.getCachedVotesServers();
		if (cachedServers != null) {
			for (String server : cachedServers) {
				Collection<OfflineBungeeVote> votes = cache.getVotes(server);
				if (hasPendingHttpDelivery(votes)) return true;
			}
		}
		Collection<String> onlinePlayers = cache.getOnlineVoteUUIDs();
		if (onlinePlayers != null) {
			for (String uuid : onlinePlayers) {
				Collection<OfflineBungeeVote> votes = cache.getOnlineVotes(uuid);
				if (hasPendingHttpDelivery(votes)) return true;
			}
		}
		Collection<VoteTimeQueue> timedVotes = cache.getTimeChangeQueue();
		if (timedVotes != null) {
			for (VoteTimeQueue vote : timedVotes) {
				if (vote != null && vote.hasPendingHttpBroadcastDeliveryIds()) return true;
			}
		}
		return false;
	}

	private static boolean hasPendingHttpDelivery(Collection<OfflineBungeeVote> votes) {
		if (votes == null) return false;
		for (OfflineBungeeVote vote : votes) {
			if (vote != null && vote.hasPendingHttpDeliveryIds()) return true;
		}
		return false;
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

	protected synchronized boolean sendHttpEnvelope(String server, JsonEnvelope envelope) {
		HttpProxyTransportServer transport = httpTransportServer;
		return transport != null && transport.send(server, envelope);
	}

	/**
	 * Sends a vote envelope while recovering the stable ID exposed when durable
	 * publication is indeterminate. The first attempt has already published a
	 * quarantine file, so retrying the identical envelope with a new ID could
	 * deliver the vote twice.
	 */
	protected boolean sendHttpEnvelopeWithRecovery(String server, JsonEnvelope envelope) {
		return sendHttpEnvelopeWithRecovery(server, envelope, null);
	}

	/**
	 * Sends a non-reward HTTP message without allowing a transport handoff failure
	 * to retry the surrounding vote transaction. An ambiguous durable publication
	 * is recovered with its original ID; a definite rejection is reported and
	 * logged, rather than escaping through the void global-message adapter.
	 */
	protected boolean sendGenericHttpEnvelope(String server, JsonEnvelope envelope) {
		try {
			boolean accepted = sendHttpEnvelopeWithRecovery(server, envelope);
			if (!accepted) debug("HTTP transport rejected auxiliary delivery for " + server);
			return accepted;
		} catch (RuntimeException failure) {
			debug("Unable to send HTTP auxiliary delivery: " + failure.getMessage());
			return false;
		}
	}

	protected boolean sendStableHttpEnvelope(String server, String deliveryId, JsonEnvelope envelope) {
		try {
			boolean accepted = sendHttpEnvelope(server, deliveryId, envelope);
			if (!accepted) debug("HTTP transport rejected auxiliary delivery for " + server);
			return accepted;
		} catch (RuntimeException failure) {
			debug("Unable to send HTTP auxiliary delivery: " + failure.getMessage());
			return false;
		}
	}

	protected boolean sendHttpEnvelopeWithRecovery(String server, JsonEnvelope envelope, OfflineBungeeVote cachedVote) {
		String persistedId = cachedVote == null ? null : cachedVote.getHttpDeliveryId(server);
		String stableId = persistedId != null ? persistedId
				: stableCachedHttpDeliveryId("reward", server, envelope, cachedVote);
		try {
			boolean accepted = stableId == null ? sendHttpEnvelope(server, envelope)
					: sendHttpEnvelope(server, stableId, envelope);
			if (accepted && persistedId != null) {
				cachedVote.setHttpDeliveryId(server, null);
				cachedVote.setDeliveryStateDirty(true);
			}
			return accepted;
		} catch (HttpProxyTransportServer.DeliveryRetryException failure) {
			if (cachedVote != null) {
				cachedVote.setHttpDeliveryId(server, failure.deliveryId());
				cachedVote.setDeliveryStateDirty(true);
				// Retry only after the caller durably records the recovered transport ID.
				return false;
			}
			try {
				boolean accepted = sendHttpEnvelope(server, failure.deliveryId(), envelope);
				return accepted;
			} catch (RuntimeException retryFailure) {
				debug("Unable to recover HTTP vote delivery " + failure.deliveryId() + ": " + retryFailure.getMessage());
				return false;
			}
		}
	}

	/** Derives the same transport identity after a crash before cache cleanup. */
	private String stableCachedHttpDeliveryId(String purpose, String server, JsonEnvelope envelope,
			OfflineBungeeVote cachedVote) {
		if (cachedVote == null || server == null || envelope == null) return null;
		String voteIdentity = cachedVote.getVoteId() == null ? stableCachedVoteRowIdentity(cachedVote)
				: cachedVote.getVoteId().toString();
		String key = "VotingPlugin:http-cache:v1\u0000" + purpose + "\u0000"
				+ server.toLowerCase(Locale.ROOT) + "\u0000" + envelope.getSubChannel() + "\u0000" + voteIdentity;
		return UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString();
	}

	/**
	 * Distinguishes pre-vote-ID cache rows that can otherwise share every visible
	 * vote field. SQL primary keys and JSON entry keys are durable before this
	 * fallback is used; the tuple remains only for callers holding an unbound
	 * legacy object during a mixed-version transition.
	 */
	private String stableCachedVoteRowIdentity(OfflineBungeeVote vote) {
		if (vote.getServerVoteCacheRowId() > 0) return "server-sql:" + vote.getServerVoteCacheRowId();
		if (vote.getOnlineVoteCacheRowId() > 0) return "online-sql:" + vote.getOnlineVoteCacheRowId();
		if (vote.getServerVoteCacheJsonKey() != null) return "server-json:" + vote.getServerVoteCacheJsonKey();
		if (vote.getOnlineVoteCacheJsonKey() != null) {
			return "online-json:" + vote.getUuid() + ":" + vote.getOnlineVoteCacheJsonKey();
		}
		return "legacy:" + vote.getUuid() + "\u0000" + vote.getService() + "\u0000" + vote.getTime();
	}

	protected synchronized boolean sendHttpEnvelope(String server, String deliveryId, JsonEnvelope envelope) {
		HttpProxyTransportServer transport = httpTransportServer;
		return transport != null && transport.send(server, deliveryId, envelope);
	}

	private void startHttpTransport() {
		try {
			PreparedHttpTransport prepared = PREPARED_HTTP_TRANSPORTS.remove(httpTransportPreparationKey());
			if (prepared != null) {
				if (!prepared.matches(getConfig())) {
					prepared.close();
					throw new IllegalStateException("Prepared HTTP transport does not match the installed configuration");
				}
				httpTransportServer = prepared.server;
				httpEnrollmentAuthority = prepared.authority;
				prepared.owner.set(this);
			} else {
				URI endpoint = validatedHttpEndpoint(getConfig().getHttpPublicEndpoint());
				File directory = new File(getDataFolderPlugin(), "http");
				HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.toPath(), endpoint.getHost());
				httpEnrollmentAuthority = new HttpEnrollmentAuthority(identity, directory.toPath());
				httpTransportServer = new HttpProxyTransportServer(
						new InetSocketAddress(getConfig().getHttpHost(), getConfig().getHttpPort()), identity,
						httpEnrollmentAuthority, directory.toPath().resolve("outgoing-v1"),
						this::handleHttpTransportEnvelope, this::acknowledgeHttpDelivery);
				httpTransportServer.start();
			}
			liveHttpHost = getConfig().getHttpHost();
			liveHttpPort = getConfig().getHttpPort();
			liveHttpPublicEndpoint = getConfig().getHttpPublicEndpoint();
			logInfo("HTTP transport listening securely on " + getConfig().getHttpHost() + ":"
					+ httpTransportServer.port() + "; use /votingpluginproxy httpcode <server> for each backend");
		} catch (Exception failure) {
			closeHttpTransport();
			throw new IllegalStateException("HTTP transport could not start securely", failure);
		}
	}

	/** Keeps the authenticated mTLS backend identity attached to security-sensitive proxy routing. */
	protected void handleHttpTransportEnvelope(HttpProxyTransportServer.ReceivedEnvelope received) {
		if (!isAuthenticatedHttpEnvelopeAllowed(received)) {
			debug("Ignored HTTP envelope whose player-presence claim did not match its authenticated backend");
			return;
		}
		GlobalMessageProxyHandler handler = globalMessageProxyHandler;
		if (handler == null) throw new IllegalStateException("HTTP message router is not ready");
		handler.onMessage(received.envelope());
	}

	private boolean isAuthenticatedHttpEnvelopeAllowed(HttpProxyTransportServer.ReceivedEnvelope received) {
		if (received == null || received.envelope() == null || received.serverId() == null) return false;
		String stampedServer = received.envelope().getFields().getOrDefault(VotingPluginWire.K_SERVER, "");
		if (!received.serverId().equalsIgnoreCase(stampedServer)) return false;
		if (!VotingPluginWire.SUB_LOGIN.equals(received.envelope().getSubChannel())) return true;
		VotingPluginWire.PlayerPresenceEvent event = VotingPluginWire.readPlayerPresenceEvent(received.envelope());
		boolean modern = event.connectionId != null || event.backendIncarnationId != null
				|| event.backendStartedAt != 0L || event.presenceTimestamp != 0L;
		if (!modern || isDedicatedVotingProxyEnabled()) return true;
		// A player-facing proxy has a stronger authority than any backend: its live
		// player connection supplies both the current route and (in online mode) UUID.
		return isLegacyLoginDestinationAuthoritative(event.player, event.uuid, received.serverId());
	}

	private synchronized void closeHttpTransport() {
		HttpProxyTransportServer transport = httpTransportServer;
		httpTransportServer = null;
		httpEnrollmentAuthority = null;
		liveHttpHost = null;
		liveHttpPort = 0;
		liveHttpPublicEndpoint = null;
		if (transport != null) transport.close();
	}

	/**
	 * Starts the candidate HTTP listener before Control publishes an HTTP method
	 * change. The replacement runtime adopts this listener, avoiding a bind/TLS/
	 * queue failure after the old runtime has already been torn down.
	 */
	public synchronized void prepareHttpTransportChange(VotingPluginProxyConfig candidate) {
		cancelPreparedHttpTransportChange();
		PreparedHttpTransport prepared = createPreparedHttpTransport(candidate);
		PREPARED_HTTP_TRANSPORTS.put(httpTransportPreparationKey(), prepared);
	}

	/** Cancels a candidate listener when configuration publication fails. */
	public synchronized void cancelPreparedHttpTransportChange() {
		PreparedHttpTransport prepared = PREPARED_HTTP_TRANSPORTS.remove(httpTransportPreparationKey());
		if (prepared != null) prepared.close();
	}

	private PreparedHttpTransport createPreparedHttpTransport(VotingPluginProxyConfig candidate) {
		HttpProxyTransportServer server = null;
		try {
			URI endpoint = validatedHttpEndpoint(candidate.getHttpPublicEndpoint());
			File directory = new File(getDataFolderPlugin(), "http");
			HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.toPath(), endpoint.getHost());
			HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.toPath());
			AtomicReference<VotingPluginProxy> owner = new AtomicReference<>();
			server = new HttpProxyTransportServer(
					new InetSocketAddress(candidate.getHttpHost(), candidate.getHttpPort()), identity, authority,
					directory.toPath().resolve("outgoing-v1"), received -> {
						VotingPluginProxy active = owner.get();
						if (active == null) throw new IllegalStateException("HTTP runtime replacement is not active");
						active.handleHttpTransportEnvelope(received);
					}, (backend, deliveryId) -> {
						VotingPluginProxy active = owner.get();
						if (active == null) throw new IOException("HTTP runtime replacement is not active");
						active.acknowledgeHttpDelivery(backend, deliveryId);
					});
			server.start();
			return new PreparedHttpTransport(server, authority, owner, candidate.getHttpHost(), candidate.getHttpPort(),
					candidate.getHttpPublicEndpoint());
		} catch (Exception failure) {
			if (server != null) server.close();
			throw new IllegalStateException("HTTP transport could not be prepared securely", failure);
		}
	}

	private Path httpTransportPreparationKey() {
		return getDataFolderPlugin().toPath().toAbsolutePath().normalize();
	}

	private URI validatedHttpEndpoint(String publicEndpoint) {
		URI endpoint = URI.create(publicEndpoint);
		if (!"https".equalsIgnoreCase(endpoint.getScheme()) || endpoint.getHost() == null
				|| endpoint.getPort() == 0 || endpoint.getPort() > 65535
				|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
				|| (endpoint.getPath() != null && !endpoint.getPath().isEmpty() && !"/".equals(endpoint.getPath()))) {
			throw new IllegalArgumentException("HTTP.PublicEndpoint must be an HTTPS origin");
		}
		return endpoint;
	}

	/**
	 * Receives every durable HTTP acknowledgement, including non-vote-party
	 * deliveries. The SimpleAPI queue removes the acknowledged entry immediately
	 * after this callback returns, so defer inspection to the proxy scheduler.
	 */
	protected void acknowledgeHttpDelivery(String server, String deliveryId) throws IOException {
		acknowledgeVotePartyDelivery(server, deliveryId);
		scheduleDeferredHttpTransportReconciliation();
	}

	/** Rebuilds the proxy runtime only after the retained HTTP transport is proven empty. */
	private void invalidateDeferredHttpTransportReconciliation() {
		synchronized (this) {
			httpTransportReconciliationGeneration++;
			httpTransportReconciliationScheduled = false;
			// Older work is fenced by its generation and must not suppress the
			// replacement runtime's own single-flight probe.
			httpTransportReconciliationRunning = false;
		}
	}

	private void scheduleDeferredHttpTransportReconciliation() {
		scheduleDeferredHttpTransportReconciliation(HTTP_TRANSPORT_RECONCILIATION_DELAY_MILLIS);
	}

	private void scheduleDeferredHttpTransportReconciliation(long delayMillis) {
		scheduleDeferredHttpTransportReconciliation(delayMillis, -1L);
	}

	private void scheduleDeferredHttpTransportReconciliation(long delayMillis, long requiredGeneration) {
		ScheduledExecutorService scheduler = getScheduler();
		if (scheduler == null) return;
		long generation;
		synchronized (this) {
			if (requiredGeneration >= 0L && requiredGeneration != httpTransportReconciliationGeneration) return;
			if (!deferredHttpTransportReconciliation || method != BungeeMethod.HTTP
					|| httpTransportReconciliationScheduled || httpTransportReconciliationRunning) return;
			httpTransportReconciliationScheduled = true;
			generation = httpTransportReconciliationGeneration;
		}
		try {
			scheduler.schedule(() -> reconcileDeferredHttpTransport(generation), delayMillis, TimeUnit.MILLISECONDS);
		} catch (RuntimeException unavailable) {
			synchronized (this) {
				if (generation == httpTransportReconciliationGeneration)
					httpTransportReconciliationScheduled = false;
			}
			debug("Unable to schedule deferred HTTP transport reconciliation: " + unavailable.getMessage());
		}
	}

	private void reconcileDeferredHttpTransport(long generation) {
		BungeeMethod configuredMethod;
		boolean stillPending;
		synchronized (this) {
			if (generation != httpTransportReconciliationGeneration) return;
			httpTransportReconciliationScheduled = false;
			if (!enabled || !deferredHttpTransportReconciliation || method != BungeeMethod.HTTP) return;
			configuredMethod = BungeeMethod.getByName(getConfig().getBungeeMethod());
			if (configuredMethod == null) configuredMethod = BungeeMethod.PLUGINMESSAGING;
			retainHttpForPendingDeliveries(configuredMethod);
			stillPending = deferredHttpTransportReconciliation;
			if (!stillPending) httpTransportReconciliationRunning = true;
		}
		if (stillPending) {
			// The acknowledgement callback happens before queue removal. Re-arm the
			// same single-flight probe so the final removal/cached-state clear is
			// observed without relying on another inbound message.
			scheduleDeferredHttpTransportReconciliation(HTTP_TRANSPORT_RECONCILIATION_POLL_MILLIS, generation);
			return;
		}
		synchronized (this) {
			if (generation != httpTransportReconciliationGeneration || !enabled) {
				httpTransportReconciliationRunning = false;
				return;
			}
		}
		try {
			// The concrete platform checks this generation again while holding its
			// reload lock. That closes the gap between this scheduler callback and a
			// manual platform reload/shutdown without introducing lock inversion.
			reloadDeferredHttpTransportCore(generation);
		} catch (RuntimeException reconciliationFailure) {
			debug("Deferred HTTP transport reconciliation failed: " + reconciliationFailure.getMessage());
		} finally {
			boolean retry;
			synchronized (this) {
				retry = generation == httpTransportReconciliationGeneration && enabled
						&& deferredHttpTransportReconciliation && method == BungeeMethod.HTTP;
				if (generation == httpTransportReconciliationGeneration)
					httpTransportReconciliationRunning = false;
			}
			if (retry)
				scheduleDeferredHttpTransportReconciliation(HTTP_TRANSPORT_RECONCILIATION_POLL_MILLIS, generation);
		}
	}

	public String createHttpConnectionCode(String serverId) {
		HttpEnrollmentAuthority authority = httpEnrollmentAuthority;
		if (method != BungeeMethod.HTTP || authority == null) {
			throw new IllegalStateException("The HTTP transport is not running");
		}
		String publicEndpoint = liveHttpPublicEndpoint != null
				? liveHttpPublicEndpoint : getConfig().getHttpPublicEndpoint();
		return authority.createConnectionCode(serverId, URI.create(publicEndpoint), Duration.ofMinutes(15))
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

	/** Completion boundary used before durable HTTP vote-party command progress is advanced. */
	protected CompletableFuture<Void> runVotePartyConsoleCommand(String command) {
		runConsoleCommand(command);
		return CompletableFuture.completedFuture(null);
	}

	/** Schedules the liveness fence for a platform command whose completion is uncertain. */
	protected void scheduleVotePartyProxyCommandTimeout(Runnable timeout) {
		CompletableFuture.delayedExecutor(60, TimeUnit.SECONDS).execute(timeout);
	}

	public abstract void saveVoteCacheFile();

	public abstract void reloadCore(boolean mysql);

	/** Platform implementations recheck this under their reload lock before replacing the runtime. */
	protected void reloadDeferredHttpTransportCore(long generation) {
		if (isDeferredHttpTransportGenerationCurrent(generation)) reloadCore(true);
	}

	protected synchronized boolean isDeferredHttpTransportGenerationCurrent(long generation) {
		return enabled && generation == httpTransportReconciliationGeneration
				&& httpTransportReconciliationRunning && method == BungeeMethod.HTTP;
	}

	/** True when a changed configured transport must not retire this runtime's live HTTP queue yet. */
	public synchronized boolean isRetainingHttpTransportForDeferredReconciliation() {
		return deferredHttpTransportReconciliation && method == BungeeMethod.HTTP;
	}

	/** An active HTTP runtime changing method or endpoint needs a pre-teardown retention probe. */
	public synchronized boolean requiresHttpRetentionCheckBeforeRuntimeReplacement() {
		BungeeMethod configuredMethod = BungeeMethod.getByName(getConfig().getBungeeMethod());
		if (configuredMethod == null) configuredMethod = BungeeMethod.PLUGINMESSAGING;
		return method == BungeeMethod.HTTP
				&& (configuredMethod != BungeeMethod.HTTP || hasChangedLiveHttpConfiguration());
	}

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

	public synchronized void sendVoteParty(String server) {
		if (!isSomeoneOnlineServerForVoteRouting(server)) return;
		if (method != BungeeMethod.HTTP) {
			globalMessageProxyHandler.sendMessage(server, 1, VotingPluginWire.votePartyBungee());
			return;
		}
		Collection<String> pending = getVoteCachePendingVotePartyRewardIds(server);
		if (pending != null && pending.size() >= MAX_PENDING_VOTE_PARTY_REWARDS) {
			logSevere("HTTP vote-party reward backlog is full for " + server);
			return;
		}
		String deliveryId = UUID.randomUUID().toString();
		// Persist intent before the bounded HTTP queue is attempted. A rejection or
		// restart therefore leaves a retryable reward instead of silently losing it.
		setVoteCachePendingVotePartyReward(server, deliveryId, true);
		try {
			saveVotePartyStateDurably();
		} catch (IOException failure) {
			setVoteCachePendingVotePartyReward(server, deliveryId, false);
			throw new IllegalStateException("Unable to persist HTTP vote-party reward", failure);
		}
		retryPendingVotePartyRewards();
	}

	protected synchronized void retryPendingVotePartyRewards() {
		if (!enabled || method != BungeeMethod.HTTP) return;
		boolean retryRequired = false;
		Collection<String> servers = getVoteCachePendingVotePartyServers();
		if (servers == null) return;
		for (String server : new ArrayList<>(servers)) {
			Collection<String> pendingIds = getVoteCachePendingVotePartyRewardIds(server);
			if (pendingIds == null || pendingIds.isEmpty()) continue;
			String routingServer = resolveVotePartyRoutingServer(server);
			for (String deliveryId : new ArrayList<>(pendingIds)) {
				if (!isSomeoneOnlineServerForVoteRouting(routingServer)
						|| !sendHttpEnvelope(routingServer, deliveryId, VotingPluginWire.votePartyBungee())) {
					retryRequired = true;
					break;
				}
				retryRequired = true;
			}
		}
		if (retryRequired) scheduleVotePartyDeliveryRetry();
	}

	protected synchronized boolean retryPendingVotePartyProxyEffects() {
		if (!enabled) return true;
		if (votePartyProxyCommandInFlight != 0L) return false;
		if (votePartyProxyCommandCompletedUnpersisted
				&& !quarantineInFlightVotePartyProxyCommandForReplacement()) {
			scheduleVotePartyDeliveryRetry();
			return false;
		}
		PendingVotePartyProxyEffects pending;
		try {
			pending = getVoteCachePendingVotePartyProxyEffects();
		} catch (RuntimeException invalid) {
			logSevere("Pending HTTP vote-party proxy effects are invalid; retaining them without execution");
			return false;
		}
		while (!pending.isEmpty()) {
			PendingVotePartyProxyEffects remaining;
			try {
				if (!pending.broadcast().isEmpty()) {
					broadcast(pending.broadcast());
					remaining = new PendingVotePartyProxyEffects("", pending.commands());
				} else {
					CompletableFuture<Void> execution = runVotePartyConsoleCommand(pending.commands().get(0));
					remaining = new PendingVotePartyProxyEffects("", pending.commands().subList(1, pending.commands().size()));
					if (!execution.isDone()) {
						long attempt = ++votePartyProxyCommandAttemptSequence;
						if (attempt == 0L) attempt = ++votePartyProxyCommandAttemptSequence;
						long commandAttempt = attempt;
						votePartyProxyCommandInFlight = commandAttempt;
						votePartyProxyCommandExecution = execution;
						votePartyProxyCommandCompletedUnpersisted = false;
						PendingVotePartyProxyEffects expected = pending;
						PendingVotePartyProxyEffects completed = remaining;
						execution.whenComplete((ignored, failure) ->
								completeVotePartyProxyCommand(commandAttempt, expected, completed, failure));
						long scheduledAttempt = commandAttempt;
						try {
							scheduleVotePartyProxyCommandTimeout(() ->
									quarantineTimedOutVotePartyProxyCommand(scheduledAttempt, expected, completed, execution));
						} catch (RuntimeException unavailable) {
							logSevere("Unable to schedule the HTTP vote-party command liveness fence; the command remains pending");
						}
						return false;
					}
					execution.join();
				}
			} catch (RuntimeException failure) {
				logSevere("A committed HTTP vote-party proxy effect failed and remains pending for retry");
				scheduleVotePartyDeliveryRetry();
				return false;
			}

			if (!persistVotePartyProxyEffectProgress(pending, remaining)) return false;
			pending = remaining;
		}
		return true;
	}

	private void completeVotePartyProxyCommand(long attempt, PendingVotePartyProxyEffects expected,
			PendingVotePartyProxyEffects remaining, Throwable failure) {
		synchronized (this) {
			if (votePartyProxyCommandInFlight != attempt) return;
			votePartyProxyCommandInFlight = 0L;
			votePartyProxyCommandExecution = null;
			if (failure != null) {
				logSevere("A committed HTTP vote-party proxy command failed and remains pending for retry");
				if (enabled) scheduleVotePartyDeliveryRetry();
				return;
			}
			PendingVotePartyProxyEffects current;
			try {
				current = getVoteCachePendingVotePartyProxyEffects();
			} catch (RuntimeException invalid) {
				logSevere("Pending HTTP vote-party proxy effects became invalid while a command was running");
				return;
			}
			if (!current.equals(expected)) {
				logSevere("Pending HTTP vote-party proxy effects changed while a command was running; progress was not advanced");
				return;
			}
			if (!persistVotePartyProxyEffectProgress(expected, remaining)) {
				votePartyProxyCommandCompletedUnpersisted = true;
				if (!enabled) quarantineInFlightVotePartyProxyCommandForReplacement();
				return;
			}
			votePartyProxyCommandCompletedUnpersisted = false;
			if (enabled && retryPendingVotePartyProxyEffects()) {
				if (method == BungeeMethod.HTTP) retryPendingVotePartyRewards();
				if (votePartyVotes >= currentVotePartyVotesRequired) checkVoteParty();
			}
		}
	}

	private void quarantineTimedOutVotePartyProxyCommand(long attempt, PendingVotePartyProxyEffects expected,
			PendingVotePartyProxyEffects remaining, CompletableFuture<Void> execution) {
		synchronized (this) {
			if (!enabled || execution.isDone() || votePartyProxyCommandInFlight != attempt) return;
			PendingVotePartyProxyEffects current;
			PendingVotePartyProxyEffects previousQuarantine;
			try {
				current = getVoteCachePendingVotePartyProxyEffects();
				previousQuarantine = getVoteCacheQuarantinedVotePartyProxyEffects();
			} catch (RuntimeException invalid) {
				logSevere("Pending HTTP vote-party proxy effects became invalid while a command was running");
				return;
			}
			if (!current.equals(expected)) {
				logSevere("Pending HTTP vote-party proxy effects changed while a command was running; the attempt remains fenced");
				return;
			}
			PendingVotePartyProxyEffects quarantine;
			try {
				java.util.List<String> commands = new java.util.ArrayList<>(previousQuarantine.commands());
				commands.add(expected.commands().get(0));
				quarantine = new PendingVotePartyProxyEffects(previousQuarantine.broadcast(), commands);
			} catch (RuntimeException full) {
				logSevere("HTTP vote-party command quarantine is full; the uncertain command remains fenced");
				return;
			}
			setVoteCacheQuarantinedVotePartyProxyEffects(quarantine);
			setVoteCachePendingVotePartyProxyEffects(remaining);
			try {
				saveVotePartyStateDurably();
			} catch (IOException | RuntimeException failure) {
				setVoteCacheQuarantinedVotePartyProxyEffects(previousQuarantine);
				setVoteCachePendingVotePartyProxyEffects(expected);
				logSevere("Unable to durably quarantine an uncertain HTTP vote-party command; the attempt remains fenced");
				return;
			}
			votePartyProxyCommandInFlight = 0L;
			votePartyProxyCommandExecution = null;
			logSevere("An HTTP vote-party proxy command did not complete within 60 seconds and was durably quarantined without retry");
			if (retryPendingVotePartyProxyEffects()) {
				if (method == BungeeMethod.HTTP) retryPendingVotePartyRewards();
				if (votePartyVotes >= currentVotePartyVotesRequired) checkVoteParty();
			}
		}
	}

	protected synchronized boolean quarantineInFlightVotePartyProxyCommandForReplacement() {
		if (votePartyProxyCommandInFlight == 0L && !votePartyProxyCommandCompletedUnpersisted) return true;
		PendingVotePartyProxyEffects pending;
		PendingVotePartyProxyEffects previousQuarantine;
		try {
			pending = getVoteCachePendingVotePartyProxyEffects();
			previousQuarantine = getVoteCacheQuarantinedVotePartyProxyEffects();
			if (pending.commands().isEmpty()) return false;
			java.util.List<String> commands = new java.util.ArrayList<>(previousQuarantine.commands());
			commands.add(pending.commands().get(0));
			PendingVotePartyProxyEffects quarantine = new PendingVotePartyProxyEffects(previousQuarantine.broadcast(), commands);
			PendingVotePartyProxyEffects remaining = new PendingVotePartyProxyEffects("",
					pending.commands().subList(1, pending.commands().size()));
			setVoteCacheQuarantinedVotePartyProxyEffects(quarantine);
			setVoteCachePendingVotePartyProxyEffects(remaining);
			try {
				saveVotePartyStateDurably();
			} catch (IOException | RuntimeException failure) {
				setVoteCacheQuarantinedVotePartyProxyEffects(previousQuarantine);
				setVoteCachePendingVotePartyProxyEffects(pending);
				return false;
			}
			votePartyProxyCommandInFlight = 0L;
			votePartyProxyCommandExecution = null;
			votePartyProxyCommandCompletedUnpersisted = false;
			logSevere("An in-flight HTTP vote-party proxy command was durably quarantined for runtime replacement");
			return true;
		} catch (RuntimeException invalid) {
			return false;
		}
	}

	/** Gives an executing platform command a bounded chance to finish and persist progress before final teardown. */
	protected void awaitInFlightVotePartyProxyCommand() {
		CompletableFuture<Void> execution = votePartyProxyCommandExecution;
		if (execution == null || execution.isDone()) return;
		try {
			execution.get(5, TimeUnit.SECONDS);
		} catch (java.util.concurrent.ExecutionException ignored) {
			// The completion callback retains failed commands for retry.
		} catch (java.util.concurrent.TimeoutException timeout) {
			logSevere("Timed out waiting for an in-flight HTTP vote-party command during final shutdown");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			logSevere("Interrupted while waiting for an in-flight HTTP vote-party command during final shutdown");
		}
	}

	private boolean persistVotePartyProxyEffectProgress(PendingVotePartyProxyEffects previous,
			PendingVotePartyProxyEffects remaining) {
		setVoteCachePendingVotePartyProxyEffects(remaining);
		try {
			saveVotePartyStateDurably();
			return true;
		} catch (IOException | RuntimeException failure) {
			// Execution succeeded, but its progress was not durably confirmed. Restoring
			// the marker gives at-least-once recovery rather than silently skipping it.
			setVoteCachePendingVotePartyProxyEffects(previous);
			logSevere("Unable to persist HTTP vote-party proxy-effect progress; the effect remains pending");
			scheduleVotePartyDeliveryRetry();
			return false;
		}
	}

	private String resolveVotePartyRoutingServer(String canonicalServer) {
		for (String configuredServer : getAllAvailableServers()) {
			if (configuredServer.equalsIgnoreCase(canonicalServer)) return configuredServer;
		}
		return canonicalServer;
	}

	protected synchronized void acknowledgeVotePartyDelivery(String server, String deliveryId) throws IOException {
		Collection<String> pendingServers = getVoteCachePendingVotePartyServers();
		if (pendingServers == null) return;
		for (String pendingServer : new ArrayList<>(pendingServers)) {
			if (!pendingServer.equalsIgnoreCase(server)) continue;
			Collection<String> pending = getVoteCachePendingVotePartyRewardIds(pendingServer);
			if (pending == null || !pending.contains(deliveryId)) return;
			setVoteCachePendingVotePartyReward(pendingServer, deliveryId, false);
			try {
				saveVotePartyStateDurably();
			} catch (IOException | RuntimeException failure) {
				setVoteCachePendingVotePartyReward(pendingServer, deliveryId, true);
				if (failure instanceof IOException ioFailure) throw ioFailure;
				throw (RuntimeException) failure;
			}
			return;
		}
	}

	private void scheduleVotePartyDeliveryRetry() {
		if (!enabled || votePartyDeliveryRetryScheduled || getScheduler() == null) return;
		boolean pendingProxyEffects;
		try {
			pendingProxyEffects = !getVoteCachePendingVotePartyProxyEffects().isEmpty();
		} catch (RuntimeException invalid) {
			logSevere("Pending HTTP vote-party proxy effects are invalid; automatic execution is disabled");
			return;
		}
		Collection<String> pendingServers = method == BungeeMethod.HTTP ? getVoteCachePendingVotePartyServers() : null;
		if (!pendingProxyEffects && (pendingServers == null || pendingServers.isEmpty())) return;
		votePartyDeliveryRetryScheduled = true;
		try {
			getScheduler().schedule(() -> {
				synchronized (VotingPluginProxy.this) { votePartyDeliveryRetryScheduled = false; }
				if (retryPendingVotePartyProxyEffects()) {
					if (method == BungeeMethod.HTTP) retryPendingVotePartyRewards();
					if (votePartyVotes >= currentVotePartyVotesRequired) checkVoteParty();
				}
			}, 5, TimeUnit.SECONDS);
		} catch (RuntimeException failure) {
			votePartyDeliveryRetryScheduled = false;
			debug("Unable to schedule HTTP vote-party reward retry: " + failure.getMessage());
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

	public abstract void setVoteCachePendingVotePartyReward(String server, String deliveryId, boolean pending);

	public abstract void setVoteCachePendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects);

	public abstract void setVoteCacheQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects);

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

	private boolean sendVoteDelayRejected(UUID voteId, String player, String uuid, String service,
			boolean playerOnline, String playerServer) {
		if (!playerOnline || playerServer == null || !getAllAvailableServers().contains(playerServer)) {
			debug("Not sending vote delay rejection for " + player + " because the player is offline");
			return true;
		}

		JsonEnvelope envelope = VotingPluginWire.voteDelayRejected(player, uuid, service, true);
		if (method == BungeeMethod.HTTP) {
			String key = voteId + "\u0000vote-delay-rejected\u0000" + playerServer.toLowerCase(Locale.ROOT);
			String deliveryId = UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString();
			return sendStableHttpEnvelope(playerServer, deliveryId, envelope);
		}
		globalMessageProxyHandler.sendMessage(playerServer, 1, envelope);
		return true;
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
		UUID voteId = UUID.randomUUID();
		if (vote(player, service, realVote, timeQueue, queueTime, text, uuid, null, voteId) == QueuedVoteResult.RETRY) {
			liveVoteRetries.remove(voteId);
			throw new VoteRetryException();
		}
	}

	public synchronized void vote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid, UUID voteId) {
		if (vote(player, service, realVote, timeQueue, queueTime, text, uuid, null, voteId) == QueuedVoteResult.RETRY) {
			throw new VoteRetryException();
		}
	}

	/** Releases retry-only state after the bounded event-listener retries are exhausted. */
	public synchronized void abandonLiveVoteRetry(UUID voteId) {
		if (voteId != null) liveVoteRetries.remove(voteId);
	}

	private enum QueuedVoteResult {
		SUCCESS, RETRY, TERMINAL
	}

	private synchronized QueuedVoteResult vote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid, VoteTimeQueue queuedVote) {
		return vote(player, service, realVote, timeQueue, queueTime, text, uuid, queuedVote, null);
	}

	private synchronized QueuedVoteResult vote(String player, String service, boolean realVote, boolean timeQueue,
			long queueTime, VoteTotalsSnapshot text, String uuid, VoteTimeQueue queuedVote, UUID requestedVoteId) {
		try {
			String requestPlayer = player;
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
			String requestIdentity = player.toLowerCase(Locale.ROOT) + "\u0000" + service.toLowerCase(Locale.ROOT);
			// A platform listener can receive a vote in the small interval after the
			// replacement gate succeeds and before the platform publishes its fresh
			// runtime. Tell its bounded retry wrapper to retry against that fresh
			// runtime rather than creating state that would be lost with this one.
			if (runtimeReplacementPrepared) return QueuedVoteResult.RETRY;

			UUID voteId = queuedVote == null ? null : queuedVote.getVoteId();
			if (voteId == null) {
				voteId = requestedVoteId == null
						? queuedVote == null ? UUID.randomUUID() : legacyTimedVoteId(queuedVote)
						: requestedVoteId;
				if (queuedVote != null && !getVoteCacheHandler().assignLegacyTimeVoteId(queuedVote, voteId)) {
					warn("Unable to assign a stable ID to a legacy timed vote; retaining it for retry");
					return QueuedVoteResult.RETRY;
				}
			}
			LiveVoteRetryState retryState = liveVoteRetries.get(voteId);
			if (retryState != null && !requestIdentity.equals(retryState.requestIdentity)) {
				throw new IllegalArgumentException("Retry ID does not match the original vote");
			}
			boolean resumingAfterTotals = retryState != null;

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
			if (retryState != null) {
				broadcastForwardedServers.addAll(retryState.broadcastForwardedServers);
			}
			boolean proxyBroadcastHandled = queuedVote != null && queuedVote.isProxyBroadcastHandled();
			boolean processesTotals = getConfig().getPrimaryServer() || !getConfig().getMultiProxySupport();
			boolean managesTotals = processesTotals && getConfig().getBungeeManageTotals();
			boolean canValidateStandaloneBroadcast = canForwardStandaloneBroadcast(managesTotals);
			ArrayList<Column> data = retryState == null ? null : retryState.totalsInput;
			boolean queueForTimeChange = false;

			// A completion callback can wipe totals and replay older queued votes. Run it
			// before loading this vote's database snapshot so the calculations below use
			// the post-rollover state.
			if (!resumingAfterTotals && getConfig().getGlobalDataEnabled()
					&& getGlobalDataHandler().isTimeChangedHappened()) {
				getGlobalDataHandler().checkForFinishedTimeChanges();
				queueForTimeChange = timeQueue && getGlobalDataHandler().isTimeChangedHappened();
			}

			// Validate the vote before any immediate announcement. This keeps duplicate
			// votes rejected by the delay check out of the GlobalData rollover queue and
			// prevents announcing a vote that will not be processed.
			if (!resumingAfterTotals && managesTotals) {
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
					if (!sendVoteDelayRejected(voteId, player, uuid, service, playerOnline, playerServer)
							&& queuedVote != null) return QueuedVoteResult.RETRY;
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
				delayedVote.setRealVote(realVote);
				if (!getVoteCacheHandler().addTimeVoteToCache(delayedVote)) {
					logSevere("Unable to persist queued rollover vote for " + player + "/" + service
							+ "; skipping proxy broadcast");
					return QueuedVoteResult.RETRY;
				}
				if (proxyBroadcastHandled) {
					for (String target : broadcastTargets) {
						Set<String> forwarded = sendProxyBroadcast(Collections.singleton(target), uuid, player,
								service, time, projectedTotals == null ? "" : projectedTotals.toString(), false, delayedVote);
						boolean newlyForwarded = delayedVote.getBroadcastForwardedServers().addAll(forwarded);
						broadcastForwardedServers.addAll(forwarded);
						if (newlyForwarded || delayedVote.isDeliveryStateDirty()) {
							persistTimeVoteDelivery(delayedVote);
						}
					}
				}
				log("Caching vote from " + player + "/" + service
						+ " because time change is happening right now");
				return QueuedVoteResult.SUCCESS;
			}

			if (retryState == null) {
				if (liveVoteRetries.size() >= MAX_LIVE_VOTE_RETRIES) return QueuedVoteResult.RETRY;
				retryState = new LiveVoteRetryState();
				retryState.requestIdentity = requestIdentity;
				retryState.totalsInput = data;
				retryState.player = requestPlayer;
				retryState.service = service;
				retryState.uuid = uuid;
				retryState.time = time;
				retryState.realVote = realVote;
				liveVoteRetries.put(voteId, retryState);
			}
			if (queuedVote != null) {
				retryState.queuedVote = queuedVote;
				retryState.multiProxyForwardingHandled |= queuedVote.isMultiProxyForwardingHandled();
			}
			if (!retryState.votePartyApplied) {
				// Fence the side effect before invoking it. If the call reports an
				// indeterminate failure, a listener retry must not increment the party again.
				retryState.votePartyApplied = true;
				addVoteParty();
			}

			if (!retryState.totalsApplied) {
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
					retryState.totals = text;
					retryState.totalsApplied = true;
					getProxyMySQL().update(uuid, update);
				} else {
					text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
				}
				}
				if (text == null) {
					text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
				}
				retryState.totals = text;
				retryState.totalsApplied = true;
			} else {
				text = retryState.totals;
			}
			if (text == null) {
				text = new VoteTotalsSnapshot(0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired, 0);
			}

			VoteLogStatus voteStatus = VoteLogStatus.IMMEDIATE;
			boolean standaloneProxyBroadcast = canValidateStandaloneBroadcast && (proxyBroadcastHandled
					|| proxyBroadcastDecider.usesImmediateForwarding(playerOnline));
			if (getConfig().getSendVotesToAllServers() && retryState.rewardServers == null) {
				retryState.rewardServers = new LinkedHashSet<>(getAllAvailableServers());
			}
			Set<String> rewardServers = retryState.rewardServers == null
					? Collections.emptySet() : retryState.rewardServers;
			Set<String> proxyBroadcastTargets = Collections.emptySet();
			OfflineBungeeVote standaloneBroadcastState = null;
			boolean standaloneBroadcastStatePersisted = false;
			if (standaloneProxyBroadcast) {
				// A handled queued broadcast was necessarily sampled while the player was
				// offline. Retry only targets that did not previously accept delivery.
				proxyBroadcastTargets = proxyBroadcastHandled ? new LinkedHashSet<>(broadcastTargets)
						: proxyBroadcastDecider.resolveTargets(false, null);
				Set<String> remainingTargets = new LinkedHashSet<>(proxyBroadcastTargets);
				remainingTargets.removeAll(broadcastForwardedServers);
				standaloneBroadcastState = retryState.standaloneBroadcastState;
				if (standaloneBroadcastState == null) {
					standaloneBroadcastState = new OfflineBungeeVote(voteId, player, uuid, service, time, realVote,
							text == null ? "" : text.toString(), false, true, proxyBroadcastTargets,
							broadcastForwardedServers, !getConfig().getSendVotesToAllServers(), Collections.emptyMap(), queuedVote == null
								? Collections.emptyMap() : queuedVote.getHttpBroadcastDeliveryIds());
					if (getConfig().getSendVotesToAllServers()) markRewardJournalTargets(standaloneBroadcastState, rewardServers);
					retryState.standaloneBroadcastState = standaloneBroadcastState;
					retryState.rewardJournalOwner = standaloneBroadcastState;
				} else {
					proxyBroadcastTargets = new LinkedHashSet<>(standaloneBroadcastState.getBroadcastTargets());
					broadcastForwardedServers.addAll(standaloneBroadcastState.getBroadcastForwardedServers());
					remainingTargets = new LinkedHashSet<>(proxyBroadcastTargets);
					remainingTargets.removeAll(broadcastForwardedServers);
				}
				// The canonical broadcast journal must reach durable storage before the
				// first target sees the message. Otherwise a proxy crash after one accepted
				// send can permanently lose every remaining target.
				standaloneBroadcastStatePersisted = persistAndSendStandaloneBroadcast(uuid,
						standaloneBroadcastState, remainingTargets, broadcastForwardedServers);
				retryState.broadcastForwardedServers.addAll(broadcastForwardedServers);
				if (!standaloneBroadcastStatePersisted) {
					logSevere("Unable to durably journal standalone broadcast for " + uuid);
					return QueuedVoteResult.RETRY;
				}
				if (queuedVote != null) {
					// Keep the source row's in-memory completion state aligned with the
					// canonical broadcast journal. If persisting the final processed marker
					// fails, processQueue can retry that marker without publishing an already
					// accepted broadcast again.
					queuedVote.getBroadcastForwardedServers().addAll(broadcastForwardedServers);
					for (String forwardedServer : broadcastForwardedServers) {
						queuedVote.setHttpBroadcastDeliveryId(forwardedServer, null);
					}
				}
			}

			// ===========================
			// Send vote(s) to backend(s)
			// ===========================
			if (getConfig().getSendVotesToAllServers()) {
				OfflineBungeeVote rewardJournalOwner = retryState.rewardJournalOwner;
				if (rewardJournalOwner == null) {
					rewardJournalOwner = createCachedRewardVote(voteId, player, uuid, service, time, realVote,
							text.toString(), false);
					markRewardJournalTargets(rewardJournalOwner, rewardServers);
					retryState.rewardJournalOwner = rewardJournalOwner;
					if (!getVoteCacheHandler().addOnlineVoteDurably(uuid, rewardJournalOwner)) {
						if (getVoteCacheHandler().retainOnlineVoteForPersistenceRetry(uuid, rewardJournalOwner)) {
							scheduleCachedVoteDeliveryRetry();
						}
						return QueuedVoteResult.RETRY;
					}
				}
				for (String server : rewardServers) {
					if (retryState.deliveredRewardServers.contains(server)) continue;
					OfflineBungeeVote rewardState = retryState.rewardStates.get(server.toLowerCase(Locale.ROOT));
					if (rewardState == null) {
						rewardState = createCachedRewardVote(voteId, player, uuid, service, time,
								realVote, text.toString(), standaloneProxyBroadcast);
						retryState.rewardStates.put(server.toLowerCase(Locale.ROOT), rewardState);
					}
					// Every target begins as pending so a crash before its send cannot lose
					// the reward. Accepted deliveries are marked complete below.
					rewardState.setRewardDelivered(false);
					if (rewardState.getHttpDeliveryId(server) == null) {
						rewardState.setHttpDeliveryId(server, rewardJournalDeliveryId(rewardJournalOwner, server));
					}
					if (!getVoteCacheHandler().addServerVoteDurably(server, rewardState)) {
						if (getVoteCacheHandler().retainServerVoteForPersistenceRetry(server, rewardState)) {
							scheduleCachedVoteDeliveryRetry();
						}
						logSevere("Unable to durably journal vote reward for " + server);
						return QueuedVoteResult.RETRY;
					}
				}
				if (!retryState.rewardJournalsDurable) {
					// The source owner is removable only after every target-specific row is durable.
					rewardJournalOwner.setRewardDelivered(true);
					rewardJournalOwner.setDeliveryStateDirty(true);
					if (!persistOnlineVoteDelivery(uuid, rewardJournalOwner)) return QueuedVoteResult.RETRY;
					retryState.rewardJournalsDurable = true;
					if (!rewardJournalOwner.isProxyBroadcastHandled() || rewardJournalOwner.isProxyBroadcastComplete()) {
						if (!getVoteCacheHandler().tryRemoveOnlineVote(uuid, rewardJournalOwner)) {
							scheduleCachedVoteDeliveryRetry();
						}
					}
				}
				for (String s : rewardServers) {
					if (retryState.deliveredRewardServers.contains(s)) continue;

					boolean forceCache = getConfig().getWaitForUserOnline()
							&& (!playerOnline || playerServer == null || !playerServer.equalsIgnoreCase(s));

					if (forceCache) {
						debug("Forcing vote to cache for server " + s);
					}

					if ((!isSomeoneOnlineServerForVoteRouting(s) && method.requiresPlayerOnline()) || forceCache) {
						voteStatus = VoteLogStatus.CACHED;
						debug("Caching vote for " + player + " on " + service + " for " + s);
					} else {
						boolean broadcastHere = !broadcastForwardedServers.contains(s);
						if (broadcastHere && getConfig().getProxyBroadcastEnabled()) {
							Set<String> targets = standaloneProxyBroadcast ? proxyBroadcastTargets
									: proxyBroadcastDecider.resolveTargets(playerOnline, playerServer);
							broadcastHere = proxyBroadcastDecider.shouldBroadcast(s, targets);
						}

						OfflineBungeeVote pendingVote = retryState.rewardStates.get(s.toLowerCase(Locale.ROOT));
						boolean rewardAccepted = sendVoteEnvelopeAccepted(s, 2,
								VotingPluginWire.vote(player, uuid, service, time, true, realVote, text.toString(),
										voteId, getConfig().getBungeeManageTotals(), broadcastHere, 1, 1), pendingVote);
						if (!rewardAccepted) {
							pendingVote.setRewardDelivered(false);
							pendingVote.setDeliveryStateDirty(true);
							if (!persistServerVoteDelivery(s, pendingVote)) {
								logSevere("Unable to persist the rejected vote delivery for " + s);
								return QueuedVoteResult.RETRY;
							}
							voteStatus = VoteLogStatus.CACHED;
							debug("Caching vote after the transport rejected delivery for " + s);
						} else {
							pendingVote.setRewardDelivered(true);
							retryState.deliveredRewardServers.add(s);
							pendingVote.setDeliveryStateDirty(true);
							if (!persistServerVoteDelivery(s, pendingVote)) return QueuedVoteResult.RETRY;
							getVoteCacheHandler().removeServerVotes(s,
									new ArrayList<>(Collections.singletonList(pendingVote)));
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

					OfflineBungeeVote pendingVote = retryState.rewardStates.get(server.toLowerCase(Locale.ROOT));
					if (pendingVote == null) {
						pendingVote = standaloneProxyBroadcast
							? new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString(), false,
									true, proxyBroadcastTargets, broadcastForwardedServers, false, Collections.emptyMap(),
									standaloneBroadcastState.getHttpBroadcastDeliveryIds())
							: createCachedRewardVote(voteId, player, uuid, service, time, realVote, text.toString(), false);
						retryState.rewardStates.put(server.toLowerCase(Locale.ROOT), pendingVote);
					}
					boolean rewardAccepted = retryState.deliveredRewardServers.contains(server);
					if (!rewardAccepted) {
						rewardAccepted = sendVoteEnvelopeAccepted(server, 1,
								VotingPluginWire.voteOnline(player, uuid, service, time, true, realVote, text.toString(),
										voteId, getConfig().getBungeeManageTotals(), broadcastHere, 1, 1), pendingVote);
						if (rewardAccepted) retryState.deliveredRewardServers.add(server);
					}
					if (!rewardAccepted) {
						if (standaloneBroadcastState != null) {
							standaloneBroadcastState.setRewardDelivered(false);
							standaloneBroadcastState.setDeliveryStateDirty(true);
							if (!persistOnlineVoteDelivery(uuid, standaloneBroadcastState)) {
								return QueuedVoteResult.RETRY;
							}
						}
						if (!getVoteCacheHandler().addOnlineVoteDurably(uuid, pendingVote)) {
							if (getVoteCacheHandler().retainOnlineVoteForPersistenceRetry(uuid, pendingVote)) {
								scheduleCachedVoteDeliveryRetry();
							}
							logSevere("Unable to durably cache the rejected online vote delivery for " + uuid);
							return QueuedVoteResult.RETRY;
						}
						voteStatus = VoteLogStatus.CACHED;
						standaloneBroadcastStatePersisted |= standaloneBroadcastState != null;
						debug("Caching online vote after the transport rejected delivery for " + server);
					} else if (standaloneBroadcastState != null
							&& standaloneBroadcastState.isProxyBroadcastComplete()) {
						getVoteCacheHandler().removeOnlineVote(uuid, standaloneBroadcastState);
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
					if (standaloneBroadcastState != null) {
						standaloneBroadcastState.setRewardDelivered(false);
						standaloneBroadcastState.setDeliveryStateDirty(true);
						if (!persistOnlineVoteDelivery(uuid, standaloneBroadcastState)) {
							return QueuedVoteResult.RETRY;
						}
					}
					OfflineBungeeVote cachedReward = standaloneProxyBroadcast
							? new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text.toString(), false,
									true, proxyBroadcastTargets, broadcastForwardedServers, false, Collections.emptyMap(),
									standaloneBroadcastState.getHttpBroadcastDeliveryIds())
							: createCachedRewardVote(voteId, player, uuid, service, time, realVote, text.toString(), false);
					retryState.pendingOnlineRewardState = cachedReward;
					boolean cachedDurably = getVoteCacheHandler().addOnlineVoteDurably(uuid, cachedReward);
					if (!cachedDurably) {
						if (getVoteCacheHandler().retainOnlineVoteForPersistenceRetry(uuid, cachedReward)) {
							scheduleCachedVoteDeliveryRetry();
						}
						logSevere("Unable to durably cache online vote for " + uuid
								+ "; retaining it for persistence retry");
						return QueuedVoteResult.RETRY;
					}
					retryState.pendingOnlineRewardState = null;
					standaloneBroadcastStatePersisted |= standaloneBroadcastState != null;
					debug("Caching online vote for " + player + " on " + service);
				}

				int delay = 2;
				for (String s : getAllAvailableServers()) {
					globalMessageProxyHandler.sendMessage(s, delay + 1, VotingPluginWire.voteUpdate(uuid,
							votePartyVotes, currentVotePartyVotesRequired, service, time, text.toString()));
					delay += 2;
				}
			}

			if (!persistUncachedStandaloneBroadcast(uuid, standaloneBroadcastState,
					standaloneBroadcastStatePersisted)) {
				logSevere("Unable to durably cache the pending standalone broadcast for " + uuid);
				return QueuedVoteResult.RETRY;
			}

			// Vote logging
			if (voteLogMysqlTable != null && getConfig().getVoteLoggingEnabled()) {
				voteLogMysqlTable.logVote(voteId, voteStatus, service, uuid, player, time,
						getVoteCacheHandler().getProxyCachedTotal(uuid));
			}

			// ===========================
			// Multiproxy forwarding
			// ===========================
			if (!retryState.multiProxyForwardingHandled && getConfig().getMultiProxySupport()
					&& getConfig().getPrimaryServer()
					// An overflowed received envelope carries its sender in this field. It
					// must be acknowledged after local completion, never forwarded again.
					&& (queuedVote == null || queuedVote.getMultiProxyOrigin().isBlank())) {
				if (!getConfig().getMultiProxyOneGlobalReward()) {
					debug("Sending global proxy vote envelope");
					if (!beginMultiProxyForwarding(retryState,
							queuedVote == null ? retryState.queuedVote : queuedVote, player, uuid, service, time, realVote, text)) {
						return QueuedVoteResult.RETRY;
					}
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
						if (!beginMultiProxyForwarding(retryState,
								queuedVote == null ? retryState.queuedVote : queuedVote, player, uuid, service, time, realVote, text)) {
							return QueuedVoteResult.RETRY;
						}
					} else {
						debug("Not sending global proxy message for voteonline, player already got reward");
						if (!markMultiProxyForwardingHandled(retryState, queuedVote)) return QueuedVoteResult.RETRY;
					}
				}
			}
			if (queuedVote != null) {
				queuedVote.setProcessed(true);
				if (!queuedVote.getMultiProxyOrigin().isBlank()) {
					// Persist the receiver's completion phase with the processed fence. A
					// failed tombstone write must resume completion, never skip to deletion.
					queuedVote.setMultiProxyCompletionPending(true);
				}
				if (!persistTimeVoteDelivery(queuedVote)) {
					if (!queuedVote.getMultiProxyOrigin().isBlank()) {
						// The receiver may delete its queue row only after a durable completion
						// fence exists. That tombstone also makes a sender retry idempotent.
						if (getVoteCacheHandler().hasMultiProxyVoteCompletion(voteId)
								|| getVoteCacheHandler().markMultiProxyVoteCompletedDurably(voteId)) {
							acknowledgeCompletedMultiProxyVote(voteId, queuedVote.getMultiProxyOrigin());
							if (getVoteCacheHandler().removeTimeVote(queuedVote)) {
								liveVoteRetries.remove(voteId);
								return QueuedVoteResult.SUCCESS;
							}
						}
						warn("Unable to persist completed forwarded vote " + queuedVote.getVoteId()
								+ "; retaining its receiver completion fence for retry");
						return QueuedVoteResult.RETRY;
					}
					// Deleting the completed source row is itself a durable completion
					// record. Prefer that fallback when updating the marker is unavailable.
					if (getVoteCacheHandler().removeTimeVote(queuedVote)) {
						liveVoteRetries.remove(voteId);
						return QueuedVoteResult.SUCCESS;
					}
					if (getVoteCacheHandler().markTimeVoteCompletedDurably(queuedVote)) {
						liveVoteRetries.remove(voteId);
						return QueuedVoteResult.SUCCESS;
					}
					warn("Unable to persist or remove completed rollover vote " + queuedVote.getVoteId()
							+ "; retaining its live completion fence for retry");
					return QueuedVoteResult.RETRY;
				}
			}
			liveVoteRetries.remove(voteId);
			return QueuedVoteResult.SUCCESS;
		} catch (IllegalArgumentException e) {
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			return QueuedVoteResult.RETRY;
		}
	}

	private UUID legacyTimedVoteId(VoteTimeQueue vote) {
		return vote.legacyTimedVoteId();
	}

	/**
	 * Creates/persists the sender outbox before the first publish. Redis and socket
	 * publish APIs are fire-and-forget, so their return value is never a delivery
	 * acknowledgement. The record stays until every configured peer confirms its
	 * own durable completion by stable vote ID.
	 */
	private boolean beginMultiProxyForwarding(LiveVoteRetryState retryState, VoteTimeQueue queuedVote, String player,
			String uuid, String service, long time, boolean realVote, VoteTotalsSnapshot totals) {
		if (multiProxyHandler == null) return false;
		multiProxyHandler.announceMultiProxyVoteCapability();
		Set<String> recipients = multiProxyHandler.getMultiProxyVoteRecipients();
		Set<String> configuredRecipients = multiProxyHandler.getConfiguredMultiProxyVoteRecipients();
		// Keep custom MultiProxyHandler integrations that override only the
		// established recipient method source-compatible while the base handler
		// learns capabilities.
		if (configuredRecipients.isEmpty() && !recipients.isEmpty()) configuredRecipients.addAll(recipients);
		if (configuredRecipients.isEmpty()) return true;
		Set<String> legacyRecipients = new LinkedHashSet<>(configuredRecipients);
		legacyRecipients.removeAll(recipients);
		if (recipients.isEmpty()) {
			// Older peers do not understand acknowledgements. Preserve their historical
			// fire-and-forget route instead of creating an outbox they can never ACK.
			multiProxyHandler.sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyVote(player, uuid, service, time,
					false, realVote, totals == null ? "" : totals.toString(), findLiveVoteId(retryState), false, false,
					1, 1, getConfig().getProxyServerName()), legacyRecipients);
			return true;
		}
		VoteTimeQueue outbox = queuedVote;
		boolean newlyCreated = false;
		if (outbox == null) {
			outbox = new VoteTimeQueue(null, player, service, time, false, Collections.emptySet(),
					Collections.emptySet(), totals == null ? "" : totals.toString(), true, uuid);
			// The live retry key is the stable vote ID; copy it from the enclosing state
			// by locating its identity rather than creating a new duplicate record.
			outbox.setVoteId(findLiveVoteId(retryState));
			outbox.requireMultiProxyAcknowledgements(getConfig().getProxyServerName(), recipients);
			outbox.setRealVote(realVote);
			outbox.setDeliveryStateDirty(true);
			retryState.queuedVote = outbox;
			if (outbox.getVoteId() == null || !getVoteCacheHandler().addTimeVoteToCache(outbox)) return false;
			newlyCreated = true;
		} else {
			boolean alreadyQueued = false;
			for (VoteTimeQueue candidate : getVoteCacheHandler().getTimeChangeQueue()) {
				if (candidate != null && java.util.Objects.equals(outbox.getVoteId(), candidate.getVoteId())) {
					alreadyQueued = true;
					break;
				}
			}
			if (!alreadyQueued && !getVoteCacheHandler().addTimeVoteToCache(outbox)) {
				// A previous durable admission may have failed after the retry state kept
				// this object in memory. Re-admit it before any publish;
				// addTimeVoteToCache is idempotent for an already-durable queue entry.
				return false;
			}
			// A recovered admission is the first safe chance to publish to legacy peers.
			// Already-queued retries must not repeat that fire-and-forget copy.
			newlyCreated = !alreadyQueued;
		}
		if (!outbox.isMultiProxyForwardingRequired()) {
			outbox.requireMultiProxyAcknowledgements(getConfig().getProxyServerName(), recipients);
			outbox.setRealVote(realVote);
			outbox.setProcessed(true);
			outbox.setDeliveryStateDirty(true);
			if (!persistTimeVoteDelivery(outbox)) return false;
		}
		if (outbox.hasCompletedMultiProxyAcknowledgements()) {
			return finishMultiProxyRetirement(retryState, outbox);
		}
		// A successful publish only means the transport accepted the invocation.
		// Keep the durable row and wait for an acknowledgement before continuing.
		if (newlyCreated && !legacyRecipients.isEmpty()) {
			// Never retry this legacy copy as part of the ACK outbox: a legacy peer has
			// no receiver dedupe/ACK contract, while capable peers stay fully durable.
			multiProxyHandler.sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyVote(player, uuid, service, time,
					false, realVote, totals == null ? "" : totals.toString(), outbox.getVoteId(), false, false, 1, 1,
					outbox.getMultiProxyOrigin()), legacyRecipients);
		}
		if (!sendDurableMultiProxyOutbox(outbox)) return false;
		scheduleTimeVoteRetry();
		return false;
	}

	private UUID findLiveVoteId(LiveVoteRetryState retryState) {
		for (Map.Entry<UUID, LiveVoteRetryState> entry : liveVoteRetries.entrySet()) {
			if (entry.getValue() == retryState) return entry.getKey();
		}
		return null;
	}

	/** Re-sends an unacknowledged outbox using the exact persisted stable ID. */
	private boolean retryDurableMultiProxyOutbox(VoteTimeQueue outbox) {
		if (outbox.hasCompletedMultiProxyAcknowledgements()) {
			return finishMultiProxyRetirement(null, outbox);
		}
		sendDurableMultiProxyOutbox(outbox);
		return false;
	}

	private boolean sendDurableMultiProxyOutbox(VoteTimeQueue outbox) {
		if (multiProxyHandler == null || outbox.getVoteId() == null || outbox.getMultiProxyOrigin().isBlank()) return false;
		Set<String> pending = new LinkedHashSet<>(outbox.getMultiProxyRecipients());
		pending.removeAll(outbox.getMultiProxyAcknowledgedServers());
		if (pending.isEmpty()) return true;
		return multiProxyHandler.sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyVote(outbox.getName(),
				outbox.getUuid(), outbox.getService(), outbox.getTime(), false, outbox.isRealVote(), outbox.getTotals(),
				outbox.getVoteId(), false, false, 1, 1, outbox.getMultiProxyOrigin()),
				pending);
	}

	/** Handles an ACK only after confirming it belongs to a configured recipient. */
	private synchronized void handleMultiProxyVoteAcknowledgement(UUID voteId, String recipient) {
		if (voteId == null || recipient == null || recipient.isBlank()) return;
		LiveVoteRetryState state = liveVoteRetries.get(voteId);
		VoteTimeQueue outbox = state == null ? null : state.queuedVote;
		if (outbox == null) {
			for (VoteTimeQueue candidate : getVoteCacheHandler().getTimeChangeQueue()) {
				if (voteId.equals(candidate.getVoteId()) && candidate.isMultiProxyForwardingRequired()) {
					outbox = candidate;
					break;
				}
			}
		}
		if (outbox == null || !outbox.acknowledgeMultiProxyRecipient(recipient)) return;
		outbox.setDeliveryStateDirty(true);
		if (!persistTimeVoteDelivery(outbox)) {
			scheduleTimeVoteRetry();
			return;
		}
		scheduleTimeVoteRetry();
	}

	private synchronized void handleMultiProxyVoteRetirementAcknowledgement(UUID voteId, String recipient) {
		if (voteId == null || recipient == null || recipient.isBlank()) return;
		LiveVoteRetryState state = liveVoteRetries.get(voteId);
		VoteTimeQueue outbox = state == null ? null : state.queuedVote;
		if (outbox == null) {
			for (VoteTimeQueue candidate : getVoteCacheHandler().getTimeChangeQueue()) {
				if (voteId.equals(candidate.getVoteId()) && candidate.isMultiProxyForwardingRequired()) {
					outbox = candidate;
					break;
				}
			}
		}
		if (outbox == null || !outbox.acknowledgeMultiProxyRetirement(recipient)) return;
		outbox.setDeliveryStateDirty(true);
		if (!persistTimeVoteDelivery(outbox)) {
			scheduleTimeVoteRetry();
			return;
		}
		scheduleTimeVoteRetry();
	}

	private boolean finishMultiProxyRetirement(LiveVoteRetryState retryState, VoteTimeQueue outbox) {
		if (!outbox.hasCompletedMultiProxyRetirements()) {
			if (multiProxyHandler != null) {
				for (String recipient : outbox.getPendingMultiProxyRetirements()) {
					multiProxyHandler.requestMultiProxyVoteRetirement(outbox.getVoteId(),
							outbox.getMultiProxyOrigin(), recipient);
				}
			}
			scheduleTimeVoteRetry();
			return false;
		}
		return markMultiProxyForwardingHandled(retryState, outbox);
	}

	/** Persists a queued vote's multi-proxy side-effect fence before later retryable work. */
	private boolean markMultiProxyForwardingHandled(LiveVoteRetryState retryState, VoteTimeQueue queuedVote) {
		if (retryState != null) retryState.multiProxyForwardingHandled = true;
		if (queuedVote == null) return true;
		queuedVote.setMultiProxyForwardingHandled(true);
		queuedVote.setDeliveryStateDirty(true);
		return persistTimeVoteDelivery(queuedVote);
	}

	/**
	 * Creates the reward cache entry for a backend. Standalone proxy broadcast
	 * progress belongs to the single voter-keyed canonical state, not to every
	 * backend row. Marking the local row as already broadcast prevents it from
	 * emitting a second broadcast while its reward waits for the player.
	 */
	protected OfflineBungeeVote createCachedRewardVote(UUID voteId, String player, String uuid, String service, long time,
			boolean realVote, String text, boolean standaloneProxyBroadcast) {
		return new OfflineBungeeVote(voteId, player, uuid, service, time, realVote, text,
				standaloneProxyBroadcast, false, Collections.emptySet(), Collections.emptySet(), false,
				Collections.emptyMap(), Collections.emptyMap());
	}

	private void markRewardJournalTargets(OfflineBungeeVote owner, Set<String> targets) {
		for (String server : targets) {
			String key = owner.getVoteId() + ":reward:" + server;
			owner.setHttpDeliveryId(REWARD_JOURNAL_TARGET_PREFIX + server,
					UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString());
		}
	}

	private boolean isIncompleteRewardJournalOwner(OfflineBungeeVote vote) {
		if (vote == null || vote.isRewardDelivered()) return false;
		for (String key : vote.getHttpDeliveryIds().keySet()) {
			if (key.startsWith(REWARD_JOURNAL_TARGET_PREFIX)) return true;
		}
		return false;
	}

	private String rewardJournalDeliveryId(OfflineBungeeVote owner, String server) {
		String encoded = owner.getHttpDeliveryId(REWARD_JOURNAL_TARGET_PREFIX + server);
		return encoded;
	}

	private boolean isRewardJournalOwner(OfflineBungeeVote vote) {
		if (vote == null) return false;
		for (String key : vote.getHttpDeliveryIds().keySet()) {
			if (key.startsWith(REWARD_JOURNAL_TARGET_PREFIX)) return true;
		}
		return false;
	}

	/** Retries cleanup for an owner whose target rows were already materialized. */
	private boolean retryCompletedRewardJournalOwner(String uuid, OfflineBungeeVote vote) {
		if (uuid == null || vote == null || !vote.isRewardDelivered() || !isRewardJournalOwner(vote)) return false;
		if (vote.isProxyBroadcastHandled() && !vote.isProxyBroadcastComplete()) return false;
		if (!getVoteCacheHandler().tryRemoveOnlineVote(uuid, vote)) {
			scheduleCachedVoteDeliveryRetry();
		}
		return true;
	}

	private boolean materializeRewardJournalOwner(String uuid, OfflineBungeeVote owner) {
		for (Map.Entry<String, String> entry : owner.getHttpDeliveryIds().entrySet()) {
			if (!entry.getKey().startsWith(REWARD_JOURNAL_TARGET_PREFIX)) continue;
			String normalizedServer = entry.getKey().substring(REWARD_JOURNAL_TARGET_PREFIX.length());
			if (normalizedServer.isEmpty()) continue;
			String server = normalizedServer;
			for (String configured : getAllConfiguredServers()) {
				if (configured.equalsIgnoreCase(normalizedServer)) {
					server = configured;
					break;
				}
			}
			String deliveryId = entry.getValue();
			OfflineBungeeVote reward = createCachedRewardVote(owner.getVoteId(), owner.getPlayerName(), owner.getUuid(),
					owner.getService(), owner.getTime(), owner.isRealVote(), owner.getText(),
					owner.isProxyBroadcastHandled());
			reward.setHttpDeliveryId(server, deliveryId);
			if (!getVoteCacheHandler().addServerVoteDurably(server, reward)) return false;
		}
		owner.setRewardDelivered(true);
		owner.setDeliveryStateDirty(true);
		if (!persistOnlineVoteDelivery(uuid, owner)) return false;
		if (!owner.isProxyBroadcastHandled() || owner.isProxyBroadcastComplete()) {
			if (!getVoteCacheHandler().tryRemoveOnlineVote(uuid, owner)) {
				scheduleCachedVoteDeliveryRetry();
			}
		}
		return true;
	}

	private OfflineBungeeVote createCachedRewardVote(OfflineBungeeVote deliveryState,
			boolean standaloneProxyBroadcast) {
		return new OfflineBungeeVote(deliveryState.getVoteId(), deliveryState.getPlayerName(), deliveryState.getUuid(),
				deliveryState.getService(), deliveryState.getTime(), deliveryState.isRealVote(), deliveryState.getText(),
				standaloneProxyBroadcast, false, Collections.emptySet(), Collections.emptySet(), false,
				OfflineBungeeVote.decodeHttpDeliveryIds(deliveryState.encodeHttpDeliveryIds()), Collections.emptyMap());
	}

	protected boolean persistUncachedStandaloneBroadcast(String uuid, OfflineBungeeVote state,
			boolean alreadyPersisted) {
		if (alreadyPersisted || state == null || state.isProxyBroadcastComplete()) return true;
		// This canonical row retries only the standalone broadcast. Reward delivery
		// remains owned by the target-specific server/online cache entry.
		state.setRewardDelivered(true);
		state.setBroadcastForwarded(false);
		if (getVoteCacheHandler().addOnlineVoteDurably(uuid, state)) return true;
		boolean retained = getVoteCacheHandler().retainOnlineVoteForPersistenceRetry(uuid, state);
		if (retained) scheduleCachedVoteDeliveryRetry();
		return false;
	}

	protected boolean persistAndSendStandaloneBroadcast(String uuid, OfflineBungeeVote state,
			Set<String> remainingTargets, Set<String> forwardedServers) {
		if (!getVoteCacheHandler().addOnlineVoteDurably(uuid, state)) {
			boolean retained = getVoteCacheHandler().retainOnlineVoteForPersistenceRetry(uuid, state);
			if (retained) scheduleCachedVoteDeliveryRetry();
			return false;
		}
		for (String target : remainingTargets) {
			Set<String> forwarded = sendProxyBroadcast(Collections.singleton(target), uuid, state.getPlayerName(),
					state.getService(), state.getTime(), state.getText(), false, state);
			forwardedServers.addAll(forwarded);
			if (state.getBroadcastForwardedServers().addAll(forwarded) || state.isDeliveryStateDirty()) {
				state.setBroadcastForwarded(state.isProxyBroadcastComplete());
				if (!persistOnlineVoteDelivery(uuid, state)) return false;
			}
		}
		return true;
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
