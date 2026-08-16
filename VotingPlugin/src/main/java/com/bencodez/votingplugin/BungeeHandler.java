// File: com/bencodez/votingplugin/BungeeHandler.java
package com.bencodez.votingplugin;

import java.io.File;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.Listener;
import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;

/**
 * Handler for Bungee/proxy server integration.
 */
public class BungeeHandler implements Listener {

	private static final long PROCESSED_VOTE_TTL_MILLIS = TimeUnit.MINUTES.toMillis(30);
	private static final long PRESENCE_HEARTBEAT_SECONDS = 30;
	private static final long PRESENCE_SNAPSHOT_REQUEST_MIN_INTERVAL_NANOS = TimeUnit.SECONDS.toNanos(30);
	private static final int PRESENCE_SNAPSHOT_CHUNK_SIZE = 100;

	@Getter
	private final ConcurrentHashMap<UUID, Long> processedWireVotes = new ConcurrentHashMap<>();
	private final ConcurrentHashMap<String, BackendPlayerPresenceSession> playerPresenceSessions = new ConcurrentHashMap<>();
	private final Object presenceLifecycleLock = new Object();
	private boolean presenceReporting;
	private String presenceServer;
	private UUID presenceIncarnationId;
	private long presenceStartedAt;
	private long presenceLastTimestamp;
	private UUID lastPresenceSnapshotRequestId;
	private long lastPresenceSnapshotRequestAtNanos;
	private ScheduledFuture<?> presenceHeartbeatTask;
	@Getter
	private ClientHandler clientHandler;

	private EncryptionHandler encryptionHandler;

	@Getter
	private BungeeMethod method;

	private VotingPluginMain plugin;

	@Getter
	private int bungeeVotePartyCurrent = -2;

	@Getter
	private int bungeeVotePartyRequired = -2;

	@Getter
	private SocketHandler socketHandler;

	private GlobalDataHandler globalDataHandler;

	@Getter
	private ScheduledExecutorService timer;

	@Getter
	private RedisHandler redisHandler;

	@Getter
	private GlobalMessageHandler globalMessageHandler;

	private Thread redisThread;

	@Getter
	private MySqlMessenger backendMysqlMessenger;

	@Getter
	private MqttHandler mqttHandler;

	/**
	 * Constructs a new BungeeHandler.
	 *
	 * @param plugin the main plugin instance
	 */
	public BungeeHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Checks and processes global data from the global data handler.
	 */
	public void checkGlobalData() {
		HashMap<String, DataValue> data = globalDataHandler.getExact(plugin.getBungeeSettings().getServer());

		if (data.containsKey("ForceUpdate")) {
			boolean b = checkGlobalDataTimeValue(data.get("ForceUpdate"));
			if (b) {
				if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
					plugin.getMysql().clearCacheBasic();
				}
				plugin.getUserManager().getDataManager().clearCache();
				plugin.setUpdate(true);
				plugin.update();
				globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), "ForceUpdate", false);
			}
		}

		boolean forceUpdate = false;

		if (checkGlobalDataTime(TimeType.MONTH, data)) {
			forceUpdate = true;
		}
		if (checkGlobalDataTime(TimeType.WEEK, data)) {
			forceUpdate = true;
		}
		if (checkGlobalDataTime(TimeType.DAY, data)) {
			forceUpdate = true;
		}

		if (forceUpdate) {
			HashMap<String, DataValue> dataToSet = new HashMap<>();
			dataToSet.put("FinishedProcessing", new DataValueBoolean(true));
			dataToSet.put("Processing", new DataValueBoolean(false));
			globalDataHandler.setData(plugin.getBungeeSettings().getServer(), dataToSet);
		}
	}

	/**
	 * Checks global data for a time type change.
	 *
	 * @param type the time type to check
	 * @param data the global data map
	 * @return true if currently processing a time change
	 */
	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		boolean isProcessing = false;
		if (data.containsKey(type.toString())) {

			DataValue value = data.get(type.toString());
			boolean b = checkGlobalDataTimeValue(value);
			if (b) {
				long lastUpdated = Long.valueOf(data.get("LastUpdated").getString()).longValue();
				plugin.debug("LastUpdated: " + lastUpdated);
				if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastUpdated > 1000 * 60 * 60
						* 2) {
					plugin.getLogger().warning("Ignoring bungee time change since it was more than 2 hours ago");
					globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), type.toString(), false);
					return false;
				}

				globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), "Processing", true);
				isProcessing = true;

				plugin.debug("Detected time change from bungee: " + type.toString());
				plugin.getTimeChecker().forceChanged(type, false, true, true);
				globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), type.toString(), false);

				HashMap<String, Object> fields = new HashMap<>();
				fields.put("server", plugin.getBungeeSettings().getServer());
				sendSubChannel("TimeChangeFinished", fields);
			}
		}
		return isProcessing;
	}

	/**
	 * Checks and extracts the boolean value from a DataValue.
	 *
	 * @param data the data value
	 * @return the boolean value
	 */
	public boolean checkGlobalDataTimeValue(DataValue data) {
		if (data.isBoolean()) {
			return data.getBoolean();
		}
		return Boolean.valueOf(data.getString());
	}

	/**
	 * Closes and cleans up all handlers and connections.
	 */
	public void close() {
		stopPresenceReporting();

		if (backendMysqlMessenger != null) {
			backendMysqlMessenger.shutdown();
		}

		if (socketHandler != null) {
			socketHandler.closeConnection();
		}
		if (clientHandler != null) {
			clientHandler.stopConnection();
		}
		plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
		plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);
		if (globalDataHandler != null) {
			globalDataHandler.getGlobalMysql().close();
		}
	}

	/**
	 * Loads and initializes the bungee handler with the configured method.
	 */
	public void load() {
		plugin.debug("Loading bungee handler");

		method = BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod());

		plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		loadGlobalMysql();

		globalMessageHandler = new GlobalMessageHandler() {
			@Override
			public void sendMessage(JsonEnvelope envelope) {
				if (method.equals(BungeeMethod.MYSQL)) {
					try {
						backendMysqlMessenger.sendToProxy(envelope);
					} catch (SQLException e) {
						e.printStackTrace();
					}
				} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
					plugin.getPluginMessaging().sendEnvelope(envelope);
				} else if (method.equals(BungeeMethod.SOCKETS)) {
					sendEnvelopeSocket(envelope);
				} else if (method.equals(BungeeMethod.REDIS)) {
					redisHandler.publishEnvelope(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin",
							envelope);
				} else if (method.equals(BungeeMethod.MQTT)) {
					try {
						mqttHandler.publishEnvelope(
								plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/proxy", envelope);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		};

		// ==========================
		// Vote / VoteOnline (wire decode)
		// ==========================

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				handleWireVote(msg);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_ONLINE) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				handleWireVote(msg);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_DELAY_REJECTED) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				handleWireVoteDelayRejected(msg);
			}
		});

		if (method.supportsBackendPresence()) {
			globalMessageHandler.addListener(
					new GlobalMessageListener(VotingPluginWire.SUB_PRESENCE_SNAPSHOT_REQUEST) {
						@Override
						public void onReceive(JsonEnvelope msg) {
							handlePresenceSnapshotRequest(msg);
						}
					});
		}
		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_UPDATE) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				// Wire decode
				VotingPluginWire.VoteUpdate v = VotingPluginWire.readVoteUpdate(msg);

				String playerUuid = v.uuid;
				if (playerUuid == null || playerUuid.isEmpty()) {
					return;
				}

				plugin.debug("pluginmessaging voteupdate received for " + playerUuid + ": " + v.votePartyCurrent + "/"
						+ v.votePartyRequired + " on " + v.service);

				// Vote party cache update
				if (v.votePartyCurrent >= 0 || bungeeVotePartyCurrent == -2) {
					bungeeVotePartyCurrent = v.votePartyCurrent;
				}
				if (v.votePartyRequired >= 0 || bungeeVotePartyRequired == -2) {
					bungeeVotePartyRequired = v.votePartyRequired;
				}
				plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
				plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

				VotingPluginUser user = plugin.getVotingPluginUserManager()
						.getVotingPluginUser(UUID.fromString(playerUuid));
				user.cache();

				user.offVote();

				// Optional: update last vote time for a service
				String service = v.service;
				long time = v.time;

				if (service != null && !service.isEmpty() && time > 0) {
					user.setTime(plugin.getVoteSiteManager().getVoteSite(service, true), time);
				} else if (service != null && !service.isEmpty() && time <= 0
						&& plugin.getBungeeSettings().isBungeeDebug()) {
					plugin.debug("Invalid last vote time received from bungee: " + time);
				}

				plugin.setUpdate(true);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_BUNGEE_TIME_CHANGE) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				checkGlobalData();
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_BROADCAST) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				Map<String, String> f = msg.getFields();

				final String uuidStr = nvl(f.get(VotingPluginWire.K_UUID));
				final String playerNameRaw = nvl(f.get(VotingPluginWire.K_PLAYER));
				final String service = nvl(f.get(VotingPluginWire.K_SERVICE));

				if (uuidStr.isEmpty() || service.isEmpty()) {
					return;
				}

				UUID javaUuid;
				try {
					javaUuid = UUID.fromString(uuidStr);
				} catch (Exception e) {
					plugin.getLogger().warning("Invalid UUID in VoteBroadcast: " + uuidStr);
					return;
				}

				// New fields (May use later)
				@SuppressWarnings("unused")
				final long time = readLongSafe(f.get(VotingPluginWire.K_TIME), 0L);
				final String totalsRaw = nvl(f.get(VotingPluginWire.K_TOTALS));
				final VoteTotalsSnapshot totals = totalsRaw.isEmpty() ? null
						: VoteTotalsSnapshot.parseStorage(totalsRaw);

				VoteSite voteSite = plugin.getVoteSiteManager()
						.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, service), true);

				if (voteSite == null) {
					plugin.getLogger().warning("No voting site with the service site: '" + service + "'");
					return;
				}
				if (!voteSite.isEnabled()) {
					plugin.debug("Votesite: " + voteSite.getKey() + " is not enabled (VoteBroadcast)");
					return;
				}

				// Same user retrieval strategy: UUID + (possibly empty) name
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid,
						playerNameRaw);

				// Keep cache/name current like normal vote path does
				user.cache();
				user.updateName(true);

				// Same broadcast logic as PlayerVoteListener
				if (plugin.getBroadcastHandler() == null) {
					return;
				}

				if (user.isVanished()) {
					plugin.debug("Not broadcasting vote for vanished user: " + user.getPlayerName());
					return;
				}

				// New proxies preserve the state sampled when the vote arrived. Fall back to
				// the legacy delivery-time behavior for envelopes from older proxies.
				final boolean online = f.containsKey(VotingPluginWire.K_WAS_ONLINE)
						? Boolean.parseBoolean(f.get(VotingPluginWire.K_WAS_ONLINE))
						: user.isOnline();
				plugin.getBroadcastHandler().broadcastVote(user.getJavaUUID(), user.getPlayerName(),
						voteSite.getDisplayName(), online, totals);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("Status") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				String server = nvl(msg.getFields().get("server"));
				HashMap<String, Object> out = new HashMap<>();
				out.put("server", server);
				sendSubChannel("statusokay", out);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("ServerName") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				String server = nvl(msg.getFields().get("server"));
				if (!plugin.getOptions().getServer().equals(server)) {
					plugin.getLogger().warning("Server name doesn't match in BungeeSettings.yml, should be " + server);
				}
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VotePartyBungee") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				for (final String cmd : plugin.getBungeeSettings().getBungeeVotePartyGlobalCommands()) {
					plugin.getBukkitScheduler().runTask(plugin, new Runnable() {
						@Override
						public void run() {
							Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), cmd);
						}
					});
				}
				for (Player p : Bukkit.getOnlinePlayers()) {
					new RewardBuilder(plugin.getBungeeSettings().getData(), "BungeeVotePartyRewards").send(p);
				}
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VotePartyBroadcast") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				String broadcast = nvl(msg.getFields().get("broadcast"));
				MiscUtils.getInstance().broadcast(broadcast);
			}
		});

		if (method.equals(BungeeMethod.MYSQL)) {
			plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());

			try {
				backendMysqlMessenger = new MySqlMessenger("VotingPlugin",
						plugin.getMysql().getMysql().getConnectionManager().getDataSource(),
						MySqlMessenger.Mode.BACKEND, plugin.getOptions().getServer(), msg -> {
							if (plugin.getBungeeSettings().isBungeeDebug()) {
								plugin.debug("Proxy sent envelope: " + msg.envelope.getSubChannel() + " "
										+ msg.envelope.getFields());
							}
							globalMessageHandler.onMessage(msg.envelope);
						});
			} catch (SQLException e) {
				e.printStackTrace();
			}
		} else if (method.equals(BungeeMethod.REDIS)) {
			redisHandler = new RedisHandler(plugin.getBungeeSettings().getRedisHost(),
					plugin.getBungeeSettings().getRedisPort(), plugin.getBungeeSettings().getRedisUsername(),
					plugin.getBungeeSettings().getRedisPassword(), plugin.getBungeeSettings().getRedisdbindex()) {

				@Override
				public void debug(String message) {
					if (plugin.getBungeeSettings().isBungeeDebug()) {
						plugin.debug(message);
					}
				}
			};

			redisThread = new Thread(new Runnable() {
				@Override
				public void run() {
					if (plugin.isEnabled()) {
						RedisListener listener = redisHandler.createEnvelopeListener(
								plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin_"
										+ plugin.getBungeeSettings().getServer(),
								(ch, env) -> globalMessageHandler.onMessage(env));
						redisHandler.loadListener(listener);
					}
				}
			});
			redisThread.start();

		} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());

			if (plugin.getBungeeSettings().isPluginMessageEncryption()) {
				encryptionHandler = new EncryptionHandler(plugin.getName(),
						new File(plugin.getDataFolder(), "secretkey.key"));
				plugin.getPluginMessaging().setEncryptionHandler(encryptionHandler);
			}

			plugin.getPluginMessaging().setDebug(plugin.getBungeeSettings().isBungeeDebug());

			plugin.getPluginMessaging().add(new PluginMessageHandler() {
				@Override
				public void onReceive(JsonEnvelope envelope) {
					globalMessageHandler.onMessage(envelope);
				}
			});

		} else if (method.equals(BungeeMethod.SOCKETS)) {
			encryptionHandler = new EncryptionHandler(plugin.getName(),
					new File(plugin.getDataFolder(), "secretkey.key"));

			clientHandler = new ClientHandler(plugin.getBungeeSettings().getBungeeServerHost(),
					plugin.getBungeeSettings().getBungeeServerPort(), encryptionHandler,
					plugin.getBungeeSettings().isBungeeDebug());

			socketHandler = new SocketHandler("vp-socket", plugin.getBungeeSettings().getSpigotServerHost(),
					plugin.getBungeeSettings().getSpigotServerPort(), encryptionHandler,
					plugin.getBungeeSettings().isBungeeDebug()) {

				@Override
				public void log(String str) {
					plugin.getLogger().info(str);
				}
			};

			socketHandler.add(new SocketReceiver() {
				@Override
				public void onReceiveEnvelope(JsonEnvelope envelope) {
					globalMessageHandler.onMessage(envelope);
				}
			});

		} else if (method.equals(BungeeMethod.MQTT)) {
			try {
				String id = plugin.getBungeeSettings().getMqttClientID();
				if (id.isEmpty()) {
					id = plugin.getOptions().getServer();
				}
				mqttHandler = new MqttHandler(new MqttServerComm(id, plugin.getBungeeSettings().getMqttBrokerURL(),
						plugin.getBungeeSettings().getMqttUsername(), plugin.getBungeeSettings().getMqttPassword()), 2);

				mqttHandler.subscribeEnvelopes(
						plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/"
								+ plugin.getOptions().getServer(),
						(topic, envelope) -> globalMessageHandler.onMessage(envelope));

			} catch (MqttException e) {
				e.printStackTrace();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		bungeeVotePartyCurrent = plugin.getServerData().getBungeeVotePartyCurrent();
		bungeeVotePartyRequired = plugin.getServerData().getBungeeVotePartyRequired();

		if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
			plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
		}

		startPresenceReporting();
	}

	/**
	 * Announces a player login. Plugin messaging uses the original login envelope;
	 * standalone transports also update the proxy presence tracker.
	 *
	 * @param playerName player name
	 * @param uuid authoritative VotingPlugin UUID
	 */
	public void playerOnline(String playerName, String uuid) {
		if (!method.supportsBackendPresence()) {
			// PLUGINMESSAGING is attached to the player-facing proxy. Preserve the
			// original login notification used for cached rewards and let the proxy
			// provide authoritative online-player/server state.
			if (globalMessageHandler != null) {
				globalMessageHandler.sendMessage(VotingPluginWire.login(playerName, uuid,
						plugin.getBungeeSettings().getServer()));
			}
			return;
		}

		BackendPlayerPresenceSession session = createPresenceSession(playerName, uuid);
		if (session == null) {
			plugin.getLogger().warning("Unable to report player login with invalid identity: " + nvl(playerName));
			return;
		}

		synchronized (presenceLifecycleLock) {
			if (!presenceReporting) {
				return;
			}
			playerPresenceSessions.put(playerKey(session.playerName), session);
			reannouncePresenceStarted();
			long eventTimestamp = nextPresenceTimestamp();
			JsonEnvelope login = VotingPluginWire.login(session.playerName, session.uuid, presenceServer,
					session.connectionId, presenceIncarnationId, presenceStartedAt, eventTimestamp);
			sendPresenceMessage(login);
		}
	}

	/**
	 * Announces the end of the latest presence-tracked player connection. Plugin
	 * messaging relies on the proxy's native disconnect state and sends no logout.
	 *
	 * @param playerName player name
	 */
	public void playerOffline(String playerName) {
		synchronized (presenceLifecycleLock) {
			if (!presenceReporting) {
				return;
			}
			BackendPlayerPresenceSession session = playerPresenceSessions.remove(playerKey(playerName));
			if (session == null || globalMessageHandler == null) {
				return;
			}

			long eventTimestamp = nextPresenceTimestamp();
			sendPresenceMessage(VotingPluginWire.logout(session.playerName, session.uuid,
					presenceServer, session.connectionId, presenceIncarnationId, presenceStartedAt, eventTimestamp));
		}
	}

	private void handlePresenceSnapshotRequest(JsonEnvelope msg) {
		VotingPluginWire.PresenceSnapshotRequest request = VotingPluginWire.readPresenceSnapshotRequest(msg);
		String server;
		UUID backendIncarnationId;
		long backendStartedAt;
		synchronized (presenceLifecycleLock) {
			server = presenceServer;
			backendIncarnationId = presenceIncarnationId;
			backendStartedAt = presenceStartedAt;
			if (!presenceReporting || server == null || request.requestId == null || request.server.isEmpty()
					|| backendIncarnationId == null
					|| !server.equalsIgnoreCase(request.server) || request.backendStartedAt != backendStartedAt
					|| !backendIncarnationId.equals(request.backendIncarnationId)
					|| request.presenceTimestamp <= 0L) {
				return;
			}
			long requestReceivedAtNanos = System.nanoTime();
			// The incarnation match rejects unrelated lifecycle traffic. Request IDs and
			// this cooldown bound duplicate or replayed matching requests on shared
			// transports.
			if (request.requestId.equals(lastPresenceSnapshotRequestId)
					|| (lastPresenceSnapshotRequestId != null
							&& requestReceivedAtNanos - lastPresenceSnapshotRequestAtNanos
									< PRESENCE_SNAPSHOT_REQUEST_MIN_INTERVAL_NANOS)) {
				return;
			}
			lastPresenceSnapshotRequestId = request.requestId;
			lastPresenceSnapshotRequestAtNanos = requestReceivedAtNanos;
		}

		// Transport listeners may run off the Bukkit thread. Snapshot Bukkit state on
		// the server thread before replying.
		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {
			@Override
			public void run() {
				if (!plugin.isEnabled() || globalMessageHandler == null
						|| !isActivePresenceGeneration(server, backendIncarnationId, backendStartedAt)) {
					return;
				}

					long snapshotTimestamp;
					List<VotingPluginWire.PresencePlayer> players = new ArrayList<>();
					synchronized (presenceLifecycleLock) {
						if (!isActivePresenceGeneration(server, backendIncarnationId, backendStartedAt)) {
							return;
						}
						for (Player player : Bukkit.getOnlinePlayers()) {
							BackendPlayerPresenceSession session = getOrCreatePresenceSession(player);
							if (session != null) {
								players.add(new VotingPluginWire.PresencePlayer(session.playerName, session.uuid,
										session.connectionId.toString()));
							}
						}
						// Bukkit player state cannot change while this server-thread task is
						// running, so timestamp the completed capture before sending it.
						snapshotTimestamp = nextPresenceTimestamp();
					}
				int chunkCount = Math.max(1,
						(players.size() + PRESENCE_SNAPSHOT_CHUNK_SIZE - 1) / PRESENCE_SNAPSHOT_CHUNK_SIZE);
				for (int chunkIndex = 0; chunkIndex < chunkCount; chunkIndex++) {
					int fromIndex = chunkIndex * PRESENCE_SNAPSHOT_CHUNK_SIZE;
					int toIndex = Math.min(players.size(), fromIndex + PRESENCE_SNAPSHOT_CHUNK_SIZE);
					sendActivePresenceMessage(server, backendIncarnationId, backendStartedAt,
							VotingPluginWire.presenceSnapshot(server, request.requestId, chunkIndex, chunkCount,
									players.subList(fromIndex, toIndex), backendIncarnationId, backendStartedAt,
									snapshotTimestamp));
				}
			}
		});
	}

	private BackendPlayerPresenceSession getOrCreatePresenceSession(Player player) {
		synchronized (presenceLifecycleLock) {
			if (!presenceReporting) {
				return null;
			}
			String key = playerKey(player.getName());
			BackendPlayerPresenceSession current = playerPresenceSessions.get(key);
			if (current != null) {
				return current;
			}

			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
			String uuid = user == null ? player.getUniqueId().toString() : user.getUUID();
			BackendPlayerPresenceSession created = createPresenceSession(player.getName(), uuid);
			if (created == null) {
				return null;
			}
			BackendPlayerPresenceSession raced = playerPresenceSessions.putIfAbsent(key, created);
			return raced == null ? created : raced;
		}
	}

	private BackendPlayerPresenceSession createPresenceSession(String playerName, String uuid) {
		String name = nvl(playerName).trim();
		String parsedUuid = nvl(uuid).trim();
		if (name.isEmpty() || parsedUuid.isEmpty()) {
			return null;
		}
		try {
			parsedUuid = UUID.fromString(parsedUuid).toString();
		} catch (IllegalArgumentException e) {
			return null;
		}
		return new BackendPlayerPresenceSession(name, parsedUuid, UUID.randomUUID());
	}

	private void startPresenceReporting() {
		if (globalMessageHandler == null || method == null || !method.supportsBackendPresence()) {
			return;
		}
		String server = plugin.getBungeeSettings().getServer();
		synchronized (presenceLifecycleLock) {
			long now = System.currentTimeMillis();
			presenceIncarnationId = UUID.randomUUID();
			presenceStartedAt = now;
			presenceLastTimestamp = now;
			presenceServer = server;
			presenceReporting = true;
			lastPresenceSnapshotRequestId = null;
			lastPresenceSnapshotRequestAtNanos = 0L;
			sendPresenceMessage(VotingPluginWire.backendStarted(server, presenceIncarnationId, presenceStartedAt,
					now));
			sendPresenceMessage(VotingPluginWire.backendHeartbeat(server, presenceIncarnationId, presenceStartedAt,
					nextPresenceTimestamp()));

			if (presenceHeartbeatTask != null) {
				presenceHeartbeatTask.cancel(false);
			}
			presenceHeartbeatTask = plugin.getTimer().scheduleAtFixedRate(new Runnable() {
				@Override
				public void run() {
					sendPresenceHeartbeat();
				}
			}, PRESENCE_HEARTBEAT_SECONDS, PRESENCE_HEARTBEAT_SECONDS, TimeUnit.SECONDS);
		}
		seedOnlinePlayerPresence();
	}

	private void seedOnlinePlayerPresence() {
		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {
			@Override
			public void run() {
				if (!plugin.isEnabled()) {
					return;
				}
				synchronized (presenceLifecycleLock) {
					reannouncePresenceStarted();
				}
				for (Player player : Bukkit.getOnlinePlayers()) {
					synchronized (presenceLifecycleLock) {
						BackendPlayerPresenceSession session = getOrCreatePresenceSession(player);
						String server = presenceServer;
						if (session != null && presenceReporting && server != null) {
							long eventTimestamp = nextPresenceTimestamp();
							JsonEnvelope login = VotingPluginWire.login(session.playerName, session.uuid, server,
									session.connectionId, presenceIncarnationId, presenceStartedAt,
									eventTimestamp);
							sendActivePresenceMessage(server, presenceIncarnationId, presenceStartedAt, login);
						}
					}
				}
			}
		});
	}

	private void stopPresenceReporting() {
		synchronized (presenceLifecycleLock) {
			String server = presenceServer;
			UUID backendIncarnationId = presenceIncarnationId;
			long backendStartedAt = presenceStartedAt;
			boolean wasReporting = presenceReporting;
			presenceReporting = false;
			presenceServer = null;
			if (presenceHeartbeatTask != null) {
				presenceHeartbeatTask.cancel(false);
				presenceHeartbeatTask = null;
			}
			if (wasReporting && globalMessageHandler != null && server != null && backendIncarnationId != null) {
				sendPresenceMessage(VotingPluginWire.backendStopped(server, backendIncarnationId, backendStartedAt,
						nextPresenceTimestamp()));
			}
			presenceIncarnationId = null;
			lastPresenceSnapshotRequestId = null;
			lastPresenceSnapshotRequestAtNanos = 0L;
			playerPresenceSessions.clear();
		}
	}

	private void sendPresenceHeartbeat() {
		synchronized (presenceLifecycleLock) {
			if (presenceReporting && presenceServer != null && presenceIncarnationId != null) {
				reannouncePresenceStarted();
				sendPresenceMessage(VotingPluginWire.backendHeartbeat(presenceServer, presenceIncarnationId,
						presenceStartedAt, nextPresenceTimestamp()));
			}
		}
	}

	private boolean isActivePresenceGeneration(String server, UUID backendIncarnationId, long backendStartedAt) {
		synchronized (presenceLifecycleLock) {
			return presenceReporting && presenceServer != null && presenceServer.equalsIgnoreCase(server)
					&& presenceIncarnationId != null && presenceIncarnationId.equals(backendIncarnationId)
					&& presenceStartedAt == backendStartedAt;
		}
	}

	private void sendActivePresenceMessage(String server, UUID backendIncarnationId, long backendStartedAt,
			JsonEnvelope envelope) {
		synchronized (presenceLifecycleLock) {
			if (presenceReporting && presenceServer != null && presenceServer.equalsIgnoreCase(server)
					&& presenceIncarnationId != null && presenceIncarnationId.equals(backendIncarnationId)
					&& presenceStartedAt == backendStartedAt) {
				sendPresenceMessage(envelope);
			}
		}
	}

	private long nextPresenceTimestamp() {
		long now = System.currentTimeMillis();
		presenceLastTimestamp = Math.max(now, presenceLastTimestamp + 1L);
		return presenceLastTimestamp;
	}

	private void reannouncePresenceStarted() {
		if (!presenceReporting || presenceServer == null || presenceIncarnationId == null) {
			return;
		}
		sendPresenceMessage(VotingPluginWire.backendStarted(presenceServer, presenceIncarnationId,
				presenceStartedAt, presenceStartedAt));
	}

	/**
	 * Restarts presence reporting when the configured backend identity changes.
	 */
	public void reloadPresenceReporting() {
		String configuredServer = plugin.getBungeeSettings().getServer();
		synchronized (presenceLifecycleLock) {
			if (presenceReporting && presenceServer != null
					&& presenceServer.equalsIgnoreCase(configuredServer)) {
				return;
			}
		}
		stopPresenceReporting();
		startPresenceReporting();
	}

	/**
	 * Stops presence reporting without closing the existing global-message handler.
	 */
	public void disablePresenceReporting() {
		stopPresenceReporting();
	}

	private void sendPresenceMessage(JsonEnvelope envelope) {
		if (globalMessageHandler == null) {
			return;
		}
		try {
			globalMessageHandler.sendMessage(envelope);
		} catch (RuntimeException e) {
			plugin.debug("Unable to send backend presence message " + envelope.getSubChannel());
			plugin.debug(e);
		}
	}

	private static String playerKey(String playerName) {
		return nvl(playerName).trim().toLowerCase(Locale.ROOT);
	}

	private static final class BackendPlayerPresenceSession {
		private final String playerName;
		private final String uuid;
		private final UUID connectionId;

		private BackendPlayerPresenceSession(String playerName, String uuid, UUID connectionId) {
			this.playerName = playerName;
			this.uuid = uuid;
			this.connectionId = connectionId;
		}
	}

	private static long readLongSafe(String v, long def) {
		if (v == null)
			return def;
		try {
			return Long.parseLong(v);
		} catch (Exception ignored) {
			return def;
		}
	}

	private void handleWireVoteDelayRejected(JsonEnvelope msg) {
		if (msg.getSchema() != VotingPluginWire.SCHEMA_VERSION) {
			plugin.getLogger().warning("Incompatible version with bungee/proxy, please update all servers: "
					+ msg.getSchema() + " != " + VotingPluginWire.SCHEMA_VERSION);
			return;
		}

		if (!plugin.getOptions().isProcessRewards()) {
			return;
		}

		VotingPluginWire.VoteDelayRejected rejected = VotingPluginWire.readVoteDelayRejected(msg);
		if (rejected.uuid.isEmpty() || rejected.service.isEmpty()) {
			return;
		}

		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(rejected.uuid);
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in VoteDelayRejected: " + rejected.uuid);
			return;
		}

		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, rejected.service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '" + rejected.service + "'");
			return;
		}

		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, rejected.player);
		user.cache();
		user.updateName(true);
		voteSite.giveWaitUntilVoteDelayRewards(user, rejected.wasOnline && user.isOnline(), true);
	}

	/**
	 * Wire vote handler (Vote + VoteOnline).
	 */
	private void handleWireVote(JsonEnvelope msg) {
		// Strict schema check (wire uses envelope schema, not a "bungeeVersion" field)
		int schema = msg.getSchema();
		if (schema != VotingPluginWire.SCHEMA_VERSION) {
			plugin.getLogger().warning("Incompatible version with bungee/proxy, please update all servers: " + schema
					+ " != " + VotingPluginWire.SCHEMA_VERSION);
			return;
		}

		VotingPluginWire.Vote v = VotingPluginWire.readVote(msg);

		String uuidStr = v.uuid;
		String player = v.player;
		String service = v.service;

		if (uuidStr == null || uuidStr.isEmpty()) {
			return;
		}
		if (!ServiceSiteValidator.isValid(service)) {
			plugin.getLogger().warning("Rejected proxy vote with invalid service site '"
					+ ServiceSiteValidator.sanitizeForLog(service) + "'");
			return;
		}

		plugin.debug("wire vote received from " + player + "/" + uuidStr + " on " + service);

		VoteTotalsSnapshot text = VoteTotalsSnapshot.parseStorage(v.totals == null ? "" : v.totals);
		@SuppressWarnings("deprecation")
		UUID voteId = v.voteId != null ? v.voteId : text.getVoteUUID();

		if (!reserveWireVote(voteId)) {
			plugin.debug("Ignoring duplicate wire vote " + voteId + " for " + player + " on " + service);
			return;
		}

		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuidStr),
				player);

		bungeeVotePartyCurrent = text.getVotePartyCurrent();
		bungeeVotePartyRequired = text.getVotePartyRequired();
		plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
		plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

		user.cache();

		boolean setTotalsOnBackend = !v.manageTotals;

		user.bungeeVotePluginMessaging(service, v.time, text, setTotalsOnBackend, v.wasOnline, v.broadcast, v.num);

		if (plugin.getBungeeSettings().isPerServerPoints()) {
			user.addPoints(plugin.getConfigFile().getPointsOnVote());
		}

		if (service != null && !service.isEmpty()) {
			plugin.getServerData().addServiceSite(service);
		}

		@SuppressWarnings("unused")
		int _ignored = v.numberOfVotes;
	}

	/**
	 * Reserves a wire vote for processing.
	 *
	 * @param voteId unique vote identifier
	 * @return true if the vote has not been processed recently
	 */
	private boolean reserveWireVote(UUID voteId) {
		if (voteId == null) {
			return true;
		}

		long now = System.currentTimeMillis();
		long expiresAt = now + PROCESSED_VOTE_TTL_MILLIS;

		while (true) {
			Long currentExpiry = processedWireVotes.get(voteId);
			if (currentExpiry == null) {
				if (processedWireVotes.putIfAbsent(voteId, expiresAt) == null) {
					cleanupProcessedWireVotes(now);
					return true;
				}
				continue;
			}

			if (currentExpiry > now) {
				return false;
			}

			if (processedWireVotes.replace(voteId, currentExpiry, expiresAt)) {
				cleanupProcessedWireVotes(now);
				return true;
			}
		}
	}

	/**
	 * Removes expired wire vote identifiers.
	 *
	 * @param now current timestamp
	 */
	private void cleanupProcessedWireVotes(long now) {
		processedWireVotes.entrySet().removeIf(entry -> entry.getValue() <= now);
	}

	/**
	 * Loads the global MySQL handler for cross-server data synchronization.
	 */
	public void loadGlobalMysql() {
		if (plugin.getBungeeSettings().isGloblalDataEnabled()) {
			if (timer != null) {
				timer.shutdown();
				try {
					timer.awaitTermination(5, TimeUnit.SECONDS);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				timer.shutdownNow();
			}
			timer = Executors.newScheduledThreadPool(1);
			timer.scheduleWithFixedDelay(new Runnable() {
				@Override
				public void run() {
					checkGlobalData();
				}
			}, 60, 10, TimeUnit.SECONDS);
			timer.scheduleWithFixedDelay(new Runnable() {
				@Override
				public void run() {
					globalDataHandler.setString(plugin.getBungeeSettings().getServer(), "LastOnline",
							"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli());
				}
			}, 1, 60, TimeUnit.MINUTES);

			if (globalDataHandler != null) {
				globalDataHandler.getGlobalMysql().close();
			}

			if (plugin.getBungeeSettings().isGloblalDataUseMainMySQL()
					&& plugin.getStorageType().equals(UserStorage.MYSQL)) {
				globalDataHandler = new GlobalDataHandler(
						new GlobalMySQL("VotingPlugin_GlobalData", plugin.getMysql().getMysql()) {
							@Override
							public void debugEx(Exception e) {
								plugin.debug(e);
							}

							@Override
							public void debugLog(String text) {
								plugin.debug(text);
							}

							@Override
							public void info(String text) {
								plugin.getLogger().info(text);
							}

							@Override
							public void logSevere(String text) {
								plugin.getLogger().severe(text);
							}

							@Override
							public void warning(String text) {
								plugin.getLogger().warning(text);
							}
						});
			} else {
				globalDataHandler = new GlobalDataHandler(
						new GlobalMySQL("VotingPlugin_GlobalData", new MysqlConfigSpigot(
								plugin.getBungeeSettings().getData().getConfigurationSection("GlobalData"))) {
							@Override
							public void debugEx(Exception e) {
								plugin.debug(e);
							}

							@Override
							public void debugLog(String text) {
								plugin.debug(text);
							}

							@Override
							public void info(String text) {
								plugin.getLogger().info(text);
							}

							@Override
							public void logSevere(String text) {
								plugin.getLogger().severe(text);
							}

							@Override
							public void warning(String text) {
								plugin.getLogger().warning(text);
							}
						});
			}

			globalDataHandler.getGlobalMysql().alterColumnType("IgnoreTime", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("MONTH", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("WEEK", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("DAY", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("FinishedProcessing", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("Processing", "VARCHAR(5)");
			globalDataHandler.getGlobalMysql().alterColumnType("LastUpdated", "MEDIUMTEXT");
			globalDataHandler.getGlobalMysql().alterColumnType("ForceUpdate", "VARCHAR(5)");
			plugin.getTimeChecker().setProcessingEnabled(false);
		}
	}

	private void sendEnvelopeSocket(JsonEnvelope envelope) {
		if (clientHandler != null) {
			clientHandler.sendEnvelope(envelope);
		}
	}

	private void sendSubChannel(String subChannel, HashMap<String, Object> fields) {
		JsonEnvelope.Builder b = JsonEnvelope.builder(subChannel).schema(VotingPluginWire.SCHEMA_VERSION);
		if (fields != null) {
			for (Map.Entry<String, Object> e : fields.entrySet()) {
				b.put(e.getKey(), e.getValue());
			}
		}
		globalMessageHandler.sendMessage(b.build());
	}

	private static String nvl(String s) {
		return s == null ? "" : s;
	}
}
