// File: com/bencodez/votingplugin/BungeeHandler.java
package com.bencodez.votingplugin;

import java.io.File;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
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
import com.bencodez.simpleapi.servercomm.mysql.BackendMessenger;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.votingplugin.proxy.BungeeMessageData;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;

public class BungeeHandler implements Listener {
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
	private BackendMessenger backendMysqlMessenger;

	@Getter
	private MqttHandler mqttHandler;

	public BungeeHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

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

	public boolean checkGlobalDataTimeValue(DataValue data) {
		if (data.isBoolean()) {
			return data.getBoolean();
		}
		return Boolean.valueOf(data.getString());
	}

	public void close() {
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
					redisHandler.publishEnvelope(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin_"
							+ plugin.getBungeeSettings().getServer(), envelope);
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

		globalMessageHandler.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_UPDATE) {
			@Override
			public void onReceive(JsonEnvelope msg) {
				// Wire decode
				VotingPluginWire.VoteUpdate v = VotingPluginWire.readVoteUpdate(msg);

				String playerUuid = v.uuid;
				if (playerUuid == null || playerUuid.isEmpty()) {
					return;
				}

				plugin.debug("pluginmessaging voteupdate received for " + playerUuid);
				VotingPluginUser user = plugin.getVotingPluginUserManager()
						.getVotingPluginUser(UUID.fromString(playerUuid));
				user.cache();

				user.offVote();

				// Vote party cache update
				if (v.votePartyCurrent != 0 || bungeeVotePartyCurrent == -2) {
					bungeeVotePartyCurrent = v.votePartyCurrent;
				}
				if (v.votePartyRequired != 0 || bungeeVotePartyRequired == -2) {
					bungeeVotePartyRequired = v.votePartyRequired;
				}
				plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
				plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

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

		// Keeping these as-is (not vote decode related)
		globalMessageHandler.addListener(new GlobalMessageListener("VoteBroadcast") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				Map<String, String> f = msg.getFields();

				String uuidStr = nvl(f.get("uuid"));
				String playerName = nvl(f.get("player"));
				String service = nvl(f.get("service"));
				if (uuidStr.isEmpty() || service.isEmpty()) {
					return;
				}

				VotingPluginUser user = plugin.getVotingPluginUserManager()
						.getVotingPluginUser(UUID.fromString(uuidStr), playerName);
				VoteSite site = plugin.getVoteSiteManager().getVoteSite(service, true);
				if (site != null) {
					plugin.getBroadcastHandler().broadcastVote(user.getJavaUUID(), user.getPlayerName(),
							site.getDisplayName(), false);
				} else {
					plugin.getLogger().warning("No votesite for " + service);
				}
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VoteBroadcastOffline") {
			@Override
			public void onReceive(JsonEnvelope msg) {
				// kept (no-op) for future use
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
				backendMysqlMessenger = new BackendMessenger("VotingPlugin",
						plugin.getMysql().getMysql().getConnectionManager().getDataSource(),
						plugin.getOptions().getServer(), msg -> {
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

		plugin.debug("wire vote received from " + player + "/" + uuidStr + " on " + service);

		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuidStr),
				player);

		// Totals payload is still the compact BungeeMessageData string (that’s fine)
		BungeeMessageData text = new BungeeMessageData(v.totals == null ? "" : v.totals);

		bungeeVotePartyCurrent = text.getVotePartyCurrent();
		bungeeVotePartyRequired = text.getVotePartyRequired();
		plugin.getPlaceholders().onBungeeVotePartyUpdate();
		plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
		plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

		user.cache();

		// Wire meaning:
		// - v.broadcast == "already broadcasted / bungeeBroadcast" signal from proxy
		// side
		boolean broadcast = true;
		boolean bungeeBroadcast = v.broadcast;

		if (!bungeeBroadcast) {
			if (!plugin.getBungeeSettings().isBungeeBroadcast() && !plugin.getBungeeSettings().isDisableBroadcast()) {
				if (v.wasOnline || plugin.getBungeeSettings().isBungeeBroadcastAlways()) {
					VoteSite site = plugin.getVoteSiteManager().getVoteSite(service, true);
					if (site != null) {
						plugin.getBroadcastHandler().broadcastVote(user.getJavaUUID(), user.getPlayerName(),
								site.getDisplayName(), false);
						broadcast = false;
					} else {
						plugin.getLogger().warning("No votesite for " + service);
					}
				}
			}
		} else {
			broadcast = false;
		}

		// Wire has no "setTotals" flag; treat as always true (so doNotSetTotals =
		// false)
		boolean doNotSetTotals = false;

		user.bungeeVotePluginMessaging(service, v.time, text, doNotSetTotals, v.wasOnline, broadcast, v.num);

		if (plugin.getBungeeSettings().isPerServerPoints()) {
			user.addPoints(plugin.getConfigFile().getPointsOnVote());
		}

		if (v.manageTotals) {
			plugin.getServerData().addServiceSite(service);
		}

		@SuppressWarnings("unused")
		int _ignored = v.numberOfVotes;
	}

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
