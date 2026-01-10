package com.bencodez.votingplugin;

import java.io.File;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

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
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler.MessageHandler;
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
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.proxy.BungeeMessageData;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.BungeeVersion;
import com.bencodez.votingplugin.user.VotingPluginUser;

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

	public BungeeHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void checkGlobalData() {
		HashMap<String, DataValue> data = globalDataHandler.getExact(plugin.getBungeeSettings().getServer());
		// plugin.debug(data.toString());

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

	/*
	 * @EventHandler public void onDateChange(DateChangedEvent event) { if
	 * (method.equals(BungeeMethod.PLUGINMESSAGING)) {
	 * plugin.getPluginMessaging().sendPluginMessage("timeupdate",
	 * plugin.getServerDataFile().getPrevMonth() + "//" +
	 * plugin.getServerDataFile().getPrevDay() + "//" +
	 * plugin.getServerDataFile().getPrevWeekDay()); } }
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
				getGlobalMessageHandler().sendMessage("TimeChangeFinished",
						"" + plugin.getBungeeSettings().getServer());
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
			public void sendMessage(String subChannel, String... messageData) {
				if (method.equals(BungeeMethod.MYSQL)) {
					// plugin.getPluginMessaging().sendPluginMessage(subChannel, messageData);

					try {
						backendMysqlMessenger.sendToProxy(subChannel + "%l%" + String.join("%l%", messageData));
					} catch (SQLException e) {
						e.printStackTrace();
					}

				} else if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
					plugin.getPluginMessaging().sendPluginMessage(subChannel, messageData);
				} else if (method.equals(BungeeMethod.SOCKETS)) {
					ArrayList<String> list = new ArrayList<>();
					list.add(subChannel);
					list.addAll(ArrayUtils.convert(messageData));
					sendData(ArrayUtils.convert(list));
				} else if (method.equals(BungeeMethod.REDIS)) {
					ArrayList<String> list = new ArrayList<>();
					list.add(subChannel);
					list.addAll(ArrayUtils.convert(messageData));
					redisHandler.sendMessage(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin",
							ArrayUtils.convert(list));
				} else if (method.equals(BungeeMethod.MQTT)) {
					ArrayList<String> list = new ArrayList<>();
					list.add(subChannel);
					list.addAll(ArrayUtils.convert(messageData));
					try {
						mqttHandler.publish(plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/proxy",
								String.join(":", list));
					} catch (Exception e) {
						e.printStackTrace();
					}

				}
			}
		};

		globalMessageHandler.addListener(new GlobalMessageListener("Vote") {

			@Override
			public void onReceive(ArrayList<String> args) {
				if (args.size() > 8) {
					int bungeeVersion = Integer.parseInt(args.get(8));
					if (bungeeVersion != BungeeVersion.getPluginMessageVersion()) {
						plugin.getLogger().warning("Incompatible version with bungee, please update all servers"
								+ bungeeVersion + ":" + BungeeVersion.getPluginMessageVersion());
						return;
					}

					String player = args.get(0);
					String uuid = args.get(1);
					String service = args.get(2);
					long time = Long.parseLong(args.get(3));
					plugin.debug("pluginmessaging vote received from " + player + "/" + uuid + " on " + service);
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid), player);

					boolean wasOnline = Boolean.valueOf(args.get(4));

					BungeeMessageData text = new BungeeMessageData(args.get(6));

					bungeeVotePartyCurrent = text.getVotePartyCurrent();
					bungeeVotePartyRequired = text.getVotePartyRequired();
					plugin.getPlaceholders().onBungeeVotePartyUpdate();
					plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
					plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

					boolean setTotals = Boolean.valueOf(args.get(7));

					user.cache();

					boolean broadcast = true;
					boolean bungeeBroadcast = false;

					if (args.size() > 9) {
						bungeeBroadcast = Boolean.valueOf(args.get(9));
					}

					int num = 1;
					if (args.size() > 10) {
						num = Integer.valueOf(args.get(10));
					}
					int numberOfVotes = 1;
					if (args.size() > 11) {
						numberOfVotes = Integer.valueOf(args.get(11));
					}

					if (!bungeeBroadcast) {
						if (!plugin.getBungeeSettings().isBungeeBroadcast()
								&& !plugin.getBungeeSettings().isDisableBroadcast()) {
							if (wasOnline || plugin.getBungeeSettings().isBungeeBroadcastAlways()) {
								if (plugin.getConfigFile().isFormatOnlyOneOfflineBroadcast() && !wasOnline) {
									if (num == 1) {
										user.offlineBroadcast(user, plugin.getBungeeSettings().isUseBungeecoord(),
												numberOfVotes);
									}
								} else {
									VoteSite site = plugin.getVoteSite(service, true);
									if (site != null) {
										site.broadcastVote(user, false);
										broadcast = false;
									} else {
										plugin.getLogger().warning("No votesite for " + service);
									}
								}

							}
						}
					} else {
						broadcast = false;
					}

					user.bungeeVotePluginMessaging(service, time, text, !setTotals, wasOnline, broadcast, num);
					if (plugin.getBungeeSettings().isPerServerPoints()) {
						user.addPoints(plugin.getConfigFile().getPointsOnVote());
					}

					if (Boolean.valueOf(args.get(5))) {
						plugin.getServerData().addServiceSite(service);
					}
				} else {
					plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
				}
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VoteOnline") {

			@Override
			public void onReceive(ArrayList<String> args) {
				if (args.size() > 8) {
					int bungeeVersion = Integer.parseInt(args.get(8));
					if (bungeeVersion != BungeeVersion.getPluginMessageVersion()) {
						plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
						return;
					}
					String player = args.get(0);
					String uuid = args.get(1);
					String service = args.get(2);
					long time = Long.parseLong(args.get(3));
					BungeeMessageData text = new BungeeMessageData(args.get(6));

					bungeeVotePartyCurrent = text.getVotePartyCurrent();
					bungeeVotePartyRequired = text.getVotePartyRequired();
					plugin.getPlaceholders().onBungeeVotePartyUpdate();
					plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
					plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);

					plugin.debug("pluginmessaging voteonline received from " + player + "/" + uuid + " on " + service);
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid), player);
					user.cache();

					boolean setTotals = Boolean.valueOf(args.get(7));

					boolean wasOnline = Boolean.valueOf(args.get(4));

					boolean broadcast = true;
					boolean bungeeBroadcast = false;

					if (args.size() > 9) {
						bungeeBroadcast = Boolean.valueOf(args.get(9));
					}

					int num = 1;
					if (args.size() > 10) {
						num = Integer.valueOf(args.get(10));
					}

					int numberOfVotes = 1;
					if (args.size() > 11) {
						numberOfVotes = Integer.valueOf(args.get(11));
					}

					if (!bungeeBroadcast) {
						if (!plugin.getBungeeSettings().isBungeeBroadcast()
								&& !plugin.getBungeeSettings().isDisableBroadcast()) {
							if (wasOnline || plugin.getBungeeSettings().isBungeeBroadcastAlways()) {
								if (plugin.getConfigFile().isFormatOnlyOneOfflineBroadcast() && !wasOnline) {
									if (num == 1) {
										user.offlineBroadcast(user, plugin.getBungeeSettings().isUseBungeecoord(),
												numberOfVotes);
									}
								} else {
									VoteSite site = plugin.getVoteSite(service, true);
									if (site != null) {
										site.broadcastVote(user, false);
										broadcast = false;
									} else {
										plugin.getLogger().warning("No votesite for " + service);
									}
								}

							}
						}
					} else {
						broadcast = false;
					}

					user.bungeeVotePluginMessaging(service, time, text, !setTotals, wasOnline, broadcast, num);
					if (plugin.getBungeeSettings().isPerServerPoints()) {
						user.addPoints(plugin.getConfigFile().getPointsOnVote());
					}

					if (Boolean.valueOf(args.get(5))) {
						plugin.getServerData().addServiceSite(service);
					}
				} else {
					plugin.getLogger().warning("Incompatible version with bungee, please update all servers");
				}

			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VoteUpdate") {

			@Override
			public void onReceive(ArrayList<String> args) {
				String player = args.get(0);
				plugin.debug("pluginmessaging voteupdate received for " + player);
				VotingPluginUser user = plugin.getVotingPluginUserManager()
						.getVotingPluginUser(UUID.fromString(player));
				user.cache();

				user.offVote();

				if (args.size() > 2) {
					bungeeVotePartyCurrent = Integer.parseInt(args.get(1));
					bungeeVotePartyRequired = Integer.parseInt(args.get(2));
					plugin.getServerData().setBungeeVotePartyCurrent(bungeeVotePartyCurrent);
					plugin.getServerData().setBungeeVotePartyRequired(bungeeVotePartyRequired);
				}

				if (args.size() > 5) {
					String service = args.get(4);
					String data = args.get(5);
					if (MessageAPI.isLong(data)) {
						long time = Long.valueOf(data);
						if (time > 0) {
							user.setTime(plugin.getVoteSite(service, true), time);
						} else {
							if (plugin.getBungeeSettings().isBungeeDebug()) {
								plugin.debug("Invalid last vote time received from bungee: " + time);
							}
						}
					}
				}

				plugin.setUpdate(true);
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("BungeeTimeChange") {

			@Override
			public void onReceive(ArrayList<String> args) {
				checkGlobalData();
			}
		});

		globalMessageHandler.addListener(new GlobalMessageListener("VoteBroadcast") {

			@Override
			public void onReceive(ArrayList<String> args) {
				if (args.size() > 2) {
					String uuid = args.get(0);
					String service = args.get(2);
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid), args.get(1));
					VoteSite site = plugin.getVoteSite(service, true);
					if (site != null) {
						site.broadcastVote(user, false);
					} else {
						plugin.getLogger().warning("No votesite for " + service);
					}
				}
			}
		});
		globalMessageHandler.addListener(new GlobalMessageListener("VoteBroadcastOffline") {

			@Override
			public void onReceive(ArrayList<String> args) {
				if (args.size() > 2) {
					String uuid = args.get(0);
					String votes = args.get(2);
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid), args.get(1));
					user.offlineBroadcast(user, false, Integer.parseInt(votes));
				}
			}
		});
		globalMessageHandler.addListener(new GlobalMessageListener("Status") {

			@Override
			public void onReceive(ArrayList<String> args) {
				String server = args.get(0);
				sendMessage(globalMessageHandler, "statusokay", server);
			}
		});
		globalMessageHandler.addListener(new GlobalMessageListener("ServerName") {

			@Override
			public void onReceive(ArrayList<String> args) {
				String server = args.get(0);
				if (!plugin.getOptions().getServer().equals(server)) {
					plugin.getLogger().warning("Server name doesn't match in BungeeSettings.yml, should be " + server);
				}
			}
		});
		globalMessageHandler.addListener(new GlobalMessageListener("VotePartyBungee") {

			@Override
			public void onReceive(ArrayList<String> args) {
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
			public void onReceive(ArrayList<String> args) {
				String broadcast = args.get(0);
				MiscUtils.getInstance().broadcast(broadcast);
			}
		});

		if (method.equals(BungeeMethod.MYSQL)) {
			plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());

			try {
				backendMysqlMessenger = new BackendMessenger("VotingPlugin",
						plugin.getMysql().getMysql().getConnectionManager().getDataSource(),
						plugin.getOptions().getServer(), msg -> {
							plugin.debug("Proxy sent: " + msg.payload);

							String[] message = msg.payload.split(Pattern.quote("%l%"));
							if (message.length > 0) {
								ArrayList<String> list = new ArrayList<>();
								for (int i = 1; i < message.length; i++) {
									list.add(message[i]);
								}
								globalMessageHandler.onMessage(message[0], list);
							}

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

				@Override
				protected void onMessage(String channel, String[] message) {
					if (plugin.getBungeeSettings().isBungeeDebug()) {
						plugin.debug(channel + " " + ArrayUtils.makeStringList(ArrayUtils.convert(message)));
					}
					if (message.length > 0) {
						ArrayList<String> list = new ArrayList<>();
						for (int i = 1; i < message.length; i++) {
							list.add(message[i]);
						}
						globalMessageHandler.onMessage(message[0], list);
					}
				}
			};
			redisThread = new Thread(new Runnable() {

				@Override
				public void run() {
					if (plugin.isEnabled()) {
						redisHandler.loadListener(
								new RedisListener(redisHandler, plugin.getBungeeSettings().getRedisPrefix()
										+ "VotingPlugin_" + plugin.getBungeeSettings().getServer()));
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
				public void onRecieve(String subChannel, ArrayList<String> args) {
					globalMessageHandler.onMessage(subChannel, args);

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
				public void onReceive(String[] data) {
					globalMessageHandler.onMessage(data[0], ArrayUtils.convertAndRemoveFirst(data));
				}
			});

		} else if (method.equals(BungeeMethod.MQTT)) {
			try {
				String id = plugin.getBungeeSettings().getMqttClientID();
				if (id.isEmpty()) {
					id = plugin.getOptions().getServer();
				}
				mqttHandler = new MqttHandler(new MqttServerComm(plugin.getBungeeSettings().getMqttClientID(),
						plugin.getBungeeSettings().getMqttBrokerURL(), plugin.getBungeeSettings().getMqttUsername(),
						plugin.getBungeeSettings().getMqttPassword()), 2);
				mqttHandler.subscribe(plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/"
						+ plugin.getOptions().getServer(), new MessageHandler() {

							@Override
							public void onMessage(String topic, String payload) {
								String[] message = payload.split(":");

								if (message.length > 0) {
									globalMessageHandler.onMessage(message[0],
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

		bungeeVotePartyCurrent = plugin.getServerData().getBungeeVotePartyCurrent();
		bungeeVotePartyRequired = plugin.getServerData().getBungeeVotePartyRequired();

		if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
			plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
		}
	}

	@Getter
	private MqttHandler mqttHandler;

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

	public void sendData(String... strings) {
		clientHandler.sendMessage(strings);
	}
}
