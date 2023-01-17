package com.bencodez.votingplugin.bungee;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.CodeSource;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.api.misc.jsonparser.JsonParser;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.usercache.value.DataValue;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueBoolean;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueInt;
import com.bencodez.advancedcore.api.user.usercache.value.DataValueString;
import com.bencodez.advancedcore.api.user.userstorage.Column;
import com.bencodez.advancedcore.api.user.userstorage.mysql.api.config.MysqlConfigBungee;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.advancedcore.bungeeapi.mysql.BungeeMySQL;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.advancedcore.bungeeapi.time.BungeeTimeChecker;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import lombok.Getter;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.CommandSender;
import net.md_5.bungee.api.chat.TextComponent;
import net.md_5.bungee.api.config.ServerInfo;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.connection.Server;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.event.PostLoginEvent;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.event.EventHandler;

public class VotingPluginBungee extends Plugin implements Listener {

	private HashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new HashMap<String, ArrayList<OfflineBungeeVote>>();

	private HashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new HashMap<String, ArrayList<OfflineBungeeVote>>();

	private HashMap<String, ClientHandler> clientHandles;

	private HashMap<String, ClientHandler> redisClientHandles;

	@Getter
	private Config config;

	private EncryptionHandler encryptionHandler;

	@Getter
	private BungeeMethod method;

	@Getter
	private BungeeMySQL mysql;

	private NonVotedPlayersCache nonVotedPlayersCache;

	private SocketHandler socketHandler;

	private SocketHandler redisSocketHandler;

	private VoteCache voteCacheFile;

	private boolean votifierEnabled = true;

	private ConcurrentHashMap<UUID, String> uuidPlayerNameCache = new ConcurrentHashMap<UUID, String>();

	@Getter
	private BungeeTimeChecker bungeeTimeChecker;

	private boolean enabled;

	@Getter
	private GlobalDataHandlerProxy globalDataHandler;

	public synchronized void checkCachedVotes(String server) {
		if (getProxy().getServerInfo(server) != null) {
			if (!getProxy().getServerInfo(server).getPlayers().isEmpty()) {
				if (cachedVotes.containsKey(server) && !config.getBlockedServers().contains(server)) {
					ArrayList<OfflineBungeeVote> c = cachedVotes.get(server);
					ArrayList<OfflineBungeeVote> newSet = new ArrayList<OfflineBungeeVote>();
					if (!c.isEmpty()) {
						int num = 1;
						for (OfflineBungeeVote cache : c) {
							boolean toSend = true;
							if (getConfig().getWaitForUserOnline()) {
								ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(cache.getUuid()));
								if (isOnline(p)) {
									toSend = false;
								} else if (p != null && p.isConnected()
										&& !p.getServer().getInfo().getName().equals(server)) {
									toSend = false;
								}

							}
							if (toSend) {
								sendPluginMessageServer(server, "Vote", cache.getPlayerName(), cache.getUuid(),
										cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
										"" + cache.isRealVote(), cache.getText(),
										"" + getConfig().getBungeeManageTotals(),
										"" + BungeeVersion.getPluginMessageVersion(), "" + config.getBroadcast(),
										"" + num);
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

	public synchronized void checkOnlineVotes(ProxiedPlayer player, String uuid, String server) {
		if (isOnline(player) && cachedOnlineVotes.containsKey(uuid)) {
			ArrayList<OfflineBungeeVote> c = cachedOnlineVotes.get(uuid);
			if (!c.isEmpty()) {
				if (server == null) {
					server = getProxy().getPlayer(UUID.fromString(uuid)).getServer().getInfo().getName();
				}
				if (!config.getBlockedServers().contains(server)) {
					int num = 1;
					for (OfflineBungeeVote cache : c) {
						sendPluginMessageServer(server, "VoteOnline", cache.getPlayerName(), cache.getUuid(),
								cache.getService(), "" + cache.getTime(), Boolean.FALSE.toString(),
								"" + cache.isRealVote(), cache.getText(), "" + getConfig().getBungeeManageTotals(),
								"" + BungeeVersion.getPluginMessageVersion(), "" + config.getBroadcast(), "" + num);
						num++;
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

	public void debug2(String msg) {
		debug(msg);
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
			getProxy().getLogger().info("There is no player with the name \"" + playerName + "\"!");
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

	public String getProperName(String uuid, String currentName) {
		ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
		if (p != null && p.isConnected()) {
			return p.getName();
		}
		return currentName;
	}

	public String getUUID(String playerName) {
		if (config.getOnlineMode()) {

			ProxiedPlayer p = getProxy().getPlayer(playerName);
			if (p != null && p.isConnected()) {
				return p.getUniqueId().toString();
			}

			for (Entry<UUID, String> entry : uuidPlayerNameCache.entrySet()) {
				if (entry.getValue().equalsIgnoreCase(playerName)) {
					return entry.getKey().toString();
				}
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
		} else {
			return UUID.nameUUIDFromBytes(("OfflinePlayer:" + playerName).getBytes(StandardCharsets.UTF_8)).toString();
		}

	}

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

	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<VoteTimeQueue>();

	public void processQueue() {
		while (getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getTimeChangeQueue().remove();
			vote(vote.getName(), vote.getService(), true, false, vote.getTime(), null, null);
		}
	}

	private void loadMysql() {
		mysql = new BungeeMySQL(this, "VotingPlugin_Users", config.getData()) {

			@Override
			public void debug(SQLException e) {
				if (config.getDebug()) {
					e.printStackTrace();
				}
			}
		};

		ArrayList<String> servers = new ArrayList<String>();
		for (String s : getProxy().getServers().keySet()) {
			if (!config.getBlockedServers().contains(s)) {
				servers.add(s);
			}
		}

		if (config.getGlobalDataEnabled()) {
			if (config.getGlobalDataUseMainMySQL()) {
				globalDataHandler = new GlobalDataHandlerProxy(
						new GlobalMySQL("VotingPlugin_GlobalData", getMysql().getMysql()) {

							@Override
							public void warning(String text) {
								getLogger().warning(text);
							}

							@Override
							public void severe(String text) {
								getLogger().severe(text);
							}

							@Override
							public void debug(Exception e) {
								if (config.getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debug(String text) {
								debug2(text);
							}
						}, servers) {
					@Override
					public void onTimeChangedFinished(TimeType type) {
						getMysql().wipeColumnData(TopVoter.of(type).getColumnName());

						if (!config.getGlobalDataEnabled()) {
							return;
						}
						for (String s : getProxy().getServers().keySet()) {
							if (!config.getBlockedServers().contains(s)) {
								getGlobalDataHandler().setBoolean(s, "ForceUpdate", true);
								if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
									sendPluginMessageServer(s, "BungeeTimeChange", "");
								} else if (method.equals(BungeeMethod.SOCKETS)) {
									sendServerMessage(s, "BungeeTimeChange");
								}
							}
						}

						processQueue();

					}

					@Override
					public void onTimeChangedFailed(String server, TimeType type) {
						getGlobalDataHandler().setBoolean(server, type.toString(), false);
					}
				};
			} else {
				globalDataHandler = new GlobalDataHandlerProxy(new GlobalMySQL("VotingPlugin_GlobalData",
						new MysqlConfigBungee(config.getData().getSection("GlobalData"))) {

					@Override
					public void warning(String text) {
						getLogger().warning(text);
					}

					@Override
					public void severe(String text) {
						getLogger().severe(text);
					}

					@Override
					public void debug(Exception e) {
						if (config.getDebug()) {
							e.printStackTrace();
						}
					}

					@Override
					public void debug(String text) {
						debug2(text);
					}
				}, servers) {
					@Override
					public void onTimeChangedFinished(TimeType type) {
						getMysql().wipeColumnData(TopVoter.of(type).getColumnName());

						if (!config.getGlobalDataEnabled()) {
							return;
						}
						for (String s : getProxy().getServers().keySet()) {
							if (!config.getBlockedServers().contains(s)) {
								getGlobalDataHandler().setBoolean(s, "ForceUpdate", true);
								if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
									sendPluginMessageServer(s, "BungeeTimeChange", "");
								} else if (method.equals(BungeeMethod.SOCKETS)) {
									sendServerMessage(s, "BungeeTimeChange");
								}
							}
						}

						processQueue();

					}

					@Override
					public void onTimeChangedFailed(String server, TimeType type) {
						getGlobalDataHandler().setBoolean(server, type.toString(), false);
					}
				};
			}
			getGlobalDataHandler().getGlobalMysql().alterColumnType("IgnoreTime", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("MONTH", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("WEEK", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("DAY", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("FinishedProcessing", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("Processing", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("ForceUpdate", "VARCHAR(5)");
			getGlobalDataHandler().getGlobalMysql().alterColumnType("LastUpdated", "MEDIUMTEXT");
		}

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
		getMysql().alterColumnType("DayVoteStreakLastUpdate", "MEDIUMTEXT");
	}

	private String buildNumber = "NOTSET";

	public void loadVersionFile() {
		Configuration conf = getVersionFile();
		if (conf != null) {
			buildNumber = conf.get("buildnumber", "NOTSET");
		}
	}

	private Configuration getVersionFile() {
		try {
			CodeSource src = this.getClass().getProtectionDomain().getCodeSource();
			if (src != null) {
				URL jar = src.getLocation();
				ZipInputStream zip = null;
				zip = new ZipInputStream(jar.openStream());
				while (true) {
					ZipEntry e = zip.getNextEntry();
					if (e != null) {
						String name = e.getName();
						if (name.equals("votingpluginversion.yml")) {
							Reader defConfigStream = new InputStreamReader(zip);
							if (defConfigStream != null) {
								Configuration conf = ConfigurationProvider
										.getProvider(net.md_5.bungee.config.YamlConfiguration.class)
										.load(defConfigStream);

								defConfigStream.close();
								return conf;
							}
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void onDisable() {
		if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
			getLogger().info("VotingPlugin saving vote cache: " + cachedVotes.size() + "/" + cachedOnlineVotes.size());
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
		}
		if (!timeChangeQueue.isEmpty()) {
			int num = 0;
			for (VoteTimeQueue vote : timeChangeQueue) {
				voteCacheFile.addTimedVote(num, vote);
				num++;
			}
		}
		voteCacheFile.save();
		nonVotedPlayersCache.save();
		if (mysql != null) {
			mysql.shutdown();
		}
		getLogger().info("VotingPlugin disabled");
		enabled = false;
	}

	private VoteEventBungee voteEventBungee;

	@Override
	public void onEnable() {
		enabled = true;
		try {
			Class.forName("com.vexsoftware.votifier.bungee.events.VotifierEvent");
		} catch (ClassNotFoundException e) {
			votifierEnabled = false;
			getLogger().warning("Votifier event not found, not loading votifier event");
		}
		if (votifierEnabled) {
			try {
				voteEventBungee = new VoteEventBungee(this);
				getProxy().getPluginManager().registerListener(this, voteEventBungee);
				getProxy().getPluginManager().registerListener(this, this);
			} catch (Exception e) {
			}
		}

		config = new Config(this);
		config.load();

		getProxy().getPluginManager().registerCommand(this, new VotingPluginBungeeCommand(this));

		boolean mysqlLoaded = true;
		try {
			if (!config.getData().getString("Host", "").isEmpty()) {
				loadMysql();
			} else {
				mysqlLoaded = false;
				getLogger().severe("MySQL settings not set in bungeeconfig.yml");
			}
		} catch (Exception e) {
			mysqlLoaded = false;
			e.printStackTrace();
		}

		method = BungeeMethod.getByName(config.getBungeeMethod());
		if (method == null) {
			method = BungeeMethod.PLUGINMESSAGING;
		}

		bungeeTimeChecker = new BungeeTimeChecker(config.getData().getInt("TimeHourOffSet")) {

			@Override
			public void warning(String text) {
				getLogger().warning(text);
			}

			@Override
			public void timeChanged(TimeType type, boolean fake, boolean pre, boolean post) {
				if (!config.getGlobalDataEnabled()) {
					return;
				}
				for (String s : getProxy().getServers().keySet()) {
					if (!config.getBlockedServers().contains(s)) {
						if (getGlobalDataHandler().getGlobalMysql().containsKey(s)) {
							String lastOnlineStr = getGlobalDataHandler().getString(s, "LastOnline");
							long lastOnline = 0;
							try {
								lastOnline = Long.valueOf(lastOnlineStr);
							} catch (NumberFormatException e) {
								e.printStackTrace();
							}

							if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli()
									- lastOnline < 1000 * 60 * 60 * 12) {
								// server has been online within the 12 hours
								HashMap<String, DataValue> dataToSet = new HashMap<String, DataValue>();
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
								getLogger().warning("Server " + s + " hasn't been online recently");
							}
						} else {
							getLogger().warning("Server " + s + " global data handler disabled?");
						}

					}
				}
				globalDataHandler.onTimeChange(type);
			}

			@Override
			public void setPrevWeek(int week) {
				voteCacheFile.getData().set("Time.Week", week);
				voteCacheFile.save();
			}

			@Override
			public void setPrevMonth(String text) {
				voteCacheFile.getData().set("Time.Month", text);
				voteCacheFile.save();
			}

			@Override
			public void setPrevDay(int day) {
				voteCacheFile.getData().set("Time.Day", day);
				voteCacheFile.save();
			}

			@Override
			public void setLastUpdated() {
				voteCacheFile.getData().set("Time.LastUpdated", System.currentTimeMillis());
				voteCacheFile.save();
			}

			@Override
			public void setIgnoreTime(boolean ignore) {
				voteCacheFile.getData().set("Time.IgnoreTime", ignore);
				voteCacheFile.save();
			}

			@Override
			public boolean isIgnoreTime() {
				return voteCacheFile.getData().getBoolean("Time.IgnoreTime");
			}

			@Override
			public boolean isEnabled() {
				return enabled;
			}

			@Override
			public void info(String text) {
				getLogger().info(text);
			}

			@Override
			public int getPrevWeek() {
				return voteCacheFile.getData().getInt("Time.Week");
			}

			@Override
			public String getPrevMonth() {
				return voteCacheFile.getData().getString("Time.Month");
			}

			@Override
			public int getPrevDay() {
				return voteCacheFile.getData().getInt("Time.Day");
			}

			@Override
			public long getLastUpdated() {
				return voteCacheFile.getData().getLong("Time.LastUpdated");
			}

			@Override
			public void debug(String text) {
				debug2(text);
			}
		};

		this.getProxy().registerChannel("vp:vp");

		if (mysqlLoaded) {

			uuidPlayerNameCache = mysql.getRowsUUIDNameQuery();

			voteCacheFile = new VoteCache(this);
			voteCacheFile.load();

			bungeeTimeChecker.loadTimer();

			nonVotedPlayersCache = new NonVotedPlayersCache(this);
			nonVotedPlayersCache.load();

			try {
				for (String key : voteCacheFile.getTimedVoteCache()) {
					Configuration data = voteCacheFile.getTimedVoteCache(key);
					timeChangeQueue.add(
							new VoteTimeQueue(data.getString("Name"), data.getString("Service"), data.getLong("Time")));
				}

				processQueue();
			} catch (Exception e) {
				e.printStackTrace();
			}

			if (method.equals(BungeeMethod.PLUGINMESSAGING)) {

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
						for (String num : voteCacheFile.getOnlineVotes(player)) {
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
						config.getBungeePort(), encryptionHandler, config.getDebug()) {

					@Override
					public void log(String str) {
						getLogger().info(str);
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
			currentVotePartyVotesRequired = getConfig().getVotePartyVotesRequired()
					+ voteCacheFile.getVotePartyInreaseVotesRequired();
			votePartyVotes = voteCacheFile.getVotePartyCurrentVotes();
		}

		if (getConfig().getRedisSupport()) {
			if (encryptionHandler == null) {
				encryptionHandler = new EncryptionHandler(new File(getDataFolder(), "secretkey.key"));
			}

			redisSocketHandler = new SocketHandler(getDescription().getVersion(), config.getRedisSocketHostHost(),
					config.getRedisSocketHostPort(), encryptionHandler, config.getDebug()) {

				@Override
				public void log(String str) {
					getLogger().info(str);
				}
			};

			redisSocketHandler.add(new SocketReceiver() {

				@Override
				public void onReceive(String[] data) {
					if (data.length > 8) {
						if (data[0].equalsIgnoreCase("Vote")) {
							// todo
							// sendRedisServerMessage("Vote", uuid, player, service, "" + votePartyVotes,""
							// + currentVotePartyVotesRequired, "" + time, "" + realVote, text.toString());
							vote(data[2], data[3], Boolean.valueOf(data[7]), true, 0, new BungeeMessageData(data[8]),
									data[1]);
						}
					}

				}
			});

			clientHandles = new HashMap<String, ClientHandler>();
			for (

			String s : config.getRedisServers()) {
				Configuration d = config.getRedisServersConfiguration(s);
				redisClientHandles.put(s, new ClientHandler(d.getString("Host", ""), d.getInt("Port", 1234),
						encryptionHandler, config.getDebug()));
			}
		}

		BStatsMetricsBungee metrics = new BStatsMetricsBungee(this, 9453);

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("bungee_method", () -> getConfig().getBungeeMethod().toString()));

		metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("sendtoallservers",
				() -> "" + getConfig().getSendVotesToAllServers()));

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("allowunjoined", () -> "" + getConfig().getAllowUnJoined()));

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("pointsonvote", () -> "" + getConfig().getPointsOnVote()));

		metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("bungeemanagetotals",
				() -> "" + getConfig().getBungeeManageTotals()));

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("waitforuseronline", () -> "" + getConfig().getWaitForUserOnline()));

		metrics.addCustomChart(
				new BStatsMetricsBungee.SimplePie("globaldata_enabled", () -> "" + getConfig().getGlobalDataEnabled()));
		if (getConfig().getGlobalDataEnabled()) {
			metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("globaldata_usemainmysql",
					() -> "" + getConfig().getGlobalDataUseMainMySQL()));
		}

		if (!buildNumber.equals("NOTSET")) {
			metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("dev_build_number", () -> "" + buildNumber));
		}

		loadVersionFile();

		getLogger().info("VotingPlugin loaded, using method: " + method.toString() + ", PluginMessagingVersion: "
				+ BungeeVersion.getPluginMessageVersion());
		if (!buildNumber.equals("NOTSET")) {
			getLogger().info("Detected using dev build number: " + buildNumber);
		}
	}

	@EventHandler
	public void onLogin(PostLoginEvent event) {
		getProxy().getScheduler().runAsync(this, new Runnable() {

			@Override
			public void run() {
				if (nonVotedPlayersCache != null) {
					nonVotedPlayersCache.addPlayer(event.getPlayer());
				}
			}
		});

	}

	public boolean isOnline(ProxiedPlayer p) {
		if (p != null) {
			return p.isConnected();
		}

		return false;
	}

	public void login(ProxiedPlayer p) {
		if (p.getServer() != null && p.getServer().getInfo() != null) {
			final String server = p.getServer().getInfo().getName();
			final ProxiedPlayer proixedPlayer = p;
			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {
					if (isOnline(p)) {
						checkCachedVotes(server);
						checkOnlineVotes(proixedPlayer, proixedPlayer.getUniqueId().toString(), server);
					}
				}
			}, 1, TimeUnit.SECONDS);
		}

	}

	@EventHandler
	public void onPluginMessage(PluginMessageEvent ev) {
		if (!ev.getTag().equals("vp:vp".toLowerCase())) {
			return;
		}

		ev.setCancelled(true);

		if (!(ev.getSender() instanceof Server)) {
			debug("Ignore plugin message");
			return;
		}

		ByteArrayInputStream instream = new ByteArrayInputStream(ev.getData());
		DataInputStream in = new DataInputStream(instream);
		try {
			ByteArrayOutputStream outstream = new ByteArrayOutputStream();
			DataOutputStream out = new DataOutputStream(outstream);
			String subchannel = in.readUTF();
			int size = in.readInt();

			debug("Received plugin message, processing...");

			// check for status message returns
			if (subchannel.equalsIgnoreCase("statusokay")) {
				String server = in.readUTF();
				getLogger().info("Status okay for " + server);
				return;
			} else if (subchannel.equalsIgnoreCase("login")) {
				String player = in.readUTF();
				String uuid = in.readUTF();
				String server = "";
				if (size > 2) {
					server = in.readUTF();
				}
				debug("Login: " + player + "/" + uuid + " " + server);
				ProxiedPlayer p = getProxy().getPlayer(player);
				login(p);
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

	public void reload(boolean loadMysql) {
		config.load();
		currentVotePartyVotesRequired = getConfig().getVotePartyVotesRequired()
				+ voteCacheFile.getVotePartyInreaseVotesRequired();
		if (loadMysql) {
			try {
				if (!config.getData().getString("Host", "").isEmpty()) {
					loadMysql();
				} else {
					getLogger().severe("MySQL settings not set in bungeeconfig.yml");
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
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

	public void sendRedisServerMessage(String... messageData) {
		for (ClientHandler h : redisClientHandles.values()) {
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
			sendServerMessage("bungeevote", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
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

			sendServerMessageServer(server, "bungeevoteonline", uuid, name, service, text.toString(),
					"" + getConfig().getBungeeManageTotals());
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

	public synchronized void vote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			BungeeMessageData text, String uuid) {
		try {
			if (player == null || player.isEmpty()) {
				getLogger().info("No name from vote on " + service);
				return;
			}

			if (timeQueue) {
				if (getConfig().getGlobalDataEnabled()) {
					if (getGlobalDataHandler().isTimeChangedHappened()) {
						timeChangeQueue.add(new VoteTimeQueue(player, service,
								LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
						getLogger().info("Cachcing vote from " + player + "/" + service
								+ " because time change is happening right now");
						return;
					}
				}
			}

			if (uuid == null || uuid.isEmpty()) {
				uuid = getUUID(player);
			}

			if (uuid.isEmpty()) {
				if (config.getGeyserSupport()) {
					if (!player.startsWith(config.getGeyserPrefix())) {
						uuid = getUUID(config.getGeyserPrefix() + player);
						player = config.getGeyserPrefix() + player;
					}
				}
			}

			if (uuid.isEmpty()) {
				if (config.getAllowUnJoined()) {
					debug("Fetching UUID online, since allowunjoined is enabled");
					UUID u = null;
					try {
						if (config.getOnlineMode()) {
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
				} else {
					getLogger().info("Ignoring vote from " + player + " since player hasn't joined before");
					return;
				}
			}

			player = getProperName(uuid, player);

			if (!getConfig().getPrimaryServer()) {
				addVoteParty();
				if (getConfig().getBungeeManageTotals()) {

					if (mysql == null) {
						getLogger().severe("Mysql is not loaded correctly, stopping vote processing");
						return;
					}

					// one time query to insert player
					if (!mysql.getUuids().contains(uuid)) {
						mysql.update(uuid, "PlayerName", new DataValueString(player));
					}

					ArrayList<Column> data = mysql.getExactQuery(new Column("uuid", new DataValueString(uuid)));

					int allTimeTotal = getValue(data, "AllTimeTotal", 1);
					int monthTotal = getValue(data, "MonthTotal", 1);
					int weeklyTotal = getValue(data, "WeeklyTotal", 1);
					int dailyTotal = getValue(data, "DailyTotal", 1);
					int points = getValue(data, "Points", getConfig().getPointsOnVote());
					int milestoneCount = getValue(data, "MilestoneCount", 1);
					text = new BungeeMessageData(allTimeTotal, monthTotal, weeklyTotal, dailyTotal, points,
							milestoneCount, votePartyVotes, currentVotePartyVotesRequired);
					ArrayList<Column> update = new ArrayList<Column>();
					update.add(new Column("AllTimeTotal", new DataValueInt(allTimeTotal)));
					update.add(new Column("MonthTotal", new DataValueInt(monthTotal)));
					update.add(new Column("WeeklyTotal", new DataValueInt(weeklyTotal)));
					update.add(new Column("DailyTotal", new DataValueInt(dailyTotal)));
					update.add(new Column("Points", new DataValueInt(points)));
					update.add(new Column("MilestoneCount", new DataValueInt(milestoneCount)));
					debug("Setting totals " + text.toString());
					mysql.update(uuid, update);
				} else {
					text = new BungeeMessageData(0, 0, 0, 0, 0, 0, votePartyVotes, currentVotePartyVotesRequired);
				}
			}

			long time = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
			if (queueTime != 0) {
				time = queueTime;
			}

			if (method.equals(BungeeMethod.PLUGINMESSAGING)) {

				if (config.getSendVotesToAllServers()) {
					for (String s : getProxy().getServers().keySet()) {
						if (!config.getBlockedServers().contains(s)) {
							ServerInfo info = getProxy().getServerInfo(s);
							boolean forceCache = false;
							ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
							if (!isOnline(p) && getConfig().getWaitForUserOnline()) {
								forceCache = true;
								debug("Forcing vote to cache");
							}
							if (config.getBroadcast()) {
								sendPluginMessageServer(s, "VoteBroadcast", uuid, player, service);
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
										Boolean.TRUE.toString(), "" + realVote, text.toString(),
										"" + getConfig().getBungeeManageTotals(),
										"" + BungeeVersion.getPluginMessageVersion(), "" + config.getBroadcast(), "1");
							}

						}
					}
				} else {
					ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
					if (isOnline(p) && !config.getBlockedServers().contains(p.getServer().getInfo().getName())) {
						sendPluginMessageServer(p.getServer().getInfo().getName(), "VoteOnline", player, uuid, service,
								"" + time, Boolean.TRUE.toString(), "" + realVote, text.toString(),
								"" + getConfig().getBungeeManageTotals(), "" + BungeeVersion.getPluginMessageVersion(),
								"" + config.getBroadcast(), "1");
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
					for (String s : getProxy().getServers().keySet()) {
						if (config.getBroadcast()) {
							sendPluginMessageServer(s, "VoteBroadcast", uuid, player, service);
						}
						sendPluginMessageServer(s, "VoteUpdate", uuid, "" + votePartyVotes,
								"" + currentVotePartyVotesRequired);

					}
				}
			} else if (method.equals(BungeeMethod.SOCKETS)) {
				sendSocketVote(player, service, text);
			}
			if (getConfig().getRedisSupport() && getConfig().getPrimaryServer()) {
				sendRedisServerMessage("Vote", uuid, player, service, "" + votePartyVotes,
						"" + currentVotePartyVotesRequired, "" + time, "" + realVote, text.toString());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	private int votePartyVotes = 0;
	private int currentVotePartyVotesRequired = 0;

	public void addCurrentVotePartyVotes(int amount) {
		votePartyVotes += amount;
		voteCacheFile.setVotePartyCurrentVotes(votePartyVotes);
		debug("Current vote party total: " + votePartyVotes);
	}

	public void addVoteParty() {
		if (getConfig().getVotePartyEnabled()) {
			addCurrentVotePartyVotes(1);

			if (votePartyVotes >= currentVotePartyVotesRequired) {
				debug("Vote party reached");
				addCurrentVotePartyVotes(-currentVotePartyVotesRequired);

				currentVotePartyVotesRequired += getConfig().getVotePartyIncreaseVotesRequired();
				voteCacheFile.setVotePartyInreaseVotesRequired(voteCacheFile.getVotePartyInreaseVotesRequired()
						+ getConfig().getVotePartyIncreaseVotesRequired());

				if (!getConfig().getVotePartyBroadcast().isEmpty()) {
					getProxy().broadcast(new TextComponent(
							ChatColor.translateAlternateColorCodes('&', getConfig().getVotePartyBroadcast())));
				}

				for (String command : getConfig().getVotePartyBungeeCommands()) {
					getProxy().getPluginManager().dispatchCommand(getProxy().getConsole(), command);
				}

				if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
					if (getConfig().getVotePartySendToAllServers()) {
						for (Entry<String, ServerInfo> entry : getProxy().getServers().entrySet()) {
							sendVoteParty(entry.getKey(), entry.getValue());
						}
					} else {
						for (String server : getConfig().getVotePartyServersToSend()) {
							ServerInfo serverInfo = getProxy().getServerInfo(server);
							if (serverInfo != null) {
								sendVoteParty(server, serverInfo);
							}
						}
					}
				}
			}
			voteCacheFile.save();
		}
	}

	public void sendVoteParty(String server, ServerInfo serverInfo) {
		if (!serverInfo.getPlayers().isEmpty()) {
			if (method.equals(BungeeMethod.PLUGINMESSAGING)) {
				sendPluginMessageServer(server, "VotePartyBungee");
			} else if (method.equals(BungeeMethod.SOCKETS)) {
				sendServerMessageServer(server, "VotePartyBungee");
			}
		}
	}

}
