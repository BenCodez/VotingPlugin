package com.bencodez.votingplugin.proxy.bungee;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.security.CodeSource;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigBungee;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.BungeeVersion;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.ProxyMysqlUserTable;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import lombok.Getter;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.TextComponent;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.connection.Server;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.event.EventHandler;

public class VotingPluginBungee extends Plugin implements Listener {

	@Getter
	private Config config;

	private NonVotedPlayersCache nonVotedPlayersCache;

	private VoteCache voteCacheFile;

	private String buildNumber = "NOTSET";

	private VoteEventBungee voteEventBungee;

	@Getter
	private VotingPluginProxy votingPluginProxy;

	@Getter
	private ScheduledExecutorService timer;

	public void debug(String msg) {
		if (config.getDebug()) {
			getLogger().info("Debug: " + msg);
		}
	}

	public void debug2(String msg) {
		debug(msg);
	}

	public Set<String> getAvailableAllServers() {
		Set<String> servers = new HashSet<>();
		if (config.getWhiteListedServers().isEmpty()) {
			for (String s : getProxy().getServers().keySet()) {
				if (!config.getBlockedServers().contains(s)) {
					servers.add(s);
				}
			}
		} else {
			servers.addAll(config.getWhiteListedServers());
		}

		return servers;
	}

	public String getProperPlayerName(String uuid, String currentName) {
		ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
		if (p != null && p.isConnected()) {
			return p.getName();
		}
		return currentName;
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

	public boolean isOnline(ProxiedPlayer p) {
		if (p != null) {
			return p.isConnected();
		}

		return false;
	}

	private void loadMysql() {
		votingPluginProxy.setProxyMySQL(new ProxyMysqlUserTable("VotingPlugin_Users",
				new MysqlConfigBungee(config.getData()), config.getDebug()) {

			@Override
			public void debug(SQLException e) {
				if (config.getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void logSevere(String string) {
				getLogger().severe(string);

			}

			@Override
			public void logInfo(String string) {
				getLogger().info(string);
			}

			@Override
			public void severe(String string) {
				getLogger().severe(string);
			}

		});

		ArrayList<String> servers = new ArrayList<>();
		for (String s : getAvailableAllServers()) {
			servers.add(s);
		}

		if (config.getGlobalDataEnabled()) {
			if (config.getGlobalDataUseMainMySQL()) {
				votingPluginProxy.setGlobalDataHandler(new GlobalDataHandlerProxy(
						new GlobalMySQL("VotingPlugin_GlobalData", getVotingPluginProxy().getProxyMySQL().getMysql()) {

							@Override
							public void debugEx(Exception e) {
								if (config.getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debugLog(String text) {
								debug2(text);
							}

							@Override
							public void info(String text) {
								getLogger().info(text);

							}

							@Override
							public void logSevere(String text) {
								getLogger().severe(text);
							}

							@Override
							public void warning(String text) {
								getLogger().warning(text);
							}
						}, servers) {

					@Override
					public void onTimeChangedFailed(String server, TimeType type) {
						getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, type.toString(), false);
						getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, "FinishedProcessing", true);
						getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, "Processing", false);
					}

					@Override
					public void onTimeChangedFinished(TimeType type) {
						if (type.equals(TimeType.MONTH)) {
							getVotingPluginProxy().getProxyMySQL().copyColumnData(TopVoter.Monthly.getColumnName(),
									"LastMonthTotal");
						}
						getVotingPluginProxy().getProxyMySQL().wipeColumnData(TopVoter.of(type).getColumnName(),
								DataType.INTEGER);

						if (!config.getGlobalDataEnabled()) {
							return;
						}
						for (String s : getAvailableAllServers()) {
							getVotingPluginProxy().getGlobalDataHandler().setBoolean(s, "ForceUpdate", true);
							getVotingPluginProxy().getGlobalMessageProxyHandler().sendMessage(s, 1, "BungeeTimeChange",
									"");
						}

						getVotingPluginProxy().processQueue();

					}
				});
			} else {
				votingPluginProxy
						.setGlobalDataHandler(new GlobalDataHandlerProxy(new GlobalMySQL("VotingPlugin_GlobalData",
								new MysqlConfigBungee(config.getData().getSection("GlobalData"))) {

							@Override
							public void debugEx(Exception e) {
								if (config.getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debugLog(String text) {
								debug2(text);
							}

							@Override
							public void info(String text) {
								getLogger().info(text);

							}

							@Override
							public void logSevere(String text) {
								getLogger().severe(text);
							}

							@Override
							public void warning(String text) {
								getLogger().warning(text);
							}
						}, servers) {

							@Override
							public void onTimeChangedFailed(String server, TimeType type) {
								getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, type.toString(),
										false);
								getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, "FinishedProcessing",
										true);
								getVotingPluginProxy().getGlobalDataHandler().setBoolean(server, "Processing", false);
							}

							@Override
							public void onTimeChangedFinished(TimeType type) {
								if (type.equals(TimeType.MONTH)) {
									getVotingPluginProxy().getProxyMySQL()
											.copyColumnData(TopVoter.Monthly.getColumnName(), "LastMonthTotal");
								}
								getVotingPluginProxy().getProxyMySQL().wipeColumnData(TopVoter.of(type).getColumnName(),
										DataType.INTEGER);

								if (!config.getGlobalDataEnabled()) {
									return;
								}
								for (String s : getAvailableAllServers()) {
									getVotingPluginProxy().getGlobalDataHandler().setBoolean(s, "ForceUpdate", true);
									getVotingPluginProxy().getGlobalMessageProxyHandler().sendMessage(s, 1,
											"BungeeTimeChange", "");
								}

								getVotingPluginProxy().processQueue();

							}
						});
			}
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("IgnoreTime", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("MONTH", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("WEEK", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("DAY", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("FinishedProcessing",
					"VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("Processing", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("ForceUpdate", "VARCHAR(5)");
			getVotingPluginProxy().getGlobalDataHandler().getGlobalMysql().alterColumnType("LastUpdated", "MEDIUMTEXT");
		}

		// column types
		getVotingPluginProxy().getProxyMySQL().alterColumnType("TopVoterIgnore", "VARCHAR(5)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("CheckWorld", "VARCHAR(5)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("Reminded", "VARCHAR(5)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("DisableBroadcast", "VARCHAR(5)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("LastOnline", "VARCHAR(20)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("PlayerName", "VARCHAR(30)");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("DailyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("WeeklyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("DayVoteStreak", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("BestDayVoteStreak", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("WeekVoteStreak", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("BestWeekVoteStreak", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("VotePartyVotes", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("MonthVoteStreak", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("Points", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("HighestDailyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("AllTimeTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("HighestMonthlyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("MilestoneCount", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("MonthTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("HighestWeeklyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("LastMonthTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("LastWeeklyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("LastDailyTotal", "INT DEFAULT '0'");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("OfflineRewards", "MEDIUMTEXT");
		getVotingPluginProxy().getProxyMySQL().alterColumnType("DayVoteStreakLastUpdate", "MEDIUMTEXT");
		if (config.getStoreMonthTotalsWithDate()) {
			getVotingPluginProxy().getProxyMySQL().alterColumnType(
					getVotingPluginProxy().getMonthTotalsWithDatePath(LocalDateTime.now()), "INT DEFAULT '0'");
			getVotingPluginProxy().getProxyMySQL().alterColumnType(
					getVotingPluginProxy().getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(1)),
					"INT DEFAULT '0'");
			getVotingPluginProxy().getProxyMySQL().alterColumnType(
					getVotingPluginProxy().getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(2)),
					"INT DEFAULT '0'");
		}
	}

	public void loadVersionFile() {
		Configuration conf = getVersionFile();
		if (conf != null) {
			buildNumber = conf.get("buildnumber", "NOTSET");
		}
	}

	public void login(ProxiedPlayer p) {
		if (p != null && p.getServer() != null && p.getServer().getInfo() != null) {
			final ProxiedPlayer proxiedPlayer = p;
			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {
					getVotingPluginProxy().login(proxiedPlayer.getName(), proxiedPlayer.getUniqueId().toString(),
							proxiedPlayer.getServer().getInfo().getName());
				}
			}, 1, TimeUnit.SECONDS);
		}

	}

	@Override
	public void onDisable() {
		if (getVotingPluginProxy().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			getLogger().info("VotingPlugin saving vote cache: " + getVotingPluginProxy().getCachedVotes().size() + "/"
					+ getVotingPluginProxy().getCachedOnlineVotes().size());
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getVotingPluginProxy().getCachedVotes()
					.entrySet()) {
				String server = entry.getKey();
				int num = 0;
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheFile.addVote(server, num, voteData);
					num++;
				}
			}
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getVotingPluginProxy().getCachedOnlineVotes()
					.entrySet()) {
				String name = entry.getKey();
				int num = 0;
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheFile.addVoteOnline(name, num, voteData);
					num++;
				}
			}
		}
		if (!getVotingPluginProxy().getTimeChangeQueue().isEmpty()) {
			int num = 0;
			for (VoteTimeQueue vote : getVotingPluginProxy().getTimeChangeQueue()) {
				voteCacheFile.addTimedVote(num, vote);
				num++;
			}
		}

		getVotingPluginProxy().onDisable();

		voteCacheFile.save();
		nonVotedPlayersCache.save();

		timer.shutdownNow();

		getLogger().info("VotingPlugin disabled");

	}

	@Override
	public void onEnable() {

		timer = Executors.newScheduledThreadPool(1);

		getProxy().getPluginManager().registerListener(this, this);

		config = new Config(this);
		config.load();

		getProxy().getPluginManager().registerCommand(this, new VotingPluginBungeeCommand(this));

		votingPluginProxy = new VotingPluginProxy() {

			@Override
			public void addNonVotedPlayer(String uuid, String playerName) {
				nonVotedPlayersCache.addPlayer(uuid, playerName);
			}

			@Override
			public void broadcast(String message) {
				getProxy().broadcast(new TextComponent(ChatColor.translateAlternateColorCodes('&', message)));
			}

			@Override
			public void debug(String str) {
				debug2(str);
			}

			@Override
			public Set<String> getAllAvailableServers() {
				return getAvailableAllServers();
			}

			@Override
			public VotingPluginProxyConfig getConfig() {
				return config;
			}

			@Override
			public String getCurrentPlayerServer(String player) {
				return getProxy().getPlayer(player).getServer().getInfo().getName();
			}

			@Override
			public File getDataFolderPlugin() {
				return getDataFolder();
			}

			@Override
			public String getProperName(String uuid, String playerName) {
				return getProperPlayerName(uuid, playerName);
			}

			@Override
			public String getUUID(String playerName) {

				if (!config.getOnlineMode()) {
					// correct casing
					ProxiedPlayer p = getProxy().getPlayer(playerName);
					if (p != null && p.isConnected()) {
						playerName = p.getName();
					}
					return UUID.nameUUIDFromBytes(("OfflinePlayer:" + playerName).getBytes(StandardCharsets.UTF_8))
							.toString();
				}
				ProxiedPlayer p = getProxy().getPlayer(playerName);
				if (p != null && p.isConnected()) {
					return p.getUniqueId().toString();
				}

				for (Entry<UUID, String> entry : getVotingPluginProxy().getUuidPlayerNameCache().entrySet()) {
					if (entry.getValue().equalsIgnoreCase(playerName)) {
						return entry.getKey().toString();
					}
				}

				if (getVotingPluginProxy().getProxyMySQL() != null) {
					String str = getVotingPluginProxy().getProxyMySQL().getUUID(playerName);
					if (str != null) {
						return str;
					}
				}
				if (nonVotedPlayersCache != null) {
					return nonVotedPlayersCache.playerExists(playerName);
				}

				return "";

			}

			@Override
			public String getPluginVersion() {
				return getDescription().getVersion();
			}

			@Override
			public int getVoteCacheCurrentVotePartyVotes() {
				return voteCacheFile.getVotePartyCurrentVotes();
			}

			@Override
			public long getVoteCacheLastUpdated() {
				return voteCacheFile.getLong("Time.LastUpdated", 0L);
			}

			@Override
			public int getVoteCachePrevDay() {
				return voteCacheFile.getInt("Time.Day", 0);
			}

			@Override
			public String getVoteCachePrevMonth() {
				return voteCacheFile.getString("Time.Month", "");
			}

			@Override
			public int getVoteCachePrevWeek() {
				return voteCacheFile.getInt("Time.Week", 0);
			}

			@Override
			public int getVoteCacheVotePartyIncreaseVotesRequired() {
				return voteCacheFile.getVotePartyInreaseVotesRequired();
			}

			@Override
			public boolean isPlayerOnline(String playerName) {
				ProxiedPlayer player = getProxy().getPlayer(playerName);
				return player != null && player.isConnected();
			}

			@Override
			public boolean isServerValid(String server) {
				return getProxy().getServerInfo(server) != null;
			}

			@Override
			public boolean isSomeoneOnlineServer(String server) {
				return !getProxy().getServerInfo(server).getPlayers().isEmpty();
			}

			@Override
			public boolean isVoteCacheIgnoreTime() {
				return voteCacheFile.getBoolean("Time.IgnoreTime", false);
			}

			@Override
			public void log(String message) {
				getLogger().info(message);
			}

			@Override
			public void logSevere(String message) {
				getLogger().severe(message);
			}

			@Override
			public void runAsync(Runnable run) {
				runAsyncNow(run);
			}

			@Override
			public void runConsoleCommand(String command) {
				getProxy().getPluginManager().dispatchCommand(getProxy().getConsole(), command);
			}

			@Override
			public void saveVoteCacheFile() {
				voteCacheFile.save();
			}

			@Override
			public void sendPluginMessageData(String server, String channel, byte[] data, boolean queue) {
				getProxy().getServerInfo(server).sendData(channel, data, queue);
			}

			@Override
			public void setVoteCacheLastUpdated() {
				voteCacheFile.setLong("Time.LastUpdated", System.currentTimeMillis());
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevDay(int day) {
				voteCacheFile.setInt("Time.Day", day);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevMonth(String text) {
				voteCacheFile.setString("Time.Month", text);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevWeek(int week) {
				voteCacheFile.setInt("Time.Week", week);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCacheVoteCacheIgnoreTime(boolean ignore) {
				voteCacheFile.setBoolean("Time.IgnoreTime", ignore);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCacheVotePartyCurrentVotes(int votes) {
				voteCacheFile.setVotePartyCurrentVotes(votes);
			}

			@Override
			public void setVoteCacheVotePartyIncreaseVotesRequired(int votes) {
				voteCacheFile.setVotePartyInreaseVotesRequired(votes);

			}

			@Override
			public void warn(String message) {
				getLogger().warning(message);
			}

			@Override
			public void reloadCore(boolean mysql) {
				reloadPlugin(mysql);
			}

			@Override
			public ScheduledExecutorService getScheduler() {
				return timer;
			}

		};
		try {
			Class.forName("com.vexsoftware.votifier.bungee.events.VotifierEvent");
		} catch (ClassNotFoundException e) {
			getVotingPluginProxy().setVotifierEnabled(false);
		}
		if (getVotingPluginProxy().isVotifierEnabled()) {
			try {
				voteEventBungee = new VoteEventBungee(this);
				getProxy().getPluginManager().registerListener(this, voteEventBungee);
			} catch (Exception e) {
				getVotingPluginProxy().setVotifierEnabled(false);
			}
		}

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

		this.getProxy().registerChannel(getConfig().getPluginMessageChannel());

		if (mysqlLoaded) {

			voteCacheFile = new VoteCache(this);
			// voteCacheFile.load();

			nonVotedPlayersCache = new NonVotedPlayersCache(this);
			// nonVotedPlayersCache.load();

			getVotingPluginProxy().load();

			try {
				for (String key : voteCacheFile.getTimedVoteCache()) {
					JsonElement dataElement = voteCacheFile.getTimedVoteCache(key);

					if (dataElement != null && dataElement.isJsonObject()) {
						JsonObject data = dataElement.getAsJsonObject();
						String name = data.has("Name") ? data.get("Name").getAsString() : "";
						String service = data.has("Service") ? data.get("Service").getAsString() : "";
						long time = data.has("Time") ? data.get("Time").getAsLong() : 0L;

						getVotingPluginProxy().getTimeChangeQueue().add(new VoteTimeQueue(name, service, time));
					}
				}

				getVotingPluginProxy().processQueue();
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String server : voteCacheFile.getServers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : voteCacheFile.getServerVotes(server)) {
						JsonElement dataElement = voteCacheFile.getServerVotes(server, num);

						if (dataElement != null && dataElement.isJsonObject()) {
							JsonObject data = dataElement.getAsJsonObject();

							String name = data.has("Name") ? data.get("Name").getAsString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").getAsString() : "";
							String service = data.has("Service") ? data.get("Service").getAsString() : "";
							long time = data.has("Time") ? data.get("Time").getAsLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").getAsBoolean();
							String text = data.has("Text") ? data.get("Text").getAsString() : "";

							votes.add(new OfflineBungeeVote(name, uuid, service, time, real, text));
						}
					}
					getVotingPluginProxy().getCachedVotes().put(server, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : voteCacheFile.getPlayers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : voteCacheFile.getOnlineVotes(player)) {
						JsonElement dataElement = voteCacheFile.getOnlineVotes(player, num);

						if (dataElement != null && dataElement.isJsonObject()) {
							JsonObject data = dataElement.getAsJsonObject();

							String name = data.has("Name") ? data.get("Name").getAsString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").getAsString() : "";
							String service = data.has("Service") ? data.get("Service").getAsString() : "";
							long time = data.has("Time") ? data.get("Time").getAsLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").getAsBoolean();
							String text = data.has("Text") ? data.get("Text").getAsString() : "";

							votes.add(new OfflineBungeeVote(name, uuid, service, time, real, text));
						}
					}
					getLogger().info("VoteCache: " + player);
					getVotingPluginProxy().getCachedOnlineVotes().put(player, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			voteCacheFile.clearData();

			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {

					for (String server : getVotingPluginProxy().getCachedVotes().keySet()) {
						getVotingPluginProxy().checkCachedVotes(server);
					}

					for (String player : getVotingPluginProxy().getCachedOnlineVotes().keySet()) {
						ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(player));
						if (p != null) {
							getVotingPluginProxy().checkOnlineVotes(p.getName(), player, null);
						}
					}
				}
			}, 120l, 60l, TimeUnit.SECONDS);

			getProxy().getScheduler().schedule(this, new Runnable() {

				@Override
				public void run() {
					if (nonVotedPlayersCache != null) {
						debug("Checking nonvotedplayers.yml...");
						nonVotedPlayersCache.check();
					}
					if (voteCacheFile != null) {
						voteCacheFile.save();
					}
				}
			}, 1l, 60l, TimeUnit.MINUTES);

		}

		if (!getVotingPluginProxy().isVotifierEnabled()) {
			if (!(getConfig().getMultiProxySupport() && !getConfig().getPrimaryServer())) {
				getLogger().warning("Votifier event not found, not loading votifier event");
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

		metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("multi_proxy_support_enabled",
				() -> "" + getConfig().getMultiProxySupport()));

		if (!buildNumber.equals("NOTSET")) {
			metrics.addCustomChart(new BStatsMetricsBungee.SimplePie("dev_build_number", () -> "" + buildNumber));
		}

		loadVersionFile();

		getLogger().info("VotingPlugin loaded, using method: " + getVotingPluginProxy().getMethod().toString()
				+ ", PluginMessagingVersion: " + BungeeVersion.getPluginMessageVersion());
		if (!buildNumber.equals("NOTSET")) {
			getLogger().info("Detected using dev build number: " + buildNumber);
		}
		getVotingPluginProxy().sendServerNameMessage();

	}

	@EventHandler
	public void onPluginMessage(PluginMessageEvent ev) {
		if (!ev.getTag().equals(getConfig().getPluginMessageChannel().toLowerCase())) {
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
			getVotingPluginProxy().onPluginMessageReceived(in);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void reloadPlugin(boolean loadMysql) {
		config.load();
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
		getVotingPluginProxy().reload();
	}

	private void runAsyncNow(Runnable runnable) {
		getProxy().getScheduler().runAsync(this, runnable);
	}

}
