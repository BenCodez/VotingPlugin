package com.bencodez.votingplugin.bungee.velocity;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
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

import org.bstats.charts.SimplePie;
import org.bstats.velocity.Metrics;
import org.slf4j.Logger;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.userstorage.DataType;
import com.bencodez.advancedcore.api.user.userstorage.mysql.api.config.MysqlConfigVelocity;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.advancedcore.bungeeapi.mysql.VelocityMySQL;
import com.bencodez.simpleapi.file.velocity.VelocityYMLFile;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.bungee.BungeeVersion;
import com.bencodez.votingplugin.bungee.OfflineBungeeVote;
import com.bencodez.votingplugin.bungee.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.bungee.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.google.inject.Inject;
import com.velocitypowered.api.command.CommandMeta;
import com.velocitypowered.api.event.Subscribe;
import com.velocitypowered.api.event.connection.PluginMessageEvent;
import com.velocitypowered.api.event.proxy.ProxyInitializeEvent;
import com.velocitypowered.api.event.proxy.ProxyShutdownEvent;
import com.velocitypowered.api.plugin.Dependency;
import com.velocitypowered.api.plugin.Plugin;
import com.velocitypowered.api.plugin.annotation.DataDirectory;
import com.velocitypowered.api.proxy.Player;
import com.velocitypowered.api.proxy.ProxyServer;
import com.velocitypowered.api.proxy.ServerConnection;
import com.velocitypowered.api.proxy.messages.ChannelIdentifier;
import com.velocitypowered.api.proxy.messages.MinecraftChannelIdentifier;
import com.velocitypowered.api.proxy.server.RegisteredServer;

import lombok.Getter;
import net.kyori.adventure.text.Component;
import ninja.leaping.configurate.ConfigurationNode;
import ninja.leaping.configurate.yaml.YAMLConfigurationLoader;

@Plugin(id = "votingplugin", name = "VotingPlugin", version = "1.0", url = "https://www.spigotmc.org/resources/votingplugin.15358/", description = "VotingPlugin Velocity Version", authors = {
		"BenCodez" }, dependencies = { @Dependency(id = "nuvotifier", optional = true) })
public class VotingPluginVelocity {

	private ChannelIdentifier CHANNEL;

	@Getter
	private Config config;
	private final Path dataDirectory;
	@Getter
	private final Logger logger;

	private final Metrics.Factory metricsFactory;

	private NonVotedPlayersCache nonVotedPlayersCache;

	private final ProxyServer server;

	private VoteCache voteCacheFile;

	private String version = "";

	private File versionFile;

	private String buildNumber = "NOTSET";

	@Getter
	private ScheduledExecutorService timer;

	@Inject
	public VotingPluginVelocity(ProxyServer server, Logger logger, Metrics.Factory metricsFactory,
			@DataDirectory Path dataDirectory) {
		this.server = server;
		this.logger = logger;
		this.dataDirectory = dataDirectory;
		this.metricsFactory = metricsFactory;
		timer = Executors.newScheduledThreadPool(1);
	}

	public void debug(String msg) {
		if (config.getDebug()) {
			logger.info("Debug: " + msg);
		}
	}

	public void debug2(String msg) {
		debug(msg);
	}

	public Set<String> getAvailableAllServers() {
		Set<String> servers = new HashSet<>();
		if (config.getWhiteListedServers().isEmpty()) {
			for (RegisteredServer s : server.getAllServers()) {
				if (!config.getBlockedServers().contains(s.getServerInfo().getName())) {
					servers.add(s.getServerInfo().getName());
				}
			}
		} else {
			for (RegisteredServer s : server.getAllServers()) {
				if (config.getWhiteListedServers().contains(s.getServerInfo().getName())) {
					servers.add(s.getServerInfo().getName());
				}
			}
		}

		return servers;
	}

	public String getProperPlayerName(String uuid, String currentName) {
		if (server.getPlayer(UUID.fromString(uuid)).isPresent()) {
			Player p = server.getPlayer(UUID.fromString(uuid)).get();
			if (p != null && p.isActive()) {
				return p.getUsername();
			}
		}
		return currentName;
	}

	private void getVersionFile() {
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
								versionFile = new File(dataDirectory.toFile(),
										"tmp" + File.separator + "votingpluginversion.yml");
								if (!versionFile.exists()) {
									versionFile.getParentFile().mkdirs();
									versionFile.createNewFile();
								}
								FileWriter fileWriter = new FileWriter(versionFile);

								int charVal;
								while ((charVal = defConfigStream.read()) != -1) {
									fileWriter.append((char) charVal);
								}

								fileWriter.close();
								YAMLConfigurationLoader loader = YAMLConfigurationLoader.builder().setFile(versionFile)
										.build();
								defConfigStream.close();
								ConfigurationNode node = loader.load();
								if (node != null) {
									version = node.getNode("version").getString("");
									buildNumber = node.getNode("buildnumber").getString("NOTSET");
								}
								return;
							}
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void loadMysql() {
		votingPluginProxy.setProxyMySQL(new VelocityMySQL("VotingPlugin_Users", config) {

			@Override
			public void debug(SQLException e) {
				if (config.getDebug()) {
					e.printStackTrace();
				}
			}

			@Override
			public void severe(String str) {
				getLogger().error(str);
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
							public void debug(Exception e) {
								if (config.getDebug()) {
									e.printStackTrace();
								}
							}

							@Override
							public void debug(String text) {
								debug2(text);
							}

							@Override
							public void info(String text) {
								logger.info(text);
							}

							@Override
							public void severe(String text) {
								logger.error(text);
							}

							@Override
							public void warning(String text) {
								logger.warn(text);
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
							if (getVotingPluginProxy().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
								getVotingPluginProxy().sendPluginMessageServer(s, "BungeeTimeChange", "");
							} else if (getVotingPluginProxy().getMethod().equals(BungeeMethod.SOCKETS)) {
								getVotingPluginProxy().sendServerMessage(s, "BungeeTimeChange");
							}
						}

						getVotingPluginProxy().processQueue();
					}
				});
			} else {
				votingPluginProxy.setGlobalDataHandler(new GlobalDataHandlerProxy(
						new GlobalMySQL("VotingPlugin_GlobalData", new MysqlConfigVelocity("GlobalData", config)) {

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

							@Override
							public void info(String text) {
								logger.info(text);
							}

							@Override
							public void severe(String text) {
								logger.error(text);
							}

							@Override
							public void warning(String text) {
								logger.warn(text);
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
							if (getVotingPluginProxy().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
								getVotingPluginProxy().sendPluginMessageServer(s, "BungeeTimeChange", "");
							} else if (getVotingPluginProxy().getMethod().equals(BungeeMethod.SOCKETS)) {
								getVotingPluginProxy().sendServerMessage(s, "BungeeTimeChange");
							}
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

	@Subscribe
	public void onPluginMessagingReceived(PluginMessageEvent event) {
		if (event.getIdentifier().getId().equals(CHANNEL.getId())) {
			event.setResult(PluginMessageEvent.ForwardResult.handled());

	        if (!(event.getSource() instanceof ServerConnection)) {
	            return;
	        }
			
			ByteArrayInputStream instream = new ByteArrayInputStream(event.getData());
			DataInputStream in = new DataInputStream(instream);
			try {
				getVotingPluginProxy().onPluginMessageReceived(in);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	@Subscribe
	public void onProxyDisable(ProxyShutdownEvent event) {
		if (votingPluginProxy.getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			logger.info("VotingPlugin saving vote cache: " + getVotingPluginProxy().getCachedVotes().size() + "/"
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

		logger.info("VotingPlugin disabled");
	}

	@Getter
	public VotingPluginProxy votingPluginProxy;

	@Subscribe
	public void onProxyInitialization(ProxyInitializeEvent event) {
		File configFile = new File(dataDirectory.toFile(), "bungeeconfig.yml");
		configFile.getParentFile().mkdirs();
		if (!configFile.exists()) {
			try {
				configFile.createNewFile();
			} catch (IOException e) {
				e.printStackTrace();
			}

			InputStream toCopyStream = VotingPluginVelocity.class.getClassLoader()
					.getResourceAsStream("bungeeconfig.yml");

			try (FileOutputStream fos = new FileOutputStream(configFile)) {
				byte[] buf = new byte[2048];
				int r;
				while (-1 != (r = toCopyStream.read(buf))) {
					fos.write(buf, 0, r);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		config = new Config(configFile);
		String[] channel = config.getPluginMessageChannel().split(":");
		CHANNEL = MinecraftChannelIdentifier.create(channel[0].toLowerCase(), channel[1].toLowerCase());
		server.getChannelRegistrar().register(CHANNEL);

		CommandMeta meta = server.getCommandManager().metaBuilder("votingpluginbungee")
				// Specify other aliases (optional)
				.aliases("vpb").build();

		server.getCommandManager().register(meta, new VotingPluginVelocityCommand(this));

		votingPluginProxy = new VotingPluginProxy() {

			@Override
			public void addNonVotedPlayer(String uuid, String playerName) {
				nonVotedPlayersCache.addPlayer(uuid, playerName);
			}

			@Override
			public void broadcast(String message) {
				server.getAllPlayers().forEach(player -> player.sendMessage(Component.text(message)));
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
				if (server.getPlayer(player).isPresent()) {
					return server.getPlayer(player).get().getCurrentServer().get().getServer().getServerInfo()
							.getName();
				}
				return "";
			}

			@Override
			public File getDataFolderPlugin() {
				return dataDirectory.toFile();
			}

			@Override
			public String getProperName(String uuid, String playerName) {
				return getProperPlayerName(uuid, playerName);
			}

			@Override
			public String getUUID(String playerName) {

				if (!config.getOnlineMode()) {
					return UUID.nameUUIDFromBytes(("OfflinePlayer:" + playerName).getBytes(StandardCharsets.UTF_8))
							.toString();
				}
				if (server.getPlayer(playerName).isPresent()) {
					Player p = server.getPlayer(playerName).get();
					if (p != null && p.isActive()) {
						return p.getUniqueId().toString();
					}
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
				return version;
			}

			@Override
			public int getVoteCacheCurrentVotePartyVotes() {
				return voteCacheFile.getVotePartyCurrentVotes();
			}

			@Override
			public long getVoteCacheLastUpdated() {
				return voteCacheFile.getNode("Time", "LastUpdated").getLong();
			}

			@Override
			public int getVoteCachePrevDay() {
				return voteCacheFile.getNode("Time", "Day").getInt();
			}

			@Override
			public String getVoteCachePrevMonth() {
				return voteCacheFile.getNode("Time", "Month").getString("");
			}

			@Override
			public int getVoteCachePrevWeek() {
				return voteCacheFile.getNode("Time", "Week").getInt();
			}

			@Override
			public int getVoteCacheVotePartyIncreaseVotesRequired() {
				return voteCacheFile.getVotePartyInreaseVotesRequired();
			}

			@Override
			public boolean isPlayerOnline(String playerName) {
				if (playerName == null) {
					return false;
				}
				if (server.getPlayer(playerName).isPresent()) {
					return server.getPlayer(playerName).get().isActive();
				}
				return false;
			}

			@Override
			public boolean isServerValid(String serverName) {
				return server.getServer(serverName).isPresent();
			}

			@Override
			public boolean isSomeoneOnlineServer(String serverName) {
				if (server.getServer(serverName).isPresent()) {
					return !server.getServer(serverName).get().getPlayersConnected().isEmpty();
				}
				return false;
			}

			@Override
			public boolean isVoteCacheIgnoreTime() {
				return voteCacheFile.getNode("Time", "IgnoreTime").getBoolean();
			}

			@Override
			public void log(String message) {
				getLogger().info(message);
			}

			@Override
			public void logSevere(String message) {
				logger.error(message);
			}

			@Override
			public void runAsync(Runnable run) {
				runAsyncNow(run);
			}

			@Override
			public void runConsoleCommand(String command) {
				server.getCommandManager().executeAsync(server.getConsoleCommandSource(), command);
			}

			@Override
			public void saveVoteCacheFile() {
				voteCacheFile.save();
			}

			@Override
			public void sendPluginMessageData(String serverName, String channel, byte[] data, boolean queue) {
				if (!server.getServer(serverName).isPresent()) {
					return;
				}
				RegisteredServer send = server.getServer(serverName).get();
				send.sendPluginMessage(CHANNEL, data);
			}

			@Override
			public void setVoteCacheLastUpdated() {
				voteCacheFile.getNode("Time", "LastUpdated").setValue(System.currentTimeMillis());
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevDay(int day) {
				voteCacheFile.getNode("Time", "Day").setValue(day);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevMonth(String text) {
				voteCacheFile.getNode("Time", "Month").setValue(text);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevWeek(int week) {
				voteCacheFile.getNode("Time", "Week").setValue(week);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCacheVoteCacheIgnoreTime(boolean ignore) {
				voteCacheFile.getNode("Time", "IgnoreTime").setValue(ignore);
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
				logger.warn(message);
			}

			@Override
			public void reloadCore(boolean mysql) {
				reloadPlugin(mysql);
			}

		};

		try {
			Class.forName("com.vexsoftware.votifier.velocity.event.VotifierEvent");
		} catch (ClassNotFoundException e) {
			getVotingPluginProxy().setVotifierEnabled(false);
		}
		if (getVotingPluginProxy().isVotifierEnabled()) {
			try {
				server.getEventManager().register(this, new VoteEventVelocity(this));
			} catch (Exception e) {
				getVotingPluginProxy().setVotifierEnabled(false);
			}
		}

		boolean mysqlLoaded = true;
		try {
			if (!config.getString(config.getNode("Host"), "").isEmpty()) {
				loadMysql();
			} else {
				mysqlLoaded = false;
				logger.error("MySQL settings not set in bungeeconfig.yml");
			}
		} catch (Exception e) {
			mysqlLoaded = false;
			e.printStackTrace();
		}

		if (mysqlLoaded) {

			voteCacheFile = new VoteCache(new File(dataDirectory.toFile(), "votecache.json"));

			// convert yml file if exists
			File yamlVoteCacheFile = new File(dataDirectory.toFile(), "votecache.yml");
			if (yamlVoteCacheFile.exists()) {
				VelocityYMLFile yamlVoteCache = new VelocityYMLFile(yamlVoteCacheFile);
				voteCacheFile.setConf(yamlVoteCache.getData());
				yamlVoteCacheFile.renameTo(new File(dataDirectory.toFile(), "oldvotecache.yml"));
				VelocityYMLFile oldYamlVoteCache = new VelocityYMLFile(
						new File(dataDirectory.toFile(), "oldvotecache.yml"));
				oldYamlVoteCache.setConf(yamlVoteCache.getData());
				voteCacheFile.save();
			}

			nonVotedPlayersCache = new NonVotedPlayersCache(
					new File(dataDirectory.toFile(), "nonvotedplayerscache.json"), this);

			// convert yml file if exists
			File yamlnonVotedPlayersCacheFile = new File(dataDirectory.toFile(), "nonvotedplayerscache.yml");
			if (yamlnonVotedPlayersCacheFile.exists()) {
				VelocityYMLFile yamlnonVotedPlayersCache = new VelocityYMLFile(yamlnonVotedPlayersCacheFile);
				voteCacheFile.setConf(yamlnonVotedPlayersCache.getData());
				yamlnonVotedPlayersCacheFile.renameTo(new File(dataDirectory.toFile(), "oldnonvotedplayerscache.yml"));
				VelocityYMLFile oldYamlnonVotedPlayersCache = new VelocityYMLFile(
						new File(dataDirectory.toFile(), "oldnonvotedplayerscache.yml"));
				oldYamlnonVotedPlayersCache.setConf(yamlnonVotedPlayersCache.getData());
				voteCacheFile.save();
			}

			getVotingPluginProxy().load();

			try {
				for (String key : voteCacheFile.getTimedVoteCache()) {
					ConfigurationNode data = voteCacheFile.getTimedVoteCache(key);
					getVotingPluginProxy().getTimeChangeQueue().add(new VoteTimeQueue(data.getNode("Name").getString(),
							data.getNode("Service").getString(), data.getNode("Time").getLong()));
				}

				getVotingPluginProxy().processQueue();
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String server : voteCacheFile.getServers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<>();
					for (String num : voteCacheFile.getServerVotes(server)) {
						ConfigurationNode data = voteCacheFile.getServerVotes(server, num);
						vote.add(new OfflineBungeeVote(data.getNode("Name").getString(),
								data.getNode("UUID").getString(), data.getNode("Service").getString(),
								data.getNode("Time").getLong(), data.getNode("Real").getBoolean(),
								data.getNode("Text").getString()));
					}
					getVotingPluginProxy().getCachedVotes().put(server, vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : voteCacheFile.getPlayers()) {
					ArrayList<OfflineBungeeVote> vote = new ArrayList<>();
					for (String num : voteCacheFile.getOnlineVotes(player)) {
						ConfigurationNode data = voteCacheFile.getOnlineVotes(player, num);
						vote.add(new OfflineBungeeVote(data.getNode("Name").getString(),
								data.getNode("UUID").getString(), data.getNode("Service").getString(),
								data.getNode("Time").getLong(), data.getNode("Real").getBoolean(),
								data.getNode("Text").getString()));
					}
					getVotingPluginProxy().getCachedOnlineVotes().put(player, vote);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			voteCacheFile.clearData();

			server.getScheduler().buildTask(this, () -> {
				if (getVotingPluginProxy().getGlobalDataHandler() == null
						|| !getVotingPluginProxy().getGlobalDataHandler().isTimeChangedHappened()) {
					for (String server : getVotingPluginProxy().getCachedVotes().keySet()) {
						getVotingPluginProxy().checkCachedVotes(server);
					}

					for (String player : getVotingPluginProxy().getCachedOnlineVotes().keySet()) {
						if (server.getPlayer(UUID.fromString(player)).isPresent()) {
							getVotingPluginProxy().checkOnlineVotes(
									server.getPlayer(UUID.fromString(player)).get().getUsername(), player, null);
						}
					}
				}
			}).delay(120, TimeUnit.SECONDS).repeat(60, TimeUnit.SECONDS).schedule();

			server.getScheduler().buildTask(this, () -> {
				if (nonVotedPlayersCache != null) {
					debug("Checking nonvotedplayerscache.yml...");
					nonVotedPlayersCache.check();
				}
				if (voteCacheFile != null) {
					voteCacheFile.save();
				}
			}).delay(1L, TimeUnit.MINUTES).repeat(60l, TimeUnit.MINUTES).schedule();

		}

		try {
			getVersionFile();
			if (versionFile != null) {
				versionFile.delete();
				versionFile.getParentFile().delete();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		if (!getVotingPluginProxy().isVotifierEnabled()) {
			if (!(getConfig().getMultiProxySupport() && !getConfig().getPrimaryServer())) {
				getLogger().warn("Votifier event not found, not loading votifier event");
			}
		}

		Metrics metrics = metricsFactory.make(this, 11547);

		metrics.addCustomChart(new SimplePie("bungee_method", () -> getConfig().getBungeeMethod().toString()));

		metrics.addCustomChart(new SimplePie("sendtoallservers", () -> "" + getConfig().getSendVotesToAllServers()));

		metrics.addCustomChart(new SimplePie("allowunjoined", () -> "" + getConfig().getAllowUnJoined()));

		metrics.addCustomChart(new SimplePie("pointsonvote", () -> "" + getConfig().getPointsOnVote()));

		metrics.addCustomChart(new SimplePie("bungeemanagetotals", () -> "" + getConfig().getBungeeManageTotals()));

		metrics.addCustomChart(new SimplePie("waitforuseronline", () -> "" + getConfig().getWaitForUserOnline()));

		metrics.addCustomChart(new SimplePie("plugin_version", () -> "" + version));

		metrics.addCustomChart(new SimplePie("globaldata_enabled", () -> "" + getConfig().getGlobalDataEnabled()));
		if (getConfig().getGlobalDataEnabled()) {
			metrics.addCustomChart(
					new SimplePie("globaldata_usemainmysql", () -> "" + getConfig().getGlobalDataUseMainMySQL()));
		}

		metrics.addCustomChart(
				new SimplePie("multi_proxy_support_enabled", () -> "" + getConfig().getMultiProxySupport()));

		if (!buildNumber.equals("NOTSET")) {
			metrics.addCustomChart(new SimplePie("dev_build_number", () -> "" + buildNumber));
		}

		logger.info("VotingPlugin velocity loaded, method: " + getVotingPluginProxy().getMethod().toString()
				+ ", PluginMessagingVersion: " + BungeeVersion.getPluginMessageVersion() + ", Internal Jar Version: "
				+ version);
		if (!buildNumber.equals("NOTSET")) {
			logger.info("Detected using dev build number: " + buildNumber);
		}
		getVotingPluginProxy().sendServerNameMessage();
	}

	public void reloadPlugin(boolean loadMysql) {
		config.load();
		if (loadMysql) {
			try {
				if (!config.getString(config.getNode("Host"), "").isEmpty()) {
					loadMysql();
				} else {
					logger.error("MySQL settings not set in bungeeconfig.yml");
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		getVotingPluginProxy().reload();
	}

	private void runAsyncNow(Runnable runnable) {
		server.getScheduler().buildTask(this, runnable).schedule();
	}

}
