package com.bencodez.votingplugin.proxy.velocity;

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
import java.util.HashSet;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.bstats.charts.SimplePie;
import org.bstats.velocity.Metrics;
import org.slf4j.Logger;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.simpleapi.file.velocity.VelocityYMLFile;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigVelocity;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
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
import com.velocitypowered.api.scheduler.ScheduledTask;

import lombok.Getter;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

/**
 * VotingPlugin proxy implementation for Velocity.
 * <p>
 * Reload behavior:
 * <ul>
 * <li><b>/vpp reload</b> = small reload (config.reload +
 * votingPluginProxy.reload)</li>
 * <li><b>/vpp reloadall</b> = full reload (rebuild runtime, reload mysql,
 * reload caches/tasks)</li>
 * </ul>
 * </p>
 */
@Plugin(id = "votingplugin", name = "VotingPlugin", version = "1.0", url = "https://www.spigotmc.org/resources/votingplugin.15358/", description = "VotingPlugin Velocity Version", authors = {
		"BenCodez" }, dependencies = { @Dependency(id = "nuvotifier", optional = true) })
public class VotingPluginVelocity {

	/**
	 * Current plugin message channel.
	 */
	private volatile ChannelIdentifier channel;

	@Getter
	private VelocityConfig config;

	private final Path dataDirectory;

	@Getter
	private final Logger logger;

	private final Metrics.Factory metricsFactory;

	private final ProxyServer server;

	private String version = "";
	private String buildNumber = "NOTSET";
	private File versionFile;

	@Getter
	private ScheduledExecutorService timer;

	@Getter
	private volatile VotingPluginProxy votingPluginProxy;

	private VelocityJsonVoteCache voteCacheFile;
	private VelocityJsonNonVotedPlayersCache nonVotedPlayersCache;

	private ScheduledTask voteCheckTask;
	private ScheduledTask cacheSaveTask;

	/**
	 * Register votifier listener only once.
	 */
	private VoteEventVelocity voteEventVelocity;

	/**
	 * Reload lock.
	 */
	private final Object reloadLock = new Object();

	/**
	 * True while reloadall is happening.
	 */
	private volatile boolean reloading = false;

	/**
	 * Plugin messages received during reload are queued.
	 */
	private final Queue<byte[]> queuedPluginMessages = new ConcurrentLinkedQueue<byte[]>();

	@Inject
	public VotingPluginVelocity(ProxyServer server, Logger logger, Metrics.Factory metricsFactory,
			@DataDirectory Path dataDirectory) {
		this.server = server;
		this.logger = logger;
		this.dataDirectory = dataDirectory;
		this.metricsFactory = metricsFactory;
		this.timer = Executors.newScheduledThreadPool(1);
	}

	/**
	 * Debug logger.
	 *
	 * @param msg message
	 */
	public void debug(String msg) {
		if (config != null && config.getDebug()) {
			logger.info("Debug: " + msg);
		}
	}

	/**
	 * Alias used by older code.
	 *
	 * @param msg message
	 */
	public void debug2(String msg) {
		debug(msg);
	}

	/**
	 * Get list of eligible servers.
	 *
	 * @return servers
	 */
	public Set<String> getAvailableAllServers() {
		Set<String> servers = new HashSet<String>();
		if (config.getWhiteListedServers().isEmpty()) {
			for (RegisteredServer s : server.getAllServers()) {
				String name = s.getServerInfo().getName();
				if (!config.getBlockedServers().contains(name)) {
					servers.add(name);
				}
			}
		} else {
			for (RegisteredServer s : server.getAllServers()) {
				String name = s.getServerInfo().getName();
				if (config.getWhiteListedServers().contains(name)) {
					servers.add(name);
				}
			}
		}
		return servers;
	}

	/**
	 * Prefer online name if possible.
	 *
	 * @param uuid        uuid string
	 * @param currentName current stored name
	 * @return name
	 */
	public String getProperPlayerName(String uuid, String currentName) {
		try {
			UUID id = UUID.fromString(uuid);
			if (server.getPlayer(id).isPresent()) {
				Player p = server.getPlayer(id).get();
				if (p != null && p.isActive()) {
					return p.getUsername();
				}
			}
		} catch (Exception ignored) {
		}
		return currentName;
	}

	/**
	 * Extracts internal version file.
	 */
	private void getVersionFile() {
		try {
			CodeSource src = this.getClass().getProtectionDomain().getCodeSource();
			if (src != null) {
				URL jar = src.getLocation();
				ZipInputStream zip = new ZipInputStream(jar.openStream());
				while (true) {
					ZipEntry e = zip.getNextEntry();
					if (e == null) {
						break;
					}
					if ("votingpluginversion.yml".equals(e.getName())) {
						Reader defConfigStream = new InputStreamReader(zip, StandardCharsets.UTF_8);
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
						defConfigStream.close();

						YamlConfigurationLoader loader = YamlConfigurationLoader.builder().path(versionFile.toPath())
								.build();
						ConfigurationNode node = loader.load();
						if (node != null) {
							version = node.node("version").getString("");
							buildNumber = node.node("buildnumber").getString("NOTSET");
						}
						return;
					}
				}
			}
		} catch (Exception e) {
			if (config != null && config.getDebug()) {
				e.printStackTrace();
			}
		}
	}

	@Subscribe
	public void onPluginMessagingReceived(PluginMessageEvent event) {
		ChannelIdentifier ch = channel;
		if (ch == null) {
			return;
		}
		if (!event.getIdentifier().equals(ch)) {
			return;
		}

		event.setResult(PluginMessageEvent.ForwardResult.handled());

		if (!(event.getSource() instanceof ServerConnection)) {
			return;
		}

		if (reloading) {
			byte[] copy = new byte[event.getData().length];
			System.arraycopy(event.getData(), 0, copy, 0, event.getData().length);
			queuedPluginMessages.add(copy);
			return;
		}

		handlePluginMessageBytes(event.getData());
	}

	@Subscribe
	public void onProxyDisable(ProxyShutdownEvent event) {
		synchronized (reloadLock) {
			reloading = true;

			cancelTasks();

			try {
				if (voteCacheFile != null) {
					voteCacheFile.save();
				}
			} catch (Exception ignored) {
			}
			try {
				if (nonVotedPlayersCache != null) {
					nonVotedPlayersCache.save();
				}
			} catch (Exception ignored) {
			}

			try {
				if (votingPluginProxy != null) {
					votingPluginProxy.onDisable();
				}
			} catch (Exception ignored) {
			}

			try {
				if (timer != null) {
					timer.shutdownNow();
				}
			} catch (Exception ignored) {
			}

			reloading = false;
		}

		logger.info("VotingPlugin disabled");
	}

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
			if (toCopyStream != null) {
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
		}

		config = new VelocityConfig(configFile);

		channel = buildChannelIdentifier(config.getPluginMessageChannel());
		server.getChannelRegistrar().register(channel);

		CommandMeta meta = server.getCommandManager().metaBuilder("votingpluginproxy").aliases("vpp").build();
		server.getCommandManager().register(meta, new VotingPluginVelocityCommand(this));

		// create runtime
		votingPluginProxy = createProxyRuntime();

		// full init
		reloadAllInternal(true);

		// version
		try {
			getVersionFile();
			if (versionFile != null) {
				versionFile.delete();
				if (versionFile.getParentFile() != null) {
					versionFile.getParentFile().delete();
				}
			}
		} catch (Exception ignored) {
		}

		// metrics (same as your original, shortened)
		Metrics metrics = metricsFactory.make(this, 11547);
		metrics.addCustomChart(new SimplePie("bungee_method", () -> getConfig().getBungeeMethod().toString()));
		metrics.addCustomChart(new SimplePie("config_onlinemode", () -> "" + getConfig().getOnlineMode()));
		metrics.addCustomChart(new SimplePie("sendtoallservers", () -> "" + getConfig().getSendVotesToAllServers()));
		metrics.addCustomChart(new SimplePie("allowunjoined", () -> "" + getConfig().getAllowUnJoined()));
		metrics.addCustomChart(new SimplePie("pointsonvote", () -> "" + getConfig().getPointsOnVote()));
		metrics.addCustomChart(new SimplePie("bungeemanagetotals", () -> "" + getConfig().getBungeeManageTotals()));
		metrics.addCustomChart(new SimplePie("waitforuseronline", () -> "" + getConfig().getWaitForUserOnline()));
		metrics.addCustomChart(new SimplePie("plugin_version", () -> "" + version));

		logger.info("VotingPlugin velocity loaded, method: " + getVotingPluginProxy().getMethod().toString()
				+ ", Internal Jar Version: " + version);
		if (!"NOTSET".equals(buildNumber)) {
			logger.info("Detected using dev build number: " + buildNumber);
		}
	}

	/**
	 * Reloads VotingPluginProxy on Velocity.
	 *
	 * <p>
	 * Two modes:
	 * </p>
	 * <ul>
	 * <li><b>Full reload</b> ({@code loadMysql=true}): shuts down the old runtime,
	 * recreates the proxy, reconnects MySQL, then loads caches and handlers.</li>
	 * <li><b>Soft reload</b> ({@code loadMysql=false}): does NOT recreate the proxy
	 * instance (because it owns MySQL). Only reloads config and applies
	 * runtime-only settings via {@link VotingPluginProxy#reload()}.</li>
	 * </ul>
	 *
	 * @param loadMysql whether to rebuild MySQL and fully reinitialize the proxy
	 */
	public void reloadAllInternal(boolean loadMysql) {
		synchronized (reloadLock) {
			reloading = true;

			// Always stop repeating tasks while we touch state
			cancelTasks();

			// Always reload config first
			try {
				config.reload(); // or config.load() - whichever your VelocityConfig provides
			} catch (Exception e) {
				logger.error("Failed to reload bungeeconfig.yml", e);
			}

			// Update plugin message channel (safe for both modes)
			try {
				ChannelIdentifier old = channel;
				ChannelIdentifier next = buildChannelIdentifier(config.getPluginMessageChannel());

				if (old != null && !old.equals(next)) {
					try {
						server.getChannelRegistrar().unregister(old);
					} catch (Exception ignored) {
					}
				}

				try {
					server.getChannelRegistrar().register(next);
				} catch (Exception ignored) {
				}

				channel = next;
			} catch (Exception e) {
				logger.error("Failed to update plugin message channel", e);
			}

			// =========================
			// SOFT RELOAD (NO MYSQL)
			// =========================
			if (!loadMysql) {
				// We intentionally do NOT call onDisable()/shutdown, because that would stop
				// MySQL.
				// We also do NOT recreate the proxy instance, because it owns the MySQL
				// pool/table.
				try {
					if (votingPluginProxy != null) {
						votingPluginProxy.reload();
					}
				} catch (Throwable t) {
					logger.error("Error while applying soft reload", t);
				}

				// Restart tasks that don't depend on rebuilding the proxy
				scheduleTasks();

				reloading = false;
			} else {

				// =========================
				// FULL RELOAD (WITH MYSQL)
				// =========================

				// Best-effort save caches before shutdown
				try {
					if (voteCacheFile != null) {
						voteCacheFile.save();
					}
				} catch (Exception ignored) {
				}
				try {
					if (nonVotedPlayersCache != null) {
						nonVotedPlayersCache.save();
					}
				} catch (Exception ignored) {
				}

				// Shutdown old runtime completely (this stops old MySQL pool)
				try {
					if (votingPluginProxy != null) {
						votingPluginProxy.onDisable();
					}
				} catch (Exception ignored) {
				}

				// Recreate runtime
				votingPluginProxy = createProxyRuntime();

				// Init MySQL BEFORE calling proxy.load(...)
				try {
					if (config.hasDatabaseConfigured()) {
						votingPluginProxy.loadMysql(getMysqlConfig(), getGlobalDataMysqlConfig());
					} else {
						logger.error("MySQL settings not set in bungeeconfig.yml");
						votingPluginProxy.setProxyMySQL(null);
					}
				} catch (Throwable t) {
					logger.error("Failed to initialize MySQL during reload", t);
					votingPluginProxy.setProxyMySQL(null);
				}

				// If MySQL is required (your proxy.load assumes it), abort cleanly
				if (votingPluginProxy.getProxyMySQL() == null) {
					logger.error("Reload aborted: Proxy MySQL is not initialized.");
					reloading = false;
					return;
				}

				// Recreate storages (or keep existing if you prefer)
				voteCacheFile = new VelocityJsonVoteCache(new File(dataDirectory.toFile(), "votecache.json"));
				nonVotedPlayersCache = new VelocityJsonNonVotedPlayersCache(
						new File(dataDirectory.toFile(), "nonvotedplayerscache.json"));

				convertYamlCachesIfPresent();

				// Load proxy state (requires MySQL)
				try {
					votingPluginProxy.load(voteCacheFile, nonVotedPlayersCache);
				} catch (Throwable t) {
					logger.error("Reload aborted while loading proxy state", t);
					reloading = false;
					return;
				}

				// Restart tasks
				scheduleTasks();

				// Apply runtime-only config
				try {
					votingPluginProxy.reload();
				} catch (Throwable t) {
					logger.error("Error while reloading proxy internals", t);
				}

				reloading = false;
			}
		}

		// Out of lock: flush queued messages (optional)
		drainQueuedPluginMessages();

		initVotifierListenerIfNeeded();

		// Optional: re-announce server names
		try {
			if (votingPluginProxy != null) {
				votingPluginProxy.sendServerNameMessage();
			}
		} catch (Exception ignored) {
		}
	}

	/**
	 * Create proxy runtime.
	 *
	 * @return runtime
	 */
	private VotingPluginProxy createProxyRuntime() {
		return new VotingPluginProxy() {

			@Override
			public void broadcast(String message) {
				server.getAllPlayers().forEach(
						player -> player.sendMessage(LegacyComponentSerializer.legacyAmpersand().deserialize(message)));
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
					Player p = server.getPlayer(player).get();
					if (p.getCurrentServer().isPresent()) {
						return p.getCurrentServer().get().getServer().getServerInfo().getName();
					}
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
				if (playerName == null || playerName.isEmpty() || "null".equalsIgnoreCase(playerName)) {
					return "";
				}

				if (!config.getOnlineMode()) {
					return UUID.nameUUIDFromBytes(("OfflinePlayer:" + playerName.toLowerCase(Locale.ROOT).trim())
							.getBytes(StandardCharsets.UTF_8)).toString();
				}

				if (server.getPlayer(playerName).isPresent()) {
					Player p = server.getPlayer(playerName).get();
					if (p != null && p.isActive()) {
						playerName = p.getUsername();
					}
				}

				for (Entry<UUID, String> entry : getVotingPluginProxy().getUuidPlayerNameCache().entrySet()) {
					if (entry.getValue() != null && entry.getValue().equalsIgnoreCase(playerName)) {
						playerName = entry.getValue();
						break;
					}
				}

				if (server.getPlayer(playerName).isPresent()) {
					Player p = server.getPlayer(playerName).get();
					if (p != null && p.isActive()) {
						return p.getUniqueId().toString();
					}
				}

				for (Entry<UUID, String> entry : getVotingPluginProxy().getUuidPlayerNameCache().entrySet()) {
					if (entry.getValue() != null && entry.getValue().equalsIgnoreCase(playerName)) {
						return entry.getKey().toString();
					}
				}

				if (getVotingPluginProxy().getProxyMySQL() != null) {
					String str = getVotingPluginProxy().getProxyMySQL().getUUID(playerName);
					if (str != null) {
						return str;
					}
				}

				return getVotingPluginProxy().getNonVotedPlayersCache().getUUID(playerName);
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
			public boolean isVoteCacheIgnoreTime() {
				return voteCacheFile.getNode("Time", "IgnoreTime").getBoolean();
			}

			@Override
			public void setVoteCacheLastUpdated() {
				voteCacheFile.set(new Object[] { "Time", "LastUpdated" }, System.currentTimeMillis());
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevDay(int day) {
				voteCacheFile.set(new Object[] { "Time", "Day" }, day);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevMonth(String text) {
				voteCacheFile.set(new Object[] { "Time", "Month" }, text);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCachePrevWeek(int week) {
				voteCacheFile.set(new Object[] { "Time", "Week" }, week);
				voteCacheFile.save();
			}

			@Override
			public void setVoteCacheVoteCacheIgnoreTime(boolean ignore) {
				voteCacheFile.set(new Object[] { "Time", "IgnoreTime" }, ignore);
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
			public boolean isPlayerOnline(String playerName) {
				if (playerName == null) {
					return false;
				}
				return server.getPlayer(playerName).map(Player::isActive).orElse(false);
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
			public void log(String message) {
				logger.info(message);
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
			public void sendPluginMessageData(String serverName, String channelName, byte[] data, boolean queue) {
				if (!server.getServer(serverName).isPresent()) {
					return;
				}
				RegisteredServer send = server.getServer(serverName).get();
				send.sendPluginMessage(channel, data);
			}

			@Override
			public void warn(String message) {
				logger.warn(message);
			}

			@Override
			public void reloadCore(boolean mysql) {
				// mysql=true is reloadall behavior
				reloadAllInternal(mysql);
			}

			@Override
			public ScheduledExecutorService getScheduler() {
				return timer;
			}

			@Override
			public MysqlConfig getVoteCacheMySQLConfig() {
				return new MysqlConfigVelocity("VoteCache", config);
			}

			@Override
			public MysqlConfig getNonVotedCacheMySQLConfig() {
				return new MysqlConfigVelocity("NonVotedCache", config);
			}

			@Override
			public MysqlConfig getVoteLoggingMySQLConfig() {
				return new MysqlConfigVelocity("VoteLogging", config);
			}

			@Override
			public void loadTaskTimer(Runnable runnable, long delaySeconds, long repeatSeconds) {
				timer.scheduleAtFixedRate(runnable, delaySeconds, repeatSeconds, TimeUnit.SECONDS);
			}
		};
	}

	/**
	 * Gets the MySQL configuration.
	 *
	 * <p>
	 * Prefers the "Database" section if it exists and is usable. If the "Database"
	 * section is missing, not a map, or has no keys, falls back to legacy/root
	 * configuration parsing.
	 * </p>
	 *
	 * @return mysql config (never null)
	 */
	public MysqlConfig getMysqlConfig() {
		final ConfigurationNode root = config.getData();
		final ConfigurationNode db = root.node("Database");

		// If Database section doesn't exist (virtual), isn't a section/map, or is empty
		// -> fallback
		if (isMissingOrEmptySection(db)) {
			return new MysqlConfigVelocity(config);
		}

		return new MysqlConfigVelocity("Database", config);
	}

	/**
	 * Checks whether a configuration node represents a missing or empty section.
	 *
	 * @param node configuration node
	 * @return true if the node is missing/virtual, not a map/section, or contains
	 *         no keys
	 */
	private boolean isMissingOrEmptySection(ConfigurationNode node) {
		if (node == null) {
			return true;
		}

		// "virtual" typically means it doesn't exist in the file
		try {
			if (node.virtual()) {
				return true;
			}
		} catch (Throwable ignored) {
			// Some shaded/older configurate nodes may not expose virtual() consistently.
		}

		// If it's not a map/section, it's not a usable "Database:" block
		if (!node.isMap()) {
			return true;
		}

		// Treat an empty map (Database: with no keys) as missing -> fallback
		return node.childrenMap() == null || node.childrenMap().isEmpty();
	}

	public MysqlConfig getGlobalDataMysqlConfig() {
		return new MysqlConfigVelocity("GlobalData", config);
	}

	/**
	 * Register votifier listener once.
	 */
	private void initVotifierListenerIfNeeded() {
		try {
			Class.forName("com.vexsoftware.votifier.velocity.event.VotifierEvent");
		} catch (ClassNotFoundException e) {
			getVotingPluginProxy().setVotifierEnabled(false);
			return;
		}

		if (getVotingPluginProxy().isVotifierEnabled()) {
			if (voteEventVelocity == null) {
				try {
					voteEventVelocity = new VoteEventVelocity(this);
					server.getEventManager().register(this, voteEventVelocity);
				} catch (Exception e) {
					getVotingPluginProxy().setVotifierEnabled(false);
				}
			}
		}
	}

	private void cancelTasks() {
		try {
			if (voteCheckTask != null) {
				voteCheckTask.cancel();
				voteCheckTask = null;
			}
		} catch (Exception ignored) {
		}
		try {
			if (cacheSaveTask != null) {
				cacheSaveTask.cancel();
				cacheSaveTask = null;
			}
		} catch (Exception ignored) {
		}
	}

	private void scheduleTasks() {
		voteCheckTask = server.getScheduler().buildTask(this, () -> {
			if (getVotingPluginProxy().getGlobalDataHandler() == null
					|| !getVotingPluginProxy().getGlobalDataHandler().isTimeChangedHappened()) {

				for (String srv : getVotingPluginProxy().getVoteCacheHandler().getCachedVotesServers()) {
					getVotingPluginProxy().checkCachedVotes(srv);
				}

				for (Player player : server.getAllPlayers()) {
					getVotingPluginProxy().checkOnlineVotes(player.getUsername(), player.getUniqueId().toString(),
							null);
				}
			}
		}).delay(120, TimeUnit.SECONDS).repeat(60, TimeUnit.SECONDS).schedule();

		cacheSaveTask = server.getScheduler().buildTask(this, () -> {
			if (nonVotedPlayersCache != null) {
				debug("Checking nonvotedplayerscache...");
				getVotingPluginProxy().getNonVotedPlayersCache().check();
			}
			if (voteCacheFile != null) {
				voteCacheFile.save();
			}
		}).delay(1L, TimeUnit.MINUTES).repeat(60L, TimeUnit.MINUTES).schedule();
	}

	private void runAsyncNow(Runnable runnable) {
		server.getScheduler().buildTask(this, runnable).schedule();
	}

	private void handlePluginMessageBytes(byte[] data) {
		ByteArrayInputStream instream = new ByteArrayInputStream(data);
		DataInputStream in = new DataInputStream(instream);
		try {
			getVotingPluginProxy().onPluginMessageReceived(in);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void drainQueuedPluginMessages() {
		byte[] msg;
		while ((msg = queuedPluginMessages.poll()) != null) {
			handlePluginMessageBytes(msg);
		}
	}

	/**
	 * Convert old YAML cache files to JSON.
	 */
	private void convertYamlCachesIfPresent() {
		// vote cache
		File yamlVoteCacheFile = new File(dataDirectory.toFile(), "votecache.yml");
		if (yamlVoteCacheFile.exists()) {
			VelocityYMLFile yamlVoteCache = new VelocityYMLFile(yamlVoteCacheFile);
			voteCacheFile.setConf(yamlVoteCache.getData());
			yamlVoteCacheFile.renameTo(new File(dataDirectory.toFile(), "oldvotecache.yml"));
			voteCacheFile.save();
		}

		// non-voted cache (FIX: write to nonVotedPlayersCache, not voteCacheFile)
		File yamlNonVotedFile = new File(dataDirectory.toFile(), "nonvotedplayerscache.yml");
		if (yamlNonVotedFile.exists()) {
			VelocityYMLFile yamlNonVoted = new VelocityYMLFile(yamlNonVotedFile);
			nonVotedPlayersCache.setConf(yamlNonVoted.getData());
			yamlNonVotedFile.renameTo(new File(dataDirectory.toFile(), "oldnonvotedplayerscache.yml"));
			nonVotedPlayersCache.save();
		}
	}

	private MinecraftChannelIdentifier buildChannelIdentifier(String raw) {
		String[] parts = raw.split(":");
		return MinecraftChannelIdentifier.create(parts[0].toLowerCase(Locale.ROOT), parts[1].toLowerCase(Locale.ROOT));
	}
}
