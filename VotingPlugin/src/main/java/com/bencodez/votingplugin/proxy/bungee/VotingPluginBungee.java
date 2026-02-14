package com.bencodez.votingplugin.proxy.bungee;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
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

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigBungee;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

import lombok.Getter;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.TextComponent;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.connection.Server;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.api.scheduler.ScheduledTask;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.event.EventHandler;

/**
 * VotingPlugin proxy implementation for BungeeCord.
 * 
 * Supports a true hot reload (no proxy restart required) by:
 * <ul>
 * <li>Stopping the old {@link VotingPluginProxy} runtime</li>
 * <li>Reloading configuration</li>
 * <li>Re-registering the plugin message channel if it changed</li>
 * <li>Creating a fresh {@link VotingPluginProxy} instance (constructor-time
 * state refreshed)</li>
 * <li>Re-initializing caches and rescheduling tasks</li>
 * </ul>
 * 
 */
public class VotingPluginBungee extends Plugin implements Listener {

	@Getter
	private BungeeConfig config;

	private BungeeJsonNonVotedPlayersCache nonVotedPlayersCache;
	private BungeeJsonVoteCache voteCacheFile;

	private String buildNumber = "NOTSET";

	/**
	 * Votifier listener (registered only once).
	 */
	private VoteEventBungee voteEventBungee;

	@Getter
	private volatile VotingPluginProxy votingPluginProxy;

	@Getter
	private ScheduledExecutorService timer;

	private ScheduledTask voteCheckTask;
	private ScheduledTask cacheSaveTask;

	/**
	 * Reload synchronization lock.
	 */
	private final Object reloadLock = new Object();

	/**
	 * True while reload is in progress.
	 */
	private volatile boolean reloading = false;

	/**
	 * Plugin messages received during reload are queued and replayed after reload.
	 */
	private final Queue<byte[]> queuedPluginMessages = new ConcurrentLinkedQueue<byte[]>();

	/**
	 * Logs debug output if enabled in config.
	 *
	 * @param msg message to log
	 */
	public void debug(String msg) {
		if (config != null && config.getDebug()) {
			getLogger().info("Debug: " + msg);
		}
	}

	/**
	 * Alias for debug for existing call sites.
	 *
	 * @param msg message to log
	 */
	public void debug2(String msg) {
		debug(msg);
	}

	/**
	 * Gets servers eligible for proxy operations.
	 *
	 * @return server names
	 */
	public Set<String> getAvailableAllServers() {
		Set<String> servers = new HashSet<String>();
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

	/**
	 * Gets proper player name if online, otherwise returns stored name.
	 *
	 * @param uuid        uuid string
	 * @param currentName current name
	 * @return proper name if online
	 */
	public String getProperPlayerName(String uuid, String currentName) {
		ProxiedPlayer p = getProxy().getPlayer(UUID.fromString(uuid));
		if (p != null && p.isConnected()) {
			return p.getName();
		}
		return currentName;
	}

	/**
	 * Loads votingpluginversion.yml from jar if present.
	 *
	 * @return configuration or null
	 */
	private Configuration getVersionFile() {
		try {
			CodeSource src = this.getClass().getProtectionDomain().getCodeSource();
			if (src != null) {
				URL jar = src.getLocation();
				ZipInputStream zip = new ZipInputStream(jar.openStream());
				while (true) {
					ZipEntry e = zip.getNextEntry();
					if (e != null) {
						if ("votingpluginversion.yml".equals(e.getName())) {
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
			// ignore
		}
		return null;
	}

	/**
	 * Loads build number from embedded version file.
	 */
	public void loadVersionFile() {
		Configuration conf = getVersionFile();
		if (conf != null) {
			buildNumber = conf.getString("buildnumber", "NOTSET");
		}
	}

	@Override
	public void onEnable() {
		timer = Executors.newScheduledThreadPool(1);

		getProxy().getPluginManager().registerListener(this, this);

		config = new BungeeConfig(this);
		config.load();

		getProxy().getPluginManager().registerCommand(this, new VotingPluginBungeeCommand(this));

		// Ensure channel registered
		try {
			getProxy().registerChannel(config.getPluginMessageChannel());
		} catch (Exception ignored) {
		}

		// Create initial runtime (fresh instance)
		votingPluginProxy = createProxyRuntime();

		// Full init using the same pathway as reloadall
		reloadPlugin(true);

		loadVersionFile();
		getLogger().info("VotingPlugin loaded, using method: " + getVotingPluginProxy().getMethod().toString());
		if (!"NOTSET".equals(buildNumber)) {
			getLogger().info("Detected using dev build number: " + buildNumber);
		}
	}

	@Override
	public void onDisable() {
		synchronized (reloadLock) {
			reloading = true;

			cancelPlatformTasks();

			try {
				if (votingPluginProxy != null) {
					votingPluginProxy.onDisable();
				}
			} catch (Exception ignored) {
			}

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
				if (timer != null) {
					timer.shutdownNow();
				}
			} catch (Exception ignored) {
			}

			reloading = false;
		}

		getLogger().info("VotingPlugin disabled");
	}

	/**
	 * Handles plugin messages from backend servers.
	 *
	 * @param ev plugin message event
	 */
	@EventHandler
	public void onPluginMessage(PluginMessageEvent ev) {
		if (config == null) {
			return;
		}

		if (!ev.getTag().equalsIgnoreCase(getConfig().getPluginMessageChannel())) {
			return;
		}

		ev.setCancelled(true);

		if (!(ev.getSender() instanceof Server)) {
			debug("Ignore plugin message (not from server)");
			return;
		}

		// During reload, queue and replay later to avoid calling into disposed runtime
		if (reloading) {
			byte[] copy = new byte[ev.getData().length];
			System.arraycopy(ev.getData(), 0, copy, 0, ev.getData().length);
			queuedPluginMessages.add(copy);
			return;
		}

		handlePluginMessageBytes(ev.getData());
	}

	/**
	 * Hot-reloads the proxy runtime.
	 *
	 * <p>
	 * Two modes:
	 * </p>
	 * <ul>
	 * <li><b>Soft reload</b> ({@code loadMysql=false}): keeps the existing
	 * {@link VotingPluginProxy} instance (and its MySQL pool) alive. Only reloads
	 * config/channel and applies runtime-only changes via
	 * {@link VotingPluginProxy#reload()}.</li>
	 * <li><b>Full reload</b> ({@code loadMysql=true}): fully tears down the old
	 * runtime (including MySQL), recreates the proxy instance, reconnects MySQL
	 * from config, then calls
	 * {@link VotingPluginProxy#load(com.bencodez.votingplugin.proxy.cache.IVoteCache, com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage)}.</li>
	 * </ul>
	 *
	 * @param loadMysql true to fully reinitialize MySQL and rebuild the proxy
	 *                  runtime, false for a config/runtime-only reload
	 */
	public void reloadPlugin(boolean loadMysql) {
		synchronized (reloadLock) {
			reloading = true;

			final String oldChannel = (config != null) ? config.getPluginMessageChannel() : null;

			// Always stop platform tasks while we touch state
			cancelPlatformTasks();

			// Always reload config first
			try {
				config.load();
			} catch (Exception e) {
				getLogger().severe("Failed to reload bungeeconfig.yml");
				e.printStackTrace();
			}

			// Update channel registration if changed (safe for both modes)
			try {
				final String newChannel = config.getPluginMessageChannel();
				if (oldChannel != null && newChannel != null && !oldChannel.equalsIgnoreCase(newChannel)) {
					try {
						getProxy().unregisterChannel(oldChannel);
					} catch (Exception ignored) {
					}
				}
				try {
					if (newChannel != null && !newChannel.isEmpty()) {
						getProxy().registerChannel(newChannel);
					}
				} catch (Exception ignored) {
				}
			} catch (Exception e) {
				// keep going; channel mismatch should not kill reload
				e.printStackTrace();
			}

			// =========================
			// SOFT RELOAD (NO MYSQL)
			// =========================
			if (!loadMysql) {
				try {
					if (votingPluginProxy != null) {
						// Applies runtime-only config (method selection, multiproxy settings, vote
						// party settings, etc.)
						votingPluginProxy.reload();
					}
				} catch (Throwable t) {
					getLogger().severe("Error while applying soft reload");
					t.printStackTrace();
				}

				// Restart tasks (they rely on already-loaded handlers on the existing proxy
				// instance)
				try {
					schedulePlatformTasks();
				} catch (Exception e) {
					e.printStackTrace();
				}

				reloading = false;
				return;
			}

			// =========================
			// FULL RELOAD (WITH MYSQL)
			// =========================

			// Save caches best-effort before teardown
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

			// Tear down old runtime (this WILL shutdown MySQL because proxy owns it)
			try {
				if (votingPluginProxy != null) {
					votingPluginProxy.onDisable();
				}
			} catch (Exception ignored) {
			}

			// Recreate runtime
			votingPluginProxy = createProxyRuntime();

			// Initialize MySQL BEFORE calling proxy.load(...)
			try {
				if (config.hasDatabaseConfigured()) {
					votingPluginProxy.loadMysql(getMysqlConfig(), getGlobalDataMysqlConfig());
				} else {
					getLogger().severe("MySQL settings not set in bungeeconfig.yml");
					votingPluginProxy.setProxyMySQL(null);
				}
			} catch (Throwable t) {
				getLogger().severe("Failed to initialize MySQL during reload");
				t.printStackTrace();
				votingPluginProxy.setProxyMySQL(null);
			}

			// Abort cleanly if MySQL did not initialize (prevents NPE inside
			// VotingPluginProxy.load)
			if (votingPluginProxy.getProxyMySQL() == null) {
				getLogger().severe("Reload aborted: Proxy MySQL is not initialized (see logs above).");
				reloading = false;
				return;
			}

			// Ensure caches exist and load into runtime
			try {
				if (voteCacheFile == null) {
					voteCacheFile = new BungeeJsonVoteCache(this);
				}
				if (nonVotedPlayersCache == null) {
					nonVotedPlayersCache = new BungeeJsonNonVotedPlayersCache(
							new File(getDataFolder(), "nonvotedplayerscache.json"));
				}

				// Load proxy state (requires MySQL)
				votingPluginProxy.load(voteCacheFile, nonVotedPlayersCache);

				// Apply runtime-only config too
				votingPluginProxy.reload();
			} catch (Throwable t) {
				getLogger().severe("Reload aborted while loading proxy state");
				t.printStackTrace();
				reloading = false;
				return;
			}

			// Restart tasks after a successful swap
			try {
				schedulePlatformTasks();
			} catch (Exception e) {
				e.printStackTrace();
			}

			reloading = false;
		}

		// Replay queued plugin messages after swap
		drainQueuedPluginMessages();

		initVotifierListenerIfNeeded();

		// Send server name message again (safe)
		try {
			getVotingPluginProxy().sendServerNameMessage();
		} catch (Exception ignored) {
		}
	}

	/**
	 * Creates a new VotingPluginProxy instance wired to this platform.
	 *
	 * @return new proxy runtime
	 */
	private VotingPluginProxy createProxyRuntime() {
		return new VotingPluginProxy() {

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
				ProxiedPlayer p = getProxy().getPlayer(player);
				if (p != null && p.getServer() != null) {
					return p.getServer().getInfo().getName();
				}
				return "";
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
				if (playerName == null || playerName.isEmpty() || "null".equalsIgnoreCase(playerName)) {
					return "";
				}

				if (!config.getOnlineMode()) {
					return UUID.nameUUIDFromBytes(("OfflinePlayer:" + playerName.toLowerCase(Locale.ROOT).trim())
							.getBytes(java.nio.charset.StandardCharsets.UTF_8)).toString();
				}

				ProxiedPlayer p = getProxy().getPlayer(playerName);
				if (p != null && p.isConnected()) {
					playerName = p.getName();
				}

				for (Entry<UUID, String> entry : getVotingPluginProxy().getUuidPlayerNameCache().entrySet()) {
					String cachedName = entry.getValue();
					if (cachedName != null && cachedName.equalsIgnoreCase(playerName)) {
						playerName = cachedName;
						break;
					}
				}

				if (p != null && p.isConnected()) {
					return p.getUniqueId().toString();
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
				return getProxy().getServerInfo(server) != null
						&& !getProxy().getServerInfo(server).getPlayers().isEmpty();
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
				if (getProxy().getServerInfo(server) != null) {
					getProxy().getServerInfo(server).sendData(channel, data, queue);
				}
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
				// mysql==true should do full reloadall behavior on the platform
				reloadPlugin(mysql);
			}

			@Override
			public ScheduledExecutorService getScheduler() {
				return timer;
			}

			@Override
			public MysqlConfig getVoteCacheMySQLConfig() {
				return new MysqlConfigBungee(config.getData().getSection("VoteCache"));
			}

			@Override
			public MysqlConfig getNonVotedCacheMySQLConfig() {
				return new MysqlConfigBungee(config.getData().getSection("NonVotedCache"));
			}

			@Override
			public MysqlConfig getVoteLoggingMySQLConfig() {
				return new MysqlConfigBungee(config.getData().getSection("VoteLogging"));
			}

			@Override
			public void loadTaskTimer(Runnable runnable, long delaySeconds, long repeatSeconds) {
				timer.scheduleAtFixedRate(runnable, delaySeconds, repeatSeconds, TimeUnit.SECONDS);
			}

		};
	}

	/**
	 * Gets the MySQL configuration for Bungee.
	 *
	 * <p>
	 * Prefers the "Database" section if it exists and contains keys; otherwise
	 * falls back to legacy root keys.
	 * </p>
	 *
	 * @return mysql config (never null)
	 */
	public MysqlConfig getMysqlConfig() {
		Configuration data = config.getData();
		Configuration db = config.sectionOrNull(data, "Database");

		if (isMissingOrEmptySection(db)) {
			// legacy root keys
			return new MysqlConfigBungee(data);
		}

		return new MysqlConfigBungee(db);
	}

	/**
	 * Checks whether a configuration section is missing or empty.
	 *
	 * @param section config section (may be null)
	 * @return true if null or contains no keys
	 */
	private static boolean isMissingOrEmptySection(Configuration section) {
		if (section == null) {
			return true;
		}
		return section.getKeys() == null || section.getKeys().isEmpty();
	}

	public MysqlConfig getGlobalDataMysqlConfig() {
		return new MysqlConfigBungee(config.getData().getSection("GlobalData"));
	}

	/**
	 * Initializes votifier listener if enabled and available. Registers the
	 * listener only once.
	 */
	private void initVotifierListenerIfNeeded() {
		boolean eventFound = true;
		try {
			Class.forName("com.vexsoftware.votifier.bungee.events.VotifierEvent");
		} catch (ClassNotFoundException e) {
			eventFound = false;
		}

		if (!eventFound) {
			getVotingPluginProxy().setVotifierEnabled(false);
			return;
		}

		if (getVotingPluginProxy().isVotifierEnabled()) {
			if (voteEventBungee == null) {
				try {
					voteEventBungee = new VoteEventBungee(this);
					getProxy().getPluginManager().registerListener(this, voteEventBungee);
				} catch (Exception e) {
					getVotingPluginProxy().setVotifierEnabled(false);
				}
			}
		}
	}

	/**
	 * Cancels Bungee scheduled tasks owned by this plugin.
	 */
	private void cancelPlatformTasks() {
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

	/**
	 * Schedules platform periodic tasks.
	 */
	private void schedulePlatformTasks() {
		voteCheckTask = getProxy().getScheduler().schedule(this, new Runnable() {
			@Override
			public void run() {
				for (String server : getVotingPluginProxy().getVoteCacheHandler().getCachedVotesServers()) {
					getVotingPluginProxy().checkCachedVotes(server);
				}
				for (ProxiedPlayer player : getProxy().getPlayers()) {
					getVotingPluginProxy().checkOnlineVotes(player.getName(), player.getUniqueId().toString(), null);
				}
			}
		}, 120L, 60L, TimeUnit.SECONDS);

		cacheSaveTask = getProxy().getScheduler().schedule(this, new Runnable() {
			@Override
			public void run() {
				if (nonVotedPlayersCache != null) {
					debug("Checking nonvotedplayerscache...");
					getVotingPluginProxy().getNonVotedPlayersCache().check();
				}
				if (voteCacheFile != null) {
					voteCacheFile.save();
				}
			}
		}, 1L, 60L, TimeUnit.MINUTES);
	}

	/**
	 * Runs a task asynchronously on the Bungee scheduler.
	 *
	 * @param runnable task
	 */
	private void runAsyncNow(Runnable runnable) {
		getProxy().getScheduler().runAsync(this, runnable);
	}

	/**
	 * Parses and handles a plugin message payload.
	 *
	 * @param data payload bytes
	 */
	private void handlePluginMessageBytes(byte[] data) {
		ByteArrayInputStream instream = new ByteArrayInputStream(data);
		DataInputStream in = new DataInputStream(instream);
		try {
			getVotingPluginProxy().onPluginMessageReceived(in);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Replays queued plugin messages after reload completes.
	 */
	private void drainQueuedPluginMessages() {
		byte[] msg;
		while ((msg = queuedPluginMessages.poll()) != null) {
			try {
				handlePluginMessageBytes(msg);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
}
