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
import java.util.Collection;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
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
import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteQueue;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteJournal;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.ProxyRuntimeReplacementLifecycle;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.VotingPluginProxy.VoteRetryException;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
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
 * 
 * Reload behavior:
 * <ul>
 * <li><b>/vpp reload</b> = small reload (config.reload +
 * votingPluginProxy.reload)</li>
 * <li><b>/vpp reloadall</b> = full reload (rebuild runtime, reload mysql,
 * reload caches/tasks)</li>
 * </ul>
 * 
 */
@Plugin(id = "votingplugin", name = "VotingPlugin", version = "1.0", url = "https://www.spigotmc.org/resources/votingplugin.15358/", description = "VotingPlugin Velocity Version", authors = {
		"BenCodez" }, dependencies = { @Dependency(id = "nuvotifier", optional = true), @Dependency(id = "mysqldriver", optional = true) })
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
	 * True only after the proxy runtime and required Votifier listener are ready.
	 */
	private volatile boolean runtimeOperational = false;
	/** True once shared runtime state loaded, even if platform tasks need a retry. */
	private volatile boolean runtimeInitialized = false;

	/**
	 * Plugin messages received during reload are queued.
	 */
	private final Queue<QueuedPluginMessage> queuedPluginMessages = new ConcurrentLinkedQueue<>();
	private final PendingIncomingVoteQueue pendingIncomingVotes = new PendingIncomingVoteQueue();
	private final PendingIncomingVoteJournal pendingIncomingVoteJournal;
	private final PendingIncomingVoteJournal pendingIncomingVoteRescueJournal;

	@Inject
	public VotingPluginVelocity(ProxyServer server, Logger logger, Metrics.Factory metricsFactory,
			@DataDirectory Path dataDirectory) {
		this.server = server;
		this.logger = logger;
		this.dataDirectory = dataDirectory;
		this.metricsFactory = metricsFactory;
		this.pendingIncomingVoteJournal = new PendingIncomingVoteJournal(dataDirectory);
		this.pendingIncomingVoteRescueJournal = PendingIncomingVoteJournal.rescue(dataDirectory);
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
		String sourceServer = ((ServerConnection) event.getSource()).getServerInfo().getName();

		if (reloading) {
			queuePluginMessage(sourceServer, event.getData());
			return;
		}
		synchronized (reloadLock) {
			if (reloading) {
				queuePluginMessage(sourceServer, event.getData());
				return;
			}
			if (!runtimeOperational || votingPluginProxy == null) {
				logger.error("Plugin message received while VotingPlugin proxy runtime is not operational; message was not processed");
				return;
			}
			handlePluginMessageBytes(sourceServer, event.getData());
		}
	}

	@Subscribe
	public void onProxyDisable(ProxyShutdownEvent event) {
		synchronized (reloadLock) {
			pendingIncomingVotes.closeAdmission();
			// Shutdown is terminal. Do not schedule reload retries against a timer that
			// is being stopped.
			reloading = false;
			runtimeOperational = false;
			runtimeInitialized = false;
			if (!persistPendingIncomingVotes(votingPluginProxy, "proxy shutdown")) {
				logger.error("Proxy shutdown cannot safely continue because accepted votes could not be journaled");
				return;
			}

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

		}

		logger.info("VotingPlugin disabled");
	}

	@Subscribe
	public com.velocitypowered.api.event.EventTask onProxyInitialization(ProxyInitializeEvent event) {
		// Velocity waits for this async lifecycle task: SQL cannot race ahead of provisioning.
		return com.velocitypowered.api.event.EventTask.async(this::initializeProxy);
	}

	private void initializeProxy() {
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
		if (!prepareDatabaseDriver()) {
			logger.error("VotingPlugin cannot initialize SQL; votes are NOT being processed. Restart after installing the required JDBC driver.");
			return;
		}
		ensureCommunicationSecret();

		channel = buildChannelIdentifier(config.getPluginMessageChannel());
		server.getChannelRegistrar().register(channel);

		CommandMeta meta = server.getCommandManager().metaBuilder("votingpluginproxy").aliases("vpp").build();
		server.getCommandManager().register(meta, new VotingPluginVelocityCommand(this));

		// Load the embedded version before the shared runtime starts optional integrations.
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

		initializeFirstRuntime();
		if (!runtimeOperational) {
			logger.error("VotingPlugin proxy runtime failed to initialize; votes are NOT being processed.");
			throw new IllegalStateException("VotingPlugin proxy runtime failed to initialize");
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

	boolean prepareDatabaseDriver() {
		if (!config.hasDatabaseConfigured()) return true;
		List<MysqlConfig> connections = new ArrayList<>();
		connections.add(getMysqlConfig());
		if (config.getGlobalDataEnabled() && !config.getGlobalDataUseMainMySQL())
			connections.add(getGlobalDataMysqlConfig());
		if (config.getVoteCacheUseMySQL() && !config.getVoteCacheUseMainMySQL())
			connections.add(new MysqlConfigVelocity("VoteCache", config));
		if (config.getNonVotedCacheUseMySQL() && !config.getNonVotedCacheUseMainMySQL())
			connections.add(new MysqlConfigVelocity("NonVotedCache", config));
		if (config.getVoteLoggingEnabled() && !config.getVoteLoggingUseMainMySQL())
			connections.add(new MysqlConfigVelocity("VoteLogging", config));
		return new VelocityDatabaseDriverInstaller(driver -> {
			try {
				Class.forName(driver, true, com.bencodez.simpleapi.sql.mysql.ConnectionManager.class.getClassLoader());
				return true;
			} catch (ClassNotFoundException missing) { return false; }
		}, VelocityDatabaseDriverInstaller::downloadLatestRelease).ready(connections,
				config.getAutoDownloadMissingDatabaseDriver(), dataDirectory, logger::info, logger::warn);
	}

	private void ensureCommunicationSecret() {
		try {
			boolean created = com.bencodez.votingplugin.proxy.security.SharedSecretKeyFile
					.ensure(dataDirectory.resolve("secretkey.key"));
			if (created) logger.info("Created secretkey.key for VotingPlugin communication security");
			if (!config.getCommunicationEncryption()) logger.warn(
					"CommunicationEncryption is disabled. Copy this proxy's secretkey.key to every VotingPlugin node, enable CommunicationEncryption everywhere, and restart (recommended).");
		} catch (IOException failure) {
			boolean required = config.getCommunicationEncryption()
					|| com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode
							.parse(config.getSharedTransportAuthentication())
							== com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode.REQUIRED;
			if (required) throw new IllegalStateException(
					"Unable to prepare required VotingPlugin communication secretkey.key", failure);
			logger.warn("Unable to create optional secretkey.key; continuing with legacy plaintext/unsigned communication. Fix the data-folder permissions before enabling communication security.");
		}
	}

	void initializeFirstRuntime() {
		// Full initialization creates the first runtime; there is no old runtime to retire.
		reloadAllInternal(true);
	}

	public boolean isRuntimeOperational() {
		return runtimeOperational;
	}

	/**
	 * Admits a Votifier vote against one runtime generation. The reload lock keeps
	 * teardown from disposing the selected runtime until the vote call returns.
	 */
	public IncomingVoteRuntimeResult processIncomingVote(String player, String service, UUID voteId) {
		if (reloading) return IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD;
		synchronized (reloadLock) {
			if (reloading) return IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD;
			VotingPluginProxy runtime = votingPluginProxy;
			if (!runtimeOperational || runtime == null) return IncomingVoteRuntimeResult.RUNTIME_UNAVAILABLE;
			runtime.vote(player, service, true, false, 0, null, null, voteId);
			return IncomingVoteRuntimeResult.PROCESSED;
		}
	}

	/** Owns a Votifier event before any fallible executor handoff. */
	public void acceptIncomingVote(String player, String service) {
		PendingIncomingVote pending = pendingIncomingVotes.admit(player, service);
		if (pending == null) {
			if (!pendingIncomingVotes.isAccepting()) {
				logger.error("Vote received after VotingPlugin proxy shutdown began; vote was not accepted for {}",
						MinecraftUsernameValidator.sanitizeForLog(player));
				return;
			}
			synchronized (reloadLock) {
				if (!pendingIncomingVotes.isAccepting()) {
					logger.error("Vote received after VotingPlugin proxy shutdown began; vote was not accepted for {}",
							MinecraftUsernameValidator.sanitizeForLog(player));
					return;
				}
				pending = new PendingIncomingVote(UUID.randomUUID(), player, service, System.currentTimeMillis());
				if (votingPluginProxy == null || !votingPluginProxy.retainIncomingVoteForRestart(pending)) {
					logger.error("Pending vote admission is full and durable overflow failed; vote was not accepted for {}",
							MinecraftUsernameValidator.sanitizeForLog(player));
					return;
				}
				votingPluginProxy.scheduleQueuedVoteReplay();
				logger.warn("Pending vote admission is full; accepted vote was handed directly to durable recovery");
				return;
			}
		}
		retryPendingIncomingVotes();
	}

	private void schedulePendingIncomingVote(PendingIncomingVote pending, long delaySeconds) {
		if (!pendingIncomingVotes.contains(pending.getVoteId()) || !pending.beginScheduling()) return;
		Runnable wakeup = () -> {
			pending.endScheduling();
			processPendingIncomingVote(pending);
		};
		try {
			if (delaySeconds == 0) timer.execute(wakeup);
			else timer.schedule(wakeup, delaySeconds, TimeUnit.SECONDS);
		} catch (RejectedExecutionException rejected) {
			pending.endScheduling();
			logger.warn("Unable to schedule pending vote processing; the accepted vote remains retained for lifecycle recovery");
		}
	}

	private void processPendingIncomingVote(PendingIncomingVote pending) {
		if (!pendingIncomingVotes.contains(pending.getVoteId()) || !pending.beginProcessing()) return;
		try {
			IncomingVoteRuntimeResult result;
			synchronized (reloadLock) {
				result = processIncomingVote(pending.getPlayer(), pending.getService(), pending.getVoteId());
				if (result == IncomingVoteRuntimeResult.PROCESSED) pendingIncomingVotes.complete(pending);
			}
			if (result == IncomingVoteRuntimeResult.PROCESSED) {
				logger.info("Vote received " + MinecraftUsernameValidator.sanitizeForLog(pending.getPlayer())
						+ " from service site " + MinecraftUsernameValidator.sanitizeForLog(pending.getService()));
				return;
			}
			if (result == IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD) {
				schedulePendingIncomingVote(pending, 1);
				return;
			}
			persistTerminalPendingVote(pending);
		} catch (VoteRetryException retryable) {
			if (pending.incrementStorageAttempts() < 12) {
				logger.warn("Vote processing is waiting for durable storage; retrying shortly");
				schedulePendingIncomingVote(pending, 5);
			} else {
				persistTerminalPendingVote(pending);
			}
		} finally {
			pending.endProcessing();
		}
	}

	private void persistTerminalPendingVote(PendingIncomingVote pending) {
		synchronized (reloadLock) {
			VotingPluginProxy runtime = votingPluginProxy;
			if (runtime != null && runtime.retainIncomingVoteForRestart(pending)) {
				pendingIncomingVotes.complete(pending);
				runtime.scheduleQueuedVoteReplay();
				logger.warn("Vote processing was handed to durable restart recovery for {}",
						MinecraftUsernameValidator.sanitizeForLog(pending.getPlayer()));
				return;
			}
		}
		logger.error("Unable to durably retain accepted vote for {}; keeping it in process memory and retrying the durable handoff",
				MinecraftUsernameValidator.sanitizeForLog(pending.getPlayer()));
		schedulePendingDurableHandoff(pending, pending.nextDurableHandoffDelaySeconds());
	}

	private void schedulePendingDurableHandoff(PendingIncomingVote pending, long delaySeconds) {
		if (!pendingIncomingVotes.contains(pending.getVoteId()) || !pending.beginScheduling()) return;
		Runnable wakeup = () -> {
			pending.endScheduling();
			if (!pendingIncomingVotes.contains(pending.getVoteId()) || !pending.beginProcessing()) return;
			try {
				persistTerminalPendingVote(pending);
			} finally {
				pending.endProcessing();
			}
		};
		try {
			timer.schedule(wakeup, delaySeconds, TimeUnit.SECONDS);
		} catch (RejectedExecutionException rejected) {
			pending.endScheduling();
			logger.warn("Unable to schedule durable vote-handoff retry; the vote remains owned for lifecycle recovery");
		}
	}

	private boolean persistPendingIncomingVotes(VotingPluginProxy runtime, String reason) {
		boolean retained = true;
		List<PendingIncomingVote> emergencyPending = new ArrayList<>();
		List<VoteTimeQueue> emergencyVotes = new ArrayList<>();
		for (PendingIncomingVote pending : pendingIncomingVotes.snapshot()) {
			if (runtime != null && runtime.retainIncomingVoteForRestart(pending)) {
				pendingIncomingVotes.complete(pending);
			} else {
				retained = false;
				emergencyPending.add(pending);
				VoteTimeQueue recovery = runtime == null
						? new VoteTimeQueue(pending.getVoteId(), pending.getPlayer(), pending.getService(), pending.getAcceptedAt())
						: runtime.snapshotIncomingVoteForRestart(pending);
				if (recovery == null) recovery = new VoteTimeQueue(pending.getVoteId(), pending.getPlayer(),
						pending.getService(), pending.getAcceptedAt());
				emergencyVotes.add(recovery);
				logger.error("Unable to retain accepted vote during {} for {}", reason,
						MinecraftUsernameValidator.sanitizeForLog(pending.getPlayer()));
			}
		}
		if (!emergencyVotes.isEmpty()) {
			try {
				pendingIncomingVoteJournal.merge(emergencyVotes);
				for (PendingIncomingVote pending : emergencyPending) pendingIncomingVotes.complete(pending);
				retained = true;
				logger.warn("Accepted votes were preserved in the emergency lifecycle journal during {}", reason);
			} catch (IOException journalFailure) {
				logger.error("Unable to write the primary emergency pending-vote journal", journalFailure);
				try {
					pendingIncomingVoteRescueJournal.merge(emergencyVotes);
					for (PendingIncomingVote pending : emergencyPending) pendingIncomingVotes.complete(pending);
					retained = true;
					logger.warn("Accepted votes were preserved in the sibling rescue journal during {}", reason);
				} catch (IOException rescueFailure) {
					logger.error("Unable to write the sibling pending-vote rescue journal", rescueFailure);
				}
			}
		}
		return retained;
	}

	private boolean recoverEmergencyIncomingVotes(VotingPluginProxy runtime) {
		boolean primaryRecovered = recoverEmergencyIncomingVotes(runtime, pendingIncomingVoteJournal);
		boolean rescueRecovered = recoverEmergencyIncomingVotes(runtime, pendingIncomingVoteRescueJournal);
		return primaryRecovered && rescueRecovered;
	}

	private boolean recoverEmergencyIncomingVotes(VotingPluginProxy runtime, PendingIncomingVoteJournal journal) {
		try {
			List<VoteTimeQueue> remaining = new ArrayList<>();
			for (VoteTimeQueue vote : journal.load()) {
				if (!runtime.retainIncomingVoteForRestart(vote)) remaining.add(vote);
			}
			journal.replace(remaining);
			return remaining.isEmpty();
		} catch (IOException failure) {
			logger.error("Unable to recover the emergency pending-vote journal", failure);
			return false;
		}
	}

	private void retryPendingIncomingVotes() {
		for (PendingIncomingVote pending : pendingIncomingVotes.snapshot()) {
			schedulePendingIncomingVote(pending, 0);
		}
	}

	/** Returns whether a runtime replacement is currently in progress. */
	public boolean isReloading() {
		return reloading;
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
			final boolean retainedRuntimeWasOperational = runtimeOperational;
			reloading = true;
			cancelTasks();

			try {
				config.reload();
			} catch (Exception e) {
				logger.error("Failed to reload bungeeconfig.yml", e);
			}

			try {
				ChannelIdentifier old = channel;
				ChannelIdentifier next = buildChannelIdentifier(config.getPluginMessageChannel());
				if (old != null && !old.equals(next)) {
					try {
						server.getChannelRegistrar().unregister(old);
					} catch (Exception ignored) {
					}
				}
				server.getChannelRegistrar().register(next);
				channel = next;
			} catch (Exception e) {
				logger.error("Failed to update plugin message channel", e);
			}

			if (!loadMysql) {
				boolean softReloadApplied = true;
				try {
					if (votingPluginProxy != null) votingPluginProxy.reload();
				} catch (Throwable t) {
					softReloadApplied = false;
					logger.error("Error while applying soft reload", t);
				}
				try {
					scheduleTasks();
					if (softReloadApplied) publishRetainedRuntimeOperational();
					else runtimeOperational = retainedRuntimeWasOperational;
				} catch (RuntimeException taskFailure) {
					runtimeOperational = false;
					logger.error("VotingPlugin could not restart proxy tasks; votes are NOT being processed.", taskFailure);
				}
				reloading = false;
			} else {
				try {
					if (votingPluginProxy != null
							&& votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement()) {
						votingPluginProxy.reload();
						if (votingPluginProxy.isRetainingHttpTransportForDeferredReconciliation()) {
							scheduleTasks();
							publishRetainedRuntimeOperational();
							reloading = false;
							drainQueuedPluginMessagesAfterReloadLock();
							return;
						}
					}
				} catch (Throwable retentionFailure) {
					logger.error("Reload aborted while checking retained HTTP delivery state; the existing runtime remains active", retentionFailure);
					try {
						scheduleTasks();
						runtimeOperational = retainedRuntimeWasOperational;
					} catch (RuntimeException taskFailure) {
						retentionFailure.addSuppressed(taskFailure);
						runtimeOperational = false;
						logger.error("VotingPlugin could not restart proxy tasks; votes are NOT being processed.", taskFailure);
					}
					reloading = false;
					drainQueuedPluginMessagesAfterReloadLock();
					return;
				}

				try {
					if (voteCacheFile != null) voteCacheFile.save();
				} catch (Exception ignored) {
				}
				try {
					if (nonVotedPlayersCache != null) nonVotedPlayersCache.save();
				} catch (Exception ignored) {
				}

				if (!persistPendingIncomingVotes(votingPluginProxy, "runtime replacement")) {
					logger.error("Reload aborted because accepted votes could not be durably retained");
					try {
						scheduleTasks();
						runtimeOperational = retainedRuntimeWasOperational;
					} catch (RuntimeException taskFailure) {
						runtimeOperational = false;
						logger.error("VotingPlugin could not restart proxy tasks; votes are NOT being processed.", taskFailure);
					}
					reloading = false;
					drainQueuedPluginMessagesAfterReloadLock();
					return;
				}
				try {
					ProxyRuntimeReplacementLifecycle.prepare(votingPluginProxy);
				} catch (Exception shutdownFailure) {
					logger.error("Reload aborted because hosted Control did not stop safely", shutdownFailure);
					try {
						scheduleTasks();
						runtimeOperational = retainedRuntimeWasOperational;
					} catch (RuntimeException taskFailure) {
						shutdownFailure.addSuppressed(taskFailure);
						runtimeOperational = false;
						logger.error("VotingPlugin could not restart proxy tasks; votes are NOT being processed.", taskFailure);
					}
					reloading = false;
					drainQueuedPluginMessagesAfterReloadLock();
					return;
				}
				try {
					ProxyRuntimeReplacementLifecycle.complete(votingPluginProxy);
				} catch (Exception cleanupFailure) {
					logger.error("Old proxy runtime cleanup was incomplete; replacement will continue", cleanupFailure);
				}
				runtimeOperational = false;
				runtimeInitialized = false;

				try {
					votingPluginProxy = createProxyRuntime();
				} catch (Throwable creationFailure) {
					logger.error("Reload aborted while creating the replacement proxy runtime", creationFailure);
					reloading = false;
					return;
				}

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
				if (votingPluginProxy.getProxyMySQL() == null) {
					logger.error("Reload aborted: Proxy MySQL is not initialized.");
					logger.error("VotingPlugin proxy runtime is NOT processing incoming votes.");
					retireFailedReplacementRuntime();
					reloading = false;
					return;
				}

				try {
					voteCacheFile = new VelocityJsonVoteCache(new File(dataDirectory.toFile(), "votecache.json"));
					nonVotedPlayersCache = new VelocityJsonNonVotedPlayersCache(
							new File(dataDirectory.toFile(), "nonvotedplayerscache.json"));
					convertYamlCachesIfPresent();
					votingPluginProxy.load(voteCacheFile, nonVotedPlayersCache);
					if (!recoverEmergencyIncomingVotes(votingPluginProxy)) {
						throw new IllegalStateException("Emergency pending votes could not be adopted");
					}
					votingPluginProxy.reload();
					runtimeInitialized = true;
				} catch (Throwable t) {
					logger.error("Reload aborted while loading proxy state", t);
					logger.error("VotingPlugin proxy runtime is NOT processing incoming votes.");
					retireFailedReplacementRuntime();
					reloading = false;
					return;
				}

				try {
					scheduleTasks();
				} catch (RuntimeException taskFailure) {
					logger.error("Reload aborted while scheduling proxy tasks", taskFailure);
					retireFailedReplacementRuntime();
					reloading = false;
					return;
				}
				if (!initVotifierListenerIfNeeded()) {
					logger.error("VotingPlugin Votifier listener failed to initialize; votes are NOT being processed.");
					retireFailedReplacementRuntime();
					reloading = false;
					return;
				}

				// Publish a completely ready runtime before clearing the reload fence.
				runtimeOperational = true;
				reloading = false;
			}
		}

		drainQueuedPluginMessages();
		if (votingPluginProxy != null) votingPluginProxy.scheduleQueuedVoteReplay();
		retryPendingIncomingVotes();
		try {
			if (votingPluginProxy != null) votingPluginProxy.sendServerNameMessage();
		} catch (Exception ignored) {
		}
	}
	/** Stops every partially initialized replacement component after a terminal load failure. */
	private void retireFailedReplacementRuntime() {
		runtimeOperational = false;
		runtimeInitialized = false;
		cancelTasks();
		try {
			if (votingPluginProxy != null) votingPluginProxy.onDisable();
		} catch (Exception cleanupFailure) {
			logger.error("Failed replacement runtime cleanup was incomplete", cleanupFailure);
		}
	}

	/** Restores readiness only for a retained runtime whose shared state finished loading. */
	void publishRetainedRuntimeOperational() {
		if (!runtimeInitialized) {
			runtimeOperational = false;
			logger.error("Soft reload cannot make an incomplete proxy runtime operational; run a full reload.");
			return;
		}
		runtimeOperational = true;
	}

	/** The retention branch returns from inside reloadLock; drain only after that lock is released. */
	private void drainQueuedPluginMessagesAfterReloadLock() {
		Thread drain = new Thread(() -> {
			synchronized (reloadLock) {
				// Acquire/release establishes that the returning reload has left its lock.
			}
			drainQueuedPluginMessages();
			if (votingPluginProxy != null) votingPluginProxy.scheduleQueuedVoteReplay();
			retryPendingIncomingVotes();
		}, "VotingPlugin-Velocity-Reload-Queue-Drain");
		drain.setDaemon(true);
		drain.start();
	}

	/** Runs a scheduled deferred replacement only if it is still current while holding reloadLock. */
	private void reloadAllInternalIfCurrent(boolean loadMysql, BooleanSupplier stillCurrent) {
		synchronized (reloadLock) {
			if (!stillCurrent.getAsBoolean()) return;
			reloadAllInternal(loadMysql);
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
			public Set<String> getAllConfiguredServers() {
				Set<String> configured = new HashSet<>();
				server.getAllServers().forEach(registered -> configured.add(registered.getServerInfo().getName()));
				return configured;
			}

			@Override
			public VotingPluginProxyConfig getConfig() {
				return config;
			}

			@Override
			public String getProxyPlatform() {
				return "VELOCITY";
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
			public Collection<String> getVoteCachePendingVotePartyServers() {
				return voteCacheFile.getPendingVotePartyRewardServers();
			}

			@Override
			public Collection<String> getVoteCachePendingVotePartyRewardIds(String server) {
				return voteCacheFile.getPendingVotePartyRewardIds(server);
			}

			@Override
			public com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects getVoteCachePendingVotePartyProxyEffects() {
				return voteCacheFile.getPendingVotePartyProxyEffects();
			}

			@Override
			public com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects getVoteCacheQuarantinedVotePartyProxyEffects() {
				return voteCacheFile.getQuarantinedVotePartyProxyEffects();
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
			public void setVoteCachePendingVotePartyReward(String server, String deliveryId, boolean pending) {
				voteCacheFile.setPendingVotePartyReward(server, deliveryId, pending);
			}

			@Override
			public void setVoteCachePendingVotePartyProxyEffects(
					com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects effects) {
				voteCacheFile.setPendingVotePartyProxyEffects(effects);
			}

			@Override
			public void setVoteCacheQuarantinedVotePartyProxyEffects(
					com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects effects) {
				voteCacheFile.setQuarantinedVotePartyProxyEffects(effects);
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
			protected java.util.concurrent.CompletableFuture<Void> runVotePartyConsoleCommand(String command) {
				return server.getCommandManager().executeAsync(server.getConsoleCommandSource(), command)
						.thenApply(executed -> {
							if (!executed) throw new IllegalStateException("Velocity declined the vote-party proxy command");
							return null;
						});
			}

			@Override
			public void saveVoteCacheFile() {
				voteCacheFile.save();
			}

			@Override
			public void saveVotePartyStateDurably() throws java.io.IOException {
				com.bencodez.votingplugin.proxy.cache.VotePartyCacheDurability.saveAndVerify(
						dataDirectory.resolve("votecache.json"), voteCacheFile);
			}

			@Override
			public boolean sendPluginMessageData(String serverName, String channelName, byte[] data, boolean queue) {
				if (!server.getServer(serverName).isPresent()) {
					return false;
				}
				RegisteredServer send = server.getServer(serverName).get();
				return send.sendPluginMessage(channel, data);
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
			protected void reloadDeferredHttpTransportCore(long generation) {
				reloadAllInternalIfCurrent(true, () -> isDeferredHttpTransportGenerationCurrent(generation));
			}

			@Override
			public void reloadControlConfiguration() throws Exception {
				config.loadControlConfiguration();
				reloadFromControl();
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
	 * Initializes the Votifier listener when Votifier is present and enabled.
	 * The listener field is published only after event registration succeeds.
	 *
	 * @return true when vote receipt is intentionally unavailable or ready
	 */
	boolean initVotifierListenerIfNeeded() {
		try {
			requireVotifierEventClass();
		} catch (ClassNotFoundException e) {
			getVotingPluginProxy().setVotifierEnabled(false);
			return true;
		} catch (LinkageError incompatible) {
			logger.error("Votifier is present but its event API could not be loaded", incompatible);
			return false;
		}

		if (!getVotingPluginProxy().isVotifierEnabled() || voteEventVelocity != null) return true;
		try {
			VoteEventVelocity candidate = createVotifierListener();
			server.getEventManager().register(this, candidate);
			voteEventVelocity = candidate;
			return true;
		} catch (RuntimeException | LinkageError e) {
			logger.error("Unable to register the Votifier listener", e);
			return false;
		}
	}

	void requireVotifierEventClass() throws ClassNotFoundException {
		Class.forName("com.vexsoftware.votifier.velocity.event.VotifierEvent");
	}

	VoteEventVelocity createVotifierListener() {
		return new VoteEventVelocity(this);
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
			getVotingPluginProxy().retryPendingOnlineBroadcasts();
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

	private void handlePluginMessageBytes(String sourceServer, byte[] data) {
		ByteArrayInputStream instream = new ByteArrayInputStream(data);
		DataInputStream in = new DataInputStream(instream);
		try {
			getVotingPluginProxy().onPluginMessageReceived(in, sourceServer);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void queuePluginMessage(String sourceServer, byte[] data) {
		byte[] copy = new byte[data.length];
		System.arraycopy(data, 0, copy, 0, data.length);
		queuedPluginMessages.add(new QueuedPluginMessage(sourceServer, copy));
	}

	private void drainQueuedPluginMessages() {
		while (true) {
			synchronized (reloadLock) {
				if (reloading || !runtimeOperational || votingPluginProxy == null) return;
				QueuedPluginMessage msg = queuedPluginMessages.poll();
				if (msg == null) return;
				handlePluginMessageBytes(msg.sourceServer(), msg.data());
			}
		}
	}

	private record QueuedPluginMessage(String sourceServer, byte[] data) { }

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
