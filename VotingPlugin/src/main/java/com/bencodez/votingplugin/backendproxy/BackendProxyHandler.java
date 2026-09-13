package com.bencodez.votingplugin.backendproxy;

import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import lombok.Getter;

/**
 * Coordinates backend/proxy communication components.
 */
public class BackendProxyHandler implements Listener {

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	private final BackendProxyTransportManager transportManager;
	private final BackendGlobalDataSync globalDataSync;

	private BackendPresenceManager presenceManager;
	private boolean presenceReportingActivated;
	private final Object inboundPublication = new Object();
	private boolean inboundPublished;
	private boolean inboundAborted;
	private BackendProxyHandler inboundRollbackTarget;
	private BackendVotePartySync votePartySync;
	private BackendProxyMessageRouter messageRouter;

	@Getter
	private BungeeMethod method;
	@Getter
	private GlobalMessageHandler globalMessageHandler;

	public BackendProxyHandler(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache());
	}

	public BackendProxyHandler(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.processedVoteCache = java.util.Objects.requireNonNull(processedVoteCache, "processedVoteCache");
		transportManager = new BackendProxyTransportManager(plugin, processedVoteCache);
		globalDataSync = new BackendGlobalDataSync(plugin, this::sendEnvelope);
	}

	/**
	 * Loads the configured backend/proxy communication components.
	 */
	public void load() {
		load(true);
	}

	/** Loads a replacement without announcing a new presence generation before publication. */
	public void loadForReplacement() {
		load(false);
	}

	private void load(boolean activatePresenceReporting) {
		plugin.debug("Loading backend proxy handler");
		method = BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod());
		plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		globalDataSync.load();
		globalMessageHandler = new GlobalMessageHandler() {
			@Override
			public void onMessage(JsonEnvelope envelope) {
				BackendProxyHandler.this.dispatchIncomingAfterPublication(envelope, () -> super.onMessage(envelope));
			}

			@Override
			public void sendMessage(JsonEnvelope envelope) {
				transportManager.send(envelope);
			}
		};

		presenceManager = new BackendPresenceManager(plugin, method, globalMessageHandler);
		votePartySync = new BackendVotePartySync(plugin);
		messageRouter = new BackendProxyMessageRouter(plugin, presenceManager, globalDataSync, votePartySync,
				processedVoteCache);
		messageRouter.register(globalMessageHandler, method);
		transportManager.start(method, globalMessageHandler, activatePresenceReporting);

		if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
			plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
		}
		if (activatePresenceReporting) {
			activatePresenceReporting();
			activateInboundMessages();
		}
	}

	/** Starts presence only after a staged handler reaches the atomic publication boundary. */
	public void activatePresenceReporting() {
		if (presenceManager != null && !presenceReportingActivated) {
			presenceManager.start();
			presenceReportingActivated = true;
		}
		// Presence startup can throw while scheduling its heartbeat. Keep inbound
		// HTTP callbacks behind the publication barrier until every fallible part of
		// the replacement is active, so rollback cannot race a queued callback.
		transportManager.activateAfterPublication();
	}

	/**
	 * Closes backend/proxy components and persists cached proxy state.
	 */
	public void close() {
		synchronized (inboundPublication) {
			if (!inboundPublished && !inboundAborted) inboundAborted = true;
			inboundPublication.notifyAll();
		}
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceReportingActivated = false;
		}
		transportManager.close();
		if (votePartySync != null) {
			votePartySync.persist();
		}
		globalDataSync.close();
	}

	/** Opens inbound dispatch only after the replacement and all handoffs are committed. */
	public void activateInboundMessages() {
		synchronized (inboundPublication) {
			if (inboundAborted) return;
			inboundPublished = true;
			inboundPublication.notifyAll();
		}
	}

	/** Routes an already accepted staged callback through the restored predecessor on rollback. */
	public void abortStagedInboundTo(BackendProxyHandler previous) {
		synchronized (inboundPublication) {
			if (inboundPublished || inboundAborted) return;
			inboundRollbackTarget = previous;
			inboundAborted = true;
			inboundPublication.notifyAll();
		}
	}

	void dispatchIncomingAfterPublication(JsonEnvelope envelope, Runnable localDispatch) {
		BackendProxyHandler rollbackTarget;
		synchronized (inboundPublication) {
			while (!inboundPublished && !inboundAborted) {
				try {
					inboundPublication.wait();
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					return;
				}
			}
			rollbackTarget = inboundPublished ? null : inboundRollbackTarget;
			if (!inboundPublished && rollbackTarget == null) return;
		}
		if (rollbackTarget != null) {
			GlobalMessageHandler rollbackHandler = rollbackTarget.globalMessageHandler;
			if (rollbackHandler != null) rollbackHandler.onMessage(envelope);
			return;
		}
		// Global-data checks perform synchronous SQL and already run on their own
		// timer worker. Explicitly dispatch an inbound wake-up asynchronously too:
		// plugin messaging can invoke this method on Bukkit's primary thread.
		if (VotingPluginWire.SUB_BUNGEE_TIME_CHANGE.equals(envelope.getSubChannel())) {
			plugin.getBukkitScheduler().runTaskAsynchronously(plugin, localDispatch);
			return;
		}
		plugin.getBukkitScheduler().executeOrScheduleSync(plugin, localDispatch);
	}

	/** Returns whether replacement preparation must preserve accepted deliveries. */
	public boolean requiresPreparationForReplacement() {
		return method == BungeeMethod.HTTP || method == BungeeMethod.PLUGINMESSAGING
				|| transportManager.hasPendingAsyncHandoff() || transportManager.hasPendingRedisReplay();
	}

	/** Prepares HTTP state or waits off-thread for an earlier cross-transport handoff. */
	public boolean prepareForReplacement(BungeeMethod replacementMethod) {
		return prepareForReplacement(replacementMethod,
				System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(25));
	}

	public boolean prepareForReplacement(BungeeMethod replacementMethod, long deadlineNanos) {
		if (method == BungeeMethod.HTTP) {
			transportManager.prepareForReplacement();
			return true;
		}
		if (method == BungeeMethod.PLUGINMESSAGING) {
			transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
			return true;
		}
		if (method == BungeeMethod.REDIS && replacementMethod != BungeeMethod.REDIS) {
			if (transportManager.hasPendingAsyncHandoff())
				transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
			return transportManager.prepareRedisReplayTransition(replacementMethod, deadlineNanos);
		}
		if (method == BungeeMethod.REDIS) {
			// Same-Redis retirement installs its send fence during the bounded
			// off-thread handoff. Returning true here lets the staged replacement
			// buffer its own sends until the predecessor FIFO is admitted at Bukkit
			// publication.
			return true;
		}
		if (!transportManager.hasPendingAsyncHandoff()) return false;
		transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
		return true;
	}

	/** Atomically fences new sends only when disabling cannot discard prepared HTTP messages. */
	public boolean commitPreparedDisable() {
		return transportManager.commitPreparedDisable();
	}

	/** Publishes the final presence update while fencing other sends before transport preparation. */
	public void preparePresenceForDisable() {
		preparePresenceForDisableInternal(null);
	}

	/** Publishes the final presence update before the caller's validation deadline. */
	public void preparePresenceForDisable(long deadlineNanos) {
		preparePresenceForDisableInternal(deadlineNanos);
	}

	private void preparePresenceForDisableInternal(Long deadlineNanos) {
		transportManager.beginPreparedDisable();
		if (presenceManager != null && presenceReportingActivated) {
			// stopForDisable may reject when the transport cannot accept the final
			// presence update. Mark this inactive first so rollback can start it again.
			presenceReportingActivated = false;
			if (deadlineNanos == null) presenceManager.stopForDisable();
			else presenceManager.stopForDisable(deadlineNanos);
		}
	}

	/** Restores delivery and presence when a prepared disable is abandoned. */
	public void restorePresenceAfterFailedDisablePreparation() {
		transportManager.cancelPreparedDisable();
		activatePresenceReporting();
	}

	public void beginPreparedHttpHandoff() {
		transportManager.beginPreparedHttpHandoff();
	}

	/** Reserves staged HTTP capacity before publication can admit replacement sends. */
	public void reservePreparedHttpHandoff(BackendProxyHandler replacement) {
		transportManager.reservePreparedTransportHandoff(replacement.transportManager);
	}

	/** Restores a prepared HTTP transport when its replacement fails validation. */
	public void restoreAfterFailedReplacement() {
		transportManager.restoreAfterFailedReplacement();
	}

	/** Restores a failed same-Redis predecessor without discarding its replacement's replay FIFO. */
	public void restoreAfterFailedReplacement(BackendProxyHandler failedReplacement) {
		transportManager.restoreAfterFailedReplacement(
				failedReplacement == null ? null : failedReplacement.transportManager);
	}

	/** Reasserts the old handler with a fresh presence generation after rollback. */
	public void refreshPresenceAfterFailedReplacement() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceManager.start();
		}
	}

	public void awaitRestoreAfterFailedReplacement(long deadlineNanos) {
		transportManager.awaitPreparedTransportRestoration(deadlineNanos);
	}

	/** Fails a configuration apply when its selected transport did not initialize. */
	public void validateTransport() {
		validateTransport(System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(25));
	}

	/** Validates transport startup without extending the caller's existing deadline. */
	public void validateTransport(long deadlineNanos) {
		if (method == null || globalMessageHandler == null || presenceManager == null) {
			throw new IllegalStateException("Backend proxy handler initialization failed");
		}
		transportManager.validate(deadlineNanos);
	}

	/** Completes the no-loss/no-duplicate same-Redis subscriber handoff after validation. */
	public void completeRedisHandoff(BackendProxyHandler replacement) {
		if (!requiresRedisHandoff(replacement)) return;
		transportManager.completeRedisHandoff(replacement.transportManager);
	}

	/** Returns whether this handler owns a Redis listener whose shutdown can block. */
	public boolean requiresRedisRetirement() {
		return method == BungeeMethod.REDIS;
	}

	/** Returns whether this replacement needs the bounded same-Redis retirement path. */
	public boolean requiresRedisHandoff(BackendProxyHandler replacement) {
		return replacement != null && method == BungeeMethod.REDIS && replacement.method == BungeeMethod.REDIS;
	}

	/** Replays Redis handoff deliveries only after inbound publication is open. */
	public void replayRedisAfterHandoffPublication() {
		transportManager.replayRedisAfterHandoffPublication();
	}

	/** Forwards messages buffered while the previous transport was fenced. */
	public void completeHttpHandoff(BackendProxyHandler replacement) {
		if (replacement == null) return;
		transportManager.completePreparedTransportHandoff(replacement.transportManager);
	}

	public void playerOnline(String playerName, String uuid) {
		if (presenceManager != null) {
			presenceManager.playerOnline(playerName, uuid);
		}
	}

	public void playerOffline(String playerName) {
		if (presenceManager != null) {
			presenceManager.playerOffline(playerName);
		}
	}

	public void reloadPresenceReporting() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.reload();
		}
	}

	public void disablePresenceReporting() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceReportingActivated = false;
		}
	}

	public void loadGlobalMysql() {
		globalDataSync.load();
	}

	public GlobalDataHandler getGlobalDataHandler() {
		return globalDataSync.getGlobalDataHandler();
	}

	public void checkGlobalData() {
		globalDataSync.checkGlobalData();
	}

	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		return globalDataSync.checkGlobalDataTime(type, data);
	}

	public boolean checkGlobalDataTimeValue(DataValue data) {
		return globalDataSync.checkGlobalDataTimeValue(data);
	}

	public ConcurrentHashMap<UUID, Long> getProcessedWireVotes() {
		return processedVoteCache.getProcessedVotes();
	}

	public int getBungeeVotePartyCurrent() {
		return votePartySync == null ? plugin.getServerData().getBungeeVotePartyCurrent() : votePartySync.getCurrent();
	}

	public int getBungeeVotePartyRequired() {
		return votePartySync == null ? plugin.getServerData().getBungeeVotePartyRequired() : votePartySync.getRequired();
	}

	public ScheduledExecutorService getTimer() {
		return globalDataSync.getTimer();
	}

	public ClientHandler getClientHandler() {
		return transportManager.getClientHandler();
	}

	public SocketHandler getSocketHandler() {
		return transportManager.getSocketHandler();
	}

	public RedisHandler getRedisHandler() {
		return transportManager.getRedisHandler();
	}

	public MySqlMessenger getBackendMysqlMessenger() {
		return transportManager.getBackendMysqlMessenger();
	}

	public MqttHandler getMqttHandler() {
		return transportManager.getMqttHandler();
	}

	private void sendEnvelope(JsonEnvelope envelope) {
		if (globalMessageHandler != null) {
			globalMessageHandler.sendMessage(envelope);
		}
	}
}
