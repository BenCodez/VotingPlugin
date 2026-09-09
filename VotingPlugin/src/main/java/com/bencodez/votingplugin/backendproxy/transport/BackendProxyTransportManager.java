package com.bencodez.votingplugin.backendproxy.transport;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.proxy.BungeeMethod;

/**
 * Selects and owns the active backend-to-proxy transport.
 */
public class BackendProxyTransportManager {
	private static final int MAX_PREPARED_SENDS = 1024;

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	private BackendProxyTransport transport;
	private BackendProxyTransport preparedTransport;
	private BackendProxyTransport retiredTransport;
	private BackendProxyTransportManager forwardingManager;
	private final java.util.ArrayDeque<JsonEnvelope> preparedSends = new java.util.ArrayDeque<>();
	private boolean preparedQueueWarning;

	public BackendProxyTransportManager(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache());
	}

	public BackendProxyTransportManager(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.processedVoteCache = processedVoteCache;
	}

	public void start(BungeeMethod method, GlobalMessageHandler messageHandler) {
		start(method, messageHandler, true);
	}

	public void start(BungeeMethod method, GlobalMessageHandler messageHandler, boolean retryInitialization) {
		close();
		switch (method) {
		case MYSQL:
			transport = new MysqlBackendProxyTransport(plugin);
			break;
		case PLUGINMESSAGING:
			transport = new PluginMessagingBackendProxyTransport(plugin);
			break;
		case SOCKETS:
			transport = new SocketBackendProxyTransport(plugin);
			break;
		case HTTP:
			transport = new HttpBackendProxyTransport(plugin);
			break;
		case REDIS:
			transport = new RedisBackendProxyTransport(plugin, processedVoteCache);
			break;
		case MQTT:
			transport = new MqttBackendProxyTransport(plugin);
			break;
		default:
			throw new IllegalArgumentException("Unsupported backend proxy method: " + method);
		}
		transport.start(messageHandler, retryInitialization);
	}

	public synchronized void send(JsonEnvelope envelope) {
		if (transport != null) {
			transport.send(envelope);
		} else if (forwardingManager != null) {
			forwardingManager.send(envelope);
		} else if (preparedTransport != null) {
			if (preparedSends.size() < MAX_PREPARED_SENDS) {
				preparedSends.addLast(envelope);
			} else if (!preparedQueueWarning) {
				preparedQueueWarning = true;
				plugin.getLogger().severe("HTTP replacement handoff queue is full; delivery was not accepted");
			}
		}
	}

	public void activateAfterPublication() {
		if (transport != null) transport.activateAfterPublication();
	}

	public synchronized void close() {
		if (transport != null) {
			transport.close();
			transport = null;
		}
		if (preparedTransport != null) {
			preparedTransport.close();
			preparedTransport = null;
		}
		if (retiredTransport != null) {
			BackendProxyTransport retired = retiredTransport;
			try {
				retired.close();
				retiredTransport = null;
			} catch (RuntimeException cleanupFailure) {
				// This transport has already been fenced and replaced. Keep its handle
				// for another cleanup attempt, but never fail or close the live replacement.
				if (plugin != null) {
					plugin.getLogger().warning("Retired backend proxy transport did not stop cleanly");
					plugin.debug(cleanupFailure);
				}
			}
		}
		if (forwardingManager == null) preparedSends.clear();
	}

	public void validate() {
		validate(System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(25));
	}

	public void validate(long deadlineNanos) {
		if (transport == null) throw new IllegalStateException("Backend proxy transport was not initialized");
		if (transport instanceof HttpBackendProxyTransport http) http.validate(deadlineNanos);
		else transport.validate();
	}

	public void prepareForReplacement() {
		BackendProxyTransport candidate;
		synchronized (this) {
			if (transport == null) return;
			candidate = transport;
			preparedTransport = candidate;
			transport = null;
		}
		try {
			// HTTP preparation can wait on a bounded network flush. Keep send() free to
			// enqueue presence and vote messages while that I/O is in progress.
			candidate.prepareForReplacement();
		} catch (RuntimeException failure) {
			synchronized (this) {
				if (preparedTransport != candidate) throw failure;
				HttpBackendProxyTransport http = candidate instanceof HttpBackendProxyTransport prepared
						? prepared : null;
				if (http != null && !http.isClosedForReplacement()) {
					// A failed flush deliberately restarts the existing connector. Reinstall
					// that live instance instead of creating a second directory owner.
					transport = candidate;
					preparedTransport = null;
					while (!preparedSends.isEmpty()) transport.send(preparedSends.removeFirst());
					preparedQueueWarning = false;
				} else {
					// Enrollment cancellation may already have closed this instance. Restore
					// from its captured configuration before configuration rollback.
					try {
						restorePreparedTransport();
					} catch (RuntimeException restorationFailure) {
						failure.addSuppressed(restorationFailure);
					}
				}
			}
			throw failure;
		}
	}

	public synchronized void completePreparedTransportHandoff(BackendProxyTransportManager replacement) {
		if (preparedTransport == null) return;
		BackendProxyTransportManager target = java.util.Objects.requireNonNull(replacement, "replacement");
		java.util.ArrayList<JsonEnvelope> pending = new java.util.ArrayList<>();
		if (preparedTransport instanceof HttpBackendProxyTransport http) {
			pending.addAll(http.preparedMessagesSnapshot());
		}
		pending.addAll(preparedSends);
		if (target.transport instanceof HttpBackendProxyTransport http) {
			http.acceptHandoffMessages(pending);
		} else {
			for (JsonEnvelope envelope : pending) target.send(envelope);
		}
		// Do not consume the old queues or forward subsequent sends until the
		// replacement has admitted every snapshot. A failed admission therefore
		// leaves rollback with the complete original FIFO intact.
		if (preparedTransport instanceof HttpBackendProxyTransport http) http.drainPreparedMessages();
		preparedSends.clear();
		forwardingManager = target;
	}

	/** Reserves replacement capacity for every old queued message and future prepared send. */
	public synchronized void reservePreparedTransportHandoff(BackendProxyTransportManager replacement) {
		if (!(preparedTransport instanceof HttpBackendProxyTransport previous)) return;
		if (!(java.util.Objects.requireNonNull(replacement, "replacement").transport
				instanceof HttpBackendProxyTransport target))
			throw new IllegalStateException("HTTP replacement transport is unavailable");
		// send() remains available while the previous credential is fenced. Reserve
		// its whole remaining bounded allowance, not only the current queue size.
		target.reservePreparedHandoffCapacity(previous.preparedMessageCount() + MAX_PREPARED_SENDS);
	}

	public void beginPreparedHttpHandoff() {
		if (!(transport instanceof HttpBackendProxyTransport http))
			throw new IllegalStateException("HTTP replacement transport is unavailable");
		http.beginPreparedHandoff();
	}

	public synchronized void restorePreparedTransport() {
		if (transport != null || preparedTransport == null) return;
		if (preparedTransport instanceof HttpBackendProxyTransport http) {
			transport = http.recreatePrepared();
		} else {
			throw new IllegalStateException("Prepared backend proxy transport cannot be restored");
		}
		preparedTransport = null;
		while (!preparedSends.isEmpty()) transport.send(preparedSends.removeFirst());
		preparedQueueWarning = false;
	}

	public void restoreAfterFailedReplacement() {
		if (transport instanceof PluginMessagingBackendProxyTransport pluginMessaging) {
			pluginMessaging.restoreAfterFailedReplacement();
		} else {
			restorePreparedTransport();
		}
	}

	public void awaitPreparedTransportRestoration(long deadlineNanos) {
		if (transport instanceof HttpBackendProxyTransport http) http.awaitCredentialRestoration(deadlineNanos);
	}

	public synchronized void closeRedisForHandoff() {
		if (!(transport instanceof RedisBackendProxyTransport)) {
			throw new IllegalStateException("Redis backend proxy transport is unavailable");
		}
		retiredTransport = transport;
		transport = null;
		try {
			((RedisBackendProxyTransport) retiredTransport).closeForHandoff();
		} catch (RuntimeException failure) {
			if (failure instanceof RedisBackendProxyTransport.HandoffQuiescenceException) {
				transport = retiredTransport;
				retiredTransport = null;
			}
			// Retain the fenced old listener so a later manager close can retry its
			// cleanup without ever touching the promoted replacement.
			throw failure;
		}
	}

	public void activateRedisAfterHandoff() {
		if (!(transport instanceof RedisBackendProxyTransport)) {
			throw new IllegalStateException("Redis replacement transport is unavailable");
		}
		((RedisBackendProxyTransport) transport).activateAfterHandoff();
	}

	/** Fences the old Redis listener before promoting the validated standby. */
	public void completeRedisHandoff(BackendProxyTransportManager replacement) {
		java.util.Objects.requireNonNull(replacement, "replacement");
		try {
			closeRedisForHandoff();
		} catch (RuntimeException retirementFailure) {
			if (retirementFailure instanceof RedisBackendProxyTransport.HandoffQuiescenceException) {
				throw retirementFailure;
			}
			// The old listener is already fenced. Treat failure to join it as cleanup
			// degradation; promotion remains safe because callbacks can no longer enter.
			if (plugin != null) {
				plugin.getLogger().warning("Previous Redis backend listener did not stop cleanly after handoff");
				plugin.debug(retirementFailure);
			}
		}
		try {
			replacement.activateRedisAfterHandoff();
		} catch (RuntimeException activationFailure) {
			restoreRetiredRedisAfterFailedHandoff(activationFailure);
			throw activationFailure;
		}
		closeRetiredRedisAfterHandoff();
	}

	private synchronized void restoreRetiredRedisAfterFailedHandoff(RuntimeException activationFailure) {
		if (!(retiredTransport instanceof RedisBackendProxyTransport redis)) return;
		try {
			redis.restoreAfterFailedHandoff();
			transport = redis;
			retiredTransport = null;
		} catch (RuntimeException restorationFailure) {
			activationFailure.addSuppressed(restorationFailure);
		}
	}

	private synchronized void closeRetiredRedisAfterHandoff() {
		if (retiredTransport == null) return;
		BackendProxyTransport retired = retiredTransport;
		retiredTransport = null;
		try {
			retired.close();
		} catch (RuntimeException cleanupFailure) {
			retiredTransport = retired;
			if (plugin != null) {
				plugin.getLogger().warning("Retired Redis backend listener did not stop cleanly after handoff");
				plugin.debug(cleanupFailure);
			}
		}
	}

	public ClientHandler getClientHandler() {
		return transport instanceof SocketBackendProxyTransport
				? ((SocketBackendProxyTransport) transport).getClientHandler() : null;
	}

	public SocketHandler getSocketHandler() {
		return transport instanceof SocketBackendProxyTransport
				? ((SocketBackendProxyTransport) transport).getSocketHandler() : null;
	}

	public RedisHandler getRedisHandler() {
		return transport instanceof RedisBackendProxyTransport
				? ((RedisBackendProxyTransport) transport).getRedisHandler() : null;
	}

	public MySqlMessenger getBackendMysqlMessenger() {
		return transport instanceof MysqlBackendProxyTransport
				? ((MysqlBackendProxyTransport) transport).getMessenger() : null;
	}

	public MqttHandler getMqttHandler() {
		return transport instanceof MqttBackendProxyTransport
				? ((MqttBackendProxyTransport) transport).getMqttHandler() : null;
	}
}
