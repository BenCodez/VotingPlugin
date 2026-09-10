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
	private static final int MAX_ASYNC_HANDOFF_SENDS = 6144;
	private static final int PLUGIN_MESSAGE_HANDOFF_BATCH_SIZE = 32;

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	private BackendProxyTransport transport;
	private BackendProxyTransport preparedTransport;
	private BackendProxyTransport retiredTransport;
	private BackendProxyTransportManager forwardingManager;
	private final java.util.ArrayDeque<JsonEnvelope> preparedSends = new java.util.ArrayDeque<>();
	private final java.util.ArrayDeque<JsonEnvelope> asyncHandoffSends = new java.util.ArrayDeque<>();
	private Thread asyncHandoffWorker;
	private boolean pluginMessageHandoffScheduled;
	private long handoffGeneration;
	private boolean preparedSendFence;
	private boolean rejectPreparedSends;
	private boolean preparedQueueWarning;
	private boolean rejectedSendWarning;
	private boolean asyncHandoffRetryWarning;

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
		if (rejectPreparedSends) {
			if (!rejectedSendWarning) {
				rejectedSendWarning = true;
				plugin.getLogger().severe("Backend proxy transport is disabled; delivery was not accepted");
			}
		} else if (preparedSendFence) {
			acceptPreparedSend(envelope);
		} else if (forwardingManager != null) {
			forwardingManager.send(envelope);
		} else if (hasPendingAsyncHandoff()) {
			acceptQueuedTransportSend(envelope);
		} else if (transport instanceof PluginMessagingBackendProxyTransport) {
			acceptQueuedTransportSend(envelope);
		} else if (transport != null) {
			transport.send(envelope);
		} else if (preparedTransport != null) {
			acceptPreparedSend(envelope);
		}
	}

	private void acceptPreparedSend(JsonEnvelope envelope) {
		if (preparedSends.size() < MAX_PREPARED_SENDS) {
			preparedSends.addLast(envelope);
		} else if (!preparedQueueWarning) {
			preparedQueueWarning = true;
			plugin.getLogger().severe("Backend proxy replacement handoff queue is full; delivery was not accepted");
		}
	}

	public void activateAfterPublication() {
		if (transport != null) transport.activateAfterPublication();
	}

	public synchronized void close() {
		handoffGeneration++;
		if (asyncHandoffWorker != null) asyncHandoffWorker.interrupt();
		asyncHandoffWorker = null;
		pluginMessageHandoffScheduled = false;
		asyncHandoffSends.clear();
		notifyAll();
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
					while (!preparedSends.isEmpty() && transport.send(preparedSends.peekFirst()))
						preparedSends.removeFirst();
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
		if (preparedTransport == null && !preparedSendFence) return;
		BackendProxyTransportManager target = java.util.Objects.requireNonNull(replacement, "replacement");
		java.util.ArrayList<JsonEnvelope> pending = new java.util.ArrayList<>();
		if (preparedTransport instanceof HttpBackendProxyTransport http) {
			pending.addAll(http.preparedMessagesSnapshot());
		}
		pending.addAll(preparedSends);
		target.acceptPreparedHandoffMessages(pending);
		// Do not consume the old queues or forward subsequent sends until the
		// replacement has admitted every snapshot. A failed admission therefore
		// leaves rollback with the complete original FIFO intact.
		if (preparedTransport instanceof HttpBackendProxyTransport http) http.drainPreparedMessages();
		preparedSends.clear();
		forwardingManager = target;
		preparedSendFence = false;
	}

	/** Prevents disabling from discarding a delivery accepted during HTTP preparation. */
	public synchronized boolean commitPreparedDisable() {
		if (preparedTransport instanceof HttpBackendProxyTransport http && http.preparedMessageCount() != 0) return false;
		if (!preparedSends.isEmpty()) return false;
		rejectPreparedSends = true;
		return true;
	}

	public synchronized boolean hasPendingAsyncHandoff() {
		return !asyncHandoffSends.isEmpty() || asyncHandoffWorker != null || pluginMessageHandoffScheduled;
	}

	/** Drains an older handoff, then atomically buffers sends until publication. */
	public synchronized void prepareAsyncHandoffForReplacement(long deadlineNanos) {
		awaitAsyncHandoff(deadlineNanos);
		preparedSendFence = true;
	}

	/** Waits off-thread so a later replacement cannot clear an unfinished admitted handoff. */
	public synchronized void awaitAsyncHandoff(long deadlineNanos) {
		while (hasPendingAsyncHandoff()) {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0)
				throw new IllegalStateException("Timed out waiting for pending backend proxy deliveries");
			try {
				java.util.concurrent.TimeUnit.NANOSECONDS.timedWait(this, remaining);
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException(
						"Interrupted while waiting for pending backend proxy deliveries", interrupted);
			}
		}
	}

	/** Holds staged replacement sends until its predecessor FIFO can be prepended. */
	public synchronized void beginPreparedTransportHandoff() {
		if (transport instanceof HttpBackendProxyTransport http) {
			http.beginPreparedHandoff();
		} else {
			preparedSendFence = true;
		}
	}

	/** Admits the predecessor before staged replacement messages without caller-thread I/O. */
	private void acceptPreparedHandoffMessages(java.util.List<JsonEnvelope> pending) {
		if (transport instanceof HttpBackendProxyTransport http) {
			http.acceptHandoffMessages(pending);
			return;
		}
		Thread worker = null;
		boolean schedulePluginMessages = false;
		long generation = 0;
		synchronized (this) {
			if (!preparedSendFence)
				throw new IllegalStateException("Replacement transport is not awaiting a prepared handoff");
			int admitted = pending.size() + preparedSends.size();
			if (admitted > MAX_ASYNC_HANDOFF_SENDS - asyncHandoffSends.size())
				throw new IllegalStateException("Backend proxy handoff queue exceeded its fixed capacity");
			asyncHandoffSends.addAll(pending);
			asyncHandoffSends.addAll(preparedSends);
			preparedSends.clear();
			preparedSendFence = false;
			if (!asyncHandoffSends.isEmpty() && asyncHandoffWorker == null && !pluginMessageHandoffScheduled) {
				if (transport instanceof PluginMessagingBackendProxyTransport) {
					pluginMessageHandoffScheduled = true;
					schedulePluginMessages = true;
					generation = handoffGeneration;
				} else {
					worker = createAsyncHandoffWorker();
				}
			}
		}
		if (schedulePluginMessages) schedulePluginMessageHandoff(generation);
		else startAsyncHandoffWorker(worker);
	}

	/** Preserves handoff FIFO and keeps plugin-message API access on the primary thread. */
	private void acceptQueuedTransportSend(JsonEnvelope envelope) {
		if (asyncHandoffSends.size() >= MAX_ASYNC_HANDOFF_SENDS) {
			if (!preparedQueueWarning) {
				preparedQueueWarning = true;
				plugin.getLogger().severe("Plugin-message delivery queue is full; delivery was not accepted");
			}
			return;
		}
		asyncHandoffSends.addLast(envelope);
		if (transport instanceof PluginMessagingBackendProxyTransport) {
			if (pluginMessageHandoffScheduled) return;
			pluginMessageHandoffScheduled = true;
			long generation = handoffGeneration;
			try {
				schedulePluginMessageHandoff(generation);
			} catch (RuntimeException failure) {
				pluginMessageHandoffScheduled = false;
				notifyAll();
				throw failure;
			}
		} else if (asyncHandoffWorker == null) {
			startAsyncHandoffWorker(createAsyncHandoffWorker());
		}
	}

	private Thread createAsyncHandoffWorker() {
		Thread worker = new Thread(this::drainAsyncHandoffMessages,
				"VotingPlugin-Backend-Transport-Handoff");
		worker.setDaemon(true);
		asyncHandoffWorker = worker;
		return worker;
	}

	private void startAsyncHandoffWorker(Thread worker) {
		if (worker == null) return;
		try {
			worker.start();
		} catch (RuntimeException failure) {
			synchronized (this) {
				if (asyncHandoffWorker == worker) asyncHandoffWorker = null;
				notifyAll();
			}
			throw failure;
		}
	}

	private void drainAsyncHandoffMessages() {
		try {
			while (!Thread.currentThread().isInterrupted()) {
				JsonEnvelope envelope;
				BackendProxyTransport target;
				synchronized (this) {
					envelope = asyncHandoffSends.peekFirst();
					if (envelope == null) {
						if (asyncHandoffWorker == Thread.currentThread()) asyncHandoffWorker = null;
						notifyAll();
						return;
					}
					target = transport;
				}
				if (target == null) return;
				boolean accepted;
				try {
					accepted = target.send(envelope);
				} catch (RuntimeException sendFailure) {
					accepted = false;
				}
				synchronized (this) {
					if (!accepted) {
						if (!asyncHandoffRetryWarning && plugin != null && plugin.getLogger() != null) {
							asyncHandoffRetryWarning = true;
							plugin.getLogger().warning(
									"Backend proxy handoff delivery was rejected; retaining it for retry");
						}
						try {
							wait(250L);
						} catch (InterruptedException interrupted) {
							Thread.currentThread().interrupt();
							return;
						}
						continue;
					}
					asyncHandoffRetryWarning = false;
					if (asyncHandoffSends.peekFirst() == envelope) asyncHandoffSends.removeFirst();
					notifyAll();
				}
			}
		} finally {
			synchronized (this) {
				if (asyncHandoffWorker == Thread.currentThread()) asyncHandoffWorker = null;
				notifyAll();
			}
		}
	}

	private void schedulePluginMessageHandoff(long generation) {
		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> drainPluginMessageHandoff(generation));
		} catch (RuntimeException failure) {
			synchronized (this) {
				if (handoffGeneration == generation) pluginMessageHandoffScheduled = false;
				notifyAll();
			}
			throw failure;
		}
	}

	private void drainPluginMessageHandoff(long generation) {
		for (int sent = 0; sent < PLUGIN_MESSAGE_HANDOFF_BATCH_SIZE; sent++) {
			JsonEnvelope envelope;
			BackendProxyTransport target;
			synchronized (this) {
				if (handoffGeneration != generation) return;
				envelope = asyncHandoffSends.peekFirst();
				if (envelope == null) {
					pluginMessageHandoffScheduled = false;
					preparedQueueWarning = false;
					notifyAll();
					return;
				}
				target = transport;
			}
			if (!(target instanceof PluginMessagingBackendProxyTransport)) return;
			try {
				if (!target.send(envelope)) {
					schedulePluginMessageHandoff(generation);
					return;
				}
			} catch (RuntimeException failure) {
				synchronized (this) {
					if (handoffGeneration == generation) pluginMessageHandoffScheduled = false;
					notifyAll();
				}
				throw failure;
			}
			synchronized (this) {
				if (handoffGeneration != generation) return;
				if (asyncHandoffSends.peekFirst() == envelope) asyncHandoffSends.removeFirst();
				notifyAll();
			}
		}
		synchronized (this) {
			if (handoffGeneration != generation) return;
			if (asyncHandoffSends.isEmpty()) {
				pluginMessageHandoffScheduled = false;
				preparedQueueWarning = false;
				notifyAll();
				return;
			}
		}
		schedulePluginMessageHandoff(generation);
	}

	/** Reserves replacement capacity for every old queued message and future prepared send. */
	public synchronized void reservePreparedTransportHandoff(BackendProxyTransportManager replacement) {
		if (preparedTransport == null && !preparedSendFence) return;
		if (!(java.util.Objects.requireNonNull(replacement, "replacement").transport
				instanceof HttpBackendProxyTransport target))
			throw new IllegalStateException("HTTP replacement transport is unavailable");
		// send() remains available while the previous credential is fenced. Reserve
		// its whole remaining bounded allowance, not only the current queue size.
		int previousMessages = preparedTransport instanceof HttpBackendProxyTransport previous
				? previous.preparedMessageCount() : 0;
		target.reservePreparedHandoffCapacity(previousMessages + MAX_PREPARED_SENDS);
	}

	public void beginPreparedHttpHandoff() {
		beginPreparedTransportHandoff();
	}

	public synchronized void restorePreparedTransport() {
		if (preparedTransport == null) {
			if (!preparedSendFence) return;
			acceptPreparedHandoffMessages(java.util.Collections.emptyList());
			preparedQueueWarning = false;
			return;
		}
		if (transport != null) return;
		if (preparedTransport instanceof HttpBackendProxyTransport http) {
			transport = http.recreatePrepared();
		} else {
			throw new IllegalStateException("Prepared backend proxy transport cannot be restored");
		}
		preparedTransport = null;
		preparedSendFence = false;
		while (!preparedSends.isEmpty() && transport.send(preparedSends.peekFirst())) preparedSends.removeFirst();
		preparedQueueWarning = false;
	}

	public void restoreAfterFailedReplacement() {
		if (transport instanceof PluginMessagingBackendProxyTransport pluginMessaging) {
			pluginMessaging.restoreAfterFailedReplacement();
		}
		restorePreparedTransport();
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

	public void replayRedisAfterHandoffPublication() {
		if (transport instanceof RedisBackendProxyTransport redis) redis.replayAfterHandoffPublication();
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
