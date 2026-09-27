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
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;

/**
 * Selects and owns the active backend-to-proxy transport.
 */
public class BackendProxyTransportManager {
	private static final int MAX_PREPARED_SENDS = 1024;
	private static final int MAX_ASYNC_HANDOFF_SENDS = 6144;
	private static final int PLUGIN_MESSAGE_HANDOFF_BATCH_SIZE = 32;
	private static final int HTTP_SHUTDOWN_ADAPT_BATCH_SIZE = 32;
	private static final long HTTP_HANDOFF_CLOSE_GRACE_MILLIS = 11_000L;

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	private com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption httpEncryption;
	private BackendProxyTransport transport;
	private BackendProxyTransport preparedTransport;
	private BackendProxyTransport retiredTransport;
	// The old Redis instance is already stopped after a successful worker-side
	// handoff, but keeps its captured handler long enough to restore it if Bukkit
	// publication subsequently fails or is abandoned.
	private RedisBackendProxyTransport completedRedisHandoffTransport;
	private BackendProxyTransportManager forwardingManager;
	private java.util.function.UnaryOperator<JsonEnvelope> forwardingAdapter;
	private final java.util.ArrayDeque<PendingHandoffEnvelope> preparedSends = new java.util.ArrayDeque<>();
	private final java.util.ArrayDeque<PendingHandoffEnvelope> asyncHandoffSends = new java.util.ArrayDeque<>();
	private Thread asyncHandoffWorker;
	private boolean pluginMessageHandoffScheduled;
	private long handoffGeneration;
	private boolean preparedSendFence;
	private boolean rejectPreparedSends;
	private boolean allowStoppedPresenceDuringDisable;
	private boolean preparedQueueWarning;
	private boolean rejectedSendWarning;
	private boolean asyncHandoffRetryWarning;
	private boolean httpHandoffSendInProgress;

	public BackendProxyTransportManager(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache());
	}

	public BackendProxyTransportManager(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.processedVoteCache = processedVoteCache;
	}

	public void setHttpEncryption(
			com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption httpEncryption) {
		this.httpEncryption = httpEncryption;
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
			transport = new HttpBackendProxyTransport(plugin, httpEncryption);
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
			if (allowStoppedPresenceDuringDisable && envelope != null
					&& VotingPluginWire.SUB_BACKEND_STOPPED.equals(envelope.getSubChannel())) {
				allowStoppedPresenceDuringDisable = false;
				BackendProxyTransport stoppingTransport = transport != null ? transport : preparedTransport;
				if (stoppingTransport == null || !stoppingTransport.send(envelope))
					throw new IllegalStateException("Backend stopped presence was not accepted before disabling transport");
				return;
			}
			if (!rejectedSendWarning) {
				rejectedSendWarning = true;
				plugin.getLogger().severe("Backend proxy transport is disabled; delivery was not accepted");
			}
		} else if (preparedSendFence) {
			acceptPreparedSend(envelope);
		} else if (forwardingManager != null) {
			forwardingManager.acceptForwardedSend(envelope, forwardingAdapter);
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

	public synchronized void updateSharedTransportSecurity(SharedTransportEnvelopeAuthenticator authenticator,
			com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption encryption) {
		if (transport instanceof RedisBackendProxyTransport redis) redis.updateSecurity(authenticator, encryption);
		else if (transport instanceof MqttBackendProxyTransport mqtt) mqtt.updateSecurity(authenticator, encryption);
		else throw new IllegalStateException("No active shared backend transport to update");
	}

	public synchronized boolean hasEquivalentSharedTransportAuthenticator(
			SharedTransportEnvelopeAuthenticator authenticator) {
		SharedInboundPolicy policy = sharedInboundPolicySnapshot();
		return policy != null && policy.authenticator() != null
				&& policy.authenticator().hasEquivalentInboundPolicy(authenticator);
	}

	public synchronized boolean hasEquivalentSharedTransportEncryption(
			com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption encryption) {
		SharedInboundPolicy policy = sharedInboundPolicySnapshot();
		return policy != null && policy.encryption() != null
				&& policy.encryption().hasEquivalentInboundPolicy(encryption);
	}

	private synchronized SharedInboundPolicy sharedInboundPolicySnapshot() {
		if (transport instanceof RedisBackendProxyTransport redis) return redis.sharedInboundPolicySnapshot();
		if (transport instanceof MqttBackendProxyTransport mqtt) return mqtt.sharedInboundPolicySnapshot();
		return null;
	}

	/** Compares broker authentication and the destination bound into its MAC without nesting manager locks. */
	public boolean hasEquivalentSharedInboundPolicy(BackendProxyTransportManager other) {
		if (other == null) return false;
		SharedInboundPolicy current = sharedInboundPolicySnapshot();
		SharedInboundPolicy restored = other.sharedInboundPolicySnapshot();
		return current != null && current.hasEquivalentPolicy(restored);
	}

	private void acceptPreparedSend(JsonEnvelope envelope) {
		acceptPreparedSend(PendingHandoffEnvelope.direct(envelope));
	}

	private void acceptPreparedSend(PendingHandoffEnvelope envelope) {
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

	public void close() {
		awaitNonHttpHandoffBeforeClose();
		synchronized (this) {
			closeLocked();
		}
	}

	/** Gives restored broker/socket sends a bounded chance to leave before their transport is detached. */
	private void awaitNonHttpHandoffBeforeClose() {
		synchronized (this) {
			if (transport instanceof HttpBackendProxyTransport
					|| transport instanceof PluginMessagingBackendProxyTransport
					|| !hasPendingAsyncHandoff()) return;
		}
		try {
			awaitAsyncHandoff(System.nanoTime()
					+ java.util.concurrent.TimeUnit.MILLISECONDS.toNanos(HTTP_HANDOFF_CLOSE_GRACE_MILLIS));
		} catch (IllegalStateException failure) {
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().severe(
						"Backend proxy handoff did not drain within the bounded shutdown grace; delivery remains at risk");
				plugin.debug(failure);
			}
		}
	}

	/** Captures all manager-owned state while preventing a concurrent send admission. */
	private void closeLocked() {
		boolean interruptedWhileAwaitingHttpAdmission = false;
		while (httpHandoffSendInProgress) {
			try {
				wait();
			} catch (InterruptedException interrupted) {
				interruptedWhileAwaitingHttpAdmission = true;
			}
		}
		if (interruptedWhileAwaitingHttpAdmission) Thread.currentThread().interrupt();
		handoffGeneration++;
		if (asyncHandoffWorker != null) asyncHandoffWorker.interrupt();
		asyncHandoffWorker = null;
		pluginMessageHandoffScheduled = false;
		java.util.ArrayDeque<PendingHandoffEnvelope> httpShutdownHandoff = new java.util.ArrayDeque<>();
		if (transport instanceof HttpBackendProxyTransport && !asyncHandoffSends.isEmpty()) {
			httpShutdownHandoff.addAll(asyncHandoffSends);
		}
		asyncHandoffSends.clear();
		notifyAll();
		if (transport != null) {
			if (transport instanceof HttpBackendProxyTransport http) {
				startHttpCloseWithPendingHandoff(http, httpShutdownHandoff);
			} else {
				transport.close();
			}
			transport = null;
		}
		if (preparedTransport != null) {
			preparedTransport.close();
			preparedTransport = null;
		}
		if (retiredTransport != null) {
			BackendProxyTransport retired = retiredTransport;
			if (retired instanceof RedisBackendProxyTransport redis) {
				// A failed Redis listener shutdown can spend its bounded join timeout in
				// close(). Publication calls the predecessor's close on Bukkit, so retry
				// that fenced cleanup independently instead of stalling publication.
				retiredTransport = null;
				closeRetiredRedisAsync(redis);
			} else try {
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
		completedRedisHandoffTransport = null;
		if (forwardingManager == null) preparedSends.clear();
	}

	/** Resolves wire-policy adapters off the server thread before HTTP performs its bounded final flush. */
	private void startHttpCloseWithPendingHandoff(HttpBackendProxyTransport http,
			java.util.ArrayDeque<PendingHandoffEnvelope> pending) {
		Thread cleanup = new Thread(() -> {
			try {
				while (!pending.isEmpty()) {
					java.util.ArrayList<JsonEnvelope> resolved =
							new java.util.ArrayList<>(HTTP_SHUTDOWN_ADAPT_BATCH_SIZE);
					for (int index = 0; index < HTTP_SHUTDOWN_ADAPT_BATCH_SIZE; index++) {
						PendingHandoffEnvelope envelope = pending.pollFirst();
						if (envelope == null) break;
						try {
							resolved.add(envelope.resolve());
						} catch (RuntimeException failure) {
							if (plugin != null && plugin.getLogger() != null) {
								plugin.getLogger().severe(
										"Unable to adapt one HTTP handoff message for the final shutdown flush");
								plugin.debug(failure);
							}
						}
					}
					if (!resolved.isEmpty()) http.appendShutdownHandoffMessages(resolved);
				}
			} catch (RuntimeException failure) {
				if (plugin != null && plugin.getLogger() != null) {
					plugin.getLogger().severe("Unable to prepare HTTP handoff messages for the final shutdown flush");
					plugin.debug(failure);
				}
			} finally {
				http.closeAndAwaitFinalHandoff();
			}
		}, "VotingPlugin-Backend-HTTP-Handoff-Close");
		cleanup.setDaemon(true);
		cleanup.start();
		retainHttpCleanupThroughJvmShutdown(cleanup);
	}

	/** Keeps the bounded daemon cleanup alive without waiting on the Bukkit lifecycle thread. */
	private void retainHttpCleanupThroughJvmShutdown(Thread cleanup) {
		Thread owner = new Thread(() -> awaitHttpHandoffCleanup(cleanup),
				"VotingPlugin-Backend-HTTP-Handoff-Owner");
		owner.setDaemon(false);
		owner.start();
	}

	private void awaitHttpHandoffCleanup(Thread cleanup) {
		if (cleanup == null) return;
		try {
			cleanup.join(HTTP_HANDOFF_CLOSE_GRACE_MILLIS);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		}
		if (cleanup.isAlive()) {
			cleanup.interrupt();
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().severe(
						"HTTP handoff final flush exceeded its bounded shutdown grace; delivery remains at risk");
			}
		}
	}

	/** Retries only a fenced retired Redis listener off the Bukkit publication callback. */
	private void closeRetiredRedisAsync(RedisBackendProxyTransport retired) {
		Thread cleanup = new Thread(() -> {
			RuntimeException cleanupFailure = null;
			for (int attempt = 0; attempt < 3; attempt++) {
				try {
					retired.close();
					return;
				} catch (RuntimeException failure) {
					cleanupFailure = failure;
					if (attempt == 2) break;
					try {
						Thread.sleep(250L);
					} catch (InterruptedException interrupted) {
						Thread.currentThread().interrupt();
						break;
					}
				}
			}
			if (cleanupFailure != null && plugin != null) {
				plugin.getLogger().warning("Retired Redis backend listener did not stop cleanly during async cleanup");
				plugin.debug(cleanupFailure);
			}
		}, "VotingPlugin-Retired-Redis-Cleanup");
		cleanup.setDaemon(true);
		cleanup.start();
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
				MqttBackendProxyTransport mqtt = candidate instanceof MqttBackendProxyTransport prepared
						? prepared : null;
				if ((http != null && !http.isClosedForReplacement()) || (mqtt != null && mqtt.isConnected())) {
					// A failed flush deliberately restarts the existing connector. Reinstall
					// that live instance instead of creating a second directory owner/client.
					transport = candidate;
					preparedTransport = null;
					preparedSendFence = false;
					resumePreparedSends();
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
		completePreparedTransportHandoff(replacement, null);
	}

	/** Transfers the predecessor FIFO for worker-side adaptation to the replacement wire policy. */
	public synchronized void completePreparedTransportHandoff(BackendProxyTransportManager replacement,
			java.util.function.UnaryOperator<JsonEnvelope> adapter) {
		if (preparedTransport == null && !preparedSendFence) return;
		BackendProxyTransportManager target = java.util.Objects.requireNonNull(replacement, "replacement");
		java.util.ArrayList<PendingHandoffEnvelope> pending = new java.util.ArrayList<>();
		if (preparedTransport instanceof HttpBackendProxyTransport http) {
			for (JsonEnvelope envelope : http.preparedMessagesSnapshot()) {
				pending.add(adapter == null ? PendingHandoffEnvelope.direct(envelope)
						: PendingHandoffEnvelope.adapted(envelope, adapter));
			}
		}
		for (PendingHandoffEnvelope envelope : preparedSends) pending.add(envelope.then(adapter));
		target.acceptPreparedHandoffMessages(pending);
		// Do not consume the old queues or forward subsequent sends until the
		// replacement has admitted every snapshot. A failed admission therefore
		// leaves rollback with the complete original FIFO intact.
		if (preparedTransport instanceof HttpBackendProxyTransport http) http.drainPreparedMessages();
		preparedSends.clear();
		forwardingManager = target;
		forwardingAdapter = adapter;
		preparedSendFence = false;
	}

	/** Prevents disabling from discarding a delivery accepted during HTTP preparation. */
	public synchronized boolean commitPreparedDisable() {
		if (preparedTransport instanceof HttpBackendProxyTransport http && http.preparedMessageCount() != 0) return false;
		if (!preparedSends.isEmpty()) return false;
		rejectPreparedSends = true;
		return true;
	}

	/** Fences ordinary sends while allowing one final stopped-presence envelope. */
	public synchronized void beginPreparedDisable() {
		rejectPreparedSends = true;
		allowStoppedPresenceDuringDisable = true;
	}

	/** Reopens delivery when a prepared disable is rolled back. */
	public synchronized void cancelPreparedDisable() {
		rejectPreparedSends = false;
		allowStoppedPresenceDuringDisable = false;
		rejectedSendWarning = false;
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
		if (transport instanceof HttpBackendProxyTransport http) http.beginPreparedHandoff();
		preparedSendFence = true;
	}

	/** Admits the predecessor before staged replacement messages without caller-thread I/O. */
	private void acceptPreparedHandoffMessages(java.util.List<PendingHandoffEnvelope> pending) {
		Thread worker = null;
		boolean schedulePluginMessages = false;
		long generation = 0;
		synchronized (this) {
			if (!preparedSendFence)
				throw new IllegalStateException("Replacement transport is not awaiting a prepared handoff");
			int admitted = pending.size() + preparedSends.size();
			if (admitted > MAX_ASYNC_HANDOFF_SENDS - asyncHandoffSends.size())
				throw new IllegalStateException("Backend proxy handoff queue exceeded its fixed capacity");
			if (transport instanceof HttpBackendProxyTransport http) {
				// Presence activation can add replacement-side sends after the predecessor's
				// provisional reservation. Validate the complete atomic admission before
				// consuming either side's queue.
				http.reservePreparedHandoffCapacity(admitted + asyncHandoffSends.size());
			}
			asyncHandoffSends.addAll(pending);
			asyncHandoffSends.addAll(preparedSends);
			preparedSends.clear();
			preparedSendFence = false;
			if (transport instanceof HttpBackendProxyTransport http) http.activatePreparedHandoffDrain();
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
	private void acceptForwardedSend(JsonEnvelope envelope,
			java.util.function.UnaryOperator<JsonEnvelope> adapter) {
		BackendProxyTransportManager successor;
		java.util.function.UnaryOperator<JsonEnvelope> successorAdapter;
		synchronized (this) {
			successor = forwardingManager;
			if (successor == null) {
				PendingHandoffEnvelope pending = adapter == null ? PendingHandoffEnvelope.direct(envelope)
						: PendingHandoffEnvelope.adapted(envelope, adapter);
				if (preparedSendFence || preparedTransport != null) acceptPreparedSend(pending);
				else acceptQueuedTransportSend(pending);
				return;
			}
			successorAdapter = composeAdapters(adapter, forwardingAdapter);
		}
		successor.acceptForwardedSend(envelope, successorAdapter);
	}

	private static java.util.function.UnaryOperator<JsonEnvelope> composeAdapters(
			java.util.function.UnaryOperator<JsonEnvelope> first,
			java.util.function.UnaryOperator<JsonEnvelope> second) {
		if (first == null) return second;
		if (second == null) return first;
		return envelope -> second.apply(first.apply(envelope));
	}

	private void acceptQueuedTransportSend(JsonEnvelope envelope) {
		acceptQueuedTransportSend(PendingHandoffEnvelope.direct(envelope));
	}

	private void acceptQueuedTransportSend(PendingHandoffEnvelope envelope) {
		boolean httpHandoffFull = transport instanceof HttpBackendProxyTransport http
				&& http.isManagerHandoffActive()
				&& !http.canAcceptManagerHandoff(asyncHandoffSends.size());
		if (asyncHandoffSends.size() >= MAX_ASYNC_HANDOFF_SENDS || httpHandoffFull) {
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
				PendingHandoffEnvelope pending;
				BackendProxyTransport target;
				boolean httpAdmission;
				synchronized (this) {
					pending = asyncHandoffSends.peekFirst();
					if (pending == null) {
						if (transport instanceof HttpBackendProxyTransport http) http.finishPreparedHandoffDrain();
						if (asyncHandoffWorker == Thread.currentThread()) asyncHandoffWorker = null;
						notifyAll();
						return;
					}
					target = transport;
					httpAdmission = target instanceof HttpBackendProxyTransport;
					if (httpAdmission) httpHandoffSendInProgress = true;
				}
				if (target == null) return;
				JsonEnvelope envelope;
				boolean resolved = false;
				try {
					envelope = pending.resolve();
					resolved = true;
				} catch (RuntimeException adaptationFailure) {
					synchronized (this) {
						if (!asyncHandoffRetryWarning && plugin != null && plugin.getLogger() != null) {
							asyncHandoffRetryWarning = true;
							plugin.getLogger().warning(
									"Backend proxy handoff adaptation failed; retaining it for retry");
						}
						try {
							wait(250L);
						} catch (InterruptedException interrupted) {
							Thread.currentThread().interrupt();
							return;
						}
					}
					continue;
				} finally {
					if (httpAdmission && !resolved) releaseHttpHandoffAdmission();
				}
				if (Thread.currentThread().isInterrupted()) {
					if (httpAdmission) releaseHttpHandoffAdmission();
					return;
				}
				if (target instanceof PluginMessagingBackendProxyTransport) {
					java.util.List<PendingHandoffEnvelope> lookahead = new java.util.ArrayList<>();
					synchronized (this) {
						for (PendingHandoffEnvelope queued : asyncHandoffSends) {
							if (lookahead.size() >= PLUGIN_MESSAGE_HANDOFF_BATCH_SIZE) break;
							lookahead.add(queued);
						}
					}
					for (PendingHandoffEnvelope queued : lookahead) {
						if (Thread.currentThread().isInterrupted()) return;
						try {
							queued.resolve();
						} catch (RuntimeException adaptationFailure) {
							break;
						}
					}
					boolean schedule;
					long generation;
					synchronized (this) {
						if (asyncHandoffWorker == Thread.currentThread()) asyncHandoffWorker = null;
						schedule = !pluginMessageHandoffScheduled;
						if (schedule) pluginMessageHandoffScheduled = true;
						generation = handoffGeneration;
					}
					if (schedule) try {
						schedulePluginMessageHandoff(generation);
					} catch (RuntimeException schedulingFailure) {
						synchronized (this) {
							if (asyncHandoffWorker == null) asyncHandoffWorker = Thread.currentThread();
							if (!asyncHandoffRetryWarning && plugin != null && plugin.getLogger() != null) {
								asyncHandoffRetryWarning = true;
								plugin.getLogger().warning(
										"Plugin-message handoff scheduling failed; retaining it for retry");
							}
							try {
								wait(250L);
							} catch (InterruptedException interrupted) {
								Thread.currentThread().interrupt();
								return;
							}
						}
						continue;
					}
					synchronized (this) {
						asyncHandoffRetryWarning = false;
						notifyAll();
					}
					return;
				}
				boolean accepted;
				boolean sendReturned = false;
				try {
					accepted = target.send(envelope);
					sendReturned = true;
				} catch (RuntimeException sendFailure) {
					accepted = false;
					sendReturned = true;
				} finally {
					if (httpAdmission && !sendReturned) synchronized (this) {
						httpHandoffSendInProgress = false;
						notifyAll();
					}
				}
				synchronized (this) {
					if (httpAdmission) {
						httpHandoffSendInProgress = false;
						notifyAll();
					}
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
							notifyAll();
							return;
						}
						continue;
					}
					asyncHandoffRetryWarning = false;
					if (asyncHandoffSends.peekFirst() == pending) asyncHandoffSends.removeFirst();
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

	private synchronized void releaseHttpHandoffAdmission() {
		httpHandoffSendInProgress = false;
		notifyAll();
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
			PendingHandoffEnvelope pending;
			BackendProxyTransport target;
			synchronized (this) {
				if (handoffGeneration != generation) return;
				pending = asyncHandoffSends.peekFirst();
				if (pending == null) {
					pluginMessageHandoffScheduled = false;
					preparedQueueWarning = false;
					notifyAll();
					return;
				}
				target = transport;
			}
			JsonEnvelope envelope = pending.resolved();
			if (envelope == null) {
				Thread worker;
				synchronized (this) {
					if (handoffGeneration != generation) return;
					pluginMessageHandoffScheduled = false;
					worker = asyncHandoffWorker == null ? createAsyncHandoffWorker() : null;
				}
				startAsyncHandoffWorker(worker);
				return;
			}
			if (!(target instanceof PluginMessagingBackendProxyTransport)) {
				synchronized (this) {
					if (handoffGeneration == generation) pluginMessageHandoffScheduled = false;
					notifyAll();
				}
				return;
			}
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
				if (asyncHandoffSends.peekFirst() == pending) asyncHandoffSends.removeFirst();
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
		BackendProxyTransportManager target = java.util.Objects.requireNonNull(replacement, "replacement");
		if (!(target.transport instanceof HttpBackendProxyTransport))
			throw new IllegalStateException("HTTP replacement transport is unavailable");
		// send() remains available while the previous credential is fenced. Reserve
		// its whole remaining bounded allowance, not only the current queue size.
		int previousMessages = preparedTransport instanceof HttpBackendProxyTransport previous
				? previous.preparedMessageCount() : 0;
		target.reservePreparedHandoffCapacity(previousMessages + MAX_PREPARED_SENDS);
	}

	private synchronized void reservePreparedHandoffCapacity(int messages) {
		if (!preparedSendFence)
			throw new IllegalStateException("HTTP replacement transport is not awaiting a prepared handoff");
		if (messages < 0 || messages > MAX_ASYNC_HANDOFF_SENDS - preparedSends.size())
			throw new IllegalStateException("HTTP prepared handoff exceeds its fixed capacity");
		if (transport instanceof HttpBackendProxyTransport http) {
			http.reservePreparedHandoffCapacity(messages);
		}
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
		} else if (preparedTransport instanceof SocketBackendProxyTransport socket) {
			socket.restoreAfterFailedReplacement();
			transport = socket;
		} else if (preparedTransport instanceof MqttBackendProxyTransport mqtt) {
			mqtt.restoreAfterFailedReplacement();
			transport = mqtt;
		} else {
			throw new IllegalStateException("Prepared backend proxy transport cannot be restored");
		}
		preparedTransport = null;
		preparedSendFence = false;
		resumePreparedSends();
		preparedQueueWarning = false;
	}

	/** Restores fenced sends through the worker so composed crypto adapters never run on Bukkit. */
	private void resumePreparedSends() {
		if (preparedSends.isEmpty()) return;
		if (preparedSends.size() > MAX_ASYNC_HANDOFF_SENDS - asyncHandoffSends.size())
			throw new IllegalStateException("Backend proxy handoff queue exceeded its fixed capacity");
		if (transport instanceof HttpBackendProxyTransport http)
			http.activateRestoredHandoffDrain(asyncHandoffSends.size() + preparedSends.size());
		asyncHandoffSends.addAll(preparedSends);
		preparedSends.clear();
		if (asyncHandoffWorker == null && !pluginMessageHandoffScheduled) {
			Thread worker = createAsyncHandoffWorker();
			startAsyncHandoffWorker(worker);
		}
	}

	public void restoreAfterFailedReplacement() {
		restoreAfterFailedReplacement(null);
	}

	/** Restores an old Redis listener together with a promoted replacement's unplayed replay FIFO. */
	public void restoreAfterFailedReplacement(BackendProxyTransportManager failedReplacement) {
		java.util.List<JsonEnvelope> replacementReplay = failedReplacement == null
				? java.util.Collections.emptyList() : failedReplacement.detachRedisReplayForFailedHandoff();
		restoreRetiredRedisAfterFailedHandoff(replacementReplay);
		restoreCompletedRedisAfterFailedHandoff(replacementReplay);
		if (transport instanceof PluginMessagingBackendProxyTransport pluginMessaging) {
			pluginMessaging.restoreAfterFailedReplacement();
		}
		restorePreparedTransport();
	}

	private synchronized java.util.List<JsonEnvelope> detachRedisReplayForFailedHandoff() {
		if (!(transport instanceof RedisBackendProxyTransport redis)) return java.util.Collections.emptyList();
		return redis.detachReplayForFailedHandoff();
	}

	public void awaitPreparedTransportRestoration(long deadlineNanos) {
		if (transport instanceof HttpBackendProxyTransport http) http.awaitCredentialRestoration(deadlineNanos);
	}

	/** Requires an off-thread drain before abandoning active Redis replay for another transport. */
	public synchronized boolean hasPendingRedisReplay() {
		return transport instanceof RedisBackendProxyTransport redis && redis.hasPendingReplayForReplacement();
	}

	public boolean prepareRedisReplayTransition(BungeeMethod replacementMethod, long deadlineNanos) {
		RedisBackendProxyTransport redis;
		synchronized (this) {
			if (replacementMethod == BungeeMethod.REDIS || !(transport instanceof RedisBackendProxyTransport)) return true;
			redis = (RedisBackendProxyTransport) transport;
		}
		if (!redis.awaitReplayDrainForNonRedisReplacement(deadlineNanos)) return false;
		synchronized (this) {
			if (transport != redis) return false;
			preparedSendFence = true;
		}
		return true;
	}

	public java.util.List<JsonEnvelope> closeRedisForHandoff(BackendProxyTransportManager replacement) {
		RedisBackendProxyTransport retiring;
		synchronized (this) {
			if (!(transport instanceof RedisBackendProxyTransport)) {
				throw new IllegalStateException("Redis backend proxy transport is unavailable");
			}
			retiring = (RedisBackendProxyTransport) transport;
			retiredTransport = retiring;
			// The Redis listener is fenced off-thread before Bukkit publishes the
			// staged replacement. Keep sends accepted in that interval in the same
			// bounded predecessor FIFO instead of dropping them once transport is
			// detached below. Publication transfers this queue ahead of messages the
			// staged replacement accepted, while rollback drains it through the
			// restored Redis transport.
			preparedSendFence = true;
		}
		java.util.List<JsonEnvelope> replay = retiring.freezeReplayForSuccessiveHandoff();
		try {
			replacement.acceptRedisReplayFromPreviousHandoff(replay);
		} catch (RuntimeException admissionFailure) {
			retiring.restoreFrozenReplayAfterFailedSuccessiveHandoff(replay);
			synchronized (this) {
				if (retiredTransport == retiring) retiredTransport = null;
			}
			throw admissionFailure;
		}
		try {
			// closeForHandoff() waits for already-running Redis callbacks. Those callbacks
			// may publish a reply through send(), so never retain this manager's monitor
			// while waiting for them to drain.
			retiring.closeForHandoff();
		} catch (RuntimeException failure) {
			synchronized (this) {
				if (failure instanceof RedisBackendProxyTransport.HandoffQuiescenceException
						&& retiredTransport == retiring && transport == retiring) {
					replacement.removeRedisReplayFromPreviousHandoff(replay);
					retiring.restoreFrozenReplayAfterFailedSuccessiveHandoff(replay);
					retiredTransport = null;
				} else if (transport == retiring) {
					transport = null;
				}
			}
			if (failure instanceof RedisBackendProxyTransport.HandoffQuiescenceException) throw failure;
			// Retain the fenced old listener so a later manager close can retry its
			// cleanup without ever touching the promoted replacement.
			if (plugin != null) {
				plugin.getLogger().warning("Previous Redis backend listener did not stop cleanly after handoff");
				plugin.debug(failure);
			}
		}
		synchronized (this) {
			if (transport == retiring) transport = null;
		}
		return replay;
	}

	private void acceptRedisReplayFromPreviousHandoff(java.util.List<JsonEnvelope> replay) {
		if (transport instanceof RedisBackendProxyTransport redis) redis.acceptReplayFromPreviousHandoff(replay);
		else if (!replay.isEmpty()) throw new IllegalStateException("Redis replacement transport is unavailable");
	}

	private void removeRedisReplayFromPreviousHandoff(java.util.List<JsonEnvelope> replay) {
		if (transport instanceof RedisBackendProxyTransport redis) redis.removeReplayFromPreviousHandoff(replay);
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
		java.util.List<JsonEnvelope> replay = closeRedisForHandoff(replacement);
		try {
			replacement.activateRedisAfterHandoff();
		} catch (RuntimeException activationFailure) {
			java.util.List<JsonEnvelope> rollbackReplay = replacement.detachRedisReplayForFailedHandoff();
			restoreRetiredRedisAfterFailedHandoff(activationFailure,
					mergeRedisRollbackReplay(replay, rollbackReplay));
			throw activationFailure;
		}
		closeRetiredRedisAfterHandoff();
	}

	private static java.util.List<JsonEnvelope> mergeRedisRollbackReplay(java.util.List<JsonEnvelope> predecessorReplay,
			java.util.List<JsonEnvelope> standbyReplay) {
		if (standbyReplay.isEmpty()) return predecessorReplay;
		if (predecessorReplay.isEmpty()) return standbyReplay;
		if (standbyReplay.size() >= predecessorReplay.size()
				&& standbyReplay.subList(0, predecessorReplay.size()).equals(predecessorReplay)) return standbyReplay;
		java.util.ArrayList<JsonEnvelope> merged = new java.util.ArrayList<>(
				predecessorReplay.size() + standbyReplay.size());
		merged.addAll(predecessorReplay);
		merged.addAll(standbyReplay);
		if (merged.size() > RedisBackendProxyTransport.MAX_REPLAY_HANDOFF_DELIVERIES)
			throw new IllegalStateException("Redis rollback replay exceeds its bounded handoff capacity");
		return merged;
	}

	private synchronized void restoreRetiredRedisAfterFailedHandoff(RuntimeException activationFailure,
			java.util.List<JsonEnvelope> replacementReplay) {
		try {
			restoreRetiredRedisAfterFailedHandoff(replacementReplay);
		} catch (RuntimeException restorationFailure) {
			activationFailure.addSuppressed(restorationFailure);
		}
	}

	private synchronized void restoreRetiredRedisAfterFailedHandoff(java.util.List<JsonEnvelope> replacementReplay) {
		if (transport != null) return;
		if (!(retiredTransport instanceof RedisBackendProxyTransport redis)) return;
		try {
			redis.restoreAfterFailedHandoff(replacementReplay);
			transport = redis;
			retiredTransport = null;
		} catch (RuntimeException restorationFailure) {
			throw new IllegalStateException("Retired Redis backend listener could not be restored", restorationFailure);
		}
	}

	private synchronized void closeRetiredRedisAfterHandoff() {
		if (retiredTransport == null) return;
		BackendProxyTransport retired = retiredTransport;
		retiredTransport = null;
		try {
			retired.close();
			if (retired instanceof RedisBackendProxyTransport redis) completedRedisHandoffTransport = redis;
		} catch (RuntimeException cleanupFailure) {
			retiredTransport = retired;
			if (plugin != null) {
				plugin.getLogger().warning("Retired Redis backend listener did not stop cleanly after handoff");
				plugin.debug(cleanupFailure);
			}
		}
	}

	/** Restores a worker-retired Redis listener when publication did not commit. */
	private synchronized void restoreCompletedRedisAfterFailedHandoff(java.util.List<JsonEnvelope> replacementReplay) {
		if (transport != null || completedRedisHandoffTransport == null) return;
		RedisBackendProxyTransport retired = completedRedisHandoffTransport;
		try {
			retired.restoreAfterFailedHandoff(replacementReplay);
			transport = retired;
			completedRedisHandoffTransport = null;
		} catch (RuntimeException restorationFailure) {
			throw new IllegalStateException("Retired Redis backend listener could not be restored", restorationFailure);
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

	/** One handed-off envelope whose potentially expensive wire-policy conversion is worker-owned. */
	private static final class PendingHandoffEnvelope {
		private final JsonEnvelope source;
		private final java.util.function.UnaryOperator<JsonEnvelope> adapter;
		private volatile JsonEnvelope resolved;

		private PendingHandoffEnvelope(JsonEnvelope source,
				java.util.function.UnaryOperator<JsonEnvelope> adapter, JsonEnvelope resolved) {
			this.source = java.util.Objects.requireNonNull(source, "source");
			this.adapter = adapter;
			this.resolved = resolved;
		}

		private static PendingHandoffEnvelope direct(JsonEnvelope envelope) {
			return new PendingHandoffEnvelope(envelope, null, envelope);
		}

		private static PendingHandoffEnvelope adapted(JsonEnvelope envelope,
				java.util.function.UnaryOperator<JsonEnvelope> adapter) {
			return new PendingHandoffEnvelope(envelope, java.util.Objects.requireNonNull(adapter, "adapter"), null);
		}

		private PendingHandoffEnvelope then(java.util.function.UnaryOperator<JsonEnvelope> following) {
			if (following == null) return this;
			return adapted(source, composeAdapters(adapter, following));
		}

		private JsonEnvelope resolved() {
			return resolved;
		}

		private synchronized JsonEnvelope resolve() {
			if (resolved == null) resolved = java.util.Objects.requireNonNull(adapter.apply(source), "adapted envelope");
			return resolved;
		}
	}
}
