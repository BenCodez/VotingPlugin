package com.bencodez.votingplugin.backendproxy.transport;

import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayDeque;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicInteger;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.http.HttpBackendTransportConnector;
import com.bencodez.simpleapi.servercomm.http.HttpClientCredentialStore;
import com.bencodez.simpleapi.servercomm.http.HttpConnectionCode;
import com.bencodez.simpleapi.servercomm.http.HttpTlsIdentity;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.DurableFiles;

/** Backend adapter for the secure outbound-only HTTP proxy transport. */
public final class HttpBackendProxyTransport implements BackendProxyTransport {
	private static final int MAX_STARTUP_QUEUE = 1024;
	private static final int MAX_PREPUBLICATION_QUEUE = 2048;
	private static final int MAX_HANDOFF_QUEUE = 4096;
	private static final long DEFAULT_STARTUP_VALIDATION_SECONDS = 25L;
	private static final long ENROLLMENT_RETRY_INITIAL_MILLIS = 1_000L;
	private static final long ENROLLMENT_RETRY_MAX_MILLIS = 60_000L;
	private static final long INCOMING_DISPATCH_SECONDS = 25L;
	private static final long SHUTDOWN_FLUSH_SECONDS = 5L;
	private static final ConcurrentHashMap<Path, Semaphore> DIRECTORY_OWNERS = new ConcurrentHashMap<>();
	private final VotingPluginMain plugin;
	private final Object lifecycle = new Object();
	private final CountDownLatch startupComplete = new CountDownLatch(1);
	private final CountDownLatch credentialRestoreComplete = new CountDownLatch(1);
	private final ArrayDeque<JsonEnvelope> startupQueue = new ArrayDeque<>();
	private final ArrayDeque<JsonEnvelope> handoffQueue = new ArrayDeque<>();
	private volatile Thread handoffWorker;
	private boolean awaitingPreparedHandoff;
	/** Capacity reserved for the prepared predecessor before this transport is published. */
	private int preparedHandoffReservation;
	private volatile HttpBackendTransportConnector connector;
	private volatile Thread worker;
	private volatile RuntimeException startupFailure;
	private volatile RuntimeException credentialRestoreFailure;
	private volatile boolean started;
	private volatile boolean closed;
	/** True only after this transport has crossed the handler publication boundary. */
	private boolean published;
	private volatile boolean restartAfterFailedFlush;
	private final java.util.concurrent.atomic.AtomicBoolean flushRecoveryRunning = new java.util.concurrent.atomic.AtomicBoolean();
	private Path configuredDirectory;
	private String configuredServerId;
	private String configuredConnectionCode;
	private GlobalMessageHandler configuredMessageHandler;
	private HttpClientCredentialStore.ActiveCredentialGeneration configuredCredentialGeneration;
	private HttpClientCredentialStore.ActiveCredentialGeneration credentialGenerationToRestore;
	private boolean retryInitialization;
	private boolean restoreUnenrolledState;
	private boolean inboundActive;
	private Semaphore directoryOwner;
	private final java.util.concurrent.atomic.AtomicBoolean queueWarning = new java.util.concurrent.atomic.AtomicBoolean();

	public HttpBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		start(messageHandler, true);
	}

	@Override
	public void start(GlobalMessageHandler messageHandler, boolean retryInitialization) {
		Path directory = plugin.getDataFolder().toPath().resolve("http");
		String serverId = plugin.getBungeeSettings().getServer();
		String connectionCode = plugin.getBungeeSettings().getHttpConnectionCode();
		start(directory, serverId, connectionCode, messageHandler, null, retryInitialization, false,
				retryInitialization);
	}

	private void start(Path directory, String serverId, String connectionCode,
			GlobalMessageHandler messageHandler) {
		start(directory, serverId, connectionCode, messageHandler, null, true, false, true);
	}

	private void start(Path directory, String serverId, String connectionCode,
			GlobalMessageHandler messageHandler,
			HttpClientCredentialStore.ActiveCredentialGeneration generationToRestore) {
		start(directory, serverId, connectionCode, messageHandler, generationToRestore, false, false, true);
	}

	private void start(Path directory, String serverId, String connectionCode,
			GlobalMessageHandler messageHandler,
			HttpClientCredentialStore.ActiveCredentialGeneration generationToRestore,
			boolean retryInitialization, boolean restoreUnenrolledState, boolean inboundActive) {
		if (generationToRestore == null && !restoreUnenrolledState)
			validateConfiguration(directory, serverId, connectionCode);
		else HttpTlsIdentity.canonicalServerId(serverId);
		configuredDirectory = directory;
		configuredServerId = serverId;
		configuredConnectionCode = connectionCode;
		configuredMessageHandler = messageHandler;
		credentialGenerationToRestore = generationToRestore;
		this.retryInitialization = retryInitialization;
		this.restoreUnenrolledState = restoreUnenrolledState;
		this.inboundActive = inboundActive;
		this.published = inboundActive;
		started = true;
		worker = new Thread(() -> initialize(directory, serverId, connectionCode, messageHandler, retryInitialization,
				restoreUnenrolledState),
				"VotingPlugin-HTTP-Backend-Setup");
		worker.setDaemon(true);
		worker.start();
	}

	HttpBackendProxyTransport recreatePrepared() {
		HttpBackendProxyTransport restored = new HttpBackendProxyTransport(plugin);
		synchronized (lifecycle) {
			// Startup and handoff queues are one FIFO from the caller's perspective.
			// The handoff queue can still contain messages accepted by the previous
			// replacement, so rollback must carry it into the restored transport too.
			restored.startupQueue.addAll(takeQueuedMessages());
		}
		restored.start(configuredDirectory, configuredServerId, configuredConnectionCode, configuredMessageHandler,
				configuredCredentialGeneration, configuredCredentialGeneration == null && retryInitialization,
				restoreUnenrolledState, true);
		return restored;
	}

	boolean isClosedForReplacement() {
		return closed;
	}

	java.util.List<JsonEnvelope> drainPreparedMessages() {
		synchronized (lifecycle) {
			return takeQueuedMessages();
		}
	}

	int preparedMessageCount() {
		synchronized (lifecycle) {
			return startupQueue.size() + handoffQueue.size();
		}
	}

	java.util.List<JsonEnvelope> preparedMessagesSnapshot() {
		synchronized (lifecycle) {
			java.util.List<JsonEnvelope> pending = new java.util.ArrayList<>(startupQueue.size() + handoffQueue.size());
			pending.addAll(startupQueue);
			pending.addAll(handoffQueue);
			return pending;
		}
	}

	/** Takes both pending queues in their original FIFO order for replacement handoff. */
	private java.util.List<JsonEnvelope> takeQueuedMessages() {
		java.util.List<JsonEnvelope> pending = new java.util.ArrayList<>(startupQueue.size() + handoffQueue.size());
		pending.addAll(startupQueue);
		pending.addAll(handoffQueue);
		startupQueue.clear();
		handoffQueue.clear();
		return pending;
	}

	@Override
	public void prepareForReplacement() {
		HttpBackendTransportConnector active;
		Thread cancelledInitialization = null;
		/*
		 * Keep the check, credential snapshot, and cancellation in one lifecycle
		 * critical section.  During ordinary first-time enrollment the setup worker
		 * owns DIRECTORY_OWNERS while it retries and connector is intentionally null.
		 * A Control replacement must be able to cancel that worker before claiming
		 * the same credential directory; there is no credential to preserve in that
		 * state.  If enrollment has already published a credential but the connector
		 * has not yet been installed, retain the generation just as we do for an
		 * established connector.
		 */
		synchronized (lifecycle) {
			if (configuredDirectory == null)
				throw new IllegalStateException("Could not preserve the active HTTP client credential before it became ready");
			active = connector;
			if (active == null && !HttpClientCredentialStore.hasEnrolledProfile(configuredDirectory)) {
				restoreUnenrolledState = true;
				cancelledInitialization = worker;
				closeForReplacement();
			} else try {
				configuredCredentialGeneration = HttpClientCredentialStore.snapshotActiveGeneration(configuredDirectory);
			} catch (Exception failure) {
				throw new IllegalStateException("Could not preserve the active HTTP client credential", failure);
			}
			if (cancelledInitialization == null && active == null) {
				closeForReplacement();
				return;
			}
		}
		if (cancelledInitialization != null) {
			awaitCancelledInitialization(cancelledInitialization);
			captureEnrollmentPublishedDuringCancellation();
			return;
		}
		flushForReplacement(active, System.nanoTime() + TimeUnit.SECONDS.toNanos(SHUTDOWN_FLUSH_SECONDS));
		closeForReplacement();
	}

	private void awaitCancelledInitialization(Thread setup) {
		try { setup.join(TimeUnit.SECONDS.toMillis(SHUTDOWN_FLUSH_SECONDS)); }
		catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted while stopping HTTP client enrollment", interrupted);
		}
		if (setup.isAlive())
			throw new IllegalStateException("Could not stop HTTP client enrollment before replacement");
	}

	private void captureEnrollmentPublishedDuringCancellation() {
		if (!HttpClientCredentialStore.hasEnrolledProfile(configuredDirectory)) return;
		try {
			HttpConnectionCode original = HttpConnectionCode.parse(configuredConnectionCode);
			if (!original.serverId().equals(HttpTlsIdentity.canonicalServerId(configuredServerId))
					|| !HttpClientCredentialStore.matchesEnrollmentCode(configuredDirectory, original))
				throw new IllegalStateException("HTTP client enrollment changed during replacement preparation");
			configuredCredentialGeneration = HttpClientCredentialStore.snapshotActiveGeneration(configuredDirectory);
			restoreUnenrolledState = false;
		} catch (IllegalStateException failure) { throw failure; }
		catch (Exception failure) {
			throw new IllegalStateException("Could not preserve the completed HTTP client enrollment", failure);
		}
	}

	void flushForReplacement(HttpBackendTransportConnector connector, long deadlineNanos) {
		if (connector.flushOutgoing(deadlineNanos)) return;
		restartAfterFailedFlush = true;
		connector.start();
		if (flushRecoveryRunning.compareAndSet(false, true)) {
			Thread recovery = new Thread(() -> resumeAfterFailedFlush(connector),
					"VotingPlugin-HTTP-Backend-Flush-Recovery");
			recovery.setDaemon(true);
			recovery.start();
		}
		throw new IllegalStateException("Could not drain the active HTTP transport before replacement");
	}

	private void resumeAfterFailedFlush(HttpBackendTransportConnector active) {
		try {
			// start() intentionally does nothing while the interrupted long-poll worker is
			// still winding down. Keep a single recovery owner alive until this transport is
			// closed or replaced so the connector's already-accepted queue cannot be stranded.
			while (!closed && connector == active) {
				active.start();
				synchronized (lifecycle) {
					while (!startupQueue.isEmpty() && active.send(startupQueue.peekFirst())) startupQueue.removeFirst();
				}
				try { TimeUnit.SECONDS.sleep(1); }
				catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); break; }
			}
		} finally {
			flushRecoveryRunning.set(false);
			if (closed || connector != active) restartAfterFailedFlush = false;
		}
	}

	private void initialize(Path directory, String serverId, String configuredCode,
			GlobalMessageHandler messageHandler, boolean retryEnrollment, boolean restoreUnenrolledState) {
		Path ownerKey = directory.toAbsolutePath().normalize();
		Semaphore owner = DIRECTORY_OWNERS.computeIfAbsent(ownerKey, ignored -> new Semaphore(1));
		boolean acquired = false, installed = false;
		HttpBackendTransportConnector replacement = null;
		try {
			owner.acquire();
			acquired = true;
			synchronized (lifecycle) {
				if (closed) {
					if (credentialGenerationToRestore != null || restoreUnenrolledState)
						credentialRestoreFailure = new IllegalStateException(
								"Previous HTTP client credential state restoration was cancelled");
					return;
				}
			}
			try {
				if (restoreUnenrolledState)
					restoreUnenrolledCredentialState(directory, serverId, configuredCode);
				if (credentialGenerationToRestore != null) {
					HttpClientCredentialStore.restoreActiveGenerationAfterReplacement(directory,
							credentialGenerationToRestore);
				}
			} catch (Exception failure) {
				credentialRestoreFailure = new IllegalStateException(
						"Could not restore the previous HTTP client credential state", failure);
				throw failure;
			}
			credentialRestoreComplete.countDown();
			// An enrolled rollback resumes the validated generation without replaying its
			// temporary code. An un-enrolled rollback reuses the original startup code
			// only after the staged credential has been made inactive.
			HttpConnectionCode code = enrollmentCode(directory, serverId,
					credentialGenerationToRestore == null ? configuredCode : null);
			if (code != null && !enrollForStartup(code, serverId, directory, retryEnrollment,
					this::waitForEnrollmentRetry)) return;
			HttpClientCredentialStore.EnrolledClient enrolled = HttpClientCredentialStore.loadEnrolled(directory);
			if (!enrolled.profile().serverId().equals(HttpTlsIdentity.canonicalServerId(serverId)))
				throw new IllegalStateException("Persisted HTTP identity belongs to a different backend Server name");
			replacement = new HttpBackendTransportConnector(directory, envelope -> {
				dispatchAfterPublication(messageHandler, envelope);
			});
			invokeConnectorLifecycle(replacement, "startPaused");
			boolean discard = false;
			synchronized (lifecycle) {
				if (closed) {
					discard = true;
				} else {
					transferStartupQueue(startupQueue, replacement);
					connector = replacement;
					directoryOwner = owner;
					installed = true;
					if (inboundActive) invokeConnectorLifecycle(replacement, "activateIncoming");
				}
			}
			if (discard) replacement.close();
		} catch (Exception failure) {
			startupFailure = new IllegalStateException("Secure HTTP backend enrollment or connection failed", failure);
			plugin.getLogger().severe("Secure HTTP backend transport is unavailable; check the connection code and proxy endpoint");
		} finally {
			credentialRestoreComplete.countDown();
			if (!installed) {
				if (replacement != null) replacement.close();
				if (acquired) owner.release();
			}
			startupComplete.countDown();
		}
	}

	static void transferStartupQueue(ArrayDeque<JsonEnvelope> queue, HttpBackendTransportConnector connector) {
		while (!queue.isEmpty()) {
			JsonEnvelope envelope = queue.peekFirst();
			if (!connector.send(envelope)) {
				throw new IllegalStateException("HTTP startup queue could not be transferred");
			}
			queue.removeFirst();
		}
	}

	@Override
	public void activateAfterPublication() {
		HttpBackendTransportConnector active;
		synchronized (lifecycle) {
			if (closed) return;
			active = connector;
		}
		if (active != null) invokeConnectorLifecycle(active, "activateIncoming");
		synchronized (lifecycle) {
			if (closed) return;
			inboundActive = true;
			published = true;
			lifecycle.notifyAll();
		}
	}

	private boolean awaitInboundPublication() {
		synchronized (lifecycle) {
			while (!inboundActive && !closed) {
				try {
					lifecycle.wait();
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					return false;
				}
			}
			return !closed;
		}
	}

	void dispatchAfterPublication(GlobalMessageHandler messageHandler, JsonEnvelope envelope) {
		if (!awaitInboundPublication())
			throw new IllegalStateException("HTTP transport closed before inbound publication");
		dispatchIncoming(messageHandler, envelope,
				System.nanoTime() + TimeUnit.SECONDS.toNanos(INCOMING_DISPATCH_SECONDS));
	}

	private static void invokeConnectorLifecycle(HttpBackendTransportConnector connector, String method) {
		try {
			connector.getClass().getMethod(method).invoke(connector);
		} catch (java.lang.reflect.InvocationTargetException failure) {
			Throwable cause = failure.getCause();
			if (cause instanceof RuntimeException runtime) throw runtime;
			if (cause instanceof Error error) throw error;
			throw new IllegalStateException("HTTP connector " + method + " failed", cause);
		} catch (NoSuchMethodException unavailable) {
			// Older published SimpleAPI snapshots do not yet expose the publication
			// barrier. The wrapper callback above provides the same fence until #79 is
			// deployed, while newer versions use the native connector barrier.
			if ("startPaused".equals(method)) connector.start();
			else if (!"activateIncoming".equals(method))
				throw new IllegalStateException("SimpleAPI HTTP connector does not support " + method, unavailable);
		} catch (ReflectiveOperationException failure) {
			throw new IllegalStateException("SimpleAPI HTTP connector does not support " + method, failure);
		}
	}

	static void restoreUnenrolledCredentialState(Path directory, String serverId, String configuredCode) throws Exception {
		Path root = directory.toAbsolutePath().normalize();
		if (configuredCode != null && !configuredCode.isBlank()
				&& HttpClientCredentialStore.hasEnrolledProfile(root)) {
			try {
				HttpConnectionCode original = HttpConnectionCode.parse(configuredCode);
				if (original.serverId().equals(HttpTlsIdentity.canonicalServerId(serverId))
						&& HttpClientCredentialStore.matchesEnrollmentCode(root, original)) return;
			} catch (Exception ignored) {
				// The pre-replacement state was un-enrolled; never retain a credential
				// that cannot be tied to its already-validated original code.
			}
		}
		Path current = root.resolve("http-transport-client-current").normalize();
		if (!current.getParent().equals(root) || Files.isSymbolicLink(current))
			throw new java.io.IOException("HTTP client credential pointer is unsafe");
		if (Files.exists(current, LinkOption.NOFOLLOW_LINKS)
				&& !Files.isRegularFile(current, LinkOption.NOFOLLOW_LINKS))
			throw new java.io.IOException("HTTP client credential pointer is unsafe");
		DurableFiles.deleteIfExists(current);
		if (HttpClientCredentialStore.hasEnrolledProfile(root))
			throw new java.io.IOException("HTTP client credential rollback did not restore the un-enrolled state");
	}

	private boolean waitForEnrollmentRetry(long delayMillis) {
		if (closed) return false;
		try {
			TimeUnit.MILLISECONDS.sleep(delayMillis);
			return !closed;
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			return false;
		}
	}

	/** Performs initial enrollment with a bounded backoff for ordinary startup. */
	boolean enrollForStartup(HttpConnectionCode code, String serverId, Path directory,
			boolean retryEnrollment, java.util.function.LongPredicate waitForRetry) throws Exception {
		long retryDelayMillis = ENROLLMENT_RETRY_INITIAL_MILLIS;
		while (true) {
			if (closed) return false;
			try {
				HttpBackendTransportConnector.enroll(code, serverId, directory);
				return true;
			} catch (Exception failure) {
				if (!retryEnrollment) throw failure;
				plugin.getLogger().warning("Secure HTTP backend enrollment failed; retrying with bounded backoff");
				if (!waitForRetry.test(retryDelayMillis)) return false;
				retryDelayMillis = Math.min(ENROLLMENT_RETRY_MAX_MILLIS, retryDelayMillis * 2L);
			}
		}
	}

	void dispatchIncoming(GlobalMessageHandler messageHandler, JsonEnvelope envelope, long deadlineNanos) {
		CountDownLatch completed = new CountDownLatch(1);
		AtomicReference<Throwable> failure = new AtomicReference<>();
		AtomicInteger state = new AtomicInteger(0); // pending, running, cancelled, finished
		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> {
				if (!state.compareAndSet(0, 1)) {
					completed.countDown();
					return;
				}
				try {
					messageHandler.onMessage(envelope);
				} catch (Throwable thrown) {
					failure.set(thrown);
				} finally {
					state.set(3);
					completed.countDown();
				}
			});
		} catch (Throwable rejected) {
			throw new IllegalStateException("Could not schedule an incoming HTTP message on the server thread", rejected);
		}
		try {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0L || !completed.await(remaining, TimeUnit.NANOSECONDS)) {
				state.compareAndSet(0, 2);
				throw new IllegalStateException("Incoming HTTP message handling exceeded its delivery deadline");
			}
		} catch (InterruptedException interrupted) {
			state.compareAndSet(0, 2);
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Incoming HTTP message handling was interrupted", interrupted);
		}
		Throwable thrown = failure.get();
		if (thrown != null)
			throw new IllegalStateException("Incoming HTTP message handling failed", thrown);
	}

	void awaitCredentialRestoration(long deadlineNanos) {
		if (credentialGenerationToRestore == null && !restoreUnenrolledState) return;
		try {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0L || !credentialRestoreComplete.await(remaining, TimeUnit.NANOSECONDS))
				throw new IllegalStateException("Previous HTTP client credential restoration timed out");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Previous HTTP client credential restoration was interrupted", interrupted);
		}
		RuntimeException failure = credentialRestoreFailure;
		if (failure != null) throw failure;
	}

	@Override
	public boolean send(JsonEnvelope envelope) {
		synchronized (lifecycle) {
			if (closed) return false;
			if (awaitingPreparedHandoff || !handoffQueue.isEmpty()) {
				int capacity = awaitingPreparedHandoff
						? Math.min(MAX_PREPUBLICATION_QUEUE, MAX_HANDOFF_QUEUE - preparedHandoffReservation)
						: MAX_HANDOFF_QUEUE;
				if (handoffQueue.size() < capacity) {
					handoffQueue.addLast(envelope);
					return true;
				}
				warnRejectedSend();
				return false;
			}
			HttpBackendTransportConnector active = connector;
			if (active != null) {
				if (!active.send(envelope)) {
					if (restartAfterFailedFlush && startupQueue.size() < MAX_STARTUP_QUEUE) {
						startupQueue.addLast(envelope);
						return true;
					}
					warnRejectedSend();
					return false;
				}
			} else if (startupQueue.size() < MAX_STARTUP_QUEUE) {
				startupQueue.addLast(envelope);
				return true;
			} else {
				warnRejectedSend();
				return false;
			}
			return true;
		}
	}

	void beginPreparedHandoff() {
		synchronized (lifecycle) {
			if (closed) throw new IllegalStateException("HTTP replacement transport is closed");
			awaitingPreparedHandoff = true;
		}
	}

	/**
	 * Reserves enough of the bounded handoff queue for the predecessor before callers
	 * can send through this staged replacement. This turns an otherwise late,
	 * destructive capacity failure into a pre-publication validation failure.
	 */
	void reservePreparedHandoffCapacity(int messages) {
		if (messages < 0 || messages > MAX_HANDOFF_QUEUE)
			throw new IllegalStateException("HTTP prepared handoff exceeds its fixed capacity");
		synchronized (lifecycle) {
			if (closed) throw new IllegalStateException("HTTP replacement transport is closed");
			if (!awaitingPreparedHandoff)
				throw new IllegalStateException("HTTP replacement transport is not awaiting a prepared handoff");
			if (handoffQueue.size() > MAX_HANDOFF_QUEUE - messages)
				throw new IllegalStateException("HTTP handoff queue exceeded its reserved capacity");
			preparedHandoffReservation = messages;
		}
	}

	void acceptHandoffMessages(java.util.List<JsonEnvelope> messages) {
		Thread drain;
		synchronized (lifecycle) {
			if (closed) throw new IllegalStateException("HTTP replacement transport is closed");
			if (messages.size() > MAX_HANDOFF_QUEUE - handoffQueue.size())
				throw new IllegalStateException("HTTP handoff queue exceeded its fixed capacity");
			java.util.ArrayDeque<JsonEnvelope> newer = new java.util.ArrayDeque<>(handoffQueue);
			handoffQueue.clear();
			handoffQueue.addAll(messages);
			handoffQueue.addAll(newer);
			awaitingPreparedHandoff = false;
			preparedHandoffReservation = 0;
			if (handoffQueue.isEmpty()) return;
			if (handoffWorker != null) return;
			drain = new Thread(this::drainHandoffMessages, "VotingPlugin-HTTP-Backend-Handoff");
			drain.setDaemon(true);
			handoffWorker = drain;
		}
		drain.start();
	}

	private void drainHandoffMessages() {
		try {
			while (!Thread.currentThread().isInterrupted()) {
				boolean delivered = false;
				synchronized (lifecycle) {
					if (closed || handoffQueue.isEmpty()) return;
					HttpBackendTransportConnector active = connector;
					if (active != null && active.send(handoffQueue.peekFirst())) {
						handoffQueue.removeFirst();
						delivered = true;
					}
				}
				if (!delivered) TimeUnit.MILLISECONDS.sleep(25L);
			}
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
		} finally {
			synchronized (lifecycle) {
				if (handoffWorker == Thread.currentThread()) handoffWorker = null;
			}
		}
	}

	java.util.List<JsonEnvelope> handoffMessagesSnapshot() {
		synchronized (lifecycle) {
			return java.util.List.copyOf(handoffQueue);
		}
	}

	private void warnRejectedSend() {
		if (queueWarning.compareAndSet(false, true))
			plugin.getLogger().severe("Secure HTTP transport queue is full or rejected an oversized message; delivery was not accepted");
	}

	@Override
	public void validate() {
		validate(System.nanoTime() + TimeUnit.SECONDS.toNanos(DEFAULT_STARTUP_VALIDATION_SECONDS));
	}

	void validate(long deadlineNanos) {
		String serverId = plugin.getBungeeSettings().getServer();
		if (serverId == null || !serverId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
			throw new IllegalStateException("HTTP requires a valid unique backend Server name");
		}
		Path directory = plugin.getDataFolder().toPath().resolve("http");
		validateConfiguration(directory, serverId, plugin.getBungeeSettings().getHttpConnectionCode());
		if (!started) return;
		try {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0L || !startupComplete.await(remaining, TimeUnit.NANOSECONDS))
				throw new IllegalStateException("Secure HTTP backend setup did not finish within the validation deadline");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Secure HTTP backend setup validation was interrupted", interrupted);
		}
		RuntimeException failure = startupFailure;
		if (failure != null) throw failure;
		HttpBackendTransportConnector active = connector;
		if (closed || active == null) throw new IllegalStateException("Secure HTTP backend transport did not initialize");
		try {
			if (!active.awaitFirstResponse(deadlineNanos))
				throw new IllegalStateException("HTTP backend could not authenticate with the proxy");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Secure HTTP backend readiness validation was interrupted", interrupted);
		}
	}

	public static void validateConfiguration(Path directory, String serverId, String configuredCode) {
		enrollmentCode(directory, serverId, configuredCode);
	}

	static HttpConnectionCode enrollmentCode(Path directory, String serverId, String configuredCode) {
		try { serverId = HttpTlsIdentity.canonicalServerId(serverId); }
		catch (IllegalArgumentException invalid) { throw new IllegalStateException("HTTP requires a valid unique backend Server name", invalid); }
		boolean enrolled = HttpClientCredentialStore.hasEnrolledProfile(directory);
		if (configuredCode != null && !configuredCode.isBlank()) {
			try {
				HttpConnectionCode code = HttpConnectionCode.parse(configuredCode);
				if (!code.serverId().equals(serverId))
					throw new IllegalArgumentException("Connection code belongs to a different backend");
				if (enrolled && HttpClientCredentialStore.matchesEnrollmentCode(directory, code)) return null;
				code.requireActive(Clock.systemUTC());
				return code;
			} catch (Exception invalid) {
				throw new IllegalStateException("HTTP ConnectionCode is invalid, expired, or belongs to a different backend", invalid);
			}
		}
		if (!enrolled)
			throw new IllegalStateException("HTTP requires a temporary ConnectionCode for initial enrollment");
		return null;
	}

	@Override
	public void close() {
		close(true);
	}

	private void closeForReplacement() {
		close(false);
	}

	private void close(boolean discardQueuedMessages) {
		Thread setup;
		Thread pendingHandoff;
		HttpBackendTransportConnector active;
		Semaphore owner;
		java.util.List<JsonEnvelope> finalHandoff;
		synchronized (lifecycle) {
			if (closed) return;
			closed = true;
			lifecycle.notifyAll();
			if (discardQueuedMessages) {
				if (published) {
					finalHandoff = new java.util.ArrayList<>(startupQueue.size() + handoffQueue.size());
					finalHandoff.addAll(startupQueue);
					finalHandoff.addAll(handoffQueue);
				} else {
					// A staged replacement never became authoritative. Its presence and
					// handoff messages must not be flushed after rollback.
					finalHandoff = java.util.List.of();
				}
				startupQueue.clear();
				handoffQueue.clear();
			} else finalHandoff = java.util.List.of();
			awaitingPreparedHandoff = false;
			setup = worker;
			worker = null;
			pendingHandoff = handoffWorker;
			handoffWorker = null;
			active = connector;
			connector = null;
			owner = directoryOwner;
			directoryOwner = null;
		}
		startupComplete.countDown();
		if (setup != null) setup.interrupt();
		if (pendingHandoff != null) pendingHandoff.interrupt();
		if (setup == null && active == null && owner == null) return;
		Thread cleanup = new Thread(() -> drain(setup, active, owner, finalHandoff),
				"VotingPlugin-HTTP-Backend-Cleanup");
		cleanup.setDaemon(true);
		cleanup.start();
	}

	private static void drain(Thread setup, HttpBackendTransportConnector active, Semaphore owner,
			java.util.List<JsonEnvelope> finalHandoff) {
		try {
			if (setup != null) try { setup.join(TimeUnit.SECONDS.toMillis(5)); }
			catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
			if (active != null) {
				flushHandoffForShutdown(active, finalHandoff,
						System.nanoTime() + TimeUnit.SECONDS.toNanos(SHUTDOWN_FLUSH_SECONDS));
				active.close();
			}
		} finally { if (owner != null) owner.release(); }
	}

	static boolean flushHandoffForShutdown(HttpBackendTransportConnector active,
			java.util.List<JsonEnvelope> finalHandoff, long deadlineNanos) {
		for (JsonEnvelope envelope : finalHandoff) {
			while (!active.send(envelope)) {
				long remaining = deadlineNanos - System.nanoTime();
				if (remaining <= 0L) return false;
				try {
					TimeUnit.NANOSECONDS.sleep(Math.min(remaining, TimeUnit.MILLISECONDS.toNanos(25L)));
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					return false;
				}
			}
		}
		return active.flushOutgoing(deadlineNanos);
	}
}
