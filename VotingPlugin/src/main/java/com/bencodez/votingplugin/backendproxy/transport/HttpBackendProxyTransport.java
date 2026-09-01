package com.bencodez.votingplugin.backendproxy.transport;

import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayDeque;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.http.HttpBackendTransportConnector;
import com.bencodez.votingplugin.backendproxy.http.HttpClientCredentialStore;
import com.bencodez.votingplugin.backendproxy.http.HttpConnectionCode;

/** Backend adapter for the secure outbound-only HTTP proxy transport. */
public final class HttpBackendProxyTransport implements BackendProxyTransport {
	private static final int MAX_STARTUP_QUEUE = 1024;
	private static final long DEFAULT_STARTUP_VALIDATION_SECONDS = 25L;
	private static final ConcurrentHashMap<Path, Semaphore> DIRECTORY_OWNERS = new ConcurrentHashMap<>();
	private final VotingPluginMain plugin;
	private final Object lifecycle = new Object();
	private final CountDownLatch startupComplete = new CountDownLatch(1);
	private final ArrayDeque<JsonEnvelope> startupQueue = new ArrayDeque<>();
	private volatile HttpBackendTransportConnector connector;
	private volatile Thread worker;
	private volatile RuntimeException startupFailure;
	private volatile boolean started;
	private volatile boolean closed;
	private Semaphore directoryOwner;
	private final java.util.concurrent.atomic.AtomicBoolean queueWarning = new java.util.concurrent.atomic.AtomicBoolean();

	public HttpBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		Path directory = plugin.getDataFolder().toPath().resolve("http");
		String serverId = plugin.getBungeeSettings().getServer();
		String connectionCode = plugin.getBungeeSettings().getHttpConnectionCode();
		validateConfiguration(directory, serverId, connectionCode);
		started = true;
		worker = new Thread(() -> initialize(directory, serverId, connectionCode, messageHandler),
				"VotingPlugin-HTTP-Backend-Setup");
		worker.setDaemon(true);
		worker.start();
	}

	private void initialize(Path directory, String serverId, String configuredCode,
			GlobalMessageHandler messageHandler) {
		Path ownerKey = directory.toAbsolutePath().normalize();
		Semaphore owner = DIRECTORY_OWNERS.computeIfAbsent(ownerKey, ignored -> new Semaphore(1));
		boolean acquired = false, installed = false;
		HttpBackendTransportConnector replacement = null;
		try {
			owner.acquire();
			acquired = true;
			synchronized (lifecycle) { if (closed) return; }
			HttpConnectionCode code = enrollmentCode(directory, serverId, configuredCode);
			if (code != null) HttpBackendTransportConnector.enroll(code, serverId, directory);
			HttpClientCredentialStore.EnrolledClient enrolled = HttpClientCredentialStore.loadEnrolled(directory);
			if (!enrolled.profile().serverId().equals(com.bencodez.votingplugin.backendproxy.http.HttpTlsIdentity.canonicalServerId(serverId)))
				throw new IllegalStateException("Persisted HTTP identity belongs to a different backend Server name");
			replacement = new HttpBackendTransportConnector(directory, messageHandler::onMessage);
			replacement.start();
			if (!replacement.awaitFirstResponse(System.nanoTime()
					+ TimeUnit.SECONDS.toNanos(DEFAULT_STARTUP_VALIDATION_SECONDS))) {
				throw new IllegalStateException("HTTP backend could not authenticate with the proxy");
			}
			boolean discard = false;
			synchronized (lifecycle) {
				if (closed) {
					discard = true;
				} else {
					while (!startupQueue.isEmpty()) {
						if (!replacement.send(startupQueue.removeFirst())) {
							throw new IllegalStateException("HTTP startup queue could not be transferred");
						}
					}
					connector = replacement;
					directoryOwner = owner;
					installed = true;
				}
			}
			if (discard) replacement.close();
		} catch (Exception failure) {
			startupFailure = new IllegalStateException("Secure HTTP backend enrollment or connection failed", failure);
			plugin.getLogger().severe("Secure HTTP backend transport is unavailable; check the connection code and proxy endpoint");
		} finally {
			if (!installed) {
				if (replacement != null) replacement.close();
				if (acquired) owner.release();
			}
			startupComplete.countDown();
		}
	}

	@Override
	public void send(JsonEnvelope envelope) {
		synchronized (lifecycle) {
			if (closed) return;
			HttpBackendTransportConnector active = connector;
			if (active != null) {
				if (!active.send(envelope) && queueWarning.compareAndSet(false, true))
					plugin.getLogger().severe("Secure HTTP transport queue is full or rejected an oversized message; delivery was not accepted");
			} else if (startupQueue.size() < MAX_STARTUP_QUEUE) {
				startupQueue.addLast(envelope);
			}
		}
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
		if (closed || connector == null) throw new IllegalStateException("Secure HTTP backend transport did not become ready");
	}

	public static void validateConfiguration(Path directory, String serverId, String configuredCode) {
		enrollmentCode(directory, serverId, configuredCode);
	}

	static HttpConnectionCode enrollmentCode(Path directory, String serverId, String configuredCode) {
		try { serverId = com.bencodez.votingplugin.backendproxy.http.HttpTlsIdentity.canonicalServerId(serverId); }
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
		Thread setup;
		HttpBackendTransportConnector active;
		Semaphore owner;
		synchronized (lifecycle) {
			if (closed) return;
			closed = true;
			startupQueue.clear();
			setup = worker;
			worker = null;
			active = connector;
			connector = null;
			owner = directoryOwner;
			directoryOwner = null;
		}
		startupComplete.countDown();
		if (setup != null) setup.interrupt();
		if (setup == null && active == null && owner == null) return;
		Thread cleanup = new Thread(() -> drain(setup, active, owner), "VotingPlugin-HTTP-Backend-Cleanup");
		cleanup.setDaemon(true);
		cleanup.start();
	}

	private static void drain(Thread setup, HttpBackendTransportConnector active, Semaphore owner) {
		try {
			if (setup != null) try { setup.join(TimeUnit.SECONDS.toMillis(5)); }
			catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
			if (active != null) active.close();
		} finally { if (owner != null) owner.release(); }
	}
}
