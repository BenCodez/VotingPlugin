package com.bencodez.votingplugin.backendproxy.transport;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
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
	private final VotingPluginMain plugin;
	private final Object lifecycle = new Object();
	private final ArrayDeque<JsonEnvelope> startupQueue = new ArrayDeque<>();
	private volatile HttpBackendTransportConnector connector;
	private volatile Thread worker;
	private volatile RuntimeException startupFailure;
	private volatile boolean closed;
	private final java.util.concurrent.atomic.AtomicBoolean queueWarning = new java.util.concurrent.atomic.AtomicBoolean();

	public HttpBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		Path directory = plugin.getDataFolder().toPath().resolve("http");
		String serverId = plugin.getBungeeSettings().getServer();
		String connectionCode = plugin.getBungeeSettings().getHttpConnectionCode();
		worker = new Thread(() -> initialize(directory, serverId, connectionCode, messageHandler),
				"VotingPlugin-HTTP-Backend-Setup");
		worker.setDaemon(true);
		worker.start();
	}

	private void initialize(Path directory, String serverId, String configuredCode,
			GlobalMessageHandler messageHandler) {
		try {
			if (!Files.isRegularFile(directory.resolve("http-transport-profile.properties"))) {
				HttpConnectionCode code = HttpConnectionCode.parse(configuredCode);
				HttpBackendTransportConnector.enroll(code, serverId, directory);
			}
			HttpClientCredentialStore.EnrolledClient enrolled = HttpClientCredentialStore.loadEnrolled(directory);
			if (!enrolled.profile().serverId().equals(com.bencodez.votingplugin.backendproxy.http.HttpTlsIdentity.canonicalServerId(serverId)))
				throw new IllegalStateException("Persisted HTTP identity belongs to a different backend Server name");
			HttpBackendTransportConnector replacement = new HttpBackendTransportConnector(enrolled, messageHandler::onMessage);
			synchronized (lifecycle) {
				if (closed) {
					replacement.close();
					return;
				}
				connector = replacement;
				replacement.start();
				while (!startupQueue.isEmpty()) {
					if (!replacement.send(startupQueue.removeFirst())) {
						throw new IllegalStateException("HTTP startup queue could not be transferred");
					}
				}
			}
		} catch (Exception failure) {
			startupFailure = new IllegalStateException("Secure HTTP backend enrollment or connection failed", failure);
			plugin.getLogger().severe("Secure HTTP backend transport is unavailable; check the connection code and proxy endpoint");
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
		RuntimeException failure = startupFailure;
		if (failure != null) throw failure;
		String serverId = plugin.getBungeeSettings().getServer();
		if (serverId == null || !serverId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
			throw new IllegalStateException("HTTP requires a valid unique backend Server name");
		}
		Path directory = plugin.getDataFolder().toPath().resolve("http");
		if (!Files.isRegularFile(directory.resolve("http-transport-profile.properties"))
				&& (plugin.getBungeeSettings().getHttpConnectionCode() == null
						|| plugin.getBungeeSettings().getHttpConnectionCode().isBlank())) {
			throw new IllegalStateException("HTTP requires a temporary ConnectionCode for initial enrollment");
		}
	}

	@Override
	public void close() {
		Thread setup;
		HttpBackendTransportConnector active;
		synchronized (lifecycle) {
			closed = true;
			startupQueue.clear();
			setup = worker;
			worker = null;
			active = connector;
			connector = null;
		}
		if (setup != null) {
			setup.interrupt();
			try {
				setup.join(TimeUnit.SECONDS.toMillis(5));
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
			}
		}
		if (active != null) active.close();
	}
}
