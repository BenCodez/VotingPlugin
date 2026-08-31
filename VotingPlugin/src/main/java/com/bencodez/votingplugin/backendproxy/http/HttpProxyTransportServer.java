package com.bencodez.votingplugin.backendproxy.http;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsExchange;
import com.sun.net.httpserver.HttpsParameters;
import com.sun.net.httpserver.HttpsServer;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLPeerUnverifiedException;

/**
 * One HTTPS listener for enrollment and the backend-to-proxy long-poll transport.
 * Every normal request is certificate-authenticated in the handler, rather than relying on TLS WANT auth.
 */
public final class HttpProxyTransportServer implements AutoCloseable {
	static {
		// JDK HttpServer reads these once when its internal server configuration is initialized.
		// Set conservative process-wide bounds before this transport creates its listener.
		setDefault("sun.net.httpserver.maxReqTime", "10");
		setDefault("sun.net.httpserver.maxRspTime", "10");
		setDefault("jdk.httpserver.maxConnections", "144");
		setDefault("sun.net.httpserver.maxReqHeaders", "32");
		setDefault("sun.net.httpserver.maxReqHeaderSize", "16384");
	}
	// Keep an idle request open long enough to reuse the TLS connection, but bound backend-origin
	// latency when a message is queued immediately after the request body has already been sent.
	public static final Duration LONG_POLL = Duration.ofSeconds(2);
	private final HttpTlsIdentity identity;
	private final HttpEnrollmentAuthority authority;
	private final HttpsServer server;
	private final ThreadPoolExecutor listenerExecutor;
	private final ThreadPoolExecutor handlerExecutor;
	private final Semaphore admission = new Semaphore(64);
	private final Map<String, BackendState> backends = new HashMap<>();
	private final Consumer<ReceivedEnvelope> onEnvelope;
	private volatile boolean closed;

	public HttpProxyTransportServer(InetSocketAddress bind, HttpTlsIdentity identity, HttpEnrollmentAuthority authority,
			Consumer<ReceivedEnvelope> onEnvelope) throws Exception {
		if (bind == null || identity == null || authority == null || onEnvelope == null) throw new IllegalArgumentException("HTTP transport configuration is required");
		this.identity = identity; this.authority = authority; this.onEnvelope = onEnvelope;
		server = HttpsServer.create(bind, 32);
		server.setHttpsConfigurator(new HttpsConfigurator(identity.serverContext()) {
			@Override public void configure(HttpsParameters parameters) {
				SSLParameters ssl = HttpPinnedTls.secureParameters(getSSLContext());
				ssl.setWantClientAuth(true); parameters.setSSLParameters(ssl);
			}
		});
		// Long polls are blocking by design. Capacity is bounded by admission, while enough workers
		// remain available for all admitted polls plus setup requests.
		listenerExecutor = executor("VotingPlugin-HTTP-listener", 72, 72);
		handlerExecutor = executor("VotingPlugin-HTTP-handler", 4, 128);
		server.setExecutor(listenerExecutor);
		server.createContext("/v1/enroll", exchange -> enroll((HttpsExchange) exchange));
		server.createContext("/v1/renew", exchange -> renew((HttpsExchange) exchange));
		server.createContext("/v1/transport", exchange -> transport((HttpsExchange) exchange));
	}

	private void renew(HttpsExchange exchange) throws IOException {
		if (!"/v1/renew".equals(exchange.getRequestURI().getPath()) || exchange.getRequestURI().getRawQuery() != null) { reply(exchange, 404, new byte[0]); return; }
		if (!"POST".equals(exchange.getRequestMethod())) { reply(exchange, 405, new byte[0]); return; }
		if (!json(exchange)) { reply(exchange, 415, new byte[0]); return; }
		if (!boundedFixedBody(exchange, 1024) || !admission.tryAcquire()) { reply(exchange, 429, new byte[0]); return; }
		try {
			String serverId = HttpTransportProtocol.parseRenewal(read(exchange.getRequestBody(), 1024));
			X509Certificate certificate = peerCertificate(exchange);
			if (certificate == null || !authority.authenticate(serverId, certificate)) { reply(exchange, 401, new byte[0]); return; }
			HttpTlsIdentity.IssuedClientCertificate issued = authority.renew(serverId, certificate);
			reply(exchange, 201, HttpTransportProtocol.enrollmentResponse(issued));
		} catch (IllegalArgumentException rejected) { reply(exchange, 403, new byte[0]);
		} catch (Exception failure) { reply(exchange, 503, new byte[0]);
		} finally { admission.release(); }
	}

	public void start() { if (closed) throw new IllegalStateException("HTTP transport is closed"); server.start(); }
	public int port() { return server.getAddress().getPort(); }
	public URI endpoint(String host) { return URI.create("https://" + host + ":" + port() + "/"); }

	/** Queues an in-memory proxy-origin envelope for a specific authenticated backend; this queue is not restart-durable. */
	public boolean send(String serverId, JsonEnvelope envelope) {
		if (closed || serverId == null || envelope == null) return false;
		try { serverId = HttpTlsIdentity.canonicalServerId(serverId); HttpTransportProtocol.validateEnvelope(envelope); }
		catch (IllegalArgumentException invalid) { return false; }
		BackendState backend;
		synchronized (backends) { backend = backends.computeIfAbsent(serverId, ignored -> new BackendState()); }
		return backend.enqueue(new HttpTransportProtocol.Delivery(UUID.randomUUID().toString(), envelope));
	}

	@Override public void close() {
		if (closed) return; closed = true; server.stop(1);
		shutdown(handlerExecutor); shutdown(listenerExecutor);
		synchronized (backends) { for (BackendState backend : backends.values()) backend.signal(); backends.clear(); }
	}

	private void enroll(HttpsExchange exchange) throws IOException {
		if (!"/v1/enroll".equals(exchange.getRequestURI().getPath()) || exchange.getRequestURI().getRawQuery() != null) { reply(exchange, 404, new byte[0]); return; }
		if (!"POST".equals(exchange.getRequestMethod())) { reply(exchange, 405, new byte[0]); return; }
		if (!json(exchange)) { reply(exchange, 415, new byte[0]); return; }
		if (!boundedFixedBody(exchange, 8192) || !admission.tryAcquire()) { reply(exchange, 429, new byte[0]); return; }
		try {
			HttpTransportProtocol.Enrollment request = HttpTransportProtocol.parseEnrollment(read(exchange.getRequestBody(), 8192));
			HttpTlsIdentity.IssuedClientCertificate issued = authority.enroll(request.server(), request.token());
			reply(exchange, 201, HttpTransportProtocol.enrollmentResponse(issued));
		} catch (Exception rejected) { reply(exchange, 403, new byte[0]); }
		finally { admission.release(); }
	}

	private void transport(HttpsExchange exchange) throws IOException {
		if (!"/v1/transport".equals(exchange.getRequestURI().getPath()) || exchange.getRequestURI().getRawQuery() != null) { reply(exchange, 404, new byte[0]); return; }
		if (!"POST".equals(exchange.getRequestMethod())) { reply(exchange, 405, new byte[0]); return; }
		if (!json(exchange)) { reply(exchange, 415, new byte[0]); return; }
		if (!boundedFixedBody(exchange, HttpTransportProtocol.MAX_BODY_BYTES) || !admission.tryAcquire()) { reply(exchange, 429, new byte[0]); return; }
		try {
			HttpTransportProtocol.Packet packet = HttpTransportProtocol.parsePacket(read(exchange.getRequestBody(), HttpTransportProtocol.MAX_BODY_BYTES));
			X509Certificate certificate = peerCertificate(exchange);
			if (certificate == null || !authority.authenticate(packet.server(), certificate)) { reply(exchange, 401, new byte[0]); return; }
			BackendState backend;
			synchronized (backends) { backend = backends.computeIfAbsent(packet.server(), ignored -> new BackendState()); }
			if (!backend.beginPoll(packet.session())) { reply(exchange, 409, new byte[0]); return; }
			try {
				handlePacket(packet, backend);
				Response response = backend.await(packet.server(), packet.session(), packet.sequence());
				reply(exchange, 200, HttpTransportProtocol.response(packet.server(), packet.session(), packet.sequence(), response.acks(), response.messages()));
			} finally { backend.endPoll(); }
		} catch (IllegalArgumentException rejected) { reply(exchange, 400, new byte[0]);
		} catch (Exception failure) { reply(exchange, 503, new byte[0]);
		} finally { admission.release(); }
	}

	private void handlePacket(HttpTransportProtocol.Packet packet, BackendState backend) {
		List<HttpTransportProtocol.Delivery> accepted;
		synchronized (backend) {
			if (!backend.allowRequest()) throw new IllegalArgumentException("transport rate limited");
			if (!backend.acceptSession(packet.session(), packet.sequence())) throw new IllegalArgumentException("stale session request");
			backend.acknowledge(packet.acks()); accepted = backend.acceptIncoming(packet.messages());
		}
		for (HttpTransportProtocol.Delivery delivery : accepted) dispatch(packet.server(), backend, delivery);
	}
	private void dispatch(String serverId, BackendState backend, HttpTransportProtocol.Delivery delivery) {
		try { handlerExecutor.execute(() -> {
			boolean success = false;
			try { onEnvelope.accept(new ReceivedEnvelope(serverId, delivery.id(), normalizeBackendIdentity(serverId, delivery.envelope()))); success = true; }
			catch (RuntimeException ignored) { }
			synchronized (backend) { backend.completeIncoming(delivery.id(), success); }
		}); } catch (RejectedExecutionException rejected) { synchronized (backend) { backend.completeIncoming(delivery.id(), false); } }
	}
	private static JsonEnvelope normalizeBackendIdentity(String serverId, JsonEnvelope envelope) {
		// The authenticated TLS identity is authoritative; never forward a forged `server` field.
		return envelope.toBuilder().put("server", serverId).build();
	}
	private static X509Certificate peerCertificate(HttpsExchange exchange) {
		try { Certificate[] peer = exchange.getSSLSession().getPeerCertificates();
			return peer.length > 0 && peer[0] instanceof X509Certificate certificate ? certificate : null;
		} catch (SSLPeerUnverifiedException absent) { return null; }
	}
	private static byte[] read(InputStream input, int maximum) throws IOException {
		ByteArrayOutputStream output = new ByteArrayOutputStream(); byte[] buffer = new byte[4096]; int total = 0, read;
		while ((read = input.read(buffer)) >= 0) { total += read; if (total > maximum) throw new IllegalArgumentException("HTTP body is too large"); output.write(buffer, 0, read); }
		return output.toByteArray();
	}
	private static void reply(HttpsExchange exchange, int status, byte[] body) throws IOException {
		Headers headers = exchange.getResponseHeaders(); headers.set("Cache-Control", "no-store"); headers.set("Content-Type", "application/json; charset=utf-8");
		exchange.sendResponseHeaders(status, body.length); try (var output = exchange.getResponseBody()) { output.write(body); }
	}
	private static boolean json(HttpsExchange exchange) {
		String contentType = exchange.getRequestHeaders().getFirst("Content-Type");
		return contentType != null && contentType.toLowerCase(java.util.Locale.ROOT).matches("application/json(?:\\s*;.*)?");
	}
	private static boolean boundedFixedBody(HttpsExchange exchange, int maximum) {
		if (exchange.getRequestHeaders().getFirst("Transfer-Encoding") != null) return false;
		String value = exchange.getRequestHeaders().getFirst("Content-Length");
		try { long length = Long.parseLong(value); return length > 0L && length <= maximum; }
		catch (RuntimeException invalid) { return false; }
	}
	private static ThreadPoolExecutor executor(String name, int threads, int queue) {
		ThreadFactory factory = task -> { Thread thread = new Thread(task, name); thread.setDaemon(true); return thread; };
		return new ThreadPoolExecutor(threads, threads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(queue), factory, new ThreadPoolExecutor.AbortPolicy());
	}
	private static void setDefault(String name, String value) { if (System.getProperty(name) == null) System.setProperty(name, value); }
	private static void shutdown(ExecutorService executor) { executor.shutdown(); try { if (!executor.awaitTermination(5, TimeUnit.SECONDS)) executor.shutdownNow(); } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); executor.shutdownNow(); } }

	public record ReceivedEnvelope(String serverId, String messageId, JsonEnvelope envelope) { }
	static record Response(Collection<String> acks, Collection<HttpTransportProtocol.Delivery> messages) { }
	static final class BackendState {
		private String session; private long sequence = -1L;
		private final LinkedHashMap<String, HttpTransportProtocol.Delivery> outgoing = new LinkedHashMap<>();
		private final Set<String> seen = new LinkedHashSet<>(); private final Set<String> processing = new LinkedHashSet<>();
		private final ArrayDeque<String> acknowledgements = new ArrayDeque<>();
		private final Set<String> delivered = new LinkedHashSet<>();
		private long lastDeliveryNanos;
		private double requestTokens = 24.0d;
		private long lastTokenNanos = System.nanoTime();
		private boolean activePoll;
		private boolean beginPoll(String requestedSession) { synchronized (this) { if (activePoll) return false; activePoll = true; return true; } }
		private void endPoll() { synchronized (this) { activePoll = false; notifyAll(); } }
		private boolean allowRequest() {
			long now = System.nanoTime(); requestTokens = Math.min(24.0d, requestTokens + ((now - lastTokenNanos) / 1_000_000_000.0d) * 2.0d);
			lastTokenNanos = now; if (requestTokens < 1.0d) return false; requestTokens -= 1.0d; return true;
		}
		boolean acceptSession(String requested, long requestedSequence) {
			if (!requested.equals(session)) { session = requested; sequence = -1L; delivered.clear(); lastDeliveryNanos = 0L; }
			// The connector allocates a fresh monotonic sequence for every attempt.  Rejecting equality
			// prevents a captured request from being replayed with altered ACKs or a new payload.
			if (requestedSequence <= sequence) return false; sequence = requestedSequence; return true;
		}
		synchronized boolean enqueue(HttpTransportProtocol.Delivery delivery) {
			if (outgoing.size() >= HttpTransportProtocol.MAX_QUEUE) return false;
			outgoing.put(delivery.id(), delivery); signal(); return true;
		}
		private void acknowledge(Collection<String> acks) { for (String id : acks) { outgoing.remove(id); delivered.remove(id); } }
		List<HttpTransportProtocol.Delivery> acceptIncoming(List<HttpTransportProtocol.Delivery> received) {
			List<HttpTransportProtocol.Delivery> accepted = new java.util.ArrayList<>();
			for (HttpTransportProtocol.Delivery delivery : received) {
				if (seen.contains(delivery.id())) { queueAck(delivery.id()); continue; }
				if (!processing.contains(delivery.id())) {
					processing.add(delivery.id()); accepted.add(delivery);
				}
			}
			return accepted;
		}
		synchronized void completeIncoming(String id, boolean success) { processing.remove(id); if (success) { seen.add(id); while (seen.size() > HttpTransportProtocol.MAX_QUEUE) seen.remove(seen.iterator().next()); queueAck(id); signal(); } }
		private void queueAck(String id) { if (acknowledgements.size() < HttpTransportProtocol.MAX_QUEUE && !acknowledgements.contains(id)) acknowledgements.add(id); }
		synchronized Response await(String serverId, String requestedSession, long requestedSequence) {
			long deadline = System.nanoTime() + LONG_POLL.toNanos();
			while (acknowledgements.isEmpty() && !hasUndelivered() && !redeliveryDue()) {
				long remaining = deadline - System.nanoTime(); if (remaining <= 0) break;
				try { TimeUnit.NANOSECONDS.timedWait(this, remaining); } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); break; }
			}
			List<String> acks = new java.util.ArrayList<>(); while (!acknowledgements.isEmpty() && acks.size() < HttpTransportProtocol.MAX_BATCH) acks.add(acknowledgements.remove());
			List<HttpTransportProtocol.Delivery> candidates = new java.util.ArrayList<>();
			if (hasUndelivered() || redeliveryDue()) for (HttpTransportProtocol.Delivery delivery : outgoing.values()) {
				if (!delivered.contains(delivery.id()) || redeliveryDue()) candidates.add(delivery);
				if (candidates.size() == HttpTransportProtocol.MAX_BATCH) break;
			}
			List<HttpTransportProtocol.Delivery> messages = HttpTransportProtocol.fittingMessages(serverId, requestedSession,
					requestedSequence, acks, candidates);
			for (HttpTransportProtocol.Delivery delivery : messages) delivered.add(delivery.id());
			if (!messages.isEmpty()) lastDeliveryNanos = System.nanoTime();
			return new Response(acks, messages);
		}
		private boolean hasUndelivered() { for (String id : outgoing.keySet()) if (!delivered.contains(id)) return true; return false; }
		private boolean redeliveryDue() { return !outgoing.isEmpty() && lastDeliveryNanos > 0L && System.nanoTime() - lastDeliveryNanos >= LONG_POLL.toNanos(); }
		private synchronized void signal() { notifyAll(); }
	}
}
