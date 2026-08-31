package com.bencodez.votingplugin.backendproxy.http;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.time.Clock;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

/** Backend-side, persistent HTTP/1.1 long-poll connector. */
public final class HttpBackendTransportConnector implements AutoCloseable {
	public static final Duration CLIENT_TIMEOUT = Duration.ofSeconds(35);
	private final HttpClientCredentialStore.HttpClientProfile profile;
	private final String serverId;
	private final Consumer<JsonEnvelope> onEnvelope;
	private final HttpClient client;
	private final URI transportEndpoint;
	private final ThreadPoolExecutor callbackExecutor;
	private final AtomicBoolean running = new AtomicBoolean();
	private final Object state = new Object();
	private final LinkedHashMap<String, HttpTransportProtocol.Delivery> outgoing = new LinkedHashMap<>();
	private final Set<String> received = new LinkedHashSet<>(), processing = new LinkedHashSet<>();
	private final ArrayDeque<String> acknowledgements = new ArrayDeque<>();
	private final String session = UUID.randomUUID().toString();
	private volatile Thread poller;
	private long sequence;

	public HttpBackendTransportConnector(HttpConnectionCode code, String serverId,
			HttpClientCredentialStore.ClientCredential credential, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(profile(code, serverId), credential, onEnvelope);
	}

	/** Starts normal transport from the non-secret profile persisted by enrollment. */
	public HttpBackendTransportConnector(HttpClientCredentialStore.EnrolledClient enrolled, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(enrolled == null ? null : enrolled.profile(), enrolled == null ? null : enrolled.credential(), onEnvelope);
	}

	public HttpBackendTransportConnector(HttpClientCredentialStore.HttpClientProfile profile,
			HttpClientCredentialStore.ClientCredential credential, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		if (profile == null || credential == null || onEnvelope == null) throw new IllegalArgumentException("HTTP backend transport configuration is invalid");
		if (!matchesCredential(profile, credential)) throw new IllegalArgumentException("HTTP client certificate does not match transport profile");
		this.profile = profile; this.serverId = profile.serverId(); this.onEnvelope = onEnvelope;
		client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).followRedirects(HttpClient.Redirect.NEVER)
			.connectTimeout(Duration.ofSeconds(5)).sslContext(clientContext(profile, credential)).build();
		transportEndpoint = profile.endpoint().resolve("v1/transport");
		callbackExecutor = executor("VotingPlugin-HTTP-callback", 2, 128);
	}

	/** Convenience constructor for the owner-only credential directory produced by {@link #enroll}. */
	public HttpBackendTransportConnector(HttpConnectionCode code, String serverId, Path credentials,
			Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(HttpClientCredentialStore.loadEnrolled(credentials), onEnvelope);
		if (code == null || !profile(code, serverId).equals(this.profile)) throw new IllegalArgumentException("HTTP transport profile does not match connection code");
	}

	/** Starts normal transport using only the persisted certificate and non-secret profile. */
	public HttpBackendTransportConnector(Path credentials, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(HttpClientCredentialStore.loadEnrolled(credentials), onEnvelope);
	}

	/** Performs enrollment network I/O; call this from a connector/setup worker, never a platform main thread. */
	public static HttpClientCredentialStore.ClientCredential enroll(HttpConnectionCode code, String serverId, Path credentials) throws Exception {
		if (code == null || credentials == null || serverId == null || !serverId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) throw new IllegalArgumentException("Enrollment configuration is invalid");
		code.requireActive(Clock.systemUTC());
		byte[] payload = ("{\"server\":\"" + serverId + "\",\"token\":\"" + code.enrollmentToken() + "\"}").getBytes(StandardCharsets.UTF_8);
		HttpClient client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).followRedirects(HttpClient.Redirect.NEVER)
			.connectTimeout(Duration.ofSeconds(5)).sslContext(HttpPinnedTls.clientContext(code)).build();
		HttpRequest request = HttpRequest.newBuilder(code.endpoint().resolve("v1/enroll")).timeout(CLIENT_TIMEOUT)
			.header("Content-Type", "application/json").header("Cache-Control", "no-store").POST(HttpRequest.BodyPublishers.ofByteArray(payload)).build();
		HttpResponse<byte[]> response = client.send(request, HttpResponse.BodyHandlers.ofByteArray());
		if (response.statusCode() != 201 || response.body().length > HttpTransportProtocol.MAX_BODY_BYTES) throw new IllegalArgumentException("Enrollment was rejected");
		HttpTlsIdentity.IssuedClientCertificate issued = HttpTransportProtocol.parseEnrollmentResponse(serverId, response.body());
		HttpClientCredentialStore.saveEnrolled(credentials, code, issued); return HttpClientCredentialStore.load(credentials);
	}

	public void start() {
		if (!running.compareAndSet(false, true)) return;
		poller = new Thread(this::pollLoop, "VotingPlugin-HTTP-poll"); poller.setDaemon(true); poller.start();
	}
	/**
	 * Inserts an in-memory at-least-once delivery. It survives retry/lost responses while this process remains alive;
	 * callers needing restart durability must retain the application operation independently.
	 */
	public boolean send(JsonEnvelope envelope) {
		if (envelope == null || !running.get()) return false;
		try { HttpTransportProtocol.validateEnvelope(envelope); }
		catch (IllegalArgumentException invalid) { return false; }
		synchronized (state) {
			if (outgoing.size() >= HttpTransportProtocol.MAX_QUEUE) return false;
			String id = UUID.randomUUID().toString(); outgoing.put(id, new HttpTransportProtocol.Delivery(id, envelope)); return true;
		}
	}
	/** A synchronous single poll, useful for lifecycle-controlled integrations and tests. */
	public boolean pollOnce() {
		if (!running.get()) return false;
		try {
			List<String> acks; List<HttpTransportProtocol.Delivery> messages; long requestSequence;
			synchronized (state) {
				acks = first(acknowledgements); requestSequence = sequence++;
				messages = HttpTransportProtocol.fittingMessages(serverId, session, requestSequence, acks, outgoing.values());
				for (int index = 0; index < acks.size(); index++) acknowledgements.removeFirst();
			}
			HttpRequest request = HttpRequest.newBuilder(transportEndpoint).timeout(CLIENT_TIMEOUT).header("Content-Type", "application/json")
				.header("Cache-Control", "no-store").POST(HttpRequest.BodyPublishers.ofByteArray(HttpTransportProtocol.request(serverId, session, requestSequence, acks, messages))).build();
			HttpResponse<byte[]> response = client.send(request, HttpResponse.BodyHandlers.ofByteArray());
			if (response.statusCode() != 200 || response.body().length > HttpTransportProtocol.MAX_BODY_BYTES) return false;
			HttpTransportProtocol.Packet packet = HttpTransportProtocol.parsePacket(response.body());
			if (!serverId.equals(packet.server()) || !session.equals(packet.session()) || packet.sequence() != requestSequence) return false;
			synchronized (state) { for (String ack : packet.acks()) outgoing.remove(ack); }
			for (HttpTransportProtocol.Delivery delivery : accept(packet.messages())) dispatch(delivery);
			return true;
		} catch (Exception failure) { return false; }
	}
	@Override public void close() {
		running.getAndSet(false);
		Thread current = poller; if (current != null) current.interrupt();
		callbackExecutor.shutdown(); try { if (!callbackExecutor.awaitTermination(5, TimeUnit.SECONDS)) callbackExecutor.shutdownNow(); }
		catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); callbackExecutor.shutdownNow(); }
	}

	private void pollLoop() {
		long retry = 1000L;
		while (running.get()) { if (pollOnce()) { retry = 1000L; continue; } try { Thread.sleep(retry); } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); break; } retry = Math.min(30_000L, retry * 2); }
	}
	List<HttpTransportProtocol.Delivery> accept(List<HttpTransportProtocol.Delivery> deliveries) {
		synchronized (state) {
			List<HttpTransportProtocol.Delivery> accepted = new java.util.ArrayList<>();
			for (HttpTransportProtocol.Delivery delivery : deliveries) {
				if (received.contains(delivery.id())) { queueAck(delivery.id()); continue; }
				if (!processing.contains(delivery.id())) { if (received.size() >= HttpTransportProtocol.MAX_QUEUE) break; processing.add(delivery.id()); accepted.add(delivery); }
			}
			return accepted;
		}
	}
	void dispatch(HttpTransportProtocol.Delivery delivery) {
		try { callbackExecutor.execute(() -> { boolean success = false; try { onEnvelope.accept(delivery.envelope()); success = true; } catch (RuntimeException ignored) { }
			synchronized (state) { processing.remove(delivery.id()); if (success) { received.add(delivery.id()); while (received.size() > HttpTransportProtocol.MAX_QUEUE) received.remove(received.iterator().next()); queueAck(delivery.id()); } }
		}); } catch (RejectedExecutionException rejected) { synchronized (state) { processing.remove(delivery.id()); } }
	}
	private void queueAck(String id) { if (acknowledgements.size() < HttpTransportProtocol.MAX_QUEUE && !acknowledgements.contains(id)) acknowledgements.add(id); }
	int queuedOutgoing() { synchronized (state) { return outgoing.size(); } }
	List<String> drainAcknowledgements() { synchronized (state) { return drain(acknowledgements); } }
	private static <T> List<T> first(Collection<T> values) { List<T> output = new java.util.ArrayList<>(); for (T value : values) { output.add(value); if (output.size() == HttpTransportProtocol.MAX_BATCH) break; } return output; }
	private static List<String> drain(ArrayDeque<String> values) { List<String> output = new java.util.ArrayList<>(); while (!values.isEmpty() && output.size() < HttpTransportProtocol.MAX_BATCH) output.add(values.remove()); return output; }
	private static ThreadPoolExecutor executor(String name, int threads, int queue) { ThreadFactory factory = task -> { Thread thread = new Thread(task, name); thread.setDaemon(true); return thread; }; return new ThreadPoolExecutor(threads, threads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(queue), factory, new ThreadPoolExecutor.AbortPolicy()); }
	private static HttpClientCredentialStore.HttpClientProfile profile(HttpConnectionCode code, String serverId) {
		if (code == null || serverId == null) throw new IllegalArgumentException("HTTP backend transport configuration is invalid");
		if (!code.serverId().equals(HttpTlsIdentity.canonicalServerId(serverId))) throw new IllegalArgumentException("HTTP connection code belongs to a different backend");
		return new HttpClientCredentialStore.HttpClientProfile(serverId, code.endpoint(), code.serverCertificatePin(), code.caCertificatePin());
	}
	private static boolean matchesCredential(HttpClientCredentialStore.HttpClientProfile profile,
			HttpClientCredentialStore.ClientCredential credential) {
		try {
			credential.certificate().checkValidity(); credential.certificate().verify(credential.caCertificate().getPublicKey());
			String expected = "urn:votingplugin:http-backend:" + profile.serverId();
			var names = credential.certificate().getSubjectAlternativeNames(); if (names == null) return false;
			for (java.util.List<?> name : names) if (name.size() == 2 && Integer.valueOf(6).equals(name.get(0)) && expected.equals(name.get(1))) return true;
			return false;
		} catch (Exception invalid) { return false; }
	}
	private static SSLContext clientContext(HttpClientCredentialStore.HttpClientProfile profile, HttpClientCredentialStore.ClientCredential credential) throws Exception {
		KeyStore store = KeyStore.getInstance("PKCS12"); store.load(null, new char[0]);
		store.setKeyEntry("client", credential.privateKey(), credential.password(), new java.security.cert.Certificate[] { credential.certificate(), credential.caCertificate() });
		KeyManagerFactory keys = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm()); keys.init(store, credential.password());
		SSLContext context = SSLContext.getInstance("TLS"); context.init(keys.getKeyManagers(), new javax.net.ssl.TrustManager[] { new PinnedTrustManager(profile) }, null); return context;
	}
	private static final class PinnedTrustManager implements javax.net.ssl.X509TrustManager {
		private final HttpClientCredentialStore.HttpClientProfile profile; PinnedTrustManager(HttpClientCredentialStore.HttpClientProfile profile) { this.profile = profile; }
		@Override public void checkClientTrusted(X509Certificate[] chain, String authType) { throw new UnsupportedOperationException(); }
		@Override public void checkServerTrusted(X509Certificate[] chain, String authType) throws java.security.cert.CertificateException {
			if (chain == null || chain.length < 2) throw new java.security.cert.CertificateException("Pinned HTTPS server chain is incomplete");
			chain[0].checkValidity(); chain[chain.length - 1].checkValidity();
			try { chain[0].verify(chain[chain.length - 1].getPublicKey()); }
			catch (java.security.GeneralSecurityException invalid) { throw new java.security.cert.CertificateException("Pinned HTTPS server signature is invalid", invalid); }
			if (chain[chain.length - 1].getBasicConstraints() < 0 || !HttpTransportSecrets.constantTimeEquals(profile.serverCertificatePin().getBytes(StandardCharsets.US_ASCII), HttpTransportSecrets.certificatePin(chain[0]).getBytes(StandardCharsets.US_ASCII))) throw new java.security.cert.CertificateException("Pinned HTTPS server mismatch");
			String caPin = HttpTransportSecrets.certificatePin(chain[chain.length - 1]);
			if (!HttpTransportSecrets.constantTimeEquals(profile.caCertificatePin().getBytes(StandardCharsets.US_ASCII), caPin.getBytes(StandardCharsets.US_ASCII)))
				throw new java.security.cert.CertificateException("Pinned HTTPS authority mismatch");
		}
		@Override public X509Certificate[] getAcceptedIssuers() { return new X509Certificate[0]; }
	}
}
