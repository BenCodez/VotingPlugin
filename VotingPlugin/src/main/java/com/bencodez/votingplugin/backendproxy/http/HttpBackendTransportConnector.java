package com.bencodez.votingplugin.backendproxy.http;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpTimeoutException;
import java.nio.ByteBuffer;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Flow;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

/** Backend-side, persistent HTTP/1.1 long-poll connector. */
public final class HttpBackendTransportConnector implements AutoCloseable {
	public static final Duration CLIENT_TIMEOUT = Duration.ofSeconds(35);
	static final int CALLBACK_QUEUE_CAPACITY = 128;
	private volatile HttpClientCredentialStore.HttpClientProfile profile;
	private final String serverId;
	private final Consumer<JsonEnvelope> onEnvelope;
	private volatile HttpClient client;
	private volatile HttpClientCredentialStore.ClientCredential credential;
	private final Path credentialDirectory;
	private final HttpInboundDeliveryStore inboundDeliveries;
	private final URI transportEndpoint;
	private final ThreadPoolExecutor callbackExecutor;
	private final AtomicBoolean running = new AtomicBoolean();
	private final CountDownLatch firstResponse = new CountDownLatch(1);
	private final Object state = new Object();
	private final Object renewal = new Object();
	private final LinkedHashMap<String, HttpTransportProtocol.Delivery> outgoing = new LinkedHashMap<>();
	private final Set<String> received = new LinkedHashSet<>(), processing = new LinkedHashSet<>();
	private final ArrayDeque<String> acknowledgements = new ArrayDeque<>();
	private final String session = UUID.randomUUID().toString();
	private volatile Thread poller;
	private long sequence;
	private volatile long nextRenewalCheckNanos;

	/** In-memory test constructor; production transport must use a directory-backed constructor. */
	HttpBackendTransportConnector(HttpConnectionCode code, String serverId,
			HttpClientCredentialStore.ClientCredential credential, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(profile(code, serverId), credential, onEnvelope);
	}

	/** In-memory test constructor; production transport must use a directory-backed constructor. */
	HttpBackendTransportConnector(HttpClientCredentialStore.EnrolledClient enrolled, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(enrolled, onEnvelope, null);
	}

	/** In-memory test constructor; production transport must use a directory-backed constructor. */
	HttpBackendTransportConnector(HttpClientCredentialStore.HttpClientProfile profile,
			HttpClientCredentialStore.ClientCredential credential, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(profile, credential, onEnvelope, null);
	}

	private HttpBackendTransportConnector(HttpClientCredentialStore.EnrolledClient enrolled, Consumer<JsonEnvelope> onEnvelope,
			Path credentialDirectory) throws Exception {
		this(enrolled == null ? null : enrolled.profile(), enrolled == null ? null : enrolled.credential(), onEnvelope, credentialDirectory);
	}

	private HttpBackendTransportConnector(HttpClientCredentialStore.HttpClientProfile profile,
			HttpClientCredentialStore.ClientCredential credential, Consumer<JsonEnvelope> onEnvelope, Path credentialDirectory) throws Exception {
		if (profile == null || credential == null || onEnvelope == null) throw new IllegalArgumentException("HTTP backend transport configuration is invalid");
		if (!matchesCredential(profile, credential)) throw new IllegalArgumentException("HTTP client certificate does not match transport profile");
		this.profile = profile; this.serverId = profile.serverId(); this.onEnvelope = onEnvelope;
		this.credential = credential;
		this.credentialDirectory = credentialDirectory;
		inboundDeliveries = credentialDirectory == null ? null : new HttpInboundDeliveryStore(credentialDirectory);
		if (inboundDeliveries != null) for (var entry : inboundDeliveries.snapshot().entrySet()) {
			if (entry.getValue() == HttpInboundDeliveryStore.State.COMPLETED) {
				received.add(entry.getKey());
				queueAck(entry.getKey());
			}
		}
		client = client(profile, credential);
		transportEndpoint = profile.endpoint().resolve("v1/transport");
		// GlobalMessageHandler routes mutate backend vote state and must observe the
		// wire order. One bounded lane preserves batch ordering without running work on
		// the long-poll thread; bounded admission below backpressures this poller.
		callbackExecutor = executor("VotingPlugin-HTTP-callback", 1, CALLBACK_QUEUE_CAPACITY);
	}

	/** Convenience constructor for the owner-only credential directory produced by {@link #enroll}. */
	public HttpBackendTransportConnector(HttpConnectionCode code, String serverId, Path credentials,
			Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(HttpClientCredentialStore.loadEnrolled(credentials), onEnvelope, credentials);
		if (code == null || !profile(code, serverId).equals(this.profile)) throw new IllegalArgumentException("HTTP transport profile does not match connection code");
	}

	/** Starts normal transport using only the persisted certificate and non-secret profile. */
	public HttpBackendTransportConnector(Path credentials, Consumer<JsonEnvelope> onEnvelope) throws Exception {
		this(HttpClientCredentialStore.loadEnrolled(credentials), onEnvelope, credentials);
	}

	/** Performs enrollment network I/O; call this from a connector/setup worker, never a platform main thread. */
	public static HttpClientCredentialStore.ClientCredential enroll(HttpConnectionCode code, String serverId, Path credentials) throws Exception {
		if (code == null || credentials == null || serverId == null || !serverId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) throw new IllegalArgumentException("Enrollment configuration is invalid");
		if (!code.serverId().equals(HttpTlsIdentity.canonicalServerId(serverId)))
			throw new IllegalArgumentException("HTTP connection code belongs to a different backend");
		code.requireActive(Clock.systemUTC());
		byte[] payload = ("{\"server\":\"" + serverId + "\",\"token\":\"" + code.enrollmentToken() + "\"}").getBytes(StandardCharsets.UTF_8);
		HttpClient client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).followRedirects(HttpClient.Redirect.NEVER)
			.connectTimeout(Duration.ofSeconds(5)).sslContext(HttpPinnedTls.clientContext(code)).build();
		HttpRequest request = HttpRequest.newBuilder(code.endpoint().resolve("v1/enroll")).timeout(CLIENT_TIMEOUT)
			.header("Content-Type", "application/json").header("Cache-Control", "no-store").POST(HttpRequest.BodyPublishers.ofByteArray(payload)).build();
		LimitedResponse response = sendLimited(client, request);
		if (response.statusCode() != 201) throw new IllegalArgumentException("Enrollment was rejected");
		HttpTlsIdentity.IssuedClientCertificate issued = HttpTransportProtocol.parseEnrollmentResponse(serverId, response.body());
		HttpClientCredentialStore.saveEnrolled(credentials, code, issued); return HttpClientCredentialStore.load(credentials);
	}

	public void start() {
		if (!running.compareAndSet(false, true)) return;
		poller = new Thread(this::pollLoop, "VotingPlugin-HTTP-poll"); poller.setDaemon(true); poller.start();
	}
	/** Waits for one authenticated, protocol-valid transport response. */
	public boolean awaitFirstResponse(long deadlineNanos) throws InterruptedException {
		long remaining = deadlineNanos - System.nanoTime();
		return remaining > 0L && firstResponse.await(remaining, TimeUnit.NANOSECONDS) && running.get();
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
	public synchronized boolean pollOnce() {
		return pollOnce(CLIENT_TIMEOUT, true, true);
	}
	private boolean pollOnce(Duration timeout, boolean requireRunning, boolean acceptIncoming) {
		if (requireRunning && !running.get()) return false;
		List<String> acks = List.of(); boolean acknowledgementsConfirmed = false;
		try {
			if (requireRunning) maybeRenewCredential();
			List<HttpTransportProtocol.Delivery> messages; long requestSequence;
			synchronized (state) {
				acks = first(acknowledgements); requestSequence = sequence++;
				messages = HttpTransportProtocol.fittingMessages(serverId, session, requestSequence, acks, outgoing.values());
				for (int index = 0; index < acks.size(); index++) acknowledgements.removeFirst();
			}
			HttpRequest request = HttpRequest.newBuilder(transportEndpoint).timeout(timeout).header("Content-Type", "application/json")
				.header("Cache-Control", "no-store").POST(HttpRequest.BodyPublishers.ofByteArray(HttpTransportProtocol.request(serverId, session, requestSequence, acks, messages))).build();
			LimitedResponse response = sendLimited(client, request);
			if (response.statusCode() != 200) return false;
			HttpTransportProtocol.Packet packet = HttpTransportProtocol.parsePacket(response.body());
			if (!serverId.equals(packet.server()) || !session.equals(packet.session()) || packet.sequence() != requestSequence) return false;
			confirmAcknowledgements(acks);
			acknowledgementsConfirmed = true;
			synchronized (state) { for (String ack : packet.acks()) outgoing.remove(ack); }
			if (acceptIncoming) for (HttpTransportProtocol.Delivery delivery : accept(packet.messages())) dispatch(delivery);
			firstResponse.countDown();
			return true;
		} catch (Exception failure) { return false;
		} finally { if (!acknowledgementsConfirmed) requeueAcknowledgements(acks); }
	}
	/** Stops normal polling and gives already-queued outbound messages a bounded final delivery attempt. */
	public boolean flushOutgoing(long deadlineNanos) {
		running.set(false);
		firstResponse.countDown();
		Thread current = poller;
		if (current != null) current.interrupt();
		if (!joinPoller(current, deadlineNanos)) return false;
		while (queuedOutgoing() != 0) {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0L) return false;
			Duration timeout = Duration.ofNanos(Math.min(CLIENT_TIMEOUT.toNanos(), remaining));
			synchronized (this) {
				if (!pollOnce(timeout, false, false)) return false;
			}
		}
		return true;
	}
	@Override public void close() {
		running.getAndSet(false);
		firstResponse.countDown();
		// Revoke this connector's journal writer before a replacement snapshots it.
		// In-flight transitions serialize with seal(): either COMPLETED is already
		// durable, or the delivery remains durably RUNNING and fail-closed.
		if (inboundDeliveries != null) inboundDeliveries.seal();
		Thread current = poller; if (current != null) current.interrupt();
		callbackExecutor.shutdown(); try { if (!callbackExecutor.awaitTermination(5, TimeUnit.SECONDS)) callbackExecutor.shutdownNow(); }
		catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); callbackExecutor.shutdownNow(); }
		// The owning transport may release the credential-directory semaphore as soon
		// as close returns. Wait for the interrupted poller so an in-flight renewal
		// cannot activate an old credential generation after that ownership handoff.
		joinPoller(current);
	}
	boolean pollerAlive() { Thread current = poller; return current != null && current.isAlive(); }
	private static boolean joinPoller(Thread poller, long deadlineNanos) {
		if (poller == null || poller == Thread.currentThread()) return true;
		boolean interrupted = false;
		while (poller.isAlive()) {
			long remaining = deadlineNanos - System.nanoTime();
			if (remaining <= 0L) {
				if (interrupted) Thread.currentThread().interrupt();
				return false;
			}
			try { TimeUnit.NANOSECONDS.timedJoin(poller, remaining); }
			catch (InterruptedException stopRequested) { interrupted = true; poller.interrupt(); }
		}
		if (interrupted) Thread.currentThread().interrupt();
		return true;
	}
	private static void joinPoller(Thread poller) {
		if (poller == null || poller == Thread.currentThread()) return;
		boolean interrupted = false;
		while (poller.isAlive()) try { poller.join(); }
		catch (InterruptedException stopRequested) { interrupted = true; poller.interrupt(); }
		if (interrupted) Thread.currentThread().interrupt();
	}

	private void pollLoop() {
		long retry = 1000L;
		while (running.get()) { if (pollOnce()) { retry = 1000L; continue; } try { Thread.sleep(retry); } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); break; } retry = Math.min(30_000L, retry * 2); }
	}
	List<HttpTransportProtocol.Delivery> accept(List<HttpTransportProtocol.Delivery> deliveries) {
		synchronized (state) {
			List<HttpTransportProtocol.Delivery> accepted = new java.util.ArrayList<>();
			for (HttpTransportProtocol.Delivery delivery : deliveries) {
				HttpInboundDeliveryStore.State persisted = inboundDeliveries == null ? null : inboundDeliveries.state(delivery.id());
				if (received.contains(delivery.id()) || persisted == HttpInboundDeliveryStore.State.COMPLETED) {
					received.add(delivery.id()); queueAck(delivery.id()); continue;
				}
				// A callback that was running when the process stopped may already have
				// produced external side effects. Keep the proxy copy without replaying or
				// acknowledging it; arbitrary plugin callbacks cannot share this journal.
				if (persisted == HttpInboundDeliveryStore.State.RUNNING) continue;
				if (!processing.contains(delivery.id())) {
					processing.add(delivery.id()); accepted.add(delivery);
				}
			}
			return accepted;
		}
	}
	void dispatch(HttpTransportProtocol.Delivery delivery) {
		Runnable callback = () -> {
			boolean success = false;
			try {
				if (inboundDeliveries != null) {
					if (inboundDeliveries.state(delivery.id()) == null) inboundDeliveries.reserve(delivery.id());
					inboundDeliveries.markRunning(delivery.id());
				}
				onEnvelope.accept(delivery.envelope());
				if (inboundDeliveries != null) inboundDeliveries.markCompleted(delivery.id());
				success = true;
			} catch (IOException persistenceFailure) {
				// Never run before RUNNING is durable and never acknowledge until
				// COMPLETED is durable. An uncertain transition stays fail-closed.
			} catch (RuntimeException callbackFailure) {
				// The callback may have failed after partial external effects. Leave RUNNING
				// unacknowledged so a restart cannot silently lose or duplicate the delivery.
			}
			completeIncoming(delivery.id(), success);
		};
		if (!executeOrdered(callbackExecutor, callback)) completeIncoming(delivery.id(), false);
	}
	void completeIncoming(String id, boolean success) { synchronized (state) {
		processing.remove(id);
		if (success) { received.add(id); while (received.size() > HttpTransportProtocol.MAX_QUEUE) received.remove(received.iterator().next()); queueAck(id); }
	} }
	private void queueAck(String id) { if (acknowledgements.size() < HttpTransportProtocol.MAX_QUEUE && !acknowledgements.contains(id)) acknowledgements.add(id); }
	private void requeueAcknowledgements(List<String> ids) { synchronized (state) {
		for (int index = ids.size() - 1; index >= 0; index--) {
			String id = ids.get(index);
			if (!acknowledgements.contains(id)) {
				while (acknowledgements.size() >= HttpTransportProtocol.MAX_QUEUE) acknowledgements.removeLast();
				acknowledgements.addFirst(id);
			}
		}
	} }
	private void confirmAcknowledgements(Collection<String> ids) {
		for (String id : ids) {
			if (inboundDeliveries != null) try { inboundDeliveries.remove(id); }
			catch (IOException cleanupFailure) { continue; }
			synchronized (state) { received.remove(id); }
		}
	}
	int queuedOutgoing() { synchronized (state) { return outgoing.size(); } }
	List<String> drainAcknowledgements() { synchronized (state) { return drain(acknowledgements); } }
	private static <T> List<T> first(Collection<T> values) { List<T> output = new java.util.ArrayList<>(); for (T value : values) { output.add(value); if (output.size() == HttpTransportProtocol.MAX_BATCH) break; } return output; }
	private static List<String> drain(ArrayDeque<String> values) { List<String> output = new java.util.ArrayList<>(); while (!values.isEmpty() && output.size() < HttpTransportProtocol.MAX_BATCH) output.add(values.remove()); return output; }
	private static ThreadPoolExecutor executor(String name, int threads, int queue) { ThreadFactory factory = task -> { Thread thread = new Thread(task, name); thread.setDaemon(true); return thread; }; return new ThreadPoolExecutor(threads, threads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(queue), factory, new ThreadPoolExecutor.AbortPolicy()); }
	static boolean executeOrdered(ThreadPoolExecutor executor, Runnable task) {
		try { executor.execute(task); return true; }
		catch (RejectedExecutionException fullOrClosed) {
			if (executor.isShutdown()) return false;
			try {
				while (!executor.isShutdown()) {
					if (!executor.getQueue().offer(task, 100L, TimeUnit.MILLISECONDS)) continue;
					if (executor.isShutdown() && executor.remove(task)) return false;
					return true;
				}
			} catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
			return false;
		}
	}
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
	private void maybeRenewCredential() {
		synchronized (renewal) {
			Path directory = credentialDirectory;
			if (directory == null || !HttpTlsIdentity.needsRenewal(credential.certificate(), Clock.systemUTC())) return;
			long now = System.nanoTime();
			if (nextRenewalCheckNanos != 0L && now - nextRenewalCheckNanos < 0L) return;
			nextRenewalCheckNanos = now + Duration.ofHours(6).toNanos();
			try {
				byte[] body = HttpTransportProtocol.renewalRequest(serverId);
				HttpRequest request = HttpRequest.newBuilder(profile.endpoint().resolve("v1/renew")).timeout(CLIENT_TIMEOUT)
						.header("Content-Type", "application/json").header("Cache-Control", "no-store")
						.POST(HttpRequest.BodyPublishers.ofByteArray(body)).build();
				LimitedResponse response = sendLimited(client, request);
				if (response.statusCode() != 201) return;
				HttpTlsIdentity.IssuedClientCertificate issued = HttpTransportProtocol.parseEnrollmentResponse(serverId, response.body());
				HttpClientCredentialStore.StagedCredential staged = HttpClientCredentialStore.stageReplacement(directory, issued);
				HttpClientCredentialStore.ClientCredential replacement = staged.credential();
				HttpClientCredentialStore.HttpClientProfile replacementProfile = staged.profile();
				if (!matchesCredential(replacementProfile, replacement)) throw new IllegalArgumentException("Renewed HTTP certificate is invalid");
				HttpClient replacementClient = client(replacementProfile, replacement);
				HttpClientCredentialStore.activateReplacement(directory, staged);
				profile = replacementProfile;
				client = replacementClient;
				credential = replacement;
			} catch (Exception ignored) { /* The active generation is unchanged; retry on the bounded schedule. */ }
		}
	}
	private static HttpClient client(HttpClientCredentialStore.HttpClientProfile profile,
			HttpClientCredentialStore.ClientCredential credential) throws Exception {
		return HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).followRedirects(HttpClient.Redirect.NEVER)
				.connectTimeout(Duration.ofSeconds(5)).sslContext(clientContext(profile, credential)).build();
	}
	static LimitedResponse sendLimited(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
		CompletableFuture<HttpResponse<byte[]>> exchange = client.sendAsync(request,
				ignored -> new LimitedBodySubscriber(HttpTransportProtocol.MAX_BODY_BYTES));
		HttpResponse<byte[]> response;
		try {
			Duration timeout = request.timeout().orElse(CLIENT_TIMEOUT);
			response = exchange.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
		} catch (TimeoutException timeout) {
			exchange.cancel(true);
			throw new HttpTimeoutException("HTTP transport response timed out");
		} catch (InterruptedException interrupted) {
			exchange.cancel(true);
			throw interrupted;
		} catch (ExecutionException failure) {
			Throwable cause = failure.getCause();
			if (cause instanceof IOException ioFailure) throw ioFailure;
			throw new IOException("HTTP transport request failed", cause);
		}
		long declaredLength = response.headers().firstValueAsLong("Content-Length").orElse(-1L);
		if (declaredLength > HttpTransportProtocol.MAX_BODY_BYTES)
			throw new IOException("HTTP transport response exceeds its limit");
		return new LimitedResponse(response.statusCode(), response.body());
	}

	private static final class LimitedBodySubscriber implements HttpResponse.BodySubscriber<byte[]> {
		private final int maximum;
		private final ByteArrayOutputStream body = new ByteArrayOutputStream();
		private final CompletableFuture<byte[]> result = new CompletableFuture<>();
		private Flow.Subscription subscription;

		private LimitedBodySubscriber(int maximum) {
			this.maximum = maximum;
		}

		@Override
		public CompletionStage<byte[]> getBody() {
			return result;
		}

		@Override
		public void onSubscribe(Flow.Subscription subscription) {
			if (this.subscription != null) {
				subscription.cancel();
				return;
			}
			this.subscription = subscription;
			subscription.request(1);
		}

		@Override
		public void onNext(List<ByteBuffer> buffers) {
			try {
				for (ByteBuffer buffer : buffers) {
					if (buffer.remaining() > maximum - body.size()) {
						subscription.cancel();
						result.completeExceptionally(new IOException("HTTP transport response exceeds its limit"));
						return;
					}
					byte[] chunk = new byte[buffer.remaining()];
					buffer.get(chunk);
					body.writeBytes(chunk);
				}
				subscription.request(1);
			} catch (RuntimeException failure) {
				subscription.cancel();
				result.completeExceptionally(failure);
			}
		}

		@Override
		public void onError(Throwable failure) {
			result.completeExceptionally(failure);
		}

		@Override
		public void onComplete() {
			result.complete(body.toByteArray());
		}
	}
	static byte[] readLimited(InputStream body) throws IOException {
		byte[] bytes = body.readNBytes(HttpTransportProtocol.MAX_BODY_BYTES + 1);
		if (bytes.length > HttpTransportProtocol.MAX_BODY_BYTES)
			throw new IOException("HTTP transport response exceeds its limit");
		return bytes;
	}
	record LimitedResponse(int statusCode, byte[] body) { }
	private static SSLContext clientContext(HttpClientCredentialStore.HttpClientProfile profile, HttpClientCredentialStore.ClientCredential credential) throws Exception {
		String caPin = HttpTransportSecrets.certificatePin(credential.caCertificate());
		if (!HttpTransportSecrets.constantTimeEquals(profile.caCertificatePin().getBytes(StandardCharsets.US_ASCII),
				caPin.getBytes(StandardCharsets.US_ASCII))) throw new IllegalArgumentException("HTTP authority does not match transport profile");
		char[] password = credential.password();
		try {
			KeyStore store = KeyStore.getInstance("PKCS12"); store.load(null, new char[0]);
			store.setKeyEntry("client", credential.privateKey(), password,
					new java.security.cert.Certificate[] { credential.certificate(), credential.caCertificate() });
			KeyManagerFactory keys = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm()); keys.init(store, password);
			KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType()); trustStore.load(null, new char[0]);
			trustStore.setCertificateEntry("http-transport-ca", credential.caCertificate());
			javax.net.ssl.TrustManagerFactory trusts = javax.net.ssl.TrustManagerFactory.getInstance(
					javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm());
			trusts.init(trustStore);
			SSLContext context = SSLContext.getInstance("TLS"); context.init(keys.getKeyManagers(), trusts.getTrustManagers(), null); return context;
		} finally { java.util.Arrays.fill(password, '\0'); }
	}
}
