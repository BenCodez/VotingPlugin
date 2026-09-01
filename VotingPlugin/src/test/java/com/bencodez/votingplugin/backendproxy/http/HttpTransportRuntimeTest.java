package com.bencodez.votingplugin.backendproxy.http;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import java.net.InetSocketAddress;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HttpTransportRuntimeTest {
	@TempDir Path directory;

	@Test
	void enrollsThenDeliversBothDirectionsWithAuthenticatedIdentity() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("authority"));
		CountDownLatch proxyReceived = new CountDownLatch(1), backendReceived = new CountDownLatch(1);
		AtomicReference<HttpProxyTransportServer.ReceivedEnvelope> received = new AtomicReference<>();
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity, authority,
				message -> { received.set(message); proxyReceived.countDown(); })) {
			server.start();
			HttpConnectionCode code = authority.createConnectionCode("lobby-1", server.endpoint("localhost"), Duration.ofMinutes(5));
			HttpBackendTransportConnector.enroll(code, "lobby-1", directory.resolve("client"));
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(directory.resolve("client"),
					envelope -> backendReceived.countDown())) {
				connector.start();
				assertTrue(connector.awaitFirstResponse(System.nanoTime() + TimeUnit.SECONDS.toNanos(8)),
						"an authenticated transport response must make the connector ready");
				assertTrue(connector.send(JsonEnvelope.builder("to-proxy").put("server", "forged").build()));
				assertTrue(proxyReceived.await(8, TimeUnit.SECONDS));
				assertEquals("lobby-1", received.get().serverId());
				assertEquals("lobby-1", received.get().envelope().getFields().get("server"));
				long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
				while (connector.queuedOutgoing() != 0 && System.nanoTime() < deadline) Thread.sleep(10);
				assertEquals(0, connector.queuedOutgoing(), "proxy ACK must remove the exact outbound delivery ID");
				assertTrue(server.send("lobby-1", JsonEnvelope.builder("to-backend").build()));
				assertTrue(backendReceived.await(8, TimeUnit.SECONDS));
				Path inboundFence = directory.resolve("client").resolve("http-transport-inbound-deliveries");
				long fenceDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
				while (countRegularFiles(inboundFence) != 0L && System.nanoTime() < fenceDeadline) Thread.sleep(20);
				assertEquals(0L, countRegularFiles(inboundFence), "a confirmed ACK must remove the backend replay fence");
			}
		}
	}

	@Test
	void normalTransportRejectsAClientWithoutCertificate() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("authority"));
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity, authority, ignored -> { })) {
			server.start();
			HttpConnectionCode code = authority.createConnectionCode("lobby-1", server.endpoint("localhost"), Duration.ofMinutes(5));
			HttpClient client = HttpClient.newBuilder().sslContext(HttpPinnedTls.clientContext(code)).build();
			byte[] body = HttpTransportProtocol.request("lobby-1", java.util.UUID.randomUUID().toString(), 0, java.util.List.of(), java.util.List.of());
			HttpResponse<byte[]> response = client.send(HttpRequest.newBuilder(code.endpoint().resolve("v1/transport"))
				.timeout(Duration.ofSeconds(5)).header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofByteArray(body)).build(), HttpResponse.BodyHandlers.ofByteArray());
			assertEquals(401, response.statusCode());
		}
	}

	@Test
	void boundedQueuesFailClosed() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("authority"));
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity, authority, ignored -> { })) {
			for (int i = 0; i < HttpTransportProtocol.MAX_QUEUE; i++) assertTrue(server.send("lobby-1", JsonEnvelope.builder("x").build()));
			assertFalse(server.send("lobby-1", JsonEnvelope.builder("x").build()));
			assertFalse(server.send("lobby-1", JsonEnvelope.builder("x").put("large", "x".repeat(HttpTransportProtocol.MAX_ENVELOPE_BYTES)).build()));
		}
	}

	@Test
	void proxyOutgoingQueueSurvivesRestartUntilBackendAcknowledges() throws Exception {
		Path proxyDirectory = directory.resolve("proxy");
		Path authorityDirectory = directory.resolve("authority");
		Path queueDirectory = directory.resolve("outgoing");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(proxyDirectory, "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, authorityDirectory);
		HttpProxyTransportServer first = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0),
				identity, authority, queueDirectory, ignored -> { });
		assertTrue(first.send("lobby-1", JsonEnvelope.builder("durable").build()));
		first.close();

		CountDownLatch received = new CountDownLatch(1);
		try (HttpProxyTransportServer restarted = new HttpProxyTransportServer(
				new InetSocketAddress("localhost", 0), identity, authority, queueDirectory, ignored -> { })) {
			restarted.start();
			HttpConnectionCode code = authority.createConnectionCode("lobby-1", restarted.endpoint("localhost"),
					Duration.ofMinutes(5));
			HttpClientCredentialStore.ClientCredential credential = HttpBackendTransportConnector.enroll(code,
					"lobby-1", directory.resolve("durable-client"));
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(code, "lobby-1",
					credential, envelope -> received.countDown())) {
				connector.start();
				assertTrue(received.await(8, TimeUnit.SECONDS));
				long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
				while (countRegularFiles(queueDirectory) != 0L && System.nanoTime() < deadline) Thread.sleep(20);
				assertEquals(0L, countRegularFiles(queueDirectory), "backend ACK must durably remove the delivery");
			}
		}
	}

	@Test
	void pollCreatedBackendStateUsesDurableOutgoingQueue() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("poll-proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("poll-authority"));
		Path queueDirectory = directory.resolve("poll-outgoing");
		CountDownLatch proxyReceived = new CountDownLatch(1), backendReceived = new CountDownLatch(1);
		CountDownLatch releaseBackendCallback = new CountDownLatch(1);
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0),
				identity, authority, queueDirectory, ignored -> proxyReceived.countDown())) {
			server.start();
			HttpConnectionCode code = authority.createConnectionCode("lobby-1", server.endpoint("localhost"),
					Duration.ofMinutes(5));
			HttpClientCredentialStore.ClientCredential credential = HttpBackendTransportConnector.enroll(code,
					"lobby-1", directory.resolve("poll-client"));
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(code, "lobby-1",
					credential, envelope -> {
						backendReceived.countDown();
						try { releaseBackendCallback.await(5, TimeUnit.SECONDS); }
						catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
					})) {
				connector.start();
				assertTrue(connector.send(JsonEnvelope.builder("establish-poll").build()));
				assertTrue(proxyReceived.await(8, TimeUnit.SECONDS));
				assertTrue(server.send("lobby-1", JsonEnvelope.builder("durable-after-poll").build()));
				assertTrue(backendReceived.await(8, TimeUnit.SECONDS));
				assertEquals(1L, countRegularFiles(queueDirectory),
						"a poll-created backend state must persist before reporting acceptance");
				releaseBackendCallback.countDown();
			}
		} finally {
			releaseBackendCallback.countDown();
		}
	}

	private static long countRegularFiles(Path root) throws Exception {
		try (java.util.stream.Stream<Path> paths = java.nio.file.Files.walk(root)) {
			return paths.filter(path -> java.nio.file.Files.isRegularFile(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)).count();
		}
	}

	@Test
	void aggregatePacketBudgetSplitsLargeValidEnvelopes() {
		java.util.List<HttpTransportProtocol.Delivery> candidates = new java.util.ArrayList<>();
		for (int index = 0; index < 12; index++) candidates.add(new HttpTransportProtocol.Delivery(
				java.util.UUID.randomUUID().toString(), JsonEnvelope.builder("large").put("value", "x".repeat(40_000)).build()));
		String session = java.util.UUID.randomUUID().toString();
		java.util.List<HttpTransportProtocol.Delivery> fitted = HttpTransportProtocol.fittingMessages(
				"lobby-1", session, 0, java.util.List.of(), candidates);
		assertTrue(fitted.size() > 0 && fitted.size() < candidates.size());
		assertTrue(HttpTransportProtocol.request("lobby-1", session, 0, java.util.List.of(), fitted).length
				<= HttpTransportProtocol.MAX_BODY_BYTES);
	}

	@Test
	void packetNumbersMustUseCanonicalJsonIntegerTokens() {
		com.google.gson.JsonObject packet = com.google.gson.JsonParser.parseString(new String(HttpTransportProtocol.request(
				"lobby-1", java.util.UUID.randomUUID().toString(), 0, java.util.List.of(), java.util.List.of()),
				java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject();
		assertDoesNotThrow(() -> HttpTransportProtocol.parsePacket(packet.toString()
				.getBytes(java.nio.charset.StandardCharsets.UTF_8)));
		String timestamp = packet.get("timestamp").getAsString();
		java.util.Map<String, java.util.List<String>> invalid = java.util.Map.of(
				"v", java.util.List.of("\"1\"", "1.0", "1e0"),
				"sequence", java.util.List.of("\"0\"", "0.0", "0e0"),
				"timestamp", java.util.List.of("\"" + timestamp + "\"", timestamp + ".0", timestamp + "e0"));
		for (var field : invalid.entrySet()) for (String token : field.getValue()) {
			com.google.gson.JsonObject rejected = packet.deepCopy();
			rejected.add(field.getKey(), com.google.gson.JsonParser.parseString(token));
			assertThrows(IllegalArgumentException.class, () -> HttpTransportProtocol.parsePacket(
					rejected.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8)), field.getKey() + "=" + token);
		}
	}

	@Test
	void packetParsingRejectsNoncanonicalUuidForms() {
		String deliveryId = java.util.UUID.randomUUID().toString();
		com.google.gson.JsonObject packet = com.google.gson.JsonParser.parseString(new String(HttpTransportProtocol.request(
				"lobby-1", java.util.UUID.randomUUID().toString(), 0, java.util.List.of(deliveryId),
				java.util.List.of(new HttpTransportProtocol.Delivery(deliveryId, JsonEnvelope.builder("payload").build()))),
				java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject();
		String abbreviated = "1-1-1-1-1";

		com.google.gson.JsonObject invalidSession = packet.deepCopy();
		invalidSession.addProperty("session", abbreviated);
		assertThrows(IllegalArgumentException.class, () -> HttpTransportProtocol.parsePacket(
				invalidSession.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8)));

		com.google.gson.JsonObject invalidAck = packet.deepCopy();
		invalidAck.getAsJsonArray("acks").set(0, new com.google.gson.JsonPrimitive(abbreviated));
		assertThrows(IllegalArgumentException.class, () -> HttpTransportProtocol.parsePacket(
				invalidAck.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8)));

		com.google.gson.JsonObject invalidMessage = packet.deepCopy();
		invalidMessage.getAsJsonArray("messages").get(0).getAsJsonObject().addProperty("id", abbreviated);
		assertThrows(IllegalArgumentException.class, () -> HttpTransportProtocol.parsePacket(
				invalidMessage.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8)));
	}

	@Test
	void persistedProfileStartsAfterTheEnrollmentCodeExpires() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("authority"));
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity, authority, ignored -> { })) {
			server.start();
			HttpConnectionCode active = authority.createConnectionCode("lobby-1", server.endpoint("localhost"), Duration.ofMinutes(5));
			HttpBackendTransportConnector.enroll(active, "lobby-1", directory.resolve("client"));
			HttpConnectionCode expired = new HttpConnectionCode(active.serverId(), active.endpoint(), active.serverCertificatePin(), active.caCertificatePin(),
					java.time.Instant.now().minusSeconds(1), active.enrollmentToken());
			try (HttpBackendTransportConnector ignored = new HttpBackendTransportConnector(expired, "lobby-1", directory.resolve("client"), message -> { })) {
				assertTrue(true);
			}
			try (HttpBackendTransportConnector ignored = new HttpBackendTransportConnector(directory.resolve("client"), message -> { })) {
				assertTrue(true);
			}
		}
	}

	@Test
	void persistedBackendConnectsAfterAutomaticServerLeafRotation() throws Exception {
		Instant now = Instant.now();
		Path proxyDirectory = directory.resolve("proxy");
		HttpTlsIdentity original = HttpTlsIdentity.loadOrCreate(proxyDirectory, "localhost",
				java.time.Clock.fixed(now.minus(Duration.ofDays(340)), java.time.ZoneOffset.UTC));
		String originalServerPin = HttpTransportSecrets.certificatePin(original.serverCertificate());
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(original, directory.resolve("authority"));
		HttpTlsIdentity rotated = HttpTlsIdentity.loadOrCreate(proxyDirectory, "localhost");
		CountDownLatch received = new CountDownLatch(1);
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), rotated,
				authority, ignored -> received.countDown())) {
			HttpConnectionCode activeCode = authority.createConnectionCode("lobby-1", server.endpoint("localhost"), Duration.ofMinutes(5));
			HttpTlsIdentity.IssuedClientCertificate issued = authority.enroll("lobby-1", activeCode.enrollmentToken());
			HttpConnectionCode oldProfileCode = new HttpConnectionCode(activeCode.serverId(), activeCode.endpoint(), originalServerPin,
					activeCode.caCertificatePin(), activeCode.expiresAt(), activeCode.enrollmentToken());
			HttpClientCredentialStore.saveEnrolled(directory.resolve("client"), oldProfileCode, issued);
			assertFalse(oldProfileCode.serverCertificatePin().equals(rotated.serverCertificatePin()));
			server.start();
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(directory.resolve("client"), ignored -> { })) {
				connector.start();
				assertTrue(connector.send(JsonEnvelope.builder("after-rotation").build()));
				assertTrue(received.await(8, TimeUnit.SECONDS));
			}
		}
	}

	@Test
	void backendRenewsClientCertificateBeforeExpiryWithoutNewConnectionCode() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate expiring = identity.issueClientCertificate("lobby-1",
				Instant.now().minus(Duration.ofDays(340)));
		String originalPin = HttpTransportSecrets.certificatePin(expiring.certificate());
		Path authorityDirectory = directory.resolve("authority");
		java.nio.file.Files.createDirectories(authorityDirectory);
		String key = java.util.Base64.getUrlEncoder().withoutPadding()
				.encodeToString("lobby-1".getBytes(java.nio.charset.StandardCharsets.UTF_8));
		java.nio.file.Files.writeString(authorityDirectory.resolve("http-transport-clients.properties"),
				"version=2\nbinding." + key + "=" + originalPin + ":-:0\n");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, authorityDirectory);
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity,
				authority, ignored -> { })) {
			HttpConnectionCode profileCode = new HttpConnectionCode("lobby-1", server.endpoint("localhost"),
					identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "A".repeat(43));
			Path clientDirectory = directory.resolve("client");
			HttpClientCredentialStore.saveEnrolled(clientDirectory, profileCode, expiring);
			server.start();
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(clientDirectory, ignored -> { })) {
				connector.start();
				long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(8);
				String renewedPin = originalPin;
				while (renewedPin.equals(originalPin) && System.nanoTime() < deadline) {
					Thread.sleep(25);
					renewedPin = HttpTransportSecrets.certificatePin(HttpClientCredentialStore.load(clientDirectory).certificate());
				}
				assertFalse(renewedPin.equals(originalPin));
				HttpClientCredentialStore.ClientCredential renewed = HttpClientCredentialStore.load(clientDirectory);
				long promotionDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
				while (authority.authenticate("lobby-1", expiring.certificate()) && System.nanoTime() < promotionDeadline) Thread.sleep(25);
				assertTrue(authority.authenticate("lobby-1", renewed.certificate()));
				assertFalse(authority.authenticate("lobby-1", expiring.certificate()));
			}
		}
	}

	@Test
	void duplicateInboundDeliveryIsReAcknowledgedWithoutSecondDispatch() {
		HttpProxyTransportServer.BackendState state = new HttpProxyTransportServer.BackendState();
		String session = java.util.UUID.randomUUID().toString();
		String id = java.util.UUID.randomUUID().toString();
		HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("x").build());
		assertTrue(state.acceptSession(session, 0));
		assertEquals(1, state.acceptIncoming(java.util.List.of(delivery)).size());
		state.completeIncoming(id, true);
		assertEquals(java.util.List.of(id), state.await("lobby-1", session, 0).acks());
		assertTrue(state.acceptSession(session, 1));
		assertTrue(state.acceptIncoming(java.util.List.of(delivery)).isEmpty());
		assertEquals(java.util.List.of(id), state.await("lobby-1", session, 1).acks());
		String replacementSession = java.util.UUID.randomUUID().toString();
		assertTrue(state.acceptSession(replacementSession, 0));
		assertTrue(state.acceptIncoming(java.util.List.of(delivery)).isEmpty());
		assertEquals(java.util.List.of(id), state.await("lobby-1", replacementSession, 0).acks());
	}

	@Test
	void proxyDedupWindowEvictsOldestCompletedDeliveryAtCapacity() {
		HttpProxyTransportServer.BackendState state = new HttpProxyTransportServer.BackendState();
		String oldest = null, newest = null;
		for (int index = 0; index < HttpTransportProtocol.MAX_QUEUE + 1; index++) {
			String id = java.util.UUID.randomUUID().toString();
			if (index == 0) oldest = id;
			newest = id;
			HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("x").build());
			assertEquals(1, state.acceptIncoming(java.util.List.of(delivery)).size());
			state.completeIncoming(id, true);
		}
		HttpTransportProtocol.Delivery evicted = new HttpTransportProtocol.Delivery(oldest, JsonEnvelope.builder("x").build());
		HttpTransportProtocol.Delivery retained = new HttpTransportProtocol.Delivery(newest, JsonEnvelope.builder("x").build());
		assertEquals(1, state.acceptIncoming(java.util.List.of(evicted)).size());
		assertTrue(state.acceptIncoming(java.util.List.of(retained)).isEmpty());
	}

	@Test
	void backendReAcknowledgesLostAckDuplicateWithoutSecondCallback() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.save(directory.resolve("client"), issued);
		HttpClientCredentialStore.HttpClientProfile profile = new HttpClientCredentialStore.HttpClientProfile("lobby-1",
				java.net.URI.create("https://localhost:8443/"), identity.serverCertificatePin(), identity.caCertificatePin());
		CountDownLatch callback = new CountDownLatch(1);
		try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(profile,
				HttpClientCredentialStore.load(directory.resolve("client")), envelope -> callback.countDown())) {
			String id = java.util.UUID.randomUUID().toString();
			HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("x").build());
			connector.dispatch(delivery);
			assertTrue(callback.await(2, TimeUnit.SECONDS));
			java.util.List<String> acknowledgements = java.util.List.of();
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
			while (acknowledgements.isEmpty() && System.nanoTime() < deadline) {
				acknowledgements = connector.drainAcknowledgements();
				if (acknowledgements.isEmpty()) Thread.sleep(5);
			}
			assertEquals(java.util.List.of(id), acknowledgements);
			assertTrue(connector.accept(java.util.List.of(delivery)).isEmpty());
			assertEquals(java.util.List.of(id), connector.drainAcknowledgements());
		}
	}

	@Test
	void backendCallbacksAreSerializedInDeliveryOrder() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("ordered-proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.save(directory.resolve("ordered-client"), issued);
		HttpClientCredentialStore.HttpClientProfile profile = new HttpClientCredentialStore.HttpClientProfile("lobby-1",
				java.net.URI.create("https://localhost:8443/"), identity.serverCertificatePin(), identity.caCertificatePin());
		CountDownLatch firstStarted = new CountDownLatch(1), releaseFirst = new CountDownLatch(1), secondStarted = new CountDownLatch(1);
		java.util.List<String> order = new java.util.concurrent.CopyOnWriteArrayList<>();
		try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(profile,
				HttpClientCredentialStore.load(directory.resolve("ordered-client")), envelope -> {
					String marker = String.valueOf(envelope.getFields().get("marker"));
					order.add(marker);
					if ("first".equals(marker)) {
						firstStarted.countDown();
						try { releaseFirst.await(5, TimeUnit.SECONDS); }
						catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
					} else secondStarted.countDown();
				})) {
			connector.dispatch(new HttpTransportProtocol.Delivery(java.util.UUID.randomUUID().toString(),
					JsonEnvelope.builder("x").put("marker", "first").build()));
			connector.dispatch(new HttpTransportProtocol.Delivery(java.util.UUID.randomUUID().toString(),
					JsonEnvelope.builder("x").put("marker", "second").build()));
			assertTrue(firstStarted.await(2, TimeUnit.SECONDS));
			assertFalse(secondStarted.await(150, TimeUnit.MILLISECONDS));
			releaseFirst.countDown();
			assertTrue(secondStarted.await(2, TimeUnit.SECONDS));
			assertEquals(java.util.List.of("first", "second"), order);
		} finally { releaseFirst.countDown(); }
	}

	@Test
	void backendCallbackQueueBackpressuresWithoutBreakingFifo() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("backpressure-proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.save(directory.resolve("backpressure-client"), issued);
		HttpClientCredentialStore.HttpClientProfile profile = new HttpClientCredentialStore.HttpClientProfile("lobby-1",
				java.net.URI.create("https://localhost:8443/"), identity.serverCertificatePin(), identity.caCertificatePin());
		CountDownLatch firstStarted = new CountDownLatch(1), releaseFirst = new CountDownLatch(1);
		int deliveries = HttpBackendTransportConnector.CALLBACK_QUEUE_CAPACITY + 2;
		CountDownLatch completed = new CountDownLatch(deliveries), overflowSubmitted = new CountDownLatch(1);
		java.util.List<Integer> order = new java.util.concurrent.CopyOnWriteArrayList<>();
		try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(profile,
				HttpClientCredentialStore.load(directory.resolve("backpressure-client")), envelope -> {
			int marker = Integer.parseInt(envelope.getFields().get("marker"));
			order.add(marker);
			if (marker == 0) {
				firstStarted.countDown();
				try { releaseFirst.await(5, TimeUnit.SECONDS); }
				catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
			}
			completed.countDown();
		})) {
			connector.dispatch(delivery(0));
			assertTrue(firstStarted.await(2, TimeUnit.SECONDS));
			for (int marker = 1; marker <= HttpBackendTransportConnector.CALLBACK_QUEUE_CAPACITY; marker++)
				connector.dispatch(delivery(marker));
			Thread overflow = new Thread(() -> {
				connector.dispatch(delivery(deliveries - 1));
				overflowSubmitted.countDown();
			}, "HTTP-overflow-submitter");
			overflow.start();
			assertFalse(overflowSubmitted.await(150, TimeUnit.MILLISECONDS), "a full ordered lane must backpressure its producer");
			releaseFirst.countDown();
			assertTrue(overflowSubmitted.await(2, TimeUnit.SECONDS));
			assertTrue(completed.await(5, TimeUnit.SECONDS));
			assertEquals(java.util.stream.IntStream.range(0, deliveries).boxed().toList(), order);
		} finally { releaseFirst.countDown(); }
	}

	@Test
	void durableBackendFencePreventsCallbackReplayAfterRestartBeforeAck() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("fence-proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", java.net.URI.create("https://localhost:8443/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(300), "A".repeat(43));
		Path clientDirectory = directory.resolve("fence-client");
		HttpClientCredentialStore.saveEnrolled(clientDirectory, code, issued);
		java.util.concurrent.atomic.AtomicInteger callbacks = new java.util.concurrent.atomic.AtomicInteger();
		String id = java.util.UUID.randomUUID().toString();
		HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("vote").build());
		try (HttpBackendTransportConnector first = new HttpBackendTransportConnector(clientDirectory,
				ignored -> callbacks.incrementAndGet())) {
			first.dispatch(delivery);
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
			while (callbacks.get() != 1 && System.nanoTime() < deadline) Thread.sleep(5);
			assertEquals(1, callbacks.get());
		}
		try (HttpBackendTransportConnector restarted = new HttpBackendTransportConnector(clientDirectory,
				ignored -> callbacks.incrementAndGet())) {
			assertEquals(java.util.List.of(id), restarted.drainAcknowledgements(),
					"restart must retain and acknowledge the pre-callback delivery fence");
			assertTrue(restarted.accept(java.util.List.of(delivery)).isEmpty());
			assertEquals(1, callbacks.get(), "a durable proxy replay must not award twice");
		}
	}

	@Test
	void failedBackendCallbackRemainsUnacknowledgedAndIsNotReplayed() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("retry-fence-proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", java.net.URI.create("https://localhost:8443/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(300), "B".repeat(43));
		Path clientDirectory = directory.resolve("retry-fence-client");
		HttpClientCredentialStore.saveEnrolled(clientDirectory, code, issued);
		java.util.concurrent.atomic.AtomicInteger attempts = new java.util.concurrent.atomic.AtomicInteger();
		CountDownLatch failed = new CountDownLatch(1);
		HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(java.util.UUID.randomUUID().toString(),
				JsonEnvelope.builder("vote").build());
		try (HttpBackendTransportConnector first = new HttpBackendTransportConnector(clientDirectory, ignored -> {
			attempts.incrementAndGet(); failed.countDown(); throw new IllegalStateException("retry");
		})) {
			first.dispatch(delivery);
			assertTrue(failed.await(2, TimeUnit.SECONDS));
			assertTrue(first.drainAcknowledgements().isEmpty());
		}
		try (HttpBackendTransportConnector restarted = new HttpBackendTransportConnector(clientDirectory,
				ignored -> attempts.incrementAndGet())) {
			assertTrue(restarted.drainAcknowledgements().isEmpty());
			java.util.List<HttpTransportProtocol.Delivery> accepted = restarted.accept(java.util.List.of(delivery));
			assertTrue(accepted.isEmpty(), "an ambiguous callback must not be awarded twice");
			assertEquals(1, attempts.get());
		}
	}

	@Test
	void reservedButNotStartedDeliveryResumesAfterRestart() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("reserved-proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", java.net.URI.create("https://localhost:8443/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(300), "C".repeat(43));
		Path clientDirectory = directory.resolve("reserved-client");
		HttpClientCredentialStore.saveEnrolled(clientDirectory, code, issued);
		String id = java.util.UUID.randomUUID().toString();
		new HttpInboundDeliveryStore(clientDirectory).reserve(id);
		CountDownLatch completed = new CountDownLatch(1);
		HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("vote").build());
		try (HttpBackendTransportConnector restarted = new HttpBackendTransportConnector(clientDirectory, ignored -> completed.countDown())) {
			assertTrue(restarted.drainAcknowledgements().isEmpty(), "a reservation alone must never be acknowledged");
			java.util.List<HttpTransportProtocol.Delivery> accepted = restarted.accept(java.util.List.of(delivery));
			assertEquals(1, accepted.size());
			restarted.dispatch(accepted.get(0));
			assertTrue(completed.await(2, TimeUnit.SECONDS));
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
			java.util.List<String> acknowledgements = java.util.List.of();
			while (acknowledgements.isEmpty() && System.nanoTime() < deadline) {
				acknowledgements = restarted.drainAcknowledgements();
				if (acknowledgements.isEmpty()) Thread.sleep(5);
			}
			assertEquals(java.util.List.of(id), acknowledgements);
		}
	}

	@Test
	void interruptedStateRenameRetainsTheFurthestSafeState() throws Exception {
		Path clientDirectory = directory.resolve("interrupted-state-client");
		String id = java.util.UUID.randomUUID().toString();
		String completedId = java.util.UUID.randomUUID().toString();
		Path states = clientDirectory.resolve("http-transport-inbound-deliveries");
		Files.createDirectories(states);
		Files.writeString(states.resolve(id + ".reserved"), id);
		Files.writeString(states.resolve(id + ".running"), id);
		Files.writeString(states.resolve(completedId + ".running"), completedId);
		Files.writeString(states.resolve(completedId + ".completed"), completedId);
		HttpInboundDeliveryStore store = new HttpInboundDeliveryStore(clientDirectory);
		assertEquals(HttpInboundDeliveryStore.State.RUNNING, store.state(id));
		assertEquals(HttpInboundDeliveryStore.State.COMPLETED, store.state(completedId));
		assertFalse(Files.exists(states.resolve(id + ".reserved")));
		assertTrue(Files.exists(states.resolve(id + ".running")));
		assertFalse(Files.exists(states.resolve(completedId + ".running")));
		assertTrue(Files.exists(states.resolve(completedId + ".completed")));
	}

	private static HttpTransportProtocol.Delivery delivery(int marker) {
		return new HttpTransportProtocol.Delivery(java.util.UUID.randomUUID().toString(),
				JsonEnvelope.builder("x").put("marker", marker).build());
	}

	@Test
	void proxyCallbacksAreSerializedInDeliveryOrder() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("ordered-proxy-server"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("ordered-authority"));
		CountDownLatch firstStarted = new CountDownLatch(1), releaseFirst = new CountDownLatch(1), secondStarted = new CountDownLatch(1);
		java.util.List<String> order = new java.util.concurrent.CopyOnWriteArrayList<>();
		try (HttpProxyTransportServer server = new HttpProxyTransportServer(new InetSocketAddress("localhost", 0), identity,
				authority, received -> {
					String marker = String.valueOf(received.envelope().getFields().get("marker"));
					order.add(marker);
					if ("first".equals(marker)) {
						firstStarted.countDown();
						try { releaseFirst.await(5, TimeUnit.SECONDS); }
						catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
					} else secondStarted.countDown();
				})) {
			server.start();
			HttpConnectionCode code = authority.createConnectionCode("lobby-1", server.endpoint("localhost"), Duration.ofMinutes(5));
			HttpClientCredentialStore.ClientCredential credential = HttpBackendTransportConnector.enroll(code, "lobby-1",
					directory.resolve("ordered-proxy-client"));
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(code, "lobby-1", credential,
					ignored -> { })) {
				connector.start();
				assertTrue(connector.send(JsonEnvelope.builder("x").put("marker", "first").build()));
				assertTrue(connector.send(JsonEnvelope.builder("x").put("marker", "second").build()));
				assertTrue(firstStarted.await(3, TimeUnit.SECONDS));
				assertFalse(secondStarted.await(150, TimeUnit.MILLISECONDS));
				releaseFirst.countDown();
				assertTrue(secondStarted.await(3, TimeUnit.SECONDS));
				assertEquals(java.util.List.of("first", "second"), order);
			}
		} finally { releaseFirst.countDown(); }
	}

	@Test
	void backendDedupWindowContinuesAfterCapacity() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.save(directory.resolve("client"), issued);
		HttpClientCredentialStore.HttpClientProfile profile = new HttpClientCredentialStore.HttpClientProfile("lobby-1",
				java.net.URI.create("https://localhost:8443/"), identity.serverCertificatePin(), identity.caCertificatePin());
		try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(profile,
				HttpClientCredentialStore.load(directory.resolve("client")), envelope -> { })) {
			for (int index = 0; index < HttpTransportProtocol.MAX_QUEUE; index++) {
				String id = java.util.UUID.randomUUID().toString();
				HttpTransportProtocol.Delivery delivery = new HttpTransportProtocol.Delivery(id, JsonEnvelope.builder("x").build());
				assertEquals(1, connector.accept(java.util.List.of(delivery)).size());
				connector.completeIncoming(id, true);
			}
			HttpTransportProtocol.Delivery next = new HttpTransportProtocol.Delivery(java.util.UUID.randomUUID().toString(),
					JsonEnvelope.builder("x").build());
			assertEquals(1, connector.accept(java.util.List.of(next)).size());
		}
	}
}
