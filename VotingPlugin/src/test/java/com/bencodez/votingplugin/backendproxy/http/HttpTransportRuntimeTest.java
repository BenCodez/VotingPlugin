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
			HttpClientCredentialStore.ClientCredential credential = HttpBackendTransportConnector.enroll(code, "lobby-1", directory.resolve("client"));
			try (HttpBackendTransportConnector connector = new HttpBackendTransportConnector(code, "lobby-1", credential,
					envelope -> backendReceived.countDown())) {
				connector.start();
				assertTrue(connector.send(JsonEnvelope.builder("to-proxy").put("server", "forged").build()));
				assertTrue(proxyReceived.await(8, TimeUnit.SECONDS));
				assertEquals("lobby-1", received.get().serverId());
				assertEquals("lobby-1", received.get().envelope().getFields().get("server"));
				long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
				while (connector.queuedOutgoing() != 0 && System.nanoTime() < deadline) Thread.sleep(10);
				assertEquals(0, connector.queuedOutgoing(), "proxy ACK must remove the exact outbound delivery ID");
				assertTrue(server.send("lobby-1", JsonEnvelope.builder("to-backend").build()));
				assertTrue(backendReceived.await(8, TimeUnit.SECONDS));
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
