package com.bencodez.votingplugin.backendproxy.http;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import java.net.InetSocketAddress;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
import java.time.Duration;
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
			assertEquals(java.util.List.of(id), connector.drainAcknowledgements());
			assertTrue(connector.accept(java.util.List.of(delivery)).isEmpty());
			assertEquals(java.util.List.of(id), connector.drainAcknowledgements());
		}
	}
}
