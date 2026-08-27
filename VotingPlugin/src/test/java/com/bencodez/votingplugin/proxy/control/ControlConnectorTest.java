package com.bencodez.votingplugin.proxy.control;

import com.bencodez.votingplugin.proxy.control.ControlConnector.ObservedBackend;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Request;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Response;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Settings;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Status;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Transport;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ControlConnectorTest {
	private ScheduledExecutorService scheduler;
	private FakeTransport transport;
	private List<String> logs;
	private ControlConnector connector;

	@BeforeEach void setUp() {
		scheduler = Executors.newSingleThreadScheduledExecutor();
		transport = new FakeTransport();
		logs = new ArrayList<>();
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(new ObservedBackend("lobby", "Lobby", true, true, 3)), logs::add,
				UUID.fromString("00000000-0000-0000-0000-000000000001"), () -> 0L);
	}

	@AfterEach void tearDown() {
		connector.close();
		scheduler.shutdownNow();
	}

	@Test void registrationHeartbeatAndPresenceAreWireCompatibleAndReplacementSnapshotsAdvance() {
		connector.cycle();
		assertEquals(Status.CONNECTED, connector.status());
		assertEquals(2, transport.requests.size());
		Request registration = transport.requests.get(0);
		assertEquals("POST", registration.method());
		assertEquals("/api/v1/nodes/register", registration.path());
		JsonObject registrationJson = JsonParser.parseString(registration.body()).getAsJsonObject();
		assertEquals("proxy-a", registrationJson.get("nodeId").getAsString());
		assertEquals("VELOCITY", registrationJson.get("platform").getAsString());
		assertEquals(1, registrationJson.get("protocolVersion").getAsInt());
		assertEquals("presence.snapshot", registrationJson.getAsJsonArray("requiredCapabilities").get(0).getAsString());

		JsonObject firstSnapshot = JsonParser.parseString(transport.requests.get(1).body()).getAsJsonObject();
		assertEquals(0, firstSnapshot.get("sequence").getAsLong());
		assertEquals("lobby", firstSnapshot.getAsJsonArray("backends").get(0).getAsJsonObject()
				.get("backendId").getAsString());

		connector.cycle();
		assertEquals("/api/v1/nodes/proxy-a/heartbeat", transport.requests.get(2).path());
		JsonObject secondSnapshot = JsonParser.parseString(transport.requests.get(3).body()).getAsJsonObject();
		assertEquals(1, secondSnapshot.get("sequence").getAsLong());
	}

	@Test void unavailableAuthenticationProtocolAndMalformedResponsesOnlyChangeConnectorState() {
		transport.nextPrimary = new Response(401, "{\"error\":{}}");
		connector.cycle();
		assertEquals(Status.AUTHENTICATION_FAILED, connector.status());
		assertEquals(1, logs.size());

		transport.nextPrimary = new Response(409, "{\"error\":{}}");
		connector.cycle();
		assertEquals(Status.INCOMPATIBLE, connector.status());

		transport.nextPrimary = new Response(200, "not-json");
		connector.cycle();
		assertEquals(Status.UNAVAILABLE, connector.status());

		transport.nextPrimary = new Response(503, "{\"error\":{}}");
		connector.cycle();
		assertEquals(Status.UNAVAILABLE, connector.status());
	}

	@Test void controlRegistryLossCausesReregistrationWithoutAffectingOtherCode() {
		connector.cycle();
		transport.nextPrimary = new Response(404, "{\"error\":{}}");
		connector.cycle();
		assertEquals(Status.UNAVAILABLE, connector.status());
		connector.cycle();
		assertEquals("POST", transport.requests.get(3).method());
		assertEquals("/api/v1/nodes/register", transport.requests.get(3).path());
	}

	@Test void explicitHeartbeatCapabilityRevocationIsIncompatibleAndStopsTheCycle() {
		connector.cycle();
		transport.nextPrimary = new Response(200,
				"{\"node\":{\"acceptedCapabilities\":[\"config.proxy-routing.v1\"]}}");

		connector.cycle();

		assertEquals(Status.INCOMPATIBLE, connector.status());
		assertEquals(3, transport.requests.size());
	}

	@Test void skipsOperationClaimsWhenConfigurationCapabilityWasNotAccepted() {
		connector.close();
		ProxyRoutingConfigurationService service = new ProxyRoutingConfigurationService(new NoOpPlatform());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = false;

		connector.cycle();

		assertEquals(Status.CONNECTED, connector.status());
		assertEquals(2, transport.requests.size());
		assertTrue(transport.requests.stream().noneMatch(request -> request.path().endsWith("/operations")));
	}

	@Test void claimsOperationsWhenConfigurationCapabilityWasAccepted() {
		connector.close();
		ProxyRoutingConfigurationService service = new ProxyRoutingConfigurationService(new NoOpPlatform());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = true;

		connector.cycle();

		assertEquals(Status.CONNECTED, connector.status());
		assertEquals(3, transport.requests.size());
		assertTrue(transport.requests.get(2).path().endsWith("/operations"));

		connector.cycle();
		assertEquals(6, transport.requests.size());
		assertTrue(transport.requests.get(5).path().endsWith("/operations"));
	}

	@Test void slowTransportDoesNotBlockCallerAndShutdownCancelsInFlightRequest() {
		CompletableFuture<Response> stalled = new CompletableFuture<>();
		transport.stalled = stalled;
		connector.cycle();
		assertFalse(stalled.isDone());
		assertEquals(Status.STARTING, connector.status());
		connector.close();
		assertTrue(stalled.isCancelled());
		assertEquals(Status.STOPPED, connector.status());
	}

	@Test void synchronousTransportFailureClearsInFlightAndRetries() {
		transport.synchronousFailure = new IllegalArgumentException("invalid header");
		connector.cycle();
		assertEquals(Status.UNAVAILABLE, connector.status());
		connector.cycle();
		assertEquals(Status.CONNECTED, connector.status());
	}

	@Test void closeWaitsForClaimedApplyToFinish() throws Exception {
		connector.close();
		CountDownLatch persistEntered = new CountDownLatch(1);
		CountDownLatch releasePersist = new CountDownLatch(1);
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		ProxyRoutingConfigurationService service = new ProxyRoutingConfigurationService(new NoOpPlatform() {
			@Override public void persist(ProxyRoutingConfiguration proposal, String expectedRevision)
					throws java.io.IOException {
				persistEntered.countDown();
				try {
					releasePersist.await();
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
					throw new java.io.IOException(e);
				}
			}
		});
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = true;
		transport.operationClaim = new CompletableFuture<>();
		connector.cycle();
		try {
			CompletableFuture<Void> executing = CompletableFuture.runAsync(() -> transport.operationClaim.complete(
					new Response(200, "{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
							+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
							+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}")));
			assertTrue(persistEntered.await(2, TimeUnit.SECONDS));
			CompletableFuture<Void> closing = CompletableFuture.runAsync(connector::close);
			assertThrows(java.util.concurrent.TimeoutException.class, () -> closing.get(100, TimeUnit.MILLISECONDS));

			releasePersist.countDown();
			executing.get(2, TimeUnit.SECONDS);
			closing.get(2, TimeUnit.SECONDS);
			assertEquals(Status.STOPPED, connector.status());
		} finally {
			releasePersist.countDown();
		}
	}

	@Test void backoffIsBoundedExponentialAndJitteredWithoutSleeping() {
		assertEquals(1000, ControlConnector.backoffMillis(1, 0));
		assertEquals(2000, ControlConnector.backoffMillis(2, 0));
		assertTrue(ControlConnector.backoffMillis(3, 999) >= 4000);
		assertTrue(ControlConnector.backoffMillis(30, Long.MAX_VALUE) <= 300000);
		assertTrue(ControlConnector.backoffMillis(30, Long.MIN_VALUE) > 0);
	}

	@Test void backendIdentityPreservesSafeNamesAndDeterministicallyMapsOtherConfiguredNames() {
		assertEquals("lobby-1", ControlConnector.stableBackendId("lobby-1"));
		String mapped = ControlConnector.stableBackendId("Modded Lobby");
		assertTrue(mapped.matches("backend-[0-9a-f]{32}"));
		assertEquals(mapped, ControlConnector.stableBackendId("Modded Lobby"));
		assertNotEquals(mapped, ControlConnector.stableBackendId("Other Lobby"));
	}

	@Test void settingsRejectCredentialsInUrlsUnsafePathsAndUnboundedTiming() {
		assertThrows(IllegalArgumentException.class, () -> new Settings("proxy-a", "Proxy", "VELOCITY", "7.1.2",
				URI.create("http://user:secret@localhost:8080"), 30, 3000, 5000));
		assertThrows(IllegalArgumentException.class, () -> new Settings("proxy-a", "Proxy", "VELOCITY", "7.1.2",
				URI.create("http://localhost:8080/api"), 30, 3000, 5000));
		assertThrows(IllegalArgumentException.class, () -> new Settings("proxy-a", "Proxy", "VELOCITY", "7.1.2",
				URI.create("http://localhost:8080"), 1, 3000, 5000));
	}

	private Settings settings() {
		return new Settings("proxy-a", "Proxy A", "VELOCITY", "7.1.2",
				URI.create("http://127.0.0.1:8080"), 30, 3000, 5000);
	}

	private static final class FakeTransport implements Transport {
		private final List<Request> requests = new ArrayList<>();
		private Response nextPrimary;
		private CompletableFuture<Response> stalled;
		private RuntimeException synchronousFailure;
		private boolean acceptConfiguration;
		private CompletableFuture<Response> operationClaim;

		@Override
		public CompletableFuture<Response> send(Request request) {
			if (synchronousFailure != null) {
				RuntimeException failure = synchronousFailure;
				synchronousFailure = null;
				throw failure;
			}
			requests.add(request);
			if (stalled != null) {
				CompletableFuture<Response> result = stalled;
				stalled = null;
				return result;
			}
			if (request.path().endsWith("/operations")) {
				if (operationClaim != null) return operationClaim;
				return CompletableFuture.completedFuture(new Response(204, ""));
			}
			if (!request.path().endsWith("/presence")) {
				Response response = nextPrimary;
				nextPrimary = null;
				if (response != null) {
					return CompletableFuture.completedFuture(response);
				}
				if ("/api/v1/nodes/register".equals(request.path())) {
					String capabilities = acceptConfiguration
							? "[\"presence.snapshot\",\"config.proxy-routing.v1\"]" : "[\"presence.snapshot\"]";
					return CompletableFuture.completedFuture(new Response(201,
							"{\"identity\":{\"protocolVersion\":1},\"node\":{\"acceptedCapabilities\":"
									+ capabilities + "}}"));
				}
				return CompletableFuture.completedFuture(new Response(200, "{\"node\":{}}"));
			}
			return CompletableFuture.completedFuture(new Response(200, "{\"applied\":true,\"node\":{}}"));
		}
	}

	private static class NoOpPlatform implements ProxyRoutingConfigurationService.Platform {
		@Override public ProxyRoutingConfiguration read() { return new ProxyRoutingConfiguration(false, List.of()); }
		@Override public java.util.Set<String> configuredServers() { return java.util.Set.of(); }
		@Override public void persist(ProxyRoutingConfiguration proposal, String expectedRevision)
				throws java.io.IOException { }
		@Override public void rollback() { }
		@Override public void reload() { }
	}
}
