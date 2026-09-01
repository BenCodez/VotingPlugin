package com.bencodez.votingplugin.proxy.control;

import com.bencodez.votingplugin.proxy.control.ControlConnector.ObservedBackend;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Request;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Response;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Settings;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Status;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Transport;
import com.bencodez.votingplugin.proxy.control.ProxyControlResultStore.StoredResult;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongSupplier;
import java.util.function.Supplier;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ControlConnectorTest {
	@TempDir Path dataDirectory;
	private ScheduledExecutorService scheduler;
	private FakeTransport transport;
	private List<String> logs;

	@Test void responseBudgetCanCarryTheLargestEscapedManagedFileTask() {
		assertTrue(ControlConnector.MAX_RESPONSE_BYTES >= ProxyConfigurationFileService.MAX_BYTES * 6);
	}
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

	@Test void fastOperationPollDoesNotSendAnotherHeartbeatOrPresenceSnapshot() {
		connector.close();
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;

		connector.cycle();
		int afterHeartbeat = transport.requests.size();
		connector.pollOperations();

		assertEquals(afterHeartbeat + 1, transport.requests.size());
		assertTrue(transport.requests.get(afterHeartbeat).path().endsWith("/operations"));
	}

	@Test void heartbeatCollidingWithOperationPollIsRearmed() throws Exception {
		connector.close();
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		connector.cycle();

		transport.operationClaim = new CompletableFuture<>();
		transport.heartbeatSent = new CountDownLatch(1);
		connector.pollOperations();
		connector.cycle();
		transport.operationClaim.complete(new Response(204, ""));

		assertTrue(transport.heartbeatSent.await(2, TimeUnit.SECONDS));
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
							+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
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

	@Test void replacementWaitsForTheOriginatingNodeToAcknowledgeItsResult() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = new CompletableFuture<>();
		connector.cycle();

		AtomicBoolean replacementStarted = new AtomicBoolean();
		assertTrue(connector.deferReplacementUntilSafe(() -> {
			assertFalse(connector.hasActiveOperation());
			replacementStarted.set(true);
		}));
		assertFalse(replacementStarted.get());

		transport.resultSubmission.complete(new Response(200, "{}"));
		assertTrue(replacementStarted.get());
	}

	@Test void onlySuccessfulProxyMethodApplyRequestsRuntimeReplacement() {
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		JsonObject configuration = new JsonObject();
		configuration.addProperty("preset", "proxy-method");
		result.add("configuration", configuration);
		result.addProperty("_controlOperationType", "READ");
		assertFalse(ControlConnector.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("_controlOperationType", "PREVIEW");
		assertFalse(ControlConnector.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("_controlOperationType", "APPLY");
		assertTrue(ControlConnector.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("success", false);
		assertFalse(ControlConnector.requiresRuntimeReplacement(new StoredResult(result, true, false)));
	}

	@Test void proxyFileCapabilityAdvertisesAndDispatchesMaskedReadResults() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\nProxy:\n  Enabled: true\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileService(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\"}}"));

		connector.cycle();

		JsonObject registration = JsonParser.parseString(transport.requests.get(0).body()).getAsJsonObject();
		assertTrue(registration.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "config.proxy-files.v1".equals(value.getAsString())));
		JsonObject result = submittedResult();
		JsonObject configuration = result.getAsJsonObject("configuration");
		assertEquals("file", configuration.get("domain").getAsString());
		assertEquals("bungeeconfig.yml", configuration.get("fileName").getAsString());
		assertTrue(configuration.get("content").getAsString().contains(ProxyConfigurationFileService.REDACTED));
		assertFalse(transport.requests.stream().map(Request::body).anyMatch(body -> body.contains("local-secret")));
	}

	@Test void proxyFilePreviewRejectsARevisionThatChangesAfterCalculation() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		ProxyConfigurationFileService actual = new ProxyConfigurationFileService(file,
				ControlConnectorTest::atomicMove);
		ProxyConfigurationFileService service = mock(ProxyConfigurationFileService.class);
		AtomicBoolean mutated = new AtomicBoolean();
		when(service.preview(ProxyConfigurationFileService.FILE_NAME, "Debug: true\n")).thenAnswer(invocation -> {
			ProxyConfigurationFileService.Preview preview = actual.preview(
					invocation.getArgument(0), invocation.getArgument(1));
			if (!mutated.compareAndSet(false, true)) return preview;
			try {
				Files.writeString(file, "Debug: changed-locally\n");
			} catch (java.io.IOException failure) {
				throw new AssertionError(failure);
			}
			return preview;
		});
		when(service.read(ProxyConfigurationFileService.FILE_NAME)).thenAnswer(invocation ->
				actual.read(invocation.getArgument(0)));
		connector.close();
		connector = fileConnector(service);
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"PREVIEW\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\",\"content\":\"Debug: true\\n\"}}"));

		connector.cycle();

		JsonObject result = submittedResult();
		assertTrue(mutated.get());
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("STALE_REVISION", result.get("code").getAsString());
		assertFalse(result.has("configuration"));
		assertEquals("Debug: changed-locally\n", Files.readString(file));
	}

	@Test void proxyFileTaskIsRejectedWhenOnlyRoutingControlWasNegotiated() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileService(file, (source, target) -> {
			throw new AssertionError("an unnegotiated proxy file task must not publish changes");
		}));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"ignored\","
						+ "\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\",\"content\":\"Debug: true\\n\"}}"));

		connector.cycle();

		JsonObject result = submittedResult();
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("UNSUPPORTED", result.get("code").getAsString());
		assertEquals("Debug: false\n", Files.readString(file));
	}

	@Test void proxyFileRejectsUnmanagedNamesWithAStructuredSafeFailure() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileService(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"../../private/local-secret.yml\"}}"));

		connector.cycle();

		JsonObject result = submittedResult();
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("VALIDATION_ERROR", result.get("code").getAsString());
		assertEquals("proxy configuration file is not managed", result.get("message").getAsString());
		assertFalse(result.toString().contains("local-secret"));
		assertFalse(result.toString().contains("../"));
	}

	@Test void proxyFileWriteAheadIntentPersistsOnlyRecoveryMetadataBeforeTheFileIsPublished() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\nProxy:\n  Enabled: true\n");
		CountDownLatch firstMove = new CountDownLatch(1);
		CountDownLatch releaseMove = new CountDownLatch(1);
		AtomicBoolean blockFirstMove = new AtomicBoolean(true);
		ProxyConfigurationFileService service = new ProxyConfigurationFileService(file, (source, target) -> {
			if (blockFirstMove.compareAndSet(true, false)) {
				firstMove.countDown();
				try {
					releaseMove.await();
				} catch (InterruptedException failure) {
					Thread.currentThread().interrupt();
					throw new java.io.IOException(failure);
				}
			}
			atomicMove(source, target);
		});
		String proposed = "Database:\n  Password: " + ProxyConfigurationFileService.REDACTED
				+ "\nProxy:\n  Enabled: false\n";
		String expectedRevision = ProxyConfigurationFileService.revision(Files.readString(file));
		String expectedProposedRevision = ProxyConfigurationFileService.revision(service.preview(
				ProxyConfigurationFileService.FILE_NAME, proposed).resolvedContent());
		connector.close();
		connector = fileConnector(service);
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + expectedRevision + "\","
						+ "\"configuration\":{\"domain\":\"file\",\"fileName\":\"bungeeconfig.yml\","
						+ "\"content\":\"Database:\\n  Password: " + ProxyConfigurationFileService.REDACTED
						+ "\\nProxy:\\n  Enabled: false\\n\"}}"));

		CompletableFuture<Void> cycle = CompletableFuture.runAsync(connector::cycle);
		assertTrue(firstMove.await(2, TimeUnit.SECONDS));
		try {
			String journal = Files.readString(dataDirectory.resolve(".control-proxy-pending-results.json"));
			StoredResult intent = ProxyControlResultStore.load(dataDirectory).results().values().iterator().next();
			JsonObject configuration = intent.result().getAsJsonObject("configuration");
			assertFalse(intent.committed());
			assertFalse(intent.result().get("reloaded").getAsBoolean());
			assertTrue(intent.result().get("message").getAsString().contains("restart the proxy"));
			assertEquals("file", configuration.get("domain").getAsString());
			assertEquals("bungeeconfig.yml", configuration.get("fileName").getAsString());
			assertEquals(expectedProposedRevision, intent.result().get("revision").getAsString());
			assertFalse(configuration.has("content"));
			assertFalse(journal.contains("local-secret"));
			assertFalse(journal.contains("Enabled: false"));
		} finally {
			releaseMove.countDown();
		}
		cycle.get(2, TimeUnit.SECONDS);
	}

	@Test void failedProxyIntentPublicationRestoresTheInMemoryWriteAheadState() throws Exception {
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileService(dataDirectory.resolve(
				ProxyConfigurationFileService.FILE_NAME), ControlConnectorTest::atomicMove));
		Path journal = dataDirectory.resolve(".control-proxy-pending-results.json");
		Path external = dataDirectory.resolve("external-journal.json");
		Files.writeString(external, "{}");
		try {
			Files.createSymbolicLink(journal, external.getFileName());
		} catch (UnsupportedOperationException unsupported) {
			return;
		}

		Method fileIntent = taskResultClass().getDeclaredMethod("fileIntent", String.class, String.class, List.class);
		fileIntent.setAccessible(true);
		Object intent = fileIntent.invoke(null, ProxyConfigurationFileService.FILE_NAME, "anticipated-revision",
				List.of("changed Proxy.Enabled"));
		Method persistIntent = ControlConnector.class.getDeclaredMethod("persistIntent", UUID.class,
				taskResultClass(), String.class);
		persistIntent.setAccessible(true);

		assertThrows(java.lang.reflect.InvocationTargetException.class, () -> persistIntent.invoke(connector,
				UUID.fromString("00000000-0000-0000-0000-000000000099"), intent,
				"00000000-0000-0000-0000-000000000199"));
		assertEquals(0, completedTaskCount());
	}

	@Test void lostResultResponseIsResubmittedBeforeAnotherOperationClaim() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = new CompletableFuture<>();
		connector.cycle();
		transport.resultSubmission.completeExceptionally(new java.io.IOException("response lost"));
		assertEquals(Status.UNAVAILABLE, connector.status());

		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		connector.cycle();

		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
	}

	@Test void largeProxyFileReadSurvivesLostAcknowledgementAndConnectorRestart() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "Large: '" + "a".repeat(300 * 1024) + "'\n");
		ProxyConfigurationFileService service = new ProxyConfigurationFileService(file,
				ControlConnectorTest::atomicMove);
		connector.close();
		connector = fileConnector(service);
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\"}}"));
		transport.resultSubmission = new CompletableFuture<>();

		connector.cycle();
		transport.resultSubmission.completeExceptionally(new java.io.IOException("acknowledgement lost"));
		assertEquals(Status.UNAVAILABLE, connector.status());
		assertTrue(Files.size(dataDirectory.resolve(".control-proxy-pending-results.json")) > 256 * 1024);

		connector.close();
		connector = fileConnector(service);
		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		connector.cycle();

		assertFalse(Files.exists(dataDirectory.resolve(".control-proxy-pending-results.json")));
		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
	}

	@Test void expiredResultLeaseIsReclaimedAndReboundBeforeResubmission() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(409,
				"{\"error\":{\"code\":\"TASK_LEASE_EXPIRED\"}}"));

		connector.cycle();

		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000299\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		connector.cycle();

		List<Request> results = transport.requests.stream().filter(request -> request.path().endsWith("/result")).toList();
		assertEquals(2, results.size());
		assertEquals("00000000-0000-0000-0000-000000000199",
				JsonParser.parseString(results.get(0).body()).getAsJsonObject().get("attemptId").getAsString());
		assertEquals("00000000-0000-0000-0000-000000000299",
				JsonParser.parseString(results.get(1).body()).getAsJsonObject().get("attemptId").getAsString());
	}

	@Test void forgottenControlOperationDoesNotBlockFutureClaims() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":"
						+ "{\"sendVotesToAllServers\":false,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}"));

		connector.cycle();
		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		connector.cycle();

		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
	}

	@Test void abandonedWriteAheadIntentIsReportedAndReleasedWhenControlForgotTheOperation() {
		connector.close();
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		ProxyRoutingConfiguration proposal = new ProxyRoutingConfiguration(true, List.of());
		JsonObject anticipated = new JsonObject();
		anticipated.addProperty("success", true);
		anticipated.addProperty("code", "OK");
		anticipated.addProperty("message", "Configuration applied");
		anticipated.addProperty("revision", proposal.revision());
		JsonObject configuration = new JsonObject();
		configuration.addProperty("sendVotesToAllServers", true);
		configuration.add("blockedServers", new com.google.gson.JsonArray());
		anticipated.add("configuration", configuration);
		anticipated.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				new ProxyRoutingConfigurationService(new NoOpPlatform()),
				Map.of(operationId, new StoredResult(anticipated, false, false)));
		transport.acceptConfiguration = true;
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}"));

		connector.cycle();
		JsonObject submitted = JsonParser.parseString(transport.requests.stream()
				.filter(request -> request.path().endsWith("/result")).findFirst().orElseThrow().body()).getAsJsonObject();
		assertEquals("RECOVERY_ABORTED", submitted.get("code").getAsString());

		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		connector.cycle();
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		assertTrue(connector.reserveRuntimeReplacement());
	}

	@Test void closeBeforeOperationPublicationPreventsTheRequestChainFromStarting() throws Exception {
		connector.close();
		transport.firstSendEntered = new CountDownLatch(1);
		transport.releaseFirstSend = new CountDownLatch(1);
		connector = new ControlConnector(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L);
		CompletableFuture<Void> cycling = CompletableFuture.runAsync(connector::cycle);
		assertTrue(transport.firstSendEntered.await(2, TimeUnit.SECONDS));

		try {
			connector.close();
		} finally {
			transport.releaseFirstSend.countDown();
		}
		cycling.get(2, TimeUnit.SECONDS);
		assertEquals(1, transport.requests.size());
		assertEquals(Status.STOPPED, connector.status());
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

	private JsonObject submittedResult() {
		return JsonParser.parseString(transport.requests.stream().filter(request -> request.path().endsWith("/result"))
				.findFirst().orElseThrow().body()).getAsJsonObject();
	}

	private static void atomicMove(Path source, Path target) throws java.io.IOException {
		Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
	}

	private static Class<?> taskResultClass() {
		return java.util.Arrays.stream(ControlConnector.class.getDeclaredClasses())
				.filter(type -> type.getSimpleName().equals("TaskResult")).findFirst().orElseThrow();
	}

	@SuppressWarnings("unchecked")
	private int completedTaskCount() throws Exception {
		Field completed = ControlConnector.class.getDeclaredField("completedTasks");
		completed.setAccessible(true);
		return ((Map<UUID, StoredResult>) completed.get(connector)).size();
	}

	@SuppressWarnings("unchecked")
	private ControlConnector fileConnector(ProxyConfigurationFileService fileService) throws Exception {
		Constructor<ControlConnector> constructor = ControlConnector.class.getDeclaredConstructor(Settings.class,
				ScheduledExecutorService.class, Transport.class, Supplier.class, Consumer.class, UUID.class,
				LongSupplier.class, ProxyRoutingConfigurationService.class, Path.class, ProxyControlResultStore.Route.class,
				boolean.class, Runnable.class, Function.class, ProxyMethodConfigurationService.class, Runnable.class,
				ProxyConfigurationFileService.class);
		constructor.setAccessible(true);
		ControlConnector created = constructor.newInstance(settings(), scheduler, transport,
				(Supplier<List<ObservedBackend>>) List::of,
				(Consumer<String>) logs::add, UUID.randomUUID(), (LongSupplier) () -> 0L, null, dataDirectory,
				new ProxyControlResultStore.Route("proxy-a", "Proxy A", "VELOCITY", "7.1.2",
						URI.create("http://127.0.0.1:8080"), "credential.txt", 30, 3000, 5000),
				false, null,
				(Function<String, CompletableFuture<com.bencodez.votingplugin.proxy.VotingPluginProxy.CommunicationTestResult>>) null,
				null, null, fileService);
		ProxyControlResultStore.State recovered = ProxyControlResultStore.load(dataDirectory);
		if (recovered != null) {
			Field completed = ControlConnector.class.getDeclaredField("completedTasks");
			completed.setAccessible(true);
			((Map<UUID, StoredResult>) completed.get(created)).putAll(recovered.results());
		}
		return created;
	}

	private static final class FakeTransport implements Transport {
		private final List<Request> requests = new ArrayList<>();
		private Response nextPrimary;
		private CompletableFuture<Response> stalled;
		private RuntimeException synchronousFailure;
		private boolean acceptConfiguration;
		private boolean acceptProxyFiles;
		private CompletableFuture<Response> operationClaim;
		private CompletableFuture<Response> resultSubmission;
		private CountDownLatch firstSendEntered;
		private CountDownLatch releaseFirstSend;
		private CountDownLatch heartbeatSent;

		@Override
		public CompletableFuture<Response> send(Request request) {
			if (synchronousFailure != null) {
				RuntimeException failure = synchronousFailure;
				synchronousFailure = null;
				throw failure;
			}
			requests.add(request);
			if (heartbeatSent != null && request.path().endsWith("/heartbeat")) heartbeatSent.countDown();
			if (firstSendEntered != null) {
				firstSendEntered.countDown();
				try {
					releaseFirstSend.await();
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
					throw new IllegalStateException(e);
				} finally {
					firstSendEntered = null;
				}
			}
			if (stalled != null) {
				CompletableFuture<Response> result = stalled;
				stalled = null;
				return result;
			}
			if (request.path().endsWith("/result") && resultSubmission != null) return resultSubmission;
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
							? "[\"presence.snapshot\",\"config.proxy-routing.v1\"]"
							: acceptProxyFiles ? "[\"presence.snapshot\",\"config.proxy-files.v1\"]"
							: "[\"presence.snapshot\"]";
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
