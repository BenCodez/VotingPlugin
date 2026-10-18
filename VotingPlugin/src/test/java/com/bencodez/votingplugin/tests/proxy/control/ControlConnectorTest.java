package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.proxy.control.ControlConnector;
import com.bencodez.votingplugin.tests.proxy.control.ProxyConfigurationFileServiceAccess;
import com.bencodez.votingplugin.tests.proxy.control.ProxyControlResultStoreAccess;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfigurationService;
import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfiguration;
import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService;

import com.bencodez.votingplugin.control.PluginDeploymentService;
import com.bencodez.votingplugin.proxy.control.ControlConnector.ObservedBackend;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Request;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Response;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Settings;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Status;
import com.bencodez.votingplugin.proxy.control.ControlConnector.Transport;
import com.bencodez.votingplugin.tests.proxy.control.ProxyControlResultStoreAccess.StoredResult;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.http.HttpClient;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
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
		assertTrue(ControlConnectorAccess.MAX_RESPONSE_BYTES >= ProxyConfigurationFileServiceAccess.MAX_BYTES * 6);
	}

	@Test void deploymentTaskRejectsUnknownV1Fields() {
		JsonObject task = new JsonObject();
		task.addProperty("deploymentId", "00000000-0000-0000-0000-000000000001");
		task.addProperty("artifactId", "VotingPlugin.jar");
		task.addProperty("sha256", "a".repeat(64));
		task.addProperty("size", "1");
		task.addProperty("attemptId", "00000000-0000-0000-0000-000000000002");
		task.addProperty("unexpected", "value");
		assertThrows(RuntimeException.class, () -> ControlConnectorAccess.deploymentTask(task));
	}
	private ControlConnector connector;

	@BeforeEach void setUp() {
		scheduler = Executors.newSingleThreadScheduledExecutor();
		transport = new FakeTransport();
		logs = new ArrayList<>();
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(new ObservedBackend("lobby", "Lobby", true, true, 3)), logs::add,
				UUID.fromString("00000000-0000-0000-0000-000000000001"), () -> 0L);
	}

	@AfterEach void tearDown() {
		connector.close();
		scheduler.shutdownNow();
	}

	@Test void registrationHeartbeatAndPresenceAreWireCompatibleAndReplacementSnapshotsAdvance() {
		ControlConnectorAccess.cycle(connector);
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

		ControlConnectorAccess.cycle(connector);
		assertEquals("/api/v1/nodes/proxy-a/heartbeat", transport.requests.get(2).path());
		JsonObject secondSnapshot = JsonParser.parseString(transport.requests.get(3).body()).getAsJsonObject();
		assertEquals(1, secondSnapshot.get("sequence").getAsLong());
	}

	@Test void bungeeDeploymentSettingDefaultsOffAndReadsExplicitTrue() {
		com.bencodez.votingplugin.proxy.bungee.BungeeConfig config = mock(
				com.bencodez.votingplugin.proxy.bungee.BungeeConfig.class, CALLS_REAL_METHODS);
		net.md_5.bungee.config.Configuration data = new net.md_5.bungee.config.Configuration();
		when(config.getData()).thenReturn(data);
		assertFalse(config.getControlAllowInsecureHttpPluginDeployment());
		data.set("Control.AllowInsecureHttpPluginDeployment", true);
		assertTrue(config.getControlAllowInsecureHttpPluginDeployment());
	}

	@Test void privateHttpWithoutOptInDoesNotPrepareOrAdvertiseProxyDeployment() throws Exception {
		com.bencodez.votingplugin.proxy.VotingPluginProxy proxy = mock(com.bencodez.votingplugin.proxy.VotingPluginProxy.class);
		com.bencodez.votingplugin.proxy.VotingPluginProxyConfig config = mock(com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.class, CALLS_REAL_METHODS);
		when(proxy.getDataFolderPlugin()).thenReturn(dataDirectory.toFile());
		when(proxy.getConfig()).thenReturn(config);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(proxy.getProxyPlatform()).thenReturn("VELOCITY");
		when(proxy.getPluginVersion()).thenReturn("7.1.2");
		when(config.getControlEnabled()).thenReturn(true);
		when(config.getControlNodeId()).thenReturn("proxy-a");
		when(config.getControlEndpoint()).thenReturn("http://192.168.0.50:2150");
		when(config.getControlCredentialFile()).thenReturn("test-credential.txt");
		Files.writeString(dataDirectory.resolve("test-credential.txt"), "test-credential");
		try (ControlConnector privateHttp = ControlConnector.create(proxy)) {
			Field field = ControlConnector.class.getDeclaredField("deployments");
			field.setAccessible(true);
			assertNull(field.get(privateHttp));
			JsonObject body = new JsonObject();
			ControlConnectorAccess.addCapabilities(body, true, true, true, true, field.get(privateHttp) != null);
			assertFalse(body.getAsJsonArray("capabilities").asList().stream()
					.anyMatch(value -> PluginDeploymentService.CAPABILITY.equals(value.getAsString())));
			verify(proxy).log(contains("Plugin deployment over private-network HTTP is disabled"));
			verify(proxy, never()).log(contains("staging is unavailable"));
			Field executor = ControlConnector.class.getDeclaredField("deploymentExecutor");
			executor.setAccessible(true);
			assertNull(executor.get(privateHttp), "disabled deployment never allocates its worker");
		}
	}

	@Test void deploymentCapabilityIsAdvertisedOnlyWhenProxyStagingIsReady() {
		JsonObject unavailable = new JsonObject();
		ControlConnectorAccess.addCapabilities(unavailable, true, true, true, true, false);
		assertFalse(unavailable.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "plugin.deploy.v1".equals(value.getAsString())));

		JsonObject ready = new JsonObject();
		ControlConnectorAccess.addCapabilities(ready, true, true, true, true, true);
		assertTrue(ready.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "plugin.deploy.v1".equals(value.getAsString())));
	}

	@Test void unavailableAuthenticationProtocolAndMalformedResponsesOnlyChangeConnectorState() {
		transport.nextPrimary = new Response(401, "{\"error\":{}}");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.AUTHENTICATION_FAILED, connector.status());
		assertEquals(1, logs.size());

		transport.nextPrimary = new Response(409, "{\"error\":{}}");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.INCOMPATIBLE, connector.status());

		transport.nextPrimary = new Response(200, "not-json");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.UNAVAILABLE, connector.status());

		transport.nextPrimary = new Response(503, "{\"error\":{}}");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.UNAVAILABLE, connector.status());
	}

	@Test void controlRegistryLossCausesReregistrationWithoutAffectingOtherCode() {
		ControlConnectorAccess.cycle(connector);
		transport.nextPrimary = new Response(404, "{\"error\":{}}");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.UNAVAILABLE, connector.status());
		ControlConnectorAccess.cycle(connector);
		assertEquals("POST", transport.requests.get(3).method());
		assertEquals("/api/v1/nodes/register", transport.requests.get(3).path());
	}

	@Test void explicitHeartbeatCapabilityRevocationIsIncompatibleAndStopsTheCycle() {
		ControlConnectorAccess.cycle(connector);
		transport.nextPrimary = new Response(200,
				"{\"node\":{\"acceptedCapabilities\":[\"config.proxy-routing.v1\"]}}");

		ControlConnectorAccess.cycle(connector);

		assertEquals(Status.INCOMPATIBLE, connector.status());
		assertEquals(3, transport.requests.size());
	}

	@Test void skipsOperationClaimsWhenConfigurationCapabilityWasNotAccepted() {
		connector.close();
		ProxyRoutingConfigurationService service = ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform());
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = false;

		ControlConnectorAccess.cycle(connector);

		assertEquals(Status.CONNECTED, connector.status());
		assertEquals(2, transport.requests.size());
		assertTrue(transport.requests.stream().noneMatch(request -> request.path().endsWith("/operations")));
	}

	@Test void claimsOperationsWhenConfigurationCapabilityWasAccepted() {
		connector.close();
		ProxyRoutingConfigurationService service = ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform());
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = true;

		ControlConnectorAccess.cycle(connector);

		assertEquals(Status.CONNECTED, connector.status());
		assertEquals(3, transport.requests.size());
		assertTrue(transport.requests.get(2).path().endsWith("/operations"));

		ControlConnectorAccess.cycle(connector);
		assertEquals(6, transport.requests.size());
		assertTrue(transport.requests.get(5).path().endsWith("/operations"));
	}

	@Test void fastOperationPollDoesNotSendAnotherHeartbeatOrPresenceSnapshot() {
		connector.close();
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;

		ControlConnectorAccess.cycle(connector);
		int afterHeartbeat = transport.requests.size();
		ControlConnectorAccess.pollOperations(connector);

		assertEquals(afterHeartbeat + 1, transport.requests.size());
		assertTrue(transport.requests.get(afterHeartbeat).path().endsWith("/operations"));
	}

	@Test void heartbeatCollidingWithOperationPollIsRearmed() throws Exception {
		connector.close();
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		ControlConnectorAccess.cycle(connector);

		transport.operationClaim = new CompletableFuture<>();
		transport.heartbeatSent = new CountDownLatch(1);
		ControlConnectorAccess.pollOperations(connector);
		ControlConnectorAccess.cycle(connector);
		transport.operationClaim.complete(new Response(204, ""));

		assertTrue(transport.heartbeatSent.await(2, TimeUnit.SECONDS));
	}

	@Test void slowTransportDoesNotBlockCallerAndShutdownCancelsInFlightRequest() {
		CompletableFuture<Response> stalled = new CompletableFuture<>();
		transport.stalled = stalled;
		ControlConnectorAccess.cycle(connector);
		assertFalse(stalled.isDone());
		assertEquals(Status.STARTING, connector.status());
		connector.close();
		assertTrue(stalled.isCancelled());
		assertEquals(Status.STOPPED, connector.status());
	}

	@Test void synchronousTransportFailureClearsInFlightAndRetries() {
		transport.synchronousFailure = new IllegalArgumentException("invalid header");
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.UNAVAILABLE, connector.status());
		ControlConnectorAccess.cycle(connector);
		assertEquals(Status.CONNECTED, connector.status());
	}

	@Test void closeWaitsForClaimedApplyToFinish() throws Exception {
		connector.close();
		CountDownLatch persistEntered = new CountDownLatch(1);
		CountDownLatch releasePersist = new CountDownLatch(1);
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		ProxyRoutingConfigurationService service = ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform() {
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
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L, service);
		transport.acceptConfiguration = true;
		transport.operationClaim = new CompletableFuture<>();
		ControlConnectorAccess.cycle(connector);
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
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = new CompletableFuture<>();
		ControlConnectorAccess.cycle(connector);

		AtomicBoolean replacementStarted = new AtomicBoolean();
		assertTrue(connector.deferReplacementUntilSafe(() -> {
			assertFalse(ControlConnectorAccess.hasActiveOperation(connector));
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
		assertFalse(ControlConnectorAccess.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("_controlOperationType", "PREVIEW");
		assertFalse(ControlConnectorAccess.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("_controlOperationType", "APPLY");
		assertTrue(ControlConnectorAccess.requiresRuntimeReplacement(new StoredResult(result, true, false)));
		result.addProperty("success", false);
		assertFalse(ControlConnectorAccess.requiresRuntimeReplacement(new StoredResult(result, true, false)));
	}

	@Test void lowercaseHttpProxyMethodRequiresV2ForEveryOperationPhase() throws Exception {
		Field accepted = ControlConnector.class.getDeclaredField("acceptedCapabilities");
		accepted.setAccessible(true);
		Method execute = ControlConnector.class.getDeclaredMethod("executeTask", UUID.class, JsonObject.class);
		execute.setAccessible(true);
		for (String method : List.of("http", " HTTP ")) {
			for (String type : List.of("READ", "PREVIEW", "APPLY")) {
				JsonObject task = JsonParser.parseString("{\"configuration\":{\"domain\":\"quick-setup\","
						+ "\"preset\":\"proxy-method\",\"options\":{\"method\":\"" + method + "\"}}}")
						.getAsJsonObject();
				task.addProperty("type", type);
				ReflectiveAccess.set(accepted, connector, Set.of("config.proxy-method.v1"));
				Object rejected = ((CompletableFuture<?>) ReflectiveAccess.invoke(execute, connector, new Object[]{UUID.randomUUID(), task})).join();
				Method json = rejected.getClass().getDeclaredMethod("json");
				json.setAccessible(true);
				assertTrue(((JsonObject) ReflectiveAccess.invoke(json, rejected, new Object[]{})).get("message").getAsString()
						.contains("not negotiated"));
				ReflectiveAccess.set(accepted, connector, Set.of("config.proxy-method.v2"));
				Object acceptedResult = ((CompletableFuture<?>) ReflectiveAccess.invoke(execute, connector, new Object[]{UUID.randomUUID(), task})).join();
				assertTrue(((JsonObject) ReflectiveAccess.invoke(json, acceptedResult, new Object[]{})).get("message").getAsString()
						.contains("unavailable"));
			}
		}
	}

	@Test void proxyFileCapabilityAdvertisesAndDispatchesMaskedReadResults() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\nProxy:\n  Enabled: true\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\"}}"));

		ControlConnectorAccess.cycle(connector);

		JsonObject registration = JsonParser.parseString(transport.requests.get(0).body()).getAsJsonObject();
		assertTrue(registration.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "config.proxy-files.v1".equals(value.getAsString())));
		JsonObject result = submittedResult();
		JsonObject configuration = result.getAsJsonObject("configuration");
		assertEquals("file", configuration.get("domain").getAsString());
		assertEquals("bungeeconfig.yml", configuration.get("fileName").getAsString());
		assertTrue(configuration.get("content").getAsString().contains(ProxyConfigurationFileServiceAccess.REDACTED));
		assertFalse(transport.requests.stream().map(Request::body).anyMatch(body -> body.contains("local-secret")));
	}

	@Test void recoveredCapabilityFilteredResultCompletesRecoveryAfterAnEmptyClaim() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		AtomicInteger recoveryCalls = new AtomicInteger();
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove), true,
				recoveryCalls::incrementAndGet);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", ProxyConfigurationFileServiceAccess.FILE_NAME);
		result.add("configuration", configuration);
		Field completed = ControlConnector.class.getDeclaredField("completedTasks");
		completed.setAccessible(true);
		@SuppressWarnings("unchecked") Map<UUID, Object> results =
				(Map<UUID, Object>) completed.get(connector);
		results.put(UUID.fromString("00000000-0000-0000-0000-000000000099"),
				ReflectiveAccess.unwrap(new StoredResult(result, true, false)));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));

		ControlConnectorAccess.cycle(connector);

		assertEquals(1, recoveryCalls.get(), "a filtered recovered result must not pin the old connector route");
		ControlConnectorAccess.cycle(connector);
		assertEquals(1, recoveryCalls.get(), "recovery completion must be idempotent");
	}

	@Test void recoveredCapabilityFilteredResultCompletesWithoutAnOperationLane() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		AtomicInteger recoveryCalls = new AtomicInteger();
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove), true,
				recoveryCalls::incrementAndGet);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", ProxyConfigurationFileServiceAccess.FILE_NAME);
		result.add("configuration", configuration);
		Field completed = ControlConnector.class.getDeclaredField("completedTasks");
		completed.setAccessible(true);
		@SuppressWarnings("unchecked") Map<UUID, Object> results =
				(Map<UUID, Object>) completed.get(connector);
		results.put(UUID.fromString("00000000-0000-0000-0000-000000000099"),
				ReflectiveAccess.unwrap(new StoredResult(result, true, false)));
		// The server can keep this node registered for presence while refusing
		// every operation capability, leaving no operation claim to return 204.
		transport.acceptConfiguration = false;
		transport.acceptProxyFiles = false;

		ControlConnectorAccess.cycle(connector);

		assertEquals(1, recoveryCalls.get(),
				"a filtered recovered result must complete after the successful no-operation handshake");
		assertTrue(transport.requests.stream().noneMatch(request -> request.path().endsWith("/operations")));
		ProxyControlResultStoreAccess.State retained = ProxyControlResultStoreAccess.load(dataDirectory);
		assertNotNull(retained);
		assertFalse(retained.routeRequired(), "the retained result must not pin the next connector to the old route");
		ControlConnectorAccess.cycle(connector);
		assertEquals(1, recoveryCalls.get(), "recovery completion must be idempotent");
	}

	@Test void proxyFilesAcceptCanonicalControlUnionWithoutWeakeningForeignFieldValidation() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		for (String type : List.of("READ", "PREVIEW")) {
			String content = "READ".equals(type) ? "null" : "\"Debug: true\\n\"";
			transport.requests.clear();
			transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
					"{\"operationId\":\"" + UUID.randomUUID() + "\",\"attemptId\":\"" + UUID.randomUUID()
					+ "\",\"type\":\"" + type + "\",\"configuration\":{\"domain\":\"file\","
					+ "\"sendVotesToAllServers\":null,\"blockedServers\":[],\"fileName\":\"bungeeconfig.yml\","
					+ "\"content\":" + content + ",\"preset\":null,\"options\":{}}}"));
			ControlConnectorAccess.cycle(connector);
			assertTrue(submittedResult().get("success").getAsBoolean(), type);
		}
		for (String foreign : List.of("\"sendVotesToAllServers\":true", "\"blockedServers\":[\"lobby\"]",
				"\"preset\":\"standalone\"", "\"options\":{\"enabled\":\"true\"}", "\"unknown\":null")) {
			transport.requests.clear();
			transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
					"{\"operationId\":\"" + UUID.randomUUID() + "\",\"attemptId\":\"" + UUID.randomUUID()
					+ "\",\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
					+ "\"fileName\":\"bungeeconfig.yml\"," + foreign + "}}"));
			ControlConnectorAccess.cycle(connector);
			assertEquals("VALIDATION_ERROR", submittedResult().get("code").getAsString(), foreign);
		}
		assertEquals("Debug: false\n", Files.readString(file), "read/preview must not write");
	}

	@Test void malformedProxyFileFieldsBecomeDurableValidationFailures() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		String[] malformedTasks = {
				"{\"domain\":\"file\",\"fileName\":\"bungeeconfig.yml\"}",
				"{\"domain\":\"file\",\"fileName\":{},\"content\":\"Debug: true\\n\"}",
				"{\"domain\":\"file\",\"fileName\":\"bungeeconfig.yml\","
						+ "\"content\":\"Debug: true\\n\",\"reload\":true}"
		};
		for (int index = 0; index < malformedTasks.length; index++) {
			String operationId = String.format("00000000-0000-0000-0000-%012d", 99 + index);
			String attemptId = String.format("00000000-0000-0000-0000-%012d", 199 + index);
			String claim = "{\"operationId\":\"" + operationId + "\","
					+ "\"attemptId\":\"" + attemptId + "\","
					+ "\"type\":\"PREVIEW\",\"configuration\":" + malformedTasks[index] + "}";
			transport.operationClaim = CompletableFuture.completedFuture(new Response(200, claim));
			ControlConnectorAccess.cycle(connector);
			JsonObject result = submittedResult();
			assertFalse(result.get("success").getAsBoolean());
			assertEquals("VALIDATION_ERROR", result.get("code").getAsString());
			assertEquals(Status.CONNECTED, connector.status());
		}
	}

	@Test void proxyFilePreviewRejectsARevisionThatChangesAfterCalculation() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		ProxyConfigurationFileServiceAccess actual = new ProxyConfigurationFileServiceAccess(file,
				ControlConnectorTest::atomicMove);
		ProxyConfigurationFileServiceAccess service = ProxyConfigurationFileServiceAccess.mock();
		AtomicBoolean mutated = new AtomicBoolean();
		when(service.preview(ProxyConfigurationFileServiceAccess.FILE_NAME, "Debug: true\n")).thenAnswer(invocation -> {
			ProxyConfigurationFileServiceAccess.Preview preview = actual.preview(
					invocation.getArgument(0), invocation.getArgument(1));
			if (!mutated.compareAndSet(false, true)) return ReflectiveAccess.unwrap(preview);
			try {
				Files.writeString(file, "Debug: changed-locally\n");
			} catch (java.io.IOException failure) {
				throw new AssertionError(failure);
			}
			return ReflectiveAccess.unwrap(preview);
		});
		when(service.read(ProxyConfigurationFileServiceAccess.FILE_NAME)).thenAnswer(invocation ->
				ReflectiveAccess.unwrap(actual.read(invocation.getArgument(0))));
		connector.close();
		connector = fileConnector(service);
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"PREVIEW\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\",\"content\":\"Debug: true\\n\"}}"));

		ControlConnectorAccess.cycle(connector);

		JsonObject result = submittedResult();
		assertTrue(mutated.get());
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("STALE_REVISION", result.get("code").getAsString());
		assertFalse(result.has("configuration"));
		assertEquals("Debug: changed-locally\n", Files.readString(file));
	}

	@Test void proxyFileTaskIsRejectedWhenOnlyRoutingControlWasNegotiated() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Debug: false\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, (source, target) -> {
			throw new AssertionError("an unnegotiated proxy file task must not publish changes");
		}));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"ignored\","
						+ "\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\",\"content\":\"Debug: true\\n\"}}"));

		ControlConnectorAccess.cycle(connector);

		JsonObject result = submittedResult();
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("UNSUPPORTED", result.get("code").getAsString());
		assertEquals("Debug: false\n", Files.readString(file));
	}

	@Test void proxyFileRejectsUnmanagedNamesWithAStructuredSafeFailure() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove));
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"../../private/local-secret.yml\"}}"));

		ControlConnectorAccess.cycle(connector);

		JsonObject result = submittedResult();
		assertFalse(result.get("success").getAsBoolean());
		assertEquals("VALIDATION_ERROR", result.get("code").getAsString());
		assertEquals("proxy configuration file is not managed", result.get("message").getAsString());
		assertFalse(result.toString().contains("local-secret"));
		assertFalse(result.toString().contains("../"));
	}

	@Test void proxyFileWriteAheadIntentPersistsOnlyRecoveryMetadataBeforeTheFileIsPublished() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\nProxy:\n  Enabled: true\n");
		CountDownLatch firstMove = new CountDownLatch(1);
		CountDownLatch releaseMove = new CountDownLatch(1);
		AtomicBoolean blockFirstMove = new AtomicBoolean(true);
		ProxyConfigurationFileServiceAccess service = new ProxyConfigurationFileServiceAccess(file, (source, target) -> {
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
		String proposed = "Database:\n  Password: " + ProxyConfigurationFileServiceAccess.REDACTED
				+ "\nProxy:\n  Enabled: false\n";
		String expectedRevision = ProxyConfigurationFileServiceAccess.revision(Files.readString(file));
		String expectedProposedRevision = ProxyConfigurationFileServiceAccess.revision(service.preview(
				ProxyConfigurationFileServiceAccess.FILE_NAME, proposed).resolvedContent());
		connector.close();
		connector = fileConnector(service);
		transport.acceptProxyFiles = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + expectedRevision + "\","
						+ "\"configuration\":{\"domain\":\"file\",\"fileName\":\"bungeeconfig.yml\","
						+ "\"content\":\"Database:\\n  Password: " + ProxyConfigurationFileServiceAccess.REDACTED
						+ "\\nProxy:\\n  Enabled: false\\n\"}}"));

		CompletableFuture<Void> cycle = CompletableFuture.runAsync(() -> ControlConnectorAccess.cycle(connector));
		assertTrue(firstMove.await(2, TimeUnit.SECONDS));
		try {
			String journal = Files.readString(dataDirectory.resolve(".control-proxy-pending-results.json"));
			StoredResult intent = ProxyControlResultStoreAccess.load(dataDirectory).results().values().iterator().next();
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
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(dataDirectory.resolve(
				ProxyConfigurationFileServiceAccess.FILE_NAME), ControlConnectorTest::atomicMove));
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
		Object intent = ReflectiveAccess.invoke(fileIntent, null, new Object[]{ProxyConfigurationFileServiceAccess.FILE_NAME, "anticipated-revision", List.of("changed Proxy.Enabled")});
		Method persistIntent = ControlConnector.class.getDeclaredMethod("persistIntent", UUID.class,
				taskResultClass(), String.class);
		persistIntent.setAccessible(true);

		assertThrows(java.lang.reflect.InvocationTargetException.class, () -> ReflectiveAccess.invoke(persistIntent, connector, new Object[]{UUID.fromString("00000000-0000-0000-0000-000000000099"), intent, "00000000-0000-0000-0000-000000000199"}));
		assertEquals(0, completedTaskCount());
	}

	@Test void proxyFileTaskResultsBoundChangeDescriptionsBeforeJournaling() throws Exception {
		Method fileIntent = taskResultClass().getDeclaredMethod("fileIntent", String.class, String.class, List.class);
		fileIntent.setAccessible(true);
		Object result = ReflectiveAccess.invoke(fileIntent, null, new Object[]{ProxyConfigurationFileServiceAccess.FILE_NAME, "anticipated-revision", List.of("changed " + "x".repeat(1000))});
		Method changes = taskResultClass().getDeclaredMethod("changes");
		changes.setAccessible(true);

		@SuppressWarnings("unchecked") List<String> bounded = (List<String>) changes.invoke(result);
		assertEquals(1, bounded.size());
		assertTrue(bounded.get(0).length() <= 240);
		assertTrue(bounded.get(0).endsWith("..."));
	}

	@Test void resultChangeBoundsRetainExactlyTwentyRealChanges() {
		List<String> changes = java.util.stream.IntStream.range(0, 20).mapToObj(index -> "changed Path" + index).toList();

		assertEquals(changes, ControlConnectorAccess.boundedResultChanges(changes));
	}

	@Test void recoveredProxyFileIntentRebuildsMaskedContent() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Database:\n  Password: local-secret\nProxy:\n  Enabled: false\n");
		ProxyConfigurationFileServiceAccess service = new ProxyConfigurationFileServiceAccess(file,
				ControlConnectorTest::atomicMove);
		connector.close();
		connector = fileConnector(service);
		ProxyConfigurationFileServiceAccess.Document installed = service.read(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Method fileIntent = taskResultClass().getDeclaredMethod("fileIntent", String.class, String.class, List.class);
		fileIntent.setAccessible(true);
		Object intent = ReflectiveAccess.invoke(fileIntent, null, new Object[]{ProxyConfigurationFileServiceAccess.FILE_NAME, installed.revision(), List.of("changed Proxy.Enabled")});
		Method persistIntent = ControlConnector.class.getDeclaredMethod("persistIntent", UUID.class,
				taskResultClass(), String.class);
		persistIntent.setAccessible(true);
		ReflectiveAccess.invoke(persistIntent, connector, new Object[]{UUID.fromString("00000000-0000-0000-0000-000000000099"), intent, "00000000-0000-0000-0000-000000000199"});
		Method prepare = ControlConnector.class.getDeclaredMethod("prepareWriteAheadIntents");
		prepare.setAccessible(true);
		ReflectiveAccess.invoke(prepare, connector, new Object[]{});

		StoredResult recovered = ProxyControlResultStoreAccess.load(dataDirectory).results().values().iterator().next();
		assertTrue(recovered.committed());
		String content = recovered.result().getAsJsonObject("configuration").get("content").getAsString();
		assertEquals(installed.content(), content);
		assertFalse(content.contains("local-secret"));
	}

	@Test void malformedRecoveredProxyFileIntentIsAborted() throws Exception {
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(
				dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME), ControlConnectorTest::atomicMove));
		Method fileIntent = taskResultClass().getDeclaredMethod("fileIntent", String.class, String.class, List.class);
		fileIntent.setAccessible(true);
		Object intent = ReflectiveAccess.invoke(fileIntent, null, new Object[]{"not-managed.yml", "anticipated-revision", List.of("changed")});
		Method persistIntent = ControlConnector.class.getDeclaredMethod("persistIntent", UUID.class,
				taskResultClass(), String.class);
		persistIntent.setAccessible(true);
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		ReflectiveAccess.invoke(persistIntent, connector, new Object[]{operationId, intent, "00000000-0000-0000-0000-000000000199"});

		Method prepare = ControlConnector.class.getDeclaredMethod("prepareWriteAheadIntents");
		prepare.setAccessible(true);
		ReflectiveAccess.invoke(prepare, connector, new Object[]{});

		StoredResult recovered = ProxyControlResultStoreAccess.load(dataDirectory).results().get(operationId);
		assertNotNull(recovered);
		assertTrue(recovered.committed());
		assertEquals("RECOVERY_ABORTED", recovered.result().get("code").getAsString());
	}

	@Test void lostResultResponseIsResubmittedBeforeAnotherOperationClaim() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = new CompletableFuture<>();
		ControlConnectorAccess.cycle(connector);
		transport.resultSubmission.completeExceptionally(new java.io.IOException("response lost"));
		assertEquals(Status.UNAVAILABLE, connector.status());

		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		ControlConnectorAccess.cycle(connector);

		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
	}

	@Test void recoveredProxyFileResultWaitsForItsCapabilityWithoutBlockingClaims() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Proxy:\n  Enabled: false\n");
		connector.close();
		connector = fileConnector(new ProxyConfigurationFileServiceAccess(file, ControlConnectorTest::atomicMove));
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", ProxyConfigurationFileServiceAccess.FILE_NAME);
		configuration.addProperty("content", "Proxy:\n  Enabled: false\n");
		result.add("configuration", configuration);
		Field completed = ControlConnector.class.getDeclaredField("completedTasks");
		completed.setAccessible(true);
		@SuppressWarnings("unchecked") Map<UUID, Object> results =
				(Map<UUID, Object>) completed.get(connector);
		results.put(UUID.fromString("00000000-0000-0000-0000-000000000099"),
				ReflectiveAccess.unwrap(new StoredResult(result, true, false)));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000299\","
						+ "\"type\":\"READ\",\"configuration\":{\"domain\":\"file\","
						+ "\"fileName\":\"bungeeconfig.yml\"}}"));

		ControlConnectorAccess.cycle(connector);

		assertEquals(0, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		assertEquals(1, completedTaskCount());
		AtomicBoolean replacement = new AtomicBoolean();
		Field deferred = ControlConnector.class.getDeclaredField("deferredReplacement");
		deferred.setAccessible(true);
		ReflectiveAccess.set(deferred, connector, (Runnable) () -> replacement.set(true));
		Method finishCycle = ControlConnector.class.getDeclaredMethod("finishCycle");
		finishCycle.setAccessible(true);
		ReflectiveAccess.invoke(finishCycle, connector, new Object[]{});
		assertTrue(replacement.get(), "a capability-blocked durable result must not prevent lifecycle replacement");
		assertEquals(1, completedTaskCount(), "the blocked result must remain journaled for a later capable session");

		Field accepted = ControlConnector.class.getDeclaredField("acceptedCapabilities");
		accepted.setAccessible(true);
		ReflectiveAccess.set(accepted, connector, Set.of("presence.snapshot", "config.proxy-files.v1"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		ControlConnectorAccess.cycle(connector);
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
		assertEquals(0, completedTaskCount());

		results.put(UUID.fromString("00000000-0000-0000-0000-000000000099"),
				ReflectiveAccess.unwrap(new StoredResult(result, false, true)));
		ReflectiveAccess.set(accepted, connector, Set.of("presence.snapshot", "config.proxy-routing.v1"));
		replacement.set(false);
		ReflectiveAccess.set(deferred, connector, (Runnable) () -> replacement.set(true));
		ReflectiveAccess.invoke(finishCycle, connector, new Object[]{});
		assertTrue(replacement.get(), "a capability-blocked lease-expired result must not prevent replacement");
		assertEquals(1, completedTaskCount(), "the lease-expired result must remain available for a capable session");
	}

	@Test void largeProxyFileReadSurvivesLostAcknowledgementAndConnectorRestart() throws Exception {
		Path file = dataDirectory.resolve(ProxyConfigurationFileServiceAccess.FILE_NAME);
		Files.writeString(file, "Large: '" + "a".repeat(300 * 1024) + "'\n");
		ProxyConfigurationFileServiceAccess service = new ProxyConfigurationFileServiceAccess(file,
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

		ControlConnectorAccess.cycle(connector);
		transport.resultSubmission.completeExceptionally(new java.io.IOException("acknowledgement lost"));
		assertEquals(Status.UNAVAILABLE, connector.status());
		assertTrue(Files.size(dataDirectory.resolve(".control-proxy-pending-results.json")) > 256 * 1024);

		connector.close();
		connector = fileConnector(service);
		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		ControlConnectorAccess.cycle(connector);

		assertFalse(Files.exists(dataDirectory.resolve(".control-proxy-pending-results.json")));
		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
	}

	@Test void expiredResultLeaseIsReclaimedAndReboundBeforeResubmission() {
		connector.close();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(409,
				"{\"error\":{\"code\":\"TASK_LEASE_EXPIRED\"}}"));

		ControlConnectorAccess.cycle(connector);

		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000299\","
						+ "\"type\":\"APPLY\",\"expectedRevision\":\"" + current.revision() + "\","
						+ "\"configuration\":{\"sendVotesToAllServers\":true,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(200, "{}"));
		ControlConnectorAccess.cycle(connector);

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
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()));
		transport.acceptConfiguration = true;
		transport.operationClaim = CompletableFuture.completedFuture(new Response(200,
				"{\"operationId\":\"00000000-0000-0000-0000-000000000099\","
						+ "\"attemptId\":\"00000000-0000-0000-0000-000000000199\","
						+ "\"type\":\"READ\",\"configuration\":"
						+ "{\"sendVotesToAllServers\":false,\"blockedServers\":[]}}"));
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}"));

		ControlConnectorAccess.cycle(connector);
		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		ControlConnectorAccess.cycle(connector);

		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
	}

	@Test void abandonedWriteAheadIntentRetainsItsCapabilityUntilControlCanAcceptIt() throws Exception {
		connector.close();
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		ProxyRoutingConfiguration proposal = new ProxyRoutingConfiguration(true, List.of());
		JsonObject anticipated = new JsonObject();
		anticipated.addProperty("success", true);
		anticipated.addProperty("code", "OK");
		anticipated.addProperty("message", "Configuration applied");
		anticipated.addProperty("revision", proposal.revision());
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", ProxyConfigurationFileServiceAccess.FILE_NAME);
		configuration.addProperty("content", "Proxy:\n  Enabled: true\n");
		anticipated.add("configuration", configuration);
		anticipated.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L,
				ProxyRoutingConfigurationServiceAccess.create(new NoOpPlatform()),
				Map.of(operationId, new StoredResult(anticipated, false, false)));
		transport.acceptConfiguration = true;
		transport.resultSubmission = CompletableFuture.completedFuture(new Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}"));

		ControlConnectorAccess.cycle(connector);
		assertEquals(0, transport.requests.stream().filter(request -> request.path().endsWith("/result")).count());
		assertEquals(1, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		Field accepted = ControlConnector.class.getDeclaredField("acceptedCapabilities");
		accepted.setAccessible(true);
		ReflectiveAccess.set(accepted, connector, Set.of("presence.snapshot", "config.proxy-files.v1"));
		ControlConnectorAccess.cycle(connector);
		JsonObject submitted = JsonParser.parseString(transport.requests.stream()
				.filter(request -> request.path().endsWith("/result")).findFirst().orElseThrow().body()).getAsJsonObject();
		assertEquals("RECOVERY_ABORTED", submitted.get("code").getAsString());

		transport.operationClaim = CompletableFuture.completedFuture(new Response(204, ""));
		ControlConnectorAccess.cycle(connector);
		assertEquals(2, transport.requests.stream().filter(request -> request.path().endsWith("/operations")).count());
		assertTrue(connector.reserveRuntimeReplacement());
	}

	@Test void closeBeforeOperationPublicationPreventsTheRequestChainFromStarting() throws Exception {
		connector.close();
		transport.firstSendEntered = new CountDownLatch(1);
		transport.releaseFirstSend = new CountDownLatch(1);
		connector = ControlConnectorAccess.create(settings(), scheduler, transport,
				() -> List.of(), logs::add, UUID.randomUUID(), () -> 0L);
		CompletableFuture<Void> cycling = CompletableFuture.runAsync(() -> ControlConnectorAccess.cycle(connector));
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

	@Test void closeCancelsARealQueuedDeploymentBeforeAwaitingItsOperation() throws Exception {
		connector.close();
		Path update = dataDirectory.resolve("update");
		connector = deploymentConnector(PluginDeploymentService.backend(update, dataDirectory.resolve("VotingPlugin.jar")));
		setConnectorField("registered", true);
		setConnectorField("deploymentsAccepted", true);
		setConnectorField("status", Status.CONNECTED);
		Field executorField = ControlConnector.class.getDeclaredField("deploymentExecutor");
		executorField.setAccessible(true);
		ExecutorService deploymentExecutor = (ExecutorService) executorField.get(connector);
		CountDownLatch workerStarted = new CountDownLatch(1);
		deploymentExecutor.submit(() -> {
			workerStarted.countDown();
			try {
				new CountDownLatch(1).await();
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
			}
		});
		assertTrue(workerStarted.await(2, TimeUnit.SECONDS));
		UUID deploymentId = UUID.randomUUID();
		transport.nextPrimary = new Response(200, "{\"deploymentId\":\"" + deploymentId
				+ "\",\"artifactId\":\"VotingPlugin.jar\",\"sha256\":\"" + "a".repeat(64)
				+ "\",\"size\":\"1\",\"attemptId\":\"" + UUID.randomUUID() + "\"}");
		Method poll = ControlConnector.class.getDeclaredMethod("pollDeployments");
		poll.setAccessible(true);
		ReflectiveAccess.invoke(poll, connector, new Object[]{});
		Field operationField = ControlConnector.class.getDeclaredField("activeOperation");
		operationField.setAccessible(true);
		CompletableFuture<?> operation = (CompletableFuture<?>) operationField.get(connector);
		assertNotNull(operation, "the actual deployment polling path must own the queued operation");

		connector.close();

		assertTrue(operation.isDone(), "shutdown must complete a deployment discarded before its executor starts it");
		assertFalse(Files.exists(update.resolve("VotingPlugin.jar")), "queued deployment work must not stage an artifact");
	}

	@Test void proxyDeploymentIsNotAdvertisedOnWindowsWhereTheLiveJarCannotBeReplaced() {
		assertFalse(ControlConnectorAccess.proxyDeploymentSupported("Windows 11"));
		assertFalse(ControlConnectorAccess.proxyDeploymentSupported("Windows Server 2022"));
		assertTrue(ControlConnectorAccess.proxyDeploymentSupported("Linux"));
		assertTrue(ControlConnectorAccess.proxyDeploymentSupported("Darwin"));
	}

	@Test void backoffIsBoundedExponentialAndJitteredWithoutSleeping() {
		assertEquals(1000, ControlConnectorAccess.backoffMillis(1, 0));
		assertEquals(2000, ControlConnectorAccess.backoffMillis(2, 0));
		assertTrue(ControlConnectorAccess.backoffMillis(3, 999) >= 4000);
		assertTrue(ControlConnectorAccess.backoffMillis(30, Long.MAX_VALUE) <= 300000);
		assertTrue(ControlConnectorAccess.backoffMillis(30, Long.MIN_VALUE) > 0);
	}

	@Test void backendIdentityPreservesSafeNamesAndDeterministicallyMapsOtherConfiguredNames() {
		assertEquals("lobby-1", ControlConnectorAccess.stableBackendId("lobby-1"));
		String mapped = ControlConnectorAccess.stableBackendId("Modded Lobby");
		assertTrue(mapped.matches("backend-[0-9a-f]{32}"));
		assertEquals(mapped, ControlConnectorAccess.stableBackendId("Modded Lobby"));
		assertNotEquals(mapped, ControlConnectorAccess.stableBackendId("Other Lobby"));
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

	private ControlConnector deploymentConnector(PluginDeploymentService deployments) throws Exception {
		Constructor<ControlConnector> constructor = ControlConnector.class.getDeclaredConstructor(Settings.class,
				ScheduledExecutorService.class, Transport.class, Supplier.class, Consumer.class, UUID.class,
				LongSupplier.class, ProxyRoutingConfigurationService.class, Path.class, ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route"),
				boolean.class, Runnable.class, Function.class, ProxyMethodConfigurationService.class, Runnable.class,
				ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService"), PluginDeploymentService.class, HttpClient.class, String.class,
				boolean.class, boolean.class);
		constructor.setAccessible(true);
		return ReflectiveAccess.newInstance(constructor, new Object[]{settings(), scheduler, transport, (Supplier<List<ObservedBackend>>) List::of, (Consumer<String>) logs::add, UUID.randomUUID(), (LongSupplier) () -> 0L, null, null, null, false, null, null, null, null, null, deployments, HttpClient.newHttpClient(), "credential", false, false});
	}

	private void setConnectorField(String name, Object value) throws Exception {
		Field field = ControlConnector.class.getDeclaredField(name);
		field.setAccessible(true);
		ReflectiveAccess.set(field, connector, value);
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
		return ((Map<UUID, Object>) completed.get(connector)).size();
	}

	@SuppressWarnings("unchecked")
	private ControlConnector fileConnector(ProxyConfigurationFileServiceAccess fileService) throws Exception {
		return fileConnector(fileService, false, null);
	}

	@SuppressWarnings("unchecked")
	private ControlConnector fileConnector(ProxyConfigurationFileServiceAccess fileService, boolean recovering,
			Runnable recoveryComplete) throws Exception {
		Constructor<ControlConnector> constructor = ControlConnector.class.getDeclaredConstructor(Settings.class,
				ScheduledExecutorService.class, Transport.class, Supplier.class, Consumer.class, UUID.class,
				LongSupplier.class, ProxyRoutingConfigurationService.class, Path.class, ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route"),
				boolean.class, Runnable.class, Function.class, ProxyMethodConfigurationService.class, Runnable.class,
				ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService"));
		constructor.setAccessible(true);
		ProxyControlResultStoreAccess.Route connectorRoute = new ProxyControlResultStoreAccess.Route("proxy-a", "Proxy A",
				"VELOCITY", "7.1.2", URI.create("http://127.0.0.1:8080"), "credential.txt", 30, 3000, 5000);
		ControlConnector created = ReflectiveAccess.newInstance(constructor, new Object[]{settings(), scheduler, transport, (Supplier<List<ObservedBackend>>) List::of, (Consumer<String>) logs::add, UUID.randomUUID(), (LongSupplier) () -> 0L, null, dataDirectory, connectorRoute, recovering, recoveryComplete, (Function<String, CompletableFuture<com.bencodez.votingplugin.proxy.VotingPluginProxy.CommunicationTestResult>>) null, null, null, fileService});
		ProxyControlResultStoreAccess.State recovered = ProxyControlResultStoreAccess.loadForRoute(dataDirectory, connectorRoute);
		if (recovered != null) {
			Field completed = ControlConnector.class.getDeclaredField("completedTasks");
			completed.setAccessible(true);
			((Map<UUID, Object>) completed.get(created)).putAll(ReflectiveAccess.nativeValues(recovered.results()));
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

	private static class NoOpPlatform implements ProxyRoutingConfigurationServiceAccess.Platform {
		@Override public ProxyRoutingConfiguration read() { return new ProxyRoutingConfiguration(false, List.of()); }
		@Override public java.util.Set<String> configuredServers() { return java.util.Set.of(); }
		@Override public void persist(ProxyRoutingConfiguration proposal, String expectedRevision)
				throws java.io.IOException { }
		@Override public void rollback() { }
		@Override public void reload() { }
	}
}
