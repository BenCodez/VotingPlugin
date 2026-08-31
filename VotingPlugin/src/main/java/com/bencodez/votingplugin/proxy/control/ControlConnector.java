package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongSupplier;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.presence.BackendPresenceStatus;
import com.bencodez.votingplugin.proxy.control.ProxyControlResultStore.Route;
import com.bencodez.votingplugin.proxy.control.ProxyControlResultStore.StoredResult;
import com.bencodez.votingplugin.util.BoundedHttpBodyHandler;
import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Optional, failure-isolated Control discovery connector shared by BungeeCord and Velocity.
 * It owns no vote-processing callbacks and never performs blocking I/O on a proxy event thread.
 */
public final class ControlConnector implements AutoCloseable {
	static final int PROTOCOL_VERSION = 1;
	static final int MAX_RESPONSE_BYTES = 4 * 1024 * 1024;
	private static final Pattern NODE_ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
	private static final Set<String> BASE_CAPABILITIES = Set.of("presence.snapshot");
	private static final String CONFIGURATION_CAPABILITY = "config.proxy-routing.v1";
	private static final String COMMUNICATION_TEST_CAPABILITY = "config.transport-test.v1";
	private static final String COMMUNICATION_TEST_PRESET = "communication-test";
	private static final String PROXY_METHOD_CAPABILITY = "config.proxy-method.v1";
	private static final String PROXY_FILE_CAPABILITY = "config.proxy-files.v1";
	private static final String PROXY_METHOD_PRESET = "proxy-method";
	private static final String INTERNAL_OPERATION_TYPE = "_controlOperationType";
	private static final long OPERATION_POLL_MILLIS = 1000;
	private static final long MAX_BACKOFF_MILLIS = TimeUnit.MINUTES.toMillis(5);
	private static final long OPERATION_SHUTDOWN_TIMEOUT_MILLIS = TimeUnit.SECONDS.toMillis(65);

	private final Settings settings;
	private final ScheduledExecutorService scheduler;
	private final Transport transport;
	private final Supplier<List<ObservedBackend>> snapshotSource;
	private final Consumer<String> logger;
	private final UUID sessionId;
	private final LongSupplier jitterSource;
	private final ProxyRoutingConfigurationService configurationService;
	private final ProxyMethodConfigurationService methodConfigurationService;
	private final ProxyConfigurationFileService fileConfigurationService;
	private final Function<String, CompletableFuture<VotingPluginProxy.CommunicationTestResult>> communicationTest;
	private final Runnable runtimeReplacement;
	private final Path dataDirectory;
	private final Route route;
	private final boolean recovering;
	private final Runnable recoveryComplete;
	private final Map<UUID, StoredResult> completedTasks = new LinkedHashMap<>();
	private final Object operationLifecycle = new Object();
	private final AtomicBoolean inFlight = new AtomicBoolean();
	private Runnable deferredReplacement;
	private volatile boolean closed;
	private volatile boolean registered;
	private volatile boolean configurationAccepted;
	private volatile int failures;
	private volatile long snapshotSequence;
	private volatile ScheduledFuture<?> scheduled;
	private volatile ScheduledFuture<?> operationPolling;
	private volatile CompletableFuture<?> activeRequest;
	private volatile CompletableFuture<Void> activeOperation;
	private volatile Status status = Status.STARTING;

	public ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource) {
		this(settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, null,
				null, null, false, null, null, null, null, null);
	}

	ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService) {
		this(settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, configurationService,
				null, null, false, null, null, null, null, null);
	}

	ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService,
			Map<UUID, StoredResult> recoveredTasks) {
		this(settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, configurationService,
				null, null, false, null, null, null, null, null);
		completedTasks.putAll(recoveredTasks);
	}

	private ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService, Path dataDirectory,
			Route route, boolean recovering, Runnable recoveryComplete,
			Function<String, CompletableFuture<VotingPluginProxy.CommunicationTestResult>> communicationTest,
			ProxyMethodConfigurationService methodConfigurationService, Runnable runtimeReplacement,
			ProxyConfigurationFileService fileConfigurationService) {
		this.settings = Objects.requireNonNull(settings, "settings");
		this.scheduler = Objects.requireNonNull(scheduler, "scheduler");
		this.transport = Objects.requireNonNull(transport, "transport");
		this.snapshotSource = Objects.requireNonNull(snapshotSource, "snapshotSource");
		this.logger = Objects.requireNonNull(logger, "logger");
		this.sessionId = Objects.requireNonNull(sessionId, "sessionId");
		this.jitterSource = Objects.requireNonNull(jitterSource, "jitterSource");
		this.configurationService = configurationService;
		this.methodConfigurationService = methodConfigurationService;
		this.communicationTest = communicationTest;
		this.runtimeReplacement = runtimeReplacement;
		this.fileConfigurationService = fileConfigurationService;
		this.dataDirectory = dataDirectory;
		this.route = route;
		this.recovering = recovering;
		this.recoveryComplete = recoveryComplete;
	}

	/** Creates the production connector without reading a credential when the feature is disabled. */
	public static ControlConnector create(VotingPluginProxy proxy) throws IOException {
		Path dataDirectory = proxy.getDataFolderPlugin().toPath().toAbsolutePath().normalize();
		ProxyControlResultStore.State recovered = ProxyControlResultStore.load(dataDirectory);
		VotingPluginProxyConfig config = proxy.getConfig();
		Settings settings;
		Route route;
		String credentialName;
		boolean recovering = recovered != null;
		if (recovered != null) {
			route = recovered.route();
			settings = settings(route);
			credentialName = route.credentialFile();
		} else {
			if (!config.getControlEnabled()) return null;
			String configuredNodeId = config.getControlNodeId();
			String nodeId = configuredNodeId == null || configuredNodeId.isBlank()
					? config.getProxyServerName() : configuredNodeId.trim();
			credentialName = config.getControlCredentialFile();
			if (credentialName == null || credentialName.isBlank()) {
				throw new IllegalArgumentException("Control.CredentialFile must be set");
			}
			String endpointValue = config.getControlEndpoint();
			if (endpointValue == null || endpointValue.isBlank()) {
				throw new IllegalArgumentException("Control.Endpoint must be set");
			}
			settings = new Settings(nodeId, nodeId, proxy.getProxyPlatform(), proxy.getPluginVersion(),
					URI.create(endpointValue.trim()), config.getControlHeartbeatSeconds(),
					config.getControlConnectTimeoutMillis(), config.getControlRequestTimeoutMillis());
			route = route(settings, credentialName);
		}
		String credential = ControlCredentialFile.read(dataDirectory, credentialName);
		HttpControlTransport transport = new HttpControlTransport(settings.endpoint(), credential,
				settings.connectTimeoutMillis(), settings.requestTimeoutMillis());
		Supplier<List<ObservedBackend>> snapshot = () -> {
			List<ObservedBackend> backends = new ArrayList<>();
			for (String server : proxy.getAllAvailableServers()) {
				if (server == null || server.isBlank()) {
					continue;
				}
				BackendPresenceStatus presence = proxy.getBackendPlayerPresenceTracker().getBackendStatus(server);
				String displayName = server.trim();
				backends.add(new ObservedBackend(stableBackendId(displayName),
						displayName.substring(0, Math.min(displayName.length(), 100)), presence != null,
						presence != null && presence.isAvailable(), presence == null ? 0 : presence.getPlayerCount()));
			}
			backends.sort(Comparator.comparing(ObservedBackend::backendId));
			return List.copyOf(backends);
		};
		ControlConnector connector = new ControlConnector(settings, proxy.getScheduler(), transport, snapshot,
				message -> proxy.log("[Control] " + message), UUID.randomUUID(),
				() -> ThreadLocalRandom.current().nextLong(), new ProxyRoutingConfigurationService(proxy), dataDirectory,
				route, recovering, proxy::restartControlServicesAfterRecovery,
				server -> proxy.testBackendCommunication(server, 5000L), new ProxyMethodConfigurationService(proxy),
				() -> proxy.reloadCore(true), new ProxyConfigurationFileService(proxy));
		if (recovered != null) connector.completedTasks.putAll(recovered.results());
		return connector;
	}

	public void start() {
		if (closed) {
			return;
		}
		status = Status.STARTING;
		schedule(0);
		operationPolling = scheduler.scheduleWithFixedDelay(this::pollOperations,
				OPERATION_POLL_MILLIS, OPERATION_POLL_MILLIS, TimeUnit.MILLISECONDS);
	}

	/** Polls only the operation queue; heartbeat and presence retain their configured cadence. */
	void pollOperations() {
		CompletableFuture<Void> operationDone;
		synchronized (operationLifecycle) {
			if (closed || !registered || status != Status.CONNECTED || !configurationAccepted
					|| !inFlight.compareAndSet(false, true)) return;
			operationDone = new CompletableFuture<>();
			activeOperation = operationDone;
		}
		CompletableFuture<Void> operation;
		try {
			if (hasCompletedTask()) {
				operation = submitCompletedResult();
			} else {
				CompletableFuture<Response> claim = transport.send(claimRequest());
				activeRequest = claim;
				operation = claim.thenCompose(this::handleClaimResponse);
			}
		} catch (RuntimeException failure) {
			operation = new CompletableFuture<>();
			operation.completeExceptionally(failure);
		}
		operation.whenComplete((ignored, failure) -> {
			Throwable cause = failure == null ? null : unwrap(failure);
			try {
				activeRequest = null;
				if (cause == null) {
					operationDone.complete(null);
				} else {
					registered = false;
					operationDone.completeExceptionally(cause);
				}
			} finally {
				if (activeOperation == operationDone) activeOperation = null;
				finishCycle();
			}
			if (cause != null && !closed) {
				ScheduledFuture<?> heartbeat = scheduled;
				if (heartbeat != null) heartbeat.cancel(false);
				onFailure(cause);
			}
		});
	}

	public Status status() {
		return status;
	}

	/** Defers a replacement while a cycle or unacknowledged result still belongs to this connector. */
	public boolean deferReplacementUntilSafe(Runnable replacement) {
		Objects.requireNonNull(replacement, "replacement");
		synchronized (operationLifecycle) {
			if (inFlight.get() || !completedTasks.isEmpty()) {
				deferredReplacement = replacement;
				return true;
			}
			closed = true;
			status = Status.STOPPED;
			return false;
		}
	}

	/** Reserves a quiescent connector for a full runtime replacement without losing a claimed result. */
	public boolean reserveRuntimeReplacement() {
		synchronized (operationLifecycle) {
			if (inFlight.get() || !completedTasks.isEmpty()) return false;
			closed = true;
			status = Status.STOPPED;
			return true;
		}
	}

	private void schedule(long delayMillis) {
		if (closed) {
			return;
		}
		try {
			scheduled = scheduler.schedule(this::cycle, delayMillis, TimeUnit.MILLISECONDS);
		} catch (java.util.concurrent.RejectedExecutionException ignored) {
			if (!closed) {
				status = Status.UNAVAILABLE;
			}
		}
	}

	void cycle() {
		synchronized (operationLifecycle) {
			if (closed) return;
			if (!inFlight.compareAndSet(false, true)) {
				// A fast operation claim can overlap the one-shot heartbeat. Re-arm
				// it so presence does not stop after this collision.
				schedule(OPERATION_POLL_MILLIS);
				return;
			}
		}
		Request first = registered ? heartbeatRequest() : registrationRequest();
		CompletableFuture<Response> primary;
		try {
			primary = transport.send(first);
		} catch (RuntimeException failure) {
			finishCycle();
			onFailure(failure);
			return;
		}
		CompletableFuture<Void> operationDone = new CompletableFuture<>();
		activeOperation = operationDone;
		if (closed) {
			primary.cancel(true);
			operationDone.complete(null);
			if (activeOperation == operationDone) activeOperation = null;
			finishCycle();
			return;
		}
		activeRequest = primary;
		CompletableFuture<Void> operation = primary.thenCompose(response -> {
			handlePrimaryResponse(response, !registered);
			registered = true;
			CompletableFuture<Response> presence = transport.send(presenceRequest());
			activeRequest = presence;
			return presence.thenCompose(responseBody -> {
				handlePresenceResponse(responseBody);
				if (!configurationAccepted) return CompletableFuture.completedFuture(null);
				if (hasCompletedTask()) return submitCompletedResult();
				CompletableFuture<Response> claim = transport.send(claimRequest());
				activeRequest = claim;
				return claim.thenCompose(this::handleClaimResponse);
			});
		});
		operation.whenComplete((ignored, failure) -> {
			try {
				activeRequest = null;
				if (!closed) {
					if (failure == null) {
						onSuccess();
					} else {
						onFailure(unwrap(failure));
					}
				}
			} finally {
				if (failure == null) {
					operationDone.complete(null);
				} else {
					operationDone.completeExceptionally(failure);
				}
				if (activeOperation == operationDone) activeOperation = null;
				finishCycle();
			}
		});
	}

	boolean hasActiveOperation() {
		return activeOperation != null;
	}

	private void finishCycle() {
		Runnable replacement = null;
		synchronized (operationLifecycle) {
			inFlight.set(false);
			if (completedTasks.isEmpty() && deferredReplacement != null) {
				replacement = deferredReplacement;
				deferredReplacement = null;
			}
		}
		if (replacement != null) replacement.run();
	}

	private void handlePrimaryResponse(Response response, boolean registration) {
		if (response.statusCode == 404 && !registration) {
			registered = false;
			throw new RegistryLostException();
		}
		requireSuccess(response);
		JsonObject body = parseObject(response.body);
		JsonObject node = body.getAsJsonObject("node");
		JsonArray accepted = node == null ? null : node.getAsJsonArray("acceptedCapabilities");
		if (registration) {
			JsonElement protocol = body.getAsJsonObject("identity").get("protocolVersion");
			if (protocol == null || protocol.getAsInt() != PROTOCOL_VERSION) {
				throw new ProtocolException();
			}
			if (accepted == null || !contains(accepted, "presence.snapshot")) {
				throw new ProtocolException();
			}
		}
		if (accepted != null) {
			if (!contains(accepted, "presence.snapshot")) {
				throw new ProtocolException();
			}
			configurationAccepted = contains(accepted, CONFIGURATION_CAPABILITY)
					|| contains(accepted, COMMUNICATION_TEST_CAPABILITY)
					|| contains(accepted, PROXY_METHOD_CAPABILITY) || contains(accepted, PROXY_FILE_CAPABILITY);
		}
	}

	private void handlePresenceResponse(Response response) {
		if (response.statusCode == 404) {
			registered = false;
			throw new RegistryLostException();
		}
		requireSuccess(response);
		JsonObject body = parseObject(response.body);
		if (!body.has("applied") || !body.has("node")) {
			throw new MalformedResponseException();
		}
	}

	private static void requireSuccess(Response response) {
		if (response.statusCode == 401 || response.statusCode == 403) {
			throw new AuthenticationException();
		}
		if (response.statusCode == 409) {
			throw new ProtocolException();
		}
		if (response.statusCode < 200 || response.statusCode >= 300) {
			throw new UnavailableException();
		}
	}

	private void onSuccess() {
		boolean wasConnected = status == Status.CONNECTED;
		failures = 0;
		status = Status.CONNECTED;
		if (!wasConnected) {
			logger.accept("authenticated discovery connected");
		}
		schedule(TimeUnit.SECONDS.toMillis(settings.heartbeatSeconds()));
	}

	private void onFailure(Throwable failure) {
		failures = Math.min(failures + 1, 30);
		Status next;
		String message;
		if (failure instanceof AuthenticationException) {
			next = Status.AUTHENTICATION_FAILED;
			message = "authentication failed; re-enroll or rotate the configured credential";
		} else if (failure instanceof ProtocolException) {
			next = Status.INCOMPATIBLE;
			message = "protocol or required capabilities are incompatible";
		} else {
			next = Status.UNAVAILABLE;
			message = "unavailable; voting remains unaffected";
		}
		if (status != next || failures == 1 || failures % 10 == 0) {
			logger.accept(message);
		}
		status = next;
		schedule(backoffMillis(failures, jitterSource.getAsLong()));
	}

	private Request registrationRequest() {
		JsonObject body = commonBody();
		body.addProperty("displayName", settings.displayName());
		body.addProperty("platform", settings.platform());
		body.addProperty("pluginVersion", settings.pluginVersion());
		addCapabilities(body);
		return new Request("POST", "/api/v1/nodes/register", body.toString());
	}

	private Request heartbeatRequest() {
		JsonObject body = sessionBody();
		addCapabilities(body);
		return new Request("PUT", "/api/v1/nodes/" + settings.nodeId() + "/heartbeat", body.toString());
	}

	private Request presenceRequest() {
		JsonObject body = sessionBody();
		body.addProperty("sequence", snapshotSequence++);
		JsonArray backends = new JsonArray();
		List<ObservedBackend> observed = snapshotSource.get();
		if (observed.size() > 4096) {
			throw new IllegalStateException("Control backend snapshot exceeds 4096 entries");
		}
		for (ObservedBackend backend : observed) {
			JsonObject item = new JsonObject();
			item.addProperty("backendId", backend.backendId());
			item.addProperty("displayName", backend.displayName());
			item.addProperty("presenceKnown", backend.presenceKnown());
			item.addProperty("available", backend.available());
			item.addProperty("playerCount", backend.playerCount());
			backends.add(item);
		}
		body.add("backends", backends);
		return new Request("PUT", "/api/v1/nodes/" + settings.nodeId() + "/presence", body.toString());
	}

	private Request claimRequest() {
		JsonObject body = new JsonObject();
		body.addProperty("sessionId", sessionId.toString());
		return new Request("POST", "/api/v1/nodes/" + settings.nodeId() + "/operations", body.toString());
	}

	private CompletableFuture<Void> handleClaimResponse(Response response) {
		if (response.statusCode == 204) return CompletableFuture.completedFuture(null);
		if (response.statusCode == 404) {
			registered = false;
			throw new RegistryLostException();
		}
		requireSuccess(response);
		JsonObject task = parseObject(response.body);
		UUID operationId = UUID.fromString(requireString(task, "operationId"));
		StoredResult result;
		synchronized (operationLifecycle) {
			result = completedTasks.get(operationId);
		}
		if (result != null) {
			if (!result.committed() && !result.claimRequired() && !anticipatedResultIsInstalled(result)) {
				result = null;
			} else {
				result = committedForAttempt(result, requireString(task, "attemptId"));
				synchronized (operationLifecycle) { completedTasks.put(operationId, result); }
				persistCompleted();
			}
		}
		if (result == null) {
			return executeTask(operationId, task).thenCompose(executed -> {
				JsonObject resultJson = executed.json();
				resultJson.addProperty("attemptId", requireString(task, "attemptId"));
				resultJson.addProperty(INTERNAL_OPERATION_TYPE, requireString(task, "type"));
				StoredResult completed = new StoredResult(resultJson, true, false);
				synchronized (operationLifecycle) { completedTasks.put(operationId, completed); }
				persistCompleted();
				return submitCompletedResult(operationId, completed);
			});
		}
		return submitCompletedResult(operationId, result);
	}

	private boolean hasCompletedTask() {
		prepareWriteAheadIntents();
		synchronized (operationLifecycle) {
			return completedTasks.values().stream().anyMatch(StoredResult::committed);
		}
	}

	private CompletableFuture<Void> submitCompletedResult() {
		Map.Entry<UUID, StoredResult> pending;
		synchronized (operationLifecycle) {
			pending = completedTasks.entrySet().stream().filter(entry -> entry.getValue().committed())
					.findFirst().orElse(null);
		}
		if (pending == null) return CompletableFuture.completedFuture(null);
		persistCompleted();
		return submitCompletedResult(pending.getKey(), pending.getValue());
	}

	private CompletableFuture<Void> submitCompletedResult(UUID operationId, StoredResult result) {
		CompletableFuture<Response> submitted = transport.send(resultRequest(operationId, result));
		activeRequest = submitted;
		return submitted.thenAccept(resultResponse -> {
			if (taskLeaseExpired(resultResponse)) {
				synchronized (operationLifecycle) {
					completedTasks.put(operationId, new StoredResult(result.result(), false, true));
				}
				persistCompleted();
				return;
			}
			if (operationNotFound(resultResponse)) {
				acknowledgeCompletedResult(operationId, result);
				return;
			}
			requireSuccess(resultResponse);
			acknowledgeCompletedResult(operationId, result);
		});
	}

	private void acknowledgeCompletedResult(UUID operationId, StoredResult result) {
		synchronized (operationLifecycle) { completedTasks.remove(operationId); }
		try {
			persistCompleted();
		} catch (RuntimeException failure) {
			synchronized (operationLifecycle) { completedTasks.put(operationId, result); }
			throw failure;
		}
		boolean drained;
		boolean replaceRuntime = requiresRuntimeReplacement(result) && runtimeReplacement != null;
		synchronized (operationLifecycle) {
			drained = completedTasks.isEmpty();
			if (replaceRuntime) deferredReplacement = runtimeReplacement;
		}
		if (recovering && drained && recoveryComplete != null && !replaceRuntime) recoveryComplete.run();
	}

	static boolean requiresRuntimeReplacement(StoredResult result) {
		JsonObject body = result.result();
		if (!body.has("success") || !body.get("success").getAsBoolean() || !body.has("configuration")
				|| !body.has(INTERNAL_OPERATION_TYPE)
				|| !"APPLY".equals(body.get(INTERNAL_OPERATION_TYPE).getAsString())) return false;
		JsonObject configuration = body.getAsJsonObject("configuration");
		return configuration != null && configuration.has("preset")
				&& PROXY_METHOD_PRESET.equals(configuration.get("preset").getAsString());
	}

	private static boolean taskLeaseExpired(Response response) {
		return responseCode(response, 409, "TASK_LEASE_EXPIRED");
	}

	private static boolean operationNotFound(Response response) {
		return responseCode(response, 404, "OPERATION_NOT_FOUND");
	}

	private static boolean responseCode(Response response, int status, String code) {
		if (response.statusCode != status) return false;
		try {
			JsonObject error = parseObject(response.body).getAsJsonObject("error");
			return error != null && error.has("code") && code.equals(error.get("code").getAsString());
		} catch (RuntimeException ignored) {
			return false;
		}
	}

	private void persistCompleted() {
		if (dataDirectory == null || route == null) return;
		Map<UUID, StoredResult> snapshot;
		synchronized (operationLifecycle) { snapshot = new LinkedHashMap<>(completedTasks); }
		try {
			ProxyControlResultStore.save(dataDirectory, route, snapshot);
		} catch (IOException e) {
			throw new UnavailableException(e);
		}
	}

	private void persistIntent(UUID operationId, TaskResult anticipated, String attemptId) {
		JsonObject result = anticipated.json();
		result.addProperty("attemptId", attemptId);
		result.addProperty(INTERNAL_OPERATION_TYPE, "APPLY");
		StoredResult previous;
		synchronized (operationLifecycle) {
			previous = completedTasks.put(operationId, new StoredResult(result, false, false));
		}
		try {
			persistCompleted();
		} catch (RuntimeException failure) {
			synchronized (operationLifecycle) {
				if (previous == null) completedTasks.remove(operationId);
				else completedTasks.put(operationId, previous);
			}
			throw failure;
		}
	}

	private void prepareWriteAheadIntents() {
		Map<UUID, StoredResult> snapshot;
		synchronized (operationLifecycle) { snapshot = new LinkedHashMap<>(completedTasks); }
		boolean changed = false;
		for (Map.Entry<UUID, StoredResult> entry : snapshot.entrySet()) {
			StoredResult pending = entry.getValue();
			if (pending.committed() || pending.claimRequired()) continue;
			StoredResult recovered = anticipatedResultIsInstalled(pending)
					? committedForAttempt(pending, requireString(pending.result(), "attemptId"))
					: abortedIntent(pending);
			synchronized (operationLifecycle) {
				if (completedTasks.get(entry.getKey()) == pending) {
					completedTasks.put(entry.getKey(), recovered);
					changed = true;
				}
			}
		}
		if (changed) {
			try {
				persistCompleted();
			} catch (RuntimeException failure) {
				synchronized (operationLifecycle) {
					completedTasks.clear();
					completedTasks.putAll(snapshot);
				}
				throw failure;
			}
		}
	}

	private static StoredResult abortedIntent(StoredResult pending) {
		JsonObject result = TaskResult.failure("RECOVERY_ABORTED",
				"Configuration apply did not finish before node recovery").json();
		result.addProperty("attemptId", requireString(pending.result(), "attemptId"));
		return new StoredResult(result, true, false);
	}

	private boolean anticipatedResultIsInstalled(StoredResult pending) {
		JsonObject result = pending.result();
		if (!result.has("revision")) return false;
		JsonObject configuration = result.getAsJsonObject("configuration");
		if (configuration != null && isProxyMethod(configuration) && methodConfigurationService != null) {
			return result.get("revision").getAsString().equals(methodConfigurationService.read().revision());
		}
		if (isProxyFile(configuration) && fileConfigurationService != null) {
			try {
				return result.get("revision").getAsString().equals(
						fileConfigurationService.read(requireString(configuration, "fileName")).revision());
			} catch (IOException failure) {
				return false;
			}
		}
		return configurationService != null
				&& result.get("revision").getAsString().equals(configurationService.read().revision());
	}

	private static StoredResult committedForAttempt(StoredResult pending, String attemptId) {
		JsonObject result = pending.result().deepCopy();
		result.addProperty("attemptId", attemptId);
		return new StoredResult(result, true, false);
	}

	private CompletableFuture<TaskResult> executeTask(UUID operationId, JsonObject task) {
		JsonObject requested = task.getAsJsonObject("configuration");
		if (isProxyFile(requested)) {
			return executeProxyFile(operationId, task, requested);
		}
		if (isCommunicationTest(requested)) return executeCommunicationTest(task, requested);
		if (isProxyMethod(requested)) return executeProxyMethod(operationId, task, requested);
		if (configurationService == null) return completed(TaskResult.failure("UNSUPPORTED", "Configuration control is unavailable"));
		String type = requireString(task, "type");
		try {
			ProxyRoutingConfiguration current = configurationService.read();
			if ("READ".equals(type)) return completed(TaskResult.success(current.revision(), current, List.of(), false));
			ProxyRoutingConfiguration proposal = parseConfiguration(task.getAsJsonObject("configuration"));
			configurationService.validate(proposal);
			List<String> changes = proposal.changesFrom(current);
			if ("PREVIEW".equals(type)) return completed(TaskResult.success(current.revision(), current, changes, false));
			if (!"APPLY".equals(type)) return completed(TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported"));
			persistIntent(operationId, TaskResult.success(proposal.revision(), proposal, changes, true),
					requireString(task, "attemptId"));
			configurationService.apply(proposal, requireString(task, "expectedRevision"));
			ProxyRoutingConfiguration applied = configurationService.read();
			return completed(TaskResult.success(applied.revision(), applied, changes, true));
		} catch (ProxyRoutingConfigurationService.StaleRevisionException e) {
			return completed(TaskResult.failure("STALE_REVISION", "Configuration changed after preview"));
		} catch (ProxyRoutingConfigurationService.ApplyFailureException e) {
			return completed(new TaskResult(false, "RELOAD_FAILED", "Reload failed after persistence", null, null,
					List.of(), false, e.rolledBack()));
		} catch (IllegalArgumentException e) {
			return completed(TaskResult.failure("VALIDATION_ERROR", e.getMessage()));
		} catch (IOException | RuntimeException e) {
			return completed(TaskResult.failure("APPLY_FAILED", "Configuration operation failed"));
		}
	}

	private CompletableFuture<TaskResult> executeProxyMethod(UUID operationId, JsonObject task, JsonObject requested) {
		if (methodConfigurationService == null) {
			return completed(TaskResult.failure("UNSUPPORTED", "Proxy method control is unavailable"));
		}
		String type = requireString(task, "type");
		try {
			ProxyMethodConfiguration current = methodConfigurationService.read();
			if ("READ".equals(type)) {
				JsonObject response = requested.deepCopy();
				JsonObject responseOptions = response.getAsJsonObject("options");
				if (responseOptions == null) {
					responseOptions = new JsonObject();
					response.add("options", responseOptions);
				}
				responseOptions.addProperty("method", current.method().name());
				return completed(TaskResult.success(current.revision(), response, List.of(), false,
						"Current proxy method is " + current.method().name()));
			}
			JsonObject options = requested.getAsJsonObject("options");
			ProxyMethodConfiguration proposal = new ProxyMethodConfiguration(
					ProxyMethodConfigurationService.canonical(requireString(options, "method")));
			methodConfigurationService.validate(proposal);
			List<String> changes = proposal.changesFrom(current);
			if ("PREVIEW".equals(type)) return completed(TaskResult.success(current.revision(), requested.deepCopy(),
					changes, false, "Required settings are present; runtime restart will follow apply"));
			if (!"APPLY".equals(type)) return completed(TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported"));
			persistIntent(operationId, TaskResult.success(proposal.revision(), requested.deepCopy(), changes, false,
					"Proxy method persisted; runtime restart is waiting for result acknowledgement"),
					requireString(task, "attemptId"));
			methodConfigurationService.apply(proposal, requireString(task, "expectedRevision"));
			return completed(TaskResult.success(proposal.revision(), requested.deepCopy(), changes, false,
					"Proxy method persisted; runtime restart follows acknowledgement"));
		} catch (ProxyMethodConfigurationService.StaleRevisionException e) {
			return completed(TaskResult.failure("STALE_REVISION", "Proxy method changed after preview"));
		} catch (ProxyMethodConfigurationService.ApplyFailureException e) {
			return completed(new TaskResult(false, "APPLY_FAILED", "Proxy method persistence failed", null, null,
					List.of(), false, e.rolledBack()));
		} catch (IllegalArgumentException e) {
			return completed(TaskResult.failure("VALIDATION_ERROR", e.getMessage()));
		} catch (IOException | RuntimeException e) {
			return completed(TaskResult.failure("APPLY_FAILED", "Proxy method operation failed"));
		}
	}

	private CompletableFuture<TaskResult> executeProxyFile(UUID operationId, JsonObject task, JsonObject requested) {
		if (fileConfigurationService == null) {
			return completed(TaskResult.failure("UNSUPPORTED", "Proxy file control is unavailable"));
		}
		String type = requireString(task, "type");
		String fileName = requireString(requested, "fileName");
		try {
			if ("READ".equals(type)) {
				return completed(TaskResult.file(fileConfigurationService.read(fileName), List.of(), false, false));
			}
			String content = requireString(requested, "content");
			ProxyConfigurationFileService.Preview preview = fileConfigurationService.preview(fileName, content);
			if ("PREVIEW".equals(type)) {
				return completed(TaskResult.file(fileConfigurationService.read(fileName), preview.changes(), false, false));
			}
			if (!"APPLY".equals(type)) return completed(TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported"));
			persistIntent(operationId, TaskResult.fileIntent(fileName,
					ProxyConfigurationFileService.revision(preview.resolvedContent()), preview.changes()),
					requireString(task, "attemptId"));
			ProxyConfigurationFileService.ApplyResult applied = fileConfigurationService.apply(fileName, content,
					requireString(task, "expectedRevision"));
			return completed(TaskResult.file(applied.document(), applied.changes(), false, applied.rolledBack(),
					"Proxy configuration saved; restart the proxy to activate general settings"));
		} catch (ProxyConfigurationFileService.StaleRevisionException failure) {
			return completed(TaskResult.failure("STALE_REVISION", "Proxy configuration changed after preview"));
		} catch (ProxyConfigurationFileService.ApplyFailureException failure) {
			return completed(new TaskResult(false, "APPLY_FAILED", "Proxy configuration could not be saved", null, null,
					List.of(), false, failure.rolledBack()));
		} catch (IllegalArgumentException failure) {
			return completed(TaskResult.failure("VALIDATION_ERROR", failure.getMessage()));
		} catch (IOException | RuntimeException failure) {
			return completed(TaskResult.failure("APPLY_FAILED", "Proxy configuration operation failed"));
		}
	}

	private CompletableFuture<TaskResult> executeCommunicationTest(JsonObject task, JsonObject requested) {
		if (!"READ".equals(requireString(task, "type"))) {
			return completed(TaskResult.failure("UNSUPPORTED_TASK", "Communication tests are read-only"));
		}
		if (communicationTest == null || configurationService == null) {
			return completed(TaskResult.failure("UNSUPPORTED", "Communication testing is unavailable"));
		}
		JsonObject options = requested == null ? null : requested.getAsJsonObject("options");
		String server = options == null ? "" : requireString(options, "server").trim();
		if (server.isEmpty() || server.length() > 100) {
			return completed(TaskResult.failure("VALIDATION_ERROR", "A valid backend server is required"));
		}
		String revision;
		try {
			revision = configurationService.read().revision();
		} catch (RuntimeException failure) {
			return completed(TaskResult.failure("READ_FAILED", "Could not read the active proxy configuration"));
		}
		return communicationTest.apply(server).handle((result, failure) -> {
			if (failure != null || result == null) {
				return TaskResult.failure("TEST_FAILED", "The communication test could not complete");
			}
			if (!result.success()) return TaskResult.failure(result.code(), result.message());
			String summary = result.server() + " replied via " + result.method() + " in "
					+ result.roundTripMillis() + " ms";
			return TaskResult.success(revision, requested.deepCopy(), List.of(summary), false, result.message());
		});
	}

	private static boolean isCommunicationTest(JsonObject requested) {
		return requested != null && requested.has("domain") && requested.has("preset")
				&& "quick-setup".equals(requested.get("domain").getAsString())
				&& COMMUNICATION_TEST_PRESET.equals(requested.get("preset").getAsString());
	}

	private static boolean isProxyMethod(JsonObject requested) {
		return requested != null && requested.has("domain") && requested.has("preset")
				&& "quick-setup".equals(requested.get("domain").getAsString())
				&& PROXY_METHOD_PRESET.equals(requested.get("preset").getAsString());
	}

	private static boolean isProxyFile(JsonObject requested) {
		return requested != null && requested.has("domain") && requested.get("domain").isJsonPrimitive()
				&& requested.getAsJsonPrimitive("domain").isString()
				&& "file".equals(requested.get("domain").getAsString());
	}

	private static CompletableFuture<TaskResult> completed(TaskResult result) {
		return CompletableFuture.completedFuture(result);
	}

	private static ProxyRoutingConfiguration parseConfiguration(JsonObject body) {
		if (body == null || !body.has("sendVotesToAllServers") || !body.has("blockedServers")) {
			throw new IllegalArgumentException("configuration is incomplete");
		}
		List<String> blocked = new ArrayList<>();
		body.getAsJsonArray("blockedServers").forEach(value -> blocked.add(value.getAsString()));
		return new ProxyRoutingConfiguration(body.get("sendVotesToAllServers").getAsBoolean(), blocked);
	}

	private Request resultRequest(UUID operationId, StoredResult result) {
		JsonObject body = result.result().deepCopy();
		body.remove(INTERNAL_OPERATION_TYPE);
		body.addProperty("sessionId", sessionId.toString());
		return new Request("POST", "/api/v1/nodes/" + settings.nodeId() + "/operations/" + operationId
				+ "/result", body.toString());
	}

	private static Route route(Settings settings, String credentialFile) {
		return new Route(settings.nodeId(), settings.displayName(), settings.platform(), settings.pluginVersion(),
				settings.endpoint(), credentialFile, settings.heartbeatSeconds(), settings.connectTimeoutMillis(),
				settings.requestTimeoutMillis());
	}

	private static Settings settings(Route route) {
		if (route.credentialFile() == null || route.credentialFile().isBlank()
				|| route.credentialFile().length() > 2048) {
			throw new IllegalArgumentException("Control.CredentialFile is invalid");
		}
		return new Settings(route.nodeId(), route.displayName(), route.platform(), route.pluginVersion(), route.endpoint(),
				route.heartbeatSeconds(), route.connectTimeoutMillis(), route.requestTimeoutMillis());
	}

	private static JsonObject configurationJson(ProxyRoutingConfiguration configuration) {
		JsonObject body = new JsonObject();
		body.addProperty("sendVotesToAllServers", configuration.sendVotesToAllServers());
		JsonArray blocked = new JsonArray();
		configuration.blockedServers().forEach(blocked::add);
		body.add("blockedServers", blocked);
		return body;
	}

	private static String requireString(JsonObject body, String name) {
		if (body == null || !body.has(name) || !body.get(name).isJsonPrimitive()) throw new MalformedResponseException();
		return body.get(name).getAsString();
	}

	private JsonObject commonBody() {
		JsonObject body = sessionBody();
		body.addProperty("nodeId", settings.nodeId());
		return body;
	}

	private JsonObject sessionBody() {
		JsonObject body = new JsonObject();
		body.addProperty("sessionId", sessionId.toString());
		body.addProperty("protocolVersion", PROTOCOL_VERSION);
		return body;
	}

	private void addCapabilities(JsonObject body) {
		JsonArray advertised = new JsonArray();
		BASE_CAPABILITIES.stream().sorted().forEach(advertised::add);
		if (configurationService != null) advertised.add(CONFIGURATION_CAPABILITY);
		if (communicationTest != null) advertised.add(COMMUNICATION_TEST_CAPABILITY);
		if (methodConfigurationService != null) advertised.add(PROXY_METHOD_CAPABILITY);
		if (fileConfigurationService != null) advertised.add(PROXY_FILE_CAPABILITY);
		body.add("capabilities", advertised);
		JsonArray required = new JsonArray();
		required.add("presence.snapshot");
		body.add("requiredCapabilities", required);
	}

	static long backoffMillis(int failures, long jitterValue) {
		int exponent = Math.max(0, Math.min(failures - 1, 8));
		long base = Math.min(1000L << exponent, MAX_BACKOFF_MILLIS);
		long jitterBound = Math.max(1, base / 4);
		return Math.min(MAX_BACKOFF_MILLIS, base + Math.floorMod(jitterValue, jitterBound));
	}

	static String stableBackendId(String serverName) {
		String value = serverName == null ? "" : serverName.trim();
		if (NODE_ID.matcher(value).matches()) {
			return value;
		}
		try {
			byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
			return "backend-" + HexFormat.of().formatHex(digest, 0, 16);
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable");
		}
	}

	@Override
	public void close() {
		synchronized (operationLifecycle) {
			closed = true;
			status = Status.STOPPED;
			deferredReplacement = null;
		}
		ScheduledFuture<?> scheduledRequest = scheduled;
		if (scheduledRequest != null) {
			scheduledRequest.cancel(false);
		}
		ScheduledFuture<?> polling = operationPolling;
		if (polling != null) polling.cancel(false);
		CompletableFuture<?> request = activeRequest;
		if (request != null) {
			request.cancel(true);
		}
		CompletableFuture<Void> operation = activeOperation;
		if (operation != null) {
			try {
				operation.get(OPERATION_SHUTDOWN_TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
			} catch (java.util.concurrent.CancellationException | java.util.concurrent.ExecutionException ignored) {
				// Cancellation/failure completes the chain and makes transport shutdown safe.
			} catch (java.util.concurrent.TimeoutException e) {
				throw new IllegalStateException("Control operation did not stop before connector shutdown", e);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Interrupted while waiting for the Control operation", e);
			}
		}
		transport.close();
	}

	private static JsonObject parseObject(String body) {
		try {
			JsonElement element = JsonParser.parseString(body);
			if (!element.isJsonObject()) {
				throw new MalformedResponseException();
			}
			return element.getAsJsonObject();
		} catch (RuntimeException e) {
			if (e instanceof ControlFailure) {
				throw e;
			}
			throw new MalformedResponseException();
		}
	}

	private static boolean contains(JsonArray array, String value) {
		for (JsonElement element : array) {
			if (element.isJsonPrimitive() && value.equals(element.getAsString())) {
				return true;
			}
		}
		return false;
	}

	private static Throwable unwrap(Throwable failure) {
		Throwable result = failure;
		while (result instanceof CompletionException && result.getCause() != null) {
			result = result.getCause();
		}
		return result;
	}

	public enum Status {
		STARTING, CONNECTED, AUTHENTICATION_FAILED, INCOMPATIBLE, UNAVAILABLE, STOPPED
	}

	public record Settings(String nodeId, String displayName, String platform, String pluginVersion, URI endpoint,
			int heartbeatSeconds, int connectTimeoutMillis, int requestTimeoutMillis) {
		public Settings {
			if (nodeId == null || !NODE_ID.matcher(nodeId).matches()) {
				throw new IllegalArgumentException("Control node ID is invalid");
			}
			if (displayName == null || displayName.isBlank() || displayName.length() > 100) {
				throw new IllegalArgumentException("Control display name is invalid");
			}
			if (!Set.of("BUNGEECORD", "VELOCITY").contains(platform)) {
				throw new IllegalArgumentException("Control platform is invalid");
			}
			if (pluginVersion == null || pluginVersion.isBlank() || pluginVersion.length() > 40) {
				throw new IllegalArgumentException("Control plugin version is invalid");
			}
			Objects.requireNonNull(endpoint, "endpoint");
			if (!Set.of("http", "https").contains(endpoint.getScheme()) || endpoint.getHost() == null
					|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
					|| (endpoint.getPath() != null && !endpoint.getPath().isEmpty() && !"/".equals(endpoint.getPath()))) {
				throw new IllegalArgumentException("Control endpoint must be an HTTP(S) origin without credentials or a path");
			}
			if (heartbeatSeconds < 10 || heartbeatSeconds > 300) {
				throw new IllegalArgumentException("Control heartbeat must be between 10 and 300 seconds");
			}
			if (connectTimeoutMillis < 500 || connectTimeoutMillis > 30000
					|| requestTimeoutMillis < 500 || requestTimeoutMillis > 30000) {
				throw new IllegalArgumentException("Control timeouts must be between 500 and 30000 milliseconds");
			}
		}
	}

	public record ObservedBackend(String backendId, String displayName, boolean presenceKnown,
			boolean available, int playerCount) {
		public ObservedBackend {
			if (backendId == null || !NODE_ID.matcher(backendId).matches()) {
				throw new IllegalArgumentException("Control backend ID is invalid");
			}
			if (displayName == null || displayName.isBlank() || displayName.length() > 100) {
				throw new IllegalArgumentException("Control backend display name is invalid");
			}
			if (playerCount < 0 || playerCount > 100000) {
				throw new IllegalArgumentException("Control backend player count is invalid");
			}
		}
	}

	public record Request(String method, String path, String body) { }
	public record Response(int statusCode, String body) { }

	private record TaskResult(boolean success, String code, String message, String revision,
			JsonObject configuration, List<String> changes, boolean reloaded, boolean rolledBack) {
		private JsonObject json() {
			JsonObject body = new JsonObject();
			body.addProperty("success", success);
			body.addProperty("code", code);
			body.addProperty("message", message);
			if (revision != null) body.addProperty("revision", revision);
			if (configuration != null) body.add("configuration", configuration);
			JsonArray listed = new JsonArray();
			changes.forEach(listed::add);
			body.add("changes", listed);
			body.addProperty("reloaded", reloaded);
			body.addProperty("rolledBack", rolledBack);
			return body;
		}

		private static TaskResult success(String revision, ProxyRoutingConfiguration configuration,
				List<String> changes, boolean reloaded) {
			return success(revision, configurationJson(configuration), changes, reloaded, "Operation completed");
		}
		private static TaskResult success(String revision, JsonObject configuration, List<String> changes,
				boolean reloaded, String message) {
			return new TaskResult(true, "OK", message, revision, configuration, changes, reloaded, false);
		}
		private static TaskResult file(ProxyConfigurationFileService.Document document, List<String> changes,
				boolean reloaded, boolean rolledBack) {
			return file(document, changes, reloaded, rolledBack, "Operation completed");
		}

		private static TaskResult file(ProxyConfigurationFileService.Document document, List<String> changes,
				boolean reloaded, boolean rolledBack, String message) {
			JsonObject configuration = new JsonObject();
			configuration.addProperty("domain", "file");
			configuration.addProperty("fileName", document.fileName());
			configuration.addProperty("content", document.content());
			return new TaskResult(true, "OK", message, document.revision(), configuration,
					List.copyOf(changes), reloaded, rolledBack);
		}

		private static TaskResult fileIntent(String fileName, String revision, List<String> changes) {
			JsonObject configuration = new JsonObject();
			configuration.addProperty("domain", "file");
			configuration.addProperty("fileName", fileName);
			return new TaskResult(true, "OK",
					"Proxy configuration saved; restart the proxy to activate general settings", revision, configuration,
					List.copyOf(changes), false, false);
		}
		private static TaskResult failure(String code, String message) {
			return new TaskResult(false, code, message == null ? "Operation failed" : message, null, null, List.of(), false, false);
		}
	}

	public interface Transport extends AutoCloseable {
		CompletableFuture<Response> send(Request request);
		@Override default void close() { }
	}

	static final class HttpControlTransport implements Transport {
		private final URI endpoint;
		private final String credential;
		private final Duration requestTimeout;
		private final HttpClient client;

		HttpControlTransport(URI endpoint, String credential, int connectTimeoutMillis, int requestTimeoutMillis) {
			this.endpoint = endpoint;
			this.credential = credential;
			this.requestTimeout = Duration.ofMillis(requestTimeoutMillis);
			this.client = HttpClient.newBuilder().connectTimeout(Duration.ofMillis(connectTimeoutMillis))
					.followRedirects(HttpClient.Redirect.NEVER).build();
		}

		@Override
		public CompletableFuture<Response> send(Request request) {
			HttpRequest httpRequest = HttpRequest.newBuilder(endpoint.resolve(request.path))
					.timeout(requestTimeout).header("Content-Type", "application/json")
					.header("Authorization", "Bearer " + credential)
					.method(request.method, HttpRequest.BodyPublishers.ofString(request.body, StandardCharsets.UTF_8))
					.build();
			return client.sendAsync(httpRequest, new BoundedHttpBodyHandler(MAX_RESPONSE_BYTES, requestTimeout))
					.thenApply(response -> new Response(response.statusCode(),
							new String(response.body(), StandardCharsets.UTF_8)));
		}
	}

	@SuppressWarnings("serial")
	private abstract static class ControlFailure extends RuntimeException { }
	@SuppressWarnings("serial")
	private static final class AuthenticationException extends ControlFailure { }
	@SuppressWarnings("serial")
	private static final class ProtocolException extends ControlFailure { }
	@SuppressWarnings("serial")
	private static final class RegistryLostException extends ControlFailure { }
	@SuppressWarnings("serial")
	private static final class UnavailableException extends ControlFailure {
		private UnavailableException() { }
		private UnavailableException(Throwable cause) { super.initCause(cause); }
	}
	@SuppressWarnings("serial")
	private static final class MalformedResponseException extends ControlFailure { }
}
