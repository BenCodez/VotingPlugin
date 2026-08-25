package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
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
import java.util.function.LongSupplier;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.presence.BackendPresenceStatus;
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
	static final int MAX_RESPONSE_BYTES = 64 * 1024;
	private static final Pattern NODE_ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
	private static final Set<String> BASE_CAPABILITIES = Set.of("presence.snapshot");
	private static final String CONFIGURATION_CAPABILITY = "config.proxy-routing.v1";
	private static final long MAX_BACKOFF_MILLIS = TimeUnit.MINUTES.toMillis(5);

	private final Settings settings;
	private final ScheduledExecutorService scheduler;
	private final Transport transport;
	private final Supplier<List<ObservedBackend>> snapshotSource;
	private final Consumer<String> logger;
	private final UUID sessionId;
	private final LongSupplier jitterSource;
	private final ProxyRoutingConfigurationService configurationService;
	private final Map<UUID, TaskResult> completedTasks = new LinkedHashMap<>();
	private final AtomicBoolean inFlight = new AtomicBoolean();
	private volatile boolean closed;
	private volatile boolean registered;
	private volatile boolean configurationAccepted;
	private volatile int failures;
	private volatile long snapshotSequence;
	private volatile ScheduledFuture<?> scheduled;
	private volatile CompletableFuture<?> activeRequest;
	private volatile Status status = Status.STARTING;

	public ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource) {
		this(settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, null);
	}

	ControlConnector(Settings settings, ScheduledExecutorService scheduler, Transport transport,
			Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId,
			LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService) {
		this.settings = Objects.requireNonNull(settings, "settings");
		this.scheduler = Objects.requireNonNull(scheduler, "scheduler");
		this.transport = Objects.requireNonNull(transport, "transport");
		this.snapshotSource = Objects.requireNonNull(snapshotSource, "snapshotSource");
		this.logger = Objects.requireNonNull(logger, "logger");
		this.sessionId = Objects.requireNonNull(sessionId, "sessionId");
		this.jitterSource = Objects.requireNonNull(jitterSource, "jitterSource");
		this.configurationService = configurationService;
	}

	/** Creates the production connector without reading a credential when the feature is disabled. */
	public static ControlConnector create(VotingPluginProxy proxy) throws IOException {
		VotingPluginProxyConfig config = proxy.getConfig();
		if (!config.getControlEnabled()) {
			return null;
		}
		String configuredNodeId = config.getControlNodeId();
		String nodeId = configuredNodeId == null || configuredNodeId.isBlank()
				? config.getProxyServerName() : configuredNodeId.trim();
		Path dataDirectory = proxy.getDataFolderPlugin().toPath().toAbsolutePath().normalize();
		String credentialName = config.getControlCredentialFile();
		if (credentialName == null || credentialName.isBlank()) {
			throw new IllegalArgumentException("Control.CredentialFile must be set");
		}
		Path credentialFile = dataDirectory.resolve(credentialName).normalize();
		if (!credentialFile.startsWith(dataDirectory)) {
			throw new IllegalArgumentException("Control.CredentialFile must stay within the plugin data folder");
		}
		String credential = readCredential(credentialFile);
		String endpointValue = config.getControlEndpoint();
		if (endpointValue == null || endpointValue.isBlank()) {
			throw new IllegalArgumentException("Control.Endpoint must be set");
		}
		Settings settings = new Settings(nodeId, nodeId, proxy.getProxyPlatform(), proxy.getPluginVersion(),
				URI.create(endpointValue.trim()), config.getControlHeartbeatSeconds(),
				config.getControlConnectTimeoutMillis(), config.getControlRequestTimeoutMillis());
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
		return new ControlConnector(settings, proxy.getScheduler(), transport, snapshot,
				message -> proxy.log("[Control] " + message), UUID.randomUUID(),
				() -> ThreadLocalRandom.current().nextLong(), new ProxyRoutingConfigurationService(proxy));
	}

	public void start() {
		if (closed) {
			return;
		}
		status = Status.STARTING;
		schedule(0);
	}

	public Status status() {
		return status;
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
		if (closed || !inFlight.compareAndSet(false, true)) {
			return;
		}
		Request first = registered ? heartbeatRequest() : registrationRequest();
		CompletableFuture<Response> primary;
		try {
			primary = transport.send(first);
		} catch (RuntimeException failure) {
			inFlight.set(false);
			onFailure(failure);
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
				CompletableFuture<Response> claim = transport.send(claimRequest());
				activeRequest = claim;
				return claim.thenCompose(this::handleClaimResponse);
			});
		});
		operation.whenComplete((ignored, failure) -> {
			activeRequest = null;
			inFlight.set(false);
			if (closed) {
				return;
			}
			if (failure == null) {
				onSuccess();
			} else {
				onFailure(unwrap(failure));
			}
		});
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
		configurationAccepted = accepted != null && contains(accepted, CONFIGURATION_CAPABILITY);
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
		TaskResult result;
		synchronized (completedTasks) {
			result = completedTasks.get(operationId);
		}
		if (result == null) {
			result = executeTask(task);
			synchronized (completedTasks) {
				completedTasks.put(operationId, result);
				while (completedTasks.size() > 128) completedTasks.remove(completedTasks.keySet().iterator().next());
			}
		}
		CompletableFuture<Response> submitted = transport.send(resultRequest(operationId, result));
		activeRequest = submitted;
		return submitted.thenAccept(ControlConnector::requireSuccess);
	}

	private TaskResult executeTask(JsonObject task) {
		if (configurationService == null) return TaskResult.failure("UNSUPPORTED", "Configuration control is unavailable");
		String type = requireString(task, "type");
		try {
			ProxyRoutingConfiguration current = configurationService.read();
			if ("READ".equals(type)) return TaskResult.success(current.revision(), current, List.of(), false);
			ProxyRoutingConfiguration proposal = parseConfiguration(task.getAsJsonObject("configuration"));
			configurationService.validate(proposal);
			List<String> changes = proposal.changesFrom(current);
			if ("PREVIEW".equals(type)) return TaskResult.success(current.revision(), current, changes, false);
			if (!"APPLY".equals(type)) return TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported");
			configurationService.apply(proposal, requireString(task, "expectedRevision"));
			ProxyRoutingConfiguration applied = configurationService.read();
			return TaskResult.success(applied.revision(), applied, changes, true);
		} catch (ProxyRoutingConfigurationService.StaleRevisionException e) {
			return TaskResult.failure("STALE_REVISION", "Configuration changed after preview");
		} catch (ProxyRoutingConfigurationService.ApplyFailureException e) {
			return new TaskResult(false, "RELOAD_FAILED", "Reload failed after persistence", null, null, List.of(),
					false, e.rolledBack());
		} catch (IllegalArgumentException e) {
			return TaskResult.failure("VALIDATION_ERROR", e.getMessage());
		} catch (IOException | RuntimeException e) {
			return TaskResult.failure("APPLY_FAILED", "Configuration operation failed");
		}
	}

	private static ProxyRoutingConfiguration parseConfiguration(JsonObject body) {
		if (body == null || !body.has("sendVotesToAllServers") || !body.has("blockedServers")) {
			throw new IllegalArgumentException("configuration is incomplete");
		}
		List<String> blocked = new ArrayList<>();
		body.getAsJsonArray("blockedServers").forEach(value -> blocked.add(value.getAsString()));
		return new ProxyRoutingConfiguration(body.get("sendVotesToAllServers").getAsBoolean(), blocked);
	}

	private Request resultRequest(UUID operationId, TaskResult result) {
		JsonObject body = new JsonObject();
		body.addProperty("sessionId", sessionId.toString());
		body.addProperty("success", result.success);
		body.addProperty("code", result.code);
		body.addProperty("message", result.message);
		if (result.revision != null) body.addProperty("revision", result.revision);
		if (result.configuration != null) body.add("configuration", configurationJson(result.configuration));
		JsonArray changes = new JsonArray();
		result.changes.forEach(changes::add);
		body.add("changes", changes);
		body.addProperty("reloaded", result.reloaded);
		body.addProperty("rolledBack", result.rolledBack);
		return new Request("POST", "/api/v1/nodes/" + settings.nodeId() + "/operations/" + operationId
				+ "/result", body.toString());
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
		closed = true;
		status = Status.STOPPED;
		ScheduledFuture<?> scheduledRequest = scheduled;
		if (scheduledRequest != null) {
			scheduledRequest.cancel(false);
		}
		CompletableFuture<?> request = activeRequest;
		if (request != null) {
			request.cancel(true);
		}
		transport.close();
	}

	private static String readCredential(Path path) throws IOException {
		if (!Files.isRegularFile(path) || Files.size(path) > 512) {
			throw new IOException("Control credential file is missing or invalid");
		}
		String credential = Files.readString(path, StandardCharsets.UTF_8).trim();
		if (credential.isEmpty() || credential.length() > 512) {
			throw new IOException("Control credential file is missing or invalid");
		}
		return credential;
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
			ProxyRoutingConfiguration configuration, List<String> changes, boolean reloaded, boolean rolledBack) {
		private static TaskResult success(String revision, ProxyRoutingConfiguration configuration,
				List<String> changes, boolean reloaded) {
			return new TaskResult(true, "OK", "Operation completed", revision, configuration, changes, reloaded, false);
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
			return client.sendAsync(httpRequest, HttpResponse.BodyHandlers.ofInputStream())
					.thenApply(response -> new Response(response.statusCode(), readBounded(response.body())));
		}

		private static String readBounded(InputStream input) {
			try (input) {
				byte[] bytes = input.readNBytes(MAX_RESPONSE_BYTES + 1);
				if (bytes.length > MAX_RESPONSE_BYTES) {
					throw new MalformedResponseException();
				}
				return new String(bytes, StandardCharsets.UTF_8);
			} catch (IOException e) {
				throw new CompletionException(new UnavailableException());
			}
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
	private static final class UnavailableException extends ControlFailure { }
	@SuppressWarnings("serial")
	private static final class MalformedResponseException extends ControlFailure { }
}
