package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.control.BackendControlResultStore.Route;
import com.bencodez.votingplugin.control.BackendControlResultStore.StoredResult;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.bencodez.votingplugin.util.BoundedHttpBodyHandler;
import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Optional outbound Bukkit node connector for full configuration-file and quick-setup control. */
public final class BackendControlConnector implements AutoCloseable {
	private static final int PROTOCOL_VERSION = 1;
	private static final int MAX_RESPONSE_BYTES = 4 * 1024 * 1024;
	private static final long SHUTDOWN_TIMEOUT_SECONDS = 65;
	private static final Pattern NODE_ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
	private static final Set<String> CAPABILITIES = Set.of("config.files.v1", "config.file-comments.v1",
			"config.quick-setup.v1", "config.vote-sites-sync.v1", "config.proxy-method.v1");

	private final VotingPluginMain plugin;
	private final Path dataDirectory;
	private final Settings settings;
	private final String credential;
	private final String credentialVerifier;
	private volatile HostConfiguration hostedConfiguration;
	private final ScheduledExecutorService executor;
	private final HttpClient http;
	private final BackendConfigurationService configurations;
	private final UUID sessionId = UUID.randomUUID();
	private final Map<UUID, StoredResult> completed = new LinkedHashMap<>();
	private final boolean recovering;
	private final AtomicBoolean running = new AtomicBoolean();
	private final Object operationLifecycle = new Object();
	private final Object journalLifecycle = new Object();
	private volatile boolean closed;
	private volatile boolean registered;
	private volatile boolean operationsAccepted;
	private volatile boolean quickSetupsAccepted;
	private volatile boolean voteSitesSyncAccepted;
	private volatile int failures;
	private volatile ScheduledFuture<?> scheduled;
	private volatile Future<?> activeReload;
	private volatile CompletableFuture<Void> activeOperation;

	private BackendControlConnector(VotingPluginMain plugin, Path dataDirectory, Settings settings, String credential,
			HostConfiguration hostedConfiguration, boolean recovering) {
		this.plugin = plugin;
		this.dataDirectory = dataDirectory;
		this.settings = settings;
		this.credential = credential;
		this.credentialVerifier = ControlCredentialFile.sha256Verifier(credential);
		this.hostedConfiguration = hostedConfiguration;
		this.recovering = recovering;
		ThreadFactory factory = runnable -> {
			Thread thread = new Thread(runnable, "votingplugin-control-backend");
			thread.setDaemon(true);
			return thread;
		};
		executor = Executors.newSingleThreadScheduledExecutor(factory);
		http = HttpClient.newBuilder().connectTimeout(Duration.ofMillis(settings.connectTimeoutMillis()))
				.followRedirects(HttpClient.Redirect.NEVER).build();
		configurations = new BackendConfigurationService(plugin.getDataFolder().toPath(), this::reloadConfiguration);
	}

	private void reloadConfiguration(String fileName) throws Exception {
		Future<?> reload;
		synchronized (operationLifecycle) {
			if (closed) throw new IllegalStateException("Bukkit Control connector is stopping");
			reload = plugin.getServer().getScheduler().callSyncMethod(plugin, () -> {
				plugin.reloadFromControl();
				if ("BungeeSettings.yml".equals(fileName)) plugin.restartBackendProxyHandler();
				return null;
			});
			activeReload = reload;
		}
		try {
			reload.get(30, TimeUnit.SECONDS);
		} finally {
			synchronized (operationLifecycle) {
				if (activeReload == reload) activeReload = null;
			}
		}
	}

	public static BackendControlConnector create(VotingPluginMain plugin) throws IOException {
		Path root = plugin.getDataFolder().toPath().toAbsolutePath().normalize();
		BackendControlResultStore.State recovered = BackendControlResultStore.load(root);
		if (recovered != null) {
			Settings settings = Settings.from(recovered.route());
			String credential = ControlCredentialFile.read(root, settings.credentialFile());
			BackendControlConnector connector = new BackendControlConnector(plugin, root, settings, credential,
					recovered.hostedConfiguration(), true);
			connector.completed.putAll(recovered.results());
			return connector;
		}
		ConfigurationSection control = plugin.getConfigFile().getData().getConfigurationSection("Control.Backend");
		if (control == null || !control.getBoolean("Enabled", false)) return null;
		String nodeId = BackendControlAutoEnrollment.configuredNodeId(plugin, control);
		if (!NODE_ID.matcher(nodeId).matches()) throw new IllegalArgumentException("Control.Backend.NodeId is invalid");
		String endpointValue = control.getString("Endpoint", "").trim();
		URI endpoint = URI.create(endpointValue);
		if (!Set.of("http", "https").contains(endpoint.getScheme()) || endpoint.getHost() == null
				|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
				|| (endpoint.getPath() != null && !endpoint.getPath().isEmpty() && !"/".equals(endpoint.getPath()))) {
			throw new IllegalArgumentException("Control.Backend.Endpoint must be an HTTP(S) origin");
		}
		String credentialFile = control.getString("CredentialFile", "control/control-credential.txt");
		String credential = ControlCredentialFile.read(root, credentialFile);
		Settings settings = new Settings(nodeId, endpoint, credentialFile,
				bounded(control.getInt("HeartbeatSeconds", 30), 10, 300, "HeartbeatSeconds"),
				bounded(control.getInt("ConnectTimeoutMillis", 3000), 500, 30000, "ConnectTimeoutMillis"),
				bounded(control.getInt("RequestTimeoutMillis", 10000), 500, 30000, "RequestTimeoutMillis"));
		return new BackendControlConnector(plugin, root, settings, credential,
				plugin.getActiveBackendHostedControlConfigurationSnapshot(), false);
	}

	/** Hosted settings that must remain active while a durable result is recovered. */
	public static HostConfiguration recoveredHostedConfiguration(Path dataDirectory) throws IOException {
		BackendControlResultStore.State recovered = BackendControlResultStore.load(dataDirectory);
		return recovered == null ? null : recovered.hostedConfiguration();
	}

	public boolean hasPendingResults() {
		synchronized (completed) { return !completed.isEmpty(); }
	}

	public boolean isClosed() {
		return closed;
	}

	public void start() { schedule(0); }

	private void schedule(long delayMillis) {
		if (!closed) scheduled = executor.schedule(this::cycle, delayMillis, TimeUnit.MILLISECONDS);
	}

	private void cycle() {
		if (closed || !running.compareAndSet(false, true)) return;
		try {
			JsonObject primary = registered ? heartbeat() : register();
			JsonObject node = primary.has("node") ? primary.getAsJsonObject("node") : null;
			if (!registered) {
				JsonObject identity = primary.has("identity") ? primary.getAsJsonObject("identity") : null;
				if (identity == null || identity.get("protocolVersion").getAsInt() != PROTOCOL_VERSION) {
					throw new ConnectorException("protocol mismatch");
				}
			}
			operationsAccepted = negotiatedCapability(node, "config.files.v1", operationsAccepted);
			quickSetupsAccepted = negotiatedCapability(node, "config.quick-setup.v1", quickSetupsAccepted);
			voteSitesSyncAccepted = negotiatedCapability(node, "config.vote-sites-sync.v1", voteSitesSyncAccepted);
			try {
				requireFileCapability(operationsAccepted);
			} catch (ConnectorException incompatible) {
				registered = false;
				throw incompatible;
			}
			registered = true;
			plugin.backendControlAuthenticated(settings.nodeId(), settings.credentialFile(),
					settings.endpoint().toString(), credentialVerifier);
			if (closed) return;
			if (operationsAccepted) {
				CompletableFuture<Void> operation = new CompletableFuture<>();
				synchronized (operationLifecycle) {
					if (closed) return;
					activeOperation = operation;
				}
				try {
					claimAndExecute();
				} finally {
					operation.complete(null);
					synchronized (operationLifecycle) {
						if (activeOperation == operation) activeOperation = null;
					}
				}
			}
			if (failures > 0) plugin.getLogger().info("[Control] Bukkit configuration connector recovered");
			failures = 0;
		} catch (Exception failure) {
			failures = Math.min(30, failures + 1);
			if (failures == 1 || failures % 10 == 0) {
				plugin.getLogger().warning("[Control] Bukkit configuration connector unavailable; VotingPlugin remains active");
			}
		} finally {
			running.set(false);
			long delay = failures == 0 ? TimeUnit.SECONDS.toMillis(settings.heartbeatSeconds())
					: Math.min(TimeUnit.MINUTES.toMillis(5), 1000L << Math.min(failures - 1, 8));
			schedule(delay);
		}
	}

	private JsonObject register() throws Exception {
		// A registration must explicitly establish required capabilities. Heartbeats may omit the unchanged set.
		operationsAccepted = false;
		quickSetupsAccepted = false;
		voteSitesSyncAccepted = false;
		JsonObject body = sessionBody();
		body.addProperty("nodeId", settings.nodeId());
		body.addProperty("displayName", settings.nodeId());
		body.addProperty("platform", "BUKKIT");
		body.addProperty("pluginVersion", plugin.getDescription().getVersion());
		JsonArray detectedPlugins = new JsonArray();
		java.util.Arrays.stream(plugin.getServer().getPluginManager().getPlugins())
				.map(installed -> installed.getDescription().getName()).filter(name -> name != null && !name.isBlank())
				.distinct().sorted(String.CASE_INSENSITIVE_ORDER).limit(128).forEach(detectedPlugins::add);
		body.add("detectedPlugins", detectedPlugins);
		addCapabilities(body);
		return requireObject(send("POST", "/api/v1/nodes/register", body), 200, 201);
	}

	private JsonObject heartbeat() throws Exception {
		JsonObject body = sessionBody();
		addCapabilities(body);
		Response response = send("PUT", "/api/v1/nodes/" + settings.nodeId() + "/heartbeat", body);
		if (response.status() == 404) {
			registered = false;
			return register();
		}
		return requireObject(response, 200);
	}

	private void claimAndExecute() throws Exception {
		if (submitCompletedResult()) return;
		JsonObject body = new JsonObject();
		body.addProperty("sessionId", sessionId.toString());
		Response response = send("POST", "/api/v1/nodes/" + settings.nodeId() + "/operations", body);
		if (response.status() == 204) return;
		JsonObject task = requireObject(response, 200);
		if (closed) return;
		UUID operationId = UUID.fromString(string(task, "operationId"));
		StoredResult result;
		synchronized (completed) {
			result = completed.get(operationId);
		}
		if (result != null) {
			if (!result.committed() && !result.claimRequired() && !anticipatedResultIsInstalled(result)) {
				result = null;
			} else {
				result = committedForAttempt(result, string(task, "attemptId"));
				synchronized (completed) { completed.put(operationId, result); }
				persistCompleted();
			}
		}
		if (result == null) {
			TaskResult executed = execute(operationId, task);
			JsonObject resultJson = executed.json();
			resultJson.addProperty("attemptId", string(task, "attemptId"));
			result = new StoredResult(resultJson, executed.restartConnector(), true, false);
			synchronized (completed) {
				completed.put(operationId, result);
			}
			persistCompleted();
		}
		submitCompletedResult(operationId, result);
	}

	private boolean submitCompletedResult() throws Exception {
		prepareWriteAheadIntents();
		Map.Entry<UUID, StoredResult> pending;
		synchronized (completed) {
			pending = completed.entrySet().stream().filter(entry -> entry.getValue().committed()).findFirst().orElse(null);
		}
		if (pending == null) return false;
		persistCompleted();
		return submitCompletedResult(pending.getKey(), pending.getValue());
	}

	private boolean submitCompletedResult(UUID operationId, StoredResult submitted) throws Exception {
		JsonObject resultBody = submitted.result().deepCopy();
		resultBody.addProperty("sessionId", sessionId.toString());
		Response response = send("POST", "/api/v1/nodes/" + settings.nodeId() + "/operations/" + operationId
				+ "/result", resultBody);
		if (taskLeaseExpired(response)) {
			synchronized (completed) {
				completed.put(operationId,
						new StoredResult(submitted.result(), submitted.restartConnector(), false, true));
			}
			persistCompleted();
			return false;
		}
		if (operationNotFound(response)) {
			acknowledgeCompletedResult(operationId, submitted);
			return true;
		}
		afterResultAcknowledged(
				() -> requireObject(response, 200),
				() -> acknowledgeCompletedResult(operationId, submitted));
		return true;
	}

	private void acknowledgeCompletedResult(UUID operationId, StoredResult submitted) throws Exception {
		synchronized (completed) { completed.remove(operationId); }
		try {
			persistCompleted();
		} catch (Exception failure) {
			synchronized (completed) { completed.put(operationId, submitted); }
			throw failure;
		}
		boolean drained;
		synchronized (completed) { drained = completed.isEmpty(); }
		if (drained && (submitted.restartConnector() || recovering
				|| plugin.hasDeferredBackendControlReconciliation())) {
			plugin.getServer().getScheduler().runTask(plugin, plugin::restartBackendControlConnector);
		}
	}

	static boolean taskLeaseExpired(Response response) {
		return responseCode(response, 409, "TASK_LEASE_EXPIRED");
	}

	static boolean operationNotFound(Response response) {
		return responseCode(response, 404, "OPERATION_NOT_FOUND");
	}

	private static boolean responseCode(Response response, int status, String code) {
		if (response.status() != status) return false;
		try {
			JsonObject error = JsonParser.parseString(response.body()).getAsJsonObject().getAsJsonObject("error");
			return error != null && error.has("code") && code.equals(error.get("code").getAsString());
		} catch (RuntimeException ignored) {
			return false;
		}
	}

	private void persistCompleted() throws IOException {
		synchronized (journalLifecycle) {
			Map<UUID, StoredResult> snapshot;
			synchronized (completed) { snapshot = new LinkedHashMap<>(completed); }
			if (!snapshot.isEmpty()) hostedConfiguration = plugin.getActiveBackendHostedControlConfigurationSnapshot();
			BackendControlResultStore.save(dataDirectory, settings.route(), hostedConfiguration, snapshot);
		}
	}

	/** Durably aligns pending results before the hosted lifecycle publishes a new active process. */
	public void publishHostedConfiguration(HostConfiguration configuration, Runnable publication) throws IOException {
		synchronized (journalLifecycle) {
			Map<UUID, StoredResult> snapshot;
			synchronized (completed) { snapshot = new LinkedHashMap<>(completed); }
			if (!snapshot.isEmpty()) {
				BackendControlResultStore.save(dataDirectory, settings.route(), configuration, snapshot);
			}
			hostedConfiguration = configuration;
			publication.run();
		}
	}

	private void persistIntent(UUID operationId, TaskResult anticipated, String attemptId) throws IOException {
		JsonObject result = anticipated.json();
		result.addProperty("attemptId", attemptId);
		synchronized (completed) {
			completed.put(operationId, new StoredResult(result, anticipated.restartConnector(), false, false));
		}
		persistCompleted();
	}

	private void prepareWriteAheadIntents() throws IOException {
		Map<UUID, StoredResult> snapshot;
		synchronized (completed) { snapshot = new LinkedHashMap<>(completed); }
		boolean changed = false;
		for (Map.Entry<UUID, StoredResult> entry : snapshot.entrySet()) {
			StoredResult pending = entry.getValue();
			if (pending.committed() || pending.claimRequired()) continue;
			StoredResult recovered = anticipatedResultIsInstalled(pending)
					? committedForAttempt(pending, string(pending.result(), "attemptId"))
					: abortedIntent(pending);
			synchronized (completed) {
				if (completed.get(entry.getKey()) == pending) {
					completed.put(entry.getKey(), recovered);
					changed = true;
				}
			}
		}
		if (changed) persistCompleted();
	}

	static StoredResult abortedIntent(StoredResult pending) {
		JsonObject result = TaskResult.failure("RECOVERY_ABORTED",
				"Configuration apply did not finish before node recovery").json();
		result.addProperty("attemptId", string(pending.result(), "attemptId"));
		return new StoredResult(result, false, true, false);
	}

	private boolean anticipatedResultIsInstalled(StoredResult pending) throws IOException {
		JsonObject result = pending.result();
		if (!result.has("revision") || !result.has("configuration")) return false;
		String revision = result.get("revision").getAsString();
		JsonObject configuration = result.getAsJsonObject("configuration");
		String domain = string(configuration, "domain");
		if ("file".equals(domain)) {
			return revision.equals(configurations.read(string(configuration, "fileName")).revision());
		}
		if ("quick-setup".equals(domain)) {
			return revision.equals(configurations.currentQuickSetupRevision(string(configuration, "preset")));
		}
		return false;
	}

	private static StoredResult committedForAttempt(StoredResult pending, String attemptId) {
		JsonObject result = pending.result().deepCopy();
		result.addProperty("attemptId", attemptId);
		return new StoredResult(result, pending.restartConnector(), true, false);
	}

	static void afterResultAcknowledged(ResultSubmission submission, ResultAcknowledgement acknowledged)
			throws Exception {
		submission.submit();
		acknowledged.acknowledge();
	}

	private TaskResult execute(UUID operationId, JsonObject task) {
		try {
			String type = string(task, "type");
			JsonObject configuration = task.getAsJsonObject("configuration");
			String domain = string(configuration, "domain");
			if ("file".equals(domain)) return executeFile(operationId, type, configuration, task);
			if ("quick-setup".equals(domain)) {
				if (!quickSetupsAccepted) return TaskResult.failure("UNSUPPORTED_TASK", "Quick setups were not negotiated");
				return executeQuick(operationId, type, configuration, task);
			}
			return TaskResult.failure("UNSUPPORTED_TASK", "Configuration domain is unsupported");
		} catch (BackendConfigurationService.StaleRevisionException e) {
			return TaskResult.failure("STALE_REVISION", "Configuration changed after preview");
		} catch (BackendConfigurationService.ApplyFailureException e) {
			return TaskResult.failure("RELOAD_FAILED", "Reload failed after persistence", e.rolledBack());
		} catch (IllegalArgumentException e) {
			return TaskResult.failure("VALIDATION_ERROR", e.getMessage());
		} catch (Exception e) {
			return TaskResult.failure("APPLY_FAILED", "Configuration operation failed");
		}
	}

	private TaskResult executeFile(UUID operationId, String type, JsonObject configuration, JsonObject task)
			throws IOException {
		String fileName = string(configuration, "fileName");
		if ("READ".equals(type)) {
			BackendConfigurationService.Document document = configurations.read(fileName);
			return TaskResult.file(document, List.of(), false, false, false);
		}
		String content = string(configuration, "content");
		if ("PREVIEW".equals(type)) {
			BackendConfigurationService.Preview preview = configurations.preview(fileName, content);
			BackendConfigurationService.Document current = configurations.read(fileName);
			if (!preview.revision().equals(current.revision())) throw new BackendConfigurationService.StaleRevisionException();
			return TaskResult.file(current, preview.changes(), false, false, false);
		}
		if ("APPLY".equals(type)) {
			BackendConfigurationService.Preview preview = configurations.preview(fileName, content);
			persistIntent(operationId,
					TaskResult.file(configurations.proposedDocument(preview), preview.changes(), true, false,
							"Config.yml".equals(fileName)), string(task, "attemptId"));
			BackendConfigurationService.ApplyResult applied = configurations.apply(fileName, content,
					string(task, "expectedRevision"));
			return TaskResult.file(applied.document(), applied.changes(), true, applied.rolledBack(),
					"Config.yml".equals(fileName));
		}
		return TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported");
	}

	private TaskResult executeQuick(UUID operationId, String type, JsonObject configuration, JsonObject task)
			throws IOException {
		if ("READ".equals(type)) return TaskResult.failure("UNSUPPORTED_TASK", "Quick setups cannot be read");
		String preset = string(configuration, "preset");
		if (!quickSetupCapabilityAccepted(preset, quickSetupsAccepted, voteSitesSyncAccepted)) {
			return TaskResult.failure("UNSUPPORTED_TASK", "VoteSites sync was not negotiated");
		}
		Map<String, String> options = options(configuration.getAsJsonObject("options"));
		if ("PREVIEW".equals(type)) {
			BackendConfigurationService.QuickPreview preview = configurations.previewQuickSetup(preset, options);
			return TaskResult.quick(preset, options, preview.revision(), preview.changes(), false);
		}
		if ("APPLY".equals(type)) {
			BackendConfigurationService.QuickPreview preview = configurations.previewQuickSetup(preset, options);
			persistIntent(operationId,
					TaskResult.quick(preset, options, configurations.proposedQuickSetupRevision(preview), preview.changes(),
							true, "Config.yml".equals(preview.proposal().fileName())), string(task, "attemptId"));
			BackendConfigurationService.ApplyResult applied = configurations.applyQuickSetup(preset, options,
					string(task, "expectedRevision"));
			return TaskResult.quick(preset, options, applied.document().revision(), applied.changes(), true,
					"Config.yml".equals(applied.document().fileName()));
		}
		return TaskResult.failure("UNSUPPORTED_TASK", "Task type is unsupported");
	}

	static boolean quickSetupCapabilityAccepted(String preset, boolean quickSetupsAccepted,
			boolean voteSitesSyncAccepted) {
		return quickSetupsAccepted && (!"sync-vote-sites".equals(preset) || voteSitesSyncAccepted);
	}

	private Response send(String method, String path, JsonObject body) throws Exception {
		HttpRequest request = HttpRequest.newBuilder(settings.endpoint().resolve(path))
				.timeout(Duration.ofMillis(settings.requestTimeoutMillis())).header("Content-Type", "application/json")
				.header("Authorization", "Bearer " + credential)
				.method(method, HttpRequest.BodyPublishers.ofString(body.toString(), StandardCharsets.UTF_8)).build();
		HttpResponse<byte[]> response = http.send(request,
				new BoundedHttpBodyHandler(MAX_RESPONSE_BYTES,
						Duration.ofMillis(settings.requestTimeoutMillis())));
		return new Response(response.statusCode(), new String(response.body(), StandardCharsets.UTF_8));
	}

	private static JsonObject requireObject(Response response, int... statuses) {
		boolean accepted = false;
		for (int status : statuses) if (response.status() == status) accepted = true;
		if (!accepted) throw new ConnectorException("unexpected response");
		JsonElement element = JsonParser.parseString(response.body());
		if (!element.isJsonObject()) throw new ConnectorException("malformed response");
		return element.getAsJsonObject();
	}

	private JsonObject sessionBody() {
		JsonObject body = new JsonObject();
		body.addProperty("sessionId", sessionId.toString());
		body.addProperty("protocolVersion", PROTOCOL_VERSION);
		return body;
	}

	static void addCapabilities(JsonObject body) {
		JsonArray capabilities = new JsonArray();
		CAPABILITIES.stream().sorted().forEach(capabilities::add);
		body.add("capabilities", capabilities);
		JsonArray required = new JsonArray();
		required.add("config.files.v1");
		body.add("requiredCapabilities", required);
	}

	private static boolean contains(JsonArray accepted, String expected) {
		if (accepted == null) return false;
		for (JsonElement value : accepted) if (value.isJsonPrimitive() && expected.equals(value.getAsString())) return true;
		return false;
	}

	static boolean negotiatedCapability(JsonObject node, String capability, boolean current) {
		if (node == null || !node.has("acceptedCapabilities")) return current;
		return contains(node.getAsJsonArray("acceptedCapabilities"), capability);
	}

	static void requireFileCapability(boolean accepted) {
		if (!accepted) throw new ConnectorException("required config.files.v1 capability was not accepted");
	}

	private static String string(JsonObject object, String name) {
		if (object == null || !object.has(name) || object.get(name).isJsonNull()
				|| !object.get(name).isJsonPrimitive()) throw new IllegalArgumentException(name + " is required");
		return object.get(name).getAsString();
	}

	private static Map<String, String> options(JsonObject object) {
		Map<String, String> values = new LinkedHashMap<>();
		if (object != null) object.entrySet().forEach(entry -> values.put(entry.getKey(), entry.getValue().getAsString()));
		return Map.copyOf(values);
	}

	private static int bounded(int value, int min, int max, String name) {
		if (value < min || value > max) throw new IllegalArgumentException("Control.Backend." + name + " is invalid");
		return value;
	}

	@Override
	public void close() {
		Future<?> reload;
		CompletableFuture<Void> operation;
		synchronized (operationLifecycle) {
			closed = true;
			reload = activeReload;
			operation = activeOperation;
		}
		ScheduledFuture<?> current = scheduled;
		if (current != null) current.cancel(false);
		if (reload != null && Bukkit.isPrimaryThread()) reload.cancel(false);
		awaitShutdown(executor, operation);
	}

	static void awaitShutdown(ScheduledExecutorService executor, CompletableFuture<Void> operation) {
		executor.shutdown();
		if (operation != null) {
			try {
				operation.get(SHUTDOWN_TIMEOUT_SECONDS, TimeUnit.SECONDS);
			} catch (java.util.concurrent.CancellationException | java.util.concurrent.ExecutionException ignored) {
				// A completed failed operation is no longer able to outlive shutdown.
			} catch (java.util.concurrent.TimeoutException e) {
				throw new IllegalStateException("Bukkit Control operation did not stop before connector shutdown", e);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Interrupted while waiting for the Bukkit Control operation", e);
			}
		}
		try {
			if (!executor.awaitTermination(SHUTDOWN_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
				throw new IllegalStateException("Bukkit Control operation did not stop before connector shutdown");
			}
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted while waiting for the Bukkit Control operation", e);
		}
	}

	private record Settings(String nodeId, URI endpoint, String credentialFile, int heartbeatSeconds,
			int connectTimeoutMillis, int requestTimeoutMillis) {
		private Settings {
			if (!NODE_ID.matcher(nodeId).matches()) throw new IllegalArgumentException("Control.Backend.NodeId is invalid");
			if (!Set.of("http", "https").contains(endpoint.getScheme()) || endpoint.getHost() == null
					|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
					|| (endpoint.getPath() != null && !endpoint.getPath().isEmpty() && !"/".equals(endpoint.getPath()))) {
				throw new IllegalArgumentException("Control.Backend.Endpoint must be an HTTP(S) origin");
			}
			if (credentialFile == null || credentialFile.isBlank() || credentialFile.length() > 2048) {
				throw new IllegalArgumentException("Control.Backend.CredentialFile is invalid");
			}
			bounded(heartbeatSeconds, 10, 300, "HeartbeatSeconds");
			bounded(connectTimeoutMillis, 500, 30000, "ConnectTimeoutMillis");
			bounded(requestTimeoutMillis, 500, 30000, "RequestTimeoutMillis");
		}

		private static Settings from(Route route) {
			return new Settings(route.nodeId(), route.endpoint(), route.credentialFile(), route.heartbeatSeconds(),
					route.connectTimeoutMillis(), route.requestTimeoutMillis());
		}

		private Route route() {
			return new Route(nodeId, endpoint, credentialFile, heartbeatSeconds, connectTimeoutMillis,
					requestTimeoutMillis);
		}
	}
	record Response(int status, String body) { }
	@FunctionalInterface
	interface ResultSubmission { void submit() throws Exception; }
	@FunctionalInterface
	interface ResultAcknowledgement { void acknowledge() throws Exception; }

	private record TaskResult(boolean success, String code, String message, String revision,
			JsonObject configuration, List<String> changes, boolean reloaded, boolean rolledBack,
			boolean restartConnector) {
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

		private static TaskResult file(BackendConfigurationService.Document document, List<String> changes,
				boolean reloaded, boolean rolledBack, boolean restartConnector) {
			JsonObject config = new JsonObject();
			config.addProperty("domain", "file");
			config.addProperty("fileName", document.fileName());
			config.addProperty("content", document.content());
			return new TaskResult(true, "OK", "Operation completed", document.revision(), config,
					List.copyOf(changes), reloaded, rolledBack, restartConnector);
		}

		private static TaskResult quick(String preset, Map<String, String> options, String revision,
				List<String> changes, boolean reloaded) {
			return quick(preset, options, revision, changes, reloaded, false);
		}
		private static TaskResult quick(String preset, Map<String, String> options, String revision,
				List<String> changes, boolean reloaded, boolean restartConnector) {
			JsonObject config = new JsonObject();
			config.addProperty("domain", "quick-setup");
			config.addProperty("preset", preset);
			JsonObject values = new JsonObject();
			options.forEach((name, value) -> {
				// The source document is an input to the merge, not result data. It
				// may be large and must not be retained or echoed by Control.
				if (!"sourceContent".equals(name)) values.addProperty(name, value);
			});
			config.add("options", values);
			return new TaskResult(true, "OK", "Operation completed", revision, config, List.copyOf(changes),
					reloaded, false, restartConnector);
		}

		private static TaskResult failure(String code, String message) { return failure(code, message, false); }
		private static TaskResult failure(String code, String message, boolean rolledBack) {
			return new TaskResult(false, code, message == null ? "Operation failed" : message, null, null,
					List.of(), false, rolledBack, false);
		}
	}

	@SuppressWarnings("serial") private static final class ConnectorException extends RuntimeException {
		private ConnectorException(String message) { super(message); }
	}
}
