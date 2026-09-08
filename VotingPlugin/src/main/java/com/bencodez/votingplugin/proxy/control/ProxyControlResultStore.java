package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Durable result journal for proxy-routing operations accepted by Control nodes.
 *
 * <p>The journal is intentionally a single bounded file. A result is kept under
 * the stable identity of the coordinator that accepted its operation, so changing
 * plugin version, heartbeat, or HTTP timeouts cannot move it to another route.
 * Saving one route merges it into the existing journal; it never replaces other
 * coordinators' results.</p>
 */
final class ProxyControlResultStore {
	private static final int LEGACY_VERSION = 2;
	private static final int VERSION = 3;
	/** Matches the bounded Control response envelope, including escaped managed-file content. */
	private static final int MAX_BYTES = 4 * 1024 * 1024;
	private static final int MAX_ROUTES = 16;
	private static final int MAX_JOURNAL_BYTES = MAX_ROUTES * MAX_BYTES;
	private static final int MAX_RESULT_BYTES = MAX_BYTES;
	/** Global bound across every coordinator route in this journal. */
	private static final int MAX_RESULTS = 128;
	private static final String FILE_NAME = ".control-proxy-pending-results.json";

	private ProxyControlResultStore() { }

	/**
	 * Loads one route for legacy callers which only have a single connector. New
	 * code should use {@link #loadForRoute(Path, Route)} so another coordinator's
	 * results can never be imported accidentally.
	 */
	static State load(Path dataDirectory) throws IOException {
		Journal journal = loadJournal(dataDirectory);
		if (journal == null || journal.routes().isEmpty()) return null;
		return state(journal.routes().values().iterator().next());
	}

	/** Loads only results belonging to the requested stable coordinator identity. */
	static State loadForRoute(Path dataDirectory, Route route) throws IOException {
		Objects.requireNonNull(route, "route");
		Journal journal = loadJournal(dataDirectory);
		if (journal == null) return null;
		RouteEntry entry = journal.routes().get(route.identity());
		return entry == null ? null : state(entry);
	}

	/**
	 * Returns one route which still requires its originating coordinator. This is
	 * only a compatibility fallback for startup while Control is disabled; enabled
	 * connectors must always call {@link #loadForRoute(Path, Route)}.
	 */
	static State loadRequired(Path dataDirectory) throws IOException {
		Journal journal = loadJournal(dataDirectory);
		if (journal == null) return null;
		for (RouteEntry entry : journal.routes().values()) {
			if (entry.routeRequired()) return state(entry);
		}
		return null;
	}

	/**
	 * Selects the next route a configured connector must drive. Required recovery
	 * routes take precedence so configuration changes cannot strand their results;
	 * once they drain, the connector restart advances to the next route and finally
	 * returns to the configured coordinator.
	 */
	static State loadPreferred(Path dataDirectory, Route configuredRoute) throws IOException {
		Objects.requireNonNull(configuredRoute, "configuredRoute");
		Journal journal = loadJournal(dataDirectory);
		if (journal == null) return null;
		for (RouteEntry entry : journal.routes().values()) {
			if (entry.routeRequired()) return state(entry);
		}
		RouteEntry configured = journal.routes().get(configuredRoute.identity());
		return configured == null ? null : state(configured);
	}

	static void save(Path dataDirectory, Route route, Map<UUID, StoredResult> results) throws IOException {
		save(dataDirectory, route, results, true);
	}

	/**
	 * Replaces only the requested route's results and atomically writes the merged
	 * global journal. If the global result or byte bound would be exceeded, this
	 * method fails before touching the existing file; unacknowledged results are
	 * never evicted to make room.
	 */
	static void save(Path dataDirectory, Route route, Map<UUID, StoredResult> results, boolean routeRequired)
			throws IOException {
		Objects.requireNonNull(dataDirectory, "dataDirectory");
		Objects.requireNonNull(route, "route");
		Objects.requireNonNull(results, "results");
		if (results.size() > MAX_RESULTS) throw new IOException("Too many pending Control proxy results");

		Path target = target(dataDirectory);
		Journal journal = loadJournal(dataDirectory);
		LinkedHashMap<String, RouteEntry> routes = journal == null
				? new LinkedHashMap<>() : new LinkedHashMap<>(journal.routes());
		String identity = route.identity();
		if (results.isEmpty()) {
			if (Files.isSymbolicLink(target)) throw new IOException("Control proxy-result journal is unsafe");
			routes.remove(identity);
			if (routes.isEmpty()) {
				DurableFiles.deleteIfExists(target);
				return;
			}
		} else {
			LinkedHashMap<UUID, StoredResult> copiedResults = new LinkedHashMap<>();
			results.forEach((operationId, result) -> {
				if (operationId == null || result == null) throw invalid();
				copiedResults.put(operationId, result);
			});
			// Keep the newest runtime metadata for the same stable coordinator. The
			// identity itself deliberately excludes mutable timing/version fields.
			if (!routes.containsKey(identity) && routes.size() >= MAX_ROUTES)
				throw new IOException("Too many pending Control coordinator routes");
			routes.put(identity, new RouteEntry(route, copiedResults, routeRequired));
		}
		if (totalResults(routes) > MAX_RESULTS) throw new IOException("Too many pending Control proxy results");

		byte[] bytes = serialize(routes);
		if (bytes.length > MAX_JOURNAL_BYTES) throw new IOException("Control proxy-result journal is too large");

		Files.createDirectories(dataDirectory);
		if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)
				&& (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target))) {
			throw new IOException("Control proxy-result journal is unsafe");
		}
		Path staging = Files.createTempFile(dataDirectory, ".control-proxy-results-", ".json");
		try {
			try (FileChannel channel = FileChannel.open(staging, StandardOpenOption.TRUNCATE_EXISTING,
					StandardOpenOption.WRITE)) {
				ByteBuffer buffer = ByteBuffer.wrap(bytes);
				while (buffer.hasRemaining()) channel.write(buffer);
				channel.force(true);
			}
			Files.move(staging, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			DurableFiles.forceDirectory(dataDirectory);
		} finally {
			Files.deleteIfExists(staging);
		}
	}

	private static Journal loadJournal(Path dataDirectory) throws IOException {
		Path target = target(dataDirectory);
		if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)
				|| Files.size(target) > MAX_JOURNAL_BYTES) {
			throw new IOException("Control proxy-result journal is unsafe or too large");
		}
		byte[] bytes;
		try (SeekableByteChannel channel = Files.newByteChannel(target,
				Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
			ByteBuffer buffer = ByteBuffer.allocate((int) Files.size(target) + 1);
			while (channel.read(buffer) >= 0 && buffer.hasRemaining()) { }
			if (!buffer.hasRemaining()) throw new IOException("Control proxy-result journal is too large");
			buffer.flip();
			bytes = new byte[buffer.remaining()];
			buffer.get(bytes);
		}
		try {
			JsonElement parsed = JsonParser.parseString(new String(bytes, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw invalid();
			JsonObject root = parsed.getAsJsonObject();
			int version = integer(root, "version");
			if (version == LEGACY_VERSION) return legacyJournal(root);
			if (version == VERSION) return currentJournal(root);
			throw invalid();
		} catch (RuntimeException e) {
			throw new IOException("Control proxy-result journal is malformed", e);
		}
	}

	private static Journal legacyJournal(JsonObject root) {
		Route route = parseRoute(object(root, "route"));
		boolean routeRequired = !root.has("routeRequired") || bool(root, "routeRequired");
		Map<UUID, StoredResult> results = parseResults(array(root, "results"));
		LinkedHashMap<String, RouteEntry> routes = new LinkedHashMap<>();
		routes.put(route.identity(), new RouteEntry(route, results, routeRequired));
		return new Journal(routes);
	}

	private static Journal currentJournal(JsonObject root) {
		JsonArray listedRoutes = array(root, "routes");
		if (listedRoutes.size() == 0) throw invalid();
		LinkedHashMap<String, RouteEntry> routes = new LinkedHashMap<>();
		for (JsonElement element : listedRoutes) {
			if (!element.isJsonObject()) throw invalid();
			JsonObject listed = element.getAsJsonObject();
			Route route = parseRoute(object(listed, "route"));
			boolean routeRequired = bool(listed, "routeRequired");
			Map<UUID, StoredResult> results = parseResults(array(listed, "results"));
			if (routes.size() >= MAX_ROUTES
					|| routes.put(route.identity(), new RouteEntry(route, results, routeRequired)) != null) throw invalid();
		}
		if (totalResults(routes) > MAX_RESULTS) throw invalid();
		return new Journal(routes);
	}

	private static int totalResults(Map<String, RouteEntry> routes) {
		long total = 0;
		for (RouteEntry entry : routes.values()) total += entry.results().size();
		return total > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) total;
	}

	private static Route parseRoute(JsonObject routeJson) {
		return new Route(string(routeJson, "nodeId"), string(routeJson, "displayName"),
				string(routeJson, "platform"), string(routeJson, "pluginVersion"),
				URI.create(string(routeJson, "endpoint")), string(routeJson, "credentialFile"),
				integer(routeJson, "heartbeatSeconds"), integer(routeJson, "connectTimeoutMillis"),
				integer(routeJson, "requestTimeoutMillis"));
	}

	private static Map<UUID, StoredResult> parseResults(JsonArray listed) {
		if (listed.size() == 0 || listed.size() > MAX_RESULTS) throw invalid();
		LinkedHashMap<UUID, StoredResult> results = new LinkedHashMap<>();
		for (JsonElement element : listed) {
			if (!element.isJsonObject()) throw invalid();
			JsonObject item = element.getAsJsonObject();
			UUID operationId = UUID.fromString(string(item, "operationId"));
			boolean committed = bool(item, "committed");
			boolean claimRequired = bool(item, "claimRequired");
			JsonObject result = object(item, "result");
			if (result.toString().getBytes(StandardCharsets.UTF_8).length > MAX_RESULT_BYTES) throw invalid();
			StoredResult previous = results.put(operationId,
					new StoredResult(result.deepCopy(), committed, claimRequired));
			if (previous != null) throw invalid();
		}
		return Collections.unmodifiableMap(new LinkedHashMap<>(results));
	}

	private static byte[] serialize(Map<String, RouteEntry> routes) {
		JsonObject root = new JsonObject();
		root.addProperty("version", VERSION);
		JsonArray listedRoutes = new JsonArray();
		for (RouteEntry entry : routes.values()) {
			JsonObject listed = new JsonObject();
			listed.add("route", routeJson(entry.route()));
			listed.addProperty("routeRequired", entry.routeRequired());
			JsonArray listedResults = new JsonArray();
			for (Map.Entry<UUID, StoredResult> resultEntry : entry.results().entrySet()) {
				UUID operationId = resultEntry.getKey();
				StoredResult result = resultEntry.getValue();
				if (operationId == null || result == null) throw invalid();
				JsonObject item = new JsonObject();
				item.addProperty("operationId", operationId.toString());
				item.add("result", result.result().deepCopy());
				item.addProperty("committed", result.committed());
				item.addProperty("claimRequired", result.claimRequired());
				listedResults.add(item);
			}
			listed.add("results", listedResults);
			if (listed.toString().getBytes(StandardCharsets.UTF_8).length > MAX_BYTES) throw invalid();
			listedRoutes.add(listed);
		}
		root.add("routes", listedRoutes);
		return root.toString().getBytes(StandardCharsets.UTF_8);
	}

	private static JsonObject routeJson(Route route) {
		JsonObject routeJson = new JsonObject();
		routeJson.addProperty("nodeId", route.nodeId());
		routeJson.addProperty("displayName", route.displayName());
		routeJson.addProperty("platform", route.platform());
		routeJson.addProperty("pluginVersion", route.pluginVersion());
		routeJson.addProperty("endpoint", route.endpoint().toString());
		routeJson.addProperty("credentialFile", route.credentialFile());
		routeJson.addProperty("heartbeatSeconds", route.heartbeatSeconds());
		routeJson.addProperty("connectTimeoutMillis", route.connectTimeoutMillis());
		routeJson.addProperty("requestTimeoutMillis", route.requestTimeoutMillis());
		return routeJson;
	}

	private static State state(RouteEntry entry) {
		return new State(entry.route(), entry.results(), entry.routeRequired());
	}

	private static Path target(Path dataDirectory) {
		return dataDirectory.toAbsolutePath().normalize().resolve(FILE_NAME);
	}

	private static JsonObject object(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonObject()) throw invalid();
		return object.getAsJsonObject(name);
	}

	private static JsonArray array(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonArray()) throw invalid();
		return object.getAsJsonArray(name);
	}

	private static String string(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()
				|| !object.getAsJsonPrimitive(name).isString()) throw invalid();
		String value = object.get(name).getAsString();
		if (value == null || value.isBlank() || value.length() > 2048) throw invalid();
		return value;
	}

	private static int integer(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()
				|| !object.getAsJsonPrimitive(name).isNumber()) throw invalid();
		return object.get(name).getAsInt();
	}

	private static boolean bool(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()
				|| !object.getAsJsonPrimitive(name).isBoolean()) throw invalid();
		return object.get(name).getAsBoolean();
	}

	private static IllegalArgumentException invalid() {
		return new IllegalArgumentException("invalid proxy-result journal");
	}

	private static String stableEndpoint(URI endpoint) {
		Objects.requireNonNull(endpoint, "endpoint");
		String scheme = endpoint.getScheme().toLowerCase(Locale.ROOT);
		String host = endpoint.getHost();
		if (host == null) return endpoint.normalize().toString();
		host = host.toLowerCase(Locale.ROOT);
		if (host.startsWith("[") && host.endsWith("]")) host = host.substring(1, host.length() - 1);
		if (host.indexOf(':') >= 0) host = "[" + host + "]";
		int port = endpoint.getPort();
		boolean defaultPort = ("http".equals(scheme) && port == 80) || ("https".equals(scheme) && port == 443);
		StringBuilder identity = new StringBuilder(scheme).append("://").append(host);
		if (port >= 0 && !defaultPort) identity.append(':').append(port);
		String path = endpoint.getRawPath();
		if (path != null && !path.isEmpty() && !"/".equals(path)) identity.append(path);
		if (endpoint.getRawQuery() != null) identity.append('?').append(endpoint.getRawQuery());
		if (endpoint.getRawFragment() != null) identity.append('#').append(endpoint.getRawFragment());
		return identity.toString();
	}

	record Route(String nodeId, String displayName, String platform, String pluginVersion, URI endpoint,
			String credentialFile, int heartbeatSeconds, int connectTimeoutMillis, int requestTimeoutMillis) {
		String identity() {
			return nodeId.trim() + "\n" + stableEndpoint(endpoint) + "\n"
					+ platform.trim().toUpperCase(Locale.ROOT);
		}
	}

	record StoredResult(JsonObject result, boolean committed, boolean claimRequired) {
		StoredResult {
			if (result == null || (committed && claimRequired)) {
				throw new IllegalArgumentException("invalid proxy-result state");
			}
		}
	}

	record State(Route route, Map<UUID, StoredResult> results, boolean routeRequired) { }

	private record RouteEntry(Route route, Map<UUID, StoredResult> results, boolean routeRequired) { }
	private record Journal(LinkedHashMap<String, RouteEntry> routes) { }
}
