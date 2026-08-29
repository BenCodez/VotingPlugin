package com.bencodez.votingplugin.control;

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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.bencodez.votingplugin.util.DurableFiles;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Durable result journal used to make locally applied Control operations restart-safe. */
final class BackendControlResultStore {
	private static final int VERSION = 3;
	private static final int LEGACY_VERSION = 2;
	// A valid 512 KiB YAML result may expand up to sixfold when characters require JSON unicode escaping.
	private static final int MAX_BYTES = 4 * 1024 * 1024;
	private static final int MAX_RESULTS = 128;
	private static final String FILE_NAME = ".control-pending-results.json";

	private BackendControlResultStore() { }

	static State load(Path dataDirectory) throws IOException {
		Path target = target(dataDirectory);
		if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)
				|| Files.size(target) > MAX_BYTES) {
			throw new IOException("Control pending-result journal is unsafe or too large");
		}
		byte[] bytes;
		try (SeekableByteChannel channel = Files.newByteChannel(target,
				Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
			ByteBuffer buffer = ByteBuffer.allocate((int) Files.size(target) + 1);
			while (channel.read(buffer) >= 0 && buffer.hasRemaining()) { }
			if (!buffer.hasRemaining()) throw new IOException("Control pending-result journal is too large");
			buffer.flip();
			bytes = new byte[buffer.remaining()];
			buffer.get(bytes);
		}
		try {
			JsonElement parsed = JsonParser.parseString(new String(bytes, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw invalid();
			JsonObject root = parsed.getAsJsonObject();
			int version = integer(root, "version");
			if (version != VERSION && version != LEGACY_VERSION) throw invalid();
			JsonObject routeJson = object(root, "route");
			Route route = new Route(string(routeJson, "nodeId"), URI.create(string(routeJson, "endpoint")),
					string(routeJson, "credentialFile"), integer(routeJson, "heartbeatSeconds"),
					integer(routeJson, "connectTimeoutMillis"), integer(routeJson, "requestTimeoutMillis"));
			HostConfiguration hosted = version == LEGACY_VERSION ? legacyHostedConfiguration()
					: hostedConfiguration(object(root, "hosted"));
			JsonArray listed = array(root, "results");
			if (listed.size() == 0 || listed.size() > MAX_RESULTS) throw invalid();
			Map<UUID, StoredResult> results = new LinkedHashMap<>();
			for (JsonElement element : listed) {
				if (!element.isJsonObject()) throw invalid();
				JsonObject item = element.getAsJsonObject();
				UUID operationId = UUID.fromString(string(item, "operationId"));
				JsonObject result = object(item, "result").deepCopy();
				if (!item.has("restartConnector") || !item.get("restartConnector").isJsonPrimitive()
						|| !item.has("committed") || !item.get("committed").isJsonPrimitive()
						|| !item.has("claimRequired") || !item.get("claimRequired").isJsonPrimitive()) throw invalid();
				StoredResult previous = results.put(operationId,
						new StoredResult(result, item.get("restartConnector").getAsBoolean(),
								item.get("committed").getAsBoolean(), item.get("claimRequired").getAsBoolean()));
				if (previous != null) throw invalid();
			}
			return new State(route, hosted, Map.copyOf(results));
		} catch (RuntimeException e) {
			throw new IOException("Control pending-result journal is malformed", e);
		}
	}

	static void save(Path dataDirectory, Route route, HostConfiguration hosted,
			Map<UUID, StoredResult> results) throws IOException {
		Path target = target(dataDirectory);
		if (results.isEmpty()) {
			if (Files.isSymbolicLink(target)) throw new IOException("Control pending-result journal is unsafe");
			DurableFiles.deleteIfExists(target);
			return;
		}
		if (results.size() > MAX_RESULTS) throw new IOException("Too many pending Control results");
		if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)
				&& (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target))) {
			throw new IOException("Control pending-result journal is unsafe");
		}
		JsonObject root = new JsonObject();
		root.addProperty("version", VERSION);
		JsonObject routeJson = new JsonObject();
		routeJson.addProperty("nodeId", route.nodeId());
		routeJson.addProperty("endpoint", route.endpoint().toString());
		routeJson.addProperty("credentialFile", route.credentialFile());
		routeJson.addProperty("heartbeatSeconds", route.heartbeatSeconds());
		routeJson.addProperty("connectTimeoutMillis", route.connectTimeoutMillis());
		routeJson.addProperty("requestTimeoutMillis", route.requestTimeoutMillis());
		root.add("route", routeJson);
		JsonObject hostedJson = new JsonObject();
		hostedJson.addProperty("enabled", hosted.enabled());
		hostedJson.addProperty("autoDownload", hosted.autoDownload());
		hostedJson.addProperty("autoUpdate", hosted.autoUpdate());
		hostedJson.addProperty("downloadUrl", hosted.downloadUrl());
		hostedJson.addProperty("sha256", hosted.sha256());
		hostedJson.addProperty("jarFile", hosted.jarFile());
		hostedJson.addProperty("dataDirectory", hosted.dataDirectory());
		hostedJson.addProperty("host", hosted.host());
		hostedJson.addProperty("port", hosted.port());
		hostedJson.addProperty("startupTimeoutSeconds", hosted.startupTimeoutSeconds());
		hostedJson.addProperty("downloadTimeoutSeconds", hosted.downloadTimeoutSeconds());
		root.add("hosted", hostedJson);
		JsonArray listed = new JsonArray();
		results.forEach((operationId, result) -> {
			JsonObject item = new JsonObject();
			item.addProperty("operationId", operationId.toString());
			item.add("result", result.result().deepCopy());
			item.addProperty("restartConnector", result.restartConnector());
			item.addProperty("committed", result.committed());
			item.addProperty("claimRequired", result.claimRequired());
			listed.add(item);
		});
		root.add("results", listed);
		byte[] bytes = root.toString().getBytes(StandardCharsets.UTF_8);
		if (bytes.length > MAX_BYTES) throw new IOException("Control pending-result journal is too large");
		Files.createDirectories(dataDirectory);
		Path staging = Files.createTempFile(dataDirectory, ".control-results-", ".json");
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
		if (!object.has(name) || !object.get(name).isJsonPrimitive()) throw invalid();
		String value = object.get(name).getAsString();
		if (value == null || value.isBlank() || value.length() > 2048) throw invalid();
		return value;
	}

	private static String optionalString(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()) throw invalid();
		String value = object.get(name).getAsString();
		if (value == null || value.length() > 2048) throw invalid();
		return value;
	}

	private static boolean bool(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()) throw invalid();
		return object.get(name).getAsBoolean();
	}

	private static HostConfiguration hostedConfiguration(JsonObject hosted) {
		return new HostConfiguration(bool(hosted, "enabled"), bool(hosted, "autoDownload"),
				bool(hosted, "autoUpdate"), optionalString(hosted, "downloadUrl"),
				optionalString(hosted, "sha256"), string(hosted, "jarFile"),
				string(hosted, "dataDirectory"), string(hosted, "host"), integer(hosted, "port"),
				integer(hosted, "startupTimeoutSeconds"), integer(hosted, "downloadTimeoutSeconds"));
	}

	private static HostConfiguration legacyHostedConfiguration() {
		// Version 2 predates Bukkit-hosted Control, so recovery must keep hosting disabled
		// until the old connector route has submitted its pending result.
		return new HostConfiguration(false, false, false, "", "", "control/votingplugin-control.jar",
				"control/data", "127.0.0.1", 8080, 30, 60);
	}

	private static int integer(JsonObject object, String name) {
		if (!object.has(name) || !object.get(name).isJsonPrimitive()) throw invalid();
		return object.get(name).getAsInt();
	}

	private static IllegalArgumentException invalid() {
		return new IllegalArgumentException("invalid pending-result journal");
	}

	record Route(String nodeId, URI endpoint, String credentialFile, int heartbeatSeconds,
			int connectTimeoutMillis, int requestTimeoutMillis) { }
	record StoredResult(JsonObject result, boolean restartConnector, boolean committed, boolean claimRequired) {
		StoredResult {
			if (committed && claimRequired) throw new IllegalArgumentException("committed result cannot require a claim");
		}
	}
	record State(Route route, HostConfiguration hostedConfiguration, Map<UUID, StoredResult> results) { }
}
