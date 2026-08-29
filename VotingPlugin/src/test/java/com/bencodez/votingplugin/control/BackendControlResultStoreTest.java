package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.control.BackendControlResultStore.Route;
import com.bencodez.votingplugin.control.BackendControlResultStore.StoredResult;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.google.gson.JsonObject;

class BackendControlResultStoreTest {
	@TempDir Path directory;
	private final HostConfiguration hosted = new HostConfiguration(true, false, false, "", "0".repeat(64),
			"control/control.jar", "control/data", "127.0.0.1", 8080, 30, 60);

	@Test void pendingResultAndOriginalRouteSurviveRestartUntilAcknowledged() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("backend-old", URI.create("https://control.example:8443"), "old-credential.txt",
				30, 3000, 10000);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("revision", "applied-revision");
		Map<UUID, StoredResult> pending = new LinkedHashMap<>();
		pending.put(operationId, new StoredResult(result, true, true, false));

		BackendControlResultStore.save(directory, route, hosted, pending);
		BackendControlResultStore.State recovered = BackendControlResultStore.load(directory);

		assertEquals(route, recovered.route());
		assertEquals(hosted, recovered.hostedConfiguration());
		assertEquals("applied-revision", recovered.results().get(operationId).result().get("revision").getAsString());
		assertTrue(recovered.results().get(operationId).committed());
		BackendControlResultStore.save(directory, route, hosted, Map.of());
		assertFalse(Files.exists(directory.resolve(".control-pending-results.json")));
	}

	@Test void writeAheadIntentRetainsItsUncommittedStateAcrossRestart() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("backend-old", URI.create("https://control.example:8443"), "old-credential.txt",
				30, 3000, 10000);
		JsonObject result = new JsonObject();
		result.addProperty("revision", "anticipated-revision");

		BackendControlResultStore.save(directory, route, hosted,
				Map.of(operationId, new StoredResult(result, false, false, false)));

		StoredResult recovered = BackendControlResultStore.load(directory).results().get(operationId);
		assertFalse(recovered.committed());
		assertFalse(recovered.claimRequired());
	}

	@Test void versionTwoJournalRecoversWithBackendHostingDisabled() throws Exception {
		JsonObject root = new JsonObject();
		root.addProperty("version", 2);
		JsonObject route = new JsonObject();
		route.addProperty("nodeId", "backend-old");
		route.addProperty("endpoint", "https://control.example:8443");
		route.addProperty("credentialFile", "old-credential.txt");
		route.addProperty("heartbeatSeconds", 30);
		route.addProperty("connectTimeoutMillis", 3000);
		route.addProperty("requestTimeoutMillis", 10000);
		root.add("route", route);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		JsonObject pending = new JsonObject();
		pending.addProperty("operationId", "00000000-0000-0000-0000-000000000099");
		pending.add("result", result);
		pending.addProperty("restartConnector", true);
		pending.addProperty("committed", true);
		pending.addProperty("claimRequired", false);
		com.google.gson.JsonArray results = new com.google.gson.JsonArray();
		results.add(pending);
		root.add("results", results);
		Files.writeString(directory.resolve(".control-pending-results.json"), root.toString());

		BackendControlResultStore.State recovered = BackendControlResultStore.load(directory);

		assertEquals(URI.create("https://control.example:8443"), recovered.route().endpoint());
		assertFalse(recovered.hostedConfiguration().enabled());
		assertTrue(recovered.results().values().iterator().next().committed());
	}

	@Test void symbolicPendingResultJournalIsRejected() throws Exception {
		Path external = directory.resolve("external.json");
		Files.writeString(external, "{}");
		Files.createSymbolicLink(directory.resolve(".control-pending-results.json"), external);

		assertThrows(java.io.IOException.class, () -> BackendControlResultStore.load(directory));
	}

	@Test void maximumEscapeHeavyConfigurationFitsTheEncodedJournal() throws Exception {
		Route route = new Route("backend-old", URI.create("https://control.example:8443"), "credential.txt",
				30, 3000, 10000);
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", "Config.yml");
		configuration.addProperty("content", "\\".repeat(BackendConfigurationService.MAX_CONTENT_BYTES));
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.add("configuration", configuration);

		BackendControlResultStore.save(directory, route, hosted,
				Map.of(UUID.fromString("00000000-0000-0000-0000-000000000099"),
						new StoredResult(result, false, true, false)));

		assertEquals(BackendConfigurationService.MAX_CONTENT_BYTES,
				BackendControlResultStore.load(directory).results().values().iterator().next().result()
						.getAsJsonObject("configuration").get("content").getAsString().length());
	}
}
