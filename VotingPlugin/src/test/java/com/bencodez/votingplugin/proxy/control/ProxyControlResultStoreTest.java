package com.bencodez.votingplugin.proxy.control;

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

import com.bencodez.votingplugin.proxy.control.ProxyControlResultStore.Route;
import com.bencodez.votingplugin.proxy.control.ProxyControlResultStore.StoredResult;
import com.google.gson.JsonObject;

class ProxyControlResultStoreTest {
	@TempDir Path directory;

	@Test void proxyResultAndOriginSurviveRestartUntilControlAcknowledgesThem() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("proxy-old", "Proxy Old", "VELOCITY", "7.1.2",
				URI.create("https://control.example:8443"), "old-credential.txt", 30, 3000, 5000);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("revision", "applied-revision");
		Map<UUID, StoredResult> pending = new LinkedHashMap<>();
		pending.put(operationId, new StoredResult(result, true, false));

		ProxyControlResultStore.save(directory, route, pending);
		ProxyControlResultStore.State recovered = ProxyControlResultStore.load(directory);

		assertEquals(route, recovered.route());
		assertEquals("applied-revision", recovered.results().get(operationId).result().get("revision").getAsString());
		assertTrue(recovered.results().get(operationId).committed());
		ProxyControlResultStore.save(directory, route, Map.of());
		assertFalse(Files.exists(directory.resolve(".control-proxy-pending-results.json")));
	}

	@Test void writeAheadIntentRetainsItsUncommittedStateAcrossRestart() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("proxy-old", "Proxy Old", "VELOCITY", "7.1.2",
				URI.create("https://control.example:8443"), "old-credential.txt", 30, 3000, 5000);
		JsonObject result = new JsonObject();
		result.addProperty("revision", "anticipated-revision");

		ProxyControlResultStore.save(directory, route, Map.of(operationId, new StoredResult(result, false, false)));

		StoredResult recovered = ProxyControlResultStore.load(directory).results().get(operationId);
		assertFalse(recovered.committed());
		assertFalse(recovered.claimRequired());
	}

	@Test void managedFileReadLargerThanLegacyJournalLimitSurvivesRestart() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("proxy-old", "Proxy Old", "VELOCITY", "7.1.2",
				URI.create("https://control.example:8443"), "old-credential.txt", 30, 3000, 5000);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", ProxyConfigurationFileService.FILE_NAME);
		configuration.addProperty("content", "a".repeat(300 * 1024));
		result.add("configuration", configuration);

		ProxyControlResultStore.save(directory, route,
				Map.of(operationId, new StoredResult(result, true, false)));
		ProxyControlResultStore.State recovered = ProxyControlResultStore.load(directory);

		assertEquals(300 * 1024, recovered.results().get(operationId).result()
				.getAsJsonObject("configuration").get("content").getAsString().length());
		assertTrue(Files.size(directory.resolve(".control-proxy-pending-results.json")) > 256 * 1024);
	}

	@Test void symbolicProxyResultJournalIsRejected() throws Exception {
		Path external = directory.resolve("external.json");
		Files.writeString(external, "{}");
		Files.createSymbolicLink(directory.resolve(".control-proxy-pending-results.json"), external);

		assertThrows(java.io.IOException.class, () -> ProxyControlResultStore.load(directory));
	}
}
