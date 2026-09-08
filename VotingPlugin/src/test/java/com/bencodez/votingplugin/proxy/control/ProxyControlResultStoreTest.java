package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
import com.google.gson.JsonParser;

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
		assertTrue(recovered.routeRequired());
		assertEquals("applied-revision", recovered.results().get(operationId).result().get("revision").getAsString());
		assertTrue(recovered.results().get(operationId).committed());
		ProxyControlResultStore.save(directory, route, Map.of());
		assertFalse(Files.exists(directory.resolve(".control-proxy-pending-results.json")));
	}

	@Test void completedRecoveryRetainsResultsWithoutPinningTheOldRoute() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("proxy-old", "Proxy Old", "VELOCITY", "7.1.2",
				URI.create("https://control.example:8443"), "old-credential.txt", 30, 3000, 5000);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);

		ProxyControlResultStore.save(directory, route,
				Map.of(operationId, new StoredResult(result, true, false)), false);
		ProxyControlResultStore.State recovered = ProxyControlResultStore.load(directory);

		assertFalse(recovered.routeRequired());
		assertTrue(recovered.results().containsKey(operationId));
	}

	@Test void releasedResultsStayBoundWhenTheConfiguredRouteChanges() throws Exception {
		UUID oldOperation = UUID.fromString("00000000-0000-0000-0000-000000000099");
		UUID newOperation = UUID.fromString("00000000-0000-0000-0000-000000000100");
		Route oldRoute = new Route("proxy-old", "Proxy Old", "VELOCITY", "7.1.2",
				URI.create("https://old-control.example:8443"), "old-credential.txt", 30, 3000, 5000);
		Route newRoute = new Route("proxy-new", "Proxy New", "VELOCITY", "7.1.2",
				URI.create("https://new-control.example:8443"), "new-credential.txt", 30, 3000, 5000);
		JsonObject oldResult = new JsonObject();
		oldResult.addProperty("success", true);
		JsonObject newResult = new JsonObject();
		newResult.addProperty("success", true);

		ProxyControlResultStore.save(directory, oldRoute,
				Map.of(oldOperation, new StoredResult(oldResult, true, false)), false);
		ProxyControlResultStore.save(directory, newRoute,
				Map.of(newOperation, new StoredResult(newResult, true, false)));

		assertEquals(oldRoute, ProxyControlResultStore.loadForRoute(directory, oldRoute).route(),
				"the released old route must remain durably bound");
		assertNull(ProxyControlResultStore.loadForRoute(directory, new Route("other", "Other", "VELOCITY", "7.1.2",
				URI.create("https://other-control.example:8443"), "other-credential.txt", 30, 3000, 5000)));
		ProxyControlResultStore.State current = ProxyControlResultStore.loadForRoute(directory, newRoute);
		assertNotNull(current);
		assertEquals(newRoute, current.route());
		assertTrue(current.results().containsKey(newOperation));

		ProxyControlResultStore.save(directory, newRoute, Map.of());
		assertNull(ProxyControlResultStore.loadForRoute(directory, newRoute));
		assertTrue(ProxyControlResultStore.loadForRoute(directory, oldRoute).results().containsKey(oldOperation),
				"acknowledging the new route must not remove the old route result");
	}

	@Test void stableRouteIdentityIgnoresVersionTimingAndCredentialRotation() throws Exception {
		Route original = new Route("proxy-stable", "Old display name", "VELOCITY", "7.1.2",
				URI.create("https://CONTROL.example:443"), "credential.txt", 30, 3000, 5000);
		Route updated = new Route("proxy-stable", "New display name", "VELOCITY", "7.2.0",
				URI.create("https://control.example"), "rotated-credential.txt", 120, 10000, 15000);
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000101");

		ProxyControlResultStore.save(directory, original,
				Map.of(operationId, new StoredResult(result(true), true, false)));
		ProxyControlResultStore.save(directory, updated,
				Map.of(operationId, new StoredResult(result(true), true, false)));

		ProxyControlResultStore.State recovered = ProxyControlResultStore.loadForRoute(directory, updated);
		assertNotNull(recovered);
		assertEquals(updated, recovered.route(), "metadata may refresh without changing route identity");
		assertEquals(1, recovered.results().size(), "runtime and credential changes must not split the coordinator");
		assertEquals(3, com.google.gson.JsonParser.parseString(
				Files.readString(directory.resolve(".control-proxy-pending-results.json")))
				.getAsJsonObject().get("version").getAsInt());
	}

	@Test void requiredOriginRoutesAreSelectedBeforeCurrentConfiguration() throws Exception {
		Route first = route("required-first");
		Route second = route("required-second");
		Route configured = route("configured");
		ProxyControlResultStore.save(directory, first,
				Map.of(UUID.randomUUID(), new StoredResult(result(true), true, false)), true);
		ProxyControlResultStore.save(directory, second,
				Map.of(UUID.randomUUID(), new StoredResult(result(true), true, false)), true);
		ProxyControlResultStore.save(directory, configured,
				Map.of(UUID.randomUUID(), new StoredResult(result(true), true, false)), false);

		assertEquals(first, ProxyControlResultStore.loadPreferred(directory, configured).route());
		ProxyControlResultStore.save(directory, first, Map.of());
		assertEquals(second, ProxyControlResultStore.loadPreferred(directory, configured).route());
		ProxyControlResultStore.save(directory, second, Map.of());
		assertEquals(configured, ProxyControlResultStore.loadPreferred(directory, configured).route());
	}

	@Test void globalResultLimitRejectsAnAdditionalRouteWithoutReplacingTheJournal() throws Exception {
		Route original = route("bounded-original");
		Map<UUID, StoredResult> existing = new LinkedHashMap<>();
		for (int index = 0; index < 128; index++) {
			existing.put(UUID.nameUUIDFromBytes(("operation-" + index).getBytes(java.nio.charset.StandardCharsets.UTF_8)),
					new StoredResult(result(true), true, false));
		}
		ProxyControlResultStore.save(directory, original, existing);
		Route additional = route("bounded-additional");
		UUID additionalId = UUID.fromString("00000000-0000-0000-0000-000000000102");
		assertThrows(java.io.IOException.class, () -> ProxyControlResultStore.save(directory, additional,
				Map.of(additionalId, new StoredResult(result(true), true, false))));
		assertEquals(128, ProxyControlResultStore.loadForRoute(directory, original).results().size());
		assertNull(ProxyControlResultStore.loadForRoute(directory, additional));
	}

	@Test void currentJournalRejectsAggregateResultsAboveTheGlobalLimit() throws Exception {
		Route original = route("aggregate-load-original");
		Map<UUID, StoredResult> existing = new LinkedHashMap<>();
		for (int index = 0; index < 65; index++) {
			existing.put(UUID.nameUUIDFromBytes(("aggregate-operation-" + index)
					.getBytes(java.nio.charset.StandardCharsets.UTF_8)), new StoredResult(result(true), true, false));
		}
		ProxyControlResultStore.save(directory, original, existing);
		Path journal = directory.resolve(".control-proxy-pending-results.json");
		JsonObject root = JsonParser.parseString(Files.readString(journal)).getAsJsonObject();
		JsonObject duplicate = root.getAsJsonArray("routes").get(0).getAsJsonObject().deepCopy();
		duplicate.getAsJsonObject("route").addProperty("nodeId", "aggregate-load-copy");
		duplicate.getAsJsonObject("route").addProperty("endpoint", "https://aggregate-copy.example:8443");
		root.getAsJsonArray("routes").add(duplicate);
		Files.writeString(journal, root.toString());

		assertThrows(java.io.IOException.class, () -> ProxyControlResultStore.load(directory));
	}

	@Test void releasedRouteByteLimitDoesNotStarveAnotherRoute() throws Exception {
		Route original = route("bytes-original");
		JsonObject large = result(true);
		large.addProperty("payload", "x".repeat(3_950_000));
		UUID originalOperation = UUID.fromString("00000000-0000-0000-0000-000000000103");
		ProxyControlResultStore.save(directory, original,
				Map.of(originalOperation, new StoredResult(large, true, false)));

		Route additional = route("bytes-additional");
		JsonObject extra = result(true);
		extra.addProperty("payload", "y".repeat(300_000));
		UUID additionalOperation = UUID.fromString("00000000-0000-0000-0000-000000000104");
		ProxyControlResultStore.save(directory, additional,
				Map.of(additionalOperation, new StoredResult(extra, true, false)));
		assertTrue(ProxyControlResultStore.loadForRoute(directory, original).results().containsKey(originalOperation));
		assertTrue(ProxyControlResultStore.loadForRoute(directory, additional).results().containsKey(additionalOperation));
	}

	@Test void legacyV2JournalLoadsByStableIdentityAndMigratesOnNextSave() throws Exception {
		Route legacy = route("legacy");
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000105");
		String legacyJson = "{\"version\":2,\"routeRequired\":true,\"route\":{"
				+ "\"nodeId\":\"legacy\",\"displayName\":\"Proxy legacy\",\"platform\":\"VELOCITY\","
				+ "\"pluginVersion\":\"7.1.2\",\"endpoint\":\"https://legacy.control.example:8443\","
				+ "\"credentialFile\":\"legacy-credential.txt\",\"heartbeatSeconds\":30,"
				+ "\"connectTimeoutMillis\":3000,\"requestTimeoutMillis\":5000},\"results\":[{"
				+ "\"operationId\":\"" + operationId + "\",\"result\":{\"success\":true},"
				+ "\"committed\":true,\"claimRequired\":false}]}";
		Files.writeString(directory.resolve(".control-proxy-pending-results.json"), legacyJson);

		ProxyControlResultStore.State recovered = ProxyControlResultStore.loadForRoute(directory, legacy);
		assertNotNull(recovered);
		assertTrue(recovered.results().containsKey(operationId));
		ProxyControlResultStore.save(directory, legacy, recovered.results(), recovered.routeRequired());
		JsonObject migrated = com.google.gson.JsonParser.parseString(
				Files.readString(directory.resolve(".control-proxy-pending-results.json"))).getAsJsonObject();
		assertEquals(3, migrated.get("version").getAsInt());
		assertEquals(1, migrated.getAsJsonArray("routes").size());
	}

	private static JsonObject result(boolean success) {
		JsonObject result = new JsonObject();
		result.addProperty("success", success);
		return result;
	}

	private static Route route(String nodeId) {
		return new Route(nodeId, "Proxy " + nodeId, "VELOCITY", "7.1.2",
				URI.create("https://" + nodeId + ".control.example:8443"), nodeId + "-credential.txt",
				30, 3000, 5000);
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
