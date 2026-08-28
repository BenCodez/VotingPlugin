package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
import com.google.gson.JsonObject;

class BackendControlResultStoreTest {
	@TempDir Path directory;

	@Test void pendingResultAndOriginalRouteSurviveRestartUntilAcknowledged() throws Exception {
		UUID operationId = UUID.fromString("00000000-0000-0000-0000-000000000099");
		Route route = new Route("backend-old", URI.create("https://control.example:8443"), "old-credential.txt",
				30, 3000, 10000);
		JsonObject result = new JsonObject();
		result.addProperty("success", true);
		result.addProperty("revision", "applied-revision");
		Map<UUID, StoredResult> pending = new LinkedHashMap<>();
		pending.put(operationId, new StoredResult(result, true));

		BackendControlResultStore.save(directory, route, pending);
		BackendControlResultStore.State recovered = BackendControlResultStore.load(directory);

		assertEquals(route, recovered.route());
		assertEquals("applied-revision", recovered.results().get(operationId).result().get("revision").getAsString());
		BackendControlResultStore.save(directory, route, Map.of());
		assertFalse(Files.exists(directory.resolve(".control-pending-results.json")));
	}

	@Test void symbolicPendingResultJournalIsRejected() throws Exception {
		Path external = directory.resolve("external.json");
		Files.writeString(external, "{}");
		Files.createSymbolicLink(directory.resolve(".control-pending-results.json"), external);

		assertThrows(java.io.IOException.class, () -> BackendControlResultStore.load(directory));
	}
}
