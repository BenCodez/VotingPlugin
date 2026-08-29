package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.control.BackendControlResultStore.StoredResult;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

class BackendControlConnectorProtocolTest {
	@Test void onlyTheLeaseExpiryConflictRequestsAResultReclaim() {
		assertTrue(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(409,
				"{\"error\":{\"code\":\"TASK_LEASE_EXPIRED\"}}")));
		assertFalse(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(409,
				"{\"error\":{\"code\":\"SESSION_MISMATCH\"}}")));
		assertFalse(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(500, "not-json")));
		assertTrue(BackendControlConnector.operationNotFound(new BackendControlConnector.Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}")));
		assertFalse(BackendControlConnector.operationNotFound(new BackendControlConnector.Response(404,
				"{\"error\":{\"code\":\"NODE_NOT_FOUND\"}}")));
	}

	@Test void abandonedBackendIntentBecomesATerminalRecoveryResult() {
		JsonObject anticipated = new JsonObject();
		anticipated.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");

		StoredResult recovered = BackendControlConnector.abortedIntent(
				new StoredResult(anticipated, true, false, false));

		assertTrue(recovered.committed());
		assertFalse(recovered.claimRequired());
		assertFalse(recovered.restartConnector());
		assertTrue("RECOVERY_ABORTED".equals(recovered.result().get("code").getAsString()));
	}

	@Test void registrationRequiresFileControlButAllowsQuickSetupToRemainOptional() {
		assertThrows(RuntimeException.class, () -> BackendControlConnector.requireFileCapability(false));
		assertDoesNotThrow(() -> BackendControlConnector.requireFileCapability(true));
	}

	@Test void registrationAdvertisesCommentPreservingFilesAsAnOptionalCapability() {
		JsonObject registration = new JsonObject();
		BackendControlConnector.addCapabilities(registration);

		JsonArray advertised = registration.getAsJsonArray("capabilities");
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.file-comments.v1".equals(value.getAsString())));
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.vote-sites-sync.v1".equals(value.getAsString())));
		JsonArray required = registration.getAsJsonArray("requiredCapabilities");
		assertTrue(required.asList().stream()
				.anyMatch(value -> "config.files.v1".equals(value.getAsString())));
		assertFalse(required.asList().stream()
				.anyMatch(value -> "config.file-comments.v1".equals(value.getAsString())));
	}

	@Test void heartbeatRetainsOmittedCapabilitiesAndHonorsExplicitReplacement() {
		JsonObject omitted = new JsonObject();
		assertTrue(BackendControlConnector.negotiatedCapability(omitted, "config.files.v1", true));

		JsonObject explicit = new JsonObject();
		JsonArray accepted = new JsonArray();
		accepted.add("config.quick-setup.v1");
		explicit.add("acceptedCapabilities", accepted);
		assertFalse(BackendControlConnector.negotiatedCapability(explicit, "config.files.v1", true));
		assertTrue(BackendControlConnector.negotiatedCapability(explicit, "config.quick-setup.v1", false));
	}

	@Test void voteSitesSyncRequiresBothNegotiatedCapabilities() {
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", true, false));
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", false, true));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", true, true));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("common-settings", true, false));
	}

	@Test void shutdownWaitsForTheClaimedBackendOperation() throws Exception {
		var executor = Executors.newSingleThreadScheduledExecutor();
		CompletableFuture<Void> operation = new CompletableFuture<>();
		CompletableFuture<Void> closing = CompletableFuture.runAsync(
				() -> BackendControlConnector.awaitShutdown(executor, operation));

		assertThrows(java.util.concurrent.TimeoutException.class, () -> closing.get(100, TimeUnit.MILLISECONDS));
		operation.complete(null);
		closing.get(2, TimeUnit.SECONDS);
		assertTrue(executor.isTerminated());
	}

	@Test void failedResultAcknowledgementDoesNotTriggerConnectorHandoff() throws Exception {
		AtomicBoolean handedOff = new AtomicBoolean();
		assertThrows(IOException.class, () -> BackendControlConnector.afterResultAcknowledged(
				() -> { throw new IOException("Control result was not acknowledged"); },
				() -> handedOff.set(true)));
		assertFalse(handedOff.get());

		BackendControlConnector.afterResultAcknowledged(() -> { }, () -> handedOff.set(true));
		assertTrue(handedOff.get());
	}
}
