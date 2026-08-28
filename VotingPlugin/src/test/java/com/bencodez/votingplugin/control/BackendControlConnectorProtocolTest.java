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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

class BackendControlConnectorProtocolTest {
	@Test void registrationRequiresFileControlButAllowsQuickSetupToRemainOptional() {
		assertThrows(RuntimeException.class, () -> BackendControlConnector.requireFileCapability(false));
		assertDoesNotThrow(() -> BackendControlConnector.requireFileCapability(true));
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
