package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}
