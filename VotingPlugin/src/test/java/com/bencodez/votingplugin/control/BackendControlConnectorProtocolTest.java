package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class BackendControlConnectorProtocolTest {
	@Test void registrationRequiresFileControlButAllowsQuickSetupToRemainOptional() {
		assertThrows(RuntimeException.class, () -> BackendControlConnector.requireFileCapability(false));
		assertDoesNotThrow(() -> BackendControlConnector.requireFileCapability(true));
	}
}
