package com.bencodez.votingplugin.servicesites;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ServiceSiteHandlerLimitsTest {
	@Test
	void acceptsNormalEntries() {
		assertTrue(ServiceSiteHandler.isSafeEntry("PlanetMinecraft", "planetminecraft.com"));
	}

	@Test
	void rejectsOversizedRemoteEntries() {
		assertFalse(ServiceSiteHandler.isSafeEntry("x".repeat(ServiceSiteHandler.MAX_KEY_LENGTH + 1), "example.com"));
		assertFalse(ServiceSiteHandler.isSafeEntry("Example", "x".repeat(ServiceSiteHandler.MAX_VALUE_LENGTH + 1)));
	}
}
