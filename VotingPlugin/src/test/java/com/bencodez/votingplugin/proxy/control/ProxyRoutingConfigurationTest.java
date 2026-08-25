package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import org.junit.jupiter.api.Test;

class ProxyRoutingConfigurationTest {
	@Test void revisionIsStableAcrossBlockedServerCaseAndOrder() {
		ProxyRoutingConfiguration first = new ProxyRoutingConfiguration(true, List.of("Lobby", "survival"));
		ProxyRoutingConfiguration second = new ProxyRoutingConfiguration(true, List.of("SURVIVAL", "lobby"));
		assertEquals(first.revision(), second.revision());
	}

	@Test void rejectsDuplicateBlankAndOversizedServerLists() {
		assertThrows(IllegalArgumentException.class,
				() -> new ProxyRoutingConfiguration(true, List.of("Lobby", "lobby")));
		assertThrows(IllegalArgumentException.class,
				() -> new ProxyRoutingConfiguration(true, List.of(" ")));
		assertThrows(IllegalArgumentException.class,
				() -> new ProxyRoutingConfiguration(true, java.util.Collections.nCopies(257, "lobby")));
	}
}
