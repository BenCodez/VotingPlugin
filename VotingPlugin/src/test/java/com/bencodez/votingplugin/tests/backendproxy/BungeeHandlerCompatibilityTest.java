package com.bencodez.votingplugin.tests.backendproxy;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.BungeeHandler;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;

public class BungeeHandlerCompatibilityTest {

	@Test
	public void testDeprecatedCompatibilityAlias() {
		assertTrue(BackendProxyHandler.class.isAssignableFrom(BungeeHandler.class));
		assertTrue(BungeeHandler.class.isAnnotationPresent(Deprecated.class));
	}
}
