package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.BungeeMethod;

public class BungeeMethodTest {
	@Test
	public void pluginMessagingUsesProxyPresenceInsteadOfBackendPresence() {
		assertTrue(BungeeMethod.PLUGINMESSAGING.requiresPlayerOnline());
		assertFalse(BungeeMethod.PLUGINMESSAGING.supportsBackendPresence());
	}

	@Test
	public void standaloneTransportsSupportBackendPresence() {
		for (BungeeMethod method : BungeeMethod.values()) {
			if (method != BungeeMethod.PLUGINMESSAGING) {
				assertTrue(method.supportsBackendPresence(), method.toString());
			}
		}
	}
}
