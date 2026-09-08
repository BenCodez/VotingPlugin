package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;

class VotingPluginMainBackendProxyPublicationTest {
	@Test
	void publishesReplacementBeforeOpeningItsTransportGate() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		doAnswer(invocation -> {
			assertSame(replacement, plugin.getBackendProxyHandler(),
					"inbound callbacks must not open before the replacement is published");
			return null;
		}).when(replacement).activatePresenceReporting();

		plugin.publishBackendProxyHandler(null, replacement);

		assertSame(replacement, plugin.getBackendProxyHandler());
	}

	@Test
	void restoresPreviousHandlerWhenActivationFails() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		doAnswer(invocation -> { throw new IllegalStateException("activation failed"); })
				.when(replacement).activatePresenceReporting();

		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> plugin.publishBackendProxyHandler(previous, replacement));

		assertSame(previous, plugin.getBackendProxyHandler());
	}
}
