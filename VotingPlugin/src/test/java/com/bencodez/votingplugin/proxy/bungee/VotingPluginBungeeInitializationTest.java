package com.bencodez.votingplugin.proxy.bungee;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

class VotingPluginBungeeInitializationTest {
	@Test
	void freshInitializationDoesNotCreateADisposableRuntimeBeforeFullLoad() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		java.util.concurrent.atomic.AtomicInteger reloadCalls = new java.util.concurrent.atomic.AtomicInteger();
		doAnswer(invocation -> {
			assertNull(plugin.getVotingPluginProxy());
			reloadCalls.incrementAndGet();
			return null;
		}).when(plugin).reloadPlugin(true);

		plugin.initializeFirstRuntime();

		assertEquals(1, reloadCalls.get());
	}
}
