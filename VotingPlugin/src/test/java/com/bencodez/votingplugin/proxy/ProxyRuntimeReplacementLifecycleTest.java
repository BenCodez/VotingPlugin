package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.Test;

class ProxyRuntimeReplacementLifecycleTest {
	@Test
	void freshBungeeInitializationHasNoRuntimeToTearDown() {
		VotingPluginProxy absentRuntime = null;

		assertFalse(ProxyRuntimeReplacementLifecycle.prepare(absentRuntime));
		ProxyRuntimeReplacementLifecycle.complete(absentRuntime);
	}

	@Test
	void freshVelocityInitializationHasNoRuntimeToTearDown() {
		VotingPluginProxy unrelatedRuntime = mock(VotingPluginProxy.class);
		VotingPluginProxy absentRuntime = null;

		assertFalse(ProxyRuntimeReplacementLifecycle.prepare(absentRuntime));
		ProxyRuntimeReplacementLifecycle.complete(absentRuntime);
		verifyNoInteractions(unrelatedRuntime);
	}

	@Test
	void fullReloadCleansLoadedRuntimeExactlyOnce() {
		VotingPluginProxy previous = mock(VotingPluginProxy.class);

		assertTrue(ProxyRuntimeReplacementLifecycle.prepare(previous));
		ProxyRuntimeReplacementLifecycle.complete(previous);

		verify(previous).prepareForRuntimeReplacement();
		verify(previous).completeRuntimeReplacementShutdown();
	}
}
