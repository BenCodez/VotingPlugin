package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.nio.file.Path;

import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;

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
		verify(previous).validateReplacementTransportSecurity();
	}

	@Test
	void invalidRequiredAuthenticationLeavesExistingRuntimeActive(@TempDir Path dataDirectory) {
		VotingPluginProxyTestImpl previous = org.mockito.Mockito.spy(new VotingPluginProxyTestImpl());
		previous.setDataFolder(dataDirectory.toFile());
		when(previous.getConfig().getBungeeMethod()).thenReturn("REDIS");
		when(previous.getConfig().getSharedTransportAuthentication()).thenReturn("REQUIRED");

		assertThrows(IllegalStateException.class, () -> ProxyRuntimeReplacementLifecycle.prepare(previous));
		verify(previous, never()).prepareForRuntimeReplacement();
		verify(previous, never()).completeRuntimeReplacementShutdown();

		when(previous.getConfig().getSharedTransportAuthentication()).thenReturn("INVALID");
		assertThrows(IllegalArgumentException.class, () -> ProxyRuntimeReplacementLifecycle.prepare(previous));
		verify(previous, never()).prepareForRuntimeReplacement();
	}
}
