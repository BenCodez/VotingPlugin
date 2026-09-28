package com.bencodez.votingplugin.proxy.bungee;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;

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

	@Test
	void failedInitialReloadLeavesRuntimeNonOperational() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		doAnswer(invocation -> null).when(plugin).reloadPlugin(true);

		plugin.initializeFirstRuntime();

		assertFalse(plugin.isRuntimeOperational());
	}

	@Test
	void votifierRegistrationFailureDoesNotReportReady() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.plugin.PluginManager manager =
				mock(net.md_5.bungee.api.plugin.PluginManager.class);
		BungeeConfig config = mock(BungeeConfig.class);
		when(plugin.getVotingPluginProxy()).thenReturn(runtime);
		when(runtime.isVotifierEnabled()).thenReturn(true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getPluginManager()).thenReturn(manager);
		when(plugin.getConfig()).thenReturn(config);
		when(config.getDebug()).thenReturn(false);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		doThrow(new RuntimeException("registration failed")).when(manager)
				.registerListener(eq(plugin), any(VoteEventBungee.class));

		assertFalse(plugin.initVotifierListenerIfNeeded());
		assertFalse(plugin.isRuntimeOperational());
	}
}
