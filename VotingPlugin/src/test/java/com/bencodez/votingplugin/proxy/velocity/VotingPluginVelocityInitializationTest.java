package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Path;

import org.bstats.velocity.Metrics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.slf4j.Logger;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.velocitypowered.api.event.EventManager;
import com.velocitypowered.api.proxy.ProxyServer;

class VotingPluginVelocityInitializationTest {
	@Test
	void freshInitializationDoesNotCreateADisposableRuntimeBeforeFullLoad(@TempDir Path dataDirectory) {
		TestVelocity plugin = new TestVelocity(dataDirectory);
		try {
			plugin.initializeFirstRuntime();

			assertEquals(1, plugin.reloadCalls);
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}


	@Test
	void failedInitialReloadLeavesRuntimeNonOperational(@TempDir Path dataDirectory) {
		TestVelocity plugin = new TestVelocity(dataDirectory);
		try {
			plugin.initializeFirstRuntime();
			assertFalse(plugin.isRuntimeOperational());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void votifierRegistrationFailureDoesNotReportReady(@TempDir Path dataDirectory) throws Exception {
		ProxyServer server = mock(ProxyServer.class);
		Logger logger = mock(Logger.class);
		VotingPluginVelocity plugin =
				new VotingPluginVelocity(server, logger, mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		when(runtime.isVotifierEnabled()).thenReturn(true);
		EventManager eventManager = mock(EventManager.class);
		when(server.getEventManager()).thenReturn(eventManager);
		doThrow(new RuntimeException("registration failed")).when(eventManager)
				.register(eq(plugin), any(VoteEventVelocity.class));
		java.lang.reflect.Field runtimeField = VotingPluginVelocity.class.getDeclaredField("votingPluginProxy");
		runtimeField.setAccessible(true);
		runtimeField.set(plugin, runtime);
		try {
			assertFalse(plugin.initVotifierListenerIfNeeded());
			assertFalse(plugin.isRuntimeOperational());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	private static final class TestVelocity extends VotingPluginVelocity {
		private int reloadCalls;

		private TestVelocity(Path dataDirectory) {
			super(mock(ProxyServer.class), mock(Logger.class), mock(Metrics.Factory.class), dataDirectory);
		}

		@Override
		public void reloadAllInternal(boolean loadMysql) {
			assertNull(getVotingPluginProxy());
			reloadCalls++;
		}
	}
}
