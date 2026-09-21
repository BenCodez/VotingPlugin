package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.nio.file.Path;

import org.bstats.velocity.Metrics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.slf4j.Logger;

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
