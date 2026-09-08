package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;

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

	@Test
	void keepsPreparedHttpQueueWithPreviousHandlerUntilPublicationSucceeds() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		doAnswer(invocation -> { throw new IllegalStateException("activation failed"); })
				.when(replacement).activatePresenceReporting();
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);

		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> plugin.completeBackendProxyHandlerRestart(restart));

		verify(previous, never()).completeHttpHandoff(replacement);
		verify(previous, never()).completeRedisHandoff(replacement);
		assertSame(previous, plugin.getBackendProxyHandler());
	}

	@Test
	void restoresPreviousHandlerWhenRedisPromotionFails() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		doAnswer(invocation -> { throw new IllegalStateException("promotion failed"); })
				.when(previous).completeRedisHandoff(replacement);

		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> plugin.completeBackendProxyHandlerRestart(restart(previous, replacement)));

		assertSame(previous, plugin.getBackendProxyHandler());
		verify(previous, never()).completeHttpHandoff(replacement);
		verify(previous, never()).close();
	}

	private VotingPluginMain.BackendProxyRestart restart(BackendProxyHandler previous,
			BackendProxyHandler replacement) throws Exception {
		Constructor<VotingPluginMain.BackendProxyRestart> constructor = VotingPluginMain.BackendProxyRestart.class
				.getDeclaredConstructor(BackendProxyHandler.class, BackendProxyHandler.class, boolean.class, boolean.class);
		constructor.setAccessible(true);
		return constructor.newInstance(previous, replacement, false, true);
	}

	private void setBackendProxyHandler(VotingPluginMain plugin, BackendProxyHandler handler) throws Exception {
		Field field = VotingPluginMain.class.getDeclaredField("backendProxyHandler");
		field.setAccessible(true);
		field.set(plugin, handler);
	}
}
