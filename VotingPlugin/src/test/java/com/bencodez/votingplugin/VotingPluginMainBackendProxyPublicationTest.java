package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;

import org.bukkit.configuration.file.FileConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.proxy.BungeeMethod;

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
		verify(replacement).abortStagedInboundTo(previous);
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
		org.mockito.InOrder rollback = org.mockito.Mockito.inOrder(replacement, previous);
		rollback.verify(replacement).abortStagedInboundTo(previous);
		rollback.verify(replacement).close();
		rollback.verify(previous).refreshPresenceAfterFailedReplacement();
	}

	@Test
	void restoresPreviousHandlerWhenPreparedDeliveryAdmissionFails() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		doThrow(new IllegalStateException("handoff admission failed"))
				.when(previous).completeHttpHandoff(replacement);

		assertThrows(IllegalStateException.class,
				() -> plugin.completeBackendProxyHandlerRestart(restart(previous, replacement)));

		assertSame(previous, plugin.getBackendProxyHandler());
		verify(replacement).close();
		verify(previous).restoreAfterFailedReplacement();
		verify(previous).refreshPresenceAfterFailedReplacement();
		verify(previous, never()).close();
	}

	@Test
	void predecessorCleanupFailureDoesNotRollBackPublishedReplacement() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
		Config config = mock(Config.class);
		when(config.getData()).thenReturn(mock(FileConfiguration.class));
		when(plugin.getConfigFile()).thenReturn(config);
		doAnswer(invocation -> null).when(plugin).debug(org.mockito.ArgumentMatchers.any(Throwable.class));
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		doThrow(new IllegalStateException("listener did not stop")).when(previous).close();
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);

		assertDoesNotThrow(() -> plugin.completeBackendProxyHandlerRestart(restart));

		assertSame(replacement, plugin.getBackendProxyHandler());
		assertFalse(plugin.requestBackendProxyHandlerRestartAbandonment(restart),
				"published runtime state must not be treated as rollbackable");
		org.mockito.InOrder publication = org.mockito.Mockito.inOrder(previous, replacement);
		publication.verify(previous).completeHttpHandoff(replacement);
		publication.verify(replacement).activateInboundMessages();
		publication.verify(previous).close();
	}

	@Test
	void disablingBungeeCoordinationDrainsActiveHttpTransportBeforeClose() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(settings.isUseBungeecoord()).thenReturn(false);
		setField(plugin, "bungeeSettings", settings);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		when(previous.getMethod()).thenReturn(BungeeMethod.HTTP);
		when(previous.requiresPreparationForReplacement()).thenReturn(true);
		when(previous.prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);
		when(previous.commitPreparedDisable()).thenReturn(true);
		setBackendProxyHandler(plugin, previous);
		VotingPluginMain.BackendProxyRestart restart = plugin.prepareBackendProxyHandlerRestart();

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + 1_000_000_000L);
		plugin.completeBackendProxyHandlerRestart(restart);

		verify(previous).prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong());
		verify(previous).close();
		assertNull(plugin.getBackendProxyHandler());
	}

	@Test
	void disablingKeepsPreviousHandlerWhenPreparationAcceptedAnotherDelivery() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(settings.isUseBungeecoord()).thenReturn(false);
		setField(plugin, "bungeeSettings", settings);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		when(previous.requiresPreparationForReplacement()).thenReturn(true);
		when(previous.prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);
		when(previous.commitPreparedDisable()).thenReturn(false);
		setBackendProxyHandler(plugin, previous);
		VotingPluginMain.BackendProxyRestart restart = plugin.prepareBackendProxyHandlerRestart();

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + 1_000_000_000L);
		assertThrows(IllegalStateException.class, () -> plugin.completeBackendProxyHandlerRestart(restart));

		assertSame(previous, plugin.getBackendProxyHandler());
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
		setField(plugin, "backendProxyHandler", handler);
	}

	private void setField(VotingPluginMain plugin, String name, Object value) throws Exception {
		Field field = VotingPluginMain.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(plugin, value);
	}
}
