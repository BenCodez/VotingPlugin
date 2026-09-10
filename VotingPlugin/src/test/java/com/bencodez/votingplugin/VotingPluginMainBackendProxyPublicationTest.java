package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.bukkit.configuration.file.FileConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.proxy.BungeeMethod;

class VotingPluginMainBackendProxyPublicationTest {
	@Test
	void proxyMethodControlPreparationReloadsOnlyNarrowSettingsBeforeReturningTheStagedRestart() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		doCallRealMethod().when(plugin).prepareBackendProxyMethodRestartFromControl();
		BungeeSettings settings = mock(BungeeSettings.class);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		VotingPluginMain.BackendProxyRestart restart = mock(VotingPluginMain.BackendProxyRestart.class);
		setField(plugin, "bungeeSettings", settings);
		when(settings.getServer()).thenReturn("backend-1");
		when(plugin.getOptions()).thenReturn(options);
		when(plugin.prepareBackendProxyHandlerRestart()).thenReturn(restart);

		assertSame(restart, plugin.prepareBackendProxyMethodRestartFromControl());

		verify(settings).reloadData();
		verify(options).setServer("backend-1");
		verify(plugin).updateAdvancedCoreHook();
		verify(plugin).prepareBackendProxyHandlerRestart();
		verify(plugin, never()).reloadFromControl();
	}

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
	void failsRedisRetirementDuringWorkerValidationBeforeBukkitPublication() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(true);
		doAnswer(invocation -> { throw new IllegalStateException("promotion failed"); })
				.when(previous).completeRedisHandoff(replacement);

		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> plugin.validateBackendProxyHandlerRestart(restart(previous, replacement),
						System.nanoTime() + 1_000_000_000L));

		assertSame(previous, plugin.getBackendProxyHandler());
		verify(previous, never()).completeHttpHandoff(replacement);
		verify(previous, never()).close();
		verify(replacement, never()).abortStagedInboundTo(previous);
		verify(replacement, never()).close();
		verify(previous, never()).refreshPresenceAfterFailedReplacement();
	}

	@Test
	void preparesRedisBeforePublishingAndRetiresItOffTheBukkitThread() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisRetirement()).thenReturn(true);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(false);
		when(replacement.getMethod()).thenReturn(BungeeMethod.HTTP);
		when(previous.prepareForReplacement(org.mockito.ArgumentMatchers.eq(BungeeMethod.HTTP),
				org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);
		CountDownLatch closed = new CountDownLatch(1);
		java.util.concurrent.atomic.AtomicReference<String> closeThread = new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(invocation -> {
			closeThread.set(Thread.currentThread().getName());
			closed.countDown();
			return null;
		}).when(previous).close();
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement, true);

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + TimeUnit.SECONDS.toNanos(1));

		org.mockito.InOrder retirement = org.mockito.Mockito.inOrder(previous);
		retirement.verify(previous).prepareForReplacement(org.mockito.ArgumentMatchers.eq(BungeeMethod.HTTP),
				org.mockito.ArgumentMatchers.anyLong());
		verify(previous, never()).close();

		plugin.closePublishedPreviousBackendProxyHandler(previous, "after publication");
		assertTrue(closed.await(1, TimeUnit.SECONDS));
		assertTrue(closeThread.get().startsWith("VotingPlugin-Retired-Redis-Backend"));
	}

	@Test
	void preparesRedisBeforePublishingDisabledState() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisRetirement()).thenReturn(true);
		when(previous.prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);
		VotingPluginMain.BackendProxyRestart restart = restart(previous, null, true);

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + TimeUnit.SECONDS.toNanos(1));

		org.mockito.InOrder retirement = org.mockito.Mockito.inOrder(previous);
		retirement.verify(previous).prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong());
		verify(previous, never()).close();
	}

	@Test
	void restoresWorkerRetiredRedisListenerWhenPublicationIsAborted() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(true);
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + 1_000_000_000L);
		plugin.abortBackendProxyHandlerRestart(restart);

		verify(previous).completeRedisHandoff(replacement);
		verify(replacement, org.mockito.Mockito.timeout(1_000).times(1)).close();
		verify(previous).restoreAfterFailedReplacement(replacement);
	}

	@Test
	void rollbackRestoresPromotedRedisReplayBeforeClosingReplacement() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(true);
		doThrow(new IllegalStateException("publication admission failed"))
				.when(previous).completeHttpHandoff(replacement);
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);
		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + TimeUnit.SECONDS.toNanos(1));

		assertThrows(IllegalStateException.class, () -> plugin.completeBackendProxyHandlerRestart(restart));

		org.mockito.InOrder rollback = org.mockito.Mockito.inOrder(previous, replacement);
		rollback.verify(previous).restoreAfterFailedReplacement(replacement);
		rollback.verify(replacement, org.mockito.Mockito.timeout(1_000).times(1)).close();
	}

	@Test
	void refusesRedisPublicationUntilWorkerHandoffWasValidated() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(true);

		assertThrows(IllegalStateException.class,
				() -> plugin.completeBackendProxyHandlerRestart(restart(previous, replacement)));

		assertSame(previous, plugin.getBackendProxyHandler());
		verify(previous, never()).completeRedisHandoff(replacement);
		verify(replacement, never()).activateInboundMessages();
	}

	@Test
	void abortWaitsForInProgressRedisHandoffAndRestoresItAfterValidation() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		when(previous.requiresRedisHandoff(replacement)).thenReturn(true);
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		doAnswer(invocation -> {
			entered.countDown();
			assertTrue(release.await(1, TimeUnit.SECONDS));
			return null;
		}).when(previous).completeRedisHandoff(replacement);
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);
		Thread validation = new Thread(() -> plugin.validateBackendProxyHandlerRestart(restart,
				System.nanoTime() + TimeUnit.SECONDS.toNanos(1)));
		validation.start();
		assertTrue(entered.await(1, TimeUnit.SECONDS));

		plugin.abortBackendProxyHandlerRestart(restart);
		verify(replacement, never()).close();
		release.countDown();
		validation.join(TimeUnit.SECONDS.toMillis(1));
		assertFalse(validation.isAlive(), "validation must finish and perform the requested rollback");

		verify(replacement, org.mockito.Mockito.timeout(1_000).times(1)).close();
		verify(previous).restoreAfterFailedReplacement(replacement);
	}

	@Test
	void stagedRedisAbortFencesOnBukkitButClosesTheReplacementOffThread() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		BackendProxyHandler replacement = mock(BackendProxyHandler.class);
		setBackendProxyHandler(plugin, previous);
		VotingPluginMain.BackendProxyRestart restart = restart(previous, replacement);
		Field completed = VotingPluginMain.BackendProxyRestart.class.getDeclaredField("redisHandoffCompleted");
		completed.setAccessible(true);
		completed.setBoolean(restart, true);
		CountDownLatch closeStarted = new CountDownLatch(1);
		CountDownLatch releaseClose = new CountDownLatch(1);
		java.util.concurrent.atomic.AtomicReference<String> closeThread = new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(invocation -> {
			closeThread.set(Thread.currentThread().getName());
			closeStarted.countDown();
			releaseClose.await(1, TimeUnit.SECONDS);
			return null;
		}).when(replacement).close();

		try {
			org.junit.jupiter.api.Assertions.assertTimeoutPreemptively(java.time.Duration.ofMillis(250),
					() -> plugin.abortBackendProxyHandlerRestart(restart));
			assertTrue(closeStarted.await(1, TimeUnit.SECONDS));
			assertTrue(closeThread.get().startsWith("VotingPlugin-Staged-Redis-Rollback"));
		} finally {
			releaseClose.countDown();
		}
		verify(replacement, org.mockito.Mockito.timeout(1_000)).close();
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
		publication.verify(replacement).replayRedisAfterHandoffPublication();
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

		org.mockito.InOrder disable = org.mockito.Mockito.inOrder(previous);
		disable.verify(previous).preparePresenceForDisable();
		disable.verify(previous).prepareForReplacement(org.mockito.ArgumentMatchers.isNull(),
				org.mockito.ArgumentMatchers.anyLong());
		disable.verify(previous).commitPreparedDisable();
		disable.verify(previous).close();
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
		plugin.abortBackendProxyHandlerRestart(restart);

		assertSame(previous, plugin.getBackendProxyHandler());
		verify(previous, never()).close();
		verify(previous).restoreAfterFailedReplacement();
		verify(previous).restorePresenceAfterFailedDisablePreparation();
	}

	@Test
	void disablingUnpreparedTransportStopsPresenceBeforeRejectingFurtherSends() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(settings.isUseBungeecoord()).thenReturn(false);
		setField(plugin, "bungeeSettings", settings);
		BackendProxyHandler previous = mock(BackendProxyHandler.class);
		when(previous.commitPreparedDisable()).thenReturn(true);
		setBackendProxyHandler(plugin, previous);
		VotingPluginMain.BackendProxyRestart restart = plugin.prepareBackendProxyHandlerRestart();

		plugin.validateBackendProxyHandlerRestart(restart, System.nanoTime() + 1_000_000_000L);
		plugin.completeBackendProxyHandlerRestart(restart);

		org.mockito.InOrder disable = org.mockito.Mockito.inOrder(previous);
		disable.verify(previous).preparePresenceForDisable();
		disable.verify(previous).commitPreparedDisable();
		disable.verify(previous).close();
		assertNull(plugin.getBackendProxyHandler());
	}

	private VotingPluginMain.BackendProxyRestart restart(BackendProxyHandler previous,
			BackendProxyHandler replacement) throws Exception {
		return restart(previous, replacement, false);
	}

	private VotingPluginMain.BackendProxyRestart restart(BackendProxyHandler previous,
			BackendProxyHandler replacement, boolean previousRequiresPreparation) throws Exception {
		Constructor<VotingPluginMain.BackendProxyRestart> constructor = VotingPluginMain.BackendProxyRestart.class
				.getDeclaredConstructor(BackendProxyHandler.class, BackendProxyHandler.class, boolean.class, boolean.class);
		constructor.setAccessible(true);
		return constructor.newInstance(previous, replacement, replacement == null, previousRequiresPreparation);
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
