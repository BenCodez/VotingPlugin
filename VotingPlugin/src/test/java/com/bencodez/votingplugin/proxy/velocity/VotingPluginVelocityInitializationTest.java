package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Path;

import org.bstats.velocity.Metrics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.slf4j.Logger;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteQueue;
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

	@Test
	void absentVotifierIsAnIntentionalNoListenerState(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = spy(new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory));
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		setField(plugin, "votingPluginProxy", runtime);
		doThrow(new ClassNotFoundException("absent")).when(plugin).requireVotifierEventClass();
		try {
			assertTrue(plugin.initVotifierListenerIfNeeded());
			verify(runtime).setVotifierEnabled(false);
			verify(plugin, never()).createVotifierListener();
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void disabledVotifierDoesNotRequireAListener(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = spy(new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory));
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		setField(plugin, "votingPluginProxy", runtime);
		when(runtime.isVotifierEnabled()).thenReturn(false);
		try {
			assertTrue(plugin.initVotifierListenerIfNeeded());
			verify(plugin, never()).createVotifierListener();
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void successfulVotifierRegistrationPublishesListener(@TempDir Path dataDirectory) throws Exception {
		ProxyServer server = mock(ProxyServer.class);
		EventManager eventManager = mock(EventManager.class);
		when(server.getEventManager()).thenReturn(eventManager);
		VotingPluginVelocity plugin = spy(new VotingPluginVelocity(server, mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory));
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		VoteEventVelocity listener = mock(VoteEventVelocity.class);
		setField(plugin, "votingPluginProxy", runtime);
		when(runtime.isVotifierEnabled()).thenReturn(true);
		when(plugin.createVotifierListener()).thenReturn(listener);
		try {
			assertTrue(plugin.initVotifierListenerIfNeeded());
			verify(eventManager).register(plugin, listener);
			assertSame(listener, getField(plugin, "voteEventVelocity"));
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void liveVoteAdmissionIsFencedToThePublishedRuntime(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy predecessor = mock(VotingPluginProxy.class);
		VotingPluginProxy replacement = mock(VotingPluginProxy.class);
		setField(plugin, "votingPluginProxy", predecessor);
		java.util.UUID voteId = java.util.UUID.randomUUID();
		try {
			setField(plugin, "reloading", true);
			assertSame(IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD,
					plugin.processIncomingVote("Player", "Service", voteId));
			setField(plugin, "votingPluginProxy", replacement);
			setField(plugin, "runtimeOperational", true);
			setField(plugin, "reloading", false);
			assertSame(IncomingVoteRuntimeResult.PROCESSED,
					plugin.processIncomingVote("Player", "Service", voteId));
			verify(replacement).vote("Player", "Service", true, false, 0, null, null, voteId);
			org.mockito.Mockito.verifyNoInteractions(predecessor);

			setField(plugin, "runtimeOperational", false);
			assertSame(IncomingVoteRuntimeResult.RUNTIME_UNAVAILABLE,
					plugin.processIncomingVote("Player", "Service", voteId));
			org.mockito.Mockito.verifyNoMoreInteractions(replacement);
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void rejectedExecutorAdmissionLeavesVoteOwnedUntilDurableHandoff(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		java.util.concurrent.ScheduledExecutorService rejected = mock(java.util.concurrent.ScheduledExecutorService.class);
		plugin.getTimer().shutdownNow();
		setField(plugin, "timer", rejected);
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		doThrow(new java.util.concurrent.RejectedExecutionException("stopped")).when(rejected).execute(any(Runnable.class));
		when(runtime.retainIncomingVoteForRestart(any(PendingIncomingVote.class))).thenReturn(true);

		plugin.acceptIncomingVote("Player", "Service");

		PendingIncomingVoteQueue pending = (PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes");
		assertEquals(1, pending.size());
		java.lang.reflect.Method persist = VotingPluginVelocity.class.getDeclaredMethod(
				"persistPendingIncomingVotes", VotingPluginProxy.class, String.class);
		persist.setAccessible(true);
		assertTrue((Boolean) persist.invoke(plugin, runtime, "test shutdown"));
		org.mockito.ArgumentCaptor<PendingIncomingVote> retained =
				org.mockito.ArgumentCaptor.forClass(PendingIncomingVote.class);
		verify(runtime).retainIncomingVoteForRestart(retained.capture());
		assertEquals("Player", retained.getValue().getPlayer());
		assertEquals(0, pending.size());
	}

	@Test
	void acceptedVoteCompletesOnceWithItsOriginalId(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		java.util.concurrent.ScheduledExecutorService timer = mock(java.util.concurrent.ScheduledExecutorService.class);
		java.util.concurrent.atomic.AtomicReference<Runnable> task = new java.util.concurrent.atomic.AtomicReference<>();
		plugin.getTimer().shutdownNow();
		setField(plugin, "timer", timer);
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		doAnswer(invocation -> {
			task.set(invocation.getArgument(0));
			return null;
		}).when(timer).execute(any(Runnable.class));

		plugin.acceptIncomingVote("Player", "Service");
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.snapshot().get(0);
		task.get().run();

		verify(runtime).vote("Player", "Service", true, false, 0, null, null, pending.getVoteId());
		assertEquals(0, ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).size());
	}

	@Test
	void reloadWaitingKeepsStableIdWithoutConsumingStorageAttempts(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		java.util.concurrent.ScheduledExecutorService timer = mock(java.util.concurrent.ScheduledExecutorService.class);
		java.util.concurrent.atomic.AtomicReference<Runnable> initial = new java.util.concurrent.atomic.AtomicReference<>();
		java.util.concurrent.atomic.AtomicReference<Runnable> retry = new java.util.concurrent.atomic.AtomicReference<>();
		plugin.getTimer().shutdownNow();
		setField(plugin, "timer", timer);
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		setField(plugin, "reloading", true);
		doAnswer(invocation -> {
			initial.set(invocation.getArgument(0));
			return null;
		}).when(timer).execute(any(Runnable.class));
		doAnswer(invocation -> {
			retry.set(invocation.getArgument(0));
			return null;
		}).when(timer).schedule(any(Runnable.class), eq(1L), eq(java.util.concurrent.TimeUnit.SECONDS));

		plugin.acceptIncomingVote("Player", "Service");
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.snapshot().get(0);
		initial.get().run();

		assertEquals(0, pending.getStorageAttempts());
		assertTrue(((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).contains(pending.getVoteId()));
		setField(plugin, "reloading", false);
		retry.get().run();
		verify(runtime).vote("Player", "Service", true, false, 0, null, null, pending.getVoteId());
		assertEquals(0, ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).size());
	}

	@Test
	void terminalReplacementFailureRetiresInternalRuntimeWork(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		setField(plugin, "votingPluginProxy", runtime);
		java.lang.reflect.Method retire = VotingPluginVelocity.class
				.getDeclaredMethod("retireFailedReplacementRuntime");
		retire.setAccessible(true);
		try {
			retire.invoke(plugin);
			verify(runtime).onDisable();
			assertFalse(plugin.isRuntimeOperational());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void successfulSoftReloadTaskRetryRestoresRetainedRuntimeReadiness(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		setField(plugin, "runtimeInitialized", true);
		setField(plugin, "runtimeOperational", false);
		try {
			plugin.publishRetainedRuntimeOperational();
			assertTrue(plugin.isRuntimeOperational());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void softReloadCannotPublishAnIncompleteRuntime(@TempDir Path dataDirectory) throws Exception {
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), mock(Logger.class),
				mock(Metrics.Factory.class), dataDirectory);
		setField(plugin, "runtimeInitialized", false);
		setField(plugin, "runtimeOperational", true);
		try {
			plugin.publishRetainedRuntimeOperational();
			assertFalse(plugin.isRuntimeOperational());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		java.lang.reflect.Field field = VotingPluginVelocity.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static Object getField(Object target, String name) throws Exception {
		java.lang.reflect.Field field = VotingPluginVelocity.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
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
