package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
