package com.bencodez.votingplugin.proxy.bungee;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
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

	@Test
	void absentVotifierIsAnIntentionalNoListenerState() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		when(plugin.getVotingPluginProxy()).thenReturn(runtime);
		doThrow(new ClassNotFoundException("absent")).when(plugin).requireVotifierEventClass();

		assertTrue(plugin.initVotifierListenerIfNeeded());
		verify(runtime).setVotifierEnabled(false);
		verify(plugin, never()).createVotifierListener();
	}

	@Test
	void disabledVotifierDoesNotRequireAListener() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		when(plugin.getVotingPluginProxy()).thenReturn(runtime);
		when(runtime.isVotifierEnabled()).thenReturn(false);

		assertTrue(plugin.initVotifierListenerIfNeeded());
		verify(plugin, never()).createVotifierListener();
	}

	@Test
	void successfulVotifierRegistrationPublishesListener() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.plugin.PluginManager manager =
				mock(net.md_5.bungee.api.plugin.PluginManager.class);
		VoteEventBungee listener = mock(VoteEventBungee.class);
		when(plugin.getVotingPluginProxy()).thenReturn(runtime);
		when(runtime.isVotifierEnabled()).thenReturn(true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getPluginManager()).thenReturn(manager);
		when(plugin.createVotifierListener()).thenReturn(listener);

		assertTrue(plugin.initVotifierListenerIfNeeded());
		verify(manager).registerListener(plugin, listener);
		assertSame(listener, getField(plugin, "voteEventBungee"));
	}

	@Test
	void liveVoteAdmissionIsFencedToThePublishedRuntime() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy predecessor = mock(VotingPluginProxy.class);
		VotingPluginProxy replacement = mock(VotingPluginProxy.class);
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "votingPluginProxy", predecessor);
		java.util.UUID voteId = java.util.UUID.randomUUID();

		setField(plugin, "reloading", true);
		assertSame(IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD,
				plugin.processIncomingVote("Player", "Service", voteId));
		setField(plugin, "votingPluginProxy", replacement);
		setField(plugin, "runtimeOperational", true);
		setField(plugin, "reloading", false);
		assertSame(IncomingVoteRuntimeResult.PROCESSED,
				plugin.processIncomingVote("Player", "Service", voteId));
		verify(replacement).vote("Player", "Service", true, true, 0, null, null, voteId);
		org.mockito.Mockito.verifyNoInteractions(predecessor);

		setField(plugin, "runtimeOperational", false);
		assertSame(IncomingVoteRuntimeResult.RUNTIME_UNAVAILABLE,
				plugin.processIncomingVote("Player", "Service", voteId));
		org.mockito.Mockito.verifyNoMoreInteractions(replacement);
	}

	@Test
	void terminalReplacementFailureRetiresInternalRuntimeWork() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		setField(plugin, "votingPluginProxy", runtime);
		java.lang.reflect.Method retire = VotingPluginBungee.class
				.getDeclaredMethod("retireFailedReplacementRuntime");
		retire.setAccessible(true);

		retire.invoke(plugin);

		verify(runtime).onDisable();
		assertFalse(plugin.isRuntimeOperational());
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		java.lang.reflect.Field field = VotingPluginBungee.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static Object getField(Object target, String name) throws Exception {
		java.lang.reflect.Field field = VotingPluginBungee.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}
}
