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
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteQueue;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteJournal;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;

class VotingPluginBungeeInitializationTest {
	@Test
	void missingMultiProxySectionIsAnEmptyDiagnosticInventory() {
		BungeeConfig config = mock(BungeeConfig.class, CALLS_REAL_METHODS);
		net.md_5.bungee.config.Configuration yaml = new net.md_5.bungee.config.Configuration();
		when(config.getData()).thenReturn(yaml);
		assertTrue(config.getMultiProxyServers().isEmpty());
	}

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
	void rejectedSchedulerAdmissionLeavesVoteOwnedUntilDurableHandoff() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		doThrow(new IllegalStateException("scheduler stopped")).when(scheduler)
				.runAsync(eq(plugin), any(Runnable.class));
		when(runtime.retainIncomingVoteForRestart(any(PendingIncomingVote.class))).thenReturn(true);

		plugin.acceptIncomingVote("Player", "Service");

		PendingIncomingVoteQueue pending = (PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes");
		assertEquals(1, pending.size());
		java.lang.reflect.Method persist = VotingPluginBungee.class.getDeclaredMethod(
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
	void failedDurableHandoffSchedulesAnotherAttempt() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		PendingIncomingVoteQueue queue = new PendingIncomingVoteQueue();
		PendingIncomingVote pending = queue.admit("Player", "Service");
		java.util.concurrent.atomic.AtomicReference<Runnable> retry = new java.util.concurrent.atomic.AtomicReference<>();
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", queue);
		setField(plugin, "votingPluginProxy", runtime);
		when(plugin.getProxy()).thenReturn(proxy);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		when(runtime.retainIncomingVoteForRestart(pending)).thenReturn(false, true);
		doAnswer(invocation -> {
			retry.set(invocation.getArgument(1));
			return mock(net.md_5.bungee.api.scheduler.ScheduledTask.class);
		}).when(scheduler).schedule(eq(plugin), any(Runnable.class), eq(5L),
				eq(java.util.concurrent.TimeUnit.SECONDS));
		when(proxy.getScheduler()).thenReturn(scheduler);
		java.lang.reflect.Method persist = VotingPluginBungee.class.getDeclaredMethod(
				"persistTerminalPendingVote", PendingIncomingVote.class);
		persist.setAccessible(true);

		persist.invoke(plugin, pending);

		assertEquals(1, queue.size());
		assertTrue(retry.get() != null);
		retry.get().run();
		assertEquals(0, queue.size());
		verify(runtime, org.mockito.Mockito.times(2)).retainIncomingVoteForRestart(pending);
	}

	@Test
	void voteIsOwnedBeforeAFullReloadReleasesTheLifecycleLock() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		Object reloadLock = new Object();
		PendingIncomingVoteQueue pending = new PendingIncomingVoteQueue();
		setField(plugin, "reloadLock", reloadLock);
		setField(plugin, "pendingIncomingVotes", pending);
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "reloading", true);
		when(runtime.retainIncomingVoteForRestart(any(PendingIncomingVote.class))).thenReturn(true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(scheduler.runAsync(eq(plugin), any(Runnable.class)))
				.thenReturn(mock(net.md_5.bungee.api.scheduler.ScheduledTask.class));
		java.util.concurrent.CountDownLatch returned = new java.util.concurrent.CountDownLatch(1);

		synchronized (reloadLock) {
			Thread callback = new Thread(() -> {
				plugin.acceptIncomingVote("Player", "Service");
				returned.countDown();
			});
			callback.start();
			assertTrue(returned.await(2, java.util.concurrent.TimeUnit.SECONDS));
			assertEquals(1, pending.size());
			java.util.UUID admittedId = pending.snapshot().get(0).getVoteId();
			pending.closeAdmission();
			java.lang.reflect.Method persist = VotingPluginBungee.class.getDeclaredMethod(
					"persistPendingIncomingVotes", VotingPluginProxy.class, String.class);
			persist.setAccessible(true);
			assertTrue((Boolean) persist.invoke(plugin, runtime, "test shutdown"));
			org.mockito.ArgumentCaptor<PendingIncomingVote> retained =
					org.mockito.ArgumentCaptor.forClass(PendingIncomingVote.class);
			verify(runtime).retainIncomingVoteForRestart(retained.capture());
			assertEquals(admittedId, retained.getValue().getVoteId());
		}

		assertEquals(0, pending.snapshot().size());
	}

	@Test
	void failedRuntimeHandoffUsesEmergencyJournal(@TempDir java.nio.file.Path dataDirectory) throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		setField(plugin, "pendingIncomingVoteJournal", new PendingIncomingVoteJournal(dataDirectory));
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		doThrow(new IllegalStateException("scheduler stopped")).when(scheduler)
				.runAsync(eq(plugin), any(Runnable.class));
		when(runtime.retainIncomingVoteForRestart(any(PendingIncomingVote.class))).thenReturn(false);

		plugin.acceptIncomingVote("Player", "Service");
		java.lang.reflect.Method persist = VotingPluginBungee.class.getDeclaredMethod(
				"persistPendingIncomingVotes", VotingPluginProxy.class, String.class);
		persist.setAccessible(true);

		assertTrue((Boolean) persist.invoke(plugin, runtime, "test shutdown"));
		assertEquals(0, ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).size());
		assertEquals(1, new PendingIncomingVoteJournal(dataDirectory).load().size());
	}

	@Test
	void failedPrimaryEmergencyJournalUsesSiblingRescue() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		PendingIncomingVoteQueue queue = new PendingIncomingVoteQueue();
		queue.admit("Player", "Service");
		PendingIncomingVoteJournal primary = mock(PendingIncomingVoteJournal.class);
		PendingIncomingVoteJournal rescue = mock(PendingIncomingVoteJournal.class);
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", queue);
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "pendingIncomingVoteJournal", primary);
		setField(plugin, "pendingIncomingVoteRescueJournal", rescue);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		when(runtime.retainIncomingVoteForRestart(any(PendingIncomingVote.class))).thenReturn(false);
		doThrow(new java.io.IOException("primary unavailable")).when(primary).merge(any());
		java.lang.reflect.Method persist = VotingPluginBungee.class.getDeclaredMethod(
				"persistPendingIncomingVotes", VotingPluginProxy.class, String.class);
		persist.setAccessible(true);

		assertTrue((Boolean) persist.invoke(plugin, runtime, "test shutdown"));

		assertEquals(0, queue.size());
		verify(rescue).merge(any());
	}

	@Test
	void acceptedVoteCompletesOnceWithItsOriginalId() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		java.util.concurrent.atomic.AtomicReference<Runnable> task = new java.util.concurrent.atomic.AtomicReference<>();
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		doAnswer(invocation -> {
			task.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runAsync(eq(plugin), any(Runnable.class));

		plugin.acceptIncomingVote("Player", "Service");
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.snapshot().get(0);
		task.get().run();

		verify(runtime).vote("Player", "Service", true, true, 0, null, null, pending.getVoteId());
		assertEquals(0, ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).size());
	}

	@Test
	void reloadWaitingKeepsStableIdWithoutConsumingStorageAttempts() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		java.util.concurrent.atomic.AtomicReference<Runnable> initial = new java.util.concurrent.atomic.AtomicReference<>();
		java.util.concurrent.atomic.AtomicReference<Runnable> retry = new java.util.concurrent.atomic.AtomicReference<>();
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
		setField(plugin, "reloading", true);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		doAnswer(invocation -> {
			initial.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runAsync(eq(plugin), any(Runnable.class));
		doAnswer(invocation -> {
			retry.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).schedule(eq(plugin), any(Runnable.class), eq(1L),
				eq(java.util.concurrent.TimeUnit.SECONDS));

		plugin.acceptIncomingVote("Player", "Service");
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.snapshot().get(0);
		initial.get().run();

		assertEquals(0, pending.getStorageAttempts());
		assertTrue(((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).contains(pending.getVoteId()));
		setField(plugin, "reloading", false);
		retry.get().run();
		verify(runtime).vote("Player", "Service", true, true, 0, null, null, pending.getVoteId());
		assertEquals(0, ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes")).size());
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

	@Test
	void successfulSoftReloadTaskRetryRestoresRetainedRuntimeReadiness() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		setField(plugin, "runtimeInitialized", true);
		setField(plugin, "runtimeOperational", false);

		plugin.publishRetainedRuntimeOperational();

		assertTrue(plugin.isRuntimeOperational());
	}

	@Test
	void softReloadCannotPublishAnIncompleteRuntime() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("VotingPluginBungeeInitializationTest"));
		setField(plugin, "runtimeInitialized", false);
		setField(plugin, "runtimeOperational", true);

		plugin.publishRetainedRuntimeOperational();

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
