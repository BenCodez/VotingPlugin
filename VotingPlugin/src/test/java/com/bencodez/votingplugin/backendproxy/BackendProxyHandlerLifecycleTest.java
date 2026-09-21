package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.util.ArrayDeque;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.MysqlBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.PluginMessagingBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.SocketBackendProxyTransport;
import com.bencodez.votingplugin.proxy.BungeeMethod;

class BackendProxyHandlerLifecycleTest {
	@Test
	void globalDataWakeupMovesOffTheCallingAndPrimaryThreads() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		handler.activateInboundMessages();
		Runnable localDispatch = mock(Runnable.class);

		handler.dispatchIncomingAfterPublication(
				JsonEnvelope.builder(VotingPluginWire.SUB_BUNGEE_TIME_CHANGE).build(), localDispatch);

		verify(scheduler).runTaskAsynchronously(plugin, localDispatch);
		verify(localDispatch, never()).run();
	}

	@Test
	void voteAndVoteUpdateMessagesStayOrderedAcrossAsyncAndPlatformWork() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
		setField(handler, "messageRouter", router);
		handler.activateInboundMessages();

		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));

		JsonEnvelope voteEnvelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build();
		JsonEnvelope updateEnvelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build();
		JsonEnvelope onlineEnvelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_ONLINE).build();
		java.util.ArrayList<String> handled = new java.util.ArrayList<>();
		AtomicReference<java.util.function.Consumer<OrderedVoteOutcome>> updateCompletion = new AtomicReference<>();
		doAnswer(invocation -> {
			JsonEnvelope envelope = invocation.getArgument(0);
			java.util.function.Consumer<OrderedVoteOutcome> completion = invocation.getArgument(1);
			handled.add(envelope.getSubChannel());
			if (VotingPluginWire.SUB_VOTE_UPDATE.equals(envelope.getSubChannel())) {
				updateCompletion.set(completion);
			} else {
				completion.accept(OrderedVoteOutcome.COMPLETE);
			}
			return null;
		}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());

		handler.dispatchIncomingAfterPublication(voteEnvelope, mock(Runnable.class));
		handler.dispatchIncomingAfterPublication(updateEnvelope, mock(Runnable.class));
		handler.dispatchIncomingAfterPublication(onlineEnvelope, mock(Runnable.class));

		assertEquals(1, asyncTasks.size(), "only the head of the ordered vote lane should be scheduled");

		asyncTasks.removeFirst().run();
		assertEquals(java.util.List.of(VotingPluginWire.SUB_VOTE), handled);
		assertEquals(1, asyncTasks.size(), "VoteUpdate should be scheduled only after Vote completes");

		asyncTasks.removeFirst().run();
		assertEquals(java.util.List.of(VotingPluginWire.SUB_VOTE, VotingPluginWire.SUB_VOTE_UPDATE), handled);
		assertNotNull(updateCompletion.get());
		assertTrue(asyncTasks.isEmpty(), "later votes must wait for VoteUpdate platform work");

		updateCompletion.get().accept(OrderedVoteOutcome.COMPLETE);
		assertEquals(1, asyncTasks.size(), "VoteUpdate completion should release the next vote");

		asyncTasks.removeFirst().run();
		assertEquals(java.util.List.of(VotingPluginWire.SUB_VOTE, VotingPluginWire.SUB_VOTE_UPDATE,
				VotingPluginWire.SUB_VOTE_ONLINE), handled);
		verify(scheduler, times(3)).runTaskAsynchronously(eq(plugin), any(Runnable.class));
	}

	@Test
	void orderedVoteBacklogSpillsPastBoundToDurableOverflow(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-overflow-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			setField(handler, "messageRouter", mock(BackendProxyMessageRouter.class));
			handler.activateInboundMessages();

			for (int i = 0; i < BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE + 1; i++) {
				handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
						.put(VotingPluginWire.K_VOTE_ID, new java.util.UUID(0L, i + 1L).toString()).build(),
						mock(Runnable.class));
			}

			@SuppressWarnings("unchecked")
			ArrayDeque<JsonEnvelope> queued = (ArrayDeque<JsonEnvelope>) getField(handler, "orderedVoteDispatchQueue");
			assertEquals(BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE, queued.size());
			assertEquals(1, overflow.size());
		} finally {
			overflow.close();
		}

		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(1, recovered.size(), "overflow must survive restart rather than growing memory without a bound");
		} finally {
			recovered.close();
		}
	}

	@Test
	void overflowAdmissionIsOnDiskBeforeItReturns(@TempDir Path tempDir) {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-admission-test"));
		BackendOrderedVoteOverflowQueue first = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertTrue(first.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "accepted").build()));
			BackendOrderedVoteOverflowQueue restartBeforeClose = new BackendOrderedVoteOverflowQueue(plugin);
			try {
				assertEquals(1, restartBeforeClose.size());
			} finally {
				restartBeforeClose.close();
			}
		} finally {
			first.close();
		}
	}

	@Test
	void saturatedIngressQueuesDurableAdmissionWithoutBlockingCaller(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-async-admission-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		Object persistenceLock = getField(overflow, "persistenceWriteLock");
		CountDownLatch locked = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		Thread blocker = new Thread(() -> {
			synchronized (persistenceLock) {
				locked.countDown();
				try {
					release.await();
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
				}
			}
		});
		blocker.start();
		assertTrue(locked.await(3, TimeUnit.SECONDS));
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			setField(handler, "messageRouter", mock(BackendProxyMessageRouter.class));
			handler.activateInboundMessages();
			for (int index = 0; index < BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE; index++) {
				handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
						.put("sequence", Integer.toString(index)).build(), mock(Runnable.class));
			}
			CompletableFuture<Void> admission = CompletableFuture.runAsync(() -> handler.dispatchIncomingAfterPublication(
					JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).put("sequence", "overflow").build(),
					mock(Runnable.class)));
			admission.get(500, TimeUnit.MILLISECONDS);
			assertEquals(1, overflow.size());
		} finally {
			release.countDown();
			blocker.join(3_000L);
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(1, recovered.size());
		} finally {
			recovered.close();
		}
	}

	@Test
	void unreadableOverflowCannotBeOverwritten(@TempDir Path tempDir) throws Exception {
		Path queueFile = tempDir.resolve("BackendProxyVoteQueue.yml");
		Files.createDirectory(queueFile);
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-unreadable-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertFalse(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build()));
		} finally {
			overflow.close();
		}
		assertTrue(Files.isDirectory(queueFile));
	}

	@Test
	void failedOverflowWriteDoesNotAcceptVote(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-write-failure-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			Files.createDirectory(tempDir.resolve("BackendProxyVoteQueue.yml"));
			assertFalse(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build()));
			assertEquals(0, overflow.size());
		} finally {
			overflow.close();
		}
	}

	@Test
	void oversizedFailureHistoryStopsRecoveryWithoutDiscardingEvidence(@TempDir Path tempDir) throws Exception {
		StringBuilder yaml = new StringBuilder("Envelopes: []\nFailedEnvelopes:\n");
		for (int index = 0; index < BackendOrderedVoteOverflowQueue.MAX_FAILED_ENTRIES + 20; index++) {
			yaml.append("- failed-").append(index).append('\n');
		}
		Files.writeString(tempDir.resolve("BackendProxyVoteQueue.yml"), yaml);
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-failure-bound-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(0, overflow.failedSize());
			assertFalse(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build()));
			assertTrue(Files.readString(tempDir.resolve("BackendProxyVoteQueue.yml")).contains("failed-0"));
		} finally {
			overflow.close();
		}
	}

	@Test
	void fullFailureHistoryRejectsNextQuarantineWithoutEviction(@TempDir Path tempDir) throws Exception {
		StringBuilder yaml = new StringBuilder("Envelopes: []\nFailedEnvelopes:\n");
		for (int index = 0; index < BackendOrderedVoteOverflowQueue.MAX_FAILED_ENTRIES; index++) {
			yaml.append("- failed-").append(index).append('\n');
		}
		Files.writeString(tempDir.resolve("BackendProxyVoteQueue.yml"), yaml);
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-failure-capacity-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertFalse(overflow.quarantineForShutdown(null, JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "rejected").build()));
			assertEquals(BackendOrderedVoteOverflowQueue.MAX_FAILED_ENTRIES, overflow.failedSize());
			assertTrue(overflow.isQuarantineCapacityExhausted());
			@SuppressWarnings("unchecked")
			ArrayDeque<String> failures = (ArrayDeque<String>) getField(overflow, "failedEntries");
			assertEquals("failed-0", failures.peekFirst());
			assertEquals("failed-255", failures.peekLast());

			BukkitScheduler scheduler = mock(BukkitScheduler.class);
			when(plugin.getBukkitScheduler()).thenReturn(scheduler);
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			handler.activateInboundMessages();
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build(),
					mock(Runnable.class));
			verify(scheduler, never()).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		} finally {
			overflow.close();
		}
	}

	@Test
	void ambiguousHandlerFailureIsQuarantinedBeforeLaterVote(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-failure-test"));
		LinkedBlockingQueue<Runnable> asyncTasks = new LinkedBlockingQueue<>();
		doAnswer(invocation -> {
			asyncTasks.add(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			AtomicInteger attempts = new AtomicInteger();
			doAnswer(invocation -> {
				if (attempts.incrementAndGet() == 1) throw new IllegalStateException("partial reward");
				invocation.<java.util.function.Consumer<OrderedVoteOutcome>>getArgument(1)
						.accept(OrderedVoteOutcome.COMPLETE);
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "failed").build()));
			assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "next").build()));
			Runnable first = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(first);
			assertThrows(IllegalStateException.class, first::run);
			Runnable second = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(second, "quarantine should release the next vote");
			second.run();
			assertEquals(2, attempts.get(), "failed reward must not be retried");
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.size() != 0 && System.nanoTime() < deadline) Thread.sleep(10);
			assertEquals(0, overflow.size());
			assertEquals(1, overflow.failedSize());
			assertTrue(Files.exists(tempDir.resolve("BackendProxyVoteQueue.yml")));
		} finally {
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(0, recovered.size());
			assertEquals(1, recovered.failedSize());
		} finally {
			recovered.close();
		}
	}

	@Test
	void inMemoryHandlerFailureIsQuarantinedWithoutRetry(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-memory-failure-test"));
		LinkedBlockingQueue<Runnable> asyncTasks = new LinkedBlockingQueue<>();
		doAnswer(invocation -> {
			asyncTasks.add(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			doThrow(new IllegalStateException("partial reward")).when(router)
					.handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "failed").build(), mock(Runnable.class));
			Runnable first = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(first);
			assertThrows(IllegalStateException.class, first::run);
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.failedSize() == 0 && System.nanoTime() < deadline) {
				Thread.sleep(10);
			}
			assertEquals(1, overflow.failedSize());
			assertEquals(0, overflow.size());
			assertTrue(asyncTasks.isEmpty(), "failed reward must not be retried");
		} finally {
			overflow.close();
		}
	}

	@Test
	void shutdownPersistsQueuedQuarantineWithoutReplayingFailedVote(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-failure-shutdown-test"));
		LinkedBlockingQueue<Runnable> asyncTasks = new LinkedBlockingQueue<>();
		doAnswer(invocation -> {
			asyncTasks.add(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		CountDownLatch occupied = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		ScheduledThreadPoolExecutor worker = (ScheduledThreadPoolExecutor) getField(overflow, "worker");
		worker.execute(() -> {
			occupied.countDown();
			try {
				release.await();
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
			}
		});
		assertTrue(occupied.await(3, TimeUnit.SECONDS));
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			doThrow(new IllegalStateException("partial reward")).when(router)
					.handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "failed").build(), mock(Runnable.class));
			Runnable first = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(first);
			assertThrows(IllegalStateException.class, first::run);
			handler.close();
		} finally {
			overflow.close();
			release.countDown();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(0, recovered.size());
			assertEquals(1, recovered.failedSize());
		} finally {
			recovered.close();
		}
	}

	@Test
	void crowdedOverflowReservesCapacityForShutdownPrefix(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getBukkitScheduler()).thenReturn(mock(BukkitScheduler.class));
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-capacity-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			handler.activateInboundMessages();
			for (int index = 0; index < BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE; index++) {
				handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
						.put("sequence", Integer.toString(index)).build(), mock(Runnable.class));
			}
			for (int index = 0; index < BackendOrderedVoteOverflowQueue.MAX_NORMAL_ENTRIES; index++) {
				assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
						.put("sequence", Integer.toString(index + BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE))
						.build(), BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE));
			}
			assertFalse(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build(),
					BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE));
			handler.close();
			assertEquals(BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE
					+ BackendOrderedVoteOverflowQueue.MAX_NORMAL_ENTRIES, overflow.size());
		} finally {
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(BackendProxyHandler.MAX_ORDERED_VOTE_QUEUE
					+ BackendOrderedVoteOverflowQueue.MAX_NORMAL_ENTRIES, recovered.size());
		} finally {
			recovered.close();
		}
	}

	@Test
	void failedVoteUpdateKeepsDurableHeadUntilRetrySucceeds(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-retry-test"));
		LinkedBlockingQueue<Runnable> asyncTasks = new LinkedBlockingQueue<>();
		doAnswer(invocation -> {
			asyncTasks.add(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			AtomicInteger attempts = new AtomicInteger();
			doAnswer(invocation -> {
				invocation.<java.util.function.Consumer<OrderedVoteOutcome>>getArgument(1)
						.accept(attempts.incrementAndGet() > 1
								? OrderedVoteOutcome.COMPLETE : OrderedVoteOutcome.RETRY);
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build()));

			Runnable first = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(first, "durable head must wake the ordered lane");
			first.run();
			assertEquals(1, attempts.get());
			assertEquals(1, overflow.size(), "failed lookup must not acknowledge the durable head");

			Runnable retry = asyncTasks.poll(3, TimeUnit.SECONDS);
			assertNotNull(retry, "failed lookup must schedule a bounded retry");
			retry.run();
			assertEquals(2, attempts.get());
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.size() != 0 && System.nanoTime() < deadline) Thread.sleep(10);
			assertEquals(0, overflow.size());
		} finally {
			overflow.close();
		}
	}

	@Test
	void durableCompletionIsPersistedBeforeNextVoteRuns(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-ack-test"));
		LinkedBlockingQueue<Runnable> asyncTasks = new LinkedBlockingQueue<>();
		doAnswer(invocation -> {
			asyncTasks.add(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			doAnswer(invocation -> {
				invocation.<java.util.function.Consumer<OrderedVoteOutcome>>getArgument(1)
						.accept(OrderedVoteOutcome.COMPLETE);
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build()));
			asyncTasks.poll(3, TimeUnit.SECONDS).run();
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.size() != 0 && System.nanoTime() < deadline) Thread.sleep(10);
			assertEquals(0, overflow.size());

			BackendOrderedVoteOverflowQueue restartBeforeClose = new BackendOrderedVoteOverflowQueue(plugin);
			try {
				assertEquals(0, restartBeforeClose.size(), "completed reward must not replay after a crash");
			} finally {
				restartBeforeClose.close();
			}
		} finally {
			overflow.close();
		}
	}

	@Test
	void overflowCloseReturnsWithinBoundWhenPersistenceLockIsStuck(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-close-bound-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		Object persistenceLock = getField(overflow, "persistenceWriteLock");
		CountDownLatch locked = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		Thread blocker = new Thread(() -> {
			synchronized (persistenceLock) {
				locked.countDown();
				try {
					release.await();
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
				}
			}
		});
		blocker.start();
		assertTrue(locked.await(3, TimeUnit.SECONDS));
		long started = System.nanoTime();
		try {
			overflow.close();
			assertTrue(System.nanoTime() - started < TimeUnit.SECONDS.toNanos(2));
		} finally {
			release.countDown();
			blocker.join(3_000L);
			assertTrue(overflow.awaitClose(3_000L));
		}
	}

	@Test
	void failedInMemoryVoteUpdateRetriesBeforeLaterVotes() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		AtomicReference<Runnable> delayedRetry = new AtomicReference<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		doAnswer(invocation -> {
			delayedRetry.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskLaterAsynchronously(eq(plugin), any(Runnable.class), eq(20L));
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
		setField(handler, "messageRouter", router);
		java.util.List<String> processed = new java.util.ArrayList<>();
		doAnswer(invocation -> {
			String sequence = invocation.<JsonEnvelope>getArgument(0).getFields().get("sequence");
			processed.add(sequence);
			invocation.<java.util.function.Consumer<OrderedVoteOutcome>>getArgument(1)
					.accept(!sequence.equals("first") || processed.size() > 1
							? OrderedVoteOutcome.COMPLETE : OrderedVoteOutcome.RETRY);
			return null;
		}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
		handler.activateInboundMessages();
		handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE)
				.put("sequence", "first").build(), mock(Runnable.class));
		handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
				.put("sequence", "second").build(), mock(Runnable.class));

		asyncTasks.removeFirst().run();
		assertEquals(java.util.List.of("first"), processed);
		assertTrue(asyncTasks.isEmpty());
		assertNotNull(delayedRetry.get());
		delayedRetry.get().run();
		asyncTasks.removeFirst().run();
		asyncTasks.removeFirst().run();
		assertEquals(java.util.List.of("first", "first", "second"), processed);
	}

	@Test
	void shutdownSpillsTransportCallbackArrivingAfterQueueSnapshot(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getBukkitScheduler()).thenReturn(mock(BukkitScheduler.class));
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-shutdown-test"));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyTransportManager transport = mock(BackendProxyTransportManager.class);
			setField(handler, "transportManager", transport);
			handler.activateInboundMessages();
			doAnswer(invocation -> {
				handler.dispatchIncomingAfterPublication(
						JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build(), mock(Runnable.class));
				return null;
			}).when(transport).close();

			handler.close();
			assertEquals(1, overflow.size(), "callback after snapshot must be spilled before transport retirement");
		} finally {
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(1, recovered.size(), "shutdown callback must survive process restart");
		} finally {
			recovered.close();
		}
	}

	@Test
	void shutdownWaitsForInFlightVoteBeforePersistingLaterVotes(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-shutdown-quiesce-test"));
		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			AtomicReference<java.util.function.Consumer<OrderedVoteOutcome>> completion = new AtomicReference<>();
			doAnswer(invocation -> {
				completion.set(invocation.getArgument(1));
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "active").build(), mock(Runnable.class));
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "later").build(), mock(Runnable.class));
			asyncTasks.removeFirst().run();
			CompletableFuture<Void> closing = CompletableFuture.runAsync(handler::close);
			Thread.sleep(50L);
			assertFalse(closing.isDone(), "shutdown must wait for normal in-flight completion");
			completion.get().accept(OrderedVoteOutcome.COMPLETE);
			closing.get(3, TimeUnit.SECONDS);
			assertEquals(1, overflow.size(), "only the later vote should be persisted for restart");
		} finally {
			overflow.close();
		}
	}

	@Test
	void shutdownTimeoutQuarantinesInMemoryVoteAndIgnoresLateCompletion(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-shutdown-timeout-test"));
		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		AtomicReference<java.util.function.Consumer<OrderedVoteOutcome>> completion = new AtomicReference<>();
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			doAnswer(invocation -> {
				completion.set(invocation.getArgument(1));
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			handler.activateInboundMessages();
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "active").build(), mock(Runnable.class));
			handler.dispatchIncomingAfterPublication(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "later").build(), mock(Runnable.class));
			asyncTasks.removeFirst().run();

			CompletableFuture.runAsync(handler::close).get(3, TimeUnit.SECONDS);
			long quarantineDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.failedSize() != 1 && System.nanoTime() < quarantineDeadline) Thread.sleep(10L);
			assertEquals(1, overflow.failedSize());
			assertEquals(1, overflow.size());
			completion.get().accept(OrderedVoteOutcome.COMPLETE);
			assertEquals(1, overflow.failedSize());
			assertEquals(1, overflow.size());
		} finally {
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(1, recovered.failedSize());
			assertEquals(1, recovered.size());
		} finally {
			recovered.close();
		}
	}

	@Test
	void shutdownTimeoutQuarantinesDurableHeadInsteadOfReplayingIt(@TempDir Path tempDir) throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("ordered-overflow-shutdown-timeout-test"));
		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		BackendOrderedVoteOverflowQueue overflow = new BackendOrderedVoteOverflowQueue(plugin);
		AtomicReference<java.util.function.Consumer<OrderedVoteOutcome>> completion = new AtomicReference<>();
		try {
			BackendProxyHandler handler = new BackendProxyHandler(plugin, new ProcessedVoteCache(), overflow);
			BackendProxyMessageRouter router = mock(BackendProxyMessageRouter.class);
			setField(handler, "messageRouter", router);
			doAnswer(invocation -> {
				completion.set(invocation.getArgument(1));
				return null;
			}).when(router).handleOrderedVote(any(JsonEnvelope.class), any());
			assertTrue(overflow.enqueue(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put("sequence", "active").build()));
			handler.activateInboundMessages();
			asyncTasks.removeFirst().run();

			CompletableFuture.runAsync(handler::close).get(3, TimeUnit.SECONDS);
			long quarantineDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
			while (overflow.failedSize() != 1 && System.nanoTime() < quarantineDeadline) Thread.sleep(10L);
			assertEquals(1, overflow.failedSize());
			assertEquals(0, overflow.size());
			completion.get().accept(OrderedVoteOutcome.COMPLETE);
			assertEquals(1, overflow.failedSize());
			assertEquals(0, overflow.size());
		} finally {
			overflow.close();
		}
		BackendOrderedVoteOverflowQueue recovered = new BackendOrderedVoteOverflowQueue(plugin);
		try {
			assertEquals(1, recovered.failedSize());
			assertEquals(0, recovered.size());
		} finally {
			recovered.close();
		}
	}

	@Test
	void orderedVoteHandoffMovesPausedPrefixAndForwardsLateCallbacks() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler previous = new BackendProxyHandler(plugin);
		BackendProxyHandler replacement = new BackendProxyHandler(plugin);
		BackendProxyMessageRouter replacementRouter = mock(BackendProxyMessageRouter.class);
		setField(previous, "messageRouter", mock(BackendProxyMessageRouter.class));
		setField(replacement, "messageRouter", replacementRouter);
		previous.activateInboundMessages();
		previous.pauseOrderedVoteDispatchForReplacement(System.nanoTime() + TimeUnit.SECONDS.toNanos(1));

		JsonEnvelope first = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).put("sequence", "1").build();
		JsonEnvelope second = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).put("sequence", "2").build();
		previous.dispatchIncomingAfterPublication(first, mock(Runnable.class));
		previous.dispatchIncomingAfterPublication(second, mock(Runnable.class));

		previous.completeOrderedVoteHandoff(replacement);
		JsonEnvelope late = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_ONLINE).put("sequence", "3").build();
		previous.dispatchIncomingAfterPublication(late, mock(Runnable.class));

		ArrayDeque<Runnable> asyncTasks = new ArrayDeque<>();
		doAnswer(invocation -> {
			asyncTasks.addLast(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		java.util.ArrayList<String> sequences = new java.util.ArrayList<>();
		doAnswer(invocation -> {
			JsonEnvelope envelope = invocation.getArgument(0);
			sequences.add(envelope.getFields().get("sequence"));
			invocation.<java.util.function.Consumer<OrderedVoteOutcome>>getArgument(1)
					.accept(OrderedVoteOutcome.COMPLETE);
			return null;
		}).when(replacementRouter).handleOrderedVote(any(JsonEnvelope.class), any());

		replacement.activateInboundMessages();
		while (!asyncTasks.isEmpty() || sequences.size() < 3) {
			assertFalse(asyncTasks.isEmpty(), "handoff should keep scheduling the transferred FIFO");
			asyncTasks.removeFirst().run();
		}

		assertEquals(java.util.List.of("1", "2", "3"), sequences);
	}

	@Test
	void votePartyHandoffFencesStalePredecessorCloseAndRollbackRestoresIt() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyHandler previous = new BackendProxyHandler(plugin);
		BackendProxyHandler replacement = new BackendProxyHandler(plugin);
		com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync previousState =
				mock(com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync.class);
		com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync replacementState =
				mock(com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync.class);
		when(previousState.getCurrent()).thenReturn(7);
		when(previousState.getRequired()).thenReturn(25);
		setField(previous, "votePartySync", previousState);
		setField(replacement, "votePartySync", replacementState);
		setField(previous, "orderedVoteDispatchPaused", true);

		previous.completeVotePartyHandoff(replacement);
		verify(replacementState).replace(7, 25);
		previous.close();
		verify(previousState, never()).persist();

		BackendProxyHandler rolledBack = new BackendProxyHandler(plugin);
		com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync rolledBackState =
				mock(com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync.class);
		setField(rolledBack, "votePartySync", rolledBackState);
		setField(rolledBack, "orderedVoteDispatchPaused", true);
		rolledBack.completeVotePartyHandoff(replacement);
		rolledBack.resumeOrderedVoteDispatchAfterFailedReplacement();
		rolledBack.close();
		verify(rolledBackState).persist();
	}


	@Test
	void stagedInboundWaitsForPublicationAndUsesTheBukkitScheduler() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		JsonEnvelope envelope = JsonEnvelope.builder("staged").build();
		Runnable localDispatch = mock(Runnable.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		CountDownLatch started = new CountDownLatch(1);

		CompletableFuture<Void> callback = CompletableFuture.runAsync(() -> {
			started.countDown();
			handler.dispatchIncomingAfterPublication(envelope, localDispatch);
		});
		assertTrue(started.await(1, TimeUnit.SECONDS));
		assertFalse(callback.isDone());
		verifyNoInteractions(scheduler);

		handler.activateInboundMessages();
		callback.get(1, TimeUnit.SECONDS);
		verify(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(localDispatch, never()).run();
		scheduled.get().run();
		verify(localDispatch).run();
	}

	@Test
	void stagedInboundRoutesThroughRestoredPredecessorOnRollback() throws Exception {
		BackendProxyHandler previous = new BackendProxyHandler(null);
		BackendProxyHandler replacement = new BackendProxyHandler(null);
		GlobalMessageHandler previousMessages = mock(GlobalMessageHandler.class);
		setField(previous, "globalMessageHandler", previousMessages);
		JsonEnvelope envelope = JsonEnvelope.builder("rollback").build();
		Runnable replacementDispatch = mock(Runnable.class);
		CountDownLatch started = new CountDownLatch(1);

		CompletableFuture<Void> callback = CompletableFuture.runAsync(() -> {
			started.countDown();
			replacement.dispatchIncomingAfterPublication(envelope, replacementDispatch);
		});
		assertTrue(started.await(1, TimeUnit.SECONDS));
		assertFalse(callback.isDone());

		replacement.abortStagedInboundTo(previous);
		callback.get(1, TimeUnit.SECONDS);
		verify(previousMessages).onMessage(envelope);
		verify(replacementDispatch, never()).run();
	}

	@Test
	void failedPluginMessagePublicationRestoresPreviousSharedState() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage pluginMessages =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		GlobalMessageHandler previousHandler = mock(GlobalMessageHandler.class);
		GlobalMessageHandler replacementHandler = mock(GlobalMessageHandler.class);
		AtomicReference<String> activeChannel = new AtomicReference<>("old:channel");
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getPluginMessaging()).thenReturn(pluginMessages);
		when(plugin.getBungeeChannel()).thenAnswer(ignored -> activeChannel.get());
		org.mockito.Mockito.doAnswer(invocation -> {
			activeChannel.set(invocation.getArgument(0));
			return null;
		}).when(plugin).registerBungeeChannels(org.mockito.ArgumentMatchers.anyString());

		when(settings.getPluginMessagingChannel()).thenReturn("old:channel");
		PluginMessagingBackendProxyTransport previous = new PluginMessagingBackendProxyTransport(plugin);
		previous.start(previousHandler);
		previous.activateAfterPublication();

		when(settings.getPluginMessagingChannel()).thenReturn("new:channel");
		PluginMessagingBackendProxyTransport replacement = new PluginMessagingBackendProxyTransport(plugin);
		replacement.start(replacementHandler);
		replacement.activateAfterPublication();
		replacement.close();
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		try {
			Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
			transportField.setAccessible(true);
			transportField.set(manager, previous);
			manager.restoreAfterFailedReplacement();
		} catch (ReflectiveOperationException failure) {
			throw new AssertionError(failure);
		}

		assertEquals("old:channel", activeChannel.get());
		verify(plugin, times(2)).activateBackendPluginMessageHandler(previousHandler);
		verify(plugin).activateBackendPluginMessageHandler(replacementHandler);
		verify(plugin).deactivateBackendPluginMessageHandler(replacementHandler);
	}

	@Test
	void stagedPluginMessageTransportDefersRelaySwapUntilPublication() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage pluginMessages =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		GlobalMessageHandler replacement = mock(GlobalMessageHandler.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getPluginMessaging()).thenReturn(pluginMessages);
		when(settings.getPluginMessagingChannel()).thenReturn("votingplugin:main");

		PluginMessagingBackendProxyTransport transport = new PluginMessagingBackendProxyTransport(plugin);
		transport.start(replacement);
		verify(plugin, never()).activateBackendPluginMessageHandler(replacement);
		verify(plugin, never()).registerBungeeChannels(org.mockito.ArgumentMatchers.anyString());
		verifyNoInteractions(pluginMessages);
		transport.close();
		verify(plugin, never()).deactivateBackendPluginMessageHandler(replacement);
		verifyNoInteractions(pluginMessages);

		transport = new PluginMessagingBackendProxyTransport(plugin);
		transport.start(replacement);
		transport.activateAfterPublication();
		transport.activateAfterPublication();
		verify(plugin, times(1)).registerBungeeChannels("votingplugin:main");
		verify(pluginMessages).setEncryptionHandler(null);
		verify(pluginMessages).setDebug(false);
		verify(plugin, times(1)).activateBackendPluginMessageHandler(replacement);

		transport.close();
		verify(plugin).deactivateBackendPluginMessageHandler(replacement);
	}

	@Test
	void movingPluginMessageRelayDetachesItFromThePreviousPluginMessage() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class,
				CALLS_REAL_METHODS);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage first =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage second =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		java.util.ArrayList<PluginMessageHandler> firstHandlers = new java.util.ArrayList<>();
		java.util.ArrayList<PluginMessageHandler> secondHandlers = new java.util.ArrayList<>();
		GlobalMessageHandler firstTarget = mock(GlobalMessageHandler.class);
		GlobalMessageHandler secondTarget = mock(GlobalMessageHandler.class);
		AtomicReference<com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage> current =
				new AtomicReference<>(first);
		when(plugin.getPluginMessaging()).thenAnswer(ignored -> current.get());
		when(first.getPluginMessages()).thenReturn(firstHandlers);
		when(second.getPluginMessages()).thenReturn(secondHandlers);
		org.mockito.Mockito.doAnswer(invocation -> firstHandlers.add(invocation.getArgument(0))).when(first)
				.add(org.mockito.ArgumentMatchers.any(PluginMessageHandler.class));
		org.mockito.Mockito.doAnswer(invocation -> secondHandlers.add(invocation.getArgument(0))).when(second)
				.add(org.mockito.ArgumentMatchers.any(PluginMessageHandler.class));
		Field targetField = com.bencodez.votingplugin.VotingPluginMain.class
				.getDeclaredField("backendPluginMessageTarget");
		targetField.setAccessible(true);
		targetField.set(plugin, new AtomicReference<GlobalMessageHandler>());

		plugin.activateBackendPluginMessageHandler(firstTarget);
		current.set(second);
		plugin.activateBackendPluginMessageHandler(secondTarget);

		assertTrue(firstHandlers.isEmpty());
		assertEquals(1, secondHandlers.size());
		secondHandlers.get(0).onReceive(null);
		verify(firstTarget, never()).onMessage(null);
		verify(secondTarget).onMessage(null);
	}

	@Test
	void failedPresenceActivationDoesNotAnnounceReplacementGeneration() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getServer()).thenReturn("lobby");
		when(plugin.getTimer()).thenReturn(timer);
		when(timer.scheduleAtFixedRate(org.mockito.ArgumentMatchers.any(Runnable.class),
				org.mockito.ArgumentMatchers.anyLong(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any())).thenThrow(new java.util.concurrent.RejectedExecutionException());

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		assertThrows(java.util.concurrent.RejectedExecutionException.class, presence::start);

		verifyNoInteractions(messages);
	}

	@Test
	void preparedDisablePropagatesRejectedStoppedPresence() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);
		@SuppressWarnings("unchecked")
		java.util.concurrent.ConcurrentHashMap<String, com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession> sessions =
				(java.util.concurrent.ConcurrentHashMap<String, com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession>) getField(
						presence, "playerSessions");
		sessions.put("player", com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession
				.create("Player", "550e8400-e29b-41d4-a716-446655440000"));
		setField(presence, "lastResyncRequestId", java.util.UUID.randomUUID());
		setField(presence, "lastResyncRequestAtNanos", 42L);
		setField(presence, "lastSnapshotRequestId", java.util.UUID.randomUUID());
		setField(presence, "lastSnapshotRequestAtNanos", 84L);
		doThrow(new IllegalStateException("outbound queue full")).when(messages).sendMessage(any());

		assertThrows(IllegalStateException.class, presence::stopForDisable);
		assertFalse(presence.isReporting());
		assertNull(getField(presence, "incarnationId"));
		assertEquals(0, presence.getTrackedSessionCount());
		assertNull(getField(presence, "lastResyncRequestId"));
		assertEquals(0L, getField(presence, "lastResyncRequestAtNanos"));
		assertNull(getField(presence, "lastSnapshotRequestId"));
		assertEquals(0L, getField(presence, "lastSnapshotRequestAtNanos"));
	}

	@Test
	void delayedPreparedDisableCannotStopANewerPresenceGeneration() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		org.bukkit.Server server = mock(org.bukkit.Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getServer()).thenReturn(server);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(server.isPrimaryThread()).thenReturn(false);
		AtomicReference<Runnable> queued = new AtomicReference<>();
		org.mockito.Mockito.doAnswer(invocation -> {
			queued.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.PLUGINMESSAGING, messages);
		java.util.UUID firstIncarnation = java.util.UUID.randomUUID();
		setField(presence, "lifecycleGeneration", 1L);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", firstIncarnation);
		setField(presence, "startedAt", 1L);

		assertThrows(IllegalStateException.class,
				() -> presence.stopForDisable(System.nanoTime() - 1L));
		assertNotNull(queued.get());

		java.util.UUID replacementIncarnation = java.util.UUID.randomUUID();
		setField(presence, "lifecycleGeneration", 2L);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", replacementIncarnation);
		setField(presence, "startedAt", 2L);

		queued.get().run();

		assertTrue(presence.isReporting());
		assertEquals(replacementIncarnation, getField(presence, "incarnationId"));
		verifyNoInteractions(messages);
	}

	@Test
	void preparedDisableSchedulesStoppedPresenceOnTheBukkitThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		org.bukkit.Server server = mock(org.bukkit.Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getServer()).thenReturn(server);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(server.isPrimaryThread()).thenReturn(false);
		java.util.concurrent.atomic.AtomicBoolean onScheduledTask = new java.util.concurrent.atomic.AtomicBoolean();
		org.mockito.Mockito.doAnswer(invocation -> {
			onScheduledTask.set(true);
			try {
				Runnable task = invocation.getArgument(1);
				task.run();
			} finally {
				onScheduledTask.set(false);
			}
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			assertTrue(onScheduledTask.get(), "stopped presence must be sent by the scheduled Bukkit task");
			return null;
		}).when(messages).sendMessage(any());

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.PLUGINMESSAGING, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);

		presence.stopForDisable();

		verify(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(messages).sendMessage(any());
		assertFalse(presence.isReporting());
	}

	@Test
	void preparedDisableKeepsHttpStoppedPresenceOffTheBukkitThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		org.bukkit.Server server = mock(org.bukkit.Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getServer()).thenReturn(server);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(server.isPrimaryThread()).thenReturn(false);

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);

		presence.stopForDisable();

		verify(scheduler, never()).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(messages).sendMessage(any());
		assertFalse(presence.isReporting());
	}

	@Test
	void failedPreparedDisableCanRestartPresenceDuringRollback() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		setField(handler, "presenceManager", presence);
		setField(handler, "presenceReportingActivated", true);
		doThrow(new IllegalStateException("outbound queue full")).when(presence).stopForDisable();

		assertThrows(IllegalStateException.class, handler::preparePresenceForDisable);
		handler.restorePresenceAfterFailedDisablePreparation();

		org.mockito.InOrder rollback = inOrder(presence);
		rollback.verify(presence).stopForDisable();
		rollback.verify(presence).start();
	}

	@Test
	void stagedPresenceStartsOnlyAtExplicitPublication() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		Field field = BackendProxyHandler.class.getDeclaredField("presenceManager");
		field.setAccessible(true);
		field.set(handler, presence);

		verifyNoInteractions(presence);
		handler.activatePresenceReporting();
		handler.activatePresenceReporting();

		verify(presence, times(1)).start();
	}

	@Test
	void failedPresenceStartKeepsTransportBehindPublicationBarrier() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException()).when(presence).start();
		Field presenceField = BackendProxyHandler.class.getDeclaredField("presenceManager");
		presenceField.setAccessible(true);
		presenceField.set(handler, presence);

		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		assertThrows(java.util.concurrent.RejectedExecutionException.class, handler::activatePresenceReporting);

		verify(presence).start();
		verify(transport, never()).activateAfterPublication();
	}

	@Test
	void sharesVoteDeduplicationAcrossHandlerReplacement() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		BackendProxyHandler previous = new BackendProxyHandler(null, cache);
		BackendProxyHandler replacement = new BackendProxyHandler(null, cache);

		assertSame(previous.getProcessedWireVotes(), replacement.getProcessedWireVotes());
	}

	@Test
	void keepsPluginMessageRelayActiveUntilAtomicTargetSwap() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		Field method = BackendProxyHandler.class.getDeclaredField("method");
		method.setAccessible(true);
		method.set(handler, BungeeMethod.PLUGINMESSAGING);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		handler.prepareForReplacement(BungeeMethod.PLUGINMESSAGING);

		verifyNoInteractions(transport);
	}

	@Test
	void releasesSocketListenerBeforeSamePortReplacement() throws Exception {
		SocketBackendProxyTransport handler = new SocketBackendProxyTransport(null);
		SocketHandler socket = mock(SocketHandler.class);
		Field field = SocketBackendProxyTransport.class.getDeclaredField("socketHandler");
		field.setAccessible(true);
		field.set(handler, socket);

		handler.prepareForReplacement();

		verify(socket).closeConnection();
		assertNull(handler.getSocketHandler());
	}

	@Test
	void ordinarySocketCloseToleratesListenerTimeoutButReplacementPreparationRejectsIt() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		SocketHandler ordinarySocket = mock(SocketHandler.class);
		com.bencodez.simpleapi.servercomm.sockets.SocketServer ordinaryServer =
				mock(com.bencodez.simpleapi.servercomm.sockets.SocketServer.class);
		when(ordinarySocket.getServer()).thenReturn(ordinaryServer);
		when(ordinaryServer.isAlive()).thenReturn(true);
		SocketBackendProxyTransport ordinary = new SocketBackendProxyTransport(plugin);
		setField(ordinary, "socketHandler", ordinarySocket);

		assertDoesNotThrow(ordinary::close);
		verify(ordinarySocket).closeConnection();
		verify(ordinaryServer).close();

		SocketHandler replacementSocket = mock(SocketHandler.class);
		com.bencodez.simpleapi.servercomm.sockets.SocketServer replacementServer =
				mock(com.bencodez.simpleapi.servercomm.sockets.SocketServer.class);
		when(replacementSocket.getServer()).thenReturn(replacementServer);
		when(replacementServer.isAlive()).thenReturn(true);
		SocketBackendProxyTransport replacement = new SocketBackendProxyTransport(plugin);
		setField(replacement, "socketHandler", replacementSocket);

		assertThrows(IllegalStateException.class, replacement::prepareForReplacement);
		verify(replacementSocket).closeConnection();
		verify(replacementServer).close();
	}

	@Test
	void rejectsSamePortSocketReplacementUntilThePreviousListenerIsPrepared(@TempDir Path dataFolder)
			throws Exception {
		int port;
		try (ServerSocket allocation = new ServerSocket(0)) {
			port = allocation.getLocalPort();
		}
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getName()).thenReturn("socket-lifecycle-test");
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("socket-lifecycle-test"));
		when(settings.getBungeeServerHost()).thenReturn("127.0.0.1");
		when(settings.getBungeeServerPort()).thenReturn(1);
		when(settings.getSpigotServerHost()).thenReturn("127.0.0.1");
		when(settings.getSpigotServerPort()).thenReturn(port);

		SocketBackendProxyTransport previous = new SocketBackendProxyTransport(plugin);
		SocketBackendProxyTransport replacement = new SocketBackendProxyTransport(plugin);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		try {
			previous.start(messages);
			assertThrows(IllegalStateException.class, () -> replacement.start(messages));
			assertNull(replacement.getClientHandler());

			previous.prepareForReplacement();
			assertDoesNotThrow(() -> replacement.start(messages));
			replacement.close();
			assertDoesNotThrow(previous::restoreAfterFailedReplacement);
			assertNotNull(previous.getSocketHandler());
		} finally {
			replacement.close();
			previous.close();
		}
	}

	@Test
	void keepsRedisSubscriberUntilReplacementIsReady() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		Field method = BackendProxyHandler.class.getDeclaredField("method");
		method.setAccessible(true);
		method.set(handler, BungeeMethod.REDIS);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null);
		RedisHandler redis = mock(RedisHandler.class);
		Field field = RedisBackendProxyTransport.class.getDeclaredField("redisHandler");
		field.setAccessible(true);
		field.set(transport, redis);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		handler.prepareForReplacement(BungeeMethod.REDIS);

		verifyNoInteractions(redis);
		assertSame(redis, handler.getRedisHandler());
	}

	@Test
	void releasesMqttSubscriberBeforeSameMethodReplacement() throws Exception {
		MqttBackendProxyTransport handler = new MqttBackendProxyTransport(null);
		MqttHandler mqtt = mock(MqttHandler.class);
		Field field = MqttBackendProxyTransport.class.getDeclaredField("mqttHandler");
		field.setAccessible(true);
		field.set(handler, mqtt);

		handler.prepareForReplacement();

		verify(mqtt).disconnect();
		assertNull(handler.getMqttHandler());
	}

	@Test
	void preparesSameSocketReplacementByRetiringTheExistingListener() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.SOCKETS);
		SocketHandler socket = mock(SocketHandler.class);
		SocketBackendProxyTransport transport = new SocketBackendProxyTransport(null);
		setField(transport, "socketHandler", socket);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.SOCKETS));

		assertNull(handler.getSocketHandler());
		verify(socket).closeConnection();
	}

	@Test
	void preparesSameMqttReplacementByRetiringTheExistingClient() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.MQTT);
		MqttHandler mqtt = mock(MqttHandler.class);
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null);
		setField(transport, "mqttHandler", mqtt);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.MQTT));

		assertNull(handler.getMqttHandler());
		verify(mqtt).disconnect();
	}

	@Test
	void restoresPreparedMqttClientWhenReplacementValidationIsAbandoned() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null);
		MqttBackendProxyTransport mqtt = mock(MqttBackendProxyTransport.class);
		setField(manager, "preparedTransport", mqtt);

		manager.restoreAfterFailedReplacement();

		assertSame(mqtt, transport(manager));
		verify(mqtt).restoreAfterFailedReplacement();
	}

	@Test
	void failedMqttDisconnectKeepsAStillConnectedPredecessor() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null);
		MqttBackendProxyTransport mqtt = mock(MqttBackendProxyTransport.class);
		doThrow(new IllegalStateException("disconnect failed")).when(mqtt).prepareForReplacement();
		when(mqtt.isConnected()).thenReturn(true);
		setField(manager, "transport", mqtt);

		assertThrows(IllegalStateException.class, manager::prepareForReplacement);

		assertSame(mqtt, transport(manager));
		verify(mqtt, never()).restoreAfterFailedReplacement();
	}

	@Test
	void disconnectsPartialMqttClientWhenSubscriptionFails() throws Exception {
		MqttHandler partial = mock(MqttHandler.class);
		doThrow(new MqttException(0)).when(partial).subscribeEnvelopes(any(), any());
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null) {
			@Override
			protected MqttServerComm createMqttServerComm() {
				return mock(MqttServerComm.class);
			}

			@Override
			protected MqttHandler createMqttHandler(MqttServerComm server) {
				return partial;
			}
		};
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		setField(transport, "clientId", "backend-1");
		setField(transport, "brokerUrl", "tcp://broker.invalid:1883");
		setField(transport, "subscriptionTopic", "votingplugin/servers/backend-1");

		assertThrows(IllegalStateException.class, transport::restoreAfterFailedReplacement);

		assertNull(transport.getMqttHandler());
		verify(partial).disconnect();
	}

	@Test
	void failedSameMysqlReplacementLeavesExistingTransportAvailable() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.MYSQL);
		MySqlMessenger messenger = mock(MySqlMessenger.class);
		MysqlBackendProxyTransport transport = new MysqlBackendProxyTransport(null);
		setField(transport, "messenger", messenger);
		setTransport(handler, transport);

		assertFalse(handler.prepareForReplacement(BungeeMethod.MYSQL));

		assertSame(messenger, handler.getBackendMysqlMessenger());
		verifyNoInteractions(messenger);
	}

	@Test
	void preparesHttpDeliveryQueueWhenSwitchingToAnotherMethod() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.HTTP);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.REDIS));

		verify(transport).prepareForReplacement();
	}

	@Test
	void redisSendKeepsTheChannelCapturedByTheActiveTransport() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin =
				mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getRedisPrefix()).thenReturn("new:");
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin);
		RedisHandler redis = mock(RedisHandler.class);
		setField(transport, "redisHandler", redis);
		setField(transport, "publishChannel", "old:VotingPlugin");

		transport.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("vote").build());

		verify(redis).publishEnvelope(eq("old:VotingPlugin"), any());
	}

	@Test
	void mqttSendKeepsTheTopicCapturedByTheActiveTransport() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin =
				mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getMqttPrefix()).thenReturn("new/");
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(plugin);
		MqttHandler mqtt = mock(MqttHandler.class);
		setField(transport, "mqttHandler", mqtt);
		setField(transport, "publishTopic", "old/votingplugin/servers/proxy");

		assertTrue(transport.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("vote").build()));

		verify(mqtt).publishEnvelope(eq("old/votingplugin/servers/proxy"), any());
	}

	@Test
	void mqttAndMysqlReportRejectedHandoffDeliveries() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		MqttBackendProxyTransport mqttTransport = new MqttBackendProxyTransport(plugin);
		MqttHandler mqtt = mock(MqttHandler.class);
		setField(mqttTransport, "mqttHandler", mqtt);
		setField(mqttTransport, "publishTopic", "votingplugin/servers/proxy");
		doThrow(new IllegalStateException("publish failed")).when(mqtt).publishEnvelope(any(), any());
		JsonEnvelope mqttEnvelope = JsonEnvelope.builder("mqtt").build();

		assertFalse(mqttTransport.send(mqttEnvelope));

		MysqlBackendProxyTransport mysqlTransport = new MysqlBackendProxyTransport(plugin);
		MySqlMessenger mysql = mock(MySqlMessenger.class);
		setField(mysqlTransport, "messenger", mysql);
		doThrow(new java.sql.SQLException("write failed")).when(mysql).sendToProxy(any());

		assertFalse(mysqlTransport.send(JsonEnvelope.builder("mysql").build()));
	}

	@Test
	void asyncHandoffRetainsARejectedEntryUntilAcceptance() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("retry").build();
		when(replacementTransport.send(envelope)).thenReturn(false, true);
		setField(source, "transport", sourceTransport);
		setField(replacement, "transport", replacementTransport);

		replacement.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(envelope);
		source.completePreparedTransportHandoff(replacement);

		verify(replacementTransport, org.mockito.Mockito.timeout(1500).times(2)).send(envelope);
		replacement.awaitAsyncHandoff(System.nanoTime() + TimeUnit.SECONDS.toNanos(1));
		assertFalse(replacement.hasPendingAsyncHandoff());
	}

	@Test
	void forwardsMessagesBufferedDuringPreparedHttpReplacement() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport previousTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		when(replacementTransport.send(any())).thenReturn(true);
		setField(previous, "transport", previousTransport);
		setField(replacement, "transport", replacementTransport);
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope duringValidation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("during-validation").build();
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPublication =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-publication").build();
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope replacementStarted =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("replacement-started").build();

		replacement.beginPreparedTransportHandoff();
		previous.prepareForReplacement();
		previous.send(duringValidation);
		replacement.send(replacementStarted);
		verifyNoInteractions(replacementTransport);

		previous.completePreparedTransportHandoff(replacement);
		previous.send(afterPublication);

		org.mockito.InOrder order = inOrder(replacementTransport);
		order.verify(replacementTransport, org.mockito.Mockito.timeout(1000)).send(duringValidation);
		order.verify(replacementTransport).send(replacementStarted);
		order.verify(replacementTransport).send(afterPublication);
	}

	@Test
	void crossTransportPreparedHandoffDoesNotPerformNetworkSendOnCallerThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport previousTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		setField(previous, "transport", previousTransport);
		setField(replacement, "transport", replacementTransport);
		java.util.concurrent.CountDownLatch sendStarted = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch releaseSend = new java.util.concurrent.CountDownLatch(1);
		doAnswer(invocation -> {
			sendStarted.countDown();
			releaseSend.await(2, java.util.concurrent.TimeUnit.SECONDS);
			return true;
		}).when(replacementTransport).send(any());

		replacement.beginPreparedTransportHandoff();
		previous.prepareForReplacement();
		previous.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		org.junit.jupiter.api.Assertions.assertTimeoutPreemptively(java.time.Duration.ofSeconds(1),
				() -> previous.completePreparedTransportHandoff(replacement));

		org.junit.jupiter.api.Assertions.assertTrue(sendStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
		releaseSend.countDown();
		verify(replacementTransport, org.mockito.Mockito.timeout(1000)).send(any());
	}

	@Test
	void preparedDisableRefusesToDiscardMessagesAcceptedDuringPreparation() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setField(manager, "transport", transport);

		manager.prepareForReplacement();
		manager.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("during-disable").build());

		assertFalse(manager.commitPreparedDisable());
		verify(transport, never()).close();
	}

	@Test
	void preparedDisableRejectsOrdinarySendsButDeliversOnlyOneFinalStoppedPresence() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger(getClass().getName()));
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		setField(manager, "transport", transport);
		JsonEnvelope ordinary = JsonEnvelope.builder("ordinary").build();
		JsonEnvelope stopped = com.bencodez.votingplugin.proxy.VotingPluginWire.backendStopped("backend-1");
		when(transport.send(stopped)).thenReturn(true);

		manager.beginPreparedDisable();
		manager.send(ordinary);
		manager.send(stopped);
		manager.send(stopped);

		verify(transport, never()).send(ordinary);
		verify(transport, times(1)).send(stopped);
	}

	@Test
	void preparedHttpDisableDeliversTheFinalStoppedPresenceBeforeClosingForReplacement() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setField(manager, "transport", transport);
		JsonEnvelope stopped = com.bencodez.votingplugin.proxy.VotingPluginWire.backendStopped("backend-1");
		when(transport.send(stopped)).thenReturn(true);

		manager.beginPreparedDisable();
		manager.send(stopped);
		manager.prepareForReplacement();
		assertTrue(manager.commitPreparedDisable());

		org.mockito.InOrder disable = inOrder(transport);
		disable.verify(transport).send(stopped);
		disable.verify(transport).prepareForReplacement();
	}

	@Test
	void laterReplacementWaitsForAnEarlierAsyncHandoff() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyHandler target = handlerWithTransport(BungeeMethod.REDIS);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager targetManager = (BackendProxyTransportManager) managerField.get(target);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport targetTransport = mock(BackendProxyTransport.class);
		setField(source, "transport", sourceTransport);
		setField(targetManager, "transport", targetTransport);
		java.util.concurrent.CountDownLatch sendStarted = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch releaseSend = new java.util.concurrent.CountDownLatch(1);
		doAnswer(invocation -> {
			sendStarted.countDown();
			releaseSend.await(2, java.util.concurrent.TimeUnit.SECONDS);
			return true;
		}).when(targetTransport).send(any());

		targetManager.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		source.completePreparedTransportHandoff(targetManager);
		assertTrue(sendStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
		assertTrue(target.requiresPreparationForReplacement());
		java.util.concurrent.CompletableFuture<Boolean> preparation = java.util.concurrent.CompletableFuture.supplyAsync(
				() -> target.prepareForReplacement(BungeeMethod.MQTT,
						System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(2)));
		try {
			Thread.sleep(50);
			assertFalse(preparation.isDone(), "replacement must wait while an admitted send is in flight");
		} finally {
			releaseSend.countDown();
		}
		assertTrue(preparation.get(1, java.util.concurrent.TimeUnit.SECONDS));
		assertFalse(targetManager.hasPendingAsyncHandoff());
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPreparation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-preparation").build();
		targetManager.send(afterPreparation);
		BackendProxyTransportManager next = new BackendProxyTransportManager(plugin);
		BackendProxyTransport nextTransport = mock(BackendProxyTransport.class);
		when(nextTransport.send(any())).thenReturn(true);
		setField(next, "transport", nextTransport);
		next.beginPreparedTransportHandoff();
		targetManager.completePreparedTransportHandoff(next);
		verify(nextTransport, org.mockito.Mockito.timeout(1000)).send(afterPreparation);
	}

	@Test
	void pluginMessageHandoffRunsThroughTheBukkitScheduler() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		java.util.concurrent.atomic.AtomicReference<Runnable> scheduled = new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager target = new BackendProxyTransportManager(plugin);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		PluginMessagingBackendProxyTransport targetTransport = mock(PluginMessagingBackendProxyTransport.class);
		when(targetTransport.send(any())).thenReturn(true);
		setField(source, "transport", sourceTransport);
		setField(target, "transport", targetTransport);

		target.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		source.completePreparedTransportHandoff(target);

		verify(targetTransport, never()).send(any());
		assertTrue(scheduled.get() != null);
		scheduled.get().run();
		verify(targetTransport).send(any());
		assertFalse(target.hasPendingAsyncHandoff());

		target.prepareAsyncHandoffForReplacement(
				System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(1));
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPreparation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-preparation").build();
		target.send(afterPreparation);
		BackendProxyTransportManager next = new BackendProxyTransportManager(plugin);
		BackendProxyTransport nextTransport = mock(BackendProxyTransport.class);
		when(nextTransport.send(any())).thenReturn(true);
		setField(next, "transport", nextTransport);
		next.beginPreparedTransportHandoff();
		target.completePreparedTransportHandoff(next);
		verify(nextTransport, org.mockito.Mockito.timeout(1000)).send(afterPreparation);
	}

	@Test
	void stalePluginMessageHandoffCallbackClearsItsScheduledFlag() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		java.util.concurrent.atomic.AtomicReference<Runnable> scheduled = new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		PluginMessagingBackendProxyTransport pluginMessages = mock(PluginMessagingBackendProxyTransport.class);
		setField(manager, "transport", pluginMessages);

		manager.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("queued").build());
		assertNotNull(scheduled.get());
		assertTrue((boolean) getField(manager, "pluginMessageHandoffScheduled"));
		setField(manager, "transport", null);

		scheduled.get().run();

		assertFalse((boolean) getField(manager, "pluginMessageHandoffScheduled"));
	}

	@Test
	void releasesMysqlSubscriberBeforeSameMethodReplacement() throws Exception {
		MysqlBackendProxyTransport handler = new MysqlBackendProxyTransport(null);
		MySqlMessenger messenger = mock(MySqlMessenger.class);
		Field field = MysqlBackendProxyTransport.class.getDeclaredField("messenger");
		field.setAccessible(true);
		field.set(handler, messenger);

		handler.prepareForReplacement();

		verify(messenger).shutdown();
		assertNull(handler.getMessenger());
	}

	@Test
	void promotesRedisReplacementAfterOldListenerIsFencedEvenWhenRetirementFails() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		java.util.logging.Logger logger = mock(java.util.logging.Logger.class);
		when(plugin.getLogger()).thenReturn(logger);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new IllegalStateException("listener did not stop")).when(oldTransport).closeForHandoff();
		doThrow(new IllegalStateException("connection still closing")).doNothing().when(oldTransport).close();

		previous.completeRedisHandoff(replacement);

		org.mockito.InOrder order = inOrder(newTransport, oldTransport);
		order.verify(oldTransport).closeForHandoff();
		order.verify(newTransport).activateAfterHandoff();
		assertSame(newTransport, transport(replacement));
		assertNull(transport(previous));

		assertDoesNotThrow(previous::close);
		assertDoesNotThrow(previous::close);

		verify(oldTransport, timeout(1_000).times(2)).close();
	}

	@Test
	void sameRedisHandoffBuffersOutboundSendsUntilPublicationAdmission() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("outbound-during-redis-handoff").build();
		when(newTransport.send(envelope)).thenReturn(true);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);

		// Validation fences the staged replacement before the old Redis listener is
		// retired. Its send FIFO must be admitted only after publication succeeds.
		replacement.beginPreparedTransportHandoff();
		previous.completeRedisHandoff(replacement);
		previous.send(envelope);

		verify(oldTransport, never()).send(envelope);
		verify(newTransport, never()).send(envelope);

		previous.completePreparedTransportHandoff(replacement);

		verify(newTransport, timeout(1_000).times(1)).send(envelope);
	}

	@Test
	void sameRedisHandoffRollbackDrainsBufferedOutboundSendsThroughRestoredListener() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("outbound-redis-rollback").build();
		when(oldTransport.send(envelope)).thenReturn(true);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);

		replacement.beginPreparedTransportHandoff();
		previous.completeRedisHandoff(replacement);
		previous.send(envelope);
		previous.restoreAfterFailedReplacement(replacement);

		verify(newTransport, never()).send(envelope);
		verify(oldTransport, timeout(1_000).times(1)).send(envelope);
	}

	@Test
	void doesNotPromoteRedisReplacementWhenOldCallbacksDoNotQuiesce() throws Exception {
		BackendProxyTransportManager previous = new BackendProxyTransportManager(null);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(null);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new RedisBackendProxyTransport.HandoffQuiescenceException("busy"))
				.when(oldTransport).closeForHandoff();

		assertThrows(RedisBackendProxyTransport.HandoffQuiescenceException.class,
				() -> previous.completeRedisHandoff(replacement));

		verify(newTransport, never()).activateAfterHandoff();
		assertSame(oldTransport, transport(previous));
		assertSame(newTransport, transport(replacement));
	}

	@Test
	void restoresOldRedisSubscriberWhenStandbyActivationFails() throws Exception {
		BackendProxyTransportManager previous = new BackendProxyTransportManager(null);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(null);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new IllegalStateException("handoff buffer overflowed")).when(newTransport).activateAfterHandoff();

		assertThrows(IllegalStateException.class, () -> previous.completeRedisHandoff(replacement));

		org.mockito.InOrder order = inOrder(oldTransport, newTransport);
		order.verify(oldTransport).closeForHandoff();
		order.verify(newTransport).activateAfterHandoff();
		order.verify(oldTransport).restoreAfterFailedHandoff(java.util.Collections.emptyList());
		assertSame(oldTransport, transport(previous));
		assertSame(newTransport, transport(replacement));
	}

	@Test
	void redisRollbackForcesAFreshPresenceGeneration() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager presence =
				mock(com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager.class);
		setField(handler, "presenceManager", presence);
		setField(handler, "presenceReportingActivated", true);

		handler.refreshPresenceAfterFailedReplacement();

		org.mockito.InOrder order = inOrder(presence);
		order.verify(presence).stop();
		order.verify(presence).start();
	}

	@Test
	void stopsGlobalDataTimerWhenHandlerIsReplaced() throws Exception {
		BackendGlobalDataSync handler = new BackendGlobalDataSync(null, null);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		Field field = BackendGlobalDataSync.class.getDeclaredField("timer");
		field.setAccessible(true);
		field.set(handler, timer);

		handler.close();

		verify(timer).shutdownNow();
		assertNull(handler.getTimer());
	}

	private BackendProxyHandler handlerWithTransport(BungeeMethod method) throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		setField(handler, "method", method);
		return handler;
	}

	private void setTransport(BackendProxyHandler handler, BackendProxyTransport transport) throws Exception {
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		setField(manager, "transport", transport);
	}

	private BackendProxyTransport transport(BackendProxyTransportManager manager) throws Exception {
		Field field = BackendProxyTransportManager.class.getDeclaredField("transport");
		field.setAccessible(true);
		return (BackendProxyTransport) field.get(manager);
	}

	private void setField(Object target, String name, Object value) throws Exception {
		Class<?> type = target.getClass();
		while (type != null) {
			try {
				Field field = type.getDeclaredField(name);
				field.setAccessible(true);
				field.set(target, value);
				return;
			} catch (NoSuchFieldException ignored) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldException(name);
	}

	private Object getField(Object target, String name) throws Exception {
		Class<?> type = target.getClass();
		while (type != null) {
			try {
				Field field = type.getDeclaredField(name);
				field.setAccessible(true);
				return field.get(target);
			} catch (NoSuchFieldException ignored) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldException(name);
	}
}
