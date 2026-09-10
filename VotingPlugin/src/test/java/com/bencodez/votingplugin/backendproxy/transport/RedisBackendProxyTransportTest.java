package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.spy;

import java.lang.reflect.Field;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.CopyOnWriteArrayList;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import redis.clients.jedis.DefaultJedisClientConfig;

class RedisBackendProxyTransportTest {

	@Test
	void validationHonorsTlsAndHostnameVerification() {
		DefaultJedisClientConfig config = RedisBackendProxyTransport.buildValidationClientConfig(3, "user", "secret",
				true);

		assertTrue(config.isSsl());
		assertEquals(3, config.getDatabase());
		assertEquals("HTTPS", config.getSslParameters().getEndpointIdentificationAlgorithm());
	}

	@Test
	void validationKeepsTlsDisabledByDefault() {
		assertFalse(RedisBackendProxyTransport.buildValidationClientConfig(0, null, null, false).isSsl());
	}

	@Test
	void legacyHandoffBufferDegradesAtItsFixedLimit() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, new ProcessedVoteCache());
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build();

		for (int index = 0; index <= RedisBackendProxyTransport.MAX_LEGACY_HANDOFF_DELIVERIES; index++) {
			transport.dispatchLegacy(envelope);
		}

		verify(messages, times(1)).onMessage(envelope);
	}

	@Test
	void legacyHandoffBufferDegradesForAnOversizedEnvelope() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, new ProcessedVoteCache());
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE)
				.put("payload", "x".repeat(ProcessedVoteCache.MAX_LEGACY_REDIS_DELIVERY_BYTES + 1)).build();

		transport.dispatchLegacy(envelope);

		verify(messages).onMessage(envelope);
	}

	@Test
	void retiredListenerDropsLegacyDeliveriesAfterShutdownTimeout() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, new ProcessedVoteCache());
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		transport.fenceAfterHandoff();

		transport.dispatchLegacy(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build());

		verifyNoInteractions(messages);
	}

	@Test
	void retiredListenerDoesNotReserveOrDispatchDeliveryIds() throws Exception {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		transport.fenceAfterHandoff();
		String deliveryId = "00000000-0000-0000-0000-000000000001";

		transport.dispatchIdentified(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build(), deliveryId);

		assertTrue(cache.reserveRedisDelivery(deliveryId), "retired listener must not consume shared deduplication state");
		verifyNoInteractions(messages);
	}

	@Test
	void standbyBuffersIdentifiedDeliveriesUntilItIsPublished() throws Exception {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		cache.registerRedisSubscriber(new Object());
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		Field identity = RedisBackendProxyTransport.class.getDeclaredField("subscriberIdentity");
		identity.setAccessible(true);
		cache.registerRedisSubscriber(identity.get(transport));
		Field standby = RedisBackendProxyTransport.class.getDeclaredField("standbySubscriber");
		standby.setAccessible(true);
		standby.setBoolean(transport, true);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		String deliveryId = "00000000-0000-0000-0000-000000000002";
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE)
				.put(VotingPluginWire.K_REDIS_DELIVERY_ID, deliveryId).build();

		transport.dispatchIdentified(envelope, deliveryId);

		verifyNoInteractions(messages);
		transport.activateAfterHandoff();
		verifyNoInteractions(messages);
		transport.replayAfterHandoffPublication();
		verify(messages).onMessage(envelope);
		assertFalse(cache.reserveRedisDelivery(deliveryId),
				"publication must reserve the delivery exactly when its buffered callback is replayed");
	}

	@Test
	void standbyReplaysMixedLegacyAndIdentifiedDeliveriesInArrivalOrder() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(true);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		setBoolean(transport, "standbySubscriber", true);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);
		JsonEnvelope legacy = JsonEnvelope.builder("legacy").build();
		String deliveryId = "00000000-0000-0000-0000-000000000003";
		JsonEnvelope identified = JsonEnvelope.builder("identified")
				.put(VotingPluginWire.K_REDIS_DELIVERY_ID, deliveryId).build();

		transport.dispatchLegacy(legacy);
		transport.dispatchIdentified(identified, deliveryId);
		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();

		org.mockito.InOrder order = org.mockito.Mockito.inOrder(messages);
		order.verify(messages).onMessage(legacy);
		order.verify(messages).onMessage(identified);
	}

	@Test
	void identifiedStandbyOverflowAbortsPublication() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, mock(ProcessedVoteCache.class));
		setBoolean(transport, "standbySubscriber", true);
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		for (int index = 0; index <= RedisBackendProxyTransport.MAX_IDENTIFIED_HANDOFF_DELIVERIES; index++) {
			String deliveryId = String.format("00000000-0000-0000-0000-%012d", index);
			transport.dispatchIdentified(JsonEnvelope.builder("identified")
					.put(VotingPluginWire.K_REDIS_DELIVERY_ID, deliveryId).build(), deliveryId);
		}

		assertThrows(IllegalStateException.class, transport::activateAfterHandoff);
	}

	@Test
	void oversizedLegacyStandbyDeliveryAbortsPublicationWithoutDispatching() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, mock(ProcessedVoteCache.class));
		setBoolean(transport, "standbySubscriber", true);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);
		JsonEnvelope oversized = JsonEnvelope.builder("legacy")
				.put("payload", "x".repeat(ProcessedVoteCache.MAX_LEGACY_REDIS_DELIVERY_BYTES + 1)).build();

		transport.dispatchLegacy(oversized);

		verifyNoInteractions(messages);
		assertThrows(IllegalStateException.class, transport::activateAfterHandoff);
	}

	@Test
	void legacyStandbyBufferOverflowAbortsPublicationWithoutDispatching() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, mock(ProcessedVoteCache.class));
		setBoolean(transport, "standbySubscriber", true);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);

		for (int index = 0; index <= RedisBackendProxyTransport.MAX_LEGACY_HANDOFF_DELIVERIES; index++)
			transport.dispatchLegacy(JsonEnvelope.builder("legacy-" + index).build());

		verifyNoInteractions(messages);
		assertThrows(IllegalStateException.class, transport::activateAfterHandoff);
	}

	@Test
	void activeLegacyAccountingOverflowAbortsLaggingStandbyPublication() throws Exception {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		Object previousIdentity = new Object();
		cache.registerRedisSubscriber(previousIdentity);
		RedisBackendProxyTransport replacement = new RedisBackendProxyTransport(null, cache);
		Field identity = RedisBackendProxyTransport.class.getDeclaredField("subscriberIdentity");
		identity.setAccessible(true);
		cache.registerRedisSubscriber(identity.get(replacement));
		setBoolean(replacement, "standbySubscriber", true);

		for (int index = 0; index <= 4096; index++) {
			cache.reserveLegacyRedisDelivery(previousIdentity, "old-only-" + index);
		}

		assertThrows(IllegalStateException.class, replacement::activateAfterHandoff);
	}

	@Test
	void fencingWaitsForAnIdentifiedMessageHandler() throws Exception {
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, new ProcessedVoteCache());
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			entered.countDown();
			try {
				release.await();
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
			}
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		Thread delivery = new Thread(() -> transport.dispatchIdentified(
				JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build(), "delivery"));
		delivery.start();
		assertTrue(entered.await(1, TimeUnit.SECONDS));

		CountDownLatch fenced = new CountDownLatch(1);
		java.util.concurrent.atomic.AtomicReference<Throwable> fenceFailure =
				new java.util.concurrent.atomic.AtomicReference<>();
		Thread retirement = new Thread(() -> {
			try {
				transport.fenceAfterHandoff();
			} catch (Throwable failure) {
				fenceFailure.set(failure);
			} finally {
				fenced.countDown();
			}
		});
		retirement.start();
		try {
			assertFalse(fenced.await(100, TimeUnit.MILLISECONDS));
			retirement.interrupt();
			assertTrue(fenced.await(1, TimeUnit.SECONDS));
		} finally {
			release.countDown();
		}
		delivery.join(TimeUnit.SECONDS.toMillis(1));
		retirement.join(TimeUnit.SECONDS.toMillis(1));
		assertFalse(delivery.isAlive());
		assertFalse(retirement.isAlive());
		assertTrue(fenceFailure.get() instanceof RedisBackendProxyTransport.HandoffQuiescenceException);
	}

	@Test
	void managerDoesNotHoldItsMonitorWhileWaitingForRedisCallbackReplies() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null);
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveLegacyRedisDelivery(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(true);
		RedisBackendProxyTransport transport = spy(new RedisBackendProxyTransport(null, cache));
		setField(manager, "transport", transport);
		CountDownLatch callbackEntered = new CountDownLatch(1);
		CountDownLatch allowReply = new CountDownLatch(1);
		CountDownLatch callbackCompleted = new CountDownLatch(1);
		JsonEnvelope reply = JsonEnvelope.builder("reply").build();
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			callbackEntered.countDown();
			allowReply.await();
			manager.send(reply);
			callbackCompleted.countDown();
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		setField(transport, "messageHandler", messages);

		Thread delivery = new Thread(() -> transport.dispatchLegacy(
				JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build()));
		delivery.start();
		assertTrue(callbackEntered.await(1, TimeUnit.SECONDS));

		java.util.concurrent.atomic.AtomicReference<Throwable> closeFailure =
				new java.util.concurrent.atomic.AtomicReference<>();
		BackendProxyTransportManager replacementManager = new BackendProxyTransportManager(null, cache);
		RedisBackendProxyTransport replacementTransport = mock(RedisBackendProxyTransport.class);
		when(replacementTransport.send(reply)).thenReturn(true);
		setField(replacementManager, "transport", replacementTransport);
		replacementManager.beginPreparedTransportHandoff();
		Thread retirement = new Thread(() -> {
			try {
				manager.closeRedisForHandoff(replacementManager);
			} catch (Throwable failure) {
				closeFailure.set(failure);
			}
		});
		retirement.start();
		try {
			assertTrue(awaitRetiredAfterHandoff(transport));

			allowReply.countDown();
			assertTrue(callbackCompleted.await(1, TimeUnit.SECONDS));
			retirement.join(TimeUnit.SECONDS.toMillis(1));
			delivery.join(TimeUnit.SECONDS.toMillis(1));
			assertFalse(retirement.isAlive());
			assertFalse(delivery.isAlive());
			assertNull(closeFailure.get());
			// The callback could re-enter manager.send because retirement did not hold
			// its monitor. The same-Redis fence retains that reply until the Bukkit
			// publication path admits it to the staged replacement.
			verify(transport, never()).send(reply);
			verify(replacementTransport, never()).send(reply);
			manager.completePreparedTransportHandoff(replacementManager);
			verify(replacementTransport, org.mockito.Mockito.timeout(1_000).times(1)).send(reply);
		} finally {
			allowReply.countDown();
			if (retirement.isAlive()) retirement.interrupt();
			delivery.join(TimeUnit.SECONDS.toMillis(1));
			retirement.join(TimeUnit.SECONDS.toMillis(1));
		}
	}

	@Test
	void activationFailureTransfersEveryBufferedStandbyOverlapBeforeReplacementClose() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveLegacyRedisDelivery(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		when(cache.reserveRedisDelivery("00000000-0000-0000-0000-000000001594")).thenReturn(true);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(null, cache);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(null, cache);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport standby = spy(new RedisBackendProxyTransport(null, cache));
		JsonEnvelope legacy = JsonEnvelope.builder("legacy-overlap").build();
		JsonEnvelope identified = JsonEnvelope.builder("identified-overlap").build();
		setBoolean(standby, "standbySubscriber", true);
		setField(standby, "messageHandler", mock(GlobalMessageHandler.class));
		standby.dispatchLegacy(legacy);
		standby.dispatchIdentified(identified, "00000000-0000-0000-0000-000000001594");
		org.mockito.Mockito.doThrow(new IllegalStateException("standby promotion rejected"))
				.when(standby).activateAfterHandoff();
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", standby);

		assertThrows(IllegalStateException.class, () -> previous.completeRedisHandoff(replacement));

		@SuppressWarnings("unchecked")
		org.mockito.ArgumentCaptor<java.util.List<JsonEnvelope>> restored =
				org.mockito.ArgumentCaptor.forClass(java.util.List.class);
		verify(oldTransport, times(1)).restoreAfterFailedHandoff(restored.capture());
		assertEquals(java.util.List.of(legacy, identified), restored.getValue(),
				"rollback must restore every staged overlap delivery in arrival order");

		standby.close();
		assertTrue(standby.detachReplayForFailedHandoff().isEmpty(),
				"closing the failed staged transport must not discard an untransferred delivery");
	}

	@Test
	void legacyPromotionWaitsForReservedDeliveryBeforeReplayingStandbyCopy() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveLegacyRedisDelivery(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(true, false);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(true);
		RedisBackendProxyTransport previous = new RedisBackendProxyTransport(null, cache);
		RedisBackendProxyTransport replacement = new RedisBackendProxyTransport(null, cache);
		setBoolean(replacement, "standbySubscriber", true);
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		GlobalMessageHandler previousMessages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			entered.countDown();
			release.await();
			return null;
		}).when(previousMessages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		GlobalMessageHandler replacementMessages = mock(GlobalMessageHandler.class);
		setField(previous, "messageHandler", previousMessages);
		setField(replacement, "messageHandler", replacementMessages);
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE_UPDATE).build();

		Thread delivery = new Thread(() -> previous.dispatchLegacy(envelope));
		delivery.start();
		assertTrue(entered.await(1, TimeUnit.SECONDS));
		replacement.dispatchLegacy(envelope);
		CountDownLatch promoted = new CountDownLatch(1);
		Thread promotion = new Thread(() -> {
			previous.fenceAfterHandoff();
			replacement.activateAfterHandoff();
			promoted.countDown();
		});
		promotion.start();
		assertFalse(promoted.await(100, TimeUnit.MILLISECONDS));

		release.countDown();
		assertTrue(promoted.await(1, TimeUnit.SECONDS));
		delivery.join(TimeUnit.SECONDS.toMillis(1));
		promotion.join(TimeUnit.SECONDS.toMillis(1));
		verify(previousMessages).onMessage(envelope);
		verifyNoInteractions(replacementMessages);
	}

	@Test
	void replayCompletesBeforeNewlyAdmittedDeliveries() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("new-delivery")).thenReturn(true);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		JsonEnvelope replayed = JsonEnvelope.builder("old").build();
		JsonEnvelope newer = JsonEnvelope.builder("new").build();
		CopyOnWriteArrayList<JsonEnvelope> order = new CopyOnWriteArrayList<>();
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			JsonEnvelope envelope = invocation.getArgument(0);
			order.add(envelope);
			if (envelope == replayed) {
				Thread arrival = new Thread(() -> transport.dispatchIdentified(newer, "new-delivery"));
				arrival.start();
				arrival.join(TimeUnit.SECONDS.toMillis(1));
			}
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		Field handler = RedisBackendProxyTransport.class.getDeclaredField("messageHandler");
		handler.setAccessible(true);
		handler.set(transport, messages);
		transport.dispatchLegacy(replayed);

		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();

		assertEquals(java.util.List.of(replayed, newer), order);
	}

	@Test
	void replayUsesBoundedBukkitBatchesFromItsDedicatedWorker() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		java.util.concurrent.atomic.AtomicInteger scheduled = new java.util.concurrent.atomic.AtomicInteger();
		doAnswer(invocation -> {
			scheduled.incrementAndGet();
			((Runnable) invocation.getArgument(1)).run();
			return null;
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.same(plugin),
				org.mockito.ArgumentMatchers.any(Runnable.class));
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin, cache);
		setBoolean(transport, "standbySubscriber", true);
		CountDownLatch replayed = new CountDownLatch(RedisBackendProxyTransport.REPLAY_BATCH_SIZE + 1);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			replayed.countDown();
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		setField(transport, "messageHandler", messages);
		for (int index = 0; index <= RedisBackendProxyTransport.REPLAY_BATCH_SIZE; index++)
			transport.dispatchLegacy(JsonEnvelope.builder("replay-" + index).build());

		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();

		assertTrue(replayed.await(1, TimeUnit.SECONDS));
		assertEquals(2, scheduled.get(), "a replay larger than one batch must not run in one Bukkit task");
	}

	@Test
	void fullReplayQueueBackpressuresNewCallbacksAndCloseWakesThemWithoutReserving() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("new-delivery")).thenReturn(true);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);
		setBoolean(transport, "replayingHandoff", true);
		Field queueField = RedisBackendProxyTransport.class.getDeclaredField("deliveriesAfterReplay");
		queueField.setAccessible(true);
		@SuppressWarnings("unchecked")
		java.util.ArrayDeque<JsonEnvelope> queue = (java.util.ArrayDeque<JsonEnvelope>) queueField.get(transport);
		for (int index = 0; index < RedisBackendProxyTransport.MAX_REPLAY_HANDOFF_DELIVERIES; index++)
			queue.addLast(JsonEnvelope.builder("queued-" + index).build());
		JsonEnvelope newer = JsonEnvelope.builder("new").build();
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch returned = new CountDownLatch(1);
		Thread callback = new Thread(() -> {
			entered.countDown();
			transport.dispatchIdentified(newer, "new-delivery");
			returned.countDown();
		});
		callback.start();
		assertTrue(entered.await(1, TimeUnit.SECONDS));
		assertFalse(returned.await(100, TimeUnit.MILLISECONDS),
				"a full replay queue must apply backpressure instead of overtaking replay FIFO");
		verify(cache, never()).reserveRedisDelivery("new-delivery");

		transport.close();
		assertTrue(returned.await(1, TimeUnit.SECONDS), "close must wake a blocked Redis callback");
		callback.join(TimeUnit.SECONDS.toMillis(1));

		verify(cache, never()).reserveRedisDelivery("new-delivery");
		verifyNoInteractions(messages);
	}

	@Test
	void timedOutPubSubCallbackIsRetriedUntilReplayCapacityReturns() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("received-delivery")).thenReturn(true);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		setBoolean(transport, "replayingHandoff", true);
		java.util.ArrayDeque<JsonEnvelope> queue = replayQueue(transport);
		for (int index = 0; index < RedisBackendProxyTransport.MAX_REPLAY_HANDOFF_DELIVERIES; index++)
			queue.addLast(JsonEnvelope.builder("queued-" + index).build());
		JsonEnvelope received = JsonEnvelope.builder("received").build();
		CountDownLatch completed = new CountDownLatch(1);
		Thread subscriber = new Thread(() -> {
			transport.dispatchReceivedSubscriberEnvelope(received, "received-delivery");
			completed.countDown();
		});
		subscriber.start();
		// The first bounded wait must time out without returning the consumed
		// Pub/Sub payload to Redis (which would lose it); the callback retries it.
		Thread.sleep(TimeUnit.SECONDS.toMillis(3) + 200L);
		verify(cache, never()).reserveRedisDelivery("received-delivery");
		Object lifecycle = lifecycle(transport);
		synchronized (lifecycle) {
			queue.removeFirst();
			lifecycle.notifyAll();
		}

		assertTrue(completed.await(1, TimeUnit.SECONDS));
		subscriber.join(TimeUnit.SECONDS.toMillis(1));
		verify(cache).reserveRedisDelivery("received-delivery");
		assertEquals(received, queue.getLast(), "the received payload must retain FIFO replay ownership");
		transport.close();
	}

	@Test
	void prePublicationRollbackDetachesPromotedReplayInArrivalOrder() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("before-publication")).thenReturn(true);
		when(cache.reserveRedisDelivery("during-publication")).thenReturn(true);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport replacement = new RedisBackendProxyTransport(null, cache);
		setBoolean(replacement, "standbySubscriber", true);
		setField(replacement, "messageHandler", mock(GlobalMessageHandler.class));
		JsonEnvelope before = JsonEnvelope.builder("before").build();
		JsonEnvelope during = JsonEnvelope.builder("during").build();

		replacement.dispatchIdentified(before, "before-publication");
		replacement.activateAfterHandoff();
		// This is the narrow interval after worker-side promotion but before Bukkit
		// opens publication/replay. It has already reserved its Redis identity.
		replacement.dispatchIdentified(during, "during-publication");

		assertEquals(java.util.List.of(before, during), replacement.detachReplayForFailedHandoff());
		assertTrue(replayQueue(replacement).isEmpty());
		verify(cache).reserveRedisDelivery("during-publication");
	}

	@Test
	void consecutiveRedisHandoffTransfersActiveReplayBeforeNewStandbyArrivals() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("new-overlap")).thenReturn(true);
		RedisBackendProxyTransport previous = new RedisBackendProxyTransport(null, cache);
		JsonEnvelope first = JsonEnvelope.builder("first").build();
		JsonEnvelope second = JsonEnvelope.builder("second").build();
		replayQueue(previous).addLast(first);
		replayQueue(previous).addLast(second);
		setBoolean(previous, "replayingHandoff", true);

		java.util.List<JsonEnvelope> transferred = previous.freezeReplayForSuccessiveHandoff();
		RedisBackendProxyTransport replacement = new RedisBackendProxyTransport(null, cache);
		setBoolean(replacement, "standbySubscriber", true);
		CopyOnWriteArrayList<JsonEnvelope> delivered = new CopyOnWriteArrayList<>();
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			delivered.add(invocation.getArgument(0));
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		setField(replacement, "messageHandler", messages);
		replacement.acceptReplayFromPreviousHandoff(transferred);
		JsonEnvelope newer = JsonEnvelope.builder("newer").build();
		replacement.dispatchIdentified(newer, "new-overlap");
		replacement.activateAfterHandoff();
		replacement.replayAfterHandoffPublication();

		assertEquals(java.util.List.of(first, second, newer), delivered,
				"a later same-Redis handoff must preserve the active replay FIFO ahead of new overlap arrivals");
	}

	@Test
	void callbackArrivingDuringFrozenTransferIsReleasedIntoRollbackReplay() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("during-freeze")).thenReturn(true);
		RedisBackendProxyTransport previous = new RedisBackendProxyTransport(null, cache);
		JsonEnvelope inherited = JsonEnvelope.builder("inherited").build();
		replayQueue(previous).addLast(inherited);
		setBoolean(previous, "replayingHandoff", true);
		java.util.List<JsonEnvelope> transferred = previous.freezeReplayForSuccessiveHandoff();
		JsonEnvelope duringFreeze = JsonEnvelope.builder("during-freeze").build();
		CountDownLatch completed = new CountDownLatch(1);
		Thread callback = new Thread(() -> {
			previous.dispatchIdentified(duringFreeze, "during-freeze");
			completed.countDown();
		});
		callback.start();
		assertFalse(completed.await(100, TimeUnit.MILLISECONDS),
				"the old subscriber must retain a consumed callback until transfer resolves");

		previous.restoreFrozenReplayAfterFailedSuccessiveHandoff(transferred);
		assertTrue(completed.await(1, TimeUnit.SECONDS));
		callback.join(TimeUnit.SECONDS.toMillis(1));
		assertEquals(java.util.List.of(inherited, duringFreeze), new java.util.ArrayList<>(replayQueue(previous)));
		verify(cache).reserveRedisDelivery("during-freeze");
	}

	@Test
	void successfulFrozenHandoffReleasesBlockedCallbackBeforeListenerClose() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		RedisBackendProxyTransport previous = new RedisBackendProxyTransport(null, cache);
		setField(previous, "messageHandler", mock(GlobalMessageHandler.class));
		replayQueue(previous).addLast(JsonEnvelope.builder("inherited").build());
		setBoolean(previous, "replayingHandoff", true);
		previous.freezeReplayForSuccessiveHandoff();
		CountDownLatch callbackDone = new CountDownLatch(1);
		Thread callback = new Thread(() -> {
			previous.dispatchIdentified(JsonEnvelope.builder("during-close").build(), "during-close");
			callbackDone.countDown();
		});
		callback.start();
		assertFalse(callbackDone.await(100, TimeUnit.MILLISECONDS));

		Thread close = new Thread(previous::closeForHandoff);
		close.start();
		close.join(TimeUnit.SECONDS.toMillis(1));
		assertFalse(close.isAlive(), "listener close must not wait for the frozen callback timeout");
		assertTrue(callbackDone.await(1, TimeUnit.SECONDS));
		callback.join(TimeUnit.SECONDS.toMillis(1));
		verify(cache, never()).reserveRedisDelivery("during-close");
	}

	@Test
	void redisToNonRedisTransitionDefersUntilActiveReplayIsEmpty() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		RedisBackendProxyTransport redis = new RedisBackendProxyTransport(null, cache);
		JsonEnvelope pending = JsonEnvelope.builder("pending-replay").build();
		replayQueue(redis).addLast(pending);
		setBoolean(redis, "replayingHandoff", true);
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null, cache);
		setField(manager, "transport", redis);

		assertFalse(manager.prepareRedisReplayTransition(BungeeMethod.HTTP, System.nanoTime()),
				"Redis-to-HTTP must defer rather than clearing active Redis replay");
		assertFalse(manager.prepareRedisReplayTransition(BungeeMethod.PLUGINMESSAGING, System.nanoTime()),
				"Redis-to-plugin-messaging must apply the same replay ownership gate");
		assertEquals(java.util.List.of(pending), new java.util.ArrayList<>(replayQueue(redis)));

		setBoolean(redis, "replayingHandoff", false);
		replayQueue(redis).clear();
		assertTrue(manager.prepareRedisReplayTransition(BungeeMethod.HTTP,
				System.nanoTime() + TimeUnit.SECONDS.toNanos(1)));
		assertTrue(manager.prepareRedisReplayTransition(BungeeMethod.PLUGINMESSAGING,
				System.nanoTime() + TimeUnit.SECONDS.toNanos(1)));
	}

	@Test
	void retiredRedisCleanupDoesNotBlockManagerCloseOnPublicationThread() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null, new ProcessedVoteCache());
		RedisBackendProxyTransport retired = mock(RedisBackendProxyTransport.class);
		CountDownLatch closeEntered = new CountDownLatch(1);
		CountDownLatch releaseClose = new CountDownLatch(1);
		doAnswer(invocation -> {
			closeEntered.countDown();
			releaseClose.await(1, TimeUnit.SECONDS);
			return null;
		}).when(retired).close();
		setField(manager, "retiredTransport", retired);

		long started = System.nanoTime();
		manager.close();
		long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started);

		assertTrue(elapsedMillis < 100L,
				"publication-facing manager close must not join a retired Redis listener");
		assertTrue(closeEntered.await(1, TimeUnit.SECONDS));
		releaseClose.countDown();
	}

	@Test
	void replayBatchIsRequeuedWhenBukkitSchedulingRejectsIt() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		CountDownLatch rejected = new CountDownLatch(1);
		doAnswer(invocation -> {
			rejected.countDown();
			throw new IllegalStateException("scheduler stopped");
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.same(plugin),
				org.mockito.ArgumentMatchers.any(Runnable.class));
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin, cache);
		setBoolean(transport, "standbySubscriber", true);
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		transport.dispatchLegacy(JsonEnvelope.builder("replay").build());
		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();

		assertTrue(rejected.await(1, TimeUnit.SECONDS));
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(1);
		while (replayTrackedDeliveries(transport) != 1 && System.nanoTime() < deadline)
			Thread.sleep(10L);
		assertEquals(1, replayTrackedDeliveries(transport),
				"a rejected Bukkit task must return its removed batch to replay ownership");
		transport.close();
	}

	@Test
	void interruptedReplayWorkerReturnsItsUnstartedBatch() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		CountDownLatch submitted = new CountDownLatch(1);
		doAnswer(invocation -> {
			submitted.countDown();
			return null; // emulate a Bukkit task that has not run yet
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.same(plugin),
				org.mockito.ArgumentMatchers.any(Runnable.class));
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin, cache);
		setBoolean(transport, "standbySubscriber", true);
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		transport.dispatchLegacy(JsonEnvelope.builder("replay").build());
		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();
		assertTrue(submitted.await(1, TimeUnit.SECONDS));

		Field workerField = RedisBackendProxyTransport.class.getDeclaredField("replayWorker");
		workerField.setAccessible(true);
		Thread worker = (Thread) workerField.get(transport);
		assertTrue(worker != null);
		worker.interrupt();
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(1);
		while (replayTrackedDeliveries(transport) != 1 && System.nanoTime() < deadline)
			Thread.sleep(10L);
		assertEquals(1, replayTrackedDeliveries(transport),
				"interrupting a worker before Bukkit executes must not lose its batch");
		transport.close();
	}

	@Test
	void closeFencesAlreadyScheduledReplayAfterItsCurrentBukkitCallback() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		doAnswer(invocation -> {
			((Runnable) invocation.getArgument(1)).run();
			return null;
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.same(plugin),
				org.mockito.ArgumentMatchers.any(Runnable.class));
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin, cache);
		setBoolean(transport, "standbySubscriber", true);
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		doAnswer(invocation -> {
			entered.countDown();
			release.await(1, TimeUnit.SECONDS);
			return null;
		}).when(messages).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
		setField(transport, "messageHandler", messages);
		transport.dispatchLegacy(JsonEnvelope.builder("first").build());
		transport.dispatchLegacy(JsonEnvelope.builder("second").build());
		transport.activateAfterHandoff();
		transport.replayAfterHandoffPublication();
		assertTrue(entered.await(1, TimeUnit.SECONDS));

		transport.close();
		release.countDown();
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(1);
		while (replayCallbacksInFlight(transport) != 0 && System.nanoTime() < deadline)
			Thread.sleep(10L);
		assertEquals(0, replayCallbacksInFlight(transport),
				"the executing callback must remain accounted for after its generation is cancelled");
		verify(messages, times(1)).onMessage(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
	}

	private static void setBoolean(Object target, String name, boolean value) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		field.setBoolean(target, value);
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static int replayTrackedDeliveries(RedisBackendProxyTransport transport) throws Exception {
		java.util.ArrayDeque<JsonEnvelope> queue = replayQueue(transport);
		Field inFlightField = RedisBackendProxyTransport.class.getDeclaredField("replayDeliveriesInFlight");
		inFlightField.setAccessible(true);
		Object lifecycle = lifecycle(transport);
		synchronized (lifecycle) {
			return queue.size() + inFlightField.getInt(transport);
		}
	}

	@SuppressWarnings("unchecked")
	private static java.util.ArrayDeque<JsonEnvelope> replayQueue(RedisBackendProxyTransport transport) throws Exception {
		Field queueField = RedisBackendProxyTransport.class.getDeclaredField("deliveriesAfterReplay");
		queueField.setAccessible(true);
		return (java.util.ArrayDeque<JsonEnvelope>) queueField.get(transport);
	}

	private static Object lifecycle(RedisBackendProxyTransport transport) throws Exception {
		Field lifecycleField = RedisBackendProxyTransport.class.getDeclaredField("legacyLifecycle");
		lifecycleField.setAccessible(true);
		return lifecycleField.get(transport);
	}

	private static int replayCallbacksInFlight(RedisBackendProxyTransport transport) throws Exception {
		Field callbacksField = RedisBackendProxyTransport.class.getDeclaredField("replayCallbacksInFlight");
		callbacksField.setAccessible(true);
		Object lifecycle = lifecycle(transport);
		synchronized (lifecycle) {
			return callbacksField.getInt(transport);
		}
	}

	private static boolean awaitRetiredAfterHandoff(RedisBackendProxyTransport transport) throws Exception {
		Field lifecycleField = RedisBackendProxyTransport.class.getDeclaredField("legacyLifecycle");
		lifecycleField.setAccessible(true);
		Object lifecycle = lifecycleField.get(transport);
		Field retiredField = RedisBackendProxyTransport.class.getDeclaredField("retiredAfterHandoff");
		retiredField.setAccessible(true);
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(1);
		while (System.nanoTime() < deadline) {
			synchronized (lifecycle) {
				if (retiredField.getBoolean(transport)) return true;
			}
			Thread.sleep(10L);
		}
		return false;
	}
}
