package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.CopyOnWriteArrayList;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
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
}
