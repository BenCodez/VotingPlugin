package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
	void fencingDoesNotWaitForAnIdentifiedMessageHandler() throws Exception {
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
		Thread retirement = new Thread(() -> {
			transport.fenceAfterHandoff();
			fenced.countDown();
		});
		retirement.start();
		try {
			assertTrue(fenced.await(1, TimeUnit.SECONDS));
		} finally {
			release.countDown();
		}
		delivery.join(TimeUnit.SECONDS.toMillis(1));
		retirement.join(TimeUnit.SECONDS.toMillis(1));
		assertFalse(delivery.isAlive());
		assertFalse(retirement.isAlive());
	}

	@Test
	@SuppressWarnings("unchecked")
	void replayCompletesBeforeNewlyAdmittedDeliveries() throws Exception {
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveRedisDelivery("new-delivery")).thenReturn(true);
		when(cache.consumeLegacyRedisDelivery(org.mockito.ArgumentMatchers.anyString())).thenReturn(false);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null, cache);
		JsonEnvelope replayed = JsonEnvelope.builder("old").build();
		JsonEnvelope newer = JsonEnvelope.builder("new").build();
		Field buffered = RedisBackendProxyTransport.class.getDeclaredField("bufferedLegacyDeliveries");
		buffered.setAccessible(true);
		((java.util.List<JsonEnvelope>) buffered.get(transport)).add(replayed);
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

		transport.activateAfterHandoff();

		assertEquals(java.util.List.of(replayed, newer), order);
	}
}
