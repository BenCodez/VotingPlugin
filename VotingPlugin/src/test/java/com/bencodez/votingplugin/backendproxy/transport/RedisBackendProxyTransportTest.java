package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;

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
}
