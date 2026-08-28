package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import redis.clients.jedis.DefaultJedisClientConfig;

class RedisClientConfigTest {

	@Test
	void tlsCanBeEnabledForProxyPublisherConnections() {
		VotingPluginProxyConfig config = mock(VotingPluginProxyConfig.class);
		when(config.getRedisDbIndex()).thenReturn(2);
		when(config.getRedisSsl()).thenReturn(true);

		DefaultJedisClientConfig clientConfig = VotingPluginProxy.buildRedisClientConfig(config);

		assertTrue(clientConfig.isSsl());
		assertEquals(2, clientConfig.getDatabase());
		assertEquals("HTTPS", clientConfig.getSslParameters().getEndpointIdentificationAlgorithm());
	}

	@Test
	void tlsRemainsDisabledByDefault() {
		VotingPluginProxyConfig config = mock(VotingPluginProxyConfig.class);

		assertFalse(VotingPluginProxy.buildRedisClientConfig(config).isSsl());
	}
}
