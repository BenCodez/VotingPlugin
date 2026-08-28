package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

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
}
