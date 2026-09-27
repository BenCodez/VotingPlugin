package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;

class SharedInboundPolicyTest {
	@Test
	void equivalenceIncludesTransportDestinationAndAuthenticationPolicy(@TempDir Path directory) throws Exception {
		Path keyFile = directory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		SharedTransportEnvelopeAuthenticator required = SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED);
		SharedInboundPolicy policy = new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a", required);

		assertTrue(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED))));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:b",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED))));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(MqttBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED))));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.COMPATIBILITY))));
	}
}
