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
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

class SharedInboundPolicyTest {
	@Test
	void equivalenceIncludesTransportDestinationAndAuthenticationPolicy(@TempDir Path directory) throws Exception {
		Path keyFile = directory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		SharedTransportEnvelopeAuthenticator required = SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED);
		TransportEnvelopeEncryption encrypted = TransportEnvelopeEncryption.load(keyFile,
				TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
		SharedInboundPolicy policy = new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a", required,
				encrypted);

		assertTrue(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED), TransportEnvelopeEncryption.load(keyFile,
						TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true))));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:b",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED), encrypted)));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(MqttBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED), encrypted)));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.COMPATIBILITY), encrypted)));
		assertFalse(policy.hasEquivalentPolicy(new SharedInboundPolicy(RedisBackendProxyTransport.class, "vp:a",
				SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED), TransportEnvelopeEncryption.load(keyFile,
						TransportEnvelopeEncryption.Domain.PROXY_BACKEND, false))));
	}
}
