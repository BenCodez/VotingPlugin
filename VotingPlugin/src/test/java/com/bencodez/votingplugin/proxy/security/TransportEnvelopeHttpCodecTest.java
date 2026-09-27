package com.bencodez.votingplugin.proxy.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.Domain;

class TransportEnvelopeHttpCodecTest {
	private static final byte[] OLD_KEY = "0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII);
	private static final byte[] NEW_KEY = "fedcba9876543210fedcba9876543210".getBytes(StandardCharsets.US_ASCII);

	@Test
	void appliesEncryptionOnlyAtTheWireBoundary() {
		JsonEnvelope semantic = JsonEnvelope.builder("Vote").put("player", "Alex").build();
		TransportEnvelopeHttpCodec oldCodec = codec(OLD_KEY);
		TransportEnvelopeHttpCodec newCodec = codec(NEW_KEY);

		JsonEnvelope oldWireEnvelope = oldCodec.encode(semantic);
		assertThrows(IllegalArgumentException.class, () -> newCodec.decode(oldWireEnvelope));

		JsonEnvelope retriedWithCurrentPolicy = newCodec.encode(semantic);
		JsonEnvelope decoded = newCodec.decode(retriedWithCurrentPolicy);
		assertEquals(semantic.getSubChannel(), decoded.getSubChannel());
		assertEquals(semantic.getFields(), decoded.getFields());
	}

	private TransportEnvelopeHttpCodec codec(byte[] key) {
		return new TransportEnvelopeHttpCodec(TransportEnvelopeEncryption.forTesting(key, Domain.PROXY_BACKEND, true));
	}
}
