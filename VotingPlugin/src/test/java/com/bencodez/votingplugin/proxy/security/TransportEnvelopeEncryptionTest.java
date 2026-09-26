package com.bencodez.votingplugin.proxy.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.Domain;

class TransportEnvelopeEncryptionTest {
	private static final byte[] KEY = "0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII);

	@Test
	void encryptsAndAuthenticatesCompleteEnvelope() {
		TransportEnvelopeEncryption cipher = TransportEnvelopeEncryption.forTesting(KEY, Domain.PROXY_BACKEND, true);
		JsonEnvelope original = JsonEnvelope.builder("Vote").schema(2).put("player", "Alex").put("secret", "value")
				.build();

		JsonEnvelope encrypted = cipher.encrypt(original);
		TransportEnvelopeEncryption.Decryption result = cipher.decrypt(encrypted);

		assertNotEquals(original.getSubChannel(), encrypted.getSubChannel());
		assertFalse(encrypted.getFields().toString().contains("Alex"));
		assertTrue(result.accepted());
		assertEquals(original.getSubChannel(), result.envelope().getSubChannel());
		assertEquals(original.getSchema(), result.envelope().getSchema());
		assertEquals(original.getFields(), result.envelope().getFields());
	}

	@Test
	void rejectsTamperingWrongKeyAndWrongDomain() {
		TransportEnvelopeEncryption cipher = TransportEnvelopeEncryption.forTesting(KEY, Domain.PROXY_BACKEND, true);
		JsonEnvelope encrypted = cipher.encrypt(JsonEnvelope.builder("Vote").put("player", "Alex").build());
		String ciphertext = encrypted.getFields().get("ciphertext");
		char replacement = ciphertext.charAt(ciphertext.length() - 1) == 'A' ? 'B' : 'A';
		JsonEnvelope tampered = encrypted.toBuilder()
				.put("ciphertext", ciphertext.substring(0, ciphertext.length() - 1) + replacement).build();

		assertFalse(cipher.decrypt(tampered).accepted());
		assertFalse(TransportEnvelopeEncryption.forTesting("different-key-material-32-bytes!".getBytes(StandardCharsets.US_ASCII),
				Domain.PROXY_BACKEND, true).decrypt(encrypted).accepted());
		assertFalse(TransportEnvelopeEncryption.forTesting(KEY, Domain.MULTI_PROXY, true).decrypt(encrypted).accepted());
	}

	@Test
	void disabledModeAcceptsPlaintextAndCanReceiveEncryptedRolloutTraffic() {
		JsonEnvelope plain = JsonEnvelope.builder("Status").put("server", "backend-a").build();
		TransportEnvelopeEncryption enabled = TransportEnvelopeEncryption.forTesting(KEY, Domain.PROXY_BACKEND, true);
		TransportEnvelopeEncryption disabled = TransportEnvelopeEncryption.forTesting(KEY, Domain.PROXY_BACKEND, false);

		assertSame(plain, disabled.decrypt(plain).envelope());
		assertEquals(plain.getFields(), disabled.decrypt(enabled.encrypt(plain)).envelope().getFields());
		assertFalse(enabled.decrypt(plain).accepted());
	}

	@Test
	void keyFileIsCreatedOnceAndNeverReplaced(@TempDir Path directory) throws Exception {
		Path keyFile = directory.resolve("secretkey.key");
		assertTrue(SharedSecretKeyFile.ensure(keyFile));
		String first = Files.readString(keyFile);
		assertFalse(SharedSecretKeyFile.ensure(keyFile));
		assertEquals(first, Files.readString(keyFile));
		assertEquals(32, java.util.Base64.getDecoder().decode(first).length);
	}
}
