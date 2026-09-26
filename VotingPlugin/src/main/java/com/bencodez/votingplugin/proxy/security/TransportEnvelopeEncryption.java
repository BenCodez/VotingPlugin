package com.bencodez.votingplugin.proxy.security;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;

/** Optional authenticated encryption for complete VotingPlugin transport envelopes. */
public final class TransportEnvelopeEncryption {
	public enum Domain {
		PROXY_BACKEND("votingplugin-proxy-backend-encryption-v1"),
		MULTI_PROXY("votingplugin-multi-proxy-encryption-v1");

		private final String value;

		Domain(String value) {
			this.value = value;
		}
	}

	public record Decryption(boolean accepted, JsonEnvelope envelope, String reason) {
		private static Decryption accept(JsonEnvelope envelope) {
			return new Decryption(true, envelope, null);
		}

		private static Decryption reject(String reason) {
			return new Decryption(false, null, reason);
		}
	}

	static final String SUBCHANNEL = "_vpEncrypted";
	private static final String K_VERSION = "version";
	private static final String K_DOMAIN = "domain";
	private static final String K_NONCE = "nonce";
	private static final String K_CIPHERTEXT = "ciphertext";
	private static final String VERSION = "1";
	private static final String DERIVATION_DOMAIN = "votingplugin-transport-encryption-key-v1";
	private static final int NONCE_BYTES = 12;
	private static final int TAG_BITS = 128;
	private static final int MAX_ENCODED_BYTES = 1024 * 1024;

	private final SecretKeySpec key;
	private final Domain domain;
	private final boolean enabled;
	private final SecureRandom random;

	private TransportEnvelopeEncryption(byte[] masterKey, Domain domain, boolean enabled, SecureRandom random) {
		this.key = new SecretKeySpec(deriveKey(masterKey, domain), "AES");
		this.domain = domain;
		this.enabled = enabled;
		this.random = random;
	}

	public static TransportEnvelopeEncryption load(Path keyFile, Domain domain, boolean enabled) throws IOException {
		byte[] decoded = null;
		try {
			if (!Files.isRegularFile(keyFile)) throw new IOException("Transport encryption requires secretkey.key");
			decoded = Base64.getDecoder().decode(Files.readString(keyFile, StandardCharsets.US_ASCII).trim());
			if (decoded.length < 16) throw new IOException("Transport encryption key is too short");
			return new TransportEnvelopeEncryption(decoded, domain, enabled, new SecureRandom());
		} catch (IllegalArgumentException invalid) {
			throw new IOException("Transport encryption key is invalid", invalid);
		} finally {
			if (decoded != null) java.util.Arrays.fill(decoded, (byte) 0);
		}
	}

	static TransportEnvelopeEncryption forTesting(byte[] key, Domain domain, boolean enabled) {
		return new TransportEnvelopeEncryption(key, domain, enabled, new SecureRandom());
	}

	public JsonEnvelope encrypt(JsonEnvelope envelope) {
		if (!enabled) return envelope;
		try {
			byte[] plaintext = JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8);
			if (plaintext.length > MAX_ENCODED_BYTES) throw new IllegalArgumentException("Transport envelope is too large");
			byte[] nonce = new byte[NONCE_BYTES];
			random.nextBytes(nonce);
			Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
			cipher.init(Cipher.ENCRYPT_MODE, key, new GCMParameterSpec(TAG_BITS, nonce));
			cipher.updateAAD(aad());
			byte[] ciphertext = cipher.doFinal(plaintext);
			return JsonEnvelope.builder(SUBCHANNEL).put(K_VERSION, VERSION).put(K_DOMAIN, domain.value)
					.put(K_NONCE, Base64.getEncoder().encodeToString(nonce))
					.put(K_CIPHERTEXT, Base64.getEncoder().encodeToString(ciphertext)).build();
		} catch (GeneralSecurityException failure) {
			throw new IllegalStateException("Transport envelope encryption failed", failure);
		}
	}

	/** Accept encrypted input during rollout even before this node enables outbound encryption. */
	public Decryption decrypt(JsonEnvelope envelope) {
		if (envelope == null) return Decryption.reject("missing envelope");
		if (!SUBCHANNEL.equals(envelope.getSubChannel())) {
			return enabled ? Decryption.reject("unencrypted envelope") : Decryption.accept(envelope);
		}
		try {
			if (!VERSION.equals(envelope.getFields().get(K_VERSION))
					|| !domain.value.equals(envelope.getFields().get(K_DOMAIN))) return Decryption.reject("invalid metadata");
			String encodedNonce = envelope.getFields().get(K_NONCE);
			String encodedCiphertext = envelope.getFields().get(K_CIPHERTEXT);
			if (encodedNonce == null || encodedNonce.length() > 32 || encodedCiphertext == null
					|| encodedCiphertext.length() > ((MAX_ENCODED_BYTES + 32) * 4L / 3L) + 8L)
				return Decryption.reject("invalid encrypted envelope size");
			byte[] nonce = Base64.getDecoder().decode(encodedNonce);
			byte[] ciphertext = Base64.getDecoder().decode(encodedCiphertext);
			if (nonce.length != NONCE_BYTES || ciphertext.length > MAX_ENCODED_BYTES + 32)
				return Decryption.reject("invalid encrypted envelope size");
			Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
			cipher.init(Cipher.DECRYPT_MODE, key, new GCMParameterSpec(TAG_BITS, nonce));
			cipher.updateAAD(aad());
			byte[] plaintext = cipher.doFinal(ciphertext);
			if (plaintext.length > MAX_ENCODED_BYTES) return Decryption.reject("decrypted envelope is too large");
			return Decryption.accept(JsonEnvelopeCodec.decode(new String(plaintext, StandardCharsets.UTF_8)));
		} catch (RuntimeException | GeneralSecurityException failure) {
			return Decryption.reject("authentication failed");
		}
	}

	public boolean enabled() {
		return enabled;
	}

	private byte[] aad() {
		return (VERSION + "\0" + domain.value).getBytes(StandardCharsets.UTF_8);
	}

	private static byte[] deriveKey(byte[] masterKey, Domain domain) {
		try {
			Mac mac = Mac.getInstance("HmacSHA256");
			mac.init(new SecretKeySpec(masterKey, "HmacSHA256"));
			mac.update(DERIVATION_DOMAIN.getBytes(StandardCharsets.UTF_8));
			mac.update((byte) 0);
			return mac.doFinal(domain.value.getBytes(StandardCharsets.UTF_8));
		} catch (GeneralSecurityException failure) {
			throw new IllegalStateException("Transport encryption key derivation failed", failure);
		}
	}
}
