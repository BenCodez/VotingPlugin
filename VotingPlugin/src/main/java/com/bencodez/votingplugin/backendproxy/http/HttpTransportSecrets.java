package com.bencodez.votingplugin.backendproxy.http;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.util.Base64;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/** Small, deliberately dependency-free cryptographic helpers for the HTTP transport. */
final class HttpTransportSecrets {
	private static final SecureRandom RANDOM = new SecureRandom();

	private HttpTransportSecrets() { }

	static byte[] randomBytes(int length) {
		if (length < 16) throw new IllegalArgumentException("Secret length is too small");
		byte[] value = new byte[length];
		RANDOM.nextBytes(value);
		return value;
	}

	static String randomToken() {
		return Base64.getUrlEncoder().withoutPadding().encodeToString(randomBytes(32));
	}

	static byte[] sha256(byte[] value) {
		try {
			return MessageDigest.getInstance("SHA-256").digest(value);
		} catch (NoSuchAlgorithmException impossible) {
			throw new IllegalStateException("SHA-256 is unavailable", impossible);
		}
	}

	static String sha256Hex(byte[] value) {
		StringBuilder output = new StringBuilder(64);
		for (byte part : sha256(value)) output.append(String.format("%02x", part & 0xff));
		return output.toString();
	}

	static String certificatePin(X509Certificate certificate) {
		try {
			return sha256Hex(certificate.getEncoded());
		} catch (Exception failure) {
			throw new IllegalArgumentException("Could not encode certificate", failure);
		}
	}

	static boolean constantTimeEquals(byte[] first, byte[] second) {
		return first != null && second != null && MessageDigest.isEqual(first, second);
	}

	static String hmacSha256Url(byte[] key, String value) {
		try {
			Mac mac = Mac.getInstance("HmacSHA256");
			mac.init(new SecretKeySpec(key, "HmacSHA256"));
			return Base64.getUrlEncoder().withoutPadding().encodeToString(mac.doFinal(value.getBytes(StandardCharsets.US_ASCII)));
		} catch (Exception failure) {
			throw new IllegalStateException("HMAC-SHA-256 is unavailable", failure);
		}
	}
}
