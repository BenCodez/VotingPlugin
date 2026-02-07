package com.bencodez.votingplugin.webhook;

import java.nio.charset.StandardCharsets;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * Signs webhook payloads for receiver verification.
 */
public final class WebhookSigner {

	private WebhookSigner() {
		// utility
	}

	/**
	 * Computes a signature for the given payload.
	 *
	 * @param algo algorithm string (supports "HMAC_SHA256")
	 * @param secret secret key (already expanded)
	 * @param payload payload body
	 * @return signature string (hex), or null on error/unsupported
	 */
	public static String sign(String algo, String secret, String payload) {
		if (algo == null || secret == null || payload == null) {
			return null;
		}
		if (!"HMAC_SHA256".equalsIgnoreCase(algo)) {
			return null;
		}

		try {
			Mac mac = Mac.getInstance("HmacSHA256");
			mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
			byte[] bytes = mac.doFinal(payload.getBytes(StandardCharsets.UTF_8));
			return toHex(bytes);
		} catch (Exception ignored) {
			return null;
		}
	}

	private static String toHex(byte[] bytes) {
		char[] hex = new char[bytes.length * 2];
		final char[] alphabet = "0123456789abcdef".toCharArray();
		for (int i = 0; i < bytes.length; i++) {
			int v = bytes[i] & 0xFF;
			hex[i * 2] = alphabet[v >>> 4];
			hex[i * 2 + 1] = alphabet[v & 0x0F];
		}
		return new String(hex);
	}
}
