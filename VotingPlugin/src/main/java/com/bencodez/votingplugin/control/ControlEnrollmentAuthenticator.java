package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.UUID;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/** Authenticates proxy-mediated enrollment over shared, unbound transports. */
public final class ControlEnrollmentAuthenticator {
	private static final String ALGORITHM = "HmacSHA256";
	private final byte[] key;

	private ControlEnrollmentAuthenticator(byte[] key) {
		this.key = key.clone();
	}

	public static ControlEnrollmentAuthenticator load(Path keyFile) throws IOException {
		if (!Files.isRegularFile(keyFile)) {
			throw new IOException("Control enrollment requires a shared secretkey.key");
		}
		try {
			byte[] decoded = Base64.getDecoder().decode(Files.readString(keyFile, StandardCharsets.US_ASCII).trim());
			if (decoded.length < 16) throw new IOException("Control enrollment key is too short");
			return new ControlEnrollmentAuthenticator(decoded);
		} catch (IllegalArgumentException invalid) {
			throw new IOException("Control enrollment key is invalid", invalid);
		}
	}

	public String sign(String nodeId, UUID requestId, String endpoint, String verifier, String challenge) {
		try {
			Mac mac = Mac.getInstance(ALGORITHM);
			mac.init(new SecretKeySpec(key, ALGORITHM));
			update(mac, nodeId);
			update(mac, requestId == null ? "" : requestId.toString());
			update(mac, endpoint);
			update(mac, verifier);
			update(mac, challenge);
			return java.util.HexFormat.of().formatHex(mac.doFinal());
		} catch (GeneralSecurityException impossible) {
			throw new IllegalStateException("HMAC-SHA256 is unavailable", impossible);
		}
	}

	public boolean verifies(String authenticator, String nodeId, UUID requestId, String endpoint, String verifier,
			String challenge) {
		if (authenticator == null || !authenticator.matches("[0-9a-f]{64}")) return false;
		byte[] expected = sign(nodeId, requestId, endpoint, verifier, challenge).getBytes(StandardCharsets.US_ASCII);
		return MessageDigest.isEqual(expected, authenticator.getBytes(StandardCharsets.US_ASCII));
	}

	private static void update(Mac mac, String value) {
		byte[] bytes = value == null ? new byte[0] : value.getBytes(StandardCharsets.UTF_8);
		mac.update((byte) (bytes.length >>> 24));
		mac.update((byte) (bytes.length >>> 16));
		mac.update((byte) (bytes.length >>> 8));
		mac.update((byte) bytes.length);
		mac.update(bytes);
	}
}
