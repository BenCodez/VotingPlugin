package com.bencodez.votingplugin.backendproxy.http;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.util.Base64;
import java.util.Locale;

/**
 * A copy/paste connection code.  It deliberately includes no long-lived credential:
 * its only secret is a short-lived, single-use enrollment token. The trailing MAC is a
 * corruption check keyed by that included token; it is not a proxy signature and cannot stop
 * someone who can replace the whole code from replacing it with another valid code.
 */
public record HttpConnectionCode(String serverId, URI endpoint, String serverCertificatePin, String caCertificatePin,
		Instant expiresAt, String enrollmentToken) {
	private static final String VERSION = "VPH1";
	private static final int MAX_CODE_LENGTH = 4096;

	public HttpConnectionCode {
		serverId = HttpTlsIdentity.canonicalServerId(serverId);
		endpoint = validateEndpoint(endpoint);
		serverCertificatePin = validatePin(serverCertificatePin, "server certificate pin");
		caCertificatePin = validatePin(caCertificatePin, "CA certificate pin");
		if (expiresAt == null) throw new IllegalArgumentException("Expiry is required");
		enrollmentToken = validateToken(enrollmentToken);
	}

	public String encode() {
		String endpointPart = Base64.getUrlEncoder().withoutPadding()
				.encodeToString(endpoint.toASCIIString().getBytes(StandardCharsets.UTF_8));
		String unsigned = String.join(".", VERSION, serverId, endpointPart, serverCertificatePin, caCertificatePin,
				Long.toString(expiresAt.getEpochSecond()), enrollmentToken);
		byte[] token = Base64.getUrlDecoder().decode(enrollmentToken);
		return unsigned + "." + HttpTransportSecrets.hmacSha256Url(token, unsigned);
	}

	public boolean isActive(Clock clock) {
		return expiresAt.isAfter(clock.instant());
	}

	public void requireActive(Clock clock) {
		if (!isActive(clock)) throw new IllegalArgumentException("Connection code has expired");
	}

	public static HttpConnectionCode parse(String code) {
		if (code == null || code.length() > MAX_CODE_LENGTH || code.indexOf('\n') >= 0 || code.indexOf('\r') >= 0)
			throw new IllegalArgumentException("Connection code is invalid");
		String[] parts = code.split("\\.", -1);
		if (parts.length != 8 || !VERSION.equals(parts[0])) throw new IllegalArgumentException("Connection code is invalid");
		try {
			String endpoint = new String(Base64.getUrlDecoder().decode(parts[2]), StandardCharsets.UTF_8);
			String unsigned = String.join(".", parts[0], parts[1], parts[2], parts[3], parts[4], parts[5], parts[6]);
			byte[] token = Base64.getUrlDecoder().decode(parts[6]);
			String expected = HttpTransportSecrets.hmacSha256Url(token, unsigned);
			if (!HttpTransportSecrets.constantTimeEquals(expected.getBytes(StandardCharsets.US_ASCII),
					parts[7].getBytes(StandardCharsets.US_ASCII))) throw new IllegalArgumentException("Connection code is invalid");
			return new HttpConnectionCode(parts[1], new URI(endpoint), parts[3], parts[4], Instant.ofEpochSecond(Long.parseLong(parts[5])),
					parts[6]);
		} catch (IllegalArgumentException | URISyntaxException failure) {
			throw new IllegalArgumentException("Connection code is invalid", failure);
		}
	}

	private static URI validateEndpoint(URI value) {
		if (value == null || !"https".equalsIgnoreCase(value.getScheme()) || value.getHost() == null
				|| value.getUserInfo() != null || value.getFragment() != null || value.getRawQuery() != null)
			throw new IllegalArgumentException("Endpoint must be an absolute HTTPS URL without credentials or query");
		if (value.getPort() > 65535 || value.getPort() < -1) throw new IllegalArgumentException("Endpoint port is invalid");
		String path = value.getRawPath();
		if (path == null || path.isEmpty()) path = "/";
		if (!path.endsWith("/")) path += "/";
		try {
			return new URI("https", null, value.getHost().toLowerCase(Locale.ROOT), value.getPort(), path, null, null);
		} catch (URISyntaxException failure) {
			throw new IllegalArgumentException("Endpoint is invalid", failure);
		}
	}

	private static String validatePin(String pin, String name) {
		if (pin == null || !pin.matches("[0-9a-fA-F]{64}")) throw new IllegalArgumentException(name + " is invalid");
		return pin.toLowerCase(Locale.ROOT);
	}

	private static String validateToken(String token) {
		if (token == null || token.length() < 43 || token.length() > 128 || !token.matches("[A-Za-z0-9_-]+"))
			throw new IllegalArgumentException("Enrollment token is invalid");
		try {
			if (Base64.getUrlDecoder().decode(token).length < 32) throw new IllegalArgumentException("Enrollment token is invalid");
			return token;
		} catch (IllegalArgumentException failure) {
			throw new IllegalArgumentException("Enrollment token is invalid", failure);
		}
	}
}
