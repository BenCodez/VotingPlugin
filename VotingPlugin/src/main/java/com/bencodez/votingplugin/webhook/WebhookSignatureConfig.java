package com.bencodez.votingplugin.webhook;

/**
 * Signature configuration used to sign webhook payloads for verification by the receiver.
 */
public final class WebhookSignatureConfig {

	private final boolean enabled;
	private final String algo;
	private final String header;
	private final String secret;

	/**
	 * Creates a new signature config.
	 *
	 * @param enabled whether signing is enabled
	 * @param algo algorithm name (currently supports "HMAC_SHA256")
	 * @param header header name to send the signature in (e.g. "X-Signature")
	 * @param secret secret key used to compute the signature (may contain placeholders)
	 */
	public WebhookSignatureConfig(boolean enabled, String algo, String header, String secret) {
		this.enabled = enabled;
		this.algo = algo;
		this.header = header;
		this.secret = secret;
	}

	/** @return true if enabled */
	public boolean isEnabled() {
		return enabled;
	}

	/** @return algorithm name */
	public String getAlgo() {
		return algo;
	}

	/** @return header name */
	public String getHeader() {
		return header;
	}

	/** @return secret string (may contain placeholders) */
	public String getSecret() {
		return secret;
	}
}
