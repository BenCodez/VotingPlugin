package com.bencodez.votingplugin.webhook;

/**
 * Supported HTTP methods for webhook requests.
 */
public enum WebhookHttpMethod {
	POST, PUT, PATCH;

	/**
	 * Parses a method string safely.
	 *
	 * @param s method string
	 * @return parsed method or POST if invalid/null
	 */
	public static WebhookHttpMethod parse(String s) {
		if (s == null) {
			return POST;
		}
		try {
			return WebhookHttpMethod.valueOf(s.trim().toUpperCase());
		} catch (Exception ignored) {
			return POST;
		}
	}
}
