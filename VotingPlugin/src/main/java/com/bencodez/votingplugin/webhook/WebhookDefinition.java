package com.bencodez.votingplugin.webhook;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Webhook endpoint definition.
 *
 * <p>These are loaded from Config.yml under Webhooks.Definitions.*</p>
 */
public final class WebhookDefinition {

	private final String id;
	private final boolean enabled;
	private final String url;

	private final WebhookHttpMethod method;
	private final String contentType;

	private final int timeoutMs;

	private final boolean handleDiscordRateLimits;

	private final Map<String, String> headers;
	private final WebhookSignatureConfig signature;

	private final boolean retryEnabled;
	private final int retryMaxAttempts;
	private final long retryBackoffMs;
	private final long retryMaxBackoffMs;

	private volatile long lastRetryAfterMs;

	/**
	 * Constructs a webhook definition.
	 *
	 * @param id id/key of this webhook definition
	 * @param enabled whether this definition is enabled
	 * @param url webhook URL
	 * @param method HTTP method
	 * @param contentType request content type (usually application/json)
	 * @param timeoutMs request timeout in milliseconds
	 * @param handleDiscordRateLimits whether to use Discord retry_after for HTTP 429
	 * @param headers extra headers (values may contain placeholders)
	 * @param signature signature config (optional)
	 * @param retryEnabled whether retry is enabled
	 * @param retryMaxAttempts max attempts including the first attempt
	 * @param retryBackoffMs initial backoff in milliseconds
	 * @param retryMaxBackoffMs maximum backoff in milliseconds
	 */
	public WebhookDefinition(String id, boolean enabled, String url, WebhookHttpMethod method, String contentType,
			int timeoutMs, boolean handleDiscordRateLimits, Map<String, String> headers, WebhookSignatureConfig signature,
			boolean retryEnabled, int retryMaxAttempts, long retryBackoffMs, long retryMaxBackoffMs) {

		this.id = id;
		this.enabled = enabled;
		this.url = url;

		this.method = method == null ? WebhookHttpMethod.POST : method;
		this.contentType = (contentType == null || contentType.isEmpty()) ? "application/json" : contentType;

		this.timeoutMs = timeoutMs <= 0 ? 8000 : timeoutMs;
		this.handleDiscordRateLimits = handleDiscordRateLimits;

		this.headers = headers == null ? Collections.emptyMap()
				: Collections.unmodifiableMap(new LinkedHashMap<>(headers));
		this.signature = signature;

		this.retryEnabled = retryEnabled;
		this.retryMaxAttempts = retryMaxAttempts <= 0 ? 1 : retryMaxAttempts;
		this.retryBackoffMs = Math.max(0, retryBackoffMs);
		this.retryMaxBackoffMs = Math.max(this.retryBackoffMs, retryMaxBackoffMs);
	}

	/** @return definition id */
	public String getId() {
		return id;
	}

	/** @return true if enabled */
	public boolean isEnabled() {
		return enabled;
	}

	/** @return webhook URL */
	public String getUrl() {
		return url;
	}

	/** @return HTTP method */
	public WebhookHttpMethod getMethod() {
		return method;
	}

	/** @return content type */
	public String getContentType() {
		return contentType;
	}

	/** @return timeout in milliseconds */
	public int getTimeoutMs() {
		return timeoutMs;
	}

	/** @return true if Discord 429 retry_after handling is enabled */
	public boolean isHandleDiscordRateLimits() {
		return handleDiscordRateLimits;
	}

	/** @return extra headers (unmodifiable) */
	public Map<String, String> getHeaders() {
		return headers;
	}

	/** @return signature config (may be null) */
	public WebhookSignatureConfig getSignature() {
		return signature;
	}

	/** @return true if retry is enabled */
	public boolean isRetryEnabled() {
		return retryEnabled;
	}

	/** @return max attempts */
	public int getRetryMaxAttempts() {
		return retryMaxAttempts;
	}

	/** @return initial backoff in ms */
	public long getRetryBackoffMs() {
		return retryBackoffMs;
	}

	/** @return max backoff in ms */
	public long getRetryMaxBackoffMs() {
		return retryMaxBackoffMs;
	}

	/**
	 * Returns a safe URL string for logs (removes sensitive token segments).
	 *
	 * @return sanitized URL for logging
	 */
	public String safeUrlForLog() {
		if (url == null) {
			return "null";
		}
		// Discord webhooks: https://discord.com/api/webhooks/{id}/{token}
		int idx = url.indexOf("/api/webhooks/");
		if (idx == -1) {
			return url;
		}
		String prefix = url.substring(0, idx);
		String rest = url.substring(idx);
		String[] parts = rest.split("/");
		if (parts.length >= 5) {
			return prefix + "/api/webhooks/" + parts[3] + "/REDACTED";
		}
		return prefix + "/api/webhooks/REDACTED";
	}

	/** @return retry_after in ms, or 0 if none */
	public long getLastRetryAfterMs() {
		return lastRetryAfterMs;
	}

	/** @param lastRetryAfterMs retry delay in ms */
	public void setLastRetryAfterMs(long lastRetryAfterMs) {
		this.lastRetryAfterMs = lastRetryAfterMs;
	}
}
