package com.bencodez.votingplugin.webhook;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;

/**
 * Loads webhook definitions from VotingPlugin's Config.yml section.
 *
 * Expected structure under the root section you pass:
 * <pre>
 * Enabled: true
 * Definitions:
 *   myhook:
 *     Enabled: true
 *     Url: "https://..."
 *     Method: POST
 *     ContentType: "application/json"
 *     TimeoutMs: 8000
 *     Headers:
 *       Authorization: "Bearer {API_KEY}"
 *     HandleDiscordRateLimits: true
 *     Retry:
 *       Enabled: true
 *       MaxAttempts: 5
 *       BackoffMs: 1500
 *       MaxBackoffMs: 15000
 *     Signature:
 *       Enabled: false
 *       Algo: "HMAC_SHA256"
 *       Header: "X-Signature"
 *       Secret: "secret"
 * </pre>
 * 
 */
public final class WebhookConfigLoader {

	private WebhookConfigLoader() {
		// utility
	}

	/**
	 * Loads webhook definitions.
	 *
	 * @param webhooksRoot root section for webhooks (e.g., config.getConfigurationSection("Webhooks"))
	 * @return definitions by id (never null)
	 */
	public static Map<String, WebhookDefinition> load(ConfigurationSection webhooksRoot) {
		if (webhooksRoot == null) {
			return Collections.emptyMap();
		}

		if (!webhooksRoot.getBoolean("Enabled", false)) {
			return Collections.emptyMap();
		}

		ConfigurationSection defsSec = webhooksRoot.getConfigurationSection("Definitions");
		if (defsSec == null) {
			return Collections.emptyMap();
		}

		Map<String, WebhookDefinition> out = new LinkedHashMap<>();

		for (String id : defsSec.getKeys(false)) {
			ConfigurationSection s = defsSec.getConfigurationSection(id);
			if (s == null) {
				continue;
			}

			boolean defEnabled = s.getBoolean("Enabled", true);
			String url = s.getString("Url", "");
			if (url == null || url.isEmpty()) {
				continue;
			}

			WebhookHttpMethod method = WebhookHttpMethod.parse(s.getString("Method", "POST"));
			String contentType = s.getString("ContentType", "application/json");
			int timeoutMs = s.getInt("TimeoutMs", 8000);

			Map<String, String> headers = loadStringMap(s.getConfigurationSection("Headers"));

			boolean handleDiscord = s.getBoolean("HandleDiscordRateLimits", true);

			// Retry
			boolean retryEnabled = true;
			int maxAttempts = 5;
			long backoffMs = 1500;
			long maxBackoffMs = 15000;

			ConfigurationSection retrySec = s.getConfigurationSection("Retry");
			if (retrySec != null) {
				retryEnabled = retrySec.getBoolean("Enabled", true);
				maxAttempts = retrySec.getInt("MaxAttempts", 5);
				backoffMs = retrySec.getLong("BackoffMs", 1500);
				maxBackoffMs = retrySec.getLong("MaxBackoffMs", 15000);
			}

			// Signature
			WebhookSignatureConfig signature = null;
			ConfigurationSection sigSec = s.getConfigurationSection("Signature");
			if (sigSec != null) {
				boolean sigEnabled = sigSec.getBoolean("Enabled", false);
				String algo = sigSec.getString("Algo", "HMAC_SHA256");
				String header = sigSec.getString("Header", "X-Signature");
				String secret = sigSec.getString("Secret", "");
				signature = new WebhookSignatureConfig(sigEnabled, algo, header, secret);
			}

			WebhookDefinition def = new WebhookDefinition(id, defEnabled, url, method, contentType, timeoutMs, handleDiscord,
					headers, signature, retryEnabled, maxAttempts, backoffMs, maxBackoffMs);

			out.put(id, def);
		}

		return out;
	}

	private static Map<String, String> loadStringMap(ConfigurationSection sec) {
		if (sec == null) {
			return Collections.emptyMap();
		}
		Map<String, String> out = new LinkedHashMap<>();
		for (String k : sec.getKeys(false)) {
			Object v = sec.get(k);
			if (v != null) {
				out.put(k, String.valueOf(v));
			}
		}
		return out;
	}
}
