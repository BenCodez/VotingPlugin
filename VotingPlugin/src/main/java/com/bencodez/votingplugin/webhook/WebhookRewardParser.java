package com.bencodez.votingplugin.webhook;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;

/**
 * Parses reward "Webhooks:" entries from a Bukkit {@link ConfigurationSection}.
 *
 * 
 * This is intentionally tolerant of missing fields, so presets won't hard-crash
 * reward parsing.
 * 
 *
 * 
 * Supported YAML examples:
 * 
 * <pre>
 * Webhooks:
 *   - webhook: "my-api"
 *     json: |
 *       {"event":"vote","player":"{PLAYER}"}
 *
 *   - webhook: "discord-staff"
 *     content:
 *       enabled: true
 *       message: "✅ {PLAYER} voted"
 *     embed:
 *       enabled: true
 *       title: "Vote"
 *       description: "{PLAYER} voted"
 *       color: "#2ecc71"
 *       thumbnail: "https://..."
 *       footer:
 *         enabled: true
 *         message: "{TIMESTAMP}"
 * </pre>
 * 
 */
public final class WebhookRewardParser {

	private WebhookRewardParser() {
		// utility
	}

	/**
	 * Parses webhook reward entries from a reward section.
	 *
	 * @param rewardSection section that contains "Webhooks:" list
	 * @return parsed entries (never null)
	 */
	public static List<WebhookRewardEntry> parse(ConfigurationSection rewardSection) {
		List<WebhookRewardEntry> out = new ArrayList<>();
		if (rewardSection == null) {
			return out;
		}

		List<Map<?, ?>> list = rewardSection.getMapList("Webhooks");
		if (list == null || list.isEmpty()) {
			return out;
		}

		for (Map<?, ?> raw : list) {
			if (raw == null) {
				continue;
			}

			String webhookId = string(raw.get("webhook"));
			if (webhookId == null || webhookId.isEmpty()) {
				continue;
			}

			String json = string(raw.get("json"));

			String content = null;
			boolean embedEnabled = false;
			String embedTitle = null;
			String embedDescription = null;
			String embedColor = null;
			String embedThumb = null;
			boolean footerEnabled = false;
			String footerText = null;

			Object contentObj = raw.get("content");
			if (contentObj instanceof Map) {
				Map<?, ?> c = (Map<?, ?>) contentObj;
				boolean enabled = bool(c.get("enabled"), true);
				if (enabled) {
					content = string(c.get("message"));
				}
			}

			Object embedObj = raw.get("embed");
			if (embedObj instanceof Map) {
				Map<?, ?> e = (Map<?, ?>) embedObj;
				embedEnabled = bool(e.get("enabled"), false);
				if (embedEnabled) {
					embedTitle = string(e.get("title"));
					embedDescription = string(e.get("description"));
					embedColor = string(e.get("color"));
					embedThumb = string(e.get("thumbnail"));

					Object footerObj = e.get("footer");
					if (footerObj instanceof Map) {
						Map<?, ?> f = (Map<?, ?>) footerObj;
						footerEnabled = bool(f.get("enabled"), false);
						if (footerEnabled) {
							footerText = string(f.get("message"));
						}
					}
				}
			}

			out.add(new WebhookRewardEntry(webhookId, json, content, embedEnabled, embedTitle, embedDescription,
					embedColor, embedThumb, footerEnabled, footerText));
		}

		return out;
	}

	private static String string(Object o) {
		if (o == null) {
			return null;
		}
		return String.valueOf(o);
	}

	private static boolean bool(Object o, boolean def) {
		if (o == null) {
			return def;
		}
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		return Boolean.parseBoolean(String.valueOf(o));
	}
}
