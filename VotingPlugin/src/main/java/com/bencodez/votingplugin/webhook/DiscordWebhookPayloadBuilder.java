package com.bencodez.votingplugin.webhook;

/**
 * Builds Discord-compatible webhook JSON payloads.
 *
 * <p>This builder only outputs fields that are set/enabled.</p>
 */
public final class DiscordWebhookPayloadBuilder {

	private String username;
	private String avatarUrl;

	private String content;

	private boolean embedEnabled;
	private String embedTitle;
	private String embedDescription;
	private String embedColorHex;
	private String embedThumbnailUrl;

	private boolean footerEnabled;
	private String footerText;

	/** @return new builder */
	public static DiscordWebhookPayloadBuilder create() {
		return new DiscordWebhookPayloadBuilder();
	}

	private DiscordWebhookPayloadBuilder() {
		// builder
	}

	/**
	 * Sets the username override.
	 *
	 * @param username username override
	 * @return this builder
	 */
	public DiscordWebhookPayloadBuilder username(String username) {
		this.username = username;
		return this;
	}

	/**
	 * Sets the avatar URL override.
	 *
	 * @param avatarUrl avatar URL override
	 * @return this builder
	 */
	public DiscordWebhookPayloadBuilder avatarUrl(String avatarUrl) {
		this.avatarUrl = avatarUrl;
		return this;
	}

	/**
	 * Sets the message content.
	 *
	 * @param content message content (plain text/markdown)
	 * @return this builder
	 */
	public DiscordWebhookPayloadBuilder content(String content) {
		this.content = content;
		return this;
	}

	/**
	 * Enables an embed.
	 *
	 * @param title embed title
	 * @param description embed description
	 * @param colorHex color in #RRGGBB (optional)
	 * @param thumbnailUrl thumbnail URL (optional)
	 * @param footerEnabled whether footer is enabled
	 * @param footerText footer text (optional)
	 * @return builder
	 */
	public DiscordWebhookPayloadBuilder embed(String title, String description, String colorHex, String thumbnailUrl,
			boolean footerEnabled, String footerText) {
		this.embedEnabled = true;
		this.embedTitle = title;
		this.embedDescription = description;
		this.embedColorHex = colorHex;
		this.embedThumbnailUrl = thumbnailUrl;
		this.footerEnabled = footerEnabled;
		this.footerText = footerText;
		return this;
	}

	/**
	 * Builds a JSON payload string suitable for Discord webhooks.
	 *
	 * @return JSON string
	 */
	public String buildJson() {
		StringBuilder sb = new StringBuilder(256);
		sb.append('{');

		boolean first = true;

		first = appendString(sb, "username", username, first);
		first = appendString(sb, "avatar_url", avatarUrl, first);
		first = appendString(sb, "content", content, first);

		if (embedEnabled) {
			if (!first) {
				sb.append(',');
			}
			sb.append("\"embeds\":[{");
			boolean eFirst = true;

			eFirst = appendString(sb, "title", embedTitle, eFirst);
			eFirst = appendString(sb, "description", embedDescription, eFirst);

			Integer color = parseHexColor(embedColorHex);
			if (color != null) {
				if (!eFirst) {
					sb.append(',');
				}
				sb.append("\"color\":").append(color.intValue());
				eFirst = false;
			}

			if (embedThumbnailUrl != null && !embedThumbnailUrl.isEmpty()) {
				if (!eFirst) {
					sb.append(',');
				}
				sb.append("\"thumbnail\":{");
				sb.append("\"url\":\"").append(escapeJson(embedThumbnailUrl)).append("\"");
				sb.append('}');
				eFirst = false;
			}

			if (footerEnabled && footerText != null && !footerText.isEmpty()) {
				if (!eFirst) {
					sb.append(',');
				}
				sb.append("\"footer\":{");
				sb.append("\"text\":\"").append(escapeJson(footerText)).append("\"");
				sb.append('}');
			}

			sb.append("}]");
			first = false;
		}

		sb.append('}');
		return sb.toString();
	}

	private static boolean appendString(StringBuilder sb, String key, String value, boolean first) {
		if (value == null || value.isEmpty()) {
			return first;
		}
		if (!first) {
			sb.append(',');
		}
		sb.append('"').append(key).append("\":\"").append(escapeJson(value)).append('"');
		return false;
	}

	private static Integer parseHexColor(String hex) {
		if (hex == null || hex.isEmpty()) {
			return null;
		}
		String h = hex.trim();
		if (h.startsWith("#")) {
			h = h.substring(1);
		}
		if (h.length() != 6) {
			return null;
		}
		try {
			return Integer.valueOf(Integer.parseInt(h, 16));
		} catch (NumberFormatException ignored) {
			return null;
		}
	}

	private static String escapeJson(String s) {
		StringBuilder out = new StringBuilder(s.length() + 16);
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			switch (c) {
			case '\\':
				out.append("\\\\");
				break;
			case '"':
				out.append("\\\"");
				break;
			case '\n':
				out.append("\\n");
				break;
			case '\r':
				out.append("\\r");
				break;
			case '\t':
				out.append("\\t");
				break;
			default:
				out.append(c);
				break;
			}
		}
		return out.toString();
	}
}
