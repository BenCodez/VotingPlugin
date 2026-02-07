package com.bencodez.votingplugin.webhook;

/**
 * Represents one webhook entry under a reward's "Webhooks:" list.
 *
 * <p>Two payload styles:
 * <ul>
 *   <li>Raw JSON: {@link #getJson()}</li>
 *   <li>Discord convenience payload: content + optional embed fields</li>
 * </ul>
 * </p>
 *
 * <p>All fields may contain placeholders and are expanded at execution time.</p>
 */
public final class WebhookRewardEntry {

	private final String webhookId;

	// Generic mode
	private final String json;

	// Discord convenience mode
	private final String content;

	private final boolean embedEnabled;
	private final String embedTitle;
	private final String embedDescription;
	private final String embedColorHex;
	private final String embedThumbnailUrl;

	private final boolean footerEnabled;
	private final String footerText;

	/**
	 * Creates a reward entry.
	 *
	 * @param webhookId webhook definition id
	 * @param json raw json payload (nullable)
	 * @param content discord content (nullable)
	 * @param embedEnabled embed enabled
	 * @param embedTitle embed title
	 * @param embedDescription embed description
	 * @param embedColorHex hex color #RRGGBB (nullable)
	 * @param embedThumbnailUrl thumbnail url (nullable)
	 * @param footerEnabled footer enabled
	 * @param footerText footer text (nullable)
	 */
	public WebhookRewardEntry(String webhookId, String json, String content, boolean embedEnabled, String embedTitle,
			String embedDescription, String embedColorHex, String embedThumbnailUrl, boolean footerEnabled, String footerText) {
		this.webhookId = webhookId;
		this.json = json;
		this.content = content;
		this.embedEnabled = embedEnabled;
		this.embedTitle = embedTitle;
		this.embedDescription = embedDescription;
		this.embedColorHex = embedColorHex;
		this.embedThumbnailUrl = embedThumbnailUrl;
		this.footerEnabled = footerEnabled;
		this.footerText = footerText;
	}

	/** @return webhook definition id */
	public String getWebhookId() {
		return webhookId;
	}

	/** @return raw json payload or null */
	public String getJson() {
		return json;
	}

	/** @return content string or null */
	public String getContent() {
		return content;
	}

	/** @return true if embed enabled */
	public boolean isEmbedEnabled() {
		return embedEnabled;
	}

	/** @return embed title */
	public String getEmbedTitle() {
		return embedTitle;
	}

	/** @return embed description */
	public String getEmbedDescription() {
		return embedDescription;
	}

	/** @return embed color hex */
	public String getEmbedColorHex() {
		return embedColorHex;
	}

	/** @return embed thumbnail url */
	public String getEmbedThumbnailUrl() {
		return embedThumbnailUrl;
	}

	/** @return footer enabled */
	public boolean isFooterEnabled() {
		return footerEnabled;
	}

	/** @return footer text */
	public String getFooterText() {
		return footerText;
	}

	/** @return true if using raw json mode */
	public boolean isJsonMode() {
		return json != null && !json.isEmpty();
	}
}
