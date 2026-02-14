package com.bencodez.votingplugin.webhook;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Executes webhook rewards using configured definitions.
 *
 * 
 * This class:
 * <ul>
 * <li>expands placeholders for payloads/headers/secrets</li>
 * <li>builds either raw JSON payloads or Discord-friendly payloads</li>
 * <li>queues requests on {@link WebhookService}</li>
 * </ul>
 * 
 */
public final class WebhookRewardExecutor {

	private final WebhookService service;
	private final Map<String, WebhookDefinition> definitions;

	/**
	 * Creates the executor.
	 *
	 * @param service     webhook service
	 * @param definitions definitions by id
	 */
	public WebhookRewardExecutor(WebhookService service, Map<String, WebhookDefinition> definitions) {
		this.service = Objects.requireNonNull(service, "service");
		this.definitions = definitions == null ? Collections.emptyMap() : definitions;
	}

	/**
	 * Executes all entries.
	 *
	 * @param entries  reward entries (may be empty)
	 * @param expander placeholder expander
	 */
	public void executeAll(List<WebhookRewardEntry> entries, PlaceholderExpander expander) {
		if (entries == null || entries.isEmpty() || expander == null) {
			return;
		}
		for (WebhookRewardEntry e : entries) {
			executeOne(e, expander);
		}
	}

	/**
	 * Executes one webhook entry.
	 *
	 * @param entry    entry
	 * @param expander placeholder expander
	 */
	public void executeOne(WebhookRewardEntry entry, PlaceholderExpander expander) {
		if (entry == null || expander == null) {
			return;
		}

		WebhookDefinition def = definitions.get(entry.getWebhookId());
		if (def == null || !def.isEnabled()) {
			return;
		}

		String body = buildBody(entry, expander);

		Map<String, String> expandedHeaders = expandHeaders(def.getHeaders(), expander);
		String expandedSecret = expandSignatureSecret(def.getSignature(), expander);

		service.submit(new WebhookRequest(def.getId(), body, expandedHeaders, expandedSecret));
	}

	private static String buildBody(WebhookRewardEntry entry, PlaceholderExpander expander) {
		if (entry.isJsonMode()) {
			return expander.expand(entry.getJson());
		}

		DiscordWebhookPayloadBuilder b = DiscordWebhookPayloadBuilder.create();

		String content = expander.expand(entry.getContent());
		if (content != null && !content.isEmpty()) {
			b.content(content);
		}

		if (entry.isEmbedEnabled()) {
			String title = expander.expand(entry.getEmbedTitle());
			String desc = expander.expand(entry.getEmbedDescription());
			String color = expander.expand(entry.getEmbedColorHex());
			String thumb = expander.expand(entry.getEmbedThumbnailUrl());
			String footer = expander.expand(entry.getFooterText());
			b.embed(nullToEmpty(title), nullToEmpty(desc), nullToEmpty(color), nullToEmpty(thumb),
					entry.isFooterEnabled(), nullToEmpty(footer));
		}

		return b.buildJson();
	}

	private static Map<String, String> expandHeaders(Map<String, String> headers, PlaceholderExpander expander) {
		if (headers == null || headers.isEmpty()) {
			return null;
		}
		Map<String, String> out = new HashMap<>();
		for (Map.Entry<String, String> e : headers.entrySet()) {
			if (e.getKey() == null || e.getKey().isEmpty()) {
				continue;
			}
			out.put(e.getKey(), expander.expand(e.getValue()));
		}
		return out;
	}

	private static String expandSignatureSecret(WebhookSignatureConfig sig, PlaceholderExpander expander) {
		if (sig == null || !sig.isEnabled()) {
			return null;
		}
		String secret = sig.getSecret();
		if (secret == null || secret.isEmpty()) {
			return null;
		}
		return expander.expand(secret);
	}

	private static String nullToEmpty(String s) {
		return s == null ? "" : s;
	}
}
