package com.bencodez.votingplugin.webhook;

import java.util.Map;

/**
 * A queued webhook request.
 *
 * <p>All placeholders should be expanded before creating this object.</p>
 */
public final class WebhookRequest {

	private final String definitionId;
	private final String body;
	private final Map<String, String> headers;
	private final String signatureSecretOverride;

	/**
	 * Creates a webhook request.
	 *
	 * @param definitionId definition id
	 * @param body request body (usually JSON)
	 * @param headers expanded headers (may be null)
	 * @param signatureSecretOverride expanded signature secret override (may be null)
	 */
	public WebhookRequest(String definitionId, String body, Map<String, String> headers, String signatureSecretOverride) {
		this.definitionId = definitionId;
		this.body = body;
		this.headers = headers;
		this.signatureSecretOverride = signatureSecretOverride;
	}

	/** @return definition id */
	public String getDefinitionId() {
		return definitionId;
	}

	/** @return body */
	public String getBody() {
		return body;
	}

	/** @return headers (may be null) */
	public Map<String, String> getHeaders() {
		return headers;
	}

	/** @return signature secret override (may be null) */
	public String getSignatureSecretOverride() {
		return signatureSecretOverride;
	}
}
