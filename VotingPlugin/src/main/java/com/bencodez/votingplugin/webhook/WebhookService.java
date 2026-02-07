package com.bencodez.votingplugin.webhook;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Asynchronous webhook dispatcher.
 *
 * <p>
 * This service queues webhook requests and sends them off the main thread to
 * avoid blocking server ticks. It supports:
 * <ul>
 * <li>HTTP method override (POST/PUT/PATCH)</li>
 * <li>custom content-type</li>
 * <li>custom headers</li>
 * <li>optional HMAC signature header</li>
 * <li>Discord 429 retry_after handling</li>
 * <li>retry/backoff for transient failures</li>
 * </ul>
 * </p>
 */
public final class WebhookService {

	private final WebhookLogger logger;

	private final Map<String, WebhookDefinition> definitions = new ConcurrentHashMap<>();
	private final LinkedBlockingQueue<WebhookRequest> queue = new LinkedBlockingQueue<>();

	private volatile boolean running;
	private Thread worker;

	/**
	 * Creates a webhook service.
	 *
	 * @param logger logger implementation
	 */
	public WebhookService(WebhookLogger logger) {
		this.logger = Objects.requireNonNull(logger, "logger");
	}

	/**
	 * Replaces all known webhook definitions (e.g. after config reload).
	 *
	 * @param defs definitions keyed by id
	 */
	public void setDefinitions(Map<String, WebhookDefinition> defs) {
		definitions.clear();
		if (defs != null) {
			definitions.putAll(defs);
		}
	}

	/**
	 * Starts the background worker thread.
	 */
	public synchronized void start() {
		if (running) {
			return;
		}
		running = true;
		worker = new Thread(this::runLoop, "VotingPlugin-WebhookWorker");
		worker.setDaemon(true);
		worker.start();
	}

	/**
	 * Stops the background worker thread and clears queued requests.
	 */
	public synchronized void stop() {
		running = false;
		if (worker != null) {
			worker.interrupt();
		}
		queue.clear();
	}

	/**
	 * Queues a webhook request to be sent asynchronously.
	 *
	 * @param request request (placeholders must already be expanded)
	 */
	public void submit(WebhookRequest request) {
		if (!running || request == null) {
			return;
		}
		if (request.getDefinitionId() == null || request.getDefinitionId().isEmpty()) {
			return;
		}
		if (request.getBody() == null || request.getBody().isEmpty()) {
			return;
		}

		WebhookDefinition def = definitions.get(request.getDefinitionId());
		if (def == null || !def.isEnabled()) {
			return;
		}

		queue.offer(request);
	}

	private void runLoop() {
		while (running) {
			try {
				WebhookRequest req = queue.poll(250, TimeUnit.MILLISECONDS);
				if (req == null) {
					continue;
				}

				WebhookDefinition def = definitions.get(req.getDefinitionId());
				if (def == null || !def.isEnabled()) {
					continue;
				}

				sendWithRetry(def, req);

			} catch (InterruptedException ignored) {
				// allow exit
			} catch (Throwable t) {
				logger.warn("[Webhooks] Worker error: " + t.getMessage());
			}
		}
	}

	private void sendWithRetry(WebhookDefinition def, WebhookRequest req) throws InterruptedException {
		int attempts = 0;

		long backoffMs = Math.max(0, def.getRetryBackoffMs());
		long maxBackoffMs = Math.max(backoffMs, def.getRetryMaxBackoffMs());

		while (true) {
			attempts++;

			try {
				int code = sendOnce(def, req);

				if (code >= 200 && code < 300) {
					return;
				}

				// Discord 429 handling
				if (code == 429 && def.isHandleDiscordRateLimits()) {
					long waitMs = def.getLastRetryAfterMs();
					if (waitMs > 0) {
						Thread.sleep(waitMs);
						continue;
					}
				}

				if (!def.isRetryEnabled() || attempts >= def.getRetryMaxAttempts()) {
					if (code != 429) {
						logger.warn("[Webhooks] Non-success HTTP " + code + " for " + def.safeUrlForLog());
					}
					return;
				}

			} catch (IOException e) {
				if (!def.isRetryEnabled() || attempts >= def.getRetryMaxAttempts()) {
					logger.warn("[Webhooks] Failed sending to " + def.safeUrlForLog() + " ("
							+ e.getClass().getSimpleName() + "): " + e.getMessage());
					return;
				}
			}

			if (def.isRetryEnabled()) {
				Thread.sleep(backoffMs);
				backoffMs = Math.min(maxBackoffMs, Math.max(backoffMs + 250, backoffMs * 2));
			} else {
				return;
			}
		}
	}

	private int sendOnce(WebhookDefinition def, WebhookRequest req) throws IOException, InterruptedException {

		def.setLastRetryAfterMs(0);

		HttpURLConnection conn = (HttpURLConnection) new URL(def.getUrl()).openConnection();
		conn.setConnectTimeout(def.getTimeoutMs());
		conn.setReadTimeout(def.getTimeoutMs());
		conn.setUseCaches(false);
		conn.setDoOutput(true);
		conn.setRequestMethod(def.getMethod().name());

		conn.setRequestProperty("Content-Type", def.getContentType());

		// Headers
		if (req.getHeaders() != null) {
			for (Map.Entry<String, String> e : req.getHeaders().entrySet()) {
				if (e.getKey() != null && !e.getKey().isEmpty() && e.getValue() != null) {
					conn.setRequestProperty(e.getKey(), e.getValue());
				}
			}
		}

		// Signature
		WebhookSignatureConfig sig = def.getSignature();
		if (sig != null && sig.isEnabled()) {
			String secret = req.getSignatureSecretOverride();
			if (secret == null || secret.isEmpty()) {
				secret = sig.getSecret();
			}
			if (secret != null && !secret.isEmpty()) {
				String signed = WebhookSigner.sign(sig.getAlgo(), secret, req.getBody());
				if (signed != null && sig.getHeader() != null && !sig.getHeader().isEmpty()) {
					conn.setRequestProperty(sig.getHeader(), signed);
				}
			}
		}

		// Write body
		byte[] data = req.getBody().getBytes(StandardCharsets.UTF_8);
		conn.setFixedLengthStreamingMode(data.length);

		try (OutputStream os = conn.getOutputStream()) {
			os.write(data);
		}

		int code = conn.getResponseCode();

		// Read body for Discord retry_after
		String body = null;
		InputStream is = (code >= 200 && code < 400) ? conn.getInputStream() : conn.getErrorStream();

		if (is != null) {
			try (BufferedReader br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
				StringBuilder sb = new StringBuilder();
				String line;
				while ((line = br.readLine()) != null) {
					sb.append(line);
				}
				body = sb.toString();
			}
		}

		if (code == 429 && def.isHandleDiscordRateLimits()) {
			long retryAfterMs = DiscordRateLimitUtil.extractRetryAfterMs(body);
			def.setLastRetryAfterMs(retryAfterMs);
		}

		conn.disconnect();
		return code;
	}

}
