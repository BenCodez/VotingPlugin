package com.bencodez.votingplugin.webhook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.sun.net.httpserver.HttpServer;

class WebhookBoundsTest {

	@Test
	void logUrlOmitsAllPotentialCredentialLocations() {
		WebhookDefinition credentials = definition(
				"https://user:secret@example.com:8443/hook/api-key?token=secret#private");
		assertEquals("https://example.com:8443", credentials.safeUrlForLog());
		WebhookDefinition discord = definition("https://user:secret@discord.com/api/webhooks/123/token");
		assertEquals("https://discord.com", discord.safeUrlForLog());
		assertEquals("https://[::1]:8080", definition("https://[::1]:8080/token").safeUrlForLog());
		assertEquals("[REDACTED URL]", definition("not a valid URL?token=secret").safeUrlForLog());
	}

	@Test
	void malformedRequestTargetDoesNotReachWorkerLog() throws Exception {
		String secret = "never-log-this-token";
		List<String> warnings = new CopyOnWriteArrayList<>();
		WebhookService service = new WebhookService(warnings::add, 1, delayMs -> { });
		try {
			service.setDefinitions(Collections.singletonMap("hook",
					definition("not a valid URL?token=" + secret)));
			service.start();
			service.submit(request());
			awaitWarning(warnings, "Invalid request target");
			assertTrue(warnings.stream().noneMatch(message -> message.contains(secret)));
			assertTrue(warnings.stream().allMatch(message -> !message.contains("not a valid URL")));
		} finally {
			service.stop();
		}
	}

	@Test
	void rejectsAdmissionWhenTheBoundedQueueIsFull() throws Exception {
		CountDownLatch firstRequestStarted = new CountDownLatch(1);
		CountDownLatch releaseFirstRequest = new CountDownLatch(1);
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/", exchange -> {
			firstRequestStarted.countDown();
			await(releaseFirstRequest);
			exchange.sendResponseHeaders(204, -1);
			exchange.close();
		});
		server.start();

		List<String> warnings = new CopyOnWriteArrayList<>();
		WebhookService service = new WebhookService(warnings::add, 1, delayMs -> { });
		try {
			service.setDefinitions(Collections.singletonMap("hook", definition(url(server), 1, 0, 0)));
			service.start();
			service.submit(request());
			assertTrue(firstRequestStarted.await(2, TimeUnit.SECONDS));
			service.submit(request());
			service.submit(request());
			service.submit(request());

			assertEquals(1, warnings.stream().filter(message -> message.contains("Queue is full")).count());
		} finally {
			releaseFirstRequest.countDown();
			service.stop();
			server.stop(0);
		}
	}

	@Test
	void discord429RetryAfterStillConsumesTheAttemptBudget() throws Exception {
		AtomicInteger attempts = new AtomicInteger();
		CountDownLatch thirdAttempt = new CountDownLatch(1);
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/", exchange -> {
			int attempt = attempts.incrementAndGet();
			byte[] response = "{\"retry_after\":0.001}".getBytes(java.nio.charset.StandardCharsets.UTF_8);
			exchange.sendResponseHeaders(429, response.length);
			exchange.getResponseBody().write(response);
			exchange.close();
			if (attempt == 3) {
				thirdAttempt.countDown();
			}
		});
		server.start();

		List<Long> delays = new CopyOnWriteArrayList<>();
		List<String> warnings = new CopyOnWriteArrayList<>();
		WebhookService service = new WebhookService(warnings::add, 1, delays::add);
		try {
			service.setDefinitions(Collections.singletonMap("hook", definition(url(server), 3, 0, 0)));
			service.start();
			service.submit(request());
			assertTrue(thirdAttempt.await(2, TimeUnit.SECONDS));
			assertEquals(3, attempts.get());
			assertEquals(List.of(251L, 251L), delays);
			awaitWarning(warnings, "Discord rate limit exhausted");
		} finally {
			service.stop();
			server.stop(0);
		}
	}

	@Test
	void logsNonSuccessResponseWhenRetriesAreDisabled() throws Exception {
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/", exchange -> {
			exchange.sendResponseHeaders(500, -1);
			exchange.close();
		});
		server.start();

		List<String> warnings = new CopyOnWriteArrayList<>();
		WebhookService service = new WebhookService(warnings::add, 1, delayMs -> { });
		try {
			service.setDefinitions(Collections.singletonMap("hook", definition(url(server), false, 5, 0, 0)));
			service.start();
			service.submit(request());
			awaitWarning(warnings, "Non-success HTTP 500");
			assertEquals(1, warnings.stream().filter(message -> message.contains("Non-success HTTP 500")).count());
		} finally {
			service.stop();
			server.stop(0);
		}
	}

	@Test
	void clampsDiscordRetryDelay() {
		assertEquals(WebhookDefinition.MAX_RETRY_DELAY_MS,
				DiscordRateLimitUtil.extractRetryAfterMs("{\"retry_after\":999999}"));
		assertEquals(1_484L, DiscordRateLimitUtil.extractRetryAfterMs("{\"retry_after\":1.234}"));
	}

	@Test
	void clampsRetryConfigurationToSafeBounds() {
		YamlConfiguration config = new YamlConfiguration();
		config.set("Webhooks.Enabled", true);
		config.set("Webhooks.Definitions.hook.Url", "https://example.invalid/hook");
		config.set("Webhooks.Definitions.hook.Retry.MaxAttempts", 999);
		config.set("Webhooks.Definitions.hook.Retry.BackoffMs", 999_999L);
		config.set("Webhooks.Definitions.hook.Retry.MaxBackoffMs", Long.MAX_VALUE);

		WebhookDefinition definition = WebhookConfigLoader.load(config.getConfigurationSection("Webhooks")).get("hook");

		assertEquals(WebhookDefinition.MAX_RETRY_ATTEMPTS, definition.getRetryMaxAttempts());
		assertEquals(WebhookDefinition.MAX_RETRY_DELAY_MS, definition.getRetryBackoffMs());
		assertEquals(WebhookDefinition.MAX_RETRY_DELAY_MS, definition.getRetryMaxBackoffMs());
		assertFalse(WebhookConfigLoader.load(null).containsKey("hook"));
	}

	private static WebhookRequest request() {
		return new WebhookRequest("hook", "{}", Collections.emptyMap(), null);
	}

	private static WebhookDefinition definition(String url) {
		return definition(url, 1, 0, 0);
	}

	private static WebhookDefinition definition(String url, int maxAttempts, long backoffMs, long maxBackoffMs) {
		return definition(url, true, maxAttempts, backoffMs, maxBackoffMs);
	}

	private static WebhookDefinition definition(String url, boolean retryEnabled, int maxAttempts, long backoffMs,
			long maxBackoffMs) {
		return new WebhookDefinition("hook", true, url, WebhookHttpMethod.POST, "application/json", 2_000, true,
				Collections.emptyMap(), null, retryEnabled, maxAttempts, backoffMs, maxBackoffMs);
	}

	private static String url(HttpServer server) {
		return "http://127.0.0.1:" + server.getAddress().getPort() + "/";
	}

	private static void await(CountDownLatch latch) throws IOException {
		try {
			if (!latch.await(2, TimeUnit.SECONDS)) {
				throw new IOException("test server release was not signalled");
			}
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IOException("test server was interrupted", e);
		}
	}

	private static void awaitWarning(List<String> warnings, String expected) throws InterruptedException {
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
		while (warnings.stream().noneMatch(message -> message.contains(expected)) && System.nanoTime() < deadline) {
			Thread.sleep(10);
		}
		assertTrue(warnings.stream().anyMatch(message -> message.contains(expected)));
	}
}
