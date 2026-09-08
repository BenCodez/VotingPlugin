package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.bencodez.simpleapi.servercomm.http.HttpBackendTransportConnector;
import com.bencodez.simpleapi.servercomm.http.HttpClientCredentialStore;
import com.bencodez.simpleapi.servercomm.http.HttpConnectionCode;
import com.bencodez.simpleapi.servercomm.http.HttpTlsIdentity;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import java.net.URI;
import java.nio.file.Path;
import java.time.Instant;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

class HttpBackendProxyTransportTest {
	@TempDir Path directory;

	@Test
	void dispatchesIncomingMessagesOnTheServerSchedulerAndWaitsForCompletion() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicReference<String> callbackThread = new AtomicReference<>();
		CountDownLatch callbackFinished = new CountDownLatch(1);
		GlobalMessageHandler handler = new GlobalMessageHandler() {
			@Override public void sendMessage(JsonEnvelope envelope) { }
		};
		handler.addListener(new GlobalMessageListener("test") {
			@Override public void onReceive(JsonEnvelope envelope) {
				callbackThread.set(Thread.currentThread().getName());
				callbackFinished.countDown();
			}
		});
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(1);
			Thread serverThread = new Thread(task, "test-server-thread");
			serverThread.start();
			return null;
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), org.mockito.ArgumentMatchers.any(Runnable.class));
		JsonEnvelope envelope = JsonEnvelope.builder("test").put("message", "payload").build();

		new HttpBackendProxyTransport(plugin).dispatchIncoming(handler, envelope,
				System.nanoTime() + TimeUnit.SECONDS.toNanos(1));

		assertTrue(callbackFinished.await(1, TimeUnit.SECONDS));
		assertEquals("test-server-thread", callbackThread.get());
	}

	@Test
	void timedOutScheduledMessageCannotExecuteLater() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), org.mockito.ArgumentMatchers.any(Runnable.class));
		java.util.concurrent.atomic.AtomicInteger deliveries = new java.util.concurrent.atomic.AtomicInteger();
		GlobalMessageHandler handler = new GlobalMessageHandler() {
			@Override public void sendMessage(JsonEnvelope envelope) { }
		};
		handler.addListener(new GlobalMessageListener("test") {
			@Override public void onReceive(JsonEnvelope envelope) { deliveries.incrementAndGet(); }
		});

		assertThrows(IllegalStateException.class,
				() -> new HttpBackendProxyTransport(plugin).dispatchIncoming(handler,
						JsonEnvelope.builder("test").build(), System.nanoTime()));
		assertTrue(scheduled.get() != null);
		scheduled.get().run();
		assertEquals(0, deliveries.get(), "a delivery rejected at its deadline must stay fenced");
	}

	@Test
	void interruptedScheduledMessageCannotExecuteLater() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), org.mockito.ArgumentMatchers.any(Runnable.class));
		java.util.concurrent.atomic.AtomicInteger deliveries = new java.util.concurrent.atomic.AtomicInteger();
		GlobalMessageHandler handler = new GlobalMessageHandler() {
			@Override public void sendMessage(JsonEnvelope envelope) { }
		};
		handler.addListener(new GlobalMessageListener("test") {
			@Override public void onReceive(JsonEnvelope envelope) { deliveries.incrementAndGet(); }
		});
		AtomicReference<Throwable> dispatchFailure = new AtomicReference<>();
		Thread dispatch = new Thread(() -> {
			try {
				new HttpBackendProxyTransport(plugin).dispatchIncoming(handler,
						JsonEnvelope.builder("test").build(), System.nanoTime() + TimeUnit.SECONDS.toNanos(5));
			} catch (Throwable thrown) {
				dispatchFailure.set(thrown);
			}
		});
		dispatch.start();
		while (scheduled.get() == null) Thread.onSpinWait();
		dispatch.interrupt();
		dispatch.join(TimeUnit.SECONDS.toMillis(1));

		assertTrue(dispatchFailure.get() instanceof IllegalStateException);
		scheduled.get().run();
		assertEquals(0, deliveries.get(), "an interrupted delivery must stay fenced");
	}

	@Test
	void validatesInitialConnectionCodeSynchronously() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getDataFolder()).thenReturn(directory.toFile());
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getServer()).thenReturn("lobby-1");
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(plugin);

		when(settings.getHttpConnectionCode()).thenReturn("malformed");
		assertThrows(IllegalStateException.class, transport::validate);

		when(settings.getHttpConnectionCode()).thenReturn(code("lobby-1", Instant.now().minusSeconds(1)).encode());
		assertThrows(IllegalStateException.class, transport::validate);

		when(settings.getHttpConnectionCode()).thenReturn(code("survival", Instant.now().plusSeconds(60)).encode());
		assertThrows(IllegalStateException.class, transport::validate);
		assertThrows(IllegalStateException.class, () -> transport.start(mock(GlobalMessageHandler.class)),
				"invalid configuration must fail before the enrollment worker starts");

		when(settings.getHttpConnectionCode()).thenReturn(code("lobby-1", Instant.now().plusSeconds(60)).encode());
		assertDoesNotThrow(() -> transport.validate());
	}

	@Test
	void ordinaryStartupRetriesTransientInitialEnrollmentWithBoundedBackoff() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(plugin);
		HttpConnectionCode enrollment = code("lobby-1", Instant.now().plusSeconds(60));
		List<Long> delays = new ArrayList<>();
		java.util.concurrent.atomic.AtomicInteger attempts = new java.util.concurrent.atomic.AtomicInteger();

		try (MockedStatic<HttpBackendTransportConnector> connector = org.mockito.Mockito.mockStatic(HttpBackendTransportConnector.class)) {
			connector.when(() -> HttpBackendTransportConnector.enroll(enrollment, "lobby-1", directory))
					.thenAnswer(ignored -> {
						if (attempts.getAndIncrement() == 0) throw new IOException("proxy unavailable");
						return null;
					});
			assertTrue(transport.enrollForStartup(enrollment, "lobby-1", directory, true,
					delay -> { delays.add(delay); return true; }));
		}

		assertEquals(2, attempts.get());
		assertEquals(List.of(1_000L), delays);
	}

	@Test
	void stagedEnrollmentRemainsFailFast() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(plugin);
		HttpConnectionCode enrollment = code("lobby-1", Instant.now().plusSeconds(60));
		try (MockedStatic<HttpBackendTransportConnector> connector = org.mockito.Mockito.mockStatic(HttpBackendTransportConnector.class)) {
			connector.when(() -> HttpBackendTransportConnector.enroll(enrollment, "lobby-1", directory))
					.thenThrow(new IOException("proxy unavailable"));
			assertThrows(IOException.class,
					() -> transport.enrollForStartup(enrollment, "lobby-1", directory, false,
							delay -> { throw new AssertionError("staged enrollment must not retry"); }));
		}
	}

	@Test
	void freshConnectionCodeOverridesAnExistingEnrollment() throws Exception {
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "proxy.example.test");
		HttpConnectionCode original = new HttpConnectionCode("lobby-1", URI.create("https://proxy.example.test:1297/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().minusSeconds(1), "A".repeat(43));
		HttpClientCredentialStore.saveEnrolled(credentials, original, identity.issueClientCertificate("lobby-1"));
		assertNull(HttpBackendProxyTransport.enrollmentCode(credentials, "lobby-1", ""));
		assertNull(HttpBackendProxyTransport.enrollmentCode(credentials, "lobby-1", original.encode()),
				"the already-consumed code must not be retried, even after it expires");

		HttpConnectionCode replacement = new HttpConnectionCode("lobby-1", original.endpoint(), original.serverCertificatePin(),
				original.caCertificatePin(), Instant.now().plusSeconds(60), "B".repeat(43));
		assertEquals(replacement.encode(), HttpBackendProxyTransport.enrollmentCode(credentials, "lobby-1", replacement.encode()).encode());
		assertThrows(IllegalStateException.class,
				() -> HttpBackendProxyTransport.enrollmentCode(credentials, "lobby-1", "malformed"));
	}

	@Test
	void failedReplacementFlushResumesTheExistingConnector() {
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		when(connector.flushOutgoing(org.mockito.ArgumentMatchers.anyLong())).thenReturn(false);
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));

		assertThrows(IllegalStateException.class,
				() -> transport.flushForReplacement(connector, System.nanoTime()));

		verify(connector).start();
	}

	@Test
	void closeNeverWaitsForSetupOnTheCallingThread() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		CountDownLatch started = new CountDownLatch(1), release = new CountDownLatch(1);
		Thread blocked = new Thread(() -> {
			started.countDown();
			while (release.getCount() != 0) try { release.await(); }
			catch (InterruptedException ignored) { /* Simulate setup I/O that has not unwound yet. */ }
		});
		blocked.start();
		assertTrue(started.await(1, TimeUnit.SECONDS));
		java.lang.reflect.Field worker = HttpBackendProxyTransport.class.getDeclaredField("worker");
		worker.setAccessible(true);
		worker.set(transport, blocked);

		long startedAt = System.nanoTime();
		transport.close();
		long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startedAt);
		try { assertTrue(elapsedMillis < 500, "close blocked the calling thread for " + elapsedMillis + " ms"); }
		finally { release.countDown(); blocked.join(TimeUnit.SECONDS.toMillis(1)); }
	}

	@Test
	void prepareForReplacementSnapshotsThePersistedCredentialGeneration() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("replacement-proxy"), "proxy.example.test");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", URI.create("https://proxy.example.test:1297/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "D".repeat(43));
		HttpClientCredentialStore.saveEnrolled(credentials, code, identity.issueClientCertificate("lobby-1"));
		HttpClientCredentialStore.ActiveCredentialGeneration generation =
				HttpClientCredentialStore.snapshotActiveGeneration(credentials);
		java.lang.reflect.Field directoryField = HttpBackendProxyTransport.class.getDeclaredField("configuredDirectory");
		directoryField.setAccessible(true);
		directoryField.set(transport, credentials);
		java.lang.reflect.Field connectorField = HttpBackendProxyTransport.class.getDeclaredField("connector");
		connectorField.setAccessible(true);
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		when(connector.flushOutgoing(org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);
		connectorField.set(transport, connector);

		assertDoesNotThrow(transport::prepareForReplacement);
		java.lang.reflect.Field generationField =
				HttpBackendProxyTransport.class.getDeclaredField("configuredCredentialGeneration");
		generationField.setAccessible(true);
		assertEquals(generation, generationField.get(transport));
	}

	@Test
	@SuppressWarnings("unchecked")
	void validationWaitsForThePreviousDirectoryOwnerBeforeReadinessFailure() throws Exception {
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy-owner"), "proxy.example.test");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", URI.create("https://proxy.example.test:1297/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "C".repeat(43));
		HttpClientCredentialStore.saveEnrolled(credentials, code, identity.issueClientCertificate("lobby-1"));
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getDataFolder()).thenReturn(directory.toFile());
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		when(settings.getServer()).thenReturn("lobby-1");
		when(settings.getHttpConnectionCode()).thenReturn("");
		java.lang.reflect.Field ownersField = HttpBackendProxyTransport.class.getDeclaredField("DIRECTORY_OWNERS");
		ownersField.setAccessible(true);
		var owners = (java.util.concurrent.ConcurrentHashMap<Path, java.util.concurrent.Semaphore>) ownersField.get(null);
		java.util.concurrent.Semaphore predecessor = new java.util.concurrent.Semaphore(0);
		owners.put(credentials.toAbsolutePath().normalize(), predecessor);
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(plugin);
		transport.start(mock(GlobalMessageHandler.class));
		java.util.concurrent.atomic.AtomicReference<Throwable> failure = new java.util.concurrent.atomic.AtomicReference<>();
		CountDownLatch finished = new CountDownLatch(1);
		Thread validation = new Thread(() -> {
			try { transport.validate(System.nanoTime() + TimeUnit.SECONDS.toNanos(1)); }
			catch (Throwable thrown) { failure.set(thrown); }
			finally { finished.countDown(); }
		});
		validation.start();
		assertFalse(finished.await(150, TimeUnit.MILLISECONDS), "validation published before journal handoff");
		predecessor.release();
		assertTrue(finished.await(3, TimeUnit.SECONDS));
		validation.join(TimeUnit.SECONDS.toMillis(1));
		assertTrue(failure.get() instanceof IllegalStateException);
		java.lang.reflect.Field connectorField = HttpBackendProxyTransport.class.getDeclaredField("connector");
		connectorField.setAccessible(true);
		assertTrue(connectorField.get(transport) instanceof HttpBackendTransportConnector,
				"a readiness timeout must not tear down the connector's background retry loop");
		transport.close();
	}

	private static HttpConnectionCode code(String serverId, Instant expiry) {
		return new HttpConnectionCode(serverId, URI.create("https://proxy.example.test:1297/"), "a".repeat(64),
				"b".repeat(64), expiry, "A".repeat(43));
	}
}
