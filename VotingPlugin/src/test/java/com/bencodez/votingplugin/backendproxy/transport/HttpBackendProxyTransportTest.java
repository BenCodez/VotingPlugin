package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
	void publicationActivatesTheSimpleApiInboundBarrier() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		setField(transport, "connector", connector);

		transport.activateAfterPublication();

		assertTrue((boolean) field(transport, "inboundActive"));
	}

	@Test
	void closedPublicationGateRejectsDeliveryForReplay() {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		transport.close();

		assertThrows(IllegalStateException.class, () -> transport.dispatchAfterPublication(
				mock(GlobalMessageHandler.class), JsonEnvelope.builder("test").build()));
	}

	@Test
	void orderlyShutdownFlushesHandoffAfterMakingConnectorCapacity() {
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		JsonEnvelope pending = JsonEnvelope.builder("backend-stopped").build();
		when(connector.send(pending)).thenReturn(false, true);
		when(connector.flushOutgoing(org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);

		assertTrue(HttpBackendProxyTransport.flushHandoffForShutdown(connector, List.of(pending),
				System.nanoTime() + TimeUnit.SECONDS.toNanos(1)));

		org.mockito.InOrder order = org.mockito.Mockito.inOrder(connector);
		order.verify(connector, org.mockito.Mockito.times(2)).send(pending);
		order.verify(connector).flushOutgoing(org.mockito.ArgumentMatchers.anyLong());
	}

	@Test
	@SuppressWarnings("unchecked")
	void unpublishedHandoffMessagesAreDiscardedOnRollback() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		setField(transport, "connector", connector);
		((java.util.ArrayDeque<JsonEnvelope>) field(transport, "handoffQueue"))
				.add(JsonEnvelope.builder("unpublished").build());

		transport.close();

		verify(connector, org.mockito.Mockito.timeout(1000)).close();
		verify(connector, org.mockito.Mockito.never()).send(org.mockito.ArgumentMatchers.any(JsonEnvelope.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	void orderlyShutdownFlushesRecoveryQueueBeforeHandoffQueue() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		JsonEnvelope recovery = JsonEnvelope.builder("recovery").build();
		JsonEnvelope handoff = JsonEnvelope.builder("handoff").build();
		setField(transport, "connector", connector);
		setField(transport, "published", true);
		((java.util.ArrayDeque<JsonEnvelope>) field(transport, "startupQueue")).add(recovery);
		((java.util.ArrayDeque<JsonEnvelope>) field(transport, "handoffQueue")).add(handoff);
		when(connector.send(org.mockito.ArgumentMatchers.any(JsonEnvelope.class))).thenReturn(true);
		when(connector.flushOutgoing(org.mockito.ArgumentMatchers.anyLong())).thenReturn(true);

		transport.close();

		verify(connector, org.mockito.Mockito.timeout(1000)).flushOutgoing(org.mockito.ArgumentMatchers.anyLong());
		org.mockito.InOrder order = org.mockito.Mockito.inOrder(connector);
		order.verify(connector).send(recovery);
		order.verify(connector).send(handoff);
		order.verify(connector).flushOutgoing(org.mockito.ArgumentMatchers.anyLong());
	}

	@Test
	void failedPreparationAfterCloseRestoresCapturedHttpTransport() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(mock(VotingPluginMain.class));
		HttpBackendProxyTransport failed = mock(HttpBackendProxyTransport.class);
		HttpBackendProxyTransport restored = mock(HttpBackendProxyTransport.class);
		JsonEnvelope queued = JsonEnvelope.builder("queued-after-failure").build();
		setField(manager, "transport", failed);
		org.mockito.Mockito.doThrow(new IllegalStateException("worker still stopping"))
				.when(failed).prepareForReplacement();
		when(failed.isClosedForReplacement()).thenReturn(true);
		when(failed.recreatePrepared()).thenReturn(restored);

		assertThrows(IllegalStateException.class, manager::prepareForReplacement);
		manager.send(queued);

		verify(restored).send(queued);
	}

	@Test
	void failedFlushKeepsTheRestartedHttpTransport() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(mock(VotingPluginMain.class));
		HttpBackendProxyTransport active = mock(HttpBackendProxyTransport.class);
		JsonEnvelope queued = JsonEnvelope.builder("queued-after-flush-timeout").build();
		setField(manager, "transport", active);
		org.mockito.Mockito.doThrow(new IllegalStateException("flush timed out"))
				.when(active).prepareForReplacement();

		assertThrows(IllegalStateException.class, manager::prepareForReplacement);
		manager.send(queued);

		verify(active).send(queued);
		verify(active, org.mockito.Mockito.never()).recreatePrepared();
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
	void replacementCancelsRetryingInitialEnrollmentWithoutCredential() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		Path credentials = directory.resolve("http");
		java.lang.reflect.Field directoryField = HttpBackendProxyTransport.class.getDeclaredField("configuredDirectory");
		directoryField.setAccessible(true);
		directoryField.set(transport, credentials);
		java.lang.reflect.Field ownersField = HttpBackendProxyTransport.class.getDeclaredField("DIRECTORY_OWNERS");
		ownersField.setAccessible(true);
		@SuppressWarnings("unchecked")
		var owners = (java.util.concurrent.ConcurrentHashMap<Path, java.util.concurrent.Semaphore>) ownersField.get(null);
		Path ownerKey = credentials.toAbsolutePath().normalize();
		java.util.concurrent.Semaphore owner = new java.util.concurrent.Semaphore(0);
		owners.put(ownerKey, owner);
		CountDownLatch workerStarted = new CountDownLatch(1);
		Thread retryingEnrollment = new Thread(() -> {
			workerStarted.countDown();
			try { Thread.sleep(Long.MAX_VALUE); }
			catch (InterruptedException expected) { Thread.currentThread().interrupt(); }
			finally { owner.release(); }
		});
		java.lang.reflect.Field workerField = HttpBackendProxyTransport.class.getDeclaredField("worker");
		workerField.setAccessible(true);
		workerField.set(transport, retryingEnrollment);
		retryingEnrollment.start();
		assertTrue(workerStarted.await(1, TimeUnit.SECONDS));
		JsonEnvelope queued = JsonEnvelope.builder("queued-before-replacement").build();
		transport.send(queued);

		assertDoesNotThrow(transport::prepareForReplacement,
				"a staged Control replacement must cancel a first-time retry loop without an active credential");
		java.lang.reflect.Field restoreAbsent =
				HttpBackendProxyTransport.class.getDeclaredField("restoreUnenrolledState");
		restoreAbsent.setAccessible(true);
		assertTrue(restoreAbsent.getBoolean(transport), "rollback must remember that no credential existed");
		assertEquals(List.of(queued), transport.drainPreparedMessages(),
				"messages accepted before replacement must remain available for handoff");

		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
		while (owner.availablePermits() == 0 && System.nanoTime() < deadline) Thread.onSpinWait();
		assertEquals(1, owner.availablePermits(),
				"cancelled enrollment must release the credential-directory ownership");
		owners.remove(ownerKey, owner);
	}

	@Test
	void replacementSnapshotsInitialEnrollmentPublishedWhileCancellationCompletes() throws Exception {
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(mock(VotingPluginMain.class));
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("original-proxy"),
				"proxy.example.test");
		HttpConnectionCode original = new HttpConnectionCode("lobby-1", URI.create("https://proxy.example.test:8443/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "G".repeat(43));
		setField(transport, "configuredDirectory", credentials);
		setField(transport, "configuredServerId", "lobby-1");
		setField(transport, "configuredConnectionCode", original.encode());
		CountDownLatch workerStarted = new CountDownLatch(1);
		java.util.concurrent.atomic.AtomicReference<Throwable> failure = new java.util.concurrent.atomic.AtomicReference<>();
		Thread finishingEnrollment = new Thread(() -> {
			workerStarted.countDown();
			try {
				Thread.sleep(Long.MAX_VALUE);
			} catch (InterruptedException expected) {
				try {
					HttpClientCredentialStore.saveEnrolled(credentials, original,
							identity.issueClientCertificate("lobby-1"));
				} catch (Throwable thrown) { failure.set(thrown); }
			}
		});
		setField(transport, "worker", finishingEnrollment);
		finishingEnrollment.start();
		assertTrue(workerStarted.await(1, TimeUnit.SECONDS));

		transport.prepareForReplacement();

		assertNull(failure.get());
		assertNotNull(field(transport, "configuredCredentialGeneration"),
				"a credential published while cancellation finishes must be retained for rollback");
		assertFalse((boolean) field(transport, "restoreUnenrolledState"));
	}

	@Test
	void rollbackRestoresAnExplicitlyUnenrolledCredentialState() throws Exception {
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("replacement-proxy"),
				"proxy.example.test");
		HttpConnectionCode stagedCode = new HttpConnectionCode("lobby-1",
				URI.create("https://proxy.example.test:1297/"), identity.serverCertificatePin(),
				identity.caCertificatePin(), Instant.now().plusSeconds(60), "E".repeat(43));
		HttpClientCredentialStore.saveEnrolled(credentials, stagedCode,
				identity.issueClientCertificate("lobby-1"));
		assertTrue(HttpClientCredentialStore.hasEnrolledProfile(credentials));

		HttpConnectionCode original = code("lobby-1", Instant.now().plusSeconds(60));
		HttpBackendProxyTransport.restoreUnenrolledCredentialState(credentials, "lobby-1", original.encode());

		assertFalse(HttpClientCredentialStore.hasEnrolledProfile(credentials),
				"a failed staged first enrollment must not remain active during rollback");
	}

	@Test
	void rollbackRetainsEnrollmentIssuedForTheOriginalOneTimeCode() throws Exception {
		Path credentials = directory.resolve("http");
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("same-code-proxy"),
				"proxy.example.test");
		HttpConnectionCode original = new HttpConnectionCode("lobby-1", URI.create("https://proxy.example.test:1297/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "F".repeat(43));
		HttpClientCredentialStore.saveEnrolled(credentials, original, identity.issueClientCertificate("lobby-1"));

		HttpBackendProxyTransport.restoreUnenrolledCredentialState(credentials, "lobby-1", original.encode());

		assertTrue(HttpClientCredentialStore.hasEnrolledProfile(credentials),
				"a credential issued for the consumed original code must remain recoverable");
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

	@Test
	void preparedHandoffMessagesStayAheadOfNewlyPublishedSendsWhenConnectorIsFull() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		HttpBackendProxyTransport transport = new HttpBackendProxyTransport(plugin);
		HttpBackendTransportConnector connector = mock(HttpBackendTransportConnector.class);
		when(connector.send(org.mockito.ArgumentMatchers.any(JsonEnvelope.class))).thenReturn(false);
		setField(transport, "connector", connector);
		JsonEnvelope acceptedBeforePublication = JsonEnvelope.builder("old").build();
		JsonEnvelope sentAfterPublication = JsonEnvelope.builder("new").build();

		transport.beginPreparedHandoff();
		transport.send(sentAfterPublication);
		transport.acceptHandoffMessages(List.of(acceptedBeforePublication));

		assertEquals(List.of(acceptedBeforePublication, sentAfterPublication), transport.handoffMessagesSnapshot());
		transport.close();
	}

	private static HttpConnectionCode code(String serverId, Instant expiry) {
		return new HttpConnectionCode(serverId, URI.create("https://proxy.example.test:1297/"), "a".repeat(64),
				"b".repeat(64), expiry, "A".repeat(43));
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		java.lang.reflect.Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static Object field(Object target, String name) throws Exception {
		java.lang.reflect.Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}
}
