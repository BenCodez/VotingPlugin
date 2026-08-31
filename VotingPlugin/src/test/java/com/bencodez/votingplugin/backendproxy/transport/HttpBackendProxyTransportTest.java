package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.http.HttpClientCredentialStore;
import com.bencodez.votingplugin.backendproxy.http.HttpConnectionCode;
import com.bencodez.votingplugin.backendproxy.http.HttpTlsIdentity;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import java.net.URI;
import java.nio.file.Path;
import java.time.Instant;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HttpBackendProxyTransportTest {
	@TempDir Path directory;

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
		assertDoesNotThrow(transport::validate);
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

	private static HttpConnectionCode code(String serverId, Instant expiry) {
		return new HttpConnectionCode(serverId, URI.create("https://proxy.example.test:1297/"), "a".repeat(64),
				"b".repeat(64), expiry, "A".repeat(43));
	}
}
