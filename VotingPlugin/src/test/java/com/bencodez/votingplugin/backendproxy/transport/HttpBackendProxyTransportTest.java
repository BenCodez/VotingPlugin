package com.bencodez.votingplugin.backendproxy.transport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.http.HttpConnectionCode;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import java.net.URI;
import java.nio.file.Path;
import java.time.Instant;
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

	private static HttpConnectionCode code(String serverId, Instant expiry) {
		return new HttpConnectionCode(serverId, URI.create("https://proxy.example.test:1297/"), "a".repeat(64),
				"b".repeat(64), expiry, "A".repeat(43));
	}
}
