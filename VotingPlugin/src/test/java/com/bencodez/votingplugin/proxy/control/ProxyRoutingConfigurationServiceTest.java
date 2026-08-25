package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

class ProxyRoutingConfigurationServiceTest {
	@Test
	void validatesBlockedServersAgainstCompleteConfiguredSet() {
		FakePlatform platform = new FakePlatform();
		platform.configuredServers = Set.of("Lobby");

		ProxyRoutingConfigurationService service = new ProxyRoutingConfigurationService(platform);
		assertDoesNotThrow(() -> service.validate(new ProxyRoutingConfiguration(true, List.of("Lobby"))));
		assertThrows(IllegalArgumentException.class,
				() -> service.validate(new ProxyRoutingConfiguration(true, List.of("lobby"))));
	}

	@Test
	void strictReloadFailureRestoresBackupAndReloadsAgain() throws Exception {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		ProxyRoutingConfiguration proposal = new ProxyRoutingConfiguration(true, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.failFirstReload = true;

		ProxyRoutingConfigurationService.ApplyFailureException failure = assertThrows(
				ProxyRoutingConfigurationService.ApplyFailureException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(proposal, current.revision()));

		assertTrue(failure.rolledBack());
		assertTrue(platform.rolledBack);
		assertTrue(platform.reloadCalls == 2);
	}

	private static final class FakePlatform implements ProxyRoutingConfigurationService.Platform {
		private ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		private Set<String> configuredServers = Set.of();
		private boolean failFirstReload;
		private boolean rolledBack;
		private int reloadCalls;

		@Override public ProxyRoutingConfiguration read() { return current; }
		@Override public Set<String> configuredServers() { return configuredServers; }
		@Override public void persist(ProxyRoutingConfiguration proposal) { current = proposal; }
		@Override public void rollback() { rolledBack = true; }
		@Override public void reload() throws Exception {
			reloadCalls++;
			if (failFirstReload && reloadCalls == 1) throw new IOException("invalid reloaded configuration");
		}
	}
}
