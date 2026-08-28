package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.DurableFiles;

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

	@Test
	void persistenceRevisionRaceRemainsAStaleRevision() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.staleDuringPersist = true;

		assertThrows(ProxyRoutingConfigurationService.StaleRevisionException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));
		assertTrue(platform.reloadCalls == 0);
	}

	@Test
	void postRenameDirectoryForceFailureRollsBackPublishedConfiguration() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.failAfterPublication = true;

		ProxyRoutingConfigurationService.ApplyFailureException failure = assertThrows(
				ProxyRoutingConfigurationService.ApplyFailureException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));

		assertTrue(failure.rolledBack());
		assertTrue(platform.rolledBack);
		assertTrue(platform.current.equals(current));
		assertTrue(platform.reloadCalls == 1);
	}

	@Test
	void publishedRollbackStillReloadsTheRestoredConfiguration() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.failFirstReload = true;
		platform.failAfterRollbackPublication = true;

		ProxyRoutingConfigurationService.ApplyFailureException failure = assertThrows(
				ProxyRoutingConfigurationService.ApplyFailureException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));

		assertTrue(failure.rolledBack());
		assertTrue(platform.rolledBack);
		assertTrue(platform.current.equals(current));
		assertTrue(platform.reloadCalls == 2);
		assertTrue(java.util.Arrays.stream(failure.getCause().getSuppressed())
				.anyMatch(DurableFiles.PublishedException.class::isInstance));
	}

	@Test
	void failedReloadPreservesAConcurrentEditDetectedByTheAdapter() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.manualEditDuringFailedReload = true;

		ProxyRoutingConfigurationService.ApplyFailureException failure = assertThrows(
				ProxyRoutingConfigurationService.ApplyFailureException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));

		assertFalse(failure.rolledBack());
		assertFalse(platform.rolledBack);
		assertTrue(platform.current.blockedServers().contains("manual-edit"));
	}

	@Test
	void successfulReloadReconcilesAndRejectsAConcurrentEdit() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.manualEditDuringSuccessfulReload = true;

		assertThrows(ProxyRoutingConfigurationService.StaleRevisionException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));
		assertTrue(platform.reloadCalls == 2);
		assertTrue(platform.current.blockedServers().contains("manual-edit"));
	}

	@Test
	void reconciliationReloadIsRecheckedAndRepeatedAfterAnotherConcurrentEdit() {
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		FakePlatform platform = new FakePlatform();
		platform.current = current;
		platform.manualEditsDuringSuccessfulReloads = 2;

		assertThrows(ProxyRoutingConfigurationService.StaleRevisionException.class,
				() -> new ProxyRoutingConfigurationService(platform).apply(
						new ProxyRoutingConfiguration(true, List.of()), current.revision()));
		assertTrue(platform.reloadCalls == 3);
		assertTrue(platform.current.blockedServers().contains("manual-edit-2"));
	}

	private static final class FakePlatform implements ProxyRoutingConfigurationService.Platform {
		private ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(false, List.of());
		private Set<String> configuredServers = Set.of();
		private boolean failFirstReload;
		private boolean rolledBack;
		private boolean staleDuringPersist;
		private boolean failAfterPublication;
		private boolean failAfterRollbackPublication;
		private boolean manualEditDuringFailedReload;
		private boolean manualEditDuringSuccessfulReload;
		private int manualEditsDuringSuccessfulReloads;
		private ProxyRoutingConfiguration installed;
		private ProxyRoutingConfiguration previous;
		private int reloadCalls;

		@Override public ProxyRoutingConfiguration read() { return current; }
		@Override public Set<String> configuredServers() { return configuredServers; }
		@Override public void persist(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException {
			if (staleDuringPersist) throw new com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException();
			previous = current;
			current = proposal;
			installed = proposal;
			if (failAfterPublication) throw new DurableFiles.PublishedException(new IOException("directory force failed"));
		}
		@Override public void rollback() throws IOException {
			if (!current.equals(installed)) throw new IOException("active configuration changed");
			current = previous;
			rolledBack = true;
			if (failAfterRollbackPublication) {
				throw new DurableFiles.PublishedException(new IOException("directory force failed"));
			}
		}
		@Override public void reload() throws Exception {
			reloadCalls++;
			if (manualEditDuringSuccessfulReload && reloadCalls == 1) {
				current = new ProxyRoutingConfiguration(false, List.of("manual-edit"));
			}
			if (reloadCalls <= manualEditsDuringSuccessfulReloads) {
				current = new ProxyRoutingConfiguration(false, List.of("manual-edit-" + reloadCalls));
			}
			if (manualEditDuringFailedReload && reloadCalls == 1) {
				current = new ProxyRoutingConfiguration(false, List.of("manual-edit"));
				throw new IOException("invalid reloaded configuration");
			}
			if (failFirstReload && reloadCalls == 1) throw new IOException("invalid reloaded configuration");
		}
		@Override public void verifyInstalled() throws IOException {
			if (!current.equals(installed)) {
				throw new com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException();
			}
		}
		@Override public byte[] captureInstalledSnapshot() {
			return current.revision().getBytes(StandardCharsets.US_ASCII);
		}
		@Override public void verifyInstalledSnapshot(byte[] snapshot) throws IOException {
			if (!java.util.Arrays.equals(snapshot, current.revision().getBytes(StandardCharsets.US_ASCII))) {
				throw new com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException();
			}
		}
	}
}
