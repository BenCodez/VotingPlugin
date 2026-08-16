package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.presence.BackendGenerationStateStore;
import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker;
import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker.BackendGenerationState;

public class BackendGenerationStateStoreTest {
	@TempDir
	Path temporaryDirectory;

	@Test
	public void persistedProxyOrderFencesOldReplayWithoutComparingBackendClocks() throws Exception {
		BackendGenerationStateStore store = new BackendGenerationStateStore(temporaryDirectory);
		UUID oldIncarnation = UUID.randomUUID();
		UUID replacementIncarnation = UUID.randomUUID();

		BackendPlayerPresenceTracker beforeProxyRestart = new BackendPlayerPresenceTracker();
		assertTrue(beforeProxyRestart.backendStarted("survival", oldIncarnation, 1000L, 1000L, 10L));
		store.save(beforeProxyRestart);

		BackendPlayerPresenceTracker afterFirstProxyRestart = new BackendPlayerPresenceTracker();
		assertEquals(Set.of("survival"),
				store.loadInto(afterFirstProxyRestart, List.of("survival"), 20L));
		// Equal or rolled-back wall clocks are allowed: authenticated receipt order at
		// the proxy establishes the replacement, not backendStartedAt.
		assertTrue(afterFirstProxyRestart.backendStarted("survival", replacementIncarnation,
				1000L, 1000L, 30L));
		store.save(afterFirstProxyRestart);

		BackendPlayerPresenceTracker afterSecondProxyRestart = new BackendPlayerPresenceTracker();
		assertEquals(Set.of("survival"),
				store.loadInto(afterSecondProxyRestart, List.of("survival"), 40L));
		assertFalse(afterSecondProxyRestart.backendStarted("survival", oldIncarnation,
				1000L, 1000L, 50L));
		assertTrue(afterSecondProxyRestart.heartbeat("survival", replacementIncarnation,
				1000L, 1100L, 60L));
		assertEquals(replacementIncarnation,
				afterSecondProxyRestart.getBackendIncarnationId("survival"));
	}

	@Test
	public void maximumBoundedStateCanBeSavedAndLoaded() throws Exception {
		BackendGenerationStateStore store = new BackendGenerationStateStore(temporaryDirectory);
		List<BackendGenerationState> states = new ArrayList<>();
		List<String> configuredServers = new ArrayList<>();
		for (int backendIndex = 0; backendIndex < 1024; backendIndex++) {
			String suffix = String.format("%04d", backendIndex);
			String server = "s".repeat(124) + suffix;
			configuredServers.add(server);
			Set<UUID> retired = new LinkedHashSet<>();
			for (int retiredIndex = 0; retiredIndex < 64; retiredIndex++) {
				retired.add(new UUID(backendIndex, retiredIndex));
			}
			states.add(new BackendGenerationState(server, new UUID(-1L, backendIndex),
					10L, 10L, false, retired));
		}
		BackendPlayerPresenceTracker source = new BackendPlayerPresenceTracker() {
			@Override
			public synchronized List<BackendGenerationState> getBackendGenerationStates() {
				return states;
			}
		};

		store.save(source);
		assertTrue(Files.size(temporaryDirectory.resolve("backend-presence-generations.dat")) > 1024L * 1024L);

		BackendPlayerPresenceTracker restored = new BackendPlayerPresenceTracker();
		assertEquals(1024, store.loadInto(restored, configuredServers, 20L).size());
		assertEquals(1024, restored.getTrackedBackendCount());
	}
}
