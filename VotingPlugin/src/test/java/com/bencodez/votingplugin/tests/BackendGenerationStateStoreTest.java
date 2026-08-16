package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.presence.BackendGenerationStateStore;
import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker;

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
}
