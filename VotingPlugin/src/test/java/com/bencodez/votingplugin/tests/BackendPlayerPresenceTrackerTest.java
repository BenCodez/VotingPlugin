package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.VotingPluginWire.PresencePlayer;
import com.bencodez.votingplugin.proxy.presence.BackendPlayerPresenceTracker;
import com.bencodez.votingplugin.proxy.presence.PlayerPresence;

public class BackendPlayerPresenceTrackerTest {

	@Test
	public void tracksPlayersByUuidAndCaseInsensitiveName() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID connectionId = UUID.randomUUID();

		assertTrue(tracker.playerOnline("Player", uuid.toString(), "survival", connectionId, 10L));

		PlayerPresence byUuid = tracker.getPlayer(uuid).orElseThrow();
		PlayerPresence byName = tracker.getPlayer("pLaYeR").orElseThrow();
		assertEquals(byUuid, byName);
		assertEquals("survival", byUuid.getServer());
		assertEquals(connectionId, byUuid.getConnectionId());
		assertEquals(1, tracker.getOnlinePlayerCount());
	}

	@Test
	public void conflictingServerClaimIsRejectedWhenModernPresenceIsKnown() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID connectionId = UUID.randomUUID();

		assertTrue(tracker.playerOnline("Player", uuid.toString(), "survival", connectionId, 10L));

		assertFalse(tracker.hasConflictingPresence("Player", uuid.toString(), "survival"));
		assertTrue(tracker.hasConflictingPresence("Player", uuid.toString(), "creative"));
		assertTrue(tracker.hasConflictingPresence("player", UUID.randomUUID().toString(), "creative"));
	}

	@Test
	public void proxyRestartTreatsPlayersAsOfflineUntilFreshSnapshot() {
		UUID incarnationId = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		BackendPlayerPresenceTracker beforeRestart = new BackendPlayerPresenceTracker();
		assertTrue(beforeRestart.backendStarted("survival", incarnationId, 1000L, 1000L, 10L));
		assertTrue(beforeRestart.playerOnline("Player", playerUuid.toString(), "survival", UUID.randomUUID(),
				incarnationId, 1000L, 1100L, 20L));

		BackendPlayerPresenceTracker afterRestart = new BackendPlayerPresenceTracker();
		assertTrue(afterRestart.getPlayer(playerUuid).isEmpty());
		assertTrue(afterRestart.backendStarted("survival", incarnationId, 1000L, 1000L, 30L));
		UUID requestId = UUID.randomUUID();
		assertEquals(requestId, afterRestart.beginSnapshot("survival", requestId, incarnationId, 1000L, 40L));
		assertTrue(afterRestart.applySnapshotChunk("survival", requestId, 0, 1, Collections.emptyList(),
				incarnationId, 1000L, 1200L, 50L));
		assertEquals(0, afterRestart.getOnlinePlayerCount());
	}

	@Test
	public void olderIncarnationCannotReplaceFirstGenerationSeenAfterProxyRestart() {
		BackendPlayerPresenceTracker afterRestart = new BackendPlayerPresenceTracker();
		UUID olderIncarnation = UUID.randomUUID();
		UUID survivingIncarnation = UUID.randomUUID();

		assertTrue(afterRestart.backendStarted("survival", survivingIncarnation, 2000L, 2000L, 10L));
		assertFalse(afterRestart.backendStarted("survival", olderIncarnation, 1000L, 2100L, 20L));
		assertFalse(afterRestart.backendStopped("survival", olderIncarnation, 1000L, 2200L, 30L));
		assertEquals(survivingIncarnation, afterRestart.getBackendIncarnationId("survival"));
	}

	@Test
	public void staleLogoutCannotClearNewerConnection() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID firstConnection = UUID.randomUUID();
		UUID secondConnection = UUID.randomUUID();

		tracker.playerOnline("Player", uuid.toString(), "survival", firstConnection, 10L);
		tracker.playerOnline("Player", uuid.toString(), "creative", secondConnection, 20L);

		assertFalse(tracker.playerOffline(uuid.toString(), "survival", firstConnection, 30L));
		assertEquals(secondConnection, tracker.getPlayer(uuid).orElseThrow().getConnectionId());
		assertTrue(tracker.playerOffline(uuid.toString(), "creative", secondConnection, 40L));
		assertTrue(tracker.getPlayer(uuid).isEmpty());
	}

	@Test
	public void snapshotRebuildsOneBackendWithoutTouchingOthers() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID oldUuid = UUID.randomUUID();
		UUID otherUuid = UUID.randomUUID();
		UUID snapshotUuid = UUID.randomUUID();
		tracker.playerOnline("Old", oldUuid.toString(), "survival", UUID.randomUUID(), 10L);
		tracker.playerOnline("Other", otherUuid.toString(), "creative", UUID.randomUUID(), 10L);
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);

		assertTrue(tracker.applySnapshot("survival", requestId,
				List.of(player("Current", snapshotUuid)), 20L));

		assertTrue(tracker.getPlayer(oldUuid).isEmpty());
		assertEquals("survival", tracker.getPlayer(snapshotUuid).orElseThrow().getServer());
		assertEquals("creative", tracker.getPlayer(otherUuid).orElseThrow().getServer());
	}

	@Test
	public void snapshotCannotOverwriteLoginReceivedAfterRequest() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID oldConnection = UUID.randomUUID();
		UUID newConnection = UUID.randomUUID();
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);
		tracker.playerOnline("Player", uuid.toString(), "creative", newConnection, 20L);

		assertTrue(tracker.applySnapshot("survival", requestId,
				List.of(new PresencePlayer("Player", uuid.toString(), oldConnection.toString())), 30L));

		PlayerPresence current = tracker.getPlayer(uuid).orElseThrow();
		assertEquals("creative", current.getServer());
		assertEquals(newConnection, current.getConnectionId());
	}

	@Test
	public void snapshotCannotRestoreLogoutReceivedAfterRequest() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID connectionId = UUID.randomUUID();
		tracker.playerOnline("Player", uuid.toString(), "survival", connectionId, 10L);
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);
		tracker.playerOffline(uuid.toString(), "survival", connectionId, 20L);

		assertTrue(tracker.applySnapshot("survival", requestId,
				List.of(new PresencePlayer("Player", uuid.toString(), connectionId.toString())), 30L));

		assertTrue(tracker.getPlayer(uuid).isEmpty());
	}

	@Test
	public void snapshotCannotEvictNewerOwnerOfTheSameName() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID snapshotUuid = UUID.randomUUID();
		UUID currentUuid = UUID.randomUUID();
		UUID currentConnection = UUID.randomUUID();
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);
		tracker.playerOnline("Player", currentUuid.toString(), "creative", currentConnection, 20L);

		assertTrue(tracker.applySnapshot("survival", requestId,
				List.of(new PresencePlayer("player", snapshotUuid.toString(), UUID.randomUUID().toString())), 30L));

		assertTrue(tracker.getPlayer(snapshotUuid).isEmpty());
		PlayerPresence current = tracker.getPlayer("PLAYER").orElseThrow();
		assertEquals(currentUuid, current.getUuid());
		assertEquals(currentConnection, current.getConnectionId());
		assertEquals(1, tracker.getOnlinePlayerCount());
	}

	@Test
	public void backendLifecycleAndTimeoutRemoveOwnedPlayers() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		tracker.playerOnline("Player", uuid.toString(), "survival", UUID.randomUUID(), 10L);

		tracker.backendStarted("survival", 20L);
		assertTrue(tracker.getPlayer(uuid).isEmpty());
		assertTrue(tracker.getBackendStatus("survival").isAvailable());

		tracker.playerOnline("Player", uuid.toString(), "survival", UUID.randomUUID(), 30L);
		Set<String> expired = tracker.expireBackends(91L, 60L);
		assertEquals(Set.of("survival"), expired);
		assertTrue(tracker.getPlayer(uuid).isEmpty());
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
	}

	@Test
	public void staleLogoutCannotReviveStoppedBackend() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID uuid = UUID.randomUUID();
		UUID connectionId = UUID.randomUUID();
		tracker.playerOnline("Player", uuid.toString(), "survival", connectionId, 10L);
		tracker.backendStopped("survival", 20L);

		assertFalse(tracker.playerOffline(uuid.toString(), "survival", connectionId, 30L));

		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertEquals(20L, tracker.getBackendStatus("survival").getLastSeen());
	}

	@Test
	public void delayedStopFromOlderBackendGenerationCannotClearReplacement() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID oldIncarnation = UUID.randomUUID();
		UUID replacementIncarnation = UUID.randomUUID();
		UUID oldUuid = UUID.randomUUID();
		UUID replacementUuid = UUID.randomUUID();
		UUID replacementConnection = UUID.randomUUID();

		assertTrue(tracker.backendStarted("survival", oldIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.playerOnline("Old", oldUuid.toString(), "survival", UUID.randomUUID(),
				oldIncarnation, 1000L, 1100L, 11L));
		assertTrue(tracker.backendStarted("survival", replacementIncarnation, 2000L, 2000L, 20L));
		assertTrue(tracker.playerOnline("Replacement", replacementUuid.toString(), "survival",
				replacementConnection, replacementIncarnation, 2000L, 2100L, 21L));

		// The old process shuts down later in wall-clock time, but its generation is
		// still obsolete and must not win.
		assertFalse(tracker.backendStopped("survival", oldIncarnation, 1000L, 2200L, 22L));

		assertTrue(tracker.getBackendStatus("survival").isAvailable());
		assertTrue(tracker.getPlayer(oldUuid).isEmpty());
		assertEquals(replacementConnection, tracker.getPlayer(replacementUuid).orElseThrow().getConnectionId());
		assertEquals(2100L, tracker.getPlayer(replacementUuid).orElseThrow().getLastSeen());
	}

	@Test
	public void stoppedBackendGenerationCannotBeRevivedByDelayedHeartbeat() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStopped("survival", backendIncarnation, 1000L, 1100L, 20L));

		assertFalse(tracker.heartbeat("survival", backendIncarnation, 1000L, 1050L, 30L));

		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertEquals(20L, tracker.getBackendStatus("survival").getLastSeen());
	}

	@Test
	public void captureTimestampRejectsReorderedPlayerEventsWithinGeneration() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		UUID uuid = UUID.randomUUID();
		UUID currentConnection = UUID.randomUUID();
		UUID delayedConnection = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.playerOnline("Player", uuid.toString(), "survival", currentConnection,
				backendIncarnation, 1000L, 1200L, 20L));

		assertFalse(tracker.playerOnline("Player", uuid.toString(), "survival", delayedConnection,
				backendIncarnation, 1000L, 1100L, 30L));
		assertFalse(tracker.playerOffline(uuid.toString(), "survival", currentConnection,
				backendIncarnation, 1000L, 1150L, 40L));

		PlayerPresence current = tracker.getPlayer(uuid).orElseThrow();
		assertEquals(currentConnection, current.getConnectionId());
		assertEquals(1200L, current.getLastSeen());
	}

	@Test
	public void staleLoginCannotRefreshOrReviveExpiredBackend() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "survival", UUID.randomUUID(),
				backendIncarnation, 1000L, 1200L, 20L));

		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "survival", UUID.randomUUID(),
				backendIncarnation, 1000L, 1100L, 50L));
		assertEquals(20L, tracker.getBackendStatus("survival").getLastSeen());
		assertEquals(Set.of("survival"), tracker.expireBackends(81L, 60L));

		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "survival", UUID.randomUUID(),
				backendIncarnation, 1000L, 1200L, 90L));
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertEquals(20L, tracker.getBackendStatus("survival").getLastSeen());
		assertTrue(tracker.getPlayer(playerUuid).isEmpty());
	}

	@Test
	public void delayedHeartbeatCannotReviveExpiredBackend() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.heartbeat("survival", backendIncarnation, 1000L, 1200L, 20L));
		assertEquals(Set.of("survival"), tracker.expireBackends(81L, 60L));

		assertFalse(tracker.heartbeat("survival", backendIncarnation, 1000L, 1200L, 90L));
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertEquals(20L, tracker.getBackendStatus("survival").getLastSeen());
	}

	@Test
	public void replayedLifecycleMessageCannotReviveExpiredBackend() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertEquals(Set.of("survival"), tracker.expireBackends(71L, 60L));

		assertFalse(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 80L));
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertEquals(10L, tracker.getBackendStatus("survival").getLastSeen());
	}

	@Test
	public void replacementWithRegressedStartupClockWaitsForCurrentGenerationToStop() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID oldIncarnation = UUID.randomUUID();
		UUID replacementIncarnation = UUID.randomUUID();
		UUID replacementUuid = UUID.randomUUID();

		assertTrue(tracker.backendStarted("survival", oldIncarnation, 2000L, 2000L, 10L));
		assertFalse(tracker.backendStarted("survival", replacementIncarnation, 1000L, 1000L, 20L));
		assertTrue(tracker.backendStopped("survival", oldIncarnation, 2000L, 2100L, 21L));
		assertTrue(tracker.backendStarted("survival", replacementIncarnation, 1000L, 1000L, 22L));
		assertTrue(tracker.playerOnline("Replacement", replacementUuid.toString(), "survival", UUID.randomUUID(),
				replacementIncarnation, 1000L, 1100L, 23L));

		assertFalse(tracker.backendStarted("survival", oldIncarnation, 2000L, 2200L, 30L));
		assertEquals(replacementIncarnation, tracker.getBackendIncarnationId("survival"));
		assertTrue(tracker.getPlayer(replacementUuid).isPresent());
	}

	@Test
	public void olderLoginCannotOverwriteNewerCrossBackendMove() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID survivalIncarnation = UUID.randomUUID();
		UUID creativeIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		UUID creativeConnection = UUID.randomUUID();

		assertTrue(tracker.backendStarted("survival", survivalIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStarted("creative", creativeIncarnation, 1000L, 1000L, 11L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "creative", creativeConnection,
				creativeIncarnation, 1000L, 2000L, 20L));

		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "survival", UUID.randomUUID(),
				survivalIncarnation, 1000L, 1500L, 30L));
		PlayerPresence current = tracker.getPlayer(playerUuid).orElseThrow();
		assertEquals("creative", current.getServer());
		assertEquals(creativeConnection, current.getConnectionId());
	}

	@Test
	public void snapshotConfirmsCrossBackendMoveWithoutComparingBackendClocks() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID survivalIncarnation = UUID.randomUUID();
		UUID creativeIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		UUID creativeConnection = UUID.randomUUID();
		UUID survivalConnection = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", survivalIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStarted("creative", creativeIncarnation, 1000L, 1000L, 11L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "creative", creativeConnection,
				creativeIncarnation, 1000L, 5000L, 20L));

		// The destination clock is behind the source clock. Its login cannot directly
		// replace the source, but it advances the destination event fence.
		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "survival", survivalConnection,
				survivalIncarnation, 1000L, 1100L, 30L));
		UUID requestId = UUID.randomUUID();
		assertEquals(requestId,
				tracker.beginSnapshot("survival", requestId, survivalIncarnation, 1000L, 40L));
		assertTrue(tracker.playerOffline(playerUuid.toString(), "creative", creativeConnection,
				creativeIncarnation, 1000L, 5100L, 50L));

		assertTrue(tracker.applySnapshotChunk("survival", requestId, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), survivalConnection.toString())),
				survivalIncarnation, 1000L, 1200L, 60L));
		PlayerPresence current = tracker.getPlayer(playerUuid).orElseThrow();
		assertEquals("survival", current.getServer());
		assertEquals(survivalConnection, current.getConnectionId());
	}

	@Test
	public void newerHandoffFencesSnapshotForSupersededDestination() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID sourceIncarnation = UUID.randomUUID();
		UUID firstDestinationIncarnation = UUID.randomUUID();
		UUID finalDestinationIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		UUID sourceConnection = UUID.randomUUID();
		UUID firstDestinationConnection = UUID.randomUUID();
		UUID finalDestinationConnection = UUID.randomUUID();
		UUID firstRequest = UUID.randomUUID();
		UUID finalRequest = UUID.randomUUID();

		assertTrue(tracker.backendStarted("source", sourceIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStarted("first", firstDestinationIncarnation, 1000L, 1000L, 11L));
		assertTrue(tracker.backendStarted("final", finalDestinationIncarnation, 1000L, 1000L, 12L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "source", sourceConnection,
				sourceIncarnation, 1000L, 1100L, 20L));

		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "first", firstDestinationConnection,
				firstDestinationIncarnation, 1000L, 1200L, 30L));
		assertEquals(firstRequest,
				tracker.beginSnapshot("first", firstRequest, firstDestinationIncarnation, 1000L, 31L));
		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "final", finalDestinationConnection,
				finalDestinationIncarnation, 1000L, 1300L, 40L));
		assertEquals(finalRequest,
				tracker.beginSnapshot("final", finalRequest, finalDestinationIncarnation, 1000L, 41L));

		assertTrue(tracker.applySnapshotChunk("first", firstRequest, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), firstDestinationConnection.toString())),
				firstDestinationIncarnation, 1000L, 1400L, 50L));
		assertEquals("source", tracker.getPlayer(playerUuid).orElseThrow().getServer());

		assertTrue(tracker.applySnapshotChunk("final", finalRequest, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), finalDestinationConnection.toString())),
				finalDestinationIncarnation, 1000L, 1500L, 60L));
		PlayerPresence current = tracker.getPlayer(playerUuid).orElseThrow();
		assertEquals("final", current.getServer());
		assertEquals(finalDestinationConnection, current.getConnectionId());
	}

	@Test
	public void sourceLogoutPreservesNewerDestinationFence() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID sourceIncarnation = UUID.randomUUID();
		UUID firstDestinationIncarnation = UUID.randomUUID();
		UUID finalDestinationIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		UUID sourceConnection = UUID.randomUUID();
		UUID firstDestinationConnection = UUID.randomUUID();
		UUID finalDestinationConnection = UUID.randomUUID();
		UUID firstRequest = UUID.randomUUID();
		UUID finalRequest = UUID.randomUUID();

		assertTrue(tracker.backendStarted("source", sourceIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStarted("first", firstDestinationIncarnation, 1000L, 1000L, 11L));
		assertTrue(tracker.backendStarted("final", finalDestinationIncarnation, 1000L, 1000L, 12L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "source", sourceConnection,
				sourceIncarnation, 1000L, 1100L, 20L));

		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "first", firstDestinationConnection,
				firstDestinationIncarnation, 1000L, 1200L, 30L));
		assertEquals(firstRequest,
				tracker.beginSnapshot("first", firstRequest, firstDestinationIncarnation, 1000L, 31L));
		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "final", finalDestinationConnection,
				finalDestinationIncarnation, 1000L, 1300L, 40L));
		assertEquals(finalRequest,
				tracker.beginSnapshot("final", finalRequest, finalDestinationIncarnation, 1000L, 41L));

		assertTrue(tracker.playerOffline(playerUuid.toString(), "source", sourceConnection,
				sourceIncarnation, 1000L, 1400L, 50L));
		assertTrue(tracker.getPlayer(playerUuid).isEmpty());

		assertTrue(tracker.applySnapshotChunk("first", firstRequest, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), firstDestinationConnection.toString())),
				firstDestinationIncarnation, 1000L, 1500L, 60L));
		assertTrue(tracker.getPlayer(playerUuid).isEmpty());

		assertTrue(tracker.applySnapshotChunk("final", finalRequest, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), finalDestinationConnection.toString())),
				finalDestinationIncarnation, 1000L, 1600L, 70L));
		PlayerPresence current = tracker.getPlayer(playerUuid).orElseThrow();
		assertEquals("final", current.getServer());
		assertEquals(finalDestinationConnection, current.getConnectionId());
	}

	@Test
	public void destinationLogoutPreventsInFlightHandoffSnapshotFromRestoringPlayer() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID survivalIncarnation = UUID.randomUUID();
		UUID creativeIncarnation = UUID.randomUUID();
		UUID playerUuid = UUID.randomUUID();
		UUID creativeConnection = UUID.randomUUID();
		UUID survivalConnection = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", survivalIncarnation, 1000L, 1000L, 10L));
		assertTrue(tracker.backendStarted("creative", creativeIncarnation, 1000L, 1000L, 11L));
		assertTrue(tracker.playerOnline("Player", playerUuid.toString(), "creative", creativeConnection,
				creativeIncarnation, 1000L, 5000L, 20L));
		assertFalse(tracker.playerOnline("Player", playerUuid.toString(), "survival", survivalConnection,
				survivalIncarnation, 1000L, 1100L, 30L));
		UUID requestId = UUID.randomUUID();
		assertEquals(requestId,
				tracker.beginSnapshot("survival", requestId, survivalIncarnation, 1000L, 40L));

		// This snapshot was captured before the destination logout but delivered after it.
		assertTrue(tracker.playerOffline(playerUuid.toString(), "survival", survivalConnection,
				survivalIncarnation, 1000L, 1200L, 50L));
		assertTrue(tracker.applySnapshotChunk("survival", requestId, 0, 1,
				List.of(new PresencePlayer("Player", playerUuid.toString(), survivalConnection.toString())),
				survivalIncarnation, 1000L, 1150L, 60L));

		PlayerPresence current = tracker.getPlayer(playerUuid).orElseThrow();
		assertEquals("creative", current.getServer());
		assertEquals(creativeConnection, current.getConnectionId());
	}

	@Test
	public void snapshotRequestsAreDeduplicatedAndRateLimitedBeforePendingStateChanges() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		UUID firstRequest = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		assertEquals(firstRequest,
				tracker.beginSnapshot("survival", firstRequest, backendIncarnation, 1000L, 20L));
		assertNull(tracker.beginSnapshot("survival", UUID.randomUUID(), backendIncarnation, 1000L, 21L));

		assertTrue(tracker.applySnapshotChunk("survival", firstRequest, 0, 1, List.of(),
				backendIncarnation, 1000L, 1100L, 22L));
		assertNull(tracker.beginSnapshot("survival", UUID.randomUUID(), backendIncarnation, 1000L, 23L));
		UUID laterRequest = UUID.randomUUID();
		assertEquals(laterRequest,
				tracker.beginSnapshot("survival", laterRequest, backendIncarnation, 1000L, 30020L));
	}

	@Test
	public void backwardClockJumpExpiresSnapshotAndResetsRequestCooldown() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID backendIncarnation = UUID.randomUUID();
		UUID firstRequest = UUID.randomUUID();
		UUID replacementRequest = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 100000L));
		assertEquals(firstRequest,
				tracker.beginSnapshot("survival", firstRequest, backendIncarnation, 1000L, 100000L));

		assertNull(tracker.getPendingSnapshotRequestId("survival", 90000L));
		assertEquals(replacementRequest,
				tracker.beginSnapshot("survival", replacementRequest, backendIncarnation, 1000L, 90000L));
	}

	@Test
	public void pendingSnapshotsCannotGrowLogoutTombstonesBeyondAggregateCap() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker(4, 2);
		UUID backendIncarnation = UUID.randomUUID();
		assertTrue(tracker.backendStarted("survival", backendIncarnation, 1000L, 1000L, 10L));
		tracker.beginSnapshot("survival", UUID.randomUUID(), backendIncarnation, 1000L, 11L);

		for (int index = 0; index < 3; index++) {
			UUID playerUuid = UUID.randomUUID();
			UUID connectionId = UUID.randomUUID();
			long timestamp = 1100L + index * 2L;
			assertTrue(tracker.playerOnline("Player" + index, playerUuid.toString(), "survival", connectionId,
					backendIncarnation, 1000L, timestamp, 20L + index));
			assertTrue(tracker.playerOffline(playerUuid.toString(), "survival", connectionId,
					backendIncarnation, 1000L, timestamp + 1L, 30L + index));
			assertTrue(tracker.getRetainedPlayerEventCount() <= 2);
		}

		assertEquals(0, tracker.getPendingSnapshotCount());
	}

	@Test
	public void backendStateIsBoundedAndEvictsUnavailableEntries() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker(2);
		tracker.heartbeat("survival", 10L);
		tracker.heartbeat("creative", 20L);
		tracker.heartbeat("rejected", 30L);

		assertEquals(2, tracker.getTrackedBackendCount());
		assertNull(tracker.getBackendStatus("rejected"));

		tracker.backendStopped("survival", 40L);
		tracker.heartbeat("minigames", 50L);

		assertEquals(2, tracker.getTrackedBackendCount());
		assertNull(tracker.getBackendStatus("survival"));
		assertTrue(tracker.getBackendStatus("minigames").isAvailable());
	}

	@Test
	public void expiredBackendStateRetainsReplayFences() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		tracker.heartbeat("survival", 10L);

		assertEquals(Set.of("survival"), tracker.expireBackends(71L, 60L));
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
		assertTrue(tracker.expireBackends(72L, 60L).isEmpty());
		assertFalse(tracker.getBackendStatus("survival").isAvailable());
	}

	@Test
	public void unexpectedSnapshotRequestIsIgnored() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();

		assertFalse(tracker.applySnapshot("survival", UUID.randomUUID(), List.of(), 10L));
	}

	@Test
	public void chunkedSnapshotAppliesOnlyAfterEveryChunkArrives() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID firstUuid = UUID.randomUUID();
		UUID secondUuid = UUID.randomUUID();
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);

		assertTrue(tracker.applySnapshotChunk("survival", requestId, 1, 2,
				List.of(player("Second", secondUuid)), 20L));
		assertEquals(0, tracker.getOnlinePlayerCount());
		assertTrue(tracker.applySnapshotChunk("survival", requestId, 0, 2,
				List.of(player("First", firstUuid)), 30L));

		assertEquals(2, tracker.getOnlinePlayerCount());
		assertTrue(tracker.getPlayer(firstUuid).isPresent());
		assertTrue(tracker.getPlayer(secondUuid).isPresent());
		assertEquals(0, tracker.getPendingSnapshotCount());
	}

	@Test
	public void snapshotPlayerLimitIsEnforcedBeforeChunksAreRetained() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID requestId = UUID.randomUUID();
		PresencePlayer player = player("Player", UUID.randomUUID());
		tracker.beginSnapshot("survival", requestId, 10L);

		assertTrue(tracker.applySnapshotChunk("survival", requestId, 0, 3,
				Collections.nCopies(50001, player), 20L));
		assertFalse(tracker.applySnapshotChunk("survival", requestId, 1, 3,
				Collections.nCopies(50000, player), 30L));

		assertEquals(0, tracker.getPendingSnapshotCount());
	}

	@Test
	public void incompleteSnapshotExpiresWhileBackendRemainsHealthy() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID requestId = UUID.randomUUID();
		tracker.beginSnapshot("survival", requestId, 10L);
		assertTrue(tracker.applySnapshotChunk("survival", requestId, 0, 2,
				List.of(player("Player", UUID.randomUUID())), 20L));

		tracker.heartbeat("survival", 120010L);

		assertEquals(0, tracker.getPendingSnapshotCount());
		assertFalse(tracker.applySnapshotChunk("survival", requestId, 1, 2, List.of(), 120011L));
	}

	@Test
	public void pendingSnapshotRequestIdTracksCompletionAndExpiry() {
		BackendPlayerPresenceTracker tracker = new BackendPlayerPresenceTracker();
		UUID completedRequest = UUID.randomUUID();
		tracker.beginSnapshot("survival", completedRequest, 10L);

		assertEquals(completedRequest, tracker.getPendingSnapshotRequestId("survival", 20L));
		assertTrue(tracker.applySnapshot("survival", completedRequest, List.of(), 30L));
		assertNull(tracker.getPendingSnapshotRequestId("survival", 40L));

		UUID expiredRequest = UUID.randomUUID();
		tracker.beginSnapshot("survival", expiredRequest, 50L);
		assertEquals(expiredRequest, tracker.getPendingSnapshotRequestId("survival", 120049L));
		assertNull(tracker.getPendingSnapshotRequestId("survival", 120050L));
	}

	private static PresencePlayer player(String name, UUID uuid) {
		return new PresencePlayer(name, uuid.toString(), UUID.randomUUID().toString());
	}
}
