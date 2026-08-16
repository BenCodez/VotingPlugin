package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		tracker.beginSnapshot("survival", requestId);

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
		tracker.beginSnapshot("survival", requestId);
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
		tracker.beginSnapshot("survival", requestId);
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
		tracker.beginSnapshot("survival", requestId);
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
		tracker.beginSnapshot("survival", requestId);

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

	private static PresencePlayer player(String name, UUID uuid) {
		return new PresencePlayer(name, uuid.toString(), UUID.randomUUID().toString());
	}
}
