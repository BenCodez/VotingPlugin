package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.VotingPluginWire.Vote;
import com.bencodez.votingplugin.proxy.VotingPluginWire.VoteDelayRejected;

/**
 * Tests proxy vote wire encoding and decoding.
 */
public class VotingPluginWireTest {

	@Test
	public void voteRoundTripPreservesVoteId() {
		UUID voteId = UUID.randomUUID();

		JsonEnvelope envelope = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "Service", 100L,
				true, true, "totals", voteId, true, false, 1, 1);

		Vote vote = VotingPluginWire.readVote(envelope);

		assertEquals(voteId, vote.voteId);
		assertEquals("Player", vote.player);
		assertEquals("Service", vote.service);
	}

	@Test
	public void voteOnlineRoundTripAllowsMissingVoteId() {
		JsonEnvelope envelope = VotingPluginWire.voteOnline("Player", UUID.randomUUID().toString(), "Service",
				100L, true, true, "totals", null, true, false, 1, 1);

		Vote vote = VotingPluginWire.readVote(envelope);

		assertNull(vote.voteId);
	}

	@Test
	public void voteDelayRejectedRoundTripPreservesContext() {
		String uuid = UUID.randomUUID().toString();
		JsonEnvelope envelope = VotingPluginWire.voteDelayRejected("Player", uuid, "Service", true);

		VoteDelayRejected rejected = VotingPluginWire.readVoteDelayRejected(envelope);

		assertEquals(VotingPluginWire.SUB_VOTE_DELAY_REJECTED, envelope.getSubChannel());
		assertEquals("Player", rejected.player);
		assertEquals(uuid, rejected.uuid);
		assertEquals("Service", rejected.service);
		assertEquals(true, rejected.wasOnline);
	}

	@Test
	public void voteBroadcastPreservesTheOriginalOfflineState() {
		JsonEnvelope envelope = VotingPluginWire.voteBroadcast(UUID.randomUUID().toString(), "Player", "Service",
				100L, "totals", false);

		assertFalse(Boolean.parseBoolean(envelope.getFields().get(VotingPluginWire.K_WAS_ONLINE)));
	}

	@Test
	public void playerPresenceEventsPreserveConnectionIdentity() {
		String uuid = UUID.randomUUID().toString();
		UUID connectionId = UUID.randomUUID();

		VotingPluginWire.PlayerPresenceEvent login = VotingPluginWire
				.readPlayerPresenceEvent(VotingPluginWire.login("Player", uuid, "survival", connectionId));
		VotingPluginWire.PlayerPresenceEvent logout = VotingPluginWire
				.readPlayerPresenceEvent(VotingPluginWire.logout("Player", uuid, "survival", connectionId));

		assertEquals("Player", login.player);
		assertEquals(uuid, login.uuid);
		assertEquals("survival", login.server);
		assertEquals(connectionId, login.connectionId);
		assertEquals(connectionId, logout.connectionId);
	}

	@Test
	public void legacyLoginRemainsReadableWithoutConnectionIdentity() {
		VotingPluginWire.PlayerPresenceEvent login = VotingPluginWire.readPlayerPresenceEvent(
				VotingPluginWire.login("Player", UUID.randomUUID().toString(), "survival"));

		assertNull(login.connectionId);
	}

	@Test
	public void presenceSnapshotRoundTripPreservesPlayers() {
		UUID requestId = UUID.randomUUID();
		String uuid = UUID.randomUUID().toString();
		String connectionId = UUID.randomUUID().toString();
		JsonEnvelope envelope = VotingPluginWire.presenceSnapshot("survival", requestId,
				List.of(new VotingPluginWire.PresencePlayer("Player", uuid, connectionId)));

		VotingPluginWire.PresenceSnapshot snapshot = VotingPluginWire.readPresenceSnapshot(envelope);

		assertTrue(snapshot.valid);
		assertEquals("survival", snapshot.server);
		assertEquals(requestId, snapshot.requestId);
		assertEquals(0, snapshot.chunkIndex);
		assertEquals(1, snapshot.chunkCount);
		assertEquals(1, snapshot.players.size());
		assertEquals("Player", snapshot.players.get(0).player);
		assertEquals(uuid, snapshot.players.get(0).uuid);
		assertEquals(connectionId, snapshot.players.get(0).connectionId);
	}

	@Test
	public void malformedPresenceSnapshotIsRejected() {
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_PRESENCE_SNAPSHOT)
				.schema(VotingPluginWire.SCHEMA_VERSION).put(VotingPluginWire.K_SERVER, "survival")
				.put(VotingPluginWire.K_REQUEST_ID, UUID.randomUUID().toString())
				.put(VotingPluginWire.K_PLAYERS, "not-json").build();

		assertFalse(VotingPluginWire.readPresenceSnapshot(envelope).valid);
	}

	@Test
	public void oversizedPresenceSnapshotIsRejectedBeforeParsing() {
		JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_PRESENCE_SNAPSHOT)
				.schema(VotingPluginWire.SCHEMA_VERSION).put(VotingPluginWire.K_SERVER, "survival")
				.put(VotingPluginWire.K_REQUEST_ID, UUID.randomUUID().toString())
				.put(VotingPluginWire.K_PLAYERS, "x".repeat(65537)).build();

		assertFalse(VotingPluginWire.readPresenceSnapshot(envelope).valid);
	}

	@Test
	public void chunkedPresenceSnapshotPreservesChunkMetadata() {
		UUID requestId = UUID.randomUUID();
		JsonEnvelope envelope = VotingPluginWire.presenceSnapshot("survival", requestId, 2, 4, List.of());

		VotingPluginWire.PresenceSnapshot snapshot = VotingPluginWire.readPresenceSnapshot(envelope);

		assertTrue(snapshot.valid);
		assertEquals(2, snapshot.chunkIndex);
		assertEquals(4, snapshot.chunkCount);
	}
}
