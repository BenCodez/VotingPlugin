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
	void boundaryProtocolCapabilityMustMatchTheCurrentBackendHeartbeat() {
		assertTrue(VotingPluginWire.supportsTimeChangeBoundaryProtocol("200",
				VotingPluginWire.timeChangeBoundaryProtocolHeartbeat("200")));
		assertFalse(VotingPluginWire.supportsTimeChangeBoundaryProtocol("201",
				VotingPluginWire.timeChangeBoundaryProtocolHeartbeat("200")));
		assertFalse(VotingPluginWire.supportsTimeChangeBoundaryProtocol("200", ""));
	}

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
	public void legacyTotalsSupplyTheVoteIdWhenTheTopLevelFieldIsMissing() {
		UUID voteId = UUID.randomUUID();
		String legacyTotals = "1//2//3//4//5//0//6//7//8//" + voteId;
		JsonEnvelope envelope = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "Service", 100L,
				true, true, legacyTotals, null, true, false, 1, 1);

		assertEquals(voteId, VotingPluginWire.resolveVoteId(VotingPluginWire.readVote(envelope)));
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
	public void presenceEventsPreserveBackendGenerationAndCaptureTime() {
		UUID connectionId = UUID.randomUUID();
		UUID backendIncarnationId = UUID.randomUUID();
		VotingPluginWire.PlayerPresenceEvent login = VotingPluginWire.readPlayerPresenceEvent(VotingPluginWire.login(
				"Player", UUID.randomUUID().toString(), "survival", connectionId, backendIncarnationId, 1000L,
				1100L));
		JsonEnvelope heartbeat = VotingPluginWire.backendHeartbeat("survival", backendIncarnationId, 1000L, 1200L);

		assertEquals(backendIncarnationId, login.backendIncarnationId);
		assertEquals(1000L, login.backendStartedAt);
		assertEquals(1100L, login.presenceTimestamp);
		assertEquals(backendIncarnationId, VotingPluginWire.readBackendIncarnationId(heartbeat));
		assertEquals(1000L, VotingPluginWire.readBackendStartedAt(heartbeat));
		assertEquals(1200L, VotingPluginWire.readPresenceTimestamp(heartbeat));
	}

	@Test
	public void presenceResyncRequestPreservesTargetAndRequestTime() {
		UUID requestId = UUID.randomUUID();
		JsonEnvelope envelope = VotingPluginWire.presenceResyncRequest("survival", requestId, 1200L);

		VotingPluginWire.PresenceResyncRequest request = VotingPluginWire.readPresenceResyncRequest(envelope);

		assertEquals(VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST, envelope.getSubChannel());
		assertEquals("survival", request.server);
		assertEquals(requestId, request.requestId);
		assertEquals(1200L, request.requestedAt);
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
		UUID backendIncarnationId = UUID.randomUUID();
		JsonEnvelope envelope = VotingPluginWire.presenceSnapshot("survival", requestId, 2, 4, List.of(),
				backendIncarnationId, 1000L, 1300L);

		VotingPluginWire.PresenceSnapshot snapshot = VotingPluginWire.readPresenceSnapshot(envelope);

		assertTrue(snapshot.valid);
		assertEquals(2, snapshot.chunkIndex);
		assertEquals(4, snapshot.chunkCount);
		assertEquals(backendIncarnationId, snapshot.backendIncarnationId);
		assertEquals(1000L, snapshot.backendStartedAt);
		assertEquals(1300L, snapshot.presenceTimestamp);
	}

	@Test
	public void controlEnrollmentRoundTripContainsVerifierIdentityAndRoute() {
		UUID requestId = UUID.randomUUID();
		String verifier = "a".repeat(64);
		String challenge = UUID.randomUUID().toString();

		VotingPluginWire.ControlEnrollmentRequest request = VotingPluginWire.readControlEnrollmentRequest(
				VotingPluginWire.controlEnrollmentRequest("survival", verifier, "http://10.0.0.5:2150", requestId,
						challenge));
		VotingPluginWire.ControlEnrollmentResult result = VotingPluginWire.readControlEnrollmentResult(
				VotingPluginWire.controlEnrollmentResult("survival", requestId, false, challenge));

		assertTrue(request.valid);
		assertEquals("survival", request.nodeId);
		assertEquals("survival", VotingPluginWire.controlEnrollmentRequest(
				"survival", verifier, "http://10.0.0.5:2150", requestId)
				.getFields().get(VotingPluginWire.K_SERVER));
		assertEquals(verifier, request.verifier);
		assertEquals("http://10.0.0.5:2150", request.endpoint);
		assertEquals(challenge, request.challenge);
		assertEquals(requestId, request.requestId);
		assertTrue(result.valid);
		assertFalse(result.success);
		assertEquals(challenge, result.challenge);
		assertEquals(requestId, result.requestId);
		assertEquals("", result.authenticator);
	}

	@Test
	public void blankVerifierIsAValidHostedRoutePreflight() {
		VotingPluginWire.ControlEnrollmentRequest request = VotingPluginWire.readControlEnrollmentRequest(
				VotingPluginWire.controlEnrollmentRequest("survival", "", "http://10.0.0.5:2150",
						UUID.randomUUID()));

		assertTrue(request.valid);
		assertEquals("", request.verifier);
	}

	@Test
	public void statusRoundTripCarriesCorrelationId() {
		UUID requestId = UUID.randomUUID();

		JsonEnvelope request = VotingPluginWire.status("survival", requestId);
		JsonEnvelope response = VotingPluginWire.statusOkay("survival", requestId);

		assertEquals(VotingPluginWire.SUB_STATUS, request.getSubChannel());
		assertEquals("survival", request.getFields().get(VotingPluginWire.K_SERVER));
		assertEquals(requestId.toString(), request.getFields().get(VotingPluginWire.K_REQUEST_ID));
		assertEquals(VotingPluginWire.SUB_STATUS_OKAY, response.getSubChannel());
		assertEquals(requestId.toString(), response.getFields().get(VotingPluginWire.K_REQUEST_ID));
		assertTrue(VotingPluginWire.advertisesVoteDeliveryAcknowledgement(response));
	}

	@Test
	public void voteDeliveryAcknowledgementIsAdditiveAndCorrelated() {
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		JsonEnvelope requested = VotingPluginWire.requestVoteDeliveryAcknowledgement(vote);

		assertFalse(VotingPluginWire.requestsVoteDeliveryAcknowledgement(vote));
		assertTrue(VotingPluginWire.requestsVoteDeliveryAcknowledgement(requested));
		assertEquals(voteId.toString(), requested.getFields().get(VotingPluginWire.K_VOTE_ID));

		JsonEnvelope acknowledgement = VotingPluginWire.voteDeliveryAcknowledgement(
				"survival", voteId, VotingPluginWire.SUB_VOTE);
		assertEquals("survival", acknowledgement.getFields().get(VotingPluginWire.K_SERVER));
		assertEquals(voteId.toString(), acknowledgement.getFields().get(VotingPluginWire.K_VOTE_ID));
		assertEquals(VotingPluginWire.SUB_VOTE,
				acknowledgement.getFields().get(VotingPluginWire.K_VOTE_DELIVERY_SUBCHANNEL));

		JsonEnvelope release = VotingPluginWire.voteDeliveryReceiptRelease(
				"survival", voteId, VotingPluginWire.SUB_VOTE);
		JsonEnvelope releaseAck = VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(
				"survival", voteId, VotingPluginWire.SUB_VOTE);
		assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE, release.getSubChannel());
		assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK, releaseAck.getSubChannel());
		assertTrue(VotingPluginWire.requestsVoteDeliveryAcknowledgement(release));
	}

	@Test
	public void malformedControlEnrollmentIsRejected() {
		JsonEnvelope malformed = JsonEnvelope.builder(VotingPluginWire.SUB_CONTROL_ENROLLMENT_REQUEST)
				.schema(VotingPluginWire.SCHEMA_VERSION).put(VotingPluginWire.K_NODE_ID, "../proxy")
				.put(VotingPluginWire.K_VERIFIER, "not-a-verifier")
				.put(VotingPluginWire.K_REQUEST_ID, "not-a-uuid").build();

		assertFalse(VotingPluginWire.readControlEnrollmentRequest(malformed).valid);
	}
}
