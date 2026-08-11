package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

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
		assertEquals(VotingPluginWire.SUB_VOTE, envelope.getSubChannel());
		assertEquals("Player", vote.player);
		assertEquals("Service", vote.service);
	}

	@Test
	public void voteOnlineRoundTripAllowsMissingVoteId() {
		JsonEnvelope envelope = VotingPluginWire.voteOnline("Player", UUID.randomUUID().toString(), "Service",
				100L, true, true, "totals", null, true, false, 1, 1);

		Vote vote = VotingPluginWire.readVote(envelope);

		assertNull(vote.voteId);
		assertEquals(VotingPluginWire.SUB_VOTE_ONLINE, envelope.getSubChannel());
	}

	@Test
	public void voteOnlineRoundTripPreservesIdentifiedQueuedVote() {
		UUID voteId = UUID.randomUUID();

		JsonEnvelope envelope = VotingPluginWire.voteOnline("Player", UUID.randomUUID().toString(), "Service",
				100L, true, true, "totals", voteId, true, false, 1, 1);

		Vote vote = VotingPluginWire.readVote(envelope);

		assertEquals(VotingPluginWire.SUB_VOTE_ONLINE, envelope.getSubChannel());
		assertEquals(voteId, vote.voteId);
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
}
