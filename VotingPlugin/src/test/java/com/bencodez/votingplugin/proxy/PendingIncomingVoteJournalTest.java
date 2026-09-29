package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class PendingIncomingVoteJournalTest {
	@TempDir
	Path temporaryDirectory;

	@Test
	void atomicJournalPreservesStableIdAndPartialEffectFences() throws Exception {
		PendingIncomingVoteJournal journal = new PendingIncomingVoteJournal(temporaryDirectory);
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue vote = new VoteTimeQueue(voteId, "Player", "Service", 1234L);
		vote.setUuid("00000000-0000-0000-0000-000000000001");
		vote.setWasOnline(true);
		vote.setTotals("1,2,3,4,5,6,7,8");
		vote.setVotePartyApplied(true);
		vote.setTotalsApplied(true);
		vote.setDelayValidated(true);
		vote.getBroadcastForwardedServers().add("backend-a");
		vote.setMultiProxyForwardingHandled(true);

		journal.merge(List.of(vote));

		VoteTimeQueue recovered = journal.load().get(0);
		assertEquals(voteId, recovered.getVoteId());
		assertEquals("Player", recovered.getName());
		assertEquals("Service", recovered.getService());
		assertTrue(recovered.isVotePartyApplied());
		assertTrue(recovered.isTotalsApplied());
		assertTrue(recovered.isWasOnlineKnown());
		assertTrue(recovered.isWasOnline());
		assertTrue(recovered.isDelayValidationKnown());
		assertTrue(recovered.isDelayValidated());
		assertTrue(recovered.getBroadcastForwardedServers().contains("backend-a"));
		assertTrue(recovered.isMultiProxyForwardingHandled());

		journal.replace(List.of());
		assertFalse(Files.exists(temporaryDirectory.resolve("pending-incoming-votes-v1.json")));
	}

	@Test
	void mergeDeduplicatesTheSameVoteId() throws Exception {
		PendingIncomingVoteJournal journal = new PendingIncomingVoteJournal(temporaryDirectory);
		UUID voteId = UUID.randomUUID();
		journal.merge(List.of(new VoteTimeQueue(voteId, "Player", "Service", 1L)));
		journal.merge(List.of(new VoteTimeQueue(voteId, "Player", "Service", 1L)));

		assertEquals(1, journal.load().size());
	}
}
