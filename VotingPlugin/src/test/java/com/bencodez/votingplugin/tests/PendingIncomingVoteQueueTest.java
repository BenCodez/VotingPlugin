package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteQueue;

class PendingIncomingVoteQueueTest {
	@Test
	void ownsMultipleVotesIndependentlyUntilEachCompletes() {
		PendingIncomingVoteQueue queue = new PendingIncomingVoteQueue();
		PendingIncomingVote first = queue.admit("First", "Site");
		PendingIncomingVote second = queue.admit("Second", "Site");

		assertNotEquals(first.getVoteId(), second.getVoteId());
		assertEquals(2, queue.size());
		assertTrue(first.beginProcessing());
		assertFalse(first.beginProcessing());
		first.endProcessing();

		queue.complete(first);
		assertFalse(queue.contains(first.getVoteId()));
		assertTrue(queue.contains(second.getVoteId()));
		assertEquals(1, queue.size());
	}

	@Test
	void admissionIsBoundedSoOverflowCanMoveToDurableStorage() {
		PendingIncomingVoteQueue queue = new PendingIncomingVoteQueue();
		for (int i = 0; i < 4096; i++) {
			assertTrue(queue.admit("Player" + i, "Site") != null);
		}
		assertTrue(queue.admit("Overflow", "Site") == null);
		assertEquals(4096, queue.size());
	}
}
