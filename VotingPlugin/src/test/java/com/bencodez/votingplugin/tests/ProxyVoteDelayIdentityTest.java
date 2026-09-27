package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;
import com.bencodez.votingplugin.listeners.ProxyVoteDelayCheck;

class ProxyVoteDelayIdentityTest {

	@Test
	void acceptsOnlyAnExplicitQueuedProxyOccurrenceWithAStableId() {
		UUID voteId = UUID.randomUUID();
		assertTrue(SharedVoteProcessor.isIdentifiedQueuedProxyVote(true, true, voteId));
		assertFalse(SharedVoteProcessor.isIdentifiedQueuedProxyVote(false, true, voteId));
		assertFalse(SharedVoteProcessor.isIdentifiedQueuedProxyVote(true, false, voteId));
		assertFalse(SharedVoteProcessor.isIdentifiedQueuedProxyVote(true, true, null));
	}

	@SuppressWarnings("deprecation")
	@Test
	void preservesLegacyTimestampHelpersForBinaryCompatibility() {
		assertTrue(SharedVoteProcessor.isQueuedVoteAlreadyRecorded(true, 123L, 123L));
		assertTrue(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(true, 123L, 123L));
		assertFalse(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(false, 123L, 123L));
		assertFalse(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(true, 0L, 0L));
	}
}
