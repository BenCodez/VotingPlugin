package com.bencodez.votingplugin.tests.backendproxy.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;

public class ProcessedVoteCacheTest {

	@Test
	public void duplicateVoteIdIsRejected() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		UUID voteId = UUID.randomUUID();
		assertTrue(cache.reserve(voteId));
		assertFalse(cache.reserve(voteId));
		assertEquals(1, cache.getProcessedVotes().size());
	}

	@Test
	public void nullVoteIdsRemainCompatible() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		assertTrue(cache.reserve(null));
		assertTrue(cache.reserve(null));
		assertTrue(cache.getProcessedVotes().isEmpty());
	}

	@Test
	public void expiredVoteIdCanBeReservedAgain() throws Exception {
		ProcessedVoteCache cache = new ProcessedVoteCache(1);
		UUID voteId = UUID.randomUUID();
		assertTrue(cache.reserve(voteId));
		Thread.sleep(5);
		assertTrue(cache.reserve(voteId));
		assertEquals(1, cache.getProcessedVotes().size());
	}
}
