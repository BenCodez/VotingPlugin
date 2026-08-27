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

	@Test
	public void redisDeliveryIsAcceptedOnceAcrossSharedHandlers() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		String deliveryId = UUID.randomUUID().toString();
		assertTrue(cache.reserveRedisDelivery(deliveryId));
		assertFalse(cache.reserveRedisDelivery(deliveryId));
		assertTrue(cache.reserveRedisDelivery(UUID.randomUUID().toString()));
		assertTrue(cache.reserveRedisDelivery(null));
	}

	@Test
	public void legacyRedisHandoffMatchesOldAndBufferedDeliveryCounts() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		Object previous = new Object();
		Object replacement = new Object();
		cache.registerRedisSubscriber(previous);
		cache.registerRedisSubscriber(replacement);

		assertTrue(cache.reserveLegacyRedisDelivery(previous, "same-envelope"));
		assertFalse(cache.reserveLegacyRedisDelivery(replacement, "same-envelope"));
		cache.activateRedisSubscriber(replacement);
		assertTrue(cache.consumeLegacyRedisDelivery("same-envelope"));
		assertFalse(cache.consumeLegacyRedisDelivery("same-envelope"));
		cache.finishRedisHandoff();
	}
}
