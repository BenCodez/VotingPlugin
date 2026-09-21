package com.bencodez.votingplugin.backendproxy.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ProcessedVoteCacheDurabilityTest {
	@TempDir
	Path directory;

	@Test
	void completedVoteRemainsDeduplicatedAfterBackendRestart() {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache first = new ProcessedVoteCache(receipts);

		assertTrue(first.reserve(voteId));
		assertTrue(first.complete(voteId));
		assertFalse(new ProcessedVoteCache(receipts).reserve(voteId));
	}

	@Test
	void reservationWithoutCompletionDoesNotSuppressRestartRetry() {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		assertTrue(new ProcessedVoteCache(receipts).reserve(voteId));

		assertTrue(new ProcessedVoteCache(receipts).reserve(voteId));
	}
}
