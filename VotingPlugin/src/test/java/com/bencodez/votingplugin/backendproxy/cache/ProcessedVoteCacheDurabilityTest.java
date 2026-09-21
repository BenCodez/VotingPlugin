package com.bencodez.votingplugin.backendproxy.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
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

	@Test
	void repairsTruncatedTailBeforeAppendingAnotherReceipt() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		UUID firstId = UUID.randomUUID();
		ProcessedVoteCache first = new ProcessedVoteCache(receipts);
		assertTrue(first.reserve(firstId));
		assertTrue(first.complete(firstId));
		Files.writeString(receipts, "truncated", StandardOpenOption.APPEND);

		ProcessedVoteCache repaired = new ProcessedVoteCache(receipts);
		UUID secondId = UUID.randomUUID();
		assertTrue(repaired.reserve(secondId));
		assertTrue(repaired.complete(secondId));
		ProcessedVoteCache restarted = new ProcessedVoteCache(receipts);
		assertFalse(restarted.reserve(firstId));
		assertFalse(restarted.reserve(secondId));
	}
}
