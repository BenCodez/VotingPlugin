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
		assertTrue(first.hasDurableReceipt(voteId));
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

	@Test
	void discardsParseableUnterminatedReceiptTail() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		UUID incompleteId = UUID.randomUUID();
		Files.writeString(receipts, "VP-VOTE-RECEIPTS-1\n" + incompleteId + "\t9");

		ProcessedVoteCache repaired = new ProcessedVoteCache(receipts);

		assertTrue(repaired.reserve(incompleteId));
		assertTrue(Files.readString(receipts).endsWith("\n"));
	}

	@Test
	void receiptFailureFencesCompletedVotePastReservationExpiry() throws Exception {
		Path unusableParent = directory.resolve("not-a-directory");
		Files.writeString(unusableParent, "file");
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = new ProcessedVoteCache(1L, unusableParent.resolve("receipts.dat"));

		assertTrue(cache.reserve(voteId));
		assertFalse(cache.complete(voteId));
		Thread.sleep(5L);
		assertFalse(cache.reserve(voteId));
	}

	@Test
	void proxyConfirmedReleaseLeavesRestartSafeTombstone() {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = new ProcessedVoteCache(receipts);
		assertTrue(cache.reserve(voteId));
		assertTrue(cache.complete(voteId));

		assertTrue(cache.releaseCompletedReceipt(voteId));

		assertFalse(new ProcessedVoteCache(receipts).reserve(voteId));
	}

	@Test
	void unknownReceiptReleaseLeavesRestartSafeTombstone() {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = new ProcessedVoteCache(receipts);

		assertFalse(cache.hasDurableReceipt(voteId));
		assertTrue(cache.releaseCompletedReceipt(voteId));
		assertTrue(cache.hasDurableReceipt(voteId));

		assertFalse(new ProcessedVoteCache(receipts).reserve(voteId));
	}

	@Test
	void completionHeadroomAllowsPrecedingVotesBeforeRelease() throws Exception {
		DurableVoteReceiptStore store = new DurableVoteReceiptStore(directory.resolve("receipts.dat"), 1, 2, 1);
		UUID first = UUID.randomUUID();
		UUID second = UUID.randomUUID();
		UUID third = UUID.randomUUID();
		UUID fourth = UUID.randomUUID();

		assertTrue(store.complete(first) > 0L);
		assertTrue(store.complete(second) > 0L);
		assertTrue(store.complete(third) > 0L);
		assertFalse(store.complete(fourth) > 0L);
		assertTrue(store.release(first) > 0L);
		assertTrue(store.complete(fourth) > 0L);
	}

	@Test
	void expiredReleaseTombstoneIsReclaimedOnRestart() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		Files.writeString(receipts, "VP-VOTE-RECEIPTS-1\nR\t" + voteId + "\t1\n");

		assertTrue(new ProcessedVoteCache(receipts).reserve(voteId));
	}
}
