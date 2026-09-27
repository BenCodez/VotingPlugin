package com.bencodez.votingplugin.backendproxy.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ProcessedVoteCacheDurabilityTest {

	@Test
	void duplicateRetryDoesNotScanAndPruneUnrelatedEntries() {
		ProcessedVoteCache cache = new ProcessedVoteCache(TimeUnit.MINUTES.toMillis(30), 2);
		UUID expired = UUID.randomUUID();
		UUID live = UUID.randomUUID();
		cache.getProcessedVotes().put(expired, 0L);
		cache.getProcessedVotes().put(live, Long.MAX_VALUE);

		assertTrue(cache.reserveWithOutcome(live) == ProcessedVoteCache.Reservation.DUPLICATE);
		assertTrue(cache.getProcessedVotes().containsKey(expired));
		assertTrue(cache.reserveWithOutcome(UUID.randomUUID()) == ProcessedVoteCache.Reservation.RESERVED);
	}
	@TempDir
	Path directory;

	@Test
	void releaseTombstoneCapacityCoversSustainedVoteThroughputForFullTtl() {
		long threeVotesPerSecondForOneDay = TimeUnit.DAYS.toSeconds(1) * 3;

		assertTrue(DurableVoteReceiptStore.MAX_RELEASE_TOMBSTONES >= threeVotesPerSecondForOneDay);
	}

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
	void repairsPartialAppendBeforeSameProcessRetry() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		UUID firstId = UUID.randomUUID();
		UUID secondId = UUID.randomUUID();
		ProcessedVoteCache cache = new ProcessedVoteCache(receipts);
		assertTrue(cache.reserve(firstId));
		assertTrue(cache.complete(firstId));
		Files.writeString(receipts, "partial", StandardOpenOption.APPEND);

		assertTrue(cache.reserve(secondId));
		assertTrue(cache.complete(secondId));
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
	void activeReceiptWaitsWhenTombstoneCapacityIsFull() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		DurableVoteReceiptStore store = new DurableVoteReceiptStore(receipts, 1, 0, 1);
		UUID active = UUID.randomUUID();
		assertTrue(store.complete(active) > 0L);
		assertTrue(store.release(UUID.randomUUID()) > 0L);

		assertFalse(store.release(active) > 0L);

		new DurableVoteReceiptStore(receipts, 1, 0, 1);
	}

	@Test
	void expiredReleaseTombstoneIsReclaimedOnRestart() throws Exception {
		Path receipts = directory.resolve("receipts.dat");
		UUID voteId = UUID.randomUUID();
		Files.writeString(receipts, "VP-VOTE-RECEIPTS-1\nR\t" + voteId + "\t1\n");

		assertTrue(new ProcessedVoteCache(receipts).reserve(voteId));
	}

	@Test
	void saturationDoesNotEvictLiveReplayFences() {
		ProcessedVoteCache cache = new ProcessedVoteCache(TimeUnit.MINUTES.toMillis(30), 2);
		UUID first = UUID.randomUUID();
		UUID second = UUID.randomUUID();

		assertTrue(cache.reserve(first));
		assertTrue(cache.reserve(second));
		assertTrue(cache.reserveWithOutcome(UUID.randomUUID()) == ProcessedVoteCache.Reservation.SATURATED);
		assertFalse(cache.reserve(first));
		assertFalse(cache.reserve(second));
		assertTrue(cache.getProcessedVotes().size() == 2);
	}

	@Test
	void defaultCapacityIncludesDurableCompletionHeadroom() {
		assertTrue(ProcessedVoteCache.DEFAULT_MAX_TRACKED_VOTES
				== DurableVoteReceiptStore.MAX_ACTIVE_RECEIPTS + DurableVoteReceiptStore.COMPLETION_HEADROOM);
		assertTrue(ProcessedVoteCache.DEFAULT_MAX_TRACKED_VOTES <= DurableVoteReceiptStore.MAX_TOTAL_RECEIPTS);
		assertTrue((long) DurableVoteReceiptStore.MAX_TOTAL_RECEIPTS * DurableVoteReceiptStore.MAX_RECORD_BYTES
				+ "VP-VOTE-RECEIPTS-1\n".length() <= DurableVoteReceiptStore.MAX_FILE_BYTES);
		assertTrue((long) (DurableVoteReceiptStore.MAX_TOTAL_RECEIPTS + 1)
				* DurableVoteReceiptStore.MAX_RECORD_BYTES + "VP-VOTE-RECEIPTS-1\n".length()
				> DurableVoteReceiptStore.MAX_FILE_BYTES);
	}

	@Test
	void mixedActiveAndReleaseReceiptsRespectTheJournalByteAlignedLimit() throws Exception {
		DurableVoteReceiptStore store = new DurableVoteReceiptStore(directory.resolve("mixed-receipts.dat"), 2, 0, 2, 2);
		UUID released = UUID.randomUUID();
		UUID active = UUID.randomUUID();

		assertTrue(store.release(released) > 0L);
		assertTrue(store.complete(active) > 0L);
		assertFalse(store.complete(UUID.randomUUID()) > 0L);
		assertFalse(store.release(UUID.randomUUID()) > 0L);
	}

	@Test
	void upgradeLoadsMixedJournalThatExceedsTheNewAdmissionCount() throws Exception {
		Path receipts = directory.resolve("upgrade-mixed-receipts.dat");
		UUID firstActive = UUID.randomUUID();
		UUID secondActive = UUID.randomUUID();
		UUID firstReleased = UUID.randomUUID();
		UUID secondReleased = UUID.randomUUID();
		long expiresAt = System.currentTimeMillis() + TimeUnit.HOURS.toMillis(1);
		Files.writeString(receipts, "VP-VOTE-RECEIPTS-1\n"
				+ firstActive + "\t" + Long.MAX_VALUE + "\n"
				+ secondActive + "\t" + Long.MAX_VALUE + "\n"
				+ "R\t" + firstReleased + "\t" + expiresAt + "\n"
				+ "R\t" + secondReleased + "\t" + expiresAt + "\n");

		DurableVoteReceiptStore store = new DurableVoteReceiptStore(receipts, 2, 0, 2, 2);

		assertTrue(store.contains(firstActive));
		assertTrue(store.contains(secondActive));
		assertTrue(store.contains(firstReleased));
		assertTrue(store.contains(secondReleased));
		assertFalse(store.complete(UUID.randomUUID()) > 0L);
	}

	@Test
	void cancelledValidationReservationFreesCapacity() {
		ProcessedVoteCache cache = new ProcessedVoteCache(TimeUnit.MINUTES.toMillis(30), 1);
		UUID invalid = UUID.randomUUID();

		assertTrue(cache.reserve(invalid));
		cache.cancelReservation(invalid);

		assertTrue(cache.reserve(UUID.randomUUID()));
	}
}
