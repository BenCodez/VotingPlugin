package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

class ReliableVoteDeliveryOutboxTest {
	@TempDir
	Path directory;

	@Test
	void updateQuotaDoesNotConsumeTheRewardEntryAllowance() throws Exception {
		Path file = directory.resolve("updates.dat");
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		for (int i = 0; i < ReliableVoteDeliveryOutbox.MAX_UPDATE_ENTRIES; i++) {
			JsonEnvelope update = VotingPluginWire.voteUpdate(new UUID(1, i).toString(), 1, 10,
					"synthetic-service", 1L, "").toBuilder()
					.put(VotingPluginWire.K_VOTE_ID, new UUID(2, i)).build();
			assertTrue(outbox.offer("backend-a", update));
		}
		JsonEnvelope extra = VotingPluginWire.voteUpdate(new UUID(1, 2048).toString(), 1, 10,
				"synthetic-service", 1L, "").toBuilder()
				.put(VotingPluginWire.K_VOTE_ID, new UUID(2, 2048)).build();
		assertFalse(outbox.offer("backend-a", extra));
		assertTrue(outbox.offer("backend-a", VotingPluginWire.vote("test-player", new UUID(1, 4096).toString(),
				"synthetic-service", 1L, true, true, "", new UUID(2, 4096), false, false, 1, 1)));
		assertEquals(ReliableVoteDeliveryOutbox.MAX_UPDATE_ENTRIES + 1,
				new ReliableVoteDeliveryOutbox(file).size());
	}

	@Test
	void persistsUntilMatchingBackendAcknowledgesCompletion() throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		ReliableVoteDeliveryOutbox first = new ReliableVoteDeliveryOutbox(file);

		assertTrue(first.offer("Survival", vote));
		assertEquals(1, first.size());
		assertTrue(Files.isRegularFile(file));

		ReliableVoteDeliveryOutbox restarted = new ReliableVoteDeliveryOutbox(file);
		assertEquals(1, restarted.size());
		assertFalse(restarted.acknowledgeCompletion("creative", voteId, VotingPluginWire.SUB_VOTE));
		assertFalse(restarted.acknowledgeCompletion("survival", voteId, VotingPluginWire.SUB_VOTE_ONLINE));
		assertTrue(restarted.acknowledgeCompletion("survival", voteId, VotingPluginWire.SUB_VOTE));
		assertEquals(1, restarted.size());
		assertTrue(new ReliableVoteDeliveryOutbox(file).snapshot().get(0).awaitingReceiptRelease());
		assertTrue(restarted.acknowledgeReceiptRelease("survival", voteId, VotingPluginWire.SUB_VOTE));
		assertEquals(0, restarted.size());
		assertFalse(Files.exists(file));
	}

	@Test
	void duplicateOfferKeepsOneStableDelivery() throws Exception {
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.voteOnline("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));

		assertTrue(outbox.offer("Survival", vote));
		assertTrue(outbox.offer("survival", vote));
		assertEquals(1, outbox.size());
		assertEquals(voteId.toString(), outbox.snapshot().get(0).envelope().getFields()
				.get(VotingPluginWire.K_VOTE_ID));
	}

	@Test
	void delayRejectionKeepsItsStableIdentityAcrossRestart() throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID voteId = UUID.randomUUID();
		JsonEnvelope rejection = VotingPluginWire.voteDelayRejected(
				"Player", UUID.randomUUID().toString(), "site", true, voteId);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);

		assertTrue(outbox.offer("survival", rejection));
		ReliableVoteDeliveryOutbox restarted = new ReliableVoteDeliveryOutbox(file);
		assertEquals(1, restarted.size());
		assertEquals(voteId.toString(), restarted.snapshot().get(0).envelope().getFields()
				.get(VotingPluginWire.K_VOTE_ID));
		assertTrue(restarted.acknowledgeCompletion(
				"survival", voteId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertTrue(restarted.acknowledgeReceiptRelease(
				"survival", voteId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertEquals(0, new ReliableVoteDeliveryOutbox(file).size());
	}

	@Test
	void durablyRetiresFencedAndPreviouslyCompletedLegacyDeliveries() throws Exception {
		Path fencedFile = directory.resolve("fenced.dat");
		UUID fencedId = UUID.randomUUID();
		JsonEnvelope fenced = VotingPluginWire.voteDelayRejected(
				"Player", UUID.randomUUID().toString(), "site", true, fencedId);
		ReliableVoteDeliveryOutbox fencedOutbox = new ReliableVoteDeliveryOutbox(fencedFile);
		assertTrue(fencedOutbox.offer("survival", fenced));
		assertTrue(fencedOutbox.beginLegacyDelivery(
				"survival", fencedId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertTrue(new ReliableVoteDeliveryOutbox(fencedFile).retireLegacyDelivery(
				"survival", fencedId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertEquals(0, new ReliableVoteDeliveryOutbox(fencedFile).size());

		Path completedFile = directory.resolve("completed.dat");
		UUID completedId = UUID.randomUUID();
		JsonEnvelope completed = VotingPluginWire.voteDelayRejected(
				"Player", UUID.randomUUID().toString(), "site", true, completedId);
		ReliableVoteDeliveryOutbox completedOutbox = new ReliableVoteDeliveryOutbox(completedFile);
		assertTrue(completedOutbox.offer("survival", completed));
		assertTrue(completedOutbox.acknowledgeCompletion(
				"survival", completedId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertTrue(new ReliableVoteDeliveryOutbox(completedFile).retireLegacyDelivery(
				"survival", completedId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED));
		assertEquals(0, new ReliableVoteDeliveryOutbox(completedFile).size());
	}

	@Test
	void removalRecordSurvivesRestartWithoutDroppingOtherVotes() throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID firstId = UUID.randomUUID();
		UUID secondId = UUID.randomUUID();
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("One", UUID.randomUUID().toString(),
				"site", 10L, true, true, "", firstId, false, false, 1, 1)));
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("Two", UUID.randomUUID().toString(),
				"site", 11L, true, true, "", secondId, false, false, 1, 1)));
		assertTrue(outbox.acknowledgeCompletion("survival", firstId, VotingPluginWire.SUB_VOTE));
		assertTrue(outbox.acknowledgeReceiptRelease("survival", firstId, VotingPluginWire.SUB_VOTE));

		ReliableVoteDeliveryOutbox restarted = new ReliableVoteDeliveryOutbox(file);
		assertEquals(1, restarted.size());
		assertEquals(secondId.toString(), restarted.snapshot().get(0).envelope().getFields()
				.get(VotingPluginWire.K_VOTE_ID));
	}

	@Test
	void restartIgnoresOnlyATruncatedFinalAppend() throws Exception {
		Path file = directory.resolve("outbox.dat");
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("One", UUID.randomUUID().toString(),
				"site", 10L, true, true, "", UUID.randomUUID(), false, false, 1, 1)));
		Files.writeString(file, "R\tc3Vydml2YWw", StandardOpenOption.APPEND);

		ReliableVoteDeliveryOutbox repaired = new ReliableVoteDeliveryOutbox(file);
		assertEquals(1, repaired.size());
		assertTrue(repaired.offer("survival", VotingPluginWire.vote("Two", UUID.randomUUID().toString(),
				"site", 11L, true, true, "", UUID.randomUUID(), false, false, 1, 1)));
		assertEquals(2, new ReliableVoteDeliveryOutbox(file).size());
	}

	@Test
	void repairsPartialAppendBeforeSameProcessRetry() throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID voteId = UUID.randomUUID();
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("One", UUID.randomUUID().toString(),
				"site", 10L, true, true, "", voteId, false, false, 1, 1)));
		Files.writeString(file, "C\tpartial", StandardOpenOption.APPEND);

		assertTrue(outbox.acknowledgeCompletion("survival", voteId, VotingPluginWire.SUB_VOTE));
		ReliableVoteDeliveryOutbox restarted = new ReliableVoteDeliveryOutbox(file);
		assertEquals(1, restarted.size());
		assertTrue(restarted.snapshot().get(0).awaitingReceiptRelease());
	}

	@Test
	void acceptedVotesReserveCapacityForCompletionAndRemovalRecords() throws Exception {
		Path file = directory.resolve("outbox.dat");
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		List<UUID> accepted = new ArrayList<>();
		String padding = "x".repeat(16 * 1024);

		while (true) {
			UUID voteId = UUID.randomUUID();
			JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
					true, true, "", voteId, false, false, 1, 1).toBuilder().put("padding", padding).build();
			if (!outbox.offer("survival", vote)) break;
			accepted.add(voteId);
		}

		assertFalse(accepted.isEmpty());
		for (UUID voteId : accepted) {
			assertTrue(outbox.acknowledgeCompletion("survival", voteId, VotingPluginWire.SUB_VOTE));
		}
		for (UUID voteId : accepted) {
			assertTrue(outbox.acknowledgeReceiptRelease("survival", voteId, VotingPluginWire.SUB_VOTE));
		}
		assertEquals(0, outbox.size());
		assertFalse(Files.exists(file));
	}

    @Test
    void deferredOwnershipSurvivesRestartBeforeTheReadyWindowAndCannotBeSentEarly() throws Exception {
        Path file = directory.resolve("deferred.dat");
        UUID id = new UUID(7, 1);
        JsonEnvelope update = update(id);
        ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
        assertTrue(outbox.retainUpdate("backend-a", update, false));
        assertTrue(outbox.snapshot().get(0).deferredUpdate());
        assertFalse(outbox.beginLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
        ReliableVoteDeliveryOutbox restored = new ReliableVoteDeliveryOutbox(file);
        assertTrue(restored.snapshot().get(0).deferredUpdate());
        assertEquals(id.toString(), restored.snapshot().get(0).envelope().getFields().get(VotingPluginWire.K_VOTE_ID));
        assertTrue(restored.setUpdateReady("backend-a", id, true));
        restored = new ReliableVoteDeliveryOutbox(file);
        assertFalse(restored.snapshot().get(0).deferredUpdate());
        assertTrue(restored.beginLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
        assertTrue(restored.retireLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
        assertEquals(0, new ReliableVoteDeliveryOutbox(file).size());
    }

    @Test
    void rejectedReadyAttemptCanReturnToDeferredOwnershipAndRecoverWithoutAnAmbiguousFence() throws Exception {
        Path file = directory.resolve("rejected-update.dat");
        UUID id = new UUID(7, 2);
        ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
        assertTrue(outbox.offer("backend-a", update(id)));
        assertTrue(outbox.beginLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
        assertFalse(outbox.setUpdateReady("backend-a", id, false));
        assertTrue(outbox.rejectLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
        assertTrue(outbox.setUpdateReady("backend-a", id, false));
        ReliableVoteDeliveryOutbox restored = new ReliableVoteDeliveryOutbox(file);
        assertTrue(restored.snapshot().get(0).deferredUpdate());
        assertFalse(restored.snapshot().get(0).legacyDeliveryFenced());
        assertFalse(restored.snapshot().get(0).legacyDeliveryRejected());
        assertTrue(restored.setUpdateReady("backend-a", id, true));
        assertTrue(restored.beginLegacyDelivery("backend-a", id, VotingPluginWire.SUB_VOTE_UPDATE));
    }

    @Test
    void deferredUpdatesRetainOrderAndDoNotAdmitARewardThroughTheDeferredApi() throws Exception {
        ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("ordered.dat"));
        UUID first = new UUID(7, 3), second = new UUID(7, 4);
        assertTrue(outbox.retainUpdate("backend-a", update(first), false));
        assertTrue(outbox.retainUpdate("backend-a", update(second), true));
        assertTrue(outbox.snapshot().get(1).deferredUpdate(), "new update bypassed an older deferred owner");
        assertFalse(outbox.isFirstPendingUpdate(null, first));
        assertFalse(outbox.isFirstPendingUpdate("backend-a", null));
        assertTrue(outbox.isFirstPendingUpdate("backend-a", first));
        assertFalse(outbox.isFirstPendingUpdate("backend-a", second));
        assertFalse(outbox.retainUpdate("backend-a", VotingPluginWire.vote("test-player",
                new UUID(1, 1).toString(), "synthetic-service", 1L, true, true, "", second,
                false, false, 1, 1), false));
    }

    @Test
    void failedDeferredPublicationDoesNotReportDurableAdmission() throws Exception {
        Path blocked = directory.resolve("blocked");
        Files.writeString(blocked, "synthetic");
        ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(blocked.resolve("updates.dat"));
        assertFalse(outbox.retainUpdate("backend-a", update(new UUID(7, 5)), false));
        assertEquals(0, outbox.size());
    }

    @Test
    void deferredCapacityIsBoundedAndDoesNotEvictExistingOwnersOrRewardAllowance() throws Exception {
        Path file = directory.resolve("deferred-capacity.dat");
        ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
        for (int i = 0; i < ReliableVoteDeliveryOutbox.MAX_DEFERRED_UPDATE_ENTRIES; i++) {
            assertTrue(outbox.retainUpdate("backend-a", update(new UUID(8, i)), false));
        }
        UUID overflow = new UUID(8, ReliableVoteDeliveryOutbox.MAX_DEFERRED_UPDATE_ENTRIES);
        assertFalse(outbox.retainUpdate("backend-a", update(overflow), false));
        assertTrue(outbox.retainUpdate("backend-a", update(new UUID(8, 0)), false));
        assertTrue(outbox.offer("backend-b", VotingPluginWire.vote("test-player",
                new UUID(1, 1).toString(), "synthetic-service", 1L, true, true, "", overflow,
                false, false, 1, 1)));
        ReliableVoteDeliveryOutbox restored = new ReliableVoteDeliveryOutbox(file);
        assertEquals(ReliableVoteDeliveryOutbox.MAX_DEFERRED_UPDATE_ENTRIES + 1, restored.size());
        assertEquals(ReliableVoteDeliveryOutbox.MAX_DEFERRED_UPDATE_ENTRIES,
                restored.snapshot().stream().filter(ReliableVoteDeliveryOutbox.Entry::deferredUpdate).count());
        assertFalse(restored.find("backend-a", overflow, VotingPluginWire.SUB_VOTE_UPDATE).isPresent());
    }

    private static JsonEnvelope update(UUID id) {
        return VotingPluginWire.voteUpdate(new UUID(1, 1).toString(), 1, 10,
                "synthetic-service", 1L, "").toBuilder().put(VotingPluginWire.K_VOTE_ID, id).build();
    }
}
