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
	void retainedVotesLoseLiveFreshnessWithoutMutatingImmediateEnvelopeAndOldRowsAreSanitized() throws Exception {
		for (boolean oldRow : new boolean[] {false, true}) {
			Path file=directory.resolve(oldRow ? "old.dat" : "new.dat"); UUID id=UUID.randomUUID();
			JsonEnvelope live=VotingPluginWire.vote("Player",UUID.randomUUID().toString(),"site",10L,true,true,"",id,false,false,1,1);
			var previous=com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder(live.getSubChannel()).schema(live.getSchema());
			for(var field:live.getFields().entrySet()) previous.put(field.getKey(),field.getValue());
			live=previous.put(VotingPluginWire.K_SESSION_DELIVERY_FRESH,true).build();
			if(oldRow) {
				var encode=java.util.Base64.getUrlEncoder().withoutPadding();
				String server=encode.encodeToString("Survival".getBytes(java.nio.charset.StandardCharsets.UTF_8));
				String envelope=encode.encodeToString(com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.encode(live).getBytes(java.nio.charset.StandardCharsets.UTF_8));
				Files.writeString(file,"VP-VOTE-OUTBOX-2\nA\t"+server+"\t"+envelope+"\n");
			} else assertTrue(new ReliableVoteDeliveryOutbox(file).offer("Survival",live));
			var retained=new ReliableVoteDeliveryOutbox(file).snapshot().getFirst().envelope();
			assertEquals("true",live.getFields().get(VotingPluginWire.K_SESSION_DELIVERY_FRESH));
			assertEquals("false",retained.getFields().get(VotingPluginWire.K_SESSION_DELIVERY_FRESH));
			var expected=new java.util.HashMap<>(live.getFields());expected.put(VotingPluginWire.K_SESSION_DELIVERY_FRESH,"false");
			assertEquals(expected,retained.getFields());assertEquals(live.getSchema(),retained.getSchema());assertEquals(live.getSubChannel(),retained.getSubChannel());
		}
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
}
