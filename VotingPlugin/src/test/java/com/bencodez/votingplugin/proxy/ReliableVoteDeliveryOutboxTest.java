package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

class ReliableVoteDeliveryOutboxTest {
	@TempDir
	Path directory;

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
		assertFalse(restarted.acknowledge("creative", voteId, VotingPluginWire.SUB_VOTE));
		assertFalse(restarted.acknowledge("survival", voteId, VotingPluginWire.SUB_VOTE_ONLINE));
		assertTrue(restarted.acknowledge("survival", voteId, VotingPluginWire.SUB_VOTE));
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
	void removalRecordSurvivesRestartWithoutDroppingOtherVotes() throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID firstId = UUID.randomUUID();
		UUID secondId = UUID.randomUUID();
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("One", UUID.randomUUID().toString(),
				"site", 10L, true, true, "", firstId, false, false, 1, 1)));
		assertTrue(outbox.offer("survival", VotingPluginWire.vote("Two", UUID.randomUUID().toString(),
				"site", 11L, true, true, "", secondId, false, false, 1, 1)));
		assertTrue(outbox.acknowledge("survival", firstId, VotingPluginWire.SUB_VOTE));

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
		Files.writeString(file, "A\ttruncated", StandardOpenOption.APPEND);

		assertEquals(1, new ReliableVoteDeliveryOutbox(file).size());
	}
}
