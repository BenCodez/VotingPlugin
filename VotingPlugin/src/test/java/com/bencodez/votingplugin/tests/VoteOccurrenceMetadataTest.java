package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.VoteOccurrenceMetadata;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class VoteOccurrenceMetadataTest {
    @SuppressWarnings("deprecation")
    @Test void metadataPreservesLegacyAndVersionedTotalsAndCorrelation() {
        UUID correlation = UUID.randomUUID();
        for (String payload : new String[] {
                "1//2//3//4//5//999//6//7//8//" + correlation,
                "v2//1//2//3//4//5//6//7//8//" + correlation }) {
            String decorated = VoteOccurrenceMetadata.store(payload, 123L);
            var original = VoteTotalsSnapshot.parseStorage(payload);
            var restored = VoteTotalsSnapshot.parseStorage(decorated);
            assertEquals(original.toStorageString(), restored.toStorageString());
            assertEquals(correlation, restored.getVoteUUID());
            assertEquals(123L, VoteOccurrenceMetadata.read(decorated));
        }
    }

    @Test void cachedTotalsUpdatesAndCopiesKeepOccurrenceSeparateFromCooldownTime() {
        var offline = new OfflineBungeeVote(UUID.randomUUID(), "Player", "uuid", "Service", 456L, true, "");
        offline.setCanonicalOccurrenceTime(123L);
        offline.setText("v2//1//2//3//4//5//6//7//8");
        var copied = new OfflineBungeeVote(offline.getVoteId(), offline.getPlayerName(), offline.getUuid(),
                offline.getService(), offline.getTime(), offline.isRealVote(), offline.getText());
        assertEquals(456L, copied.getTime());
        assertEquals(123L, copied.getCanonicalOccurrenceTime());
        var timed = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 789L);
        timed.setCanonicalOccurrenceTime(123L);
        timed.setTotals("v2//8//7//6//5//4//3//2//1");
        assertEquals(789L, timed.getTime());
        assertEquals(123L, timed.getCanonicalOccurrenceTime());
        assertEquals(8, VoteTotalsSnapshot.parseStorage(timed.getTotals()).getAllTimeTotal());
    }

    @Test void malformedOversizedAndConflictingMetadataCannotBecomeDateProvenance() {
        for (String token : new String[] { "", "0", "-1", "bad", "9".repeat(100), "123//vp-occurrence-time:456" }) {
            String payload = "v2//1//2//3//4//5//6//7//8//vp-occurrence-time:" + token;
            assertEquals(-1L, VoteOccurrenceMetadata.read(payload));
            var envelope = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "Service", 789L,
                    true, true, payload, UUID.randomUUID(), false, false, 1, 1);
            assertEquals(-1L, VotingPluginWire.readVote(envelope).canonicalOccurrenceTime);
            assertEquals(789L, VotingPluginWire.readVote(envelope).time);
            assertEquals(1, VoteTotalsSnapshot.parseStorage(payload).getAllTimeTotal());
        }
        var envelope = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "Service", 789L,
                true, true, VoteOccurrenceMetadata.store("", 123L), UUID.randomUUID(), false, false, 1, 1);
        var conflicting = envelope.toBuilder().put(VotingPluginWire.K_CANONICAL_OCCURRENCE_TIME, 456L).build();
        assertEquals(-1L, VotingPluginWire.readVote(conflicting).canonicalOccurrenceTime);
        assertEquals(789L, VotingPluginWire.readVote(conflicting).time);
    }
}
