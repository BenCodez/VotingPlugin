package com.bencodez.votingplugin.listeners;
import static org.junit.jupiter.api.Assertions.*;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
class SessionObservationMetadataTest {
    @Test void acceptedAdapterForwardsLocalOrderAndProxyClassificationWithoutChangingOccurrenceTime() {
        var input = new PlayerVoteEvent(null, "Alex", "a", true);
        input.setBungee(true); input.setProxyQueueClassificationKnown(true); input.setQueuedProxyVote(true);
        var post = new PlayerPostVoteEvent(null, null, true, false, 1, false, "a", UUID.randomUUID(), "Alex", UUID.randomUUID());
        PlayerVoteListener.copySessionObservation(post, input);
        assertEquals(input.getBackendObservationOrder(), post.getBackendObservationOrder());
        assertTrue(post.isProxySessionDelivery()); assertFalse(post.isBungee()); assertTrue(post.isProxyQueueClassificationKnown()); assertTrue(post.isQueuedProxyVote());
        assertEquals(1, post.getVoteTime());
    }
}
