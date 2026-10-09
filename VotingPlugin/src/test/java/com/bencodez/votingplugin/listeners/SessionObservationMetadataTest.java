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
        input.setUnconfirmedProxySessionDelivery(true);
        var post = new PlayerPostVoteEvent(null, null, true, false, 1, false, "a", UUID.randomUUID(), "Alex", UUID.randomUUID());
        PlayerVoteListener.copySessionObservation(post, input);
        assertEquals(input.getBackendObservationOrder(), post.getBackendObservationOrder());
        assertFalse(post.isLiveLocalSessionDelivery()); assertTrue(post.isProxySessionDelivery()); assertFalse(post.isBungee()); assertTrue(post.isProxyQueueClassificationKnown()); assertTrue(post.isQueuedProxyVote());
        assertEquals(1, post.getVoteTime()); assertTrue(post.isUnconfirmedProxySessionDelivery());
    }
    @Test void freshLocalIngressIsClassifiedWithoutReclassifyingHistoricalLocalDelivery() {
        var input = new PlayerVoteEvent(null, "Alex", "a", true);
        var post = new PlayerPostVoteEvent(null, null, true, false, 100, true, "a", UUID.randomUUID(), "Alex", UUID.randomUUID());
        PlayerVoteListener.copySessionObservation(post, input);
        assertTrue(post.isLiveLocalSessionDelivery()); assertFalse(post.isProxySessionDelivery());
        assertEquals(input.getBackendObservationOrder(), post.getBackendObservationOrder());
        assertEquals(100, post.getVoteTime()); // Cached rewards do not imply a historical local occurrence.
        input.setTime(99);
        PlayerVoteListener.copySessionObservation(post, input);
        assertFalse(post.isLiveLocalSessionDelivery()); assertEquals(100, post.getVoteTime());
    }
    @Test void recoveredZeroTimestampLocalVoteDoesNotInventCurrentObservationOrder() {
        var input = new PlayerVoteEvent(null, "Alex", "a", true);
        input.setBackendObservationOrder(0L);
        var post = new PlayerPostVoteEvent(null, null, true, false, System.currentTimeMillis(), false,
                "a", UUID.randomUUID(), "Alex", UUID.randomUUID());
        PlayerVoteListener.copySessionObservation(post,input);
        assertFalse(post.isLiveLocalSessionDelivery()); assertTrue(post.isUnconfirmedLocalSessionDelivery());
        assertEquals(0,post.getBackendObservationOrder()); assertTrue(post.getVoteTime()>0);
    }
}
