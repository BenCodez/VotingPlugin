package com.bencodez.votingplugin.tests.listeners;

import com.bencodez.votingplugin.listeners.PlayerVoteListener;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;

class PlayerVoteListenerProxyVoteIdTest {

    @Test
    @SuppressWarnings("deprecation")
    void legacyTotalsSnapshotRetainsItsCorrelationIdentity() {
        UUID legacyId = UUID.randomUUID();
        PlayerVoteEvent event = new PlayerVoteEvent(null, "Ben", "Example", true);
        event.setBungeeTextTotals(new VoteTotalsSnapshot(1, 2, 3, 4, 5, 6, 7, 8, legacyId));

        assertEquals(legacyId, PlayerVoteListenerAccess.resolveProxyVoteId(event));
    }

    @Test
    @SuppressWarnings("deprecation")
    void explicitTransportIdentityWinsOverLegacySnapshot() {
        UUID explicitId = UUID.randomUUID();
        PlayerVoteEvent event = new PlayerVoteEvent(null, "Ben", "Example", true);
        event.setProxyVoteId(explicitId);
        event.setBungeeTextTotals(new VoteTotalsSnapshot(1, 2, 3, 4, 5, 6, 7, 8, UUID.randomUUID()));

        assertEquals(explicitId, PlayerVoteListenerAccess.resolveProxyVoteId(event));
    }
}
