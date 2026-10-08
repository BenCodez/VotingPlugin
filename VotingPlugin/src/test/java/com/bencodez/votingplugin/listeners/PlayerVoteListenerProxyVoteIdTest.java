package com.bencodez.votingplugin.listeners;

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

        assertEquals(legacyId, PlayerVoteListener.resolveProxyVoteId(event));
    }

    @Test
    @SuppressWarnings("deprecation")
    void explicitTransportIdentityWinsOverLegacySnapshot() {
        UUID explicitId = UUID.randomUUID();
        PlayerVoteEvent event = new PlayerVoteEvent(null, "Ben", "Example", true);
        event.setProxyVoteId(explicitId);
        event.setBungeeTextTotals(new VoteTotalsSnapshot(1, 2, 3, 4, 5, 6, 7, 8, UUID.randomUUID()));

        assertEquals(explicitId, PlayerVoteListener.resolveProxyVoteId(event));
    }
    @Test void dateAccountingRequiresExplicitTransportIdentityInsteadOfLegacyCorrelationFallback() throws Exception {
        for (boolean explicit : new boolean[] {false, true}) {
            UUID voteId = UUID.randomUUID();
            PlayerVoteEvent event = new PlayerVoteEvent(null, "Ben", "Example", true);
            event.setTime(123L); event.setBungee(true);
            event.setBungeeTextTotals(new VoteTotalsSnapshot(1,2,3,4,5,6,7,8,voteId));
            if (explicit) event.setProxyVoteId(voteId);
            var plugin = org.mockito.Mockito.mock(com.bencodez.votingplugin.VotingPluginMain.class);
            var milestones = org.mockito.Mockito.mock(com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones.class);
            org.mockito.Mockito.when(plugin.getDateVoteMilestones()).thenReturn(milestones);
            var user = org.mockito.Mockito.mock(com.bencodez.votingplugin.user.VotingPluginUser.class);
            var site = org.mockito.Mockito.mock(com.bencodez.votingplugin.votesites.VoteSite.class);
            org.mockito.Mockito.when(site.getKey()).thenReturn("Example");
            Class<?> operations = Class.forName(PlayerVoteListener.class.getName()+"$BukkitOperations");
            var constructor = operations.getDeclaredConstructor(com.bencodez.votingplugin.VotingPluginMain.class, PlayerVoteEvent.class); constructor.setAccessible(true);
            var method = operations.getDeclaredMethod("dateMilestones",com.bencodez.votingplugin.user.VotingPluginUser.class,com.bencodez.votingplugin.votesites.VoteSite.class,long.class,UUID.class);method.setAccessible(true);
            method.invoke(constructor.newInstance(plugin,event),user,site,123L,voteId);
            org.mockito.Mockito.verify(milestones).accepted(user,"Example",voteId,123L,true,true,explicit,false,false);
            assertEquals(voteId,PlayerVoteListener.resolveProxyVoteId(event));
        }
    }

}
