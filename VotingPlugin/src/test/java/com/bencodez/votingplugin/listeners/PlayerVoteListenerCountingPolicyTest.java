package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import com.bencodez.votingplugin.core.vote.SharedVoteInput;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

class PlayerVoteListenerCountingPolicyTest {
    @Test
    void onlineAcceptedVoteAddsEachTotalAndPointsOnce() {
        assertCounts(true, true, false, true, false, true, true, true);
    }

    @Test
    void offlineVoteHonorsAddTotalsOfflineWithoutLosingPoints() {
        assertCounts(true, true, false, true, false, false, false, true);
        assertCounts(true, true, false, true, true, false, true, true);
    }

    @Test
    void eventAddTotalsFalsePreventsBothTotalsAndPoints() {
        assertCounts(true, false, false, true, true, true, false, false);
    }

    @Test
    void configAddTotalsFalseStillAwardsConfiguredPoints() {
        assertCounts(true, true, false, false, true, true, false, true);
    }

    @Test
    void fakeVoteUsesCountFakeVotesSetting() {
        assertCounts(false, true, false, true, true, true, false, false);
        assertCounts(false, true, true, true, true, true, true, true);
    }

    @Test
    void totalsAndPointsKeepProductionCallOrder() {
        VotingPluginUser user = mock(VotingPluginUser.class);
        when(user.isOnline()).thenReturn(true);
        SharedVoteInput input = input(true, true);
        SharedVotePolicy policy = new SharedVotePolicy(false, true, false, false, false);

        PlayerVoteListener.applyAcceptedVoteCounts(user, policy, input);

        InOrder order = inOrder(user);
        order.verify(user).isOnline();
        order.verify(user).addTotal();
        order.verify(user).addTotalDaily();
        order.verify(user).addTotalWeekly();
        order.verify(user).addPoints();
        order.verifyNoMoreInteractions();
    }

    @Test
    void acceptedInputKeepsProxyOriginSeparateFromForcedRoutingAndRawMetadata() {
        PlayerVoteEvent event = mock(PlayerVoteEvent.class);
        when(event.isBungee()).thenReturn(true);
        when(event.isForceBungee()).thenReturn(false);
        when(event.isWasOnline()).thenReturn(true);
        when(event.isRealVote()).thenReturn(true);
        when(event.isAddTotals()).thenReturn(false);
        UUID voteId = UUID.randomUUID();

        SharedVoteInput input = PlayerVoteListener.acceptedVoteInput(event, "CreditedName", voteId, -7L);

        assertEquals(voteId, input.voteId());
        assertEquals("CreditedName", input.playerName());
        assertNull(input.serviceSite());
        assertEquals(-7L, input.voteTime());
        assertTrue(input.proxyVote());
        assertTrue(input.wasOnline());
        assertFalse(input.forceProxyRouting());
        assertFalse(input.addTotals());
        assertEquals(" ", PlayerVoteListener.acceptedVoteInput(event, " ", voteId, -7L).playerName());
    }

    private static void assertCounts(boolean realVote, boolean eventAddsTotals, boolean countFakeVotes,
            boolean configAddTotals, boolean addTotalsOffline, boolean currentlyOnline,
            boolean expectedTotals, boolean expectedPoints) {
        VotingPluginUser user = mock(VotingPluginUser.class);
        when(user.isOnline()).thenReturn(currentlyOnline);
        SharedVotePolicy policy = new SharedVotePolicy(countFakeVotes, configAddTotals, addTotalsOffline, true, true);

        PlayerVoteListener.applyAcceptedVoteCounts(user, policy, input(realVote, eventAddsTotals));

        verify(user, times(expectedTotals ? 1 : 0)).addTotal();
        verify(user, times(expectedTotals ? 1 : 0)).addTotalDaily();
        verify(user, times(expectedTotals ? 1 : 0)).addTotalWeekly();
        verify(user, times(expectedPoints ? 1 : 0)).addPoints();
        if (!expectedPoints || !configAddTotals || addTotalsOffline) verify(user, never()).isOnline();
    }

    private static SharedVoteInput input(boolean realVote, boolean addTotals) {
        return new SharedVoteInput(UUID.randomUUID(), "Ben", "Example", 1L,
                realVote, addTotals, false, false, false);
    }
}
