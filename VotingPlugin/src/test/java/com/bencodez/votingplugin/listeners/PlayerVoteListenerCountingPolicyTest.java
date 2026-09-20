package com.bencodez.votingplugin.listeners;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.core.SharedVotePolicy;
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

    private static void assertCounts(boolean realVote, boolean eventAddsTotals, boolean countFakeVotes,
            boolean configAddTotals, boolean addTotalsOffline, boolean currentlyOnline,
            boolean expectedTotals, boolean expectedPoints) {
        VotingPluginUser user = mock(VotingPluginUser.class);
        when(user.isOnline()).thenReturn(currentlyOnline);
        SharedVotePolicy policy = new SharedVotePolicy(countFakeVotes, configAddTotals, addTotalsOffline, true, true);

        PlayerVoteListener.applyAcceptedVoteCounts(user, policy, realVote, eventAddsTotals);

        verify(user, times(expectedTotals ? 1 : 0)).addTotal();
        verify(user, times(expectedTotals ? 1 : 0)).addTotalDaily();
        verify(user, times(expectedTotals ? 1 : 0)).addTotalWeekly();
        verify(user, times(expectedPoints ? 1 : 0)).addPoints();
        if (!expectedPoints || !configAddTotals || addTotalsOffline) verify(user, never()).isOnline();
    }
}
