package com.bencodez.votingplugin.core.vote;

/**
 * Platform-neutral subset of the existing vote-processing configuration.
 * Native adapters should populate it from the current VotingPlugin config.
 */
public record SharedVotePolicy(boolean countFakeVotes, boolean addTotals,
        boolean addTotalsOffline, boolean processRewards, boolean giveOfflineRewards) {

    boolean shouldApplyConfiguredVoteMutation(SharedVoteInput input) {
        return input.addTotals() && (input.realVote() || countFakeVotes);
    }

    boolean shouldCountTotals(SharedVoteInput input, boolean online) {
        return shouldApplyConfiguredVoteMutation(input) && addTotals && (addTotalsOffline || online);
    }

    boolean shouldAwardConfiguredPoints(SharedVoteInput input) {
        // Existing Bukkit behavior awards configured vote points when the accepted
        // vote is countable even if Config.AddTotals itself is disabled.
        return shouldApplyConfiguredVoteMutation(input);
    }

    boolean shouldExecuteRewardsNow(SharedVoteInput input, boolean online) {
        // Proxy votes preserve the existing force-processing behavior. For native
        // votes, ProcessRewards and per-site offline eligibility remain authoritative.
        return input.proxyVote() || (processRewards && (online || giveOfflineRewards));
    }
}
