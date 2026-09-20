package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.function.BooleanSupplier;

/**
 * Platform-neutral subset of the existing vote-processing configuration.
 * Native adapters should populate it from the current VotingPlugin config.
 */
public record SharedVotePolicy(boolean countFakeVotes, boolean addTotals,
        boolean addTotalsOffline, boolean processRewards, boolean giveOfflineRewards) {

    boolean shouldApplyConfiguredVoteMutation(SharedVoteInput input) {
        return shouldApplyConfiguredVoteMutation(input.realVote(), input.addTotals());
    }

    /** The accepted Bukkit vote's existing fake-vote and add-totals gate. */
    public boolean shouldApplyConfiguredVoteMutation(boolean realVote, boolean voteAddsTotals) {
        return voteAddsTotals && (realVote || countFakeVotes);
    }

    boolean shouldCountTotals(SharedVoteInput input, boolean online) {
        return shouldCountTotals(input.realVote(), input.addTotals(), online);
    }

    /** Online means the user's current state, including for a proxy-origin vote. */
    public boolean shouldCountTotals(boolean realVote, boolean voteAddsTotals, boolean online) {
        return shouldCountTotals(realVote, voteAddsTotals, () -> online);
    }

    /** Check online state only if the configuration actually requires it. */
    public boolean shouldCountTotals(boolean realVote, boolean voteAddsTotals, BooleanSupplier currentlyOnline) {
        Objects.requireNonNull(currentlyOnline, "currentlyOnline");
        return shouldApplyConfiguredVoteMutation(realVote, voteAddsTotals)
                && addTotals && (addTotalsOffline || currentlyOnline.getAsBoolean());
    }

    boolean shouldAwardConfiguredPoints(SharedVoteInput input) {
        // Existing Bukkit behavior awards configured vote points when the accepted
        // vote is countable even if Config.AddTotals itself is disabled.
        return shouldApplyConfiguredVoteMutation(input.realVote(), input.addTotals());
    }

    boolean shouldExecuteRewardsNow(SharedVoteInput input, boolean online) {
        // Proxy votes preserve the existing force-processing behavior. For native
        // votes, ProcessRewards and per-site offline eligibility remain authoritative.
        return SharedVoteDelivery.shouldDeliverNow(input::proxyVote, () -> online,
                () -> giveOfflineRewards, () -> processRewards);
    }
}
