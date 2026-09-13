package com.bencodez.votingplugin.core.vote;

import java.util.Objects;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/**
 * Immutable receipt keyed by voteId in the existing persistence owner. Its initial
 * form is committed atomically with totals/points and the pending reward intent.
 * Keep the original input, resolved UUID, counting policy and per-vote snapshot
 * across retries; only the durable reward acknowledgement may change.
 */
public record SharedVoteReceipt(SharedVoteInput input, SharedVoteIdentity identity, SharedVoteMutation mutation,
        SharedVoteUserSnapshot persistedState, boolean executeRewardsNow, RewardDisposition completedDisposition) {
    public SharedVoteReceipt {
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(mutation, "mutation");
        Objects.requireNonNull(persistedState, "persistedState");
        if (!input.voteId().equals(mutation.voteId()) || !input.serviceSite().equals(mutation.serviceSite())
                || input.voteTime() != mutation.voteTime()) {
            throw new IllegalArgumentException("Vote receipt input and mutation do not match");
        }
    }

    public boolean pending() { return completedDisposition == null; }

    public SharedVoteReceipt completed(RewardDisposition disposition) {
        Objects.requireNonNull(disposition, "disposition");
        if (!pending() && disposition != completedDisposition) {
            throw new IllegalStateException("A completed vote receipt cannot change its reward result");
        }
        return new SharedVoteReceipt(input, identity, mutation, persistedState, executeRewardsNow, disposition);
    }

    public void requireInput(SharedVoteInput expected) {
        if (!input.equals(expected)) throw new IllegalStateException("voteId is already bound to different vote input");
    }

    public void requireSameOrigin(SharedVoteReceipt expected) {
        requireInput(expected.input());
        if (!identity.equals(expected.identity()) || !mutation.equals(expected.mutation())
                || !persistedState.equals(expected.persistedState()) || executeRewardsNow != expected.executeRewardsNow()) {
            throw new IllegalStateException("Vote receipt changed while acknowledging reward delivery");
        }
    }
}
