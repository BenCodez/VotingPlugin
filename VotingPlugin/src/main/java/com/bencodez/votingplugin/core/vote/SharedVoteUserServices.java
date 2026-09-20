package com.bencodez.votingplugin.core.vote;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/** Port for AdvancedCore's shared user/cache/storage owner. */
public interface SharedVoteUserServices {
    CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity, SharedVoteMutation mutation);
    CompletionStage<SharedVoteUserSnapshot> load(UUID uuid);

    default CompletionStage<SharedVoteReceipt> findVote(UUID voteId) { return unsupported(); }

    /**
     * One transaction atomically applies the mutation and inserts a pending receipt,
     * including the prepared immutable reward version, or returns the existing receipt
     * without applying totals/points again. Enforce unique voteId in that transaction.
     */
    default CompletionStage<SharedVoteReceipt> persistVoteWithReward(SharedVoteInput input,
            SharedVoteIdentity identity, SharedVoteMutation mutation, boolean executeRewardsNow,
            SharedVoteRewardPlan rewardPlan) {
        return unsupported();
    }

    /** Unsafe legacy shape intentionally has no mutation-only fallback. */
    default CompletionStage<SharedVoteReceipt> persistVoteWithReward(SharedVoteInput input,
            SharedVoteIdentity identity, SharedVoteMutation mutation, boolean executeRewardsNow) {
        return unsupported();
    }

    default CompletionStage<SharedVoteReceipt> markRewardCompleted(UUID voteId, RewardDisposition disposition) {
        return unsupported();
    }

    default CompletionStage<List<SharedVoteReceipt>> pendingVotes(int limit) { return unsupported(); }

    private static <T> CompletionStage<T> unsupported() {
        return CompletableFuture.failedFuture(new UnsupportedOperationException(
                "Shared vote persistence requires atomic vote/receipt recovery support"));
    }
}
