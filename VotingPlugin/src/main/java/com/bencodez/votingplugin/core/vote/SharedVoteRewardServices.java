package com.bencodez.votingplugin.core.vote;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/** Adapter for AdvancedCore reward orchestration and its existing durable replay owner. */
public interface SharedVoteRewardServices {
    CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);

    CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);

    /**
     * Resolve and freeze the exact reward configuration BEFORE the vote transaction.
     * versionReference must resolve to this same definition after reload/restart.
     */
    default CompletionStage<SharedVoteRewardPlan> prepareVoteRewards(SharedVoteInput input,
            SharedVoteIdentity identity, boolean executeRewardsNow) {
        return CompletableFuture.failedFuture(new UnsupportedOperationException(
                "Shared vote rewards require a persistable prepared reward version"));
    }

    /**
     * Admit/deduplicate by receipt.input().voteId() in the existing reward owner.
     * Use receipt.rewardPlan() rather than current configuration. Serialize concurrent
     * delivery/recovery for that occurrence, preserve step checkpoints and durably
     * remember the terminal disposition. A repeated call resumes pending work or
     * returns the same result without redoing completed work.
     */
    default CompletionStage<RewardDisposition> deliverOnce(SharedVoteReceipt receipt) {
        return CompletableFuture.failedFuture(new UnsupportedOperationException(
                "Shared vote rewards require keyed durable replay support"));
    }
}
