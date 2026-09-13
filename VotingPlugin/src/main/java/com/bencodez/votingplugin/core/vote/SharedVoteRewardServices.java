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
     * Admit/deduplicate by receipt.input().voteId() in the existing reward owner.
     * Serialize concurrent delivery/recovery for that occurrence, bind its prepared
     * reward definition, preserve step checkpoints and durably remember the terminal
     * disposition. A repeated call, including after restart or lost acknowledgement,
     * resumes pending work or returns the same result without redoing completed work.
     *
     * <p>Complete only after actual execution/checkpoints or durable offline handoff,
     * never task submission. Recheck live player availability in native adapters;
     * receipt.identity().online() describes the original vote, not a current player.
     * Native actions retain their documented delivery semantics; this port alone is
     * not a claim of exactly-once arbitrary external commands across process death.</p>
     */
    default CompletionStage<RewardDisposition> deliverOnce(SharedVoteReceipt receipt) {
        // Existing unkeyed methods cannot safely implement durable retry implicitly.
        return CompletableFuture.failedFuture(new UnsupportedOperationException(
                "Shared vote rewards require keyed durable replay support"));
    }
}
