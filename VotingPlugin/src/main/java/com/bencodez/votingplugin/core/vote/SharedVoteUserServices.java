package com.bencodez.votingplugin.core.vote;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/**
 * Port for AdvancedCore's shared user/cache/storage owner. Implement atomic receipt
 * operations there, not in a second VotingPlugin SQL stack/cache/queue. A voteId
 * remains bound after completion for the ingress owner's supported replay horizon.
 */
public interface SharedVoteUserServices {
    /** Legacy API retained; the recoverable processor never uses this mutation-only operation. */
    CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity, SharedVoteMutation mutation);

    CompletionStage<SharedVoteUserSnapshot> load(UUID uuid);

    /** Read-only receipt lookup; null means absent, not an unavailable database. */
    default CompletionStage<SharedVoteReceipt> findVote(UUID voteId) {
        return unsupported();
    }

    /**
     * One transaction atomically applies the mutation and inserts a pending receipt,
     * or returns the existing receipt without applying totals/points again. Enforce
     * unique voteId and reject conflicting input or identity in the SAME transaction.
     * Concurrent retries and lost commit acknowledgements must be safe. Existing
     * receipts retain their original policy/snapshot; do not recompute on replay.
     */
    default CompletionStage<SharedVoteReceipt> persistVoteWithReward(SharedVoteInput input,
            SharedVoteIdentity identity, SharedVoteMutation mutation, boolean executeRewardsNow) {
        return unsupported();
    }

    /**
     * Acknowledge only after the reward owner has durably completed delivery or
     * accepted offline responsibility. Retain the receipt/deduplication key. A retry
     * returns the same terminal receipt rather than changing its disposition.
     */
    default CompletionStage<SharedVoteReceipt> markRewardCompleted(UUID voteId, RewardDisposition disposition) {
        return unsupported();
    }

    /** Bounded SQL-backed startup/recovery scan of pending receipts, never all user rows. */
    default CompletionStage<List<SharedVoteReceipt>> pendingVotes(int limit) {
        return unsupported();
    }

    private static <T> CompletionStage<T> unsupported() {
        // No mutation-only fallback: it would recreate the crash window.
        return CompletableFuture.failedFuture(new UnsupportedOperationException(
                "Shared vote persistence requires atomic vote/receipt recovery support"));
    }
}
