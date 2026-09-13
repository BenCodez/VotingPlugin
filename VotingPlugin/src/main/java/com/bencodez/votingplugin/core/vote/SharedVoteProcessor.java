package com.bencodez.votingplugin.core.vote;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Supplier;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/**
 * Accepted vote -> atomic user mutation/pending reward receipt -> keyed reward
 * delivery -> durable acknowledgement. Ingress validation, proxy/global ownership
 * and duplicate filtering remain upstream; these receipt checks make local retries
 * recoverable and do not replace or bypass those ingress/security contracts.
 */
public final class SharedVoteProcessor {
    private static final int MAX_RECOVERY_BATCH = 100;
    private final SharedVoteIdentityResolver identities;
    private final SharedVoteUserServices users;
    private final SharedVoteRewardServices rewards;

    public SharedVoteProcessor(SharedVoteIdentityResolver identities, SharedVoteUserServices users,
            SharedVoteRewardServices rewards) {
        this.identities = Objects.requireNonNull(identities, "identities");
        this.users = Objects.requireNonNull(users, "users");
        this.rewards = Objects.requireNonNull(rewards, "rewards");
    }

    public CompletionStage<SharedVoteProcessingResult> process(SharedVoteInput input, SharedVotePolicy policy) {
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(policy, "policy");
        return call(() -> users.findVote(input.voteId()), "receipt lookup").thenCompose(existing -> {
            if (existing != null) {
                existing.requireInput(input);
                return deliver(existing);
            }
            return call(() -> identities.resolve(input), "identity resolution").thenCompose(identity -> {
                if (identity == null) return failed("Identity resolver returned null identity");
                boolean online = input.proxyVote() ? input.wasOnline() : identity.online();
                SharedVoteMutation mutation = new SharedVoteMutation(input.voteId(), input.serviceSite(), input.voteTime(),
                        policy.shouldCountTotals(input, online), policy.shouldAwardConfiguredPoints(input));
                boolean executeNow = policy.shouldExecuteRewardsNow(input, online);
                return call(() -> users.persistVoteWithReward(input, identity, mutation, executeNow), "atomic persistence")
                        .thenCompose(receipt -> {
                            if (receipt == null) return failed("User services returned null vote receipt");
                            receipt.requireInput(input);
                            if (!receipt.identity().uuid().equals(identity.uuid())) {
                                return failed("Vote receipt belongs to a different resolved identity");
                            }
                            return deliver(receipt);
                        });
            });
        });
    }

    /** Recover one persisted occurrence without recounting or resolving the player again. */
    public CompletionStage<SharedVoteProcessingResult> recover(UUID voteId) {
        Objects.requireNonNull(voteId, "voteId");
        return call(() -> users.findVote(voteId), "receipt lookup").thenCompose(receipt -> {
            if (receipt == null || !voteId.equals(receipt.input().voteId())) {
                return failed("Pending vote receipt was not found");
            }
            return deliver(receipt);
        });
    }

    /**
     * Bounded startup batch; individual failures remain pending and do not prevent
     * other entries from recovering. The returned failed stage aggregates failures
     * after the batch, so callers cannot mistake a partial recovery for success.
     */
    public CompletionStage<List<SharedVoteProcessingResult>> recoverPending(int limit) {
        if (limit < 1 || limit > MAX_RECOVERY_BATCH) throw new IllegalArgumentException("Recovery limit must be 1..100");
        return call(() -> users.pendingVotes(limit), "pending receipt scan").thenCompose(receipts -> {
            if (receipts == null || receipts.size() > limit) return failed("Invalid pending receipt batch");
            List<SharedVoteReceipt> batch = List.copyOf(receipts);
            HashSet<UUID> ids = new HashSet<>();
            for (SharedVoteReceipt receipt : batch) {
                if (!receipt.pending() || !ids.add(receipt.input().voteId())) return failed("Invalid pending receipt entry");
            }
            List<SharedVoteProcessingResult> results = new ArrayList<>();
            List<Throwable> failures = new ArrayList<>();
            CompletionStage<Void> chain = CompletableFuture.completedFuture(null);
            for (SharedVoteReceipt receipt : batch) {
                chain = chain.thenCompose(ignored -> deliver(receipt).handle((result, failure) -> {
                    if (failure == null) results.add(result);
                    else failures.add(failure);
                    return null;
                }));
            }
            return chain.thenCompose(ignored -> {
                if (failures.isEmpty()) return CompletableFuture.completedFuture(List.copyOf(results));
                IllegalStateException failure = new IllegalStateException("Some pending vote rewards could not be recovered");
                failures.forEach(failure::addSuppressed);
                return CompletableFuture.failedFuture(failure);
            });
        });
    }

    private CompletionStage<SharedVoteProcessingResult> deliver(SharedVoteReceipt receipt) {
        if (!receipt.pending()) return CompletableFuture.completedFuture(result(receipt));
        return call(() -> rewards.deliverOnce(receipt), "keyed reward delivery").thenCompose(disposition -> {
            if (disposition == null) return failed("Reward services returned null delivery disposition");
            return call(() -> users.markRewardCompleted(receipt.input().voteId(), disposition), "reward acknowledgement")
                    .thenApply(completed -> {
                        if (completed == null) throw new IllegalStateException("Missing acknowledged vote receipt");
                        completed.requireSameOrigin(receipt);
                        if (completed.completedDisposition() != disposition) {
                            throw new IllegalStateException("Vote reward acknowledgement did not preserve the delivery result");
                        }
                        return result(completed);
                    });
        });
    }

    private static SharedVoteProcessingResult result(SharedVoteReceipt receipt) {
        return new SharedVoteProcessingResult(receipt.identity(), receipt.persistedState(), receipt.completedDisposition());
    }

    private static <T> CompletionStage<T> call(Supplier<CompletionStage<T>> operation, String name) {
        try {
            CompletionStage<T> stage = operation.get();
            return stage == null ? failed("Adapter returned null stage for " + name) : stage;
        } catch (Throwable failure) {
            return CompletableFuture.failedFuture(failure);
        }
    }

    private static <T> CompletionStage<T> failed(String message) {
        return CompletableFuture.failedFuture(new IllegalStateException(message));
    }
}
