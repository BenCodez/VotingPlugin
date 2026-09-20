package com.bencodez.votingplugin.core;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Supplier;

import com.bencodez.votingplugin.core.SharedVoteProcessingResult.RewardDisposition;

/**
 * Coordinates one accepted vote through the supplied user and reward adapters.
 * Duplicate filtering belongs to ingress/storage. This class does not replay a
 * reward after failure; a failed reward may follow a completed user mutation.
 */
public final class SharedVoteProcessor {
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
        SharedVoteInput normalized = input.normalizedVoteTime(System.currentTimeMillis());
        return call(() -> identities.resolve(normalized), "identity resolution").thenCompose(identity -> {
            if (identity == null) return failed("Identity resolver returned null identity");
            boolean rewardOnline = normalized.proxyVote() ? normalized.wasOnline() : identity.online();
            SharedVoteMutation mutation = new SharedVoteMutation(normalized.voteId(), normalized.serviceSite(),
                    normalized.voteTime(), policy.shouldCountTotals(normalized, identity.online()),
                    policy.shouldAwardConfiguredPoints(normalized));
            boolean executeNow = policy.shouldExecuteRewardsNow(normalized, rewardOnline);
            return call(() -> users.persistVote(identity, mutation), "vote persistence").thenCompose(state -> {
                if (state == null) return failed("User services returned null vote state");
                Supplier<CompletionStage<Void>> delivery = executeNow
                        ? () -> rewards.executeVoteRewards(normalized, identity, state)
                        : () -> rewards.deferVoteRewards(normalized, identity, state);
                return call(delivery, "reward delivery").thenApply(ignored ->
                        new SharedVoteProcessingResult(identity, state,
                                executeNow ? RewardDisposition.EXECUTED : RewardDisposition.DEFERRED));
            });
        });
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
