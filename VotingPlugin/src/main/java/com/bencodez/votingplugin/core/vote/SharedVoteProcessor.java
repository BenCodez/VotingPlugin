package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Headless vote -> identity -> durable user mutation -> reward path.
 *
 * <p>Ingress validation, duplicate suppression, proxy/global coordination and
 * VoteSite lookup remain in their existing adapters. This processor starts only
 * after a vote has been accepted by those contracts.</p>
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

        CompletionStage<SharedVoteIdentity> resolution = identities.resolve(input);
        if (resolution == null) {
            return CompletableFuture.failedFuture(new IllegalStateException("Identity resolver returned null stage"));
        }

        return resolution.thenCompose(identity -> {
            if (identity == null) {
                return CompletableFuture.failedFuture(new IllegalStateException("Identity resolver returned null identity"));
            }
            boolean online = input.proxyVote() ? input.wasOnline() : identity.online();
            SharedVoteMutation mutation = new SharedVoteMutation(input.voteId(), input.serviceSite(), input.voteTime(),
                    policy.shouldCountTotals(input, online), policy.shouldAwardConfiguredPoints(input));

            CompletionStage<SharedVoteUserSnapshot> persistence = users.persistVote(identity, mutation);
            if (persistence == null) {
                return CompletableFuture.failedFuture(new IllegalStateException("User services returned null persistence stage"));
            }

            return persistence.thenCompose(persisted -> {
                if (persisted == null) {
                    return CompletableFuture.failedFuture(
                            new IllegalStateException("User services returned null persisted state"));
                }
                boolean executeNow = policy.shouldExecuteRewardsNow(input, online);
                CompletionStage<Void> rewardCompletion = executeNow
                        ? rewards.executeVoteRewards(input, identity, persisted)
                        : rewards.deferVoteRewards(input, identity, persisted);
                if (rewardCompletion == null) {
                    return CompletableFuture.failedFuture(new IllegalStateException(
                            "Reward services returned null " + (executeNow ? "execution" : "deferral") + " stage"));
                }
                SharedVoteProcessingResult.RewardDisposition disposition = executeNow
                        ? SharedVoteProcessingResult.RewardDisposition.EXECUTED
                        : SharedVoteProcessingResult.RewardDisposition.DEFERRED;
                return rewardCompletion.thenApply(ignored -> new SharedVoteProcessingResult(identity, persisted, disposition));
            });
        });
    }
}
