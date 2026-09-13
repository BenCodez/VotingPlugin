package com.bencodez.votingplugin.core.vote;

import java.util.concurrent.CompletionStage;

/**
 * Port for AdvancedCore shared reward orchestration. Implementations own native
 * command/message/item/effect adapters and durable offline delivery.
 */
public interface SharedVoteRewardServices {
    CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);

    CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);
}
