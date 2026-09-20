package com.bencodez.votingplugin.core;

import java.util.concurrent.CompletionStage;

/** Calls the platform's existing reward execution and offline-queue paths. */
public interface SharedVoteRewardServices {
    CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);

    CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity,
            SharedVoteUserSnapshot persistedState);
}
