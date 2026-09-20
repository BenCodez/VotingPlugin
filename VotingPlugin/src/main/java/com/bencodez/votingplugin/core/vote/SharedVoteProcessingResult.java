package com.bencodez.votingplugin.core.vote;

import java.util.Objects;

public record SharedVoteProcessingResult(SharedVoteIdentity identity, SharedVoteUserSnapshot persistedState,
        RewardDisposition rewardDisposition) {
    public SharedVoteProcessingResult {
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(persistedState, "persistedState");
        Objects.requireNonNull(rewardDisposition, "rewardDisposition");
    }

    public enum RewardDisposition {
        EXECUTED,
        DEFERRED
    }
}
