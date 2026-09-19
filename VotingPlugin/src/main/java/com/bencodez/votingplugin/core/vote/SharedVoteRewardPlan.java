package com.bencodez.votingplugin.core.vote;

import java.util.Objects;

/**
 * Immutable reference to the exact reward definition prepared for one vote.
 * The version reference must continue resolving to the same archived/snapshotted
 * definition after configuration reloads and process restarts.
 */
public record SharedVoteRewardPlan(String planId, String versionReference) {
    public SharedVoteRewardPlan {
        Objects.requireNonNull(planId, "planId");
        Objects.requireNonNull(versionReference, "versionReference");
        if (planId.isBlank()) throw new IllegalArgumentException("planId cannot be blank");
        if (versionReference.isBlank()) throw new IllegalArgumentException("versionReference cannot be blank");
    }
}
