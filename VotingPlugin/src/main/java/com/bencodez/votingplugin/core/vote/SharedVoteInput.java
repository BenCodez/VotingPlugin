package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.UUID;

/**
 * An already-accepted vote entering the shared processing path. Native ingress,
 * proxy/global duplicate filtering, service-site validation and security checks
 * remain upstream and are intentionally not reimplemented here.
 */
public record SharedVoteInput(UUID voteId, String playerName, String serviceSite, long voteTime,
        boolean realVote, boolean addTotals, boolean proxyVote, boolean wasOnline) {
    public SharedVoteInput {
        Objects.requireNonNull(voteId, "voteId");
        Objects.requireNonNull(playerName, "playerName");
        Objects.requireNonNull(serviceSite, "serviceSite");
        if (playerName.isBlank()) {
            throw new IllegalArgumentException("playerName cannot be blank");
        }
        if (serviceSite.isBlank()) {
            throw new IllegalArgumentException("serviceSite cannot be blank");
        }
        if (voteTime < 0) {
            throw new IllegalArgumentException("voteTime cannot be negative");
        }
    }
}
