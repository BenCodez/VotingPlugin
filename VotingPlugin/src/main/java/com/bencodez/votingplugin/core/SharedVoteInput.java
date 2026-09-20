package com.bencodez.votingplugin.core;

import java.util.Objects;
import java.util.UUID;

/**
 * An already-accepted vote entering the shared processing path. Native ingress,
 * proxy/global duplicate filtering, service-site validation and security checks
 * remain upstream and are intentionally not reimplemented here.
 */
public record SharedVoteInput(UUID voteId, String playerName, String serviceSite, long voteTime,
        boolean realVote, boolean addTotals, boolean proxyVote, boolean forceProxyRouting, boolean wasOnline) {
    public SharedVoteInput {
        Objects.requireNonNull(voteId, "voteId");
        Objects.requireNonNull(playerName, "playerName");
        Objects.requireNonNull(serviceSite, "serviceSite");
        if (playerName.isBlank()) throw new IllegalArgumentException("playerName cannot be blank");
        if (serviceSite.isBlank()) throw new IllegalArgumentException("serviceSite cannot be blank");
        if (voteTime < 0) throw new IllegalArgumentException("voteTime cannot be negative");
    }

    /**
     * Compatibility constructor for callers that historically had one proxy bit.
     * Native adapters should use the full constructor and map isBungee() and
     * isForceBungee() independently.
     */
    public SharedVoteInput(UUID voteId, String playerName, String serviceSite, long voteTime,
            boolean realVote, boolean addTotals, boolean proxyVote, boolean wasOnline) {
        this(voteId, playerName, serviceSite, voteTime, realVote, addTotals,
                proxyVote, proxyVote, wasOnline);
    }

    /** PlayerVoteEvent uses zero as "now"; normalize before mutating the user. */
    public SharedVoteInput normalizedVoteTime(long nowEpochMillis) {
        if (voteTime != 0) return this;
        if (nowEpochMillis <= 0) throw new IllegalArgumentException("normalized vote time must be positive");
        return new SharedVoteInput(voteId, playerName, serviceSite, nowEpochMillis,
                realVote, addTotals, proxyVote, forceProxyRouting, wasOnline);
    }

}
