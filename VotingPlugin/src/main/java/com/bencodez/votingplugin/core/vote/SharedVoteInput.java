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
        if (playerName.isBlank()) throw new IllegalArgumentException("playerName cannot be blank");
        if (serviceSite.isBlank()) throw new IllegalArgumentException("serviceSite cannot be blank");
        if (voteTime < 0) throw new IllegalArgumentException("voteTime cannot be negative");
    }

    /** PlayerVoteEvent uses zero as "now"; normalize before durable mutation/receipt creation. */
    public SharedVoteInput normalizedVoteTime(long nowEpochMillis) {
        if (voteTime != 0) return this;
        if (nowEpochMillis <= 0) throw new IllegalArgumentException("normalized vote time must be positive");
        return new SharedVoteInput(voteId, playerName, serviceSite, nowEpochMillis,
                realVote, addTotals, proxyVote, wasOnline);
    }

    /** A retry carrying the zero sentinel still identifies its already-normalized persisted receipt. */
    public boolean matchesPersisted(SharedVoteInput persisted) {
        if (persisted == null) return false;
        return voteId.equals(persisted.voteId())
                && playerName.equals(persisted.playerName())
                && serviceSite.equals(persisted.serviceSite())
                && (voteTime == 0 || voteTime == persisted.voteTime())
                && realVote == persisted.realVote()
                && addTotals == persisted.addTotals()
                && proxyVote == persisted.proxyVote()
                && wasOnline == persisted.wasOnline();
    }
}
