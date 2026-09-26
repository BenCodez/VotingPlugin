package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.UUID;

/** Trusted internal vote input. This is not a network or public ingress API. */
public record NeoForgeVoteRequest(UUID playerId, String playerName, String serviceSite,
        long voteTime, boolean realVote, boolean addTotals, boolean online, Scope scope) {
    public NeoForgeVoteRequest {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(playerName, "playerName");
        Objects.requireNonNull(serviceSite, "serviceSite");
        Objects.requireNonNull(scope, "scope");
    }

    /**
     * ACCOUNTING_ONLY is deliberately test/internal. COMPLETE requires the
     * reward and follow-up pipeline and is rejected until that pipeline exists.
     */
    public enum Scope { ACCOUNTING_ONLY, COMPLETE }
}
