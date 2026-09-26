package com.bencodez.votingplugin.neoforge;

/** Result of the internal NeoForge vote-processing boundary. */
public record NeoForgeVoteResult(Status status, NeoForgeVoteAccount account, String detail) {
    public enum Status {
        ACCOUNTED,
        UNKNOWN_PLAYER,
        UNKNOWN_SITE,
        VOTE_DELAY_ACTIVE,
        DEFERRED,
        DEFERRED_CAPACITY_REACHED,
        STOPPED
    }

    public boolean accountingMutated() { return status == Status.ACCOUNTED; }
    public boolean durablyRetained() { return status == Status.DEFERRED; }
}
