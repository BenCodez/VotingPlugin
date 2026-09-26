package com.bencodez.votingplugin.neoforge;

import java.util.Objects;

/** Configuration needed to identify a vote site and enforce its vote-delay policy. */
public record NeoForgeVoteSite(String key, String displayName, String serviceSite,
        int priority, long voteDelayMillis, boolean enabled, boolean waitUntilVoteDelay,
        boolean voteDelayDaily, int voteDelayDailyHour, boolean giveOfflineRewards) {

    public NeoForgeVoteSite {
        Objects.requireNonNull(key, "key");
        Objects.requireNonNull(displayName, "displayName");
    }

    boolean canResolveIncomingVote() {
        return enabled && serviceSite != null && !serviceSite.isEmpty();
    }

    boolean matches(String identifier) {
        return serviceSite.equalsIgnoreCase(identifier)
                || key.equalsIgnoreCase(identifier)
                || displayName.equalsIgnoreCase(identifier);
    }
}
