package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.UUID;

/** Immutable complete-vote input retained until NeoForge can finish its effects. */
public record NeoForgeDeferredVote(UUID voteId, UUID playerId, String playerName,
        String serviceSite, String siteKey, long voteTime, boolean realVote,
        boolean addTotals, boolean wasOnline) {
    public NeoForgeDeferredVote {
        Objects.requireNonNull(voteId, "voteId");
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(playerName, "playerName");
        Objects.requireNonNull(serviceSite, "serviceSite");
        Objects.requireNonNull(siteKey, "siteKey");
    }

    NeoForgeDeferredVote withReplayContext(String currentName, String configuredServiceSite) {
        Objects.requireNonNull(currentName, "currentName");
        Objects.requireNonNull(configuredServiceSite, "configuredServiceSite");
        if (playerName.equals(currentName) && serviceSite.equals(configuredServiceSite)) return this;
        return new NeoForgeDeferredVote(voteId, playerId, currentName, configuredServiceSite, siteKey,
                voteTime, realVote, addTotals, wasOnline);
    }
}
