package com.bencodez.votingplugin.neoforge;

import java.util.Objects;
import java.util.UUID;

/** Immutable complete-vote input retained until NeoForge can finish its effects. */
public record NeoForgeDeferredVote(UUID voteId, UUID playerId, String playerName,
        String serviceSite, String siteKey, long voteTime, boolean realVote,
        boolean addTotals, boolean wasOnline, NeoForgeVoteAccountingDecision accountingDecision,
        boolean quarantined, boolean releaseRequested) {
    public NeoForgeDeferredVote {
        Objects.requireNonNull(voteId, "voteId");
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(playerName, "playerName");
        Objects.requireNonNull(serviceSite, "serviceSite");
        Objects.requireNonNull(siteKey, "siteKey");
    }

    public NeoForgeDeferredVote(UUID voteId, UUID playerId, String playerName,
            String serviceSite, String siteKey, long voteTime, boolean realVote,
            boolean addTotals, boolean wasOnline) {
        this(voteId, playerId, playerName, serviceSite, siteKey, voteTime,
                realVote, addTotals, wasOnline, null, false);
    }

    public NeoForgeDeferredVote(UUID voteId, UUID playerId, String playerName,
            String serviceSite, String siteKey, long voteTime, boolean realVote,
            boolean addTotals, boolean wasOnline, NeoForgeVoteAccountingDecision accountingDecision) {
        this(voteId, playerId, playerName, serviceSite, siteKey, voteTime,
                realVote, addTotals, wasOnline, accountingDecision, false);
    }

    public NeoForgeDeferredVote(UUID voteId, UUID playerId, String playerName,
            String serviceSite, String siteKey, long voteTime, boolean realVote,
            boolean addTotals, boolean wasOnline, NeoForgeVoteAccountingDecision decision,
            boolean quarantined) {
        this(voteId, playerId, playerName, serviceSite, siteKey, voteTime,
                realVote, addTotals, wasOnline, decision, quarantined, false);
    }

    NeoForgeDeferredVote releaseRequestedCopy() {
        if (releaseRequested) return this;
        return new NeoForgeDeferredVote(voteId, playerId, playerName, serviceSite, siteKey,
                voteTime, realVote, addTotals, wasOnline, accountingDecision, quarantined, true);
    }

    NeoForgeDeferredVote withReplayContext(String currentName, String configuredServiceSite) {
        Objects.requireNonNull(currentName, "currentName");
        Objects.requireNonNull(configuredServiceSite, "configuredServiceSite");
        if (playerName.equals(currentName) && serviceSite.equals(configuredServiceSite)) return this;
        return new NeoForgeDeferredVote(voteId, playerId, currentName, configuredServiceSite, siteKey,
                voteTime, realVote, addTotals, wasOnline, accountingDecision, quarantined, releaseRequested);
    }

    NeoForgeDeferredVote quarantinedCopy() {
        if (quarantined) return this;
        return new NeoForgeDeferredVote(voteId, playerId, playerName, serviceSite, siteKey,
                voteTime, realVote, addTotals, wasOnline, accountingDecision, true, releaseRequested);
    }

    NeoForgeDeferredVote replayableCopy() {
        if (!quarantined) return this;
        return new NeoForgeDeferredVote(voteId, playerId, playerName, serviceSite, siteKey,
                voteTime, realVote, addTotals, wasOnline, accountingDecision, false, releaseRequested);
    }
}
