package com.bencodez.votingplugin.neoforge;

import java.util.Map;
import java.util.Objects;
import java.util.UUID;

/** Persisted NeoForge subset of a VotingPlugin user. */
public record NeoForgeVoteAccount(UUID uuid, String playerName, int allTimeTotal,
        int monthTotal, int dailyTotal, int weeklyTotal, int points,
        Map<String, Long> lastVotes) {

    public NeoForgeVoteAccount {
        Objects.requireNonNull(uuid, "uuid");
        Objects.requireNonNull(playerName, "playerName");
        lastVotes = Map.copyOf(lastVotes);
    }

    public long lastVote(String siteKey) {
        Objects.requireNonNull(siteKey, "siteKey");
        return lastVotes.entrySet().stream()
                .filter(entry -> entry.getKey().equalsIgnoreCase(siteKey))
                .map(Map.Entry::getValue).findFirst().orElse(0L);
    }
}
