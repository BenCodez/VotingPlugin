package com.bencodez.votingplugin.neoforge;

import java.time.DateTimeException;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
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

    boolean canVote(long lastVoteTime, LocalDateTime current, ZoneId storedTimestampZone, int hourOffset) {
        if (lastVoteTime == 0L) return true;
        LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(lastVoteTime), storedTimestampZone)
                .plusHours(hourOffset);
        if (!voteDelayDaily) {
            // Preserve Bukkit's legacy zero-delay behavior: after the first vote,
            // the site remains unavailable unless daily reset mode is enabled.
            return voteDelayMillis > 0L && current.isAfter(lastVote.plus(Duration.ofMillis(voteDelayMillis)));
        }
        try {
            LocalDateTime reset = lastVote.withHour(voteDelayDailyHour).withMinute(0).withSecond(0);
            return current.isAfter(lastVote.isBefore(reset) ? reset : reset.plusHours(24));
        } catch (DateTimeException invalidConfiguration) {
            return false;
        }
    }
}
