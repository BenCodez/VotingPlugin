package com.bencodez.votingplugin.fabric;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;

/** An immutable, read-only site configuration. */
record FabricVoteSite(String key, String service, String name, boolean enabled, int priority,
        boolean waitUntilVoteDelay, long delayMillis, boolean daily, int dailyHour) {

    boolean matches(String identifier) {
        return enabled && (service.equalsIgnoreCase(identifier) || key.equalsIgnoreCase(identifier)
                || name.equalsIgnoreCase(identifier));
    }

    boolean canVote(long lastVoteMillis, Clock clock, ZoneId zone) {
        if (!waitUntilVoteDelay || lastVoteMillis == 0L) return true;
        if (lastVoteMillis < 0L) return false;

        LocalDateTime last = LocalDateTime.ofInstant(Instant.ofEpochMilli(lastVoteMillis), zone);
        LocalDateTime now = LocalDateTime.ofInstant(clock.instant(), zone);
        if (!daily) {
            return delayMillis > 0L && now.isAfter(last.plus(Duration.ofMillis(delayMillis)));
        }
        LocalDateTime reset = last.toLocalDate().atTime(LocalTime.of(dailyHour, 0));
        return now.isAfter(last.isBefore(reset) ? reset : reset.plusDays(1));
    }
}
