package com.bencodez.votingplugin.neoforge;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKey;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyInt;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.advancedcore.core.user.storage.SqlUserStorage;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.core.vote.SharedVoteAccounting;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;

/**
 * Blocking persistence adapter for the accounting subset of an accepted native
 * vote. Callers must run these operations away from the server or region thread.
 */
public final class NeoForgeVoteAccountingStore {
    static final String PLAYER_NAME = "PlayerName";
    static final String ALL_TIME_TOTAL = "AllTimeTotal";
    static final String MONTH_TOTAL = "MonthTotal";
    static final String DAILY_TOTAL = "DailyTotal";
    static final String WEEKLY_TOTAL = "WeeklyTotal";
    static final String POINTS = "Points";
    static final String LAST_VOTES = "LastVotes";

    private final SqlUserBackend backend;
    private final NeoForgeVoteConfiguration configuration;

    NeoForgeVoteAccountingStore(SqlUserBackend backend, NeoForgeVoteConfiguration configuration) {
        this.backend = Objects.requireNonNull(backend, "backend");
        this.configuration = Objects.requireNonNull(configuration, "configuration");
    }

    static List<UserDataKey> storageKeys() {
        return List.of(new UserDataKeyString(PLAYER_NAME).setColumnType("VARCHAR(30)"),
                new UserDataKeyInt(ALL_TIME_TOTAL), new UserDataKeyInt(MONTH_TOTAL),
                new UserDataKeyInt(DAILY_TOTAL), new UserDataKeyInt(WEEKLY_TOTAL),
                new UserDataKeyInt(POINTS), new UserDataKeyString(LAST_VOTES));
    }

    /**
     * Persists the accepted vote timestamp, current name, and shared accounting
     * decision in one user-row transaction.
     */
    public NeoForgeVoteAccount apply(SharedVoteIdentity identity, SharedVoteInput input,
            SharedVotePolicy policy, NeoForgeVoteSite site, int pointsOnVote, int pointLimit) {
        return apply(identity, input, policy, site, pointsOnVote, pointLimit, votes -> true).account();
    }

    AccountingResult applyIfVoteDelayAllows(SharedVoteIdentity identity, SharedVoteInput input,
            SharedVotePolicy policy, NeoForgeVoteSite site, int pointsOnVote, int pointLimit,
            LocalDateTime currentTime, ZoneId storedTimestampZone, int hourOffset) {
        Objects.requireNonNull(currentTime, "currentTime");
        Objects.requireNonNull(storedTimestampZone, "storedTimestampZone");
        return apply(identity, input, policy, site, pointsOnVote, pointLimit, votes -> {
            long lastVote = votes.entrySet().stream()
                    .filter(entry -> entry.getKey().equalsIgnoreCase(site.key()))
                    .mapToLong(Map.Entry::getValue).findFirst().orElse(0L);
            return !site.waitUntilVoteDelay() || !input.realVote()
                    || site.canVote(lastVote, currentTime, storedTimestampZone, hourOffset);
        });
    }

    private AccountingResult apply(SharedVoteIdentity identity, SharedVoteInput input,
            SharedVotePolicy policy, NeoForgeVoteSite site, int pointsOnVote, int pointLimit,
            DelayCheck delayCheck) {
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(policy, "policy");
        Objects.requireNonNull(site, "site");
        Objects.requireNonNull(delayCheck, "delayCheck");

        AccountingDelta delta = new AccountingDelta();
        SharedVoteAccounting.apply(input, policy, identity::online,
                delta::addTotal, delta::addDaily, delta::addWeekly,
                () -> delta.addPoints(pointsOnVote));

        SqlUserStorage user = backend.user(identity.uuid());
        return user.transaction(backend.storageType(),
                Map.of(PLAYER_NAME, new DataValueString(identity.playerName())), scope -> {
                    Row current = Row.from(scope.readRow());
                    LinkedHashMap<String, Long> lastVotes = parseLastVotes(current.string(LAST_VOTES));
                    if (!delayCheck.allows(lastVotes)) return AccountingResult.delayed();
                    replaceLastVote(lastVotes, site.key(), input.voteTime());

                    int allTimeTotal = current.integer(ALL_TIME_TOTAL) + delta.total;
                    int monthTotal = current.integer(MONTH_TOTAL) + delta.total;
                    int dailyTotal = current.integer(DAILY_TOTAL) + delta.daily;
                    int weeklyTotal = current.integer(WEEKLY_TOTAL) + delta.weekly;
                    int points = current.integer(POINTS) + delta.points;
                    if (delta.pointsApplied && pointLimit > 0 && points > pointLimit) points = pointLimit;

                    HashMap<String, DataValue> updates = new HashMap<>();
                    updates.put(PLAYER_NAME, new DataValueString(identity.playerName()));
                    updates.put(ALL_TIME_TOTAL, new DataValueInt(allTimeTotal));
                    updates.put(MONTH_TOTAL, new DataValueInt(monthTotal));
                    updates.put(DAILY_TOTAL, new DataValueInt(dailyTotal));
                    updates.put(WEEKLY_TOTAL, new DataValueInt(weeklyTotal));
                    updates.put(POINTS, new DataValueInt(points));
                    updates.put(LAST_VOTES, new DataValueString(serializeLastVotes(lastVotes)));
                    scope.writeValues(updates);
                    return AccountingResult.accepted(new NeoForgeVoteAccount(identity.uuid(), identity.playerName(),
                            allTimeTotal, monthTotal, dailyTotal, weeklyTotal, points, lastVotes));
                });
    }

    public Optional<NeoForgeVoteAccount> load(UUID uuid) {
        Objects.requireNonNull(uuid, "uuid");
        List<Column> columns = backend.user(uuid).readRow(backend.storageType());
        if (columns.isEmpty()) return Optional.empty();
        Row row = Row.from(columns);
        return Optional.of(new NeoForgeVoteAccount(uuid, row.string(PLAYER_NAME),
                row.integer(ALL_TIME_TOTAL), row.integer(MONTH_TOTAL), row.integer(DAILY_TOTAL),
                row.integer(WEEKLY_TOTAL), row.integer(POINTS), parseLastVotes(row.string(LAST_VOTES))));
    }

    private static void replaceLastVote(LinkedHashMap<String, Long> votes, String siteKey, long voteTime) {
        String oldKey = votes.keySet().stream().filter(key -> key.equalsIgnoreCase(siteKey)).findFirst().orElse(null);
        if (oldKey != null) votes.remove(oldKey);
        votes.put(siteKey, voteTime);
    }

    private LinkedHashMap<String, Long> parseLastVotes(String stored) {
        LinkedHashMap<String, Long> votes = new LinkedHashMap<>();
        if (stored == null || stored.isEmpty()) return votes;
        for (String value : stored.split("%line%")) {
            String[] parts = value.split("//");
            if (parts.length < 2 || parts[0].isEmpty() || !configuration.hasEnabledSiteKey(parts[0])) continue;
            try {
                votes.put(parts[0], Long.parseLong(parts[1]));
            } catch (NumberFormatException ignored) {
                // Bukkit retains a configured site with a zero timestamp when its
                // stored timestamp is malformed.
                votes.put(parts[0], 0L);
            }
        }
        return votes;
    }

    private static String serializeLastVotes(Map<String, Long> votes) {
        List<String> values = new ArrayList<>(votes.size());
        votes.forEach((key, value) -> values.add(key + "//" + value));
        return String.join("%line%", values);
    }

    private static final class AccountingDelta {
        int total;
        int daily;
        int weekly;
        int points;
        boolean pointsApplied;
        void addTotal() { total++; }
        void addDaily() { daily++; }
        void addWeekly() { weekly++; }
        void addPoints(int amount) { points += amount; pointsApplied = true; }
    }

    private record Row(Map<String, DataValue> values) {
        static Row from(List<Column> columns) {
            Map<String, DataValue> values = new HashMap<>();
            for (Column column : columns) values.put(column.getName().toLowerCase(Locale.ROOT), column.getValue());
            return new Row(values);
        }
        int integer(String key) {
            DataValue value = values.get(key.toLowerCase(Locale.ROOT));
            return value == null ? 0 : value.getInt();
        }
        String string(String key) {
            DataValue value = values.get(key.toLowerCase(Locale.ROOT));
            return value == null ? "" : value.getString();
        }
    }

    record AccountingResult(boolean accepted, NeoForgeVoteAccount account) {
        static AccountingResult accepted(NeoForgeVoteAccount account) {
            return new AccountingResult(true, Objects.requireNonNull(account, "account"));
        }
        static AccountingResult delayed() { return new AccountingResult(false, null); }
    }

    @FunctionalInterface
    private interface DelayCheck {
        boolean allows(Map<String, Long> lastVotes);
    }
}
