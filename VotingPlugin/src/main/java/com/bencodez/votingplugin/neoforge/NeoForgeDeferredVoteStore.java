package com.bencodez.votingplugin.neoforge;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKey;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.advancedcore.core.user.storage.SqlUserStorage;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;

/** Ordered, bounded storage for complete votes awaiting NeoForge feature parity. */
public final class NeoForgeDeferredVoteStore {
    static final String DEFERRED_VOTES = "DeferredVotes";
    static final int MAX_DEFERRED_PER_USER = 64;
    static final int MAX_DEFERRED_TOTAL = 4_096;
    private static final String VERSION = "v1";
    private static final Base64.Encoder ENCODER = Base64.getUrlEncoder().withoutPadding();
    private static final Base64.Decoder DECODER = Base64.getUrlDecoder();

    private final SqlUserBackend backend;
    private final int perUserLimit;
    private final int totalLimit;
    private int retainedCount = -1;

    NeoForgeDeferredVoteStore(SqlUserBackend backend) {
        this(backend, MAX_DEFERRED_PER_USER, MAX_DEFERRED_TOTAL);
    }

    NeoForgeDeferredVoteStore(SqlUserBackend backend, int perUserLimit, int totalLimit) {
        this.backend = Objects.requireNonNull(backend, "backend");
        if (perUserLimit <= 0 || totalLimit <= 0) throw new IllegalArgumentException("limits must be positive");
        this.perUserLimit = perUserLimit;
        this.totalLimit = totalLimit;
    }

    static UserDataKey storageKey() {
        return new UserDataKeyString(DEFERRED_VOTES).setColumnType("MEDIUMTEXT");
    }

    synchronized DeferralResult defer(SharedVoteIdentity identity, SharedVoteInput input, NeoForgeVoteSite site) {
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(site, "site");
        NeoForgeDeferredVote vote = new NeoForgeDeferredVote(input.voteId(), identity.uuid(), identity.playerName(),
                input.serviceSite(), site.key(), input.voteTime(), input.realVote(), input.addTotals(), identity.online());
        SqlUserStorage user = backend.user(identity.uuid());
        List<NeoForgeDeferredVote> existing = readPending(user, identity.uuid());
        if (existing.stream().anyMatch(item -> item.voteId().equals(vote.voteId()))) {
            return new DeferralResult(Status.ALREADY_RETAINED, List.copyOf(existing));
        }
        ensureRetainedCount();
        if (existing.size() >= perUserLimit || retainedCount >= totalLimit) {
            return new DeferralResult(Status.CAPACITY_REACHED, List.copyOf(existing));
        }
        DeferralResult result = user.transaction(backend.storageType(), Map.of(), scope -> {
            Map<String, DataValue> row = row(scope.readRow());
            List<NeoForgeDeferredVote> pending = parse(value(row, DEFERRED_VOTES), identity.uuid());
            if (pending.stream().anyMatch(queued -> queued.voteId().equals(vote.voteId()))) {
                return new DeferralResult(Status.ALREADY_RETAINED, List.copyOf(pending));
            }
            if (pending.size() >= perUserLimit) {
                return new DeferralResult(Status.CAPACITY_REACHED, List.copyOf(pending));
            }
            pending.add(vote);
            scope.writeValues(Map.of(
                    NeoForgeVoteAccountingStore.PLAYER_NAME, new DataValueString(identity.playerName()),
                    DEFERRED_VOTES, new DataValueString(serialize(pending))));
            return new DeferralResult(Status.RETAINED, List.copyOf(pending));
        });
        if (result.status() == Status.RETAINED) retainedCount++;
        return result;
    }

    /** Returns the retained votes in admission order. */
    public synchronized List<NeoForgeDeferredVote> pending(UUID playerId) {
        Objects.requireNonNull(playerId, "playerId");
        return List.copyOf(readPending(backend.user(playerId), playerId));
    }

    synchronized boolean contains(UUID playerId, UUID voteId) {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(voteId, "voteId");
        return readPending(backend.user(playerId), playerId).stream()
                .anyMatch(vote -> vote.voteId().equals(voteId));
    }

    /** Removes one retained vote after a future processor has completed every required effect. */
    public synchronized boolean complete(UUID playerId, UUID voteId) {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(voteId, "voteId");
        ensureRetainedCount();
        SqlUserStorage user = backend.user(playerId);
        boolean removed = user.transaction(backend.storageType(), Map.of(), scope -> {
            List<NeoForgeDeferredVote> pending = parse(value(row(scope.readRow()), DEFERRED_VOTES), playerId);
            boolean matched = pending.removeIf(vote -> vote.voteId().equals(voteId));
            if (matched) scope.writeValues(Map.of(DEFERRED_VOTES, new DataValueString(serialize(pending))));
            return matched;
        });
        if (removed) retainedCount--;
        return removed;
    }

    private int countAllPending() {
        int count = 0;
        for (UUID playerId : backend.enumerateUsers()) {
            count += readPending(backend.user(playerId), playerId).size();
        }
        return count;
    }

    private void ensureRetainedCount() {
        if (retainedCount < 0) retainedCount = countAllPending();
    }

    private List<NeoForgeDeferredVote> readPending(SqlUserStorage user, UUID playerId) {
        List<Column> columns = user.readRow(backend.storageType());
        if (columns.isEmpty()) return List.of();
        return parse(value(row(columns), DEFERRED_VOTES), playerId);
    }

    private static String serialize(List<NeoForgeDeferredVote> votes) {
        ArrayList<String> lines = new ArrayList<>(votes.size());
        for (NeoForgeDeferredVote vote : votes) {
            lines.add(String.join("|", VERSION, vote.voteId().toString(), encode(vote.playerName()),
                    encode(vote.serviceSite()), encode(vote.siteKey()), Long.toString(vote.voteTime()),
                    Boolean.toString(vote.realVote()), Boolean.toString(vote.addTotals()),
                    Boolean.toString(vote.wasOnline())));
        }
        return String.join("\n", lines);
    }

    private static List<NeoForgeDeferredVote> parse(String stored, UUID playerId) {
        ArrayList<NeoForgeDeferredVote> votes = new ArrayList<>();
        if (stored == null || stored.isEmpty()) return votes;
        for (String line : stored.split("\\n", -1)) {
            String[] fields = line.split("\\|", -1);
            if (fields.length != 9 || !VERSION.equals(fields[0])) {
                throw new IllegalStateException("Unsupported or malformed deferred NeoForge vote data");
            }
            try {
                votes.add(new NeoForgeDeferredVote(UUID.fromString(fields[1]), playerId,
                        decode(fields[2]), decode(fields[3]), decode(fields[4]), Long.parseLong(fields[5]),
                        parseBoolean(fields[6]), parseBoolean(fields[7]), parseBoolean(fields[8])));
            } catch (IllegalArgumentException failure) {
                throw new IllegalStateException("Malformed deferred NeoForge vote data", failure);
            }
        }
        return votes;
    }

    private static boolean parseBoolean(String value) {
        if ("true".equals(value)) return true;
        if ("false".equals(value)) return false;
        throw new IllegalArgumentException("Invalid boolean");
    }

    private static String encode(String value) {
        return ENCODER.encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }

    private static String decode(String value) {
        return new String(DECODER.decode(value), StandardCharsets.UTF_8);
    }

    private static Map<String, DataValue> row(List<Column> columns) {
        HashMap<String, DataValue> values = new HashMap<>();
        for (Column column : columns) values.put(column.getName().toLowerCase(Locale.ROOT), column.getValue());
        return values;
    }

    private static String value(Map<String, DataValue> row, String key) {
        DataValue value = row.get(key.toLowerCase(Locale.ROOT));
        return value == null ? "" : value.getString();
    }

    enum Status { RETAINED, ALREADY_RETAINED, CAPACITY_REACHED }
    record DeferralResult(Status status, List<NeoForgeDeferredVote> pending) { }
}
