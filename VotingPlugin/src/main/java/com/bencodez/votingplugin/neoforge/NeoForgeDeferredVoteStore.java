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
    private static final String VERSION = "v1";
    private static final Base64.Encoder ENCODER = Base64.getUrlEncoder().withoutPadding();
    private static final Base64.Decoder DECODER = Base64.getUrlDecoder();

    private final SqlUserBackend backend;

    NeoForgeDeferredVoteStore(SqlUserBackend backend) {
        this.backend = Objects.requireNonNull(backend, "backend");
    }

    static UserDataKey storageKey() {
        return new UserDataKeyString(DEFERRED_VOTES).setColumnType("MEDIUMTEXT");
    }

    DeferralResult defer(SharedVoteIdentity identity, SharedVoteInput input, NeoForgeVoteSite site) {
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(site, "site");
        NeoForgeDeferredVote vote = new NeoForgeDeferredVote(input.voteId(), identity.uuid(), identity.playerName(),
                input.serviceSite(), site.key(), input.voteTime(), input.realVote(), input.addTotals(), identity.online());
        SqlUserStorage user = backend.user(identity.uuid());
        return user.transaction(backend.storageType(),
                Map.of(NeoForgeVoteAccountingStore.PLAYER_NAME, new DataValueString(identity.playerName())), scope -> {
                    Map<String, DataValue> row = row(scope.readRow());
                    List<NeoForgeDeferredVote> pending = parse(value(row, DEFERRED_VOTES), identity.uuid());
                    if (pending.stream().anyMatch(existing -> existing.voteId().equals(vote.voteId()))) {
                        return new DeferralResult(Status.ALREADY_RETAINED, List.copyOf(pending));
                    }
                    if (pending.size() >= MAX_DEFERRED_PER_USER) {
                        return new DeferralResult(Status.CAPACITY_REACHED, List.copyOf(pending));
                    }
                    pending.add(vote);
                    scope.writeValues(Map.of(
                            NeoForgeVoteAccountingStore.PLAYER_NAME, new DataValueString(identity.playerName()),
                            DEFERRED_VOTES, new DataValueString(serialize(pending))));
                    return new DeferralResult(Status.RETAINED, List.copyOf(pending));
                });
    }

    /** Returns the retained votes in admission order. */
    public List<NeoForgeDeferredVote> pending(UUID playerId) {
        Objects.requireNonNull(playerId, "playerId");
        List<Column> columns = backend.user(playerId).readRow(backend.storageType());
        if (columns.isEmpty()) return List.of();
        return List.copyOf(parse(value(row(columns), DEFERRED_VOTES), playerId));
    }

    /** Removes one retained vote after a future processor has completed every required effect. */
    public boolean complete(UUID playerId, UUID voteId) {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(voteId, "voteId");
        SqlUserStorage user = backend.user(playerId);
        return user.transaction(backend.storageType(), Map.of(), scope -> {
            List<NeoForgeDeferredVote> pending = parse(value(row(scope.readRow()), DEFERRED_VOTES), playerId);
            boolean removed = pending.removeIf(vote -> vote.voteId().equals(voteId));
            if (removed) scope.writeValues(Map.of(DEFERRED_VOTES, new DataValueString(serialize(pending))));
            return removed;
        });
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
