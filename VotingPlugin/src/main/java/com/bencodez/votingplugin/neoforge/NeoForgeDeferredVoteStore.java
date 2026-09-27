package com.bencodez.votingplugin.neoforge;

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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

/** Ordered, bounded storage for complete votes and their completion receipts. */
public final class NeoForgeDeferredVoteStore {
    static final String DEFERRED_VOTES = "DeferredVotes";
    static final String COMPLETED_DEFERRED_VOTES = "CompletedDeferredVotes";
    static final int MAX_DEFERRED_PER_USER = 64;
    static final int MAX_DEFERRED_TOTAL = 4_096;
    static final int MAX_COMPLETED_PER_USER = 4_096;
    static final int MAX_COMPLETED_TOTAL = 262_144;
    private static final String VERSION = "v1";
    private static final Base64.Encoder ENCODER = Base64.getUrlEncoder().withoutPadding();
    private static final Base64.Decoder DECODER = Base64.getUrlDecoder();

    private final SqlUserBackend backend;
    private final int perUserLimit;
    private final int totalLimit;
    private final int completedPerUserLimit;
    private final int completedTotalLimit;
    private final Set<OccurrenceKey> activeClaims = new HashSet<>();
    private final Map<UUID, Integer> receiptReservationsByUser = new HashMap<>();
    private int receiptReservationsTotal;
    private int retainedCount = -1;
    private int completedCount = -1;

    NeoForgeDeferredVoteStore(SqlUserBackend backend) {
        this(backend, MAX_DEFERRED_PER_USER, MAX_DEFERRED_TOTAL,
                MAX_COMPLETED_PER_USER, MAX_COMPLETED_TOTAL);
    }

    NeoForgeDeferredVoteStore(SqlUserBackend backend, int perUserLimit, int totalLimit) {
        this(backend, perUserLimit, totalLimit,
                MAX_COMPLETED_PER_USER, MAX_COMPLETED_TOTAL);
    }

    NeoForgeDeferredVoteStore(SqlUserBackend backend, int perUserLimit, int totalLimit,
            int completedPerUserLimit, int completedTotalLimit) {
        this.backend = Objects.requireNonNull(backend, "backend");
        if (perUserLimit <= 0 || totalLimit <= 0
                || completedPerUserLimit <= 0 || completedTotalLimit <= 0) {
            throw new IllegalArgumentException("limits must be positive");
        }
        this.perUserLimit = perUserLimit;
        this.totalLimit = totalLimit;
        this.completedPerUserLimit = completedPerUserLimit;
        this.completedTotalLimit = completedTotalLimit;
    }

    static List<UserDataKey> storageKeys() {
        return List.of(new UserDataKeyString(DEFERRED_VOTES).setColumnType("MEDIUMTEXT"),
                new UserDataKeyString(COMPLETED_DEFERRED_VOTES).setColumnType("MEDIUMTEXT"));
    }

    synchronized DeferralResult defer(SharedVoteIdentity identity, SharedVoteInput input, NeoForgeVoteSite site) {
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(site, "site");
        NeoForgeDeferredVote vote = new NeoForgeDeferredVote(input.voteId(), identity.uuid(), identity.playerName(),
                input.serviceSite(), site.key(), input.voteTime(), input.realVote(), input.addTotals(), identity.online());
        SqlUserStorage user = backend.user(identity.uuid());
        Map<String, DataValue> existingRow = row(user.readRow(backend.storageType()));
        List<CompletionReceipt> existingCompleted = parseCompleted(value(existingRow, COMPLETED_DEFERRED_VOTES));
        if (containsReceipt(existingCompleted, vote.voteId())) {
            reconcileCompleted(identity.uuid(), vote.voteId());
            return new DeferralResult(Status.ALREADY_COMPLETED, List.of());
        }
        List<NeoForgeDeferredVote> existingPending = parsePending(value(existingRow, DEFERRED_VOTES), identity.uuid());
        if (existingPending.stream().anyMatch(queued -> queued.voteId().equals(vote.voteId()))) {
            return new DeferralResult(Status.ALREADY_RETAINED, List.copyOf(existingPending));
        }
        ensureCounts();
        if (existingPending.size() >= perUserLimit || retainedCount >= totalLimit) {
            return new DeferralResult(Status.CAPACITY_REACHED, List.copyOf(existingPending));
        }
        Mutation<DeferralResult> mutation = user.transaction(backend.storageType(), Map.of(), scope -> {
            Map<String, DataValue> row = row(scope.readRow());
            List<CompletionReceipt> completed = parseCompleted(value(row, COMPLETED_DEFERRED_VOTES));
            if (containsReceipt(completed, vote.voteId())) {
                return new Mutation<>(new DeferralResult(Status.ALREADY_COMPLETED, List.of()), 0, 0);
            }
            List<NeoForgeDeferredVote> pending = parsePending(value(row, DEFERRED_VOTES), identity.uuid());
            if (pending.stream().anyMatch(queued -> queued.voteId().equals(vote.voteId()))) {
                return new Mutation<>(new DeferralResult(Status.ALREADY_RETAINED, List.copyOf(pending)), 0, 0);
            }
            if (pending.size() >= perUserLimit || retainedCount >= totalLimit) {
                return new Mutation<>(new DeferralResult(Status.CAPACITY_REACHED, List.copyOf(pending)), 0, 0);
            }
            pending.add(vote);
            scope.writeValues(Map.of(
                    NeoForgeVoteAccountingStore.PLAYER_NAME, new DataValueString(identity.playerName()),
                    DEFERRED_VOTES, new DataValueString(serializePending(pending)),
                    COMPLETED_DEFERRED_VOTES, new DataValueString(serializeCompleted(completed))));
            return new Mutation<>(new DeferralResult(Status.RETAINED, List.copyOf(pending)), 1, 0);
        });
        applyCounts(mutation);
        if (mutation.value().status() == Status.ALREADY_COMPLETED) {
            reconcileCompleted(identity.uuid(), vote.voteId());
        }
        return mutation.value();
    }

    /** Returns the retained votes in admission order for one player. */
    public synchronized List<NeoForgeDeferredVote> pending(UUID playerId) {
        Objects.requireNonNull(playerId, "playerId");
        return List.copyOf(readState(backend.user(playerId), playerId).pending());
    }

    /** Returns user rows that a replay service can inspect without parsing every queue here. */
    public synchronized List<UUID> users() {
        return List.copyOf(backend.enumerateUsers());
    }

    public synchronized OccurrenceState state(UUID playerId, UUID voteId) {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(voteId, "voteId");
        Map<String, DataValue> row = row(backend.user(playerId).readRow(backend.storageType()));
        if (containsReceipt(parseCompleted(value(row, COMPLETED_DEFERRED_VOTES)), voteId)) {
            reconcileCompleted(playerId, voteId);
            return OccurrenceState.COMPLETED;
        }
        return parsePending(value(row, DEFERRED_VOTES), playerId).stream()
                .anyMatch(vote -> vote.voteId().equals(voteId))
                ? OccurrenceState.PENDING : OccurrenceState.UNKNOWN;
    }

    /**
     * Claims one pending occurrence only when its completion receipt has capacity.
     * A process crash releases the in-memory reservation while leaving the durable
     * pending payload unchanged; the next claim checks durable capacity again.
     */
    public synchronized Optional<Claim> claim(UUID playerId, UUID voteId) {
        Objects.requireNonNull(playerId, "playerId");
        Objects.requireNonNull(voteId, "voteId");
        OccurrenceKey key = new OccurrenceKey(playerId, voteId);
        if (activeClaims.contains(key)) return Optional.empty();
        Map<String, DataValue> row = row(backend.user(playerId).readRow(backend.storageType()));
        List<CompletionReceipt> completed = parseCompleted(value(row, COMPLETED_DEFERRED_VOTES));
        if (containsReceipt(completed, voteId)) {
            reconcileCompleted(playerId, voteId);
            return Optional.empty();
        }
        Optional<NeoForgeDeferredVote> vote = parsePending(value(row, DEFERRED_VOTES), playerId).stream()
                .filter(candidate -> candidate.voteId().equals(voteId)).findFirst();
        if (vote.isEmpty()) return Optional.empty();
        ensureCounts();
        if (completed.size() + receiptReservationsByUser.getOrDefault(playerId, 0)
                >= completedPerUserLimit || completedCount + receiptReservationsTotal >= completedTotalLimit) {
            return Optional.empty();
        }
        activeClaims.add(key);
        receiptReservationsByUser.merge(playerId, 1, Integer::sum);
        receiptReservationsTotal++;
        return Optional.of(new Claim(key, vote.get()));
    }

    private synchronized CompletionResult complete(Claim claim) {
        Objects.requireNonNull(claim, "claim");
        if (claim.owner != this || claim.closed || !activeClaims.contains(claim.key)) {
            throw new IllegalStateException("Deferred vote claim is no longer active");
        }
        ensureCounts();
        Mutation<CompletionResult> mutation = backend.user(claim.key.playerId()).transaction(
                backend.storageType(), Map.of(), scope -> {
                    Map<String, DataValue> row = row(scope.readRow());
                    List<CompletionReceipt> completed = parseCompleted(value(row, COMPLETED_DEFERRED_VOTES));
                    if (containsReceipt(completed, claim.key.voteId())) {
                        List<NeoForgeDeferredVote> pending;
                        try {
                            pending = parsePending(value(row, DEFERRED_VOTES), claim.key.playerId());
                        } catch (MalformedDeferredVoteData malformedPending) {
                            return new Mutation<>(CompletionResult.ALREADY_COMPLETED, 0, 0);
                        }
                        int removed = removePending(pending, claim.key.voteId());
                        if (removed > 0) {
                            scope.writeValues(Map.of(DEFERRED_VOTES, new DataValueString(serializePending(pending))));
                        }
                        return new Mutation<>(CompletionResult.ALREADY_COMPLETED, -removed, 0);
                    }
                    List<NeoForgeDeferredVote> pending = parsePending(value(row, DEFERRED_VOTES), claim.key.playerId());
                    boolean isPending = pending.stream().anyMatch(vote -> vote.voteId().equals(claim.key.voteId()));
                    if (!isPending) {
                        return new Mutation<>(CompletionResult.NOT_PENDING, 0, 0);
                    }
                    if (completed.size() >= completedPerUserLimit
                            || completedCount >= completedTotalLimit) {
                        return new Mutation<>(CompletionResult.RECEIPT_CAPACITY_REACHED, 0, 0);
                    }
                    completed.add(new CompletionReceipt(claim.key.voteId()));
                    int removed = removePending(pending, claim.key.voteId());
                    scope.writeValues(Map.of(DEFERRED_VOTES, new DataValueString(serializePending(pending)),
                            COMPLETED_DEFERRED_VOTES, new DataValueString(serializeCompleted(completed))));
                    return new Mutation<>(CompletionResult.COMPLETED, -removed, 1);
                });
        applyCounts(mutation);
        if (mutation.value() != CompletionResult.RECEIPT_CAPACITY_REACHED) {
            releaseReservation(claim);
        }
        return mutation.value();
    }

    private void releaseReservation(Claim claim) {
        if (!claim.receiptReserved) return;
        claim.receiptReserved = false;
        receiptReservationsTotal--;
        receiptReservationsByUser.compute(claim.key.playerId(), (playerId, reserved) ->
                reserved == 1 ? null : reserved - 1);
    }

    private void reconcileCompleted(UUID playerId, UUID voteId) {
        int removed = backend.user(playerId).transaction(backend.storageType(), Map.of(), scope -> {
            Map<String, DataValue> row = row(scope.readRow());
            List<CompletionReceipt> completed = parseCompleted(value(row, COMPLETED_DEFERRED_VOTES));
            if (!containsReceipt(completed, voteId)) return 0;
            List<NeoForgeDeferredVote> pending;
            try {
                pending = parsePending(value(row, DEFERRED_VOTES), playerId);
            } catch (MalformedDeferredVoteData malformedPending) {
                return 0;
            }
            int removedPending = removePending(pending, voteId);
            if (removedPending > 0) {
                scope.writeValues(Map.of(DEFERRED_VOTES, new DataValueString(serializePending(pending))));
            }
            return removedPending;
        });
        if (removed > 0) {
            if (retainedCount >= 0) retainedCount = Math.max(0, retainedCount - removed);
            else retainedCount = -1;
        }
    }

    private static int removePending(List<NeoForgeDeferredVote> pending, UUID voteId) {
        int before = pending.size();
        pending.removeIf(vote -> vote.voteId().equals(voteId));
        return before - pending.size();
    }

    private void applyCounts(Mutation<?> mutation) {
        retainedCount = Math.max(0, retainedCount + mutation.pendingDelta());
        completedCount = Math.max(0, completedCount + mutation.completedDelta());
    }

    private void ensureCounts() {
        if (retainedCount >= 0 && completedCount >= 0) return;
        int pending = 0;
        int completed = 0;
        for (UUID playerId : backend.enumerateUsers()) {
            Map<String, DataValue> row = row(backend.user(playerId).readRow(backend.storageType()));
            try {
                pending = cappedAdd(pending, parsePending(value(row, DEFERRED_VOTES), playerId).size(), totalLimit);
            } catch (MalformedDeferredVoteData malformedPending) {
                pending = cappedAdd(pending, perUserLimit, totalLimit);
            }
            try {
                completed = cappedAdd(completed,
                        parseCompleted(value(row, COMPLETED_DEFERRED_VOTES)).size(), completedTotalLimit);
            } catch (MalformedDeferredVoteData malformedReceipts) {
                completed = cappedAdd(completed, completedPerUserLimit, completedTotalLimit);
            }
        }
        retainedCount = pending;
        completedCount = completed;
    }

    private static int cappedAdd(int current, int added, int limit) {
        return (int) Math.min(limit, (long) current + added);
    }

    private StoredState readState(SqlUserStorage user, UUID playerId) {
        List<Column> columns = user.readRow(backend.storageType());
        if (columns.isEmpty()) return new StoredState(new ArrayList<>(), new ArrayList<>());
        Map<String, DataValue> row = row(columns);
        return new StoredState(parsePending(value(row, DEFERRED_VOTES), playerId),
                parseCompleted(value(row, COMPLETED_DEFERRED_VOTES)));
    }

    private static String serializePending(List<NeoForgeDeferredVote> votes) {
        ArrayList<String> lines = new ArrayList<>(votes.size());
        for (NeoForgeDeferredVote vote : votes) {
            lines.add(String.join("|", VERSION, vote.voteId().toString(), encode(vote.playerName()),
                    encode(vote.serviceSite()), encode(vote.siteKey()), Long.toString(vote.voteTime()),
                    Boolean.toString(vote.realVote()), Boolean.toString(vote.addTotals()),
                    Boolean.toString(vote.wasOnline())));
        }
        return String.join("\n", lines);
    }

    private static List<NeoForgeDeferredVote> parsePending(String stored, UUID playerId) {
        ArrayList<NeoForgeDeferredVote> votes = new ArrayList<>();
        if (stored == null || stored.isEmpty()) return votes;
        for (String line : stored.split("\\n", -1)) {
            String[] fields = line.split("\\|", -1);
            if (fields.length != 9 || !VERSION.equals(fields[0])) {
                throw new MalformedDeferredVoteData("Unsupported or malformed deferred NeoForge vote data");
            }
            try {
                votes.add(new NeoForgeDeferredVote(UUID.fromString(fields[1]), playerId,
                        decode(fields[2]), decode(fields[3]), decode(fields[4]), Long.parseLong(fields[5]),
                        parseBoolean(fields[6]), parseBoolean(fields[7]), parseBoolean(fields[8])));
            } catch (IllegalArgumentException failure) {
                throw new MalformedDeferredVoteData("Malformed deferred NeoForge vote data", failure);
            }
        }
        return votes;
    }

    private static String serializeCompleted(List<CompletionReceipt> receipts) {
        ArrayList<String> lines = new ArrayList<>(receipts.size());
        for (CompletionReceipt receipt : receipts) {
            lines.add(String.join("|", VERSION, receipt.voteId().toString()));
        }
        return String.join("\n", lines);
    }

    private static List<CompletionReceipt> parseCompleted(String stored) {
        ArrayList<CompletionReceipt> receipts = new ArrayList<>();
        if (stored == null || stored.isEmpty()) return receipts;
        for (String line : stored.split("\\n", -1)) {
            String[] fields = line.split("\\|", -1);
            if (fields.length != 2 || !VERSION.equals(fields[0])) {
                throw new MalformedDeferredVoteData("Unsupported or malformed completed NeoForge vote data");
            }
            try {
                receipts.add(new CompletionReceipt(UUID.fromString(fields[1])));
            } catch (IllegalArgumentException failure) {
                throw new MalformedDeferredVoteData("Malformed completed NeoForge vote data", failure);
            }
        }
        return receipts;
    }

    private static boolean containsReceipt(List<CompletionReceipt> receipts, UUID voteId) {
        return receipts.stream().anyMatch(receipt -> receipt.voteId().equals(voteId));
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
        byte[] bytes = DECODER.decode(value);
        try {
            return StandardCharsets.UTF_8.newDecoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .decode(ByteBuffer.wrap(bytes)).toString();
        } catch (CharacterCodingException failure) {
            throw new IllegalArgumentException("Invalid UTF-8 field", failure);
        }
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

    public enum OccurrenceState { UNKNOWN, PENDING, COMPLETED }
    enum Status { RETAINED, ALREADY_RETAINED, ALREADY_COMPLETED, CAPACITY_REACHED }
    public enum CompletionResult { COMPLETED, ALREADY_COMPLETED, NOT_PENDING, RECEIPT_CAPACITY_REACHED }
    record DeferralResult(Status status, List<NeoForgeDeferredVote> pending) { }
    private record CompletionReceipt(UUID voteId) { }
    private record StoredState(List<NeoForgeDeferredVote> pending, List<CompletionReceipt> completed) { }
    private record OccurrenceKey(UUID playerId, UUID voteId) { }
    private record Mutation<T>(T value, int pendingDelta, int completedDelta) { }

    public final class Claim implements AutoCloseable {
        private final NeoForgeDeferredVoteStore owner = NeoForgeDeferredVoteStore.this;
        private final OccurrenceKey key;
        private final NeoForgeDeferredVote vote;
        private boolean receiptReserved = true;
        private boolean closed;

        private Claim(OccurrenceKey key, NeoForgeDeferredVote vote) {
            this.key = key;
            this.vote = vote;
        }

        public NeoForgeDeferredVote vote() { return vote; }
        public CompletionResult complete() { return owner.complete(this); }

        @Override public void close() {
            synchronized (owner) {
                if (closed) return;
                closed = true;
                owner.releaseReservation(this);
                owner.activeClaims.remove(key);
            }
        }
    }

    private static final class MalformedDeferredVoteData extends IllegalStateException {
        private static final long serialVersionUID = 1L;
        private MalformedDeferredVoteData(String message) { super(message); }
        private MalformedDeferredVoteData(String message, Throwable cause) { super(message, cause); }
    }
}
