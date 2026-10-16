package com.bencodez.votingplugin.topvoter;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.LongSupplier;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import org.bukkit.event.Listener;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.player.PlayerJoinEvent;

/** Bounded display snapshots only; raw rankings and award inputs stay untouched. */
public final class TopVoterDisplayFilter implements AutoCloseable, Listener {
    private static final int MAX_PENDING = 128;
    private static final int MAX_CACHED = 8192;
    private static final long MAX_AGE = java.util.concurrent.TimeUnit.SECONDS.toNanos(30);
    private record Identity(UUID uuid, String name) { }
    private static final class Evidence {
        private PlayerPresence value = PlayerPresence.UNKNOWN;
        private long completed;
        private boolean pending = true;
        private Set<UUID> identities = Set.of();
        private long joinGeneration;
        private TopVoterPlayer snapshot;
        private List<UUID> aliases;
        private final CompletableFuture<Void> settled = new CompletableFuture<>();
    }
    private final Function<TopVoterPlayer, List<UUID>> identities;
    private final BiFunction<TopVoterPlayer, List<UUID>, CompletionStage<PlayerPresence>> lookup;
    private final LongSupplier clock;
    private final LinkedHashMap<Identity, Evidence> cache = new LinkedHashMap<>(16, 0.75f, true);
    private int pending;
    private boolean closed;
    private long joinGeneration;

    public TopVoterDisplayFilter(Function<TopVoterPlayer, CompletionStage<PlayerPresence>> lookup) {
        this(lookup, System::nanoTime);
    }

    public TopVoterDisplayFilter(Function<TopVoterPlayer, CompletionStage<PlayerPresence>> lookup, LongSupplier clock) {
        this(player -> List.of(player.getUuid()), (player, aliases) -> lookup.apply(player), clock);
        Objects.requireNonNull(lookup);
    }

    /** Check the captured cache-only scope asynchronously; revalidate it before using negative evidence. */
    public TopVoterDisplayFilter(Function<TopVoterPlayer, List<UUID>> identities,
            BiFunction<TopVoterPlayer, List<UUID>, CompletionStage<PlayerPresence>> lookup) {
        this(identities, lookup, System::nanoTime);
    }

    public TopVoterDisplayFilter(Function<TopVoterPlayer, List<UUID>> identities,
            BiFunction<TopVoterPlayer, List<UUID>, CompletionStage<PlayerPresence>> lookup, LongSupplier clock) {
        this.identities = Objects.requireNonNull(identities);
        this.lookup = Objects.requireNonNull(lookup);
        this.clock = Objects.requireNonNull(clock);
    }

    /** Pending, failed, missing or stale evidence remains visible until a checked result arrives. */
    public LinkedHashMap<TopVoterPlayer, Integer> project(Map<TopVoterPlayer, Integer> rankings, boolean enabled) {
        var visible = new LinkedHashMap<TopVoterPlayer, Integer>();
        for (var entry : rankings.entrySet()) {
            if (!enabled || evidence(entry.getKey()) != PlayerPresence.NEVER_JOINED)
                visible.put(entry.getKey(), entry.getValue());
        }
        return visible;
    }

    /** Await admitted checks without blocking their worker; unavailable rows stay visible. */
    public CompletionStage<LinkedHashMap<TopVoterPlayer, Integer>> projectAsync(
            Map<TopVoterPlayer, Integer> rankings, boolean enabled) {
        var snapshot = new LinkedHashMap<>(rankings);
        if (!enabled) return CompletableFuture.completedFuture(snapshot);
        project(snapshot, true);
        var captured = new LinkedHashMap<Identity, Evidence>();
        var waiting = new java.util.ArrayList<CompletableFuture<Void>>();
        synchronized (cache) {
            for (TopVoterPlayer player : snapshot.keySet()) {
                if (player == null || player.getUuid() == null) continue;
                Identity key = new Identity(player.getUuid(), player.getPlayerName());
                Evidence record = cache.get(key);
                if (record != null) {
                    captured.put(key, record);
                    if (record.pending) waiting.add(record.settled);
                }
            }
        }
        return CompletableFuture.allOf(waiting.toArray(CompletableFuture[]::new))
                .completeOnTimeout(null, 5, java.util.concurrent.TimeUnit.SECONDS).thenApply(ignored -> {
                    var visible = new LinkedHashMap<TopVoterPlayer, Integer>();
                    for (var entry : snapshot.entrySet()) {
                        TopVoterPlayer player = entry.getKey();
                        Identity key = player == null ? null : new Identity(player.getUuid(), player.getPlayerName());
                        Evidence record = captured.get(key);
                        if (record == null || cachedValue(key, record) != PlayerPresence.NEVER_JOINED)
                            visible.put(player, entry.getValue());
                    }
                    return visible;
                });
    }

    private PlayerPresence evidence(TopVoterPlayer player) {
        if (player == null || player.getUuid() == null) return PlayerPresence.UNKNOWN;
        Identity key = new Identity(player.getUuid(), player.getPlayerName());
        Evidence record;
        boolean reuse;
        synchronized (cache) {
            if (closed) return PlayerPresence.UNKNOWN;
            record = cache.get(key);
            reuse = record != null && (record.pending || clock.getAsLong() - record.completed < MAX_AGE);
            if (!reuse) {
                if (pending >= MAX_PENDING) return PlayerPresence.UNKNOWN;
                if (record != null) cache.remove(key);
                if (cache.size() >= MAX_CACHED) {
                    var oldest = cache.entrySet().iterator();
                    while (oldest.hasNext()) if (!oldest.next().getValue().pending) { oldest.remove(); break; }
                }
                if (cache.size() >= MAX_CACHED) return PlayerPresence.UNKNOWN;
                record = new Evidence(); cache.put(key, record); pending++;
                record.joinGeneration = joinGeneration;
                record.snapshot = new TopVoterPlayer(key.uuid(), key.name(), player.getLastVoteTime());
            }
        }
        if (reuse) return cachedValue(key, record);
        Evidence admitted = record;
        try {
            List<UUID> aliases = resolveScope(admitted.snapshot);
            if (aliases == null) {
                finish(key, admitted, PlayerPresence.UNKNOWN);
                return PlayerPresence.UNKNOWN;
            }
            var checked = new java.util.HashSet<UUID>();
            for (UUID alias : aliases) if (alias != null) checked.add(alias);
            boolean valid;
            synchronized (cache) {
                valid = !closed && cache.get(key) == admitted && admitted.joinGeneration == joinGeneration;
                if (valid) {
                    admitted.identities = Set.copyOf(checked);
                    admitted.aliases = aliases;
                } else cache.remove(key, admitted);
            }
            if (!valid) {
                finish(key, admitted, PlayerPresence.UNKNOWN);
                return PlayerPresence.UNKNOWN;
            }
            CompletionStage<PlayerPresence> request = lookup.apply(admitted.snapshot, aliases);
            if (request == null) finish(key, admitted, PlayerPresence.UNKNOWN);
            else request.whenComplete((result, failure) -> finish(key, admitted,
                    failure == null && result != null ? result : PlayerPresence.UNKNOWN));
        } catch (RuntimeException unavailable) {
            finish(key, admitted, PlayerPresence.UNKNOWN);
        }
        return cachedValue(key, admitted);
    }

    private List<UUID> resolveScope(TopVoterPlayer snapshot) {
        List<UUID> resolved = identities.apply(snapshot);
        if (resolved == null || resolved.isEmpty() || resolved.size() > 8) return null;
        // Preserve null uncertainty markers and detach the provider from mutable cache data.
        return java.util.Collections.unmodifiableList(new java.util.ArrayList<>(resolved));
    }

    private boolean matchesScope(Evidence record) {
        try {
            return record.aliases != null && record.aliases.equals(resolveScope(record.snapshot));
        } catch (RuntimeException unavailable) {
            return false;
        }
    }

    private PlayerPresence cachedValue(Identity key, Evidence record) {
        PlayerPresence value;
        synchronized (cache) {
            if (closed || cache.get(key) != record || record.pending
                    || clock.getAsLong() - record.completed >= MAX_AGE) return PlayerPresence.UNKNOWN;
            value = record.value;
        }
        // The resolver is cache-only, but must never run under the evidence-cache monitor.
        if (value == PlayerPresence.NEVER_JOINED && !matchesScope(record)) {
            synchronized (cache) { cache.remove(key, record); }
            return PlayerPresence.UNKNOWN;
        }
        synchronized (cache) {
            return !closed && cache.get(key) == record && !record.pending
                    && clock.getAsLong() - record.completed < MAX_AGE ? record.value : PlayerPresence.UNKNOWN;
        }
    }

    private void finish(Identity key, Evidence record, PlayerPresence result) {
        boolean currentScope = result != PlayerPresence.NEVER_JOINED || matchesScope(record);
        synchronized (cache) {
            if (!record.pending) return;
            record.pending = false; pending--;
            if (!closed && cache.get(key) == record) {
                if (!currentScope) cache.remove(key, record);
                else { record.value = result; record.completed = clock.getAsLong(); }
            }
        }
        // Completion may invoke projection callbacks, which resolve aliases outside this monitor.
        record.settled.complete(null);
    }

    /** A newly observed join revokes matching negative snapshots and pending stale completions. */
    public void joined(UUID uuid, String name) {
        var invalidated = new java.util.ArrayList<Evidence>();
        synchronized (cache) {
            joinGeneration++;
            cache.entrySet().removeIf(entry -> {
                boolean matches = Objects.equals(entry.getKey().uuid(), uuid)
                        || (uuid != null && entry.getValue().identities.contains(uuid))
                        || (name != null && name.equalsIgnoreCase(entry.getKey().name()));
                if (matches) invalidated.add(entry.getValue());
                return matches;
            });
        }
        for (Evidence record : invalidated) record.settled.complete(null);
    }

    @EventHandler(priority = EventPriority.LOWEST)
    public void onJoin(PlayerJoinEvent event) {
        joined(event.getPlayer().getUniqueId(), event.getPlayer().getName());
    }

    @Override public void close() {
        List<Evidence> retiring;
        synchronized (cache) {
            closed = true;
            retiring = new java.util.ArrayList<>(cache.values());
            cache.clear();
        }
        for (Evidence record : retiring) record.settled.complete(null);
    }
}
