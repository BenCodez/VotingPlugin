package com.bencodez.votingplugin.core.session;

import java.net.URI;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/** Ephemeral, observation-only progress. Never invokes vote or reward processing. */
public final class GuidedVoteSession {
    public enum Status { AWAITING, RECEIVED, UNAVAILABLE, SKIPPED }
    public record Site(String key, String name, String url, boolean eligible, long lastVote) { }
    public record Entry(Site site, Status status) { }
    public record View(List<Entry> entries, int index, boolean finished) {
        public Entry current() { return entries.isEmpty() ? null : entries.get(index); }
        public long received() { return entries.stream().filter(e -> e.status() == Status.RECEIVED).count(); }
        public boolean complete() { return !entries.isEmpty() && entries.stream().allMatch(e -> e.status() == Status.RECEIVED); }
    }
    private final long started;
    private final long observationOrder;
    private final Map<String, Site> candidates = new LinkedHashMap<>();
    private boolean candidatesBound;
    private final Map<String, Site> sites = new LinkedHashMap<>();
    private final Map<String, Long> received = new LinkedHashMap<>();
    private final java.util.Set<String> skipped = new java.util.HashSet<>();
    private final Map<UUID, String> occurrences = new LinkedHashMap<>();
    private final Map<UUID, ProxyVoteSequence.Fence> proxyFences = new LinkedHashMap<>();
    private final Map<UUID, String> pendingProxy = new LinkedHashMap<>();
    private boolean initialized;
    private boolean finished;
    private int index;

    public GuidedVoteSession(long started) { this(started, VoteObservationSequence.next()); }
    public GuidedVoteSession(long started, long observationOrder) { this.started = started; this.observationOrder = observationOrder; }
    public long started() { return started; }

    /** Bind visible identities on the player context before asynchronous eligibility sampling. */
    public synchronized void bindCandidates(List<Site> available) {
        if (candidatesBound) return;
        for (Site site : available) if (candidates.size() < 100) candidates.put(site.key(), site);
        candidatesBound = true;
    }
    public synchronized void refresh(List<Site> available) {
        bindCandidates(available);
        if (!initialized) {
            for (Site site : available) if (candidates.containsKey(site.key()) && (site.eligible() || site.lastVote() > started || received.containsKey(site.key())) && sites.size() < 100) sites.put(site.key(), site);
            // A temporarily hidden/removed site can be absent from the worker sample.
            // Keep its confirmed identity so visibility restoration can recover progress.
            for (Site candidate : candidates.values()) if (received.containsKey(candidate.key()))
                sites.putIfAbsent(candidate.key(), candidate);
            initialized = true;
        }
        Map<String, Site> current = new LinkedHashMap<>();
        for (Site site : available) current.put(site.key(), site);
        sites.replaceAll((key, old) -> {
            Site fresh = current.get(key);
            return fresh == null ? new Site(key, old.name(), old.url(), false, old.lastVote()) : fresh;
        });
    }

    public synchronized boolean candidate(String key) { return candidates.containsKey(key); }
    /** The accepted pipeline supplies stable IDs and its authoritative occurrence time. */
    public synchronized void accepted(String site, UUID occurrence, long time, boolean real, boolean cancelled) {
        if (real && !cancelled && occurrence != null && time > started && candidates.containsKey(site)
                && (!initialized || sites.containsKey(site)) && !received.containsKey(site)
                && !occurrences.containsKey(occurrence)) {
            received.put(site, time);
            occurrences.put(occurrence, site);
            // A credited vote may make the first asynchronous eligibility sample unavailable.
            // Its identity was already bound, so a later notification can restore that entry.
            if (initialized) sites.putIfAbsent(site, candidates.get(site));
        }
    }
    /** Fresh local ingress uses backend-local order; proxy occurrences require their source fence. */
    public synchronized void acceptedObserved(String site, UUID occurrence, long order) {
        if (order > observationOrder && occurrence != null && candidates.containsKey(site)
                && !received.containsKey(site) && !occurrences.containsKey(occurrence)) {
            received.put(site, order);
            occurrences.put(occurrence, site);
            if (initialized) sites.putIfAbsent(site, candidates.get(site));
        }
    }
    /** A negotiated source fence is fixed for the entire session, not advanced by status checks. */
    public synchronized void bindProxyFence(ProxyVoteSequence.Fence fence) {
        if (!proxyFences.containsKey(fence.incarnation()) && proxyFences.size() < 32) {
            proxyFences.put(fence.incarnation(), fence);
            var pending = new ArrayList<>(pendingProxy.entrySet());
            for (var receipt : pending) acceptedProxy(receipt.getValue(), receipt.getKey());
        }
    }
    public synchronized boolean hasProxyFence() { return !proxyFences.isEmpty(); }
    /** The normal credited pipeline supplies the original canonical identity, including delayed votes. */
    public synchronized void acceptedProxy(String site, UUID occurrence) {
        if (occurrence == null || occurrence.version() != 8 || occurrence.variant() != 2
                || !candidates.containsKey(site) || received.containsKey(site) || occurrences.containsKey(occurrence)) return;
        UUID source = new UUID(occurrence.getMostSignificantBits(), 0x8000000000000000L);
        ProxyVoteSequence.Fence fence = proxyFences.get(source);
        if (fence == null) {
            // Bounded provisional receipts cover a new occurrence overtaking its fence reply.
            if (pendingProxy.size() < 400) pendingProxy.putIfAbsent(occurrence, site);
            return;
        }
        pendingProxy.remove(occurrence);
        if (!fence.newer(occurrence)) return;
        received.put(site, occurrence.getLeastSignificantBits() & 0x3fffffffffffffffL);
        occurrences.put(occurrence, site);
        if (initialized) sites.putIfAbsent(site, candidates.get(site));
    }
    public synchronized View view(String action) {
        if (!sites.isEmpty()) {
            String current = new ArrayList<>(sites.keySet()).get(index);
            if (action.equals("skip")) { skipped.add(current); index = (index + 1) % sites.size(); }
            if (action.equals("next")) index = (index + 1) % sites.size();
            if (action.equals("previous")) index = Math.floorMod(index - 1, sites.size());
        }
        if (action.equals("finish")) finished = true;
        List<Entry> entries = new ArrayList<>();
        for (Site site : sites.values()) {
            Status status = received.containsKey(site.key()) ? Status.RECEIVED : Status.AWAITING;
            if (status != Status.RECEIVED && !site.eligible()) status = Status.UNAVAILABLE;
            if (status == Status.AWAITING && skipped.contains(site.key())) status = Status.SKIPPED;
            entries.add(new Entry(site, status));
        }
        return new View(List.copyOf(entries), index, finished);
    }
    public static String httpUrl(String input) {
        if (input == null || input.length() > 2048) return null;
        try {
            String destination = input.trim();
            if (destination.startsWith("[")) {
                var wrapper = java.util.regex.Pattern.compile("(?i)\\[Text=\"[^\"\\r\\n]*\",\\s*url=\"([^\"\\r\\n]*)\"\\]").matcher(destination);
                if (!wrapper.matches()) return null;
                destination = wrapper.group(1);
            }
            URI uri = URI.create(destination);
            return ("http".equalsIgnoreCase(uri.getScheme()) || "https".equalsIgnoreCase(uri.getScheme()))
                    && uri.getHost() != null && uri.getUserInfo() == null ? uri.toASCIIString() : null;
        } catch (IllegalArgumentException invalid) { return null; }
    }
}
