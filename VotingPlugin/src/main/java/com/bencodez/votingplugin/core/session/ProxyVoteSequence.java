package com.bencodez.votingplugin.core.session;

import java.util.UUID;

/** Original proxy ingress order, carried by the existing opaque canonical vote UUID.
 * UUIDv8: random 60-bit source incarnation and a monotonically increasing 62-bit sequence.
 * The process/classloader owns one source across runtime replacement; a restart creates
 * another. Never assign a new ID when forwarding, retrying or replaying an occurrence. */
public final class ProxyVoteSequence {
    private static final long MAX_SEQUENCE = 0x3fffffffffffffffL;
    private static final ProxyVoteSequence LIVE = new ProxyVoteSequence(UUID.randomUUID());
    private final long source;
    private long sequence;

    public ProxyVoteSequence(UUID incarnation) {
        source = (incarnation.getMostSignificantBits() & 0xffffffffffff0fffL) | 0x8000L;
    }
    public static UUID nextVoteId() { return LIVE.next(); }
    public static Fence currentFence() { return LIVE.fence(); }
    public synchronized UUID next() {
        if (sequence == MAX_SEQUENCE) throw new IllegalStateException("Proxy vote sequence exhausted");
        return new UUID(source, 0x8000000000000000L | ++sequence);
    }
    public synchronized Fence fence() { return new Fence(new UUID(source, 0x8000000000000000L), sequence); }

    public record Fence(UUID incarnation, long watermark) {
        public Fence {
            if (incarnation == null || incarnation.version() != 8 || incarnation.variant() != 2
                    || incarnation.getLeastSignificantBits() != 0x8000000000000000L
                    || watermark < 0 || watermark > MAX_SEQUENCE)
                throw new IllegalArgumentException("Invalid proxy occurrence fence");
        }
        public boolean newer(UUID occurrence) {
            return occurrence != null && occurrence.version() == 8 && occurrence.variant() == 2
                    && occurrence.getMostSignificantBits() == incarnation.getMostSignificantBits()
                    && (occurrence.getLeastSignificantBits() & MAX_SEQUENCE) > watermark;
        }
    }
}
