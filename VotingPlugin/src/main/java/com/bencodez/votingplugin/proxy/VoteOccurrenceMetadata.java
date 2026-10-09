package com.bencodez.votingplugin.proxy;

/** Optional occurrence metadata in existing opaque cache payloads, separate from cooldown time. */
public final class VoteOccurrenceMetadata {
    private static final String MARKER = "//vp-occurrence-time:";

    private VoteOccurrenceMetadata() { }

    /** Null means an older payload without metadata; -1 means explicitly invalid metadata. */
    public static Long read(String payload) {
        if (payload == null) return null;
        int marker = payload.lastIndexOf(MARKER);
        if (marker < 0) return null;
        if (marker != payload.indexOf(MARKER)) return -1L;
        return parse(payload.substring(marker + MARKER.length(),
                Math.min(payload.length(), marker + MARKER.length() + 21)));
    }

    public static long parse(String raw) {
        if (raw == null || raw.isEmpty() || raw.length() > 20) return -1L;
        try {
            long time = Long.parseLong(raw);
            return time > 0 ? time : -1L;
        } catch (NumberFormatException invalid) {
            return -1L;
        }
    }

    /** Existing v1/v2 totals parsers ignore this tagged trailing token. */
    public static String store(String payload, long occurredAt) {
        String text = payload == null ? "" : payload;
        int marker = text.indexOf(MARKER);
        if (marker >= 0) text = text.substring(0, marker);
        return text + MARKER + (occurredAt > 0 ? occurredAt : -1L);
    }

    public static String preserve(String oldPayload, String updatedPayload) {
        Long original = read(oldPayload);
        return original == null ? updatedPayload : store(updatedPayload, original);
    }
}
