package com.bencodez.votingplugin.core.datemilestones;

import java.nio.charset.StandardCharsets;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;

/** Immutable owner-defined event accounting contract; display text is not identity. */
public record DateVoteEvent(String id, String displayName, boolean enabled, long start, long end,
        String timezone, List<Integer> thresholds, Set<String> sites, String accountingServer) {
    public DateVoteEvent {
        if (id == null || !id.matches("[A-Za-z0-9_-]{1,64}")) throw new IllegalArgumentException("Invalid event ID");
        if (start <= 0 || end <= start) throw new IllegalArgumentException("End must be after start");
        ZoneId.of(timezone);
        if (thresholds.isEmpty() || thresholds.size() > 64 || thresholds.stream().anyMatch(n -> n <= 0 || n > 4096)
                || thresholds.stream().distinct().count() != thresholds.size()) throw new IllegalArgumentException("Invalid thresholds (1–4096, unique, max 64)");
        thresholds = thresholds.stream().sorted().toList();
        if (sites.size() > 100 || sites.stream().anyMatch(s -> s == null || s.length() > 128 || s.isBlank()))
            throw new IllegalArgumentException("Invalid site filters");
        sites = Set.copyOf(sites);
        if (accountingServer == null || accountingServer.length() > 128) throw new IllegalArgumentException("Invalid accounting server");
        if (displayName == null || displayName.length() > 256) throw new IllegalArgumentException("Invalid display name");
    }
    public boolean matches(long occurredAt, String site, boolean real, boolean proxy, String server) {
        return enabled && real && occurredAt >= start && occurredAt < end && (sites.isEmpty() || sites.contains(site))
                && (!proxy || !accountingServer.isBlank() && accountingServer.equals(server));
    }
    public String fingerprint() {
        try {
            var bytes = new ByteArrayOutputStream();
            try (var data = new DataOutputStream(bytes)) {
                data.writeLong(start); data.writeLong(end); data.writeUTF(timezone);
                data.writeInt(thresholds.size());
                for (int threshold : thresholds) data.writeInt(threshold);
                data.writeInt(sites.size());
                for (String site : sites.stream().sorted().toList()) data.writeUTF(site);
                data.writeUTF(accountingServer);
            }
            return hash(bytes.toByteArray());
        } catch (java.io.IOException impossible) { throw new IllegalStateException(impossible); }
    }
    /** Portable case-sensitive identity, including on case-insensitive filesystems. */
    public String fileId() { return hash(id.getBytes(StandardCharsets.UTF_8)); }
    private static String hash(byte[] bytes) {
        try { return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes)); }
        catch (java.security.NoSuchAlgorithmException impossible) { throw new IllegalStateException(impossible); }
    }
    public static long timestamp(String localIso, String timezone) {
        LocalDateTime time = LocalDateTime.parse(localIso);
        ZoneId zone = ZoneId.of(timezone);
        if (zone.getRules().getValidOffsets(time).size() != 1)
            throw new IllegalArgumentException("Ambiguous/nonexistent local time; choose an unambiguous boundary");
        return time.atZone(zone).toInstant().toEpochMilli();
    }
}
