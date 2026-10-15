package com.bencodez.votingplugin.npc;

import java.util.Objects;
import java.util.UUID;
import java.util.regex.Pattern;

/** Durable definition; it contains no live world, player, entity or reward state. */
public record VotingNPCDefinition(String id, UUID identity, UUID world, double x, double y, double z,
        float yaw, float pitch, String name) {
    private static final Pattern ID = Pattern.compile("[a-z0-9][a-z0-9_-]{0,39}");

    public VotingNPCDefinition {
        if (id == null || !ID.matcher(id).matches()) throw new IllegalArgumentException("Invalid voting NPC ID");
        Objects.requireNonNull(identity, "identity");
        Objects.requireNonNull(world, "world");
        if (!Double.isFinite(x) || !Double.isFinite(y) || !Double.isFinite(z)
                || Math.abs(x) > 30_000_000 || Math.abs(z) > 30_000_000
                || !Float.isFinite(yaw) || !Float.isFinite(pitch)) {
            throw new IllegalArgumentException("Invalid voting NPC position");
        }
        if (name == null || name.isEmpty() || name.length() > 128
                || name.codePoints().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid voting NPC name");
        }
    }

    public int chunkX() { return Math.floorDiv((int) Math.floor(x), 16); }
    public int chunkZ() { return Math.floorDiv((int) Math.floor(z), 16); }
}
