package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.UUID;

public record SharedVoteIdentity(UUID uuid, String playerName, boolean online) {
    public SharedVoteIdentity {
        Objects.requireNonNull(uuid, "uuid");
        Objects.requireNonNull(playerName, "playerName");
        if (playerName.isBlank()) {
            throw new IllegalArgumentException("playerName cannot be blank");
        }
    }
}
