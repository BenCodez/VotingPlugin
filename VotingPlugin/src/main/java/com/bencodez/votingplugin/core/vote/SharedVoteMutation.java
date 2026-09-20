package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.UUID;

/**
 * Describes the logical vote mutation. The AdvancedCore-facing storage adapter
 * owns actual keys, cache/queue ordering, point hooks/caps, and persistence.
 */
public record SharedVoteMutation(UUID voteId, String serviceSite, long voteTime,
        boolean countTotals, boolean awardConfiguredPoints) {
    public SharedVoteMutation {
        Objects.requireNonNull(voteId, "voteId");
        Objects.requireNonNull(serviceSite, "serviceSite");
    }
}
