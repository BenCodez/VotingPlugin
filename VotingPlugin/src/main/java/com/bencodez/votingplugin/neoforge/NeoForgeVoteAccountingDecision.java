package com.bencodez.votingplugin.neoforge;

import java.util.Objects;

import com.bencodez.votingplugin.core.vote.SharedVoteAccounting;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;

/** Immutable accounting decision captured when a complete vote is accepted. */
public record NeoForgeVoteAccountingDecision(int total, int daily, int weekly,
        int points, boolean pointsApplied, int pointLimit) {
    public NeoForgeVoteAccountingDecision {
        if (total < 0 || total > 1 || daily < 0 || daily > 1 || weekly < 0 || weekly > 1) {
            throw new IllegalArgumentException("Accounting increments must be zero or one");
        }
        if (!pointsApplied && points != 0) {
            throw new IllegalArgumentException("Unapplied points must be zero");
        }
    }

    static NeoForgeVoteAccountingDecision capture(SharedVoteInput input, SharedVotePolicy policy,
            boolean currentlyOnline, int pointsOnVote, int pointLimit) {
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(policy, "policy");
        MutableDecision decision = new MutableDecision();
        SharedVoteAccounting.apply(input, policy, () -> currentlyOnline,
                () -> decision.total++, () -> decision.daily++, () -> decision.weekly++,
                () -> {
                    decision.points += pointsOnVote;
                    decision.pointsApplied = true;
                });
        return new NeoForgeVoteAccountingDecision(decision.total, decision.daily, decision.weekly,
                decision.points, decision.pointsApplied, pointLimit);
    }

    private static final class MutableDecision {
        int total;
        int daily;
        int weekly;
        int points;
        boolean pointsApplied;
    }
}
