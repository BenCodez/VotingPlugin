package com.bencodez.votingplugin.core.vote;

import java.util.Objects;
import java.util.function.BooleanSupplier;

/** Applies the accepted vote's totals and points in Bukkit's existing order. */
public final class SharedVoteAccounting {
    private SharedVoteAccounting() {
    }

    public static void apply(SharedVoteInput input, SharedVotePolicy policy, BooleanSupplier currentlyOnline,
            Runnable addTotal, Runnable addTotalDaily, Runnable addTotalWeekly, Runnable addPoints) {
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(policy, "policy");
        Objects.requireNonNull(currentlyOnline, "currentlyOnline");
        Objects.requireNonNull(addTotal, "addTotal");
        Objects.requireNonNull(addTotalDaily, "addTotalDaily");
        Objects.requireNonNull(addTotalWeekly, "addTotalWeekly");
        Objects.requireNonNull(addPoints, "addPoints");
        if (!policy.shouldAwardConfiguredPoints(input)) return;
        applyAdmitted(input, policy, policy.shouldCountTotals(input, currentlyOnline), addTotal, addTotalDaily,
                addTotalWeekly, addPoints);
    }

    /** Applies an accounting decision that was durably admitted before vote side effects began. */
    public static void applyAdmitted(SharedVoteInput input, SharedVotePolicy policy, boolean countTotals,
            Runnable addTotal, Runnable addTotalDaily, Runnable addTotalWeekly, Runnable addPoints) {
        Objects.requireNonNull(input, "input");
        Objects.requireNonNull(policy, "policy");
        Objects.requireNonNull(addTotal, "addTotal");
        Objects.requireNonNull(addTotalDaily, "addTotalDaily");
        Objects.requireNonNull(addTotalWeekly, "addTotalWeekly");
        Objects.requireNonNull(addPoints, "addPoints");
        // A durable totals admission came from an earlier evaluation of this gate.
        // Replays must honor that decision even if live configuration has changed.
        if (!countTotals && !policy.shouldAwardConfiguredPoints(input)) return;
        if (countTotals) {
            addTotal.run();
            addTotalDaily.run();
            addTotalWeekly.run();
        }
        addPoints.run();
    }
}
