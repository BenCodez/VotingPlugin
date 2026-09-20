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
        if (policy.shouldCountTotals(input, currentlyOnline)) {
            addTotal.run();
            addTotalDaily.run();
            addTotalWeekly.run();
        }
        addPoints.run();
    }
}
