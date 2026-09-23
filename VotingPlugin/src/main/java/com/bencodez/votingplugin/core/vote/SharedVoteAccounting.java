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
        applyAdmitted(policy.shouldCountTotals(input, currentlyOnline), true, addTotal, addTotalDaily,
                addTotalWeekly, addPoints);
    }

    /** Applies an accounting decision that was durably admitted before vote side effects began. */
    public static void applyAdmitted(boolean countTotals, boolean awardPoints,
            Runnable addTotal, Runnable addTotalDaily, Runnable addTotalWeekly, Runnable addPoints) {
        Objects.requireNonNull(addTotal, "addTotal");
        Objects.requireNonNull(addTotalDaily, "addTotalDaily");
        Objects.requireNonNull(addTotalWeekly, "addTotalWeekly");
        Objects.requireNonNull(addPoints, "addPoints");
        if (countTotals) {
            addTotal.run();
            addTotalDaily.run();
            addTotalWeekly.run();
        }
        if (awardPoints) addPoints.run();
    }
}
