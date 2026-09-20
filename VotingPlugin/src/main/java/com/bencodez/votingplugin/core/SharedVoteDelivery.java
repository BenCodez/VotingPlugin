package com.bencodez.votingplugin.core;

import java.util.Objects;
import java.util.function.BooleanSupplier;

/** The existing Bukkit decision to deliver a vote now or queue it for login. */
public final class SharedVoteDelivery {
    private SharedVoteDelivery() {
    }

    public static boolean shouldDeliverNow(BooleanSupplier proxyVote, BooleanSupplier currentlyOnline,
            BooleanSupplier giveOfflineRewards, BooleanSupplier processRewards) {
        Objects.requireNonNull(proxyVote, "proxyVote");
        Objects.requireNonNull(currentlyOnline, "currentlyOnline");
        Objects.requireNonNull(giveOfflineRewards, "giveOfflineRewards");
        Objects.requireNonNull(processRewards, "processRewards");
        return ((currentlyOnline.getAsBoolean() || giveOfflineRewards.getAsBoolean())
                && processRewards.getAsBoolean()) || proxyVote.getAsBoolean();
    }
}
