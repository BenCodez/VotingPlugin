package com.bencodez.votingplugin.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BooleanSupplier;

import org.junit.jupiter.api.Test;

class SharedVoteDeliveryTest {
    @Test
    void nativeOnlineAndEligibleOfflineVotesDeliverImmediately() {
        assertTrue(SharedVoteDelivery.shouldDeliverNow(() -> false, () -> true, () -> false, () -> true));
        assertTrue(SharedVoteDelivery.shouldDeliverNow(() -> false, () -> false, () -> true, () -> true));
    }

    @Test
    void nativeOfflineVoteQueuesWhenRewardsAreDisabledOrSiteDisallowsOfflineDelivery() {
        assertFalse(SharedVoteDelivery.shouldDeliverNow(() -> false, () -> false, () -> false, () -> true));
        assertFalse(SharedVoteDelivery.shouldDeliverNow(() -> false, () -> false, () -> true, () -> false));
        assertFalse(SharedVoteDelivery.shouldDeliverNow(() -> false, () -> true, () -> true, () -> false));
    }

    @Test
    void proxyVoteStillDeliversWhenNormalRewardsAreDisabled() {
        assertTrue(SharedVoteDelivery.shouldDeliverNow(() -> true, () -> false, () -> false, () -> false));
    }

    @Test
    void readsConditionsInTheSameShortCircuitOrderAsTheListener() {
        List<String> reads = new ArrayList<>();
        BooleanSupplier online = () -> { reads.add("online"); return true; };
        BooleanSupplier offline = () -> { reads.add("offline"); return false; };
        BooleanSupplier process = () -> { reads.add("process"); return false; };
        BooleanSupplier proxy = () -> { reads.add("proxy"); return true; };

        assertTrue(SharedVoteDelivery.shouldDeliverNow(proxy, online, offline, process));
        assertEquals(List.of("online", "process", "proxy"), reads);
    }
}
