package com.bencodez.votingplugin.core.maintenance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.core.maintenance.MaintenanceConfirmation.Result;

class MaintenanceConfirmationTest {
    @Test void confirmationIsActorAndOperationBoundAndSingleUse() {
        var confirmation = new MaintenanceConfirmation();
        assertEquals(Result.REQUESTED, confirmation.request("Console", "clear"));
        assertEquals(Result.REQUESTED, confirmation.request("Rcon", "clear"));
        assertEquals(Result.REQUESTED, confirmation.request("Console", "recover uuid token"));
        assertEquals(Result.REQUESTED, confirmation.request("Console", "clear"));
        assertEquals(Result.CONFIRMED, confirmation.request("Console", "clear"));
        assertEquals(Result.REQUESTED, confirmation.request("Console", "clear"));
    }
    @Test void expiryAtThirtySecondsRequestsFreshConfirmation() {
        var clock = new AtomicLong();
        var confirmation = new MaintenanceConfirmation(clock::get);
        assertEquals(Result.REQUESTED, confirmation.request("Console", "clear"));
        clock.addAndGet(TimeUnit.SECONDS.toNanos(30));
        assertEquals(Result.REQUESTED, confirmation.request("Console", "clear"));
        assertEquals(Result.CONFIRMED, confirmation.request("Console", "clear"));
    }
    @Test void pendingActorsAreBoundedAndExpiryReclaimsCapacity() {
        var clock = new AtomicLong();
        var confirmation = new MaintenanceConfirmation(clock::get);
        for (int i=0; i<128; i++) assertEquals(Result.REQUESTED, confirmation.request("actor"+i, "clear"));
        assertEquals(Result.FULL, confirmation.request("extra", "clear"));
        assertEquals(Result.CONFIRMED, confirmation.request("actor0", "clear"));
        assertEquals(Result.REQUESTED, confirmation.request("extra", "clear"));
        clock.addAndGet(TimeUnit.SECONDS.toNanos(30));
        assertEquals(Result.REQUESTED, confirmation.request("new", "clear"));
    }
    @Test void restartCreatesNoOutstandingConfirmation() {
        new MaintenanceConfirmation().request("Console", "clear");
        assertEquals(Result.REQUESTED, new MaintenanceConfirmation().request("Console", "clear"));
    }
}
