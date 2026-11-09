package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import com.bencodez.votingplugin.specialrewards.datemilestones.*;

import static org.junit.jupiter.api.Assertions.*;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.channels.FileChannel;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DateVoteLedgerAdministrationTest {
    @TempDir Path root;
    final UUID player = UUID.randomUUID();
    final DateVoteEvent event = new DateVoteEvent("sample", "Sample", true, 100, 200,
            "UTC", List.of(1, 3, 6), Set.of(), "");

    @Test void settingProgressRetainsReceiptDeduplicationAcrossRestart() throws Exception {
        var ledger = new DateVoteLedger(root);
        UUID receipt = UUID.randomUUID();
        ledger.record(event, player, receipt, false);
        assertEquals(3, ledger.setVotes(event, player, 3).votes());
        var restarted = new DateVoteLedger(root);
        restarted.record(event, player, receipt, false);
        assertEquals(3, restarted.progress(event, player).votes());
        restarted.record(event, player, UUID.randomUUID(), false);
        assertEquals(4, restarted.progress(event, player).votes());
        assertEquals(0, restarted.setVotes(event, player, 0).votes());
        restarted.record(event, player, receipt, false);
        assertEquals(0, restarted.progress(event, player).votes());
        assertEquals(List.of(), new DateVoteLedger(root).record(event, player, UUID.randomUUID(), false));
        assertEquals(1, restarted.progress(event, player).votes());
    }

    @Test void lowerCountPreservesSubmittedAndDeferredAwardsWithoutPrematureSubmission() throws Exception {
        var ledger = new DateVoteLedger(root);
        ledger.setVotes(event, player, 6);
        assertTrue(ledger.reserve(event, player, 3)); ledger.submitted(event, player, 3);
        ledger.setVotes(event, player, 0);
        var snapshot = new DateVoteLedger(root).progress(event, player);
        assertFalse(ledger.reserve(event, player, 6), "A count edit can revoke an earlier unclaimed threshold");
        assertEquals(Set.of(3), snapshot.submittedAwards());
        assertEquals(Set.of(1, 6), snapshot.deferredAwards());
        assertEquals(List.of(1), ledger.record(event, player, UUID.randomUUID(), true));
        assertEquals(Set.of(3), ledger.progress(event, player).submittedAwards());
    }

    @Test void forceReservesBeforeRewardsWithoutFabricatingVotesOrErasingUncertainty() throws Exception {
        var ledger = new DateVoteLedger(root);
        assertTrue(ledger.reserveForced(event, player, 6, false));
        var restarted = new DateVoteLedger(root);
        assertEquals(0, restarted.progress(event, player).votes());
        assertEquals(Set.of(6), restarted.progress(event, player).reservedAwards());
        assertFalse(restarted.reserveForced(event, player, 6, false));
        assertFalse(restarted.reserveForced(event, player, 6, true));
        restarted.submitted(event, player, 6);
        assertFalse(restarted.reserveForced(event, player, 6, false));
        assertTrue(restarted.reserveForced(event, player, 6, true));
        assertFalse(ledger.reserveForced(event, player, 6, true));
        assertEquals(0, ledger.progress(event, player).votes());
    }

    @Test void invalidAdministrativeValuesDoNotCreateOrResealStorage() throws Exception {
        Path absent = root.resolve("absent"); var ledger = new DateVoteLedger(absent);
        assertThrows(IllegalArgumentException.class, () -> ledger.setVotes(event, player, -1));
        assertThrows(IllegalArgumentException.class, () -> ledger.setVotes(event, player, 4097));
        assertThrows(IllegalArgumentException.class, () -> ledger.reserveForced(event, player, 2, true));
        assertFalse(Files.exists(absent));
        ledger.setVotes(event, player, 2);
        var changed = new DateVoteEvent("sample", "Sample", true, 100, 300,
                "UTC", List.of(1, 3, 6), Set.of(), "");
        assertThrows(IOException.class, () -> ledger.setVotes(changed, player, 4));
        assertThrows(IOException.class, () -> ledger.reserveForced(changed, player, 3, true));
        assertEquals(2, ledger.progress(event, player).votes());
    }

    @Test void malformedAdjustmentsBlockReadsAndAdministrativeWritesWithoutRepair() throws Exception {
        for (String adjustment : List.of("4097", "-4097", "-1", "01", "+1", "1.0", "2147483647", "")) {
        Path directory = root.resolve("case-" + adjustment.hashCode());
        var ledger = new DateVoteLedger(directory);
        ledger.setVotes(event, player, 0);
        Path file = directory.resolve(event.fileId() + "-" + player + ".properties");
        String state = Files.readString(file).replace("adjustment=0", "adjustment=" + adjustment);
        Files.writeString(file, state);
        assertThrows(IOException.class, () -> ledger.progress(event, player));
        assertThrows(IOException.class, () -> ledger.setVotes(event, player, 1));
        assertThrows(IOException.class, () -> ledger.reserveForced(event, player, 1, true));
        assertEquals(state, Files.readString(file), "An admin action must not silently repair corrupt history");
        }
    }

    @Test void adjustedCountCapacityPreservesOriginalOccurrencesAndAllowsExplicitReduction() throws Exception {
        var ledger = new DateVoteLedger(root);
        UUID receipt = UUID.randomUUID();
        ledger.record(event, player, receipt, false);
        ledger.setVotes(event, player, 4096);
        assertThrows(IOException.class, () -> ledger.record(event, player, UUID.randomUUID(), false));
        ledger.record(event, player, receipt, false);
        assertEquals(4096, ledger.progress(event, player).votes());
        ledger.setVotes(event, player, 4095);
        ledger.record(event, player, UUID.randomUUID(), false);
        assertEquals(4096, new DateVoteLedger(root).progress(event, player).votes());
    }

    @Test void administrativeWritesRespectNativeOwnershipLockAndRecordCapacity() throws Exception {
        var ledger = DateVoteLedgerAccess.create(root, 1);
        ledger.setVotes(event, player, 0);
        try (var channel = FileChannel.open(root.resolve(".ledger.lock"), StandardOpenOption.WRITE);
                var lock = channel.lock()) {
            assertThrows(IOException.class, () -> ledger.setVotes(event, player, 2));
            assertThrows(IOException.class, () -> ledger.reserveForced(event, player, 1, true));
        }
        UUID another = UUID.randomUUID();
        assertThrows(IOException.class, () -> ledger.setVotes(event, another, 1));
        assertThrows(IOException.class, () -> ledger.reserveForced(event, another, 1, true));
        assertEquals(0, ledger.progress(event, player).votes());
        assertTrue(ledger.reserveForced(event, player, 1, false), "Existing records remain usable at capacity");
    }
}
