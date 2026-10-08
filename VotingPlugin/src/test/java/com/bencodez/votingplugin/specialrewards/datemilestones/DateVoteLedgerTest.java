package com.bencodez.votingplugin.specialrewards.datemilestones;
import static org.junit.jupiter.api.Assertions.*;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
class DateVoteLedgerTest {
    @TempDir Path root;
    final UUID player = UUID.randomUUID();
    private DateVoteEvent event(String id, String name, long end) {
        return new DateVoteEvent(id, name, true, 100, end, "UTC", List.of(1, 2), Set.of(), "");
    }
    @Test void deferredThresholdPromotesOnceAcrossConcurrentReplayAndNeverDemotesAnUncertainReservation() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200); UUID occurrence = UUID.randomUUID();
        assertEquals(List.of(), ledger.record(e, player, occurrence, false));
        ledger = new DateVoteLedger(root); assertEquals(Set.of(1), ledger.progress(e, player).deferredAwards());
        final DateVoteLedger resumed = ledger;
        try (var pool = Executors.newFixedThreadPool(4)) {
            var jobs = java.util.stream.IntStream.range(0, 20).mapToObj(i -> (java.util.concurrent.Callable<List<Integer>>)
                    () -> resumed.record(e, player, occurrence, true)).toList();
            int admitted = 0;
            for (var result : pool.invokeAll(jobs)) admitted += result.get().size();
            assertEquals(1, admitted);
        }
        assertEquals(List.of(), ledger.record(e, player, occurrence, false));
        assertEquals(Set.of(1), ledger.progress(e, player).reservedAwards());
        assertTrue(ledger.progress(e, player).deferredAwards().isEmpty()); assertEquals(1, ledger.progress(e, player).votes());
        assertEquals(List.of(), new DateVoteLedger(root).record(e, player, occurrence, true));
    }
    @Test void duplicateAndRestartNeverResubmitReservedOrSubmittedAwards() throws Exception {
        var e = event("a", "A", 200); var ledger = new DateVoteLedger(root); UUID id = UUID.randomUUID();
        assertEquals(List.of(1), ledger.record(e, player, id));
        ledger = new DateVoteLedger(root);
        assertEquals(List.of(), ledger.record(e, player, id));
        assertEquals(Set.of(1), ledger.progress(e, player).reservedAwards());
        ledger.submitted(e, player, 1); ledger.submitted(e, player, 1);
        ledger = new DateVoteLedger(root);
        assertEquals(List.of(2), ledger.record(e, player, UUID.randomUUID()));
        assertEquals(Set.of(1), ledger.progress(e, player).submittedAwards());
        assertEquals(2, ledger.progress(e, player).votes());
    }
    @Test void overlapsCountOnceForEachEventButUnrelatedPlayersAreSeparate() throws Exception {
        var ledger = new DateVoteLedger(root); UUID occurrence = UUID.randomUUID();
        assertEquals(List.of(1), ledger.record(event("a", "A", 200), player, occurrence));
        assertEquals(List.of(1), ledger.record(event("b", "B", 300), player, occurrence));
        assertEquals(List.of(1), ledger.record(event("a", "A", 200), UUID.randomUUID(), occurrence));
    }
    @Test void sameOccurrenceConcurrentRequestsReserveExactlyOnce() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200); UUID occurrence = UUID.randomUUID();
        try (var pool = Executors.newFixedThreadPool(4)) {
            var jobs = java.util.stream.IntStream.range(0, 20).mapToObj(i -> (java.util.concurrent.Callable<List<Integer>>)
                    () -> ledger.record(e, player, occurrence)).toList();
            int awarded = 0;
            for (var result : pool.invokeAll(jobs)) awarded += result.get().size();
            assertEquals(1, awarded); assertEquals(1, ledger.progress(e, player).votes());
        }
    }
    @Test void immutableContractIsSealedAcrossPlayersAndDisplayRenames() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        assertEquals(1, ledger.progress(event("a", "Renamed", 200), player).votes());
        assertThrows(IOException.class, () -> ledger.record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> ledger.progress(event("a", "A", 300), player));
    }
    @Test void malformedStateFailsClosedWithoutResettingOrRewards() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        Path file = root.resolve(e.fileId() + "-" + player + ".properties");
        String original = Files.readString(file);
        for (String corruption : List.of("", "seen=\nfingerprint=bad\n", original + "seen=\n", original + "unknown=value\n")) {
            Files.writeString(file, corruption);
            assertThrows(IOException.class, () -> new DateVoteLedger(root).record(e, player, UUID.randomUUID()));
            assertEquals(corruption, Files.readString(file));
        }
    }
    @Test void missingReachedAwardFailsClosedForReadsWritesAndAcknowledgements() throws Exception {
        var e = event("a", "A", 200); var ledger = new DateVoteLedger(root);
        ledger.record(e, player, UUID.randomUUID()); ledger.submitted(e, player, 1);
        Path file = root.resolve(e.fileId() + "-" + player + ".properties");
        String broken = Files.readString(file).replace("award.1=SUBMITTED\n", "");
        Files.writeString(file, broken);
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(e, player, UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(e, player));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).submitted(e, player, 1));
        assertEquals(broken, Files.readString(file));
    }
    @Test void caseDistinctIdsHaveDistinctPortableFilesAndIndependentProgress() throws Exception {
        var first = event("Festival", "First", 200); var second = event("festival", "Second", 200);
        var ledger = new DateVoteLedger(root); UUID occurrence = UUID.randomUUID();
        assertNotEquals(first.fileId().toLowerCase(), second.fileId().toLowerCase());
        assertEquals(List.of(1), ledger.record(first, player, occurrence));
        assertEquals(List.of(1), ledger.record(second, player, occurrence));
        assertEquals(1, new DateVoteLedger(root).progress(first, player).votes());
        assertEquals(1, new DateVoteLedger(root).progress(second, player).votes());
    }
    @Test void delimiterContainingFilterCannotChangeASealedContract() throws Exception {
        var a = new DateVoteEvent("event", "A", true, 100, 200, "UTC", List.of(1), Set.of("a", "b"), "");
        var b = new DateVoteEvent("event", "B", true, 100, 200, "UTC", List.of(1), Set.of("a, b"), "");
        var ledger = new DateVoteLedger(root); ledger.record(a, player, UUID.randomUUID());
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(b, player, UUID.randomUUID()));
    }
    @Test void truncatedDefinitionsCannotResetSealedContractsForNewPlayers() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        Path definitions = root.resolve("definitions.properties"); Files.writeString(definitions, "");
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(e, player));
        assertEquals("", Files.readString(definitions));
    }
    @Test void missingDefinitionsCannotResealSurvivingHistoryForANewPlayer() throws Exception {
        var ledger = new DateVoteLedger(root); var original = event("a", "A", 200);
        ledger.record(original, player, UUID.randomUUID());
        Files.delete(root.resolve("definitions.properties"));
        var changed = event("a", "A", 300); UUID newcomer = UUID.randomUUID();
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(changed, newcomer, UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(changed, newcomer));
        assertFalse(Files.exists(root.resolve("definitions.properties")));
        assertFalse(Files.exists(root.resolve(changed.fileId() + "-" + newcomer + ".properties")));
    }
    @Test void partialDefinitionsCannotResealAnOmittedHistoricalEvent() throws Exception {
        var ledger = new DateVoteLedger(root); var first = event("a", "A", 200); var second = event("b", "B", 200);
        ledger.record(first, player, UUID.randomUUID()); ledger.record(second, player, UUID.randomUUID());
        Path definitions = root.resolve("definitions.properties");
        String partial = Files.readString(definitions).replace("a=" + first.fingerprint() + "\n", "");
        Files.writeString(definitions, partial);
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(first, UUID.randomUUID()));
        assertEquals(partial, Files.readString(definitions));
        assertEquals(1, new DateVoteLedger(root).progress(second, player).votes());
    }
    @Test void newIdsWithNoHistoryRemainUsableAlongsideSealedEvents() throws Exception {
        var ledger = new DateVoteLedger(root); ledger.record(event("a", "A", 200), player, UUID.randomUUID());
        assertEquals(List.of(1), new DateVoteLedger(root).record(event("new", "New", 300), player, UUID.randomUUID()));
    }
    @Test void unsafeDirectoryAndUnavailableStorageNeverAdmitAwards() throws Exception {
        Path file = root.resolve("file"); Files.writeString(file, "keep");
        assertThrows(IOException.class, () -> new DateVoteLedger(file).record(event("a", "A", 200), player, UUID.randomUUID()));
        Path link = root.resolve("link"); Files.createSymbolicLink(link, root);
        assertThrows(IOException.class, () -> new DateVoteLedger(link).record(event("a", "A", 200), player, UUID.randomUUID()));
        assertEquals("keep", Files.readString(file));
    }
}
