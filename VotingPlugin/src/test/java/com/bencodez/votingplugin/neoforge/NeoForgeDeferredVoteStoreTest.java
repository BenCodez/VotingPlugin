package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

class NeoForgeDeferredVoteStoreTest {
    @TempDir Path directory;

    @Test
    void preservesOrderAndDeduplicatesAcrossRestart() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID firstId = UUID.randomUUID();
        UUID secondId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(firstId, playerId, 100L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(secondId, playerId, 200L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(firstId, playerId, 999L)).status());

            assertEquals(List.of(firstId, secondId), runtime.deferredVotes().pending(playerId).stream()
                    .map(NeoForgeDeferredVote::voteId).toList());
            assertEquals(100L, runtime.deferredVotes().pending(playerId).get(0).voteTime());
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            List<NeoForgeDeferredVote> pending = runtime.deferredVotes().pending(playerId);
            assertEquals(List.of(firstId, secondId), pending.stream().map(NeoForgeDeferredVote::voteId).toList());
            assertEquals(100L, pending.get(0).voteTime());
            assertEquals(200L, pending.get(1).voteTime());
            assertNoAccounting(runtime.accounting().load(playerId).orElseThrow());
        }
    }

    @Test
    void capacityFailureIsExplicitAndDoesNotMutateAccounting() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            for (int index = 0; index < NeoForgeDeferredVoteStore.MAX_DEFERRED_PER_USER; index++) {
                assertEquals(NeoForgeVoteResult.Status.DEFERRED, runtime.voteProcessor()
                        .process(complete(new UUID(0, index + 1L), playerId, index + 1L)).status());
            }

            NeoForgeVoteResult full = runtime.voteProcessor()
                    .process(complete(UUID.randomUUID(), playerId, 1_000L));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED_CAPACITY_REACHED, full.status());
            assertFalse(full.accountingMutated());
            assertFalse(full.durablyRetained());
            assertEquals(NeoForgeDeferredVoteStore.MAX_DEFERRED_PER_USER,
                    runtime.deferredVotes().pending(playerId).size());
            assertNoAccounting(runtime.accounting().load(playerId).orElseThrow());
        }
    }

    @Test
    void globalCapacityRejectsAnotherPlayerWithoutMutatingAccounting() throws IOException {
        writeConfiguration();
        UUID first = UUID.randomUUID();
        UUID second = UUID.randomUUID();
        UUID rejected = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(first, "First", true));
            runtime.players().joined(new SharedVoteIdentity(second, "Second", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 2, 2);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());

            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(UUID.randomUUID(), first, "First", 100L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(UUID.randomUUID(), second, "Second", 200L)).status());
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(rejected, "Rejected", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 2, 2);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());
            NeoForgeVoteResult full = processor.process(
                    complete(UUID.randomUUID(), rejected, "Rejected", 300L));

            assertEquals(NeoForgeVoteResult.Status.DEFERRED_CAPACITY_REACHED, full.status());
            assertTrue(bounded.pending(rejected).isEmpty());
            assertTrue(runtime.accounting().load(rejected).isEmpty());
        }
    }

    @Test
    void retainedRetrySurvivesLogoutAndDisabledSiteWithoutAnotherEntry() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(voteId, playerId, 100L)).status());
        }
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  Example:
                    Enabled: false
                    ServiceSite: renamed.test
                """);

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteResult retry = runtime.voteProcessor().process(complete(voteId, playerId, 999L));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED, retry.status());
            assertTrue(retry.durablyRetained());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
            assertEquals(100L, runtime.deferredVotes().pending(playerId).get(0).voteTime());
            assertNoAccounting(runtime.accounting().load(playerId).orElseThrow());
        }
    }

    @Test
    void malformedStoredQueueIsNotSilentlyOverwritten() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.storage().user(playerId).write(UserStorage.SQLITE, NeoForgeDeferredVoteStore.DEFERRED_VOTES,
                    new DataValueString("not-a-supported-record"));

            assertThrows(IllegalStateException.class,
                    () -> runtime.voteProcessor().process(complete(UUID.randomUUID(), playerId, 100L)));
            assertThrows(IllegalStateException.class, () -> runtime.deferredVotes().pending(playerId));
            assertNoAccounting(runtime.accounting().load(playerId).orElseThrow());
        }
    }

    @Test
    void invalidUtf8StoredQueueIsNotNormalizedOrOverwritten() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(playerId).write(UserStorage.SQLITE, NeoForgeDeferredVoteStore.DEFERRED_VOTES,
                    new DataValueString("v1|" + voteId
                            + "|_w|ZXhhbXBsZS50ZXN0|RXhhbXBsZQ|100|true|true|true"));

            assertThrows(IllegalStateException.class, () -> runtime.deferredVotes().pending(playerId));
            assertThrows(IllegalStateException.class,
                    () -> runtime.voteProcessor().process(complete(UUID.randomUUID(), playerId, 200L)));
        }
    }

    private void writeConfiguration() throws IOException {
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AllowUnjoined: false
                AddTotals: true
                AddTotalsOffline: true
                CountFakeVotes: true
                ProcessRewards: true
                PointsOnVote: 1
                LimitVotePoints: -1
                """);
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  Example:
                    Enabled: true
                    ServiceSite: example.test
                    VoteDelay: 24
                    WaitUntilVoteDelay: true
                """);
    }

    private static NeoForgeVoteRequest complete(UUID voteId, UUID playerId, long voteTime) {
        return complete(voteId, playerId, "Alex", voteTime);
    }

    private static NeoForgeVoteRequest complete(UUID voteId, UUID playerId, String playerName, long voteTime) {
        return new NeoForgeVoteRequest(voteId, playerId, playerName, "example.test", voteTime,
                true, true, true, NeoForgeVoteRequest.Scope.COMPLETE);
    }

    private static void assertNoAccounting(NeoForgeVoteAccount account) {
        assertEquals(0, account.allTimeTotal());
        assertEquals(0, account.monthTotal());
        assertEquals(0, account.dailyTotal());
        assertEquals(0, account.weeklyTotal());
        assertEquals(0, account.points());
        assertTrue(account.lastVotes().isEmpty());
    }
}
