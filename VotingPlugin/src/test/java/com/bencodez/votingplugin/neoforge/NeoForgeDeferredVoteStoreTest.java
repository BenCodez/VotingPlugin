package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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
    void preservesOrderDeduplicatesAndCompletesOnlyTheSelectedVoteAcrossRestart() throws IOException {
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
            assertTrue(runtime.deferredVotes().complete(playerId, firstId));
            assertFalse(runtime.deferredVotes().complete(playerId, firstId));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            List<NeoForgeDeferredVote> pending = runtime.deferredVotes().pending(playerId);
            assertEquals(1, pending.size());
            assertEquals(secondId, pending.get(0).voteId());
            assertEquals(200L, pending.get(0).voteTime());
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
        return new NeoForgeVoteRequest(voteId, playerId, "Alex", "example.test", voteTime,
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
