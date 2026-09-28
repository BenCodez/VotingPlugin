package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;

class NeoForgeDeferredVoteStoreTest {
    @TempDir Path directory;

    @Test
    void restartDiscoveryIncludesOnlyRowsWithDeferredPayloads() throws IOException {
        writeConfiguration();
        UUID historical = UUID.randomUUID();
        UUID pending = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(historical).write(UserStorage.SQLITE,
                    NeoForgeVoteAccountingStore.PLAYER_NAME, new DataValueString("Historical"));
            runtime.players().joined(new SharedVoteIdentity(pending, "Pending", true));
            runtime.voteProcessor().process(complete(UUID.randomUUID(), pending, 100L));
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertEquals(List.of(pending), runtime.deferredVotes().users());
        }
    }

    @Test
    void restartDiscoveryIncludesEveryPendingRowEvenAboveCurrentAdmissionLimit() throws IOException {
        writeConfiguration();
        UUID first = new UUID(0L, 1L);
        UUID second = new UUID(0L, 2L);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(first, "First", true));
            runtime.players().joined(new SharedVoteIdentity(second, "Second", true));
            runtime.voteProcessor().process(complete(UUID.randomUUID(), first, "First", 100L));
            runtime.voteProcessor().process(complete(UUID.randomUUID(), second, "Second", 100L));

            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 1);
            assertEquals(Set.of(first, second), Set.copyOf(bounded.users()));
        }
    }

    @Test
    void restartDiscoveryDoesNotOpenEveryHistoricalAccountingRow() throws Exception {
        writeConfiguration();
        UUID pending = UUID.randomUUID();
        Path database;
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.rewardReplay().ifPresent(NeoForgeRewardReplayService::stopAdmission);
            runtime.players().joined(new SharedVoteIdentity(pending, "Pending", true));
            runtime.voteProcessor().process(complete(UUID.randomUUID(), pending, 100L));
            database = ((com.bencodez.advancedcore.core.user.storage.sql.SqliteUserBackend)
                    runtime.storage()).databaseFile();
        }
        insertHistoricalUsers(database, 5_000);

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.rewardReplay().ifPresent(NeoForgeRewardReplayService::stopAdmission);
            assertTimeoutPreemptively(Duration.ofSeconds(2),
                    () -> assertEquals(List.of(pending), runtime.deferredVotes().users()));
        }
    }

    @Test
    void completionAtomicallyReplacesPendingVoteWithRestartSafeReceipt() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        Clock clock = Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(voteId, playerId, 100L)).status());

            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                assertEquals(voteId, claim.vote().voteId());
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }

            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(playerId, voteId));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(playerId, voteId));
            NeoForgeVoteResult retry = runtime.voteProcessor().process(complete(voteId, playerId, 999L));
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED, retry.status());
            assertTrue(retry.durablyCompleted());
            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            assertNoAccounting(runtime.accounting().load(playerId).orElseThrow());
        }
    }

    @Test
    void abandonedClaimLeavesVotePendingAcrossRestart() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.voteProcessor().process(complete(voteId, playerId, 100L));
            NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes().claim(playerId, voteId).orElseThrow();
            assertTrue(runtime.deferredVotes().claim(playerId, voteId).isEmpty());
            claim.close();
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.PENDING,
                    runtime.deferredVotes().state(playerId, voteId));
            assertTrue(runtime.deferredVotes().claim(playerId, voteId).isPresent());
        }
    }

    @Test
    void quarantinedVoteRemainsFencedAcrossRestart() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.storage().user(playerId).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES,
                    new DataValueString("v1|" + voteId
                            + "|QWxleA|ZXhhbXBsZS50ZXN0|RXhhbXBsZQ|100|true|true|true"));
            assertEquals(NeoForgeDeferredVoteStore.QuarantineResult.QUARANTINED,
                    runtime.deferredVotes().quarantine(playerId, voteId));
            assertTrue(runtime.deferredVotes().pending(playerId).get(0).quarantined());
            assertTrue(runtime.deferredVotes().claim(playerId, voteId).isEmpty());
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeDeferredVote vote = runtime.deferredVotes().pending(playerId).get(0);
            assertEquals(voteId, vote.voteId());
            assertTrue(vote.quarantined());
            assertTrue(runtime.deferredVotes().claim(playerId, voteId).isEmpty());
        }
    }

    @Test
    void legacyAccountingWithoutAcceptedSnapshotFailsClosed() throws IOException {
        writeConfiguration();
        Files.writeString(directory.resolve("Config.yml"), Files.readString(directory.resolve("Config.yml"))
                .replace("AddTotalsOffline: true", "AddTotalsOffline: false"));
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        String legacy = "v1|" + voteId
                + "|QWxleA|ZXhhbXBsZS50ZXN0|RXhhbXBsZQ|100|true|true|false";
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(playerId).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString(legacy));
            NeoForgeVoteSite site = runtime.voteConfiguration().configuredSite("Example").orElseThrow();
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                assertThrows(IllegalStateException.class,
                        () -> claim.completeWithAccounting(runtime.accounting(), site, true));
            }
            assertEquals(0, runtime.accounting().load(playerId).orElseThrow().allTimeTotal());
            assertEquals(1, runtime.deferredVotes().pending(playerId).size());
        }
    }

    @Test
    void completedReceiptReconcilesStalePendingPayload() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        Clock clock = Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.voteProcessor().process(complete(voteId, playerId, 100L));
            String pending = "v1|" + voteId + "|QWxleA|ZXhhbXBsZS50ZXN0|RXhhbXBsZQ|100|true|true|true";
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
            runtime.storage().user(playerId).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString(pending));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            NeoForgeVoteResult retry = runtime.voteProcessor().process(complete(voteId, playerId, 200L));
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED, retry.status());
            assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
        }
    }

    @Test
    void completedReceiptPrecedesMalformedPendingForStateClaimAndDefer() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID completedVote = UUID.randomUUID();
        UUID otherVote = UUID.randomUUID();
        String malformedPending = "not-a-supported-record";
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            SharedVoteIdentity identity = new SharedVoteIdentity(playerId, "Alex", true);
            runtime.players().joined(identity);
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    runtime.voteProcessor().process(complete(completedVote, playerId, 100L)).status());
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, completedVote).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
            runtime.storage().user(playerId).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString(malformedPending));

            NeoForgeDeferredVoteStore store = runtime.deferredVotes();
            NeoForgeVoteSite site = runtime.voteConfiguration().resolveEnabledSite("example.test").orElseThrow();
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    store.state(playerId, completedVote));
            assertTrue(store.claim(playerId, completedVote).isEmpty());
            assertEquals(NeoForgeDeferredVoteStore.Status.ALREADY_COMPLETED,
                    store.defer(identity, input(completedVote), site,
                            accountingDecision(runtime, input(completedVote), site)).status());
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED,
                    runtime.voteProcessor().process(complete(completedVote, playerId, 200L)).status());
            assertEquals(malformedPending, storedPending(runtime, playerId));

            assertThrows(IllegalStateException.class, () -> store.pending(playerId));
            assertThrows(IllegalStateException.class, () -> store.state(playerId, otherVote));
            assertThrows(IllegalStateException.class, () -> store.claim(playerId, otherVote));
            assertThrows(IllegalStateException.class, () -> store.defer(identity, input(otherVote), site,
                    accountingDecision(runtime, input(otherVote), site)));
            assertEquals(malformedPending, storedPending(runtime, playerId));
        }
    }

    @Test
    void completeHonorsMatchingReceiptWithoutRewritingMalformedPending() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        String malformedPending = "not-a-supported-record";
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.voteProcessor().process(complete(voteId, playerId, 100L));
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                runtime.storage().user(playerId).write(UserStorage.SQLITE,
                        NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString(malformedPending));
                runtime.storage().user(playerId).write(UserStorage.SQLITE,
                        NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                        new DataValueString("v1|" + voteId));
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.ALREADY_COMPLETED, claim.complete());
            }
            assertEquals(malformedPending, storedPending(runtime, playerId));
            assertThrows(IllegalStateException.class, () -> runtime.deferredVotes().pending(playerId));
        }
    }

    @Test
    void completeStillSurfacesMalformedPendingWithoutMatchingReceipt() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        String malformedPending = "not-a-supported-record";
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.voteProcessor().process(complete(voteId, playerId, 100L));
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                runtime.storage().user(playerId).write(UserStorage.SQLITE,
                        NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString(malformedPending));
                assertThrows(IllegalStateException.class, claim::complete);
            }
            assertEquals(malformedPending, storedPending(runtime, playerId));
        }
    }

    @Test
    void fullReceiptCapacityPreventsClaimsWithoutDroppingPendingVotesAcrossRestart() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID firstId = UUID.randomUUID();
        UUID secondId = UUID.randomUUID();
        Clock clock = Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 1, 1, 1, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), clock);

            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(firstId, playerId, 100L)).status());
            try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(playerId, firstId).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(secondId, playerId, 200L)).status());
            assertTrue(bounded.claim(playerId, secondId).isEmpty());
            assertEquals(NeoForgeDeferredVoteStore.ClaimStatus.RECEIPT_CAPACITY_REACHED,
                    bounded.claimForReplay(playerId, secondId).status());

            assertEquals(List.of(secondId), bounded.pending(playerId).stream()
                    .map(NeoForgeDeferredVote::voteId).toList());
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    bounded.state(playerId, firstId));
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 1, 1, 1, 1);
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    bounded.state(playerId, firstId));
            assertEquals(List.of(secondId), bounded.pending(playerId).stream()
                    .map(NeoForgeDeferredVote::voteId).toList());
            assertTrue(bounded.claim(playerId, secondId).isEmpty());
        }
    }

    @Test
    void concurrentClaimsReserveGlobalReceiptCapacityAndClosingReleasesIt() throws Exception {
        writeConfiguration();
        UUID firstPlayer = UUID.randomUUID();
        UUID secondPlayer = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(firstPlayer, "First", true));
            runtime.players().joined(new SharedVoteIdentity(secondPlayer, "Second", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 2, 1, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(firstVote, firstPlayer, "First", 100L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(secondVote, secondPlayer, "Second", 200L)).status());

            CountDownLatch ready = new CountDownLatch(2);
            CountDownLatch start = new CountDownLatch(1);
            Optional<NeoForgeDeferredVoteStore.Claim> firstClaim;
            Optional<NeoForgeDeferredVoteStore.Claim> secondClaim;
            try (ExecutorService workers = Executors.newFixedThreadPool(2)) {
                Future<Optional<NeoForgeDeferredVoteStore.Claim>> first = workers.submit(() -> {
                    ready.countDown();
                    if (!start.await(5, TimeUnit.SECONDS)) throw new AssertionError("claim start timed out");
                    return bounded.claim(firstPlayer, firstVote);
                });
                Future<Optional<NeoForgeDeferredVoteStore.Claim>> second = workers.submit(() -> {
                    ready.countDown();
                    if (!start.await(5, TimeUnit.SECONDS)) throw new AssertionError("claim start timed out");
                    return bounded.claim(secondPlayer, secondVote);
                });
                assertTrue(ready.await(5, TimeUnit.SECONDS));
                start.countDown();
                firstClaim = first.get(5, TimeUnit.SECONDS);
                secondClaim = second.get(5, TimeUnit.SECONDS);
            }

            assertEquals(1, (firstClaim.isPresent() ? 1 : 0) + (secondClaim.isPresent() ? 1 : 0));
            UUID deniedPlayer = firstClaim.isPresent() ? secondPlayer : firstPlayer;
            UUID deniedVote = firstClaim.isPresent() ? secondVote : firstVote;
            assertTrue(bounded.claim(deniedPlayer, deniedVote).isEmpty());
            firstClaim.ifPresent(NeoForgeDeferredVoteStore.Claim::close);
            secondClaim.ifPresent(NeoForgeDeferredVoteStore.Claim::close);
            try (NeoForgeDeferredVoteStore.Claim released = bounded.claim(deniedPlayer, deniedVote).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, released.complete());
            }
            assertTrue(bounded.claim(firstClaim.isPresent() ? firstPlayer : secondPlayer,
                    firstClaim.isPresent() ? firstVote : secondVote).isEmpty());
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 2, 1, 1);
            assertEquals(1, bounded.pending(firstPlayer).size() + bounded.pending(secondPlayer).size());
            assertTrue(bounded.claim(firstPlayer, firstVote).isEmpty());
            assertTrue(bounded.claim(secondPlayer, secondVote).isEmpty());
        }
    }

    @Test
    void claimsReservePerUserCapacityAndCompletionReleasesGlobalReservation() throws IOException {
        writeConfiguration();
        UUID firstPlayer = UUID.randomUUID();
        UUID secondPlayer = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        UUID otherVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(firstPlayer, "First", true));
            runtime.players().joined(new SharedVoteIdentity(secondPlayer, "Second", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 2, 3, 1, 2);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(firstVote, firstPlayer, "First", 100L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(secondVote, firstPlayer, "First", 200L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(otherVote, secondPlayer, "Second", 300L)).status());

            try (NeoForgeDeferredVoteStore.Claim first = bounded.claim(firstPlayer, firstVote).orElseThrow()) {
                assertTrue(bounded.claim(firstPlayer, secondVote).isEmpty());
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, first.complete());
                try (NeoForgeDeferredVoteStore.Claim other = bounded.claim(secondPlayer, otherVote).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, other.complete());
                }
                assertTrue(bounded.claim(firstPlayer, secondVote).isEmpty());
            }
        }
    }

    @Test
    void completedReceiptRemainsRecognizableWithoutAgeBasedEviction() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        Instant start = Instant.parse("2026-09-26T12:00:00Z");
        Clock clock = Clock.fixed(start, ZoneOffset.UTC);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            runtime.voteProcessor().process(complete(voteId, playerId, 100L));
            try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                    .claim(playerId, voteId).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
        }

        Clock muchLater = Clock.fixed(start.plusSeconds(365L * 24L * 60L * 60L), ZoneOffset.UTC);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, muchLater)) {
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(playerId, voteId));
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED,
                    runtime.voteProcessor().process(complete(voteId, playerId, 200L)).status());
        }
    }

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

    @Test
    void malformedQueueIsIsolatedDuringGlobalCapacityCount() throws IOException {
        writeConfiguration();
        UUID corruptPlayer = UUID.randomUUID();
        UUID healthyPlayer = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(corruptPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES, new DataValueString("not-a-supported-record"));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(healthyPlayer, "Healthy", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 2, 3);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());

            NeoForgeVoteResult result = processor.process(
                    complete(UUID.randomUUID(), healthyPlayer, "Healthy", 100L));

            assertEquals(NeoForgeVoteResult.Status.DEFERRED, result.status());
            assertEquals(1, bounded.pending(healthyPlayer).size());
            assertThrows(IllegalStateException.class, () -> bounded.pending(corruptPlayer));
            assertNoAccounting(runtime.accounting().load(healthyPlayer).orElseThrow());
        }
    }

    @Test
    void malformedCompletionReceiptsRemainObservableWithoutDisablingHealthyUsers() throws IOException {
        writeConfiguration();
        UUID corruptPlayer = UUID.randomUUID();
        UUID healthyPlayer = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(corruptPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("not-a-supported-receipt"));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(healthyPlayer, "Healthy", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 2, 3, 2, 3);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());

            assertEquals(NeoForgeVoteResult.Status.DEFERRED, processor.process(
                    complete(UUID.randomUUID(), healthyPlayer, "Healthy", 100L)).status());
            assertEquals(1, bounded.pending(healthyPlayer).size());
            assertThrows(IllegalStateException.class, () -> bounded.pending(corruptPlayer));
        }
    }

    @Test
    void fullReleaseReceiptCapacityPreservesLiveTombstonesAndActiveCompletion() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.rewardReplay().ifPresent(NeoForgeRewardReplayService::stopAdmission);
            runtime.players().joined(new SharedVoteIdentity(playerId, "Alex", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 2, 2, 2, 2, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());

            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(firstVote, playerId, 100L)).status());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(secondVote, playerId, 200L)).status());
            try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(playerId, firstVote).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
            try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(playerId, secondVote).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }

            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.RELEASED,
                    bounded.release(playerId, firstVote));
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.RELEASE_CAPACITY_REACHED,
                    bounded.release(playerId, secondVote));
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED,
                    bounded.release(playerId, firstVote));
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    bounded.state(playerId, secondVote));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 2, 2, 2, 2, 1);
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED,
                    bounded.release(playerId, firstVote));
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.RELEASE_CAPACITY_REACHED,
                    bounded.release(playerId, secondVote));
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    bounded.state(playerId, secondVote));
        }
    }

    @Test
    void releasedReceiptCapacityIsGlobalAndExpiredRowsArePruned() throws IOException {
        writeConfiguration();
        UUID firstPlayer = UUID.randomUUID();
        UUID secondPlayer = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(firstPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("v2|" + firstVote + "|" + System.currentTimeMillis()));
            runtime.storage().user(secondPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("v1|" + secondVote));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 2, 2, 2, 2, 2, 1);

            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.RELEASE_CAPACITY_REACHED,
                    bounded.release(secondPlayer, secondVote));

            runtime.storage().user(firstPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("v2|" + firstVote + "|"
                            + (System.currentTimeMillis() - TimeUnit.DAYS.toMillis(8))));
            NeoForgeDeferredVoteStore restarted = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 2, 2, 2, 2, 2, 1);
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.RELEASED,
                    restarted.release(secondPlayer, secondVote));
            String firstReceipts = runtime.storage().user(firstPlayer).readRow(UserStorage.SQLITE).stream()
                    .filter(column -> column.getName().equalsIgnoreCase(
                            NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES))
                    .findFirst().orElseThrow().getValue().getString();
            assertEquals("", firstReceipts);
        }
    }

    @Test
    void repeatedReleaseRenewsExpiredTombstoneAcrossRestart() throws IOException {
        writeConfiguration();
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        long expired = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(8);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(playerId).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("v2|" + voteId + "|" + expired));

            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED,
                    runtime.deferredVotes().release(playerId, voteId));
            String renewed = runtime.storage().user(playerId).readRow(UserStorage.SQLITE).stream()
                    .filter(column -> column.getName().equalsIgnoreCase(
                            NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES))
                    .findFirst().orElseThrow().getValue().getString();
            assertTrue(Long.parseLong(renewed.split("\\|", -1)[2]) > expired);
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertEquals(NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED,
                    runtime.deferredVotes().release(playerId, voteId));
        }
    }

    @Test
    void malformedCompletionReceiptsDoNotConsumePendingCapacity() throws IOException {
        writeConfiguration();
        UUID corruptPlayer = UUID.randomUUID();
        UUID healthyPlayer = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(corruptPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new DataValueString("not-a-supported-receipt"));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(healthyPlayer, "Healthy", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 1, 1, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());

            assertEquals(NeoForgeVoteResult.Status.DEFERRED, processor.process(
                    complete(UUID.randomUUID(), healthyPlayer, "Healthy", 100L)).status());
            assertEquals(1, bounded.pending(healthyPlayer).size());
            assertThrows(IllegalStateException.class, () -> bounded.pending(corruptPlayer));
        }
    }

    @Test
    void malformedPendingVotesDoNotConsumeReceiptCapacity() throws IOException {
        writeConfiguration();
        UUID corruptPlayer = UUID.randomUUID();
        UUID healthyPlayer = UUID.randomUUID();
        UUID healthyVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(corruptPlayer).write(UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.DEFERRED_VOTES,
                    new DataValueString("not-a-supported-record"));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(healthyPlayer, "Healthy", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 2, 1, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), Clock.systemUTC());
            assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                    processor.process(complete(healthyVote, healthyPlayer, "Healthy", 100L)).status());

            try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(healthyPlayer, healthyVote).orElseThrow()) {
                assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
            }
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    bounded.state(healthyPlayer, healthyVote));
            assertThrows(IllegalStateException.class, () -> bounded.pending(corruptPlayer));
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

    private static void insertHistoricalUsers(Path database, int count) throws SQLException {
        String sql = "INSERT INTO `" + NeoForgeRuntime.USER_TABLE_NAME
                + "` (`UUID`, `PlayerName`) VALUES (?, ?)";
        try (Connection connection = DriverManager.getConnection("jdbc:sqlite:" + database.toAbsolutePath());
                PreparedStatement statement = connection.prepareStatement(sql)) {
            connection.setAutoCommit(false);
            for (int index = 0; index < count; index++) {
                statement.setString(1, new UUID(1L, index + 1L).toString());
                statement.setString(2, "Historical" + index);
                statement.addBatch();
            }
            statement.executeBatch();
            connection.commit();
        }
    }

    private static NeoForgeVoteRequest complete(UUID voteId, UUID playerId, long voteTime) {
        return complete(voteId, playerId, "Alex", voteTime);
    }

    private static SharedVoteInput input(UUID voteId) {
        return new SharedVoteInput(voteId, "Alex", "example.test", 100L,
                true, true, false, false, true);
    }

    private static NeoForgeVoteAccountingDecision accountingDecision(NeoForgeRuntime runtime,
            SharedVoteInput input, NeoForgeVoteSite site) {
        return NeoForgeVoteAccountingDecision.capture(input, runtime.voteConfiguration().policyFor(site),
                true, runtime.voteConfiguration().pointsOnVote(), runtime.voteConfiguration().limitVotePoints());
    }

    private static String storedPending(NeoForgeRuntime runtime, UUID playerId) {
        return runtime.storage().user(playerId).readRow(UserStorage.SQLITE).stream()
                .filter(column -> column.getName().equalsIgnoreCase(NeoForgeDeferredVoteStore.DEFERRED_VOTES))
                .findFirst().orElseThrow().getValue().getString();
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
