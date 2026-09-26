package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

class NeoForgeVoteProcessorTest {
    @TempDir Path directory;

    @Test
    void accountingOnlyVotePersistsThroughRestart() throws IOException {
        writeConfiguration(true, true, true, true);
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteResult result = runtime.voteProcessor().process(request(uuid, "Service", 100L, true, true, true));
            assertEquals(NeoForgeVoteResult.Status.ACCOUNTED, result.status());
            assertAccount(result.account(), 1, 1);
            assertEquals(100L, result.account().lastVotes().get("EnabledSite"));
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteAccount stored = runtime.accounting().load(uuid).orElseThrow();
            assertAccount(stored, 1, 1);
            assertEquals(100L, stored.lastVotes().get("EnabledSite"));
        }
    }

    @Test
    void unknownAndDisabledSitesDoNotMutateAccounting() throws IOException {
        writeConfiguration(true, true, true, true);
        UUID unknown = UUID.randomUUID();
        UUID disabled = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(unknown, "Unknown", true));
            runtime.players().joined(new SharedVoteIdentity(disabled, "Disabled", true));
            assertEquals(NeoForgeVoteResult.Status.UNKNOWN_SITE,
                    runtime.voteProcessor().process(request(unknown, "Unknown", 100L, true, true, true,
                            "MissingService")).status());
            assertEquals(NeoForgeVoteResult.Status.UNKNOWN_SITE,
                    runtime.voteProcessor().process(request(disabled, "Disabled", 100L, true, true, true,
                            "DisabledService")).status());
            assertTrue(runtime.accounting().load(unknown).isEmpty());
            assertTrue(runtime.accounting().load(disabled).isEmpty());
        }
    }

    @Test
    void fakeVoteUsesSharedCountingPolicy() throws IOException {
        writeConfiguration(true, true, false, true);
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteResult result = runtime.voteProcessor().process(request(uuid, "Service", 100L, false, true, true));
            assertEquals(NeoForgeVoteResult.Status.ACCOUNTED, result.status());
            assertAccount(result.account(), 0, 0);
            assertEquals(100L, result.account().lastVotes().get("EnabledSite"));
        }
    }

    @Test
    void totalsDisabledStillAwardsConfiguredPoints() throws IOException {
        writeConfiguration(true, false, true, true);
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteResult result = runtime.voteProcessor().process(
                    request(uuid, "Service", 100L, true, true, true));
            assertAccount(result.account(), 0, 1);
        }
    }

    @Test
    void offlineTotalsFollowConfiguredPolicyWhilePointsRemainEnabled() throws IOException {
        writeConfiguration(true, true, true, false);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteResult result = runtime.voteProcessor().process(
                    request(UUID.randomUUID(), "Service", 100L, true, true, false));
            assertAccount(result.account(), 0, 1);
        }
    }

    @Test
    void playerDirectoryIsAuthoritativeForOnlineTotals() throws IOException {
        writeConfiguration(true, true, true, false);
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteResult result = runtime.voteProcessor().process(
                    request(uuid, "Alex", 100L, true, true, false));
            assertAccount(result.account(), 1, 1);
        }
    }

    @Test
    void dailyVoteDelayUsesConfiguredTimeZoneForCurrentTime() throws IOException {
        writeConfiguration(true, true, true, true);
        Files.writeString(directory.resolve("Config.yml"), "TimeZone: America/New_York\n",
                StandardOpenOption.APPEND);
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  EnabledSite:
                    Enabled: true
                    ServiceSite: Service
                    VoteDelayDaily: true
                    VoteDelayDailyHour: 0
                    WaitUntilVoteDelay: true
                """);
        Clock clock = Clock.fixed(Instant.parse("2026-01-02T03:00:00Z"), ZoneOffset.UTC);
        long previousVote = Instant.parse("2026-01-01T23:30:00Z").toEpochMilli();
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory, clock)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            assertEquals(NeoForgeVoteResult.Status.ACCOUNTED,
                    runtime.voteProcessor().process(request(uuid, "Alex", previousVote, true, true, true)).status());
            NeoForgeVoteResult delayed = runtime.voteProcessor().process(
                    request(uuid, "Alex", clock.millis(), true, true, true));
            assertEquals(NeoForgeVoteResult.Status.VOTE_DELAY_ACTIVE, delayed.status());
            assertEquals(previousVote, runtime.accounting().load(uuid).orElseThrow()
                    .lastVotes().get("EnabledSite"));
        }
    }

    @Test
    void voteDelayDecisionAndMutationAreAtomic() throws IOException {
        writeConfiguration(true, true, true, true);
        UUID uuid = UUID.randomUUID();
        long voteTime = System.currentTimeMillis();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteResult first = runtime.voteProcessor().process(request(uuid, "Service", voteTime, true, true, true));
            assertEquals(NeoForgeVoteResult.Status.ACCOUNTED, first.status());
            NeoForgeVoteResult second = runtime.voteProcessor().process(request(uuid, "Service", voteTime + 1L, true, true, true));
            assertEquals(NeoForgeVoteResult.Status.VOTE_DELAY_ACTIVE, second.status());
            NeoForgeVoteAccount stored = runtime.accounting().load(uuid).orElseThrow();
            assertAccount(stored, 1, 1);
            assertEquals(voteTime, stored.lastVotes().get("EnabledSite"));
        }
    }

    @Test
    void completeRewardBearingVoteIsDeferredBeforeAnyAccountingMutation() throws IOException {
        writeConfiguration(true, true, true, true);
        UUID uuid = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.players().joined(new SharedVoteIdentity(uuid, "Alex", true));
            NeoForgeVoteRequest request = new NeoForgeVoteRequest(voteId, uuid, "Alex", "Service", 100L,
                    true, true, true, NeoForgeVoteRequest.Scope.COMPLETE);
            NeoForgeVoteResult result = runtime.voteProcessor().process(request);
            assertEquals(NeoForgeVoteResult.Status.DEFERRED, result.status());
            assertFalse(result.accountingMutated());
            assertTrue(result.durablyRetained());
            assertNoAccounting(runtime.accounting().load(uuid).orElseThrow());
            assertEquals(voteId, runtime.deferredVotes().pending(uuid).get(0).voteId());
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertNoAccounting(runtime.accounting().load(uuid).orElseThrow());
            assertEquals(voteId, runtime.deferredVotes().pending(uuid).get(0).voteId());
        }
    }

    @Test
    void unknownOnlineIdentityAndStoppedRuntimeAreRejected() throws IOException {
        writeConfiguration(false, true, true, true);
        UUID uuid = UUID.randomUUID();
        NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
        NeoForgeVoteProcessor processor = runtime.voteProcessor();
        assertEquals(NeoForgeVoteResult.Status.UNKNOWN_PLAYER,
                processor.process(request(uuid, "Alex", 100L, true, true, true)).status());
        assertTrue(runtime.accounting().load(uuid).isEmpty());
        runtime.close();
        assertEquals(NeoForgeVoteResult.Status.STOPPED,
                processor.process(request(uuid, "Alex", 100L, true, true, true)).status());
    }

    private void writeConfiguration(boolean allowUnjoined, boolean addTotals,
            boolean countFakeVotes, boolean addTotalsOffline) throws IOException {
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AllowUnjoined: %s
                AddTotals: %s
                AddTotalsOffline: %s
                CountFakeVotes: %s
                PointsOnVote: 1
                LimitVotePoints: -1
                """.formatted(allowUnjoined, addTotals, addTotalsOffline, countFakeVotes));
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  EnabledSite:
                    Enabled: true
                    Name: Enabled
                    ServiceSite: Service
                    VoteDelay: 24
                    WaitUntilVoteDelay: true
                  DisabledSite:
                    Enabled: false
                    Name: Disabled
                    ServiceSite: DisabledService
                    VoteDelay: 24
                    WaitUntilVoteDelay: true
                """);
    }

    private static NeoForgeVoteRequest request(UUID uuid, String service, long time,
            boolean real, boolean addTotals, boolean online) {
        return request(uuid, service, time, real, addTotals, online, "Service");
    }

    private static NeoForgeVoteRequest request(UUID uuid, String name, long time,
            boolean real, boolean addTotals, boolean online, String serviceSite) {
        return new NeoForgeVoteRequest(UUID.randomUUID(), uuid, name, serviceSite, time, real, addTotals, online,
                NeoForgeVoteRequest.Scope.ACCOUNTING_ONLY);
    }

    private static void assertAccount(NeoForgeVoteAccount account, int totals, int points) {
        assertEquals(totals, account.allTimeTotal());
        assertEquals(totals, account.monthTotal());
        assertEquals(totals, account.dailyTotal());
        assertEquals(totals, account.weeklyTotal());
        assertEquals(points, account.points());
    }

    private static void assertNoAccounting(NeoForgeVoteAccount account) {
        assertAccount(account, 0, 0);
        assertTrue(account.lastVotes().isEmpty());
    }
}
