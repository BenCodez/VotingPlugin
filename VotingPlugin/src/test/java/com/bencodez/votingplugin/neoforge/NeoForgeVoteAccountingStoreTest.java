package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.DriverManager;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.util.SqliteNativeLibrary;

class NeoForgeVoteAccountingStoreTest {
    @TempDir Path directory;

    @Test
    void migratesBootstrapTableAndPersistsSharedAccountingAcrossRestart() throws Exception {
        createBootstrapOnlyTable();
        writeConfiguration(3, 5);

        UUID playerId = UUID.randomUUID();
        long voteTime = 123_456L;
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteSite site = runtime.voteConfiguration().resolveEnabledSite("service.example").orElseThrow();
            SharedVoteIdentity identity = new SharedVoteIdentity(playerId, "Alex", true);
            SharedVoteInput input = vote("Alex", voteTime, true, true);
            NeoForgeVoteAccount account = runtime.accounting().apply(identity, input,
                    runtime.voteConfiguration().policyFor(site), site,
                    runtime.voteConfiguration().pointsOnVote(), runtime.voteConfiguration().limitVotePoints());

            assertAccount(account, "Alex", 1, 1, 1, 1, 3, voteTime);
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteAccount persisted = runtime.accounting().load(playerId).orElseThrow();
            assertAccount(persisted, "Alex", 1, 1, 1, 1, 3, voteTime);

            NeoForgeVoteSite site = runtime.voteConfiguration().resolveEnabledSite("service.example").orElseThrow();
            NeoForgeVoteAccount updated = runtime.accounting().apply(
                    new SharedVoteIdentity(playerId, "AlexRenamed", true),
                    vote("AlexRenamed", 234_567L, true, true),
                    runtime.voteConfiguration().policyFor(site), site,
                    runtime.voteConfiguration().pointsOnVote(), runtime.voteConfiguration().limitVotePoints());
            assertAccount(updated, "AlexRenamed", 2, 2, 2, 2, 5, 234_567L);
        }
    }

    @Test
    void sharedPolicyControlsTotalsAndPointsWithoutChangingTimestampPersistence() throws IOException {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteSite site = site();
            List<Scenario> scenarios = List.of(
                    new Scenario("real", true, true, true, true, true, 1, 2),
                    new Scenario("fake-disabled", false, true, false, true, true, 0, 0),
                    new Scenario("fake-enabled", false, true, true, true, true, 1, 2),
                    new Scenario("vote-add-totals-off", true, false, true, true, true, 0, 0),
                    new Scenario("global-totals-off", true, true, true, false, true, 0, 2),
                    new Scenario("offline-totals-off", true, true, true, true, false, 0, 2),
                    new Scenario("online-totals-on", true, true, true, true, true, 1, 2));

            long timestamp = 1_000;
            for (Scenario scenario : scenarios) {
                UUID uuid = UUID.randomUUID();
                boolean online = !scenario.name.equals("offline-totals-off");
                SharedVotePolicy policy = new SharedVotePolicy(scenario.countFakeVotes,
                        scenario.globalAddTotals, scenario.addTotalsOffline, false, false);
                NeoForgeVoteAccount account = runtime.accounting().apply(
                        new SharedVoteIdentity(uuid, scenario.name, online),
                        vote(scenario.name, timestamp, scenario.realVote, scenario.voteAddTotals),
                        policy, site, 2, -1);

                assertEquals(scenario.expectedTotal, account.allTimeTotal(), scenario.name);
                assertEquals(scenario.expectedTotal, account.monthTotal(), scenario.name);
                assertEquals(scenario.expectedTotal, account.dailyTotal(), scenario.name);
                assertEquals(scenario.expectedTotal, account.weeklyTotal(), scenario.name);
                assertEquals(scenario.expectedPoints, account.points(), scenario.name);
                assertEquals(timestamp, account.lastVote(site.key()), scenario.name);
                timestamp++;
            }
        }
    }

    @Test
    void missingUserIsNotMaterializedByRead() throws IOException {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertFalse(runtime.accounting().load(UUID.randomUUID()).isPresent());
            assertTrue(runtime.storage().enumerateUsers().isEmpty());
        }
    }

    @Test
    void updatingAnotherSiteRetainsMalformedLastVoteAsBukkitZeroTimestamp() throws IOException {
        writeVoteSites("MalformedSite", "Other");
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            UUID uuid = UUID.randomUUID();
            runtime.storage().user(uuid).write(UserStorage.SQLITE, "LastVotes",
                    new DataValueString("MalformedSite//not-a-number"));
            NeoForgeVoteSite otherSite = new NeoForgeVoteSite("Other", "Other", "other.example",
                    0, 0, true, false, false, 0, false);

            NeoForgeVoteAccount account = runtime.accounting().apply(
                    new SharedVoteIdentity(uuid, "Alex", true),
                    new SharedVoteInput(UUID.randomUUID(), "Alex", "other.example", 55L,
                            true, true, false, false, true),
                    new SharedVotePolicy(true, true, true, false, false), otherSite, 1, -1);

            assertTrue(account.lastVotes().containsKey("MalformedSite"));
            assertEquals(0L, account.lastVote("MalformedSite"));
            assertEquals(55L, account.lastVote("Other"));
        }
    }

    @Test
    void updatingAnotherSitePrunesRemovedLastVoteAcrossRestart() throws IOException {
        writeVoteSites("CurrentSite", "UpdateSite");
        UUID uuid = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(uuid).write(UserStorage.SQLITE, "LastVotes",
                    new DataValueString("CurrentSite//100%line%RemovedSite//200"));
            NeoForgeVoteSite updateSite = runtime.voteConfiguration()
                    .resolveEnabledSite("updatesite.example").orElseThrow();

            NeoForgeVoteAccount account = runtime.accounting().apply(
                    new SharedVoteIdentity(uuid, "Alex", true),
                    new SharedVoteInput(UUID.randomUUID(), "Alex", "updatesite.example", 300L,
                            true, true, false, false, true),
                    new SharedVotePolicy(true, true, true, false, false), updateSite, 1, -1);

            assertEquals(100L, account.lastVote("CurrentSite"));
            assertEquals(300L, account.lastVote("UpdateSite"));
            assertFalse(account.lastVotes().containsKey("RemovedSite"));
        }

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteAccount persisted = runtime.accounting().load(uuid).orElseThrow();
            assertEquals(100L, persisted.lastVote("CurrentSite"));
            assertEquals(300L, persisted.lastVote("UpdateSite"));
            assertFalse(persisted.lastVotes().containsKey("RemovedSite"));
        }
    }

    private void createBootstrapOnlyTable() throws Exception {
        Files.createDirectories(directory);
        SqliteNativeLibrary.ensureAvailable(directory.resolve("libraries"));
        Class.forName("org.sqlite.JDBC");
        try (var connection = DriverManager.getConnection("jdbc:sqlite:" + directory.resolve("VotingPlugin.db"));
                var statement = connection.createStatement()) {
            statement.executeUpdate("CREATE TABLE VotingPlugin_NeoForgeUsers (uuid VARCHAR(37) PRIMARY KEY)");
        }
    }

    private void writeConfiguration(int points, int pointLimit) throws IOException {
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AddTotals: true
                AddTotalsOffline: true
                CountFakeVotes: true
                ProcessRewards: true
                PointsOnVote: %d
                LimitVotePoints: %d
                """.formatted(points, pointLimit));
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  Example:
                    Enabled: true
                    ServiceSite: service.example
                    VoteDelay: 24h
                """);
    }

    private void writeVoteSites(String... siteKeys) throws IOException {
        StringBuilder yaml = new StringBuilder("VoteSites:\n");
        for (String siteKey : siteKeys) {
            yaml.append("  ").append(siteKey).append(":\n")
                    .append("    Enabled: true\n")
                    .append("    ServiceSite: ").append(siteKey.toLowerCase(Locale.ROOT)).append(".example\n")
                    .append("    VoteDelay: 24h\n");
        }
        Files.writeString(directory.resolve("VoteSites.yml"), yaml);
    }

    private static SharedVoteInput vote(String name, long time, boolean realVote, boolean addTotals) {
        return new SharedVoteInput(UUID.randomUUID(), name, "service.example", time,
                realVote, addTotals, false, false, true);
    }

    private static NeoForgeVoteSite site() {
        return new NeoForgeVoteSite("Example", "Example", "service.example",
                0, 24 * 60 * 60 * 1000L, true, false, false, 0, false);
    }

    private static void assertAccount(NeoForgeVoteAccount account, String name, int allTime,
            int month, int daily, int weekly, int points, long lastVote) {
        assertEquals(name, account.playerName());
        assertEquals(allTime, account.allTimeTotal());
        assertEquals(month, account.monthTotal());
        assertEquals(daily, account.dailyTotal());
        assertEquals(weekly, account.weeklyTotal());
        assertEquals(points, account.points());
        assertEquals(lastVote, account.lastVote("Example"));
    }

    private record Scenario(String name, boolean realVote, boolean voteAddTotals,
            boolean countFakeVotes, boolean globalAddTotals, boolean addTotalsOffline,
            int expectedTotal, int expectedPoints) {}
}
