package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.vote.SharedVotePolicy;

class NeoForgeVoteConfigurationTest {
    @TempDir Path directory;

    @Test
    void packagedDefaultsExposeCountingPolicyAndDisabledExampleSite() throws IOException {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteConfiguration config = runtime.voteConfiguration();
            assertTrue(config.addTotals());
            assertTrue(config.addTotalsOffline());
            assertTrue(config.countFakeVotes());
            assertTrue(config.processRewards());
            assertFalse(config.allowUnjoined());
            assertTrue(config.allowUnjoinedCheckServer());
            assertEquals(1, config.pointsOnVote());
            assertEquals(-1, config.limitVotePoints());
            assertFalse(config.offlineVoteLimitEnabled());
            assertFalse(config.voteSites().isEmpty());
            assertTrue(config.voteSites().stream().noneMatch(NeoForgeVoteSite::enabled));
            assertTrue(config.resolveEnabledSite("PlanetMinecraft.com").isEmpty());
        }
    }

    @Test
    void customConfigurationResolvesOnlyEnabledSitesWithBukkitIdentifiers() throws IOException {
        writeConfig(false, true, false, false, 3, 9, true, 2);
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  PrimaryKey:
                    Enabled: true
                    Name: Friendly Name
                    ServiceSite: Example.COM
                    VoteDelay: 90m
                    WaitUntilVoteDelay: true
                    VoteDelayDaily: true
                    VoteDelayDailyHour: 6
                    ForceOffline: true
                  DisabledKey:
                    Enabled: false
                    Name: Disabled Name
                    ServiceSite: disabled.example
                    VoteDelay: 2
                    VoteDelayMin: 30
                  MissingService:
                    Enabled: true
                    Name: Missing Service
                    VoteDelay: 24h
                """);

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteConfiguration config = runtime.voteConfiguration();
            NeoForgeVoteSite site = config.resolveEnabledSite("example.com").orElseThrow();
            assertEquals("PrimaryKey", site.key());
            assertEquals(site, config.resolveEnabledSite("primarykey").orElseThrow());
            assertEquals(site, config.resolveEnabledSite("FRIENDLY NAME").orElseThrow());
            assertEquals(90 * 60 * 1000L, site.voteDelayMillis());
            assertTrue(site.waitUntilVoteDelay());
            assertTrue(site.voteDelayDaily());
            assertEquals(6, site.voteDelayDailyHour());
            assertTrue(config.resolveEnabledSite("disabled.example").isEmpty());
            assertTrue(config.resolveEnabledSite("MissingService").isEmpty());
            assertTrue(config.resolveEnabledSite("unknown.example").isEmpty());
            assertTrue(config.resolveEnabledSite(null).isEmpty());

            assertFalse(config.addTotals());
            assertTrue(config.addTotalsOffline());
            assertFalse(config.countFakeVotes());
            assertFalse(config.processRewards());
            assertEquals(3, config.pointsOnVote());
            assertEquals(9, config.limitVotePoints());
            assertTrue(config.offlineVoteLimitEnabled());
            assertEquals(2, config.offlineVoteLimitAmount());
            SharedVotePolicy policy = config.policyFor(site);
            assertFalse(policy.countFakeVotes());
            assertFalse(policy.addTotals());
            assertTrue(policy.addTotalsOffline());
            assertFalse(policy.processRewards());
            assertTrue(policy.giveOfflineRewards());
        }
    }

    @Test
    void restartingReloadsConfigurationFromDisk() throws IOException {
        writeConfig(true, true, true, true, 1, -1, false, 5);
        Files.writeString(directory.resolve("VoteSites.yml"), siteYaml("old.example"));
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertTrue(runtime.voteConfiguration().resolveEnabledSite("old.example").isPresent());
        }

        writeConfig(false, false, false, false, 4, 20, true, 7);
        Files.writeString(directory.resolve("VoteSites.yml"), siteYaml("new.example"));
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteConfiguration config = runtime.voteConfiguration();
            assertTrue(config.resolveEnabledSite("old.example").isEmpty());
            assertTrue(config.resolveEnabledSite("new.example").isPresent());
            assertFalse(config.addTotals());
            assertEquals(4, config.pointsOnVote());
        }
    }

    @Test
    void resolutionUsesBukkitPriorityLegacyOfflineFallbackAndReservedKeyRules() throws IOException {
        writeConfig(true, true, true, true, 1, -1, false, 5);
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  LowPriority:
                    Enabled: true
                    ServiceSite: shared.example
                    Priority: 1
                  HigherPriority:
                    Enabled: true
                    ServiceSite: shared.example
                    Priority: 20
                    GiveOffline: true
                  'null':
                    Enabled: true
                    ServiceSite: reserved.example
                    Priority: 100
                """);

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteConfiguration config = runtime.voteConfiguration();
            NeoForgeVoteSite resolved = config.resolveEnabledSite("shared.example").orElseThrow();
            assertEquals("HigherPriority", resolved.key());
            assertTrue(config.policyFor(resolved).giveOfflineRewards());
            assertTrue(config.resolveEnabledSite("reserved.example").isEmpty());
            assertTrue(config.resolveEnabledSite("[bad]").isEmpty());
            assertTrue(config.voteSites().stream().noneMatch(site -> site.key().equalsIgnoreCase("null")));
        }
    }

    @Test
    void caseInsensitiveModeLoadsMixedCaseKeysAndIgnoresInvalidDisabledDelay() throws IOException {
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                CaseInsensitiveYMLFiles: true
                addtotals: false
                pointsonvote: 8
                offlinevoteslimit:
                  enabled: true
                  amount: 4
                """);
        Files.writeString(directory.resolve("VoteSites.yml"), """
                votesites:
                  MixedCase:
                    enabled: true
                    name: Mixed Name
                    servicesite: mixed.example
                    priority: 7
                    votedelay: 12h
                    forceoffline: true
                  DisabledInvalid:
                    enabled: false
                    servicesite: disabled.example
                    votedelay: invalid duration
                """);

        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            NeoForgeVoteConfiguration config = runtime.voteConfiguration();
            NeoForgeVoteSite site = config.resolveEnabledSite("MIXED.EXAMPLE").orElseThrow();
            assertEquals("Mixed Name", site.displayName());
            assertEquals(12 * 60 * 60 * 1000L, site.voteDelayMillis());
            assertFalse(config.addTotals());
            assertEquals(8, config.pointsOnVote());
            assertTrue(config.offlineVoteLimitEnabled());
            assertEquals(4, config.offlineVoteLimitAmount());
            assertTrue(config.policyFor(site).giveOfflineRewards());
        }
    }

    private void writeConfig(boolean addTotals, boolean addTotalsOffline, boolean countFakeVotes,
            boolean processRewards, int points, int pointLimit, boolean offlineLimit, int offlineAmount)
            throws IOException {
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AllowUnjoined: true
                AllowUnJoinedCheckServer: false
                AddTotals: %s
                AddTotalsOffline: %s
                CountFakeVotes: %s
                ProcessRewards: %s
                PointsOnVote: %d
                LimitVotePoints: %d
                OfflineVotesLimit:
                  Enabled: %s
                  Amount: %d
                """.formatted(addTotals, addTotalsOffline, countFakeVotes, processRewards,
                        points, pointLimit, offlineLimit, offlineAmount));
    }

    private static String siteYaml(String serviceSite) {
        return """
                VoteSites:
                  TestSite:
                    Enabled: true
                    Name: Test Site
                    ServiceSite: %s
                    VoteDelay: 24h
                """.formatted(serviceSite);
    }
}
