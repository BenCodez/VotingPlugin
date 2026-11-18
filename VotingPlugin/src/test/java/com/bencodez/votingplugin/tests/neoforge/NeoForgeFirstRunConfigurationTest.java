package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.neoforge.*;

class NeoForgeFirstRunConfigurationTest {
    @TempDir Path directory;

    @Test
    void freshConfigurationCanCompleteOneSupportedRewardAcrossRestart() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            assertFalse(runtime.config().node("CloseInventoryOnVote").getBoolean(true));
            assertFalse(runtime.config().node("UseVoteStreaks").getBoolean(true));
            assertEquals("NONE", runtime.config().node("VoteBroadcast", "Type").getString());
            assertTrue(runtime.voteConfiguration().voteSites().stream().noneMatch(NeoForgeVoteSite::enabled));
        }
        Path sitesFile = directory.resolve("VoteSites.yml");
        var loader = YamlConfigurationLoader.builder().path(sitesFile).build();
        var sites = loader.load();
        sites.node("VoteSites", "ExampleVoteSite", "Enabled").set(true);
        loader.save(sites);
        UUID player = UUID.randomUUID();
        UUID vote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            NeoForgePlayerDirectoryAccess.joined(runtime.players(), new SharedVoteIdentity(player, "Alex", true));
            assertEquals(NeoForgeVoteResult.Status.DEFERRED, runtime.voteProcessor().process(
                    new NeoForgeVoteRequest(vote, player, "Alex", "PlanetMinecraft.com", 100L,
                            true, true, true, NeoForgeVoteRequest.Scope.COMPLETE)).status());
            assertEquals(vote, runtime.deferredVotes().pending(player).get(0).voteId());
        }
        AtomicInteger actions = new AtomicInteger();
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory);
                NeoForgeRewardReplayService replay = NeoForgeRewardReplayServiceAccess.create(
                        runtime.voteConfiguration(), new NeoForgeRewardConfigurationAccess(runtime.config(),
                                runtime.voteSites(), runtime.specialRewards()), runtime.accounting(),
                        runtime.deferredVotes(), runtime.players(), (occurrence, plan) -> {
                            assertEquals(vote, occurrence.voteId());
                            assertEquals(1, plan.actions().size());
                            assertEquals(NeoForgeRewardPlan.ActionType.CONSOLE_COMMAND, plan.actions().get(0).type());
                            actions.incrementAndGet();
                            return CompletableFuture.completedFuture(null);
                        })) {
            assertEquals(NeoForgeRewardReplayService.Status.COMPLETED,
                    replay.replayOnce().get(5, TimeUnit.SECONDS).get(0).status());
            assertEquals(1, actions.get());
            assertEquals(1, runtime.accounting().load(player).orElseThrow().allTimeTotal());
            assertEquals(1, runtime.accounting().load(player).orElseThrow().points());
            assertTrue(runtime.deferredVotes().pending(player).isEmpty());
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED, runtime.voteProcessor().process(
                    new NeoForgeVoteRequest(vote, player, "Alex", "PlanetMinecraft.com", 100L,
                            true, true, true, NeoForgeVoteRequest.Scope.COMPLETE)).status());
            assertEquals(1, runtime.accounting().load(player).orElseThrow().allTimeTotal());
        }
    }

    @Test
    void preservesEveryExistingFileByteForByteAndStillBlocksUnsupportedSettings() throws Exception {
        String config = "# Operator configuration\nDataStorage: SQLITE\nCloseInventoryOnVote: true\n";
        Files.writeString(directory.resolve("Config.yml"), config);
        Files.writeString(directory.resolve("VoteSites.yml"), "# Existing sites\nVoteSites: {}\n");
        Files.writeString(directory.resolve("SpecialRewards.yml"), "# Existing rewards\nFirstVote: {}\n");
        List<Path> files = List.of(directory.resolve("Config.yml"), directory.resolve("VoteSites.yml"),
                directory.resolve("SpecialRewards.yml"));
        List<String> before = files.stream().map(path -> {
            try { return Files.readString(path); } catch (Exception e) { throw new RuntimeException(e); }
        }).toList();
        for (int restart = 0; restart < 2; restart++) {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
                assertTrue(runtime.config().node("CloseInventoryOnVote").getBoolean());
                assertTrue(runtime.specialRewards().node("VoteMilestones").virtual());
            }
        }
        for (int i = 0; i < files.size(); i++) assertEquals(before.get(i), Files.readString(files.get(i)));
    }

    @Test
    void existingUnsupportedSettingKeepsPendingVoteAndExplainsWhyAfterRestart() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) { }
        Path configFile = directory.resolve("Config.yml");
        var configLoader = YamlConfigurationLoader.builder().path(configFile).build();
        var config = configLoader.load();
        config.node("CloseInventoryOnVote").set(true);
        configLoader.save(config);
        Path sitesFile = directory.resolve("VoteSites.yml");
        var sitesLoader = YamlConfigurationLoader.builder().path(sitesFile).build();
        var sites = sitesLoader.load();
        sites.node("VoteSites", "ExampleVoteSite", "Enabled").set(true);
        sitesLoader.save(sites);
        UUID player = UUID.randomUUID();
        UUID vote = UUID.randomUUID();
        AtomicInteger warnings = new AtomicInteger();
        Logger logger = Logger.getLogger(NeoForgeRuntime.class.getName());
        Handler handler = new Handler() {
            @Override public void publish(LogRecord record) {
                if (record.getMessage().contains("CloseInventoryOnVote")
                        && record.getMessage().contains("remain pending")) warnings.incrementAndGet();
            }
            @Override public void flush() { }
            @Override public void close() { }
        };
        logger.addHandler(handler);
        try {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
                NeoForgePlayerDirectoryAccess.joined(runtime.players(), new SharedVoteIdentity(player, "Alex", true));
                assertEquals(NeoForgeVoteResult.Status.DEFERRED, runtime.voteProcessor().process(
                        new NeoForgeVoteRequest(vote, player, "Alex", "PlanetMinecraft.com", 100L,
                                true, true, true, NeoForgeVoteRequest.Scope.COMPLETE)).status());
            }
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory);
                    NeoForgeRewardReplayService replay = NeoForgeRewardReplayServiceAccess.create(
                            runtime.voteConfiguration(), new NeoForgeRewardConfigurationAccess(runtime.config(),
                                    runtime.voteSites(), runtime.specialRewards()), runtime.accounting(),
                            runtime.deferredVotes(), runtime.players(), (occurrence, plan) -> {
                                fail("Unsupported configuration must not execute a reward");
                                return CompletableFuture.completedFuture(null);
                            })) {
                var result = replay.replayOnce().get(5, TimeUnit.SECONDS).get(0);
                assertEquals(NeoForgeRewardReplayService.Status.BLOCKED_UNSUPPORTED, result.status());
                assertTrue(result.detail().contains("CloseInventoryOnVote"));
                assertEquals(vote, runtime.deferredVotes().pending(player).get(0).voteId());
                assertEquals(0, runtime.accounting().load(player).map(NeoForgeVoteAccount::allTimeTotal).orElse(0));
            }
            assertEquals(2, warnings.get());
        } finally {
            logger.removeHandler(handler);
        }
    }

    @Test
    void sharedBukkitResourcesKeepTheirEstablishedDefaults() throws Exception {
        Path bukkitConfig = directory.resolve("bukkit.yml");
        Path bukkitRewards = directory.resolve("bukkit-rewards.yml");
        try (var config = getClass().getClassLoader().getResourceAsStream("Config.yml");
                var rewards = getClass().getClassLoader().getResourceAsStream("SpecialRewards.yml")) {
            Files.copy(config, bukkitConfig);
            Files.copy(rewards, bukkitRewards);
        }
        var config = YamlConfigurationLoader.builder().path(bukkitConfig).build().load();
        var rewards = YamlConfigurationLoader.builder().path(bukkitRewards).build().load();
        assertTrue(config.node("CloseInventoryOnVote").getBoolean());
        assertTrue(config.node("UseVoteStreaks").getBoolean());
        assertEquals("EVERY_VOTE", config.node("VoteBroadcast", "Type").getString());
        assertTrue(rewards.node("VoteMilestones", "FirstVoteAllTime", "Enabled").getBoolean());
    }
}
