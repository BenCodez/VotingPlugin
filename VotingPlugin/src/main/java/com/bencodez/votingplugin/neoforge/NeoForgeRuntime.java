package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKey;
import com.bencodez.advancedcore.core.user.storage.sql.SqlBackendLogger;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackendFactory;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.util.SqliteNativeLibrary;

/** Owns NeoForge bootstrap resources; vote and reward services are not started here. */
public final class NeoForgeRuntime implements AutoCloseable {
    static final String USER_TABLE_NAME = "VotingPlugin_NeoForgeUsers";
    private final ConfigurationNode config;
    private final ConfigurationNode voteSites;
    private final ConfigurationNode specialRewards;
    private final ConfigurationNode bungeeSettings;
    private final NeoForgeVoteConfiguration voteConfiguration;
    private final SqlUserBackend storage;
    private final NeoForgeVoteAccountingStore accounting;
    private final NeoForgeDeferredVoteStore deferredVotes;
    private final NeoForgeVoteProcessor voteProcessor;
    private final NeoForgeRewardReplayService rewardReplay;
    private final NeoForgeProxySocketService proxySocket;
    private final NeoForgeServerScheduler scheduler = new NeoForgeServerScheduler();
    private final NeoForgePlayerDirectory players = new NeoForgePlayerDirectory();
    private boolean closed;

    private NeoForgeRuntime(Path directory, ConfigurationNode config, ConfigurationNode voteSites,
            ConfigurationNode specialRewards, ConfigurationNode bungeeSettings,
            NeoForgeVoteConfiguration voteConfiguration,
            NeoForgeProxySocketConfiguration proxyConfiguration,
            SqlUserBackend storage, Clock clock, Object server) {
        this.config = config;
        this.voteSites = voteSites;
        this.specialRewards = specialRewards;
        this.bungeeSettings = bungeeSettings;
        this.voteConfiguration = voteConfiguration;
        this.storage = storage;
        accounting = new NeoForgeVoteAccountingStore(storage, voteConfiguration);
        deferredVotes = new NeoForgeDeferredVoteStore(storage);
        voteProcessor = new NeoForgeVoteProcessor(voteConfiguration, accounting, deferredVotes, players, clock);
        rewardReplay = server == null ? null : new NeoForgeRewardReplayService(voteConfiguration,
                new NeoForgeRewardConfiguration(config, voteSites, specialRewards), accounting,
                deferredVotes, players, new NeoForgeNativeRewardActions(server, scheduler, players));
        NeoForgeProxySocketService startedProxy = null;
        try {
            if (server != null && proxyConfiguration.enabled()) {
                startedProxy = NeoForgeProxySocketService.start(directory, proxyConfiguration, voteProcessor, players);
            }
            if (rewardReplay != null) rewardReplay.start();
        } catch (RuntimeException failure) {
            NeoForgeProxySocketService failedProxy = startedProxy;
            RuntimeException cleanupFailure = closeAll(
                    () -> { if (failedProxy != null) failedProxy.close(); },
                    () -> { if (rewardReplay != null) rewardReplay.close(); },
                    scheduler::close);
            if (cleanupFailure != null) failure.addSuppressed(cleanupFailure);
            throw failure;
        }
        proxySocket = startedProxy;
    }

    public static NeoForgeRuntime start(Path directory) throws IOException {
        return start(directory, Clock.systemDefaultZone(), null);
    }

    static NeoForgeRuntime start(Path directory, Clock clock) throws IOException {
        return start(directory, clock, null);
    }

    static NeoForgeRuntime start(Path directory, Object server) throws IOException {
        return start(directory, Clock.systemDefaultZone(), Objects.requireNonNull(server, "server"));
    }

    private static NeoForgeRuntime start(Path directory, Clock clock, Object server) throws IOException {
        Objects.requireNonNull(directory, "directory");
        Objects.requireNonNull(clock, "clock");
        Files.createDirectories(directory);
        Path configFile = installDefault(directory, "Config.yml");
        Path voteSitesFile = installDefault(directory, "VoteSites.yml");
        Path specialRewardsFile = installDefault(directory, "SpecialRewards.yml");
        Path bungeeSettingsFile = installDefault(directory, "BungeeSettings.yml");
        ConfigurationNode config = YamlConfigurationLoader.builder().path(configFile).build().load();
        ConfigurationNode voteSites = YamlConfigurationLoader.builder().path(voteSitesFile).build().load();
        ConfigurationNode specialRewards = YamlConfigurationLoader.builder().path(specialRewardsFile).build().load();
        ConfigurationNode bungeeSettings = YamlConfigurationLoader.builder().path(bungeeSettingsFile).build().load();
        NeoForgeVoteConfiguration voteConfiguration = NeoForgeVoteConfiguration.load(config, voteSites);
        NeoForgeProxySocketConfiguration proxyConfiguration;
        try {
            proxyConfiguration = NeoForgeProxySocketConfiguration.load(bungeeSettings);
        } catch (IllegalArgumentException invalid) {
            throw new IOException("Invalid proxy configuration", invalid);
        }
        String storageMode = config.node("DataStorage").getString("SQLITE");
        if (!"SQLITE".equalsIgnoreCase(storageMode)) {
            throw new IOException("NeoForge supports only SQLITE; configured: " + storageMode);
        }
        SqlUserBackend storage;
        try {
            SqliteNativeLibrary.ensureAvailable(directory.resolve("libraries"));
            // Use AdvancedCore's existing SQL backend and its atomic user transactions.
            storage = SqlUserBackendFactory.sqlite(directory, "VotingPlugin", USER_TABLE_NAME,
                    storageKeys(), SqlBackendLogger.NO_OP);
        } catch (RuntimeException failure) {
            throw new IOException("User storage initialization failed", failure);
        }
        try {
            return new NeoForgeRuntime(directory, config, voteSites, specialRewards, bungeeSettings,
                    voteConfiguration, proxyConfiguration, storage, clock, server);
        } catch (RuntimeException failure) {
            storage.close();
            throw new IOException("Proxy delivery initialization failed", failure);
        }
    }

    private static List<UserDataKey> storageKeys() {
        ArrayList<UserDataKey> keys = new ArrayList<>(NeoForgeVoteAccountingStore.storageKeys());
        keys.addAll(NeoForgeDeferredVoteStore.storageKeys());
        return List.copyOf(keys);
    }

    private static Path installDefault(Path directory, String name) throws IOException {
        Path target = directory.resolve(name);
        if (Files.notExists(target)) {
            try (InputStream resource = NeoForgeRuntime.class.getClassLoader().getResourceAsStream(name)) {
                if (resource == null) throw new IOException("Missing packaged configuration: " + name);
                try {
                    Files.copy(resource, target);
                } catch (FileAlreadyExistsException ignored) {
                    // Another bootstrap installed the same file; load the installed file below.
                }
            }
        }
        return target;
    }

    public ConfigurationNode config() { return config; }
    public ConfigurationNode voteSites() { return voteSites; }
    public ConfigurationNode specialRewards() { return specialRewards; }
    public ConfigurationNode bungeeSettings() { return bungeeSettings; }
    public NeoForgeVoteConfiguration voteConfiguration() { return voteConfiguration; }
    public SqlUserBackend storage() { return storage; }
    public NeoForgeVoteAccountingStore accounting() { return accounting; }
    public NeoForgeDeferredVoteStore deferredVotes() { return deferredVotes; }
    public NeoForgeServerScheduler scheduler() { return scheduler; }
    public NeoForgePlayerDirectory players() { return players; }
    public NeoForgeVoteProcessor voteProcessor() { return voteProcessor; }
    public Optional<NeoForgeRewardReplayService> rewardReplay() { return Optional.ofNullable(rewardReplay); }
    public Optional<NeoForgeProxySocketService> proxySocket() { return Optional.ofNullable(proxySocket); }

    public void playerJoined(Object player) {
        playerJoinedIdentity(player);
    }

    SharedVoteIdentity playerJoinedIdentity(Object player) {
        SharedVoteIdentity identity = players.joinedIdentity(player);
        if (rewardReplay != null) rewardReplay.rememberIdentity(identity);
        return identity;
    }

    @Override public synchronized void close() {
        if (closed) return;
        closed = true;
        RuntimeException failure = closeAll(
                () -> { if (proxySocket != null) proxySocket.close(); },
                voteProcessor::stop,
                () -> { if (rewardReplay != null) rewardReplay.stopAdmission(); },
                scheduler::close,
                () -> { if (rewardReplay != null) rewardReplay.close(); },
                players::clear,
                storage::close);
        if (failure != null) throw failure;
    }

    static RuntimeException closeAll(Runnable... steps) {
        RuntimeException failure = null;
        for (Runnable step : steps) {
            try {
                step.run();
            } catch (RuntimeException closeFailure) {
                if (failure == null) failure = closeFailure;
                else failure.addSuppressed(closeFailure);
            }
        }
        return failure;
    }
}
