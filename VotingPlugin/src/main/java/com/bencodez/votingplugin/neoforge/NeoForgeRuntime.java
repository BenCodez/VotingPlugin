package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.Objects;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.advancedcore.core.user.storage.sql.SqlBackendLogger;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackendFactory;
import com.bencodez.votingplugin.util.SqliteNativeLibrary;

/** Owns NeoForge bootstrap resources; vote and reward services are not started here. */
public final class NeoForgeRuntime implements AutoCloseable {
    private final ConfigurationNode config;
    private final ConfigurationNode voteSites;
    private final NeoForgeVoteConfiguration voteConfiguration;
    private final SqlUserBackend storage;
    private final NeoForgeVoteAccountingStore accounting;
    private final NeoForgeVoteProcessor voteProcessor;
    private final NeoForgeServerScheduler scheduler = new NeoForgeServerScheduler();
    private final NeoForgePlayerDirectory players = new NeoForgePlayerDirectory();
    private boolean closed;

    private NeoForgeRuntime(ConfigurationNode config, ConfigurationNode voteSites,
            NeoForgeVoteConfiguration voteConfiguration, SqlUserBackend storage, Clock clock) {
        this.config = config;
        this.voteSites = voteSites;
        this.voteConfiguration = voteConfiguration;
        this.storage = storage;
        accounting = new NeoForgeVoteAccountingStore(storage, voteConfiguration);
        voteProcessor = new NeoForgeVoteProcessor(voteConfiguration, accounting, players, clock);
    }

    public static NeoForgeRuntime start(Path directory) throws IOException {
        return start(directory, Clock.systemDefaultZone());
    }

    static NeoForgeRuntime start(Path directory, Clock clock) throws IOException {
        Objects.requireNonNull(directory, "directory");
        Objects.requireNonNull(clock, "clock");
        Files.createDirectories(directory);
        Path configFile = installDefault(directory, "Config.yml");
        Path voteSitesFile = installDefault(directory, "VoteSites.yml");
        ConfigurationNode config = YamlConfigurationLoader.builder().path(configFile).build().load();
        ConfigurationNode voteSites = YamlConfigurationLoader.builder().path(voteSitesFile).build().load();
        NeoForgeVoteConfiguration voteConfiguration = NeoForgeVoteConfiguration.load(config, voteSites);
        String storageMode = config.node("DataStorage").getString("SQLITE");
        if (!"SQLITE".equalsIgnoreCase(storageMode)) {
            throw new IOException("NeoForge bootstrap currently supports only SQLITE storage; configured: " + storageMode);
        }
        SqlUserBackend storage;
        try {
            SqliteNativeLibrary.ensureAvailable(directory.resolve("libraries"));
            // Use AdvancedCore's existing SQL backend and its atomic user transactions.
            storage = SqlUserBackendFactory.sqlite(directory, "VotingPlugin", "VotingPlugin_NeoForgeUsers",
                    NeoForgeVoteAccountingStore.storageKeys(), SqlBackendLogger.NO_OP);
        } catch (RuntimeException failure) {
            throw new IOException("Could not initialize NeoForge user storage", failure);
        }
        return new NeoForgeRuntime(config, voteSites, voteConfiguration, storage, clock);
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
    public NeoForgeVoteConfiguration voteConfiguration() { return voteConfiguration; }
    public SqlUserBackend storage() { return storage; }
    public NeoForgeVoteAccountingStore accounting() { return accounting; }
    public NeoForgeServerScheduler scheduler() { return scheduler; }
    public NeoForgePlayerDirectory players() { return players; }
    public NeoForgeVoteProcessor voteProcessor() { return voteProcessor; }

    @Override public synchronized void close() {
        if (closed) return;
        closed = true;
        voteProcessor.stop();
        scheduler.close();
        players.clear();
        storage.close();
    }
}
