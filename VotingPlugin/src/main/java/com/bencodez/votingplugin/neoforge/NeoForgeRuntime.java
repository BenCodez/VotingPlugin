package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.advancedcore.core.user.storage.sql.SqlBackendLogger;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackendFactory;

/** Owns NeoForge bootstrap resources; vote and reward services are not started here. */
public final class NeoForgeRuntime implements AutoCloseable {
    private final ConfigurationNode config;
    private final ConfigurationNode voteSites;
    private final SqlUserBackend storage;
    private final NeoForgeServerScheduler scheduler = new NeoForgeServerScheduler();
    private final NeoForgePlayerDirectory players = new NeoForgePlayerDirectory();
    private boolean closed;

    private NeoForgeRuntime(ConfigurationNode config, ConfigurationNode voteSites, SqlUserBackend storage) {
        this.config = config;
        this.voteSites = voteSites;
        this.storage = storage;
    }

    public static NeoForgeRuntime start(Path directory) throws IOException {
        Objects.requireNonNull(directory, "directory");
        Files.createDirectories(directory);
        Path configFile = installDefault(directory, "Config.yml");
        Path voteSitesFile = installDefault(directory, "VoteSites.yml");
        ConfigurationNode config = YamlConfigurationLoader.builder().path(configFile).build().load();
        ConfigurationNode voteSites = YamlConfigurationLoader.builder().path(voteSitesFile).build().load();
        String storageMode = config.node("DataStorage").getString("SQLITE");
        if (!"SQLITE".equalsIgnoreCase(storageMode)) {
            throw new IOException("NeoForge bootstrap currently supports only SQLITE storage; configured: " + storageMode);
        }
        SqlUserBackend storage;
        try {
            // Use AdvancedCore's existing SQL backend. No vote/user mutations are enabled yet.
            storage = SqlUserBackendFactory.sqlite(directory, "VotingPlugin", "VotingPlugin_NeoForgeUsers",
                    List.of(), SqlBackendLogger.NO_OP);
        } catch (RuntimeException failure) {
            throw new IOException("Could not initialize NeoForge user storage", failure);
        }
        return new NeoForgeRuntime(config, voteSites, storage);
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
    public SqlUserBackend storage() { return storage; }
    public NeoForgeServerScheduler scheduler() { return scheduler; }
    public NeoForgePlayerDirectory players() { return players; }

    @Override public synchronized void close() {
        if (closed) return;
        closed = true;
        scheduler.close();
        players.clear();
        storage.close();
    }
}
