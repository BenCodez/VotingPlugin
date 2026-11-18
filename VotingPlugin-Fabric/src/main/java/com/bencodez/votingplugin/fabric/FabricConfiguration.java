package com.bencodez.votingplugin.fabric;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.Properties;

record FabricConfiguration(boolean logLifecycle) {
    private static final String DEFAULTS = "/votingplugin-fabric.properties";
    private static final int MAX_BYTES = 4096;

    static FabricConfiguration load(Path configRoot) throws IOException {
        Path directory = configRoot.resolve("votingplugin");
        Files.createDirectories(directory);
        if (!Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("VotingPlugin Fabric configuration directory must be a directory");
        }
        Path file = directory.resolve("fabric.properties");
        if (Files.notExists(file, LinkOption.NOFOLLOW_LINKS)) {
            try (InputStream defaults = FabricConfiguration.class.getResourceAsStream(DEFAULTS)) {
                if (defaults == null) {
                    throw new IOException("VotingPlugin Fabric configuration defaults are missing");
                }
                try {
                    Files.copy(defaults, file);
                } catch (FileAlreadyExistsException ignored) {
                    // Preserve a configuration created by another caller.
                }
            }
        }
        if (!Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("VotingPlugin Fabric configuration must be a regular file");
        }
        Properties properties = new Properties();
        try (InputStream input = Files.newInputStream(file, LinkOption.NOFOLLOW_LINKS)) {
            byte[] bytes = input.readNBytes(MAX_BYTES + 1);
            if (bytes.length > MAX_BYTES) {
                throw new IOException("VotingPlugin Fabric configuration exceeds 4096 bytes");
            }
            properties.load(new ByteArrayInputStream(bytes));
        } catch (IllegalArgumentException invalid) {
            throw new IOException("Malformed VotingPlugin Fabric configuration", invalid);
        }
        return switch (properties.getProperty("logLifecycle", "true").strip()) {
            case "true" -> new FabricConfiguration(true);
            case "false" -> new FabricConfiguration(false);
            default -> throw new IOException("VotingPlugin Fabric logLifecycle must be true or false");
        };
    }
}
