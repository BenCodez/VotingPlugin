package com.bencodez.votingplugin.packaging;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.Provider;
import java.util.jar.JarFile;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** Package-phase checks for the actual downloadable plugin artifact. */
public class PackagedArtifactTest {
    private static final long MAX_DOWNLOAD_BYTES = 10L * 1024L * 1024L;

    @Test
    void containsRuntimeWithoutDownloadedHttpCrypto() throws Exception {
        Path artifactPath = packagedJar();
        try (JarFile artifact = new JarFile(artifactPath.toFile())) {
            assertNotNull(artifact.getEntry("com/bencodez/votingplugin/VotingPluginMain.class"));
            assertNotNull(artifact.getEntry("plugin.yml"));
            assertNotNull(artifact.getEntry("META-INF/neoforge.mods.toml"));
            String modMetadata = new String(artifact.getInputStream(
                    artifact.getEntry("META-INF/neoforge.mods.toml")).readAllBytes(), StandardCharsets.UTF_8);
            assertTrue(modMetadata.contains("modId=\"votingplugin\""));
            assertFalse(modMetadata.contains("${"), "NeoForge metadata must have a resolved version");
            assertNotNull(artifact.getEntry("com/bencodez/votingplugin/neoforge/NeoForgeVotingPlugin.class"));
            assertNotNull(artifact.getEntry("org/sqlite/JDBC.class"));
            assertNull(artifact.getEntry("net/neoforged/neoforge/common/NeoForge.class"));
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName().startsWith("org/checkerframework/")));
            assertNull(artifact.getEntry("org/slf4j/Logger.class"));
            assertNull(artifact.getEntry("com/bencodez/votingplugin/slf4j/Logger.class"));
            assertNull(artifact.getEntry("META-INF/services/org.slf4j.spi.SLF4JServiceProvider"));
            String velocityClass = new String(artifact.getInputStream(artifact.getEntry(
                    "com/bencodez/votingplugin/proxy/velocity/VotingPluginVelocity.class")).readAllBytes(),
                    StandardCharsets.ISO_8859_1);
            assertTrue(velocityClass.contains("Lorg/slf4j/Logger;"),
                    "Velocity's injected logger must retain its platform type");
            assertFalse(velocityClass.contains("Lcom/bencodez/votingplugin/slf4j/Logger;"));
            assertNotNull(artifact.getEntry(
                    "com/bencodez/votingplugin/advancedcore/rhino/Context.class"));
            assertNotNull(artifact.getEntry(
                    "com/bencodez/votingplugin/simpleapi/scheduler/BukkitScheduler.class"));
            assertNotNull(artifact.getEntry(
                    "com/bencodez/votingplugin/simpleapi/hikari/HikariDataSource.class"));

            assertNull(artifact.getEntry("org/mozilla/javascript/Context.class"));
            assertNull(artifact.getEntry("com/zaxxer/hikari/HikariDataSource.class"));
            assertNull(artifact.getEntry("com/tcoded/folialib/FoliaLib.class"));
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName().startsWith("org/bouncycastle/")));
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName()
                    .startsWith("com/bencodez/votingplugin/bouncycastle/")));
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName().startsWith("org/sqlite/native/")));
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName()
                    .startsWith("redis/clients/jedis/search/")));

            String pluginDescriptor = new String(artifact.getInputStream(artifact.getEntry("plugin.yml")).readAllBytes(),
                    StandardCharsets.UTF_8);
            String bungeeDescriptor = new String(artifact.getInputStream(artifact.getEntry("bungee.yml")).readAllBytes(),
                    StandardCharsets.UTF_8);
            for (String coordinate : new String[] { "bcprov-jdk18on:1.85", "bcutil-jdk18on:1.85",
                    "bcpkix-jdk18on:1.85" }) {
                assertTrue(pluginDescriptor.contains(coordinate));
                assertTrue(bungeeDescriptor.contains(coordinate));
            }
        }
        long artifactBytes = Files.size(artifactPath);
        assertTrue(artifactBytes <= MAX_DOWNLOAD_BYTES,
                () -> "VotingPlugin downloadable artifact exceeded "
                        + (MAX_DOWNLOAD_BYTES / (1024L * 1024L)) + " MiB: " + artifactBytes);
        System.out.printf("VotingPlugin downloadable artifact: %,d bytes; duplicate Rhino and downloaded HTTP crypto absent%n",
                Files.size(artifactPath));
    }

    @Test
    void packagedNeoForgeRuntimeStartsWithCachedSqliteDriver(@TempDir Path directory) throws Exception {
        URL jar = packagedJar().toUri().toURL();
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        Path libraries = directory.resolve("libraries");
        Files.createDirectories(libraries);
        Files.copy(dependency("sqlite-jdbc-3.53.4.0.jar"),
                libraries.resolve("sqlite-jdbc-3.53.4.0.jar"));
        String previousPath = System.clearProperty("org.sqlite.lib.path");
        String previousName = System.clearProperty("org.sqlite.lib.name");
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, platformSlf4j },
                ClassLoader.getPlatformClassLoader())) {
            Class<?> runtime = Class.forName("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", true, loader);
            try (AutoCloseable instance = (AutoCloseable) runtime.getMethod("start", Path.class).invoke(null, directory)) {
                assertTrue(Files.isRegularFile(directory.resolve("VotingPlugin.db")));
                try (var files = Files.walk(libraries.resolve("sqlite-native"))) {
                    assertTrue(files.anyMatch(path -> Files.isRegularFile(path)
                            && path.getFileName().toString().contains("sqlite")));
                }
            }
        } finally {
            restoreProperty("org.sqlite.lib.path", previousPath);
            restoreProperty("org.sqlite.lib.name", previousName);
        }
    }

    @Test
    void packagedHttpTlsLoadsWithExternalCryptoLibraries(@TempDir Path directory) throws Exception {
        URL jar = packagedJar().toUri().toURL();
        URL bcProvider = dependency("bcprov-jdk18on-1.85.jar").toUri().toURL();
        URL bcPkix = dependency("bcpkix-jdk18on-1.85.jar").toUri().toURL();
        URL bcUtil = dependency("bcutil-jdk18on-1.85.jar").toUri().toURL();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, bcProvider, bcPkix, bcUtil },
                ClassLoader.getPlatformClassLoader())) {
            Class<?> providerType = Class.forName(
                    "org.bouncycastle.jce.provider.BouncyCastleProvider", true, loader);
            Provider provider = (Provider) providerType.getConstructor().newInstance();
            assertNotNull(provider.getService("Signature", "SHA256WITHRSA"));
            Class<?> identityType = Class.forName(
                    "com.bencodez.votingplugin.simpleapi.servercomm.http.HttpTlsIdentity", true, loader);
            Object identity = identityType.getMethod("loadOrCreate", Path.class, String.class)
                    .invoke(null, directory, "localhost");
            assertNotNull(identityType.getMethod("serverContext").invoke(identity));
            Object issued = identityType.getMethod("issueClientCertificate", String.class)
                    .invoke(identity, "packaged-artifact-test");
            assertNotNull(issued.getClass().getMethod("certificate").invoke(issued));
            assertNotNull(issued.getClass().getMethod("pkcs12").invoke(issued));
            Class<?> credentialStoreType = Class.forName(
                    "com.bencodez.votingplugin.simpleapi.servercomm.http.HttpClientCredentialStore", true, loader);
            Path clientDirectory = directory.resolve("client");
            credentialStoreType.getMethod("save", Path.class, issued.getClass())
                    .invoke(null, clientDirectory, issued);
            assertNotNull(credentialStoreType.getMethod("load", Path.class).invoke(null, clientDirectory));
        }
    }

    private static Path packagedJar() {
        String configured = System.getProperty("votingplugin.packagedJar");
        assumeTrue(configured != null, "Packaged artifact is available only in the package lifecycle");
        Path artifact = Path.of(configured).toAbsolutePath().normalize();
        assertTrue(Files.isRegularFile(artifact), "Missing packaged artifact: " + artifact);
        return artifact;
    }

    private static Path dependency(String fileName) {
        for (String entry : System.getProperty("java.class.path").split(java.io.File.pathSeparator)) {
            Path candidate = Path.of(entry);
            if (candidate.getFileName() != null && fileName.equals(candidate.getFileName().toString())) {
                return candidate;
            }
        }
        throw new IllegalStateException("Dependency was not present on the test classpath: " + fileName);
    }

    private static void restoreProperty(String key, String value) {
        if (value == null) System.clearProperty(key);
        else System.setProperty(key, value);
    }
}
