package com.bencodez.votingplugin.tests.packaging;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.bukkit.Server;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerGameModeChangeEvent;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitTask;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Answers;

/** Package-phase checks for the actual downloadable plugin artifact. */
public class PackagedArtifactTest {
    private static final long ARTIFACT_SIZE_TARGET_BYTES = 10L * 1024L * 1024L;

    @Test
    void containsOneRelocatedRuntimeWithoutUnusedHttpCrypto() throws Exception {
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
            assertNotNull(artifact.getEntry("META-INF/maven/org.xerial/sqlite-jdbc/VERSION"),
                    "sqlite-jdbc must retain its runtime version fallback");
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
            assertFalse(artifact.stream().anyMatch(entry -> entry.getName().contains("/bouncycastle/")));
            assertNotNull(artifact.getEntry("org/sqlite/native/Linux/x86_64/libsqlitejdbc.so"),
                    "The common Linux x86_64 SQLite runtime must remain available offline");
            assertFalse(artifact.stream().anyMatch(entry -> !entry.isDirectory()
                    && entry.getName().startsWith("org/sqlite/native/")
                    && !entry.getName().startsWith("org/sqlite/native/Linux/x86_64/")),
                    "Only the common offline SQLite native may be embedded");
        }
        long artifactBytes = Files.size(artifactPath);
        warnIfArtifactExceedsSizeTarget(artifactBytes, System.out);
        System.out.printf("VotingPlugin downloadable artifact: %,d bytes; duplicate Rhino, external crypto and uncommon SQLite natives absent%n",
                Files.size(artifactPath));
    }

    @Test
    void artifactSizeTargetWarnsWithoutRejectingOversizedArtifacts() {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (PrintStream report = new PrintStream(output, true, StandardCharsets.UTF_8)) {
            warnIfArtifactExceedsSizeTarget(ARTIFACT_SIZE_TARGET_BYTES, report);
            assertTrue(output.toString(StandardCharsets.UTF_8).isEmpty());
            warnIfArtifactExceedsSizeTarget(ARTIFACT_SIZE_TARGET_BYTES + 1, report);
            String warning = output.toString(StandardCharsets.UTF_8);
            assertTrue(warning.contains("[WARNING]"));
            assertTrue(warning.contains("10 MiB size target"));
            assertTrue(warning.contains("does not fail the build"));
        }
    }

    private static void warnIfArtifactExceedsSizeTarget(long artifactBytes, PrintStream output) {
        if (artifactBytes > ARTIFACT_SIZE_TARGET_BYTES) {
            output.printf("[WARNING] VotingPlugin downloadable artifact exceeds the 10 MiB size target: %,d bytes. "
                    + "Review dependency growth; this warning does not fail the build.%n", artifactBytes);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    void packagedGameModeListenerUsesTheCorrectedEntityDelayFacade() throws Exception {
        URL jar = packagedJar().toUri().toURL();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar }, getClass().getClassLoader())) {
            String prefix = "com.bencodez.votingplugin.simpleapi.";
            Class<?> libraryType = Class.forName(prefix + "folialib.FoliaLib", true, loader);
            Class<?> backendType = Class.forName(prefix + "folialib.impl.SpigotImplementation", true, loader);
            Class<?> facadeType = Class.forName(prefix + "scheduler.BukkitScheduler", true, loader);
            assertSame(loader, facadeType.getClassLoader(), "Use the shaded downloadable class, not Maven's SDK copy");
            JavaPlugin plugin = mock(JavaPlugin.class);
            Server server = mock(Server.class);
            org.bukkit.scheduler.BukkitScheduler scheduler = mock(org.bukkit.scheduler.BukkitScheduler.class);
            when(plugin.getServer()).thenReturn(server);
            when(server.getScheduler()).thenReturn(scheduler);
            AtomicReference<Object> backend = new AtomicReference<>();
            Object library = mock(libraryType, invocation -> switch (invocation.getMethod().getName()) {
                case "getPlugin" -> plugin;
                case "getImpl" -> backend.get();
                default -> Answers.RETURNS_DEFAULTS.answer(invocation);
            });
            backend.set(backendType.getConstructor(libraryType).newInstance(library));
            Object facade = mock(facadeType, invocation -> invocation.getMethod().getName().equals("getFoliaLib")
                    ? library : invocation.callRealMethod());
            Queue<Consumer<BukkitTask>> queued = new ArrayDeque<>();
            doAnswer(invocation -> {
                queued.add((Consumer<BukkitTask>) invocation.getArgument(1));
                return null;
            }).when(scheduler).runTaskLater(eq(plugin), any(Consumer.class), anyLong());
            Player player = mock(Player.class);
            when(player.isValid()).thenReturn(true);
            Runnable action = mock(Runnable.class);
            facadeType.getMethod("runTaskLater", Plugin.class, Runnable.class, long.class, Entity.class)
                    .invoke(facade, plugin, action, 2L, player);
            verify(scheduler).runTaskLater(eq(plugin), any(Consumer.class), eq(40L));
            verifyNoInteractions(action);
            assertEquals(1, queued.size());
            queued.remove().accept(mock(BukkitTask.class));
            verify(action, times(1)).run();

            // Exercise the real packaged event listener as well as the facade itself.
            clearInvocations(scheduler);
            Class<?> advancedType = Class.forName(
                    "com.bencodez.votingplugin.advancedcore.AdvancedCorePlugin", true, loader);
            Object advancedPlugin = mock(advancedType, invocation -> switch (invocation.getMethod().getName()) {
                case "getBukkitScheduler" -> facade;
                case "isEnabled" -> true;
                default -> Answers.RETURNS_DEEP_STUBS.answer(invocation);
            });
            Class<?> listenerType = Class.forName(
                    "com.bencodez.votingplugin.advancedcore.listeners.RewardEligibilityListener", true, loader);
            Object listener = listenerType.getConstructor(advancedType).newInstance(advancedPlugin);
            PlayerGameModeChangeEvent event = mock(PlayerGameModeChangeEvent.class);
            when(event.getPlayer()).thenReturn(player);
            listenerType.getMethod("onGameModeChange", PlayerGameModeChangeEvent.class).invoke(listener, event);
            verify(scheduler, times(1)).runTaskLater(eq(plugin), any(Consumer.class), eq(20L));
            assertEquals(1, queued.size());
        }
    }

    @Test
    void packagedNeoForgeRuntimeStartsAndClosesWithoutTestDependencies(@TempDir Path directory) throws Exception {
        URL jar = packagedJar().toUri().toURL();
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, platformSlf4j },
                ClassLoader.getPlatformClassLoader())) {
            Class<?> runtime = Class.forName("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", true, loader);
            try (AutoCloseable instance = (AutoCloseable) runtime.getMethod("start", Path.class).invoke(null, directory)) {
                assertTrue(Files.isRegularFile(directory.resolve("VotingPlugin.db")));
            }
        }
    }

    @Test
    void packagedRedisClientLinksWithModuleApis() throws Exception {
        URL jar = packagedJar().toUri().toURL();
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, platformSlf4j },
                ClassLoader.getPlatformClassLoader())) {
            Class<?> redisClient = Class.forName("redis.clients.jedis.RedisClient", true, loader);
            try (AutoCloseable client = (AutoCloseable) redisClient.getMethod("create").invoke(null)) {
                assertTrue(Class.forName("redis.clients.jedis.UnifiedJedis", true, loader).isInstance(client));
            }
        }
    }

    @Test
    void packagedJdkTlsIdentityWorksWithoutExternalCrypto(@TempDir Path directory) throws Exception {
        URL jar = packagedJar().toUri().toURL();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar }, ClassLoader.getPlatformClassLoader())) {
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
}
