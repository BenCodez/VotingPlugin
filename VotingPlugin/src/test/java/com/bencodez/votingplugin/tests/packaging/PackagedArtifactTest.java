package com.bencodez.votingplugin.tests.packaging;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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
    void packagedJedisUsesTheResolvedClientRatherThanAnEmbeddedProducerCopy() throws Exception {
        Path selected = Path.of(redis.clients.jedis.Jedis.class.getProtectionDomain()
                .getCodeSource().getLocation().toURI());
        try (JarFile dependency = new JarFile(selected.toFile());
                JarFile artifact = new JarFile(packagedJar().toFile())) {
            // MultiDbClient differs across the 8.0.1/8.0.2 patch boundary; Jedis alone does not.
            for (String name : java.util.List.of("redis/clients/jedis/Jedis.class",
                    "redis/clients/jedis/JedisPool.class", "redis/clients/jedis/MultiDbClient.class")) {
                assertNotNull(dependency.getEntry(name), "Missing resolved client API: " + name);
                assertNotNull(artifact.getEntry(name), "Missing packaged client API: " + name);
                assertEquals(1L, artifact.stream().filter(entry -> entry.getName().equals(name)).count(),
                        "Duplicate packaged client: " + name);
                try (var expected = dependency.getInputStream(dependency.getEntry(name));
                        var actual = artifact.getInputStream(artifact.getEntry(name))) {
                    assertArrayEquals(expected.readAllBytes(), actual.readAllBytes(),
                            "A producer's embedded client must not shadow the resolved dependency: " + name);
                }
            }
        }
    }

    @Test
    void packagedSimpleApiRedisHandlerUsesTheSelectedClientWithoutMavenDependencies(@TempDir Path directory)
            throws Exception {
        assertRedisHandlerLinks(packagedJar(),
                "com.bencodez.votingplugin.simpleapi.servercomm.redis.RedisHandler", directory);
    }

    @Test
    void publishedProducerArtifactsRetainLegacyRedisApisAndLinkTheirHandlers(@TempDir Path directory) throws Exception {
        var resources = PackagedArtifactTest.class.getClassLoader()
                .getResources("com/bencodez/simpleapi/servercomm/redis/RedisHandler.class");
        java.util.Set<URL> producers = new java.util.LinkedHashSet<>();
        while (resources.hasMoreElements()) {
            URL resource = resources.nextElement();
            assertEquals("jar", resource.getProtocol(), "Producer verification requires actual dependency JARs");
            var connection = (java.net.JarURLConnection) resource.openConnection();
            connection.setUseCaches(false);
            producers.add(connection.getJarFileURL());
        }
        assertEquals(2, producers.size(), "Verify both the normal AdvancedCore and direct SimpleAPI artifacts");
        int index = 0;
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        for (URL producer : producers) {
            try (URLClassLoader loader = new URLClassLoader(new URL[] { producer, platformSlf4j },
                    ClassLoader.getPlatformClassLoader())) {
                Class<?> endpoint = Class.forName("redis.clients.jedis.HostAndPort", true, loader);
                Class<?> config = Class.forName("redis.clients.jedis.JedisClientConfig", true, loader);
                Class.forName("redis.clients.jedis.JedisPooled", true, loader).getConstructor(endpoint, config);
                Class.forName("redis.clients.jedis.JedisSentineled", true, loader)
                        .getConstructor(String.class, config, java.util.Set.class, config);
                Class.forName("redis.clients.jedis.UnifiedJedis", true, loader).getConstructor(endpoint, config);
            }
            assertRedisHandlerLinks(Path.of(producer.toURI()), "com.bencodez.simpleapi.servercomm.redis.RedisHandler",
                    Files.createDirectory(directory.resolve("producer-" + index++)));
        }
    }

    private static void assertRedisHandlerLinks(Path artifact, String handlerName, Path directory) throws Exception {
        Path source = directory.resolve("PackagedRedisHandlerProbe.java");
        Files.writeString(source, """
                public final class PackagedRedisHandlerProbe
                        extends %s {
                    public PackagedRedisHandlerProbe(boolean tls) {
                        super("localhost", 0, "", "", 3, tls);
                    }
                    @Override public void debug(String message) { }
                    public Object listener() {
                        return createEnvelopeListener("synthetic", (channel, envelope) -> { });
                    }
                }
                """.formatted(handlerName));
        var compiler = ToolProvider.getSystemJavaCompiler();
        assertNotNull(compiler);
        StringWriter diagnostics = new StringWriter();
        try (var files = compiler.getStandardFileManager(null, null, StandardCharsets.UTF_8)) {
            assertTrue(compiler.getTask(diagnostics, files, null,
                    java.util.List.of("--release", "21", "-proc:none", "-classpath", artifact.toString(),
                            "-d", directory.toString()), null,
                    files.getJavaFileObjects(source.toFile())).call(), diagnostics.toString());
        }
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        try (URLClassLoader loader = new URLClassLoader(new URL[] {
                directory.toUri().toURL(), artifact.toUri().toURL(), platformSlf4j },
                ClassLoader.getPlatformClassLoader())) {
            Class<?> probe = Class.forName("PackagedRedisHandlerProbe", true, loader);
            Class<?> handler = probe.getSuperclass();
            Class<?> configApi = Class.forName("redis.clients.jedis.JedisClientConfig", true, loader);
            Class<?> subscriber = Class.forName("redis.clients.jedis.JedisPubSub", true, loader);
            Class<?> jedis = Class.forName("redis.clients.jedis.Jedis", true, loader);
            jedis.getMethod("publish", String.class, String.class);
            jedis.getMethod("subscribe", subscriber, String[].class);
            var configField = handler.getDeclaredField("clientConfig");
            configField.setAccessible(true);
            var poolField = handler.getDeclaredField("publisherPool");
            poolField.setAccessible(true);
            for (boolean tls : new boolean[] { false, true }) {
                Object instance = probe.getConstructor(boolean.class).newInstance(tls);
                Object pool = poolField.get(instance);
                try {
                    Object config = configField.get(instance);
                    assertEquals(tls, configApi.getMethod("isSsl").invoke(config));
                    assertEquals(3, configApi.getMethod("getDatabase").invoke(config));
                    if (tls) {
                        var parameters = (javax.net.ssl.SSLParameters) configApi.getMethod("getSslParameters").invoke(config);
                        assertEquals("HTTPS", parameters.getEndpointIdentificationAlgorithm());
                    }
                    assertTrue(subscriber.isInstance(probe.getMethod("listener").invoke(instance)));
                    assertEquals(false, pool.getClass().getMethod("isClosed").invoke(pool));
                } finally {
                    // Constructors are lazy: no publish, subscribe or connection borrow is performed.
                    handler.getMethod("close").invoke(instance);
                }
                assertEquals(true, pool.getClass().getMethod("isClosed").invoke(pool));
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
