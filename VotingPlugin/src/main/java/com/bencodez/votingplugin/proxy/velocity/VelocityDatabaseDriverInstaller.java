package com.bencodez.votingplugin.proxy.velocity;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Consumer;
import java.util.zip.ZipFile;

import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Startup-only provisioning; never loads JDBC code into a running proxy. */
final class VelocityDatabaseDriverInstaller {
    static final String JOB = "https://bencodez.com/job/MySQLDriver/";
    static final String MYSQL = "com.mysql.cj.jdbc.Driver";
    static final String MARIADB = "org.mariadb.jdbc.Driver";
    static final String POSTGRESQL = "org.postgresql.Driver";
    private static final long MAX_JAR = 32L * 1024 * 1024;

    interface DriverProbe { boolean available(String driver); }
    interface Downloader { void download(Path destination) throws IOException; }

    private final DriverProbe probe;
    private final Downloader downloader;

    VelocityDatabaseDriverInstaller(DriverProbe probe, Downloader downloader) {
        this.probe = probe;
        this.downloader = downloader;
    }

    boolean ready(List<MysqlConfig> configurations, boolean automatic, Path dataDirectory,
            Consumer<String> info, Consumer<String> warning) {
        String missing = null;
        for (MysqlConfig config : configurations) {
            String driver = requiredDriver(config);
            if (probe.available(driver)) continue;
            // ConnectionManager permits this fallback only for an implicit MariaDB driver.
            if ((config.getDriver() == null || config.getDriver().isEmpty())
                    && driver.equals(MARIADB) && probe.available(MYSQL)) continue;
            missing = driver;
            break;
        }
        if (missing == null) return true; // No filesystem or network activity when drivers work.
        warning.accept("Required JDBC driver is unavailable: " + missing);
        if (!automatic) {
            warning.accept("Automatic MySQLDriver installation is disabled. Install manually from " + JOB);
            return false;
        }
        if (!List.of(MYSQL, MARIADB, POSTGRESQL).contains(missing)) {
            warning.accept("MySQLDriver does not supply the configured custom driver; install that driver manually.");
            return false;
        }
        Path stage = null;
        try {
            // Velocity injects its plugin data folder beneath its actual plugin directory.
            Path plugins = dataDirectory.toAbsolutePath().normalize().getParent().toRealPath();
            Path target = plugins.resolve("MySQLDriver.jar");
            if (Files.exists(target, LinkOption.NOFOLLOW_LINKS) || existingDriverPlugin(plugins)) {
                warning.accept("MySQLDriver is already installed but the JDBC driver is unavailable. Check that plugin and restart the proxy; its JAR was not changed.");
                return false;
            }
            stage = Files.createTempFile(plugins, ".mysqldriver-", ".tmp");
            downloader.download(stage);
            validate(stage, missing);
            // Atomic no-clobber publication on the same filesystem. Unlike ATOMIC_MOVE,
            // createLink cannot replace a concurrently installed administrator-owned JAR.
            Files.createLink(target, stage);
            info.accept("Downloaded MySQLDriver to the Velocity plugins directory. Restart the proxy to finish installation.");
        } catch (IOException | RuntimeException failure) {
            String detail = String.valueOf(failure.getMessage()).replaceAll("[\\r\\n\\p{Cntrl}]", " ");
            if (detail.length() > 200) detail = detail.substring(0, 200);
            warning.accept("MySQLDriver installation failed (" + failure.getClass().getSimpleName()
                    + "): " + detail + ". Install manually from " + JOB);
        } finally {
            if (stage != null) {
                try { Files.deleteIfExists(stage); }
                catch (IOException cleanup) { warning.accept("Unable to remove the temporary MySQLDriver download."); }
            }
        }
        return false; // SQL must not run until the next proxy startup loads the plugin.
    }

    static String requiredDriver(MysqlConfig config) {
        if (config.getDriver() != null && !config.getDriver().isEmpty()) return config.getDriver();
        DbType type = config.getDbType();
        if (type == null) type = config.isUseMariaDB() ? DbType.MARIADB : DbType.MYSQL;
        return switch (type) {
            case MARIADB -> MARIADB;
            case POSTGRESQL -> POSTGRESQL;
            default -> MYSQL;
        };
    }

    private static boolean existingDriverPlugin(Path plugins) throws IOException {
        try (var paths = Files.newDirectoryStream(plugins, "*.jar")) {
            int inspected = 0;
            for (Path jar : paths) {
                if (++inspected > 512) throw new IOException("Too many plugin files to inspect safely");
                if (jar.getFileName().toString().toLowerCase(java.util.Locale.ROOT).startsWith("mysqldriver")) return true;
                if (!Files.isRegularFile(jar, LinkOption.NOFOLLOW_LINKS) || Files.size(jar) > MAX_JAR) continue;
                try (ZipFile zip = new ZipFile(jar.toFile())) {
                    var entry = zip.getEntry("velocity-plugin.json");
                    if (entry == null) continue;
                    try (InputStream in = zip.getInputStream(entry)) {
                        JsonObject metadata = JsonParser.parseString(new String(boundedRead(in, 65536),
                                java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject();
                        if (metadata.has("id") && "mysqldriver".equals(metadata.get("id").getAsString())) return true;
                    }
                } catch (IOException | RuntimeException unrelatedJar) {
                    // Other plugins' malformed metadata is not evidence of MySQLDriver.
                }
            }
        }
        return false;
    }

    static void validate(Path jar, String requiredDriver) throws IOException {
        if (Files.size(jar) == 0 || Files.size(jar) > MAX_JAR) throw new IOException("Invalid artifact size");
        try (ZipFile zip = new ZipFile(jar.toFile())) {
            var metadata = zip.getEntry("velocity-plugin.json");
            if (metadata == null) throw new IOException("Missing Velocity metadata");
            JsonObject plugin;
            try (InputStream in = zip.getInputStream(metadata)) {
                plugin = JsonParser.parseString(new String(boundedRead(in, 65536), java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject();
            }
            if (!"mysqldriver".equals(plugin.get("id").getAsString())
                    || !"com.bencodez.mysqldriver.velocity.MySQLDriverVelocity".equals(plugin.get("main").getAsString())
                    || zip.getEntry("com/bencodez/mysqldriver/velocity/MySQLDriverVelocity.class") == null
                    || zip.getEntry(requiredDriver.replace('.', '/') + ".class") == null) {
                throw new IOException("Unexpected MySQLDriver contents");
            }
            long expanded = 0;
            var entries = zip.entries();
            while (entries.hasMoreElements()) {
                var entry = entries.nextElement();
                if (entry.isDirectory()) continue;
                try (InputStream in = zip.getInputStream(entry)) {
                    java.util.zip.CRC32 crc = new java.util.zip.CRC32();
                    byte[] buffer = new byte[8192];
                    int n;
                    while ((n = in.read(buffer)) != -1) {
                        expanded += n;
                        if (expanded > 128L * 1024 * 1024) throw new IOException("Oversized expanded artifact");
                        crc.update(buffer, 0, n);
                    }
                    if (crc.getValue() != entry.getCrc()) throw new IOException("Corrupt ZIP entry");
                }
            }
        } catch (RuntimeException invalid) { throw new IOException("Invalid plugin metadata", invalid); }
    }

    static byte[] boundedRead(InputStream input, int maximum) throws IOException {
        byte[] bytes = input.readNBytes(maximum + 1);
        if (bytes.length > maximum) throw new IOException("Response exceeds size limit");
        return bytes;
    }

    /** Resolve immutable build number and artifact path through Jenkins JSON, never HTML. */
    static void downloadLatest(Path destination) throws IOException {
        JsonObject build;
        try (InputStream in = request(URI.create(JOB + "lastSuccessfulBuild/api/json?tree=number,artifacts%5BfileName,relativePath%5D"))) {
            build = JsonParser.parseString(new String(boundedRead(in, 65536), java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject();
        } catch (RuntimeException invalid) { throw new IOException("Invalid Jenkins build metadata", invalid); }
        String relative = null;
        for (var artifact : build.getAsJsonArray("artifacts")) {
            JsonObject item = artifact.getAsJsonObject();
            if ("MySQLDriver.jar".equals(item.get("fileName").getAsString())) relative = item.get("relativePath").getAsString();
        }
        if (relative == null || !relative.matches("[A-Za-z0-9_./-]+") || relative.contains("..") || relative.startsWith("/"))
            throw new IOException("No safe MySQLDriver artifact in Jenkins build");
        int number = build.get("number").getAsInt();
        if (number <= 0) throw new IOException("Invalid Jenkins build number");
        try (InputStream in = request(URI.create(JOB + number + "/artifact/" + relative));
                var out = Files.newOutputStream(destination)) {
            byte[] buffer = new byte[8192];
            long bytes = 0;
            long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(60);
            int n;
            while ((n = in.read(buffer)) != -1) {
                bytes += n;
                if (bytes > MAX_JAR || System.nanoTime() > deadline) throw new IOException("Artifact download limit exceeded");
                out.write(buffer, 0, n);
            }
        }
    }

    private static InputStream request(URI uri) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) uri.toURL().openConnection();
        connection.setConnectTimeout(10000);
        connection.setReadTimeout(15000);
        connection.setInstanceFollowRedirects(false);
        connection.setRequestProperty("User-Agent", "VotingPlugin");
        try {
            if (connection.getResponseCode() != 200) throw new IOException("Jenkins HTTP status " + connection.getResponseCode());
            return new java.io.FilterInputStream(connection.getInputStream()) {
                private final long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(60);
                private void checkDeadline() throws IOException {
                    if (System.nanoTime() > deadline) throw new IOException("Response timeout");
                }
                @Override public int read() throws IOException { checkDeadline(); return super.read(); }
                @Override public int read(byte[] bytes, int offset, int length) throws IOException {
                    checkDeadline(); return in.read(bytes, offset, length);
                }
                @Override public void close() throws IOException { try { super.close(); } finally { connection.disconnect(); } }
            };
        } catch (IOException failure) { connection.disconnect(); throw failure; }
    }
}
