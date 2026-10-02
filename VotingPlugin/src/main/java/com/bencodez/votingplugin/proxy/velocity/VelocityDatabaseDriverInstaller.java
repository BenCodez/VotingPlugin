package com.bencodez.votingplugin.proxy.velocity;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.function.Consumer;
import java.util.zip.ZipFile;

import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Startup-only provisioning; never loads JDBC code into a running proxy. */
final class VelocityDatabaseDriverInstaller {
    static final String RELEASE = "https://github.com/BenCodez/MySQLDriver/releases/latest";
    static final URI LATEST_RELEASE = URI.create("https://api.github.com/repos/BenCodez/MySQLDriver/releases/latest");
    static final String MYSQL = "com.mysql.cj.jdbc.Driver";
    static final String MARIADB = "org.mariadb.jdbc.Driver";
    static final String POSTGRESQL = "org.postgresql.Driver";
    // Approved official GitHub release v1.0. Trust is rooted in the VotingPlugin release,
    // never in a checksum/manifest supplied by the artifact download endpoint.
    // Review a new MySQLDriver release before adding its exact digest here.
    private static final Set<String> APPROVED_SHA256 = Set.of(
            "8c86a9664e6f30d3394b8fb6fd04c1adcc8388d2a01724cb4015fe0a67329e54");
    private static final long MAX_JAR = 32L * 1024 * 1024;

    interface DriverProbe { boolean available(String driver); }
    interface Downloader { void download(Path destination) throws IOException; }

    private final DriverProbe probe;
    private final Downloader downloader;
    private final Set<String> approvedDigests;

    VelocityDatabaseDriverInstaller(DriverProbe probe, Downloader downloader) {
        this(probe, downloader, APPROVED_SHA256);
    }

    VelocityDatabaseDriverInstaller(DriverProbe probe, Downloader downloader, Set<String> approvedDigests) {
        this.probe = probe;
        this.downloader = downloader;
        this.approvedDigests = Set.copyOf(approvedDigests);
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
            warning.accept("Automatic MySQLDriver installation is disabled. Install manually from " + RELEASE);
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
            if (Files.size(stage) == 0 || Files.size(stage) > MAX_JAR)
                throw new IOException("Invalid artifact size");
            if (!approvedDigests.contains(sha256(stage)))
                throw new IOException("Downloaded MySQLDriver is not an approved build; automatic installation refused. Update VotingPlugin for a newly approved driver build");
            validate(stage, missing);
            // Atomic no-clobber publication on the same filesystem. Unlike ATOMIC_MOVE,
            // createLink cannot replace a concurrently installed administrator-owned JAR.
            Files.createLink(target, stage);
            info.accept("Downloaded MySQLDriver to the Velocity plugins directory. Restart the proxy to finish installation.");
        } catch (IOException | RuntimeException failure) {
            String detail = String.valueOf(failure.getMessage()).replaceAll("[\\r\\n\\p{Cntrl}]", " ");
            if (detail.length() > 200) detail = detail.substring(0, 200);
            warning.accept("MySQLDriver installation failed (" + failure.getClass().getSimpleName()
                    + "): " + detail + ". Install manually from " + RELEASE);
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

    static String sha256(Path file) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (InputStream in = Files.newInputStream(file)) {
                byte[] buffer = new byte[8192];
                int n;
                while ((n = in.read(buffer)) != -1) digest.update(buffer, 0, n);
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException unavailable) {
            throw new IllegalStateException("SHA-256 unavailable", unavailable);
        }
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

    /** Discover the latest release; executable admission still requires an embedded digest. */
    static void downloadLatestRelease(Path destination) throws IOException {
        URI artifact;
        try (InputStream in = request(LATEST_RELEASE)) {
            artifact = releaseArtifact(JsonParser.parseString(new String(boundedRead(in, 65536),
                    java.nio.charset.StandardCharsets.UTF_8)).getAsJsonObject());
        } catch (RuntimeException invalid) { throw new IOException("Invalid GitHub release metadata", invalid); }
        try (InputStream in = request(artifact);
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

    static URI releaseArtifact(JsonObject release) throws IOException {
        if (release.get("draft").getAsBoolean() || release.get("prerelease").getAsBoolean())
            throw new IOException("Latest release is not a published stable release");
        URI selected = null;
        for (var element : release.getAsJsonArray("assets")) {
            JsonObject asset = element.getAsJsonObject();
            String name = asset.get("name").getAsString();
            if (!name.matches("MySQLDriver(?:-[A-Za-z0-9_.-]+)?\\.jar")) continue;
            URI uri = URI.create(asset.get("browser_download_url").getAsString());
            if (!releaseEndpointAllowed(uri) || !"github.com".equalsIgnoreCase(uri.getHost())
                    || uri.getQuery() != null || uri.getFragment() != null
                    || !uri.getRawPath().matches("/BenCodez/MySQLDriver/releases/download/[A-Za-z0-9_.-]+/" + java.util.regex.Pattern.quote(name)))
                throw new IOException("Invalid release artifact URL");
            if (selected != null) throw new IOException("Ambiguous release artifacts");
            selected = uri;
        }
        if (selected == null) throw new IOException("No MySQLDriver JAR in latest release");
        return selected;
    }

    interface ConnectionFactory { HttpURLConnection open(URI uri) throws IOException; }

    static boolean releaseEndpointAllowed(URI uri) {
        return "https".equalsIgnoreCase(uri.getScheme()) && uri.getUserInfo() == null
                && (uri.getPort() == -1 || uri.getPort() == 443)
                && (LATEST_RELEASE.equals(uri) || "github.com".equalsIgnoreCase(uri.getHost())
                        || "release-assets.githubusercontent.com".equalsIgnoreCase(uri.getHost()));
    }

    private static InputStream request(URI uri) throws IOException {
        return request(uri, target -> (HttpURLConnection) target.toURL().openConnection());
    }

    static InputStream request(URI uri, ConnectionFactory connections) throws IOException {
        for (int redirects = 0; redirects <= 3; redirects++) {
            if (!releaseEndpointAllowed(uri)) throw new IOException("Unapproved release download endpoint");
            HttpURLConnection connection = connections.open(uri);
            connection.setConnectTimeout(10000);
            connection.setReadTimeout(15000);
            connection.setInstanceFollowRedirects(false);
            connection.setRequestProperty("User-Agent", "VotingPlugin");
            try {
                int status = connection.getResponseCode();
                if (status == 301 || status == 302 || status == 303 || status == 307 || status == 308) {
                    String location = connection.getHeaderField("Location");
                    if (location == null || redirects == 3) throw new IOException("Invalid release redirect");
                    try { uri = uri.resolve(location); }
                    catch (IllegalArgumentException invalid) { throw new IOException("Invalid release redirect", invalid); }
                    connection.disconnect();
                    continue;
                }
                if (status != 200) throw new IOException("Release download HTTP status " + status);
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
        throw new IOException("Release redirect limit exceeded");
    }
}
