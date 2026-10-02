package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;

class VelocityDatabaseDriverInstallerTest {
    @TempDir Path plugins;
    private final List<String> messages = new ArrayList<>();

    private VelocityDatabaseDriverInstaller installer(VelocityDatabaseDriverInstaller.DriverProbe probe,
            VelocityDatabaseDriverInstaller.Downloader downloader) {
        try {
            Path fixture = Files.createTempFile("mysql-driver-test-", ".jar");
            try {
                java.util.HashSet<String> approved = new java.util.HashSet<>();
                for (String driver : List.of(VelocityDatabaseDriverInstaller.MYSQL,
                        VelocityDatabaseDriverInstaller.MARIADB, VelocityDatabaseDriverInstaller.POSTGRESQL)) {
                    jar(fixture, driver);
                    approved.add(VelocityDatabaseDriverInstaller.sha256(fixture));
                }
                return new VelocityDatabaseDriverInstaller(probe, downloader, approved);
            } finally { Files.deleteIfExists(fixture); }
        } catch (IOException failure) { throw new java.io.UncheckedIOException(failure); }
    }
    private MysqlConfig config(DbType type) {
        MysqlConfig config = new MysqlConfig();
        config.setDbType(type);
        return config;
    }
    private boolean ready(VelocityDatabaseDriverInstaller installer, MysqlConfig config, boolean enabled) {
        return installer.ready(List.of(config), enabled, plugins.resolve("votingplugin"), messages::add, messages::add);
    }
    private void jar(Path path, String driver) throws IOException {
        try (ZipOutputStream out = new ZipOutputStream(Files.newOutputStream(path))) {
            for (String name : List.of("velocity-plugin.json", "com/bencodez/mysqldriver/velocity/MySQLDriverVelocity.class", driver.replace('.', '/')+".class")) {
                ZipEntry entry = new ZipEntry(name); entry.setTime(0); out.putNextEntry(entry);
                out.write((name.endsWith("json") ? "{\"id\":\"mysqldriver\",\"main\":\"com.bencodez.mysqldriver.velocity.MySQLDriverVelocity\"}" : "class-fixture").getBytes(java.nio.charset.StandardCharsets.UTF_8));
                out.closeEntry();
            }
        }
    }
    @Test
    void presentDriverDoesNoDownloadOrFilesystemWork() {
        var installer = installer(driver -> true, path -> fail("download attempted"));
        for (DbType type : DbType.values())
            assertTrue(installer.ready(List.of(config(type)), true, plugins.resolve("absent/nested/votingplugin"), messages::add, messages::add));
        assertTrue(messages.isEmpty());
    }
    @Test void mariaDbFallsBackToWorkingMysql() {
        var installer = installer(VelocityDatabaseDriverInstaller.MYSQL::equals, path -> fail("download attempted"));
        assertTrue(ready(installer, config(DbType.MARIADB), true));
    }
    @Test void explicitMariaDbMustNotFallback() throws Exception {
        MysqlConfig c = config(DbType.MARIADB);c.setDriver(VelocityDatabaseDriverInstaller.MARIADB);
        AtomicInteger downloads = new AtomicInteger();
        var installer = installer(VelocityDatabaseDriverInstaller.MYSQL::equals, path -> { downloads.incrementAndGet();jar(path, c.getDriver()); });
        assertFalse(ready(installer,c,true));assertEquals(1,downloads.get());
    }
    @Test void successfulDownloadIsPublishedOnlyAfterValidationAndRequiresRestart() throws Exception {
        AtomicInteger downloads = new AtomicInteger();
        var installer = installer(driver -> false, path -> {
            assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
            assertEquals(plugins,path.getParent());downloads.incrementAndGet();jar(path,VelocityDatabaseDriverInstaller.MYSQL);
        });
        assertFalse(ready(installer,config(DbType.MYSQL),true));
        assertEquals(1,downloads.get());assertTrue(Files.isRegularFile(plugins.resolve("MySQLDriver.jar")));
        assertTrue(messages.stream().anyMatch(s -> s.contains("Restart the proxy")));
        try(var files=Files.list(plugins)){assertEquals(1,files.count());}
        // A subsequent startup with an available driver does not revisit/download the artifact.
        assertTrue(ready(installer(driver -> true, path -> fail("download attempted")),config(DbType.MYSQL),true));
    }
    @Test void disabledDoesNotDownload() {
        assertFalse(ready(installer(driver -> false,path -> fail("download attempted")),config(DbType.POSTGRESQL),false));
        assertTrue(messages.stream().anyMatch(s -> s.contains("disabled")));
    }
    @Test void unknownCustomDriverIsNotPretendedToBeProvidedByBundle() {
        MysqlConfig c=config(DbType.MYSQL);c.setDriver("custom.jdbc.Driver");
        assertFalse(ready(installer(driver -> false,path -> fail("download attempted")),c,true));
    }
    @Test void workingCustomDriverDoesNothing() {
        MysqlConfig c=config(DbType.MYSQL);c.setDriver("custom.jdbc.Driver");
        assertTrue(ready(installer("custom.jdbc.Driver"::equals,path -> fail("download attempted")),c,true));
    }
    @Test void httpFailureCleansTemporaryFile() throws Exception {
        assertFalse(ready(installer(driver -> false,path -> {Files.writeString(path,"partial");throw new IOException("HTTP 503");}),config(DbType.MYSQL),true));
        try(var files=Files.list(plugins)){assertEquals(0,files.count());}
    }
    @Test void invalidJarIsNotInstalled() throws Exception {
        assertFalse(ready(installer(driver -> false,path -> Files.writeString(path,"not a zip")),config(DbType.MYSQL),true));
        try(var files=Files.list(plugins)){assertEquals(0,files.count());}
    }
    @Test void truncatedJarIsNotInstalled() throws Exception {
        assertFalse(ready(installer(driver -> false,path -> {jar(path,VelocityDatabaseDriverInstaller.MYSQL);byte[] bytes=Files.readAllBytes(path);Files.write(path,java.util.Arrays.copyOf(bytes,bytes.length/2));}),config(DbType.MYSQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void missingRequiredClassIsRejected() throws Exception {
        assertFalse(ready(installer(driver -> false,path -> jar(path,VelocityDatabaseDriverInstaller.MYSQL)),config(DbType.POSTGRESQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void existingAdministratorJarIsUntouched() throws Exception {
        Path original=plugins.resolve("MySQLDriver.jar");Files.writeString(original,"admin-managed");
        assertFalse(ready(installer(driver -> false,path -> fail("download attempted")),config(DbType.MYSQL),true));
        assertEquals("admin-managed",Files.readString(original));
    }
    @Test void renamedDriverPluginIsDetectedByMetadata() throws Exception {
        jar(plugins.resolve("renamed-driver.jar"),VelocityDatabaseDriverInstaller.MYSQL);
        assertFalse(ready(installer(driver -> false,path -> fail("download attempted")),config(DbType.MYSQL),true));
    }
    @Test void concurrentAdministratorInstallIsNotOverwritten() throws Exception {
        assertFalse(ready(installer(driver -> false,path -> {jar(path,VelocityDatabaseDriverInstaller.MYSQL);Files.writeString(plugins.resolve("MySQLDriver.jar"),"admin-won-race");}),config(DbType.MYSQL),true));
        assertEquals("admin-won-race",Files.readString(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void hiddenOptOutDefaultsTrueAndAcceptsExplicitValues() throws Exception {
        Path file=plugins.resolve("config.yml");
        for(String content:List.of("Debug: false\n","AutoDownloadMissingDatabaseDriver: true\n","AutoDownloadMissingDatabaseDriver: false\n")) {
            Files.writeString(file,content);VelocityConfig config=new VelocityConfig(file.toFile());config.loadControlConfiguration();
            assertEquals(!content.contains("Driver: false"),config.getAutoDownloadMissingDatabaseDriver());
        }
        try(var in=getClass().getClassLoader().getResourceAsStream("bungeeconfig.yml")) {
            assertFalse(new String(in.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).contains("AutoDownloadMissingDatabaseDriver"));
        }
    }
    @Test void missingSecondaryConnectionDriverStopsStartup() throws Exception {
        var installer=installer(VelocityDatabaseDriverInstaller.MYSQL::equals,path -> jar(path,VelocityDatabaseDriverInstaller.POSTGRESQL));
        assertFalse(installer.ready(List.of(config(DbType.MYSQL),config(DbType.POSTGRESQL)),true,plugins.resolve("votingplugin"),messages::add,messages::add));
        assertTrue(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void productionTrustRejectsAnOtherwiseValidUnapprovedArtifact() throws Exception {
        var installer = new VelocityDatabaseDriverInstaller(driver -> false,
                path -> jar(path,VelocityDatabaseDriverInstaller.MYSQL));
        assertFalse(ready(installer,config(DbType.MYSQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
        assertTrue(messages.stream().anyMatch(s -> s.contains("not an approved build")));
        try(var files=Files.list(plugins)){assertEquals(0,files.count());}
    }
    @Test void modifiedClassBytesWithValidZipAndMetadataAreRejected() throws Exception {
        Path approved = plugins.resolve("approved.fixture");
        jar(approved,VelocityDatabaseDriverInstaller.MYSQL);
        String digest = VelocityDatabaseDriverInstaller.sha256(approved);
        Files.delete(approved);
        var installer = new VelocityDatabaseDriverInstaller(driver -> false, path -> {
            // Build a fully valid replacement ZIP; its entry CRCs and plugin metadata match.
            try (ZipOutputStream out = new ZipOutputStream(Files.newOutputStream(path))) {
                for (String name : List.of("velocity-plugin.json", "com/bencodez/mysqldriver/velocity/MySQLDriverVelocity.class", "com/mysql/cj/jdbc/Driver.class")) {
                    ZipEntry entry = new ZipEntry(name); entry.setTime(0);out.putNextEntry(entry);
                    out.write((name.endsWith("json") ? "{\"id\":\"mysqldriver\",\"main\":\"com.bencodez.mysqldriver.velocity.MySQLDriverVelocity\"}" : "changed-attacker-class").getBytes(java.nio.charset.StandardCharsets.UTF_8));
                    out.closeEntry();
                }
            }
            VelocityDatabaseDriverInstaller.validate(path,VelocityDatabaseDriverInstaller.MYSQL);
        },Set.of(digest));
        assertFalse(ready(installer,config(DbType.MYSQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
        assertTrue(messages.stream().anyMatch(s -> s.contains("not an approved build")));
    }

    private com.google.gson.JsonObject release(String tag, String url) {
        var release = new com.google.gson.JsonObject();
        release.addProperty("draft", false);
        release.addProperty("prerelease", false);
        var asset = new com.google.gson.JsonObject();
        asset.addProperty("name", "MySQLDriver-" + tag + ".jar");
        asset.addProperty("browser_download_url", url);
        var assets = new com.google.gson.JsonArray(); assets.add(asset);
        release.add("assets", assets);
        return release;
    }

    @Test void latestReleaseSelectionIsNotPinnedToVersionOne() throws Exception {
        for (String version : List.of("1.0", "2.0")) {
            String url = "https://github.com/BenCodez/MySQLDriver/releases/download/v" + version
                    + "/MySQLDriver-" + version + ".jar";
            assertEquals(java.net.URI.create(url), VelocityDatabaseDriverInstaller.releaseArtifact(release(version, url)));
        }
    }

    @Test void releaseMetadataCannotRedirectToUnrelatedArtifacts() {
        for (String url : List.of("http://github.com/BenCodez/MySQLDriver/releases/download/v1.0/MySQLDriver-1.0.jar",
                "https://example.com/MySQLDriver-1.0.jar",
                "https://github.com/attacker/MySQLDriver/releases/download/v1.0/MySQLDriver-1.0.jar")) {
            assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.releaseArtifact(release("1.0", url)));
        }
    }

    @Test void missingAmbiguousAndUnpublishedReleaseAssetsFailClosed() {
        String url = "https://github.com/BenCodez/MySQLDriver/releases/download/v1.0/MySQLDriver-1.0.jar";
        var metadata = release("1.0", url);
        metadata.getAsJsonArray("assets").add(metadata.getAsJsonArray("assets").get(0).deepCopy());
        assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.releaseArtifact(metadata));
        metadata.getAsJsonArray("assets").remove(1);
        metadata.addProperty("prerelease", true);
        assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.releaseArtifact(metadata));
        metadata.addProperty("prerelease", false); metadata.addProperty("draft", true);
        assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.releaseArtifact(metadata));
        metadata.addProperty("draft", false); metadata.getAsJsonArray("assets").remove(0);
        assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.releaseArtifact(metadata));
    }

    private static class Response extends java.net.HttpURLConnection {
        final int status; final String location; boolean disconnected;
        Response(java.net.URI uri, int status, String location) throws Exception {
            super(uri.toURL()); this.status = status; this.location = location;
        }
        @Override public int getResponseCode() { return status; }
        @Override public String getHeaderField(String name) { return "Location".equals(name) ? location : null; }
        @Override public java.io.InputStream getInputStream() { return new java.io.ByteArrayInputStream(new byte[]{42}); }
        @Override public void disconnect() { disconnected = true; }
        @Override public boolean usingProxy() { return false; }
        @Override public void connect() { }
    }

    @Test void githubHttpsAssetRedirectIsFollowedAndConnectionsClosed() throws Exception {
        var start = java.net.URI.create("https://github.com/BenCodez/MySQLDriver/releases/download/v1.0/MySQLDriver-1.0.jar");
        var asset = java.net.URI.create("https://release-assets.githubusercontent.com/github-production-release-asset/test?signature=test");
        Response first = new Response(start, 302, asset.toString());
        Response second = new Response(asset, 200, null);
        try (var in = VelocityDatabaseDriverInstaller.request(start, uri -> uri.equals(start) ? first : second)) {
            assertEquals(42, in.read());
        }
        assertTrue(first.disconnected); assertTrue(second.disconnected);
    }

    @Test void redirectDowngradeUnapprovedHostsAndCredentialsFailBeforeConnection() throws Exception {
        var start = java.net.URI.create("https://github.com/BenCodez/MySQLDriver/releases/download/v1.0/MySQLDriver-1.0.jar");
        for (String target : List.of("http://release-assets.githubusercontent.com/artifact", "https://evil.example/artifact",
                "https://github.com.evil.example/artifact", "https://user:password@github.com/artifact",
                "https://github.com:8443/artifact")) {
            Response first = new Response(start, 302, target);
            AtomicInteger attempts = new AtomicInteger();
            assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.request(start, uri -> {
                assertEquals(start, uri); attempts.incrementAndGet(); return first;
            }));
            assertEquals(1, attempts.get()); assertTrue(first.disconnected);
        }
    }

    @Test void redirectLoopsMissingLocationAndHttpFailureAreBounded() throws Exception {
        var uri = VelocityDatabaseDriverInstaller.LATEST_RELEASE;
        Response loop = new Response(uri, 302, uri.toString());
        AtomicInteger attempts = new AtomicInteger();
        assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.request(uri, ignored -> {
            attempts.incrementAndGet(); return loop;
        }));
        assertEquals(4, attempts.get()); assertTrue(loop.disconnected);
        for (int status : List.of(302, 404, 503)) {
            Response response = new Response(uri, status, null);
            assertThrows(IOException.class, () -> VelocityDatabaseDriverInstaller.request(uri, ignored -> response));
            assertTrue(response.disconnected);
        }
    }
}
