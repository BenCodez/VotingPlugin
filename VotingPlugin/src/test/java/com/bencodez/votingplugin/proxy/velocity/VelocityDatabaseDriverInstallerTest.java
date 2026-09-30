package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
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
                out.putNextEntry(new ZipEntry(name));
                out.write((name.endsWith("json") ? "{\"id\":\"mysqldriver\",\"main\":\"com.bencodez.mysqldriver.velocity.MySQLDriverVelocity\"}" : "class-fixture").getBytes(java.nio.charset.StandardCharsets.UTF_8));
                out.closeEntry();
            }
        }
    }
    @Test
    void presentDriverDoesNoDownloadOrFilesystemWork() {
        var installer = new VelocityDatabaseDriverInstaller(driver -> true, path -> fail("download attempted"));
        for (DbType type : DbType.values())
            assertTrue(installer.ready(List.of(config(type)), true, plugins.resolve("absent/nested/votingplugin"), messages::add, messages::add));
        assertTrue(messages.isEmpty());
    }
    @Test void mariaDbFallsBackToWorkingMysql() {
        var installer = new VelocityDatabaseDriverInstaller(VelocityDatabaseDriverInstaller.MYSQL::equals, path -> fail("download attempted"));
        assertTrue(ready(installer, config(DbType.MARIADB), true));
    }
    @Test void explicitMariaDbMustNotFallback() throws Exception {
        MysqlConfig c = config(DbType.MARIADB);c.setDriver(VelocityDatabaseDriverInstaller.MARIADB);
        AtomicInteger downloads = new AtomicInteger();
        var installer = new VelocityDatabaseDriverInstaller(VelocityDatabaseDriverInstaller.MYSQL::equals, path -> { downloads.incrementAndGet();jar(path, c.getDriver()); });
        assertFalse(ready(installer,c,true));assertEquals(1,downloads.get());
    }
    @Test void successfulDownloadIsPublishedOnlyAfterValidationAndRequiresRestart() throws Exception {
        AtomicInteger downloads = new AtomicInteger();
        var installer = new VelocityDatabaseDriverInstaller(driver -> false, path -> {
            assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
            assertEquals(plugins,path.getParent());downloads.incrementAndGet();jar(path,VelocityDatabaseDriverInstaller.MYSQL);
        });
        assertFalse(ready(installer,config(DbType.MYSQL),true));
        assertEquals(1,downloads.get());assertTrue(Files.isRegularFile(plugins.resolve("MySQLDriver.jar")));
        assertTrue(messages.stream().anyMatch(s -> s.contains("Restart the proxy")));
        try(var files=Files.list(plugins)){assertEquals(1,files.count());}
        // A subsequent startup with an available driver does not revisit/download the artifact.
        assertTrue(ready(new VelocityDatabaseDriverInstaller(driver -> true, path -> fail("download attempted")),config(DbType.MYSQL),true));
    }
    @Test void disabledDoesNotDownload() {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> fail("download attempted")),config(DbType.POSTGRESQL),false));
        assertTrue(messages.stream().anyMatch(s -> s.contains("disabled")));
    }
    @Test void unknownCustomDriverIsNotPretendedToBeProvidedByBundle() {
        MysqlConfig c=config(DbType.MYSQL);c.setDriver("custom.jdbc.Driver");
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> fail("download attempted")),c,true));
    }
    @Test void workingCustomDriverDoesNothing() {
        MysqlConfig c=config(DbType.MYSQL);c.setDriver("custom.jdbc.Driver");
        assertTrue(ready(new VelocityDatabaseDriverInstaller("custom.jdbc.Driver"::equals,path -> fail("download attempted")),c,true));
    }
    @Test void httpFailureCleansTemporaryFile() throws Exception {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> {Files.writeString(path,"partial");throw new IOException("HTTP 503");}),config(DbType.MYSQL),true));
        try(var files=Files.list(plugins)){assertEquals(0,files.count());}
    }
    @Test void invalidJarIsNotInstalled() throws Exception {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> Files.writeString(path,"not a zip")),config(DbType.MYSQL),true));
        try(var files=Files.list(plugins)){assertEquals(0,files.count());}
    }
    @Test void truncatedJarIsNotInstalled() throws Exception {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> {jar(path,VelocityDatabaseDriverInstaller.MYSQL);byte[] bytes=Files.readAllBytes(path);Files.write(path,java.util.Arrays.copyOf(bytes,bytes.length/2));}),config(DbType.MYSQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void missingRequiredClassIsRejected() throws Exception {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> jar(path,VelocityDatabaseDriverInstaller.MYSQL)),config(DbType.POSTGRESQL),true));
        assertFalse(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
    @Test void existingAdministratorJarIsUntouched() throws Exception {
        Path original=plugins.resolve("MySQLDriver.jar");Files.writeString(original,"admin-managed");
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> fail("download attempted")),config(DbType.MYSQL),true));
        assertEquals("admin-managed",Files.readString(original));
    }
    @Test void renamedDriverPluginIsDetectedByMetadata() throws Exception {
        jar(plugins.resolve("renamed-driver.jar"),VelocityDatabaseDriverInstaller.MYSQL);
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> fail("download attempted")),config(DbType.MYSQL),true));
    }
    @Test void concurrentAdministratorInstallIsNotOverwritten() throws Exception {
        assertFalse(ready(new VelocityDatabaseDriverInstaller(driver -> false,path -> {jar(path,VelocityDatabaseDriverInstaller.MYSQL);Files.writeString(plugins.resolve("MySQLDriver.jar"),"admin-won-race");}),config(DbType.MYSQL),true));
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
        var installer=new VelocityDatabaseDriverInstaller(VelocityDatabaseDriverInstaller.MYSQL::equals,path -> jar(path,VelocityDatabaseDriverInstaller.POSTGRESQL));
        assertFalse(installer.ready(List.of(config(DbType.MYSQL),config(DbType.POSTGRESQL)),true,plugins.resolve("votingplugin"),messages::add,messages::add));
        assertTrue(Files.exists(plugins.resolve("MySQLDriver.jar")));
    }
}
