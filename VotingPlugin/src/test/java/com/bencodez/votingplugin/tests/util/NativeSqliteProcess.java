package com.bencodez.votingplugin.tests.util;

import static org.junit.jupiter.api.Assertions.*;

import com.bencodez.votingplugin.util.SqliteNativeLibrary;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import java.util.concurrent.TimeUnit;

/** Test-only process boundary: Windows releases loaded SQLite DLLs at JVM exit. */
public final class NativeSqliteProcess {
    private NativeSqliteProcess() { }

    public static void run(String mode, Path directory, Path artifact) throws Exception {
        Files.createDirectories(directory);
        Path output = directory.resolve("child-output.log");
        String javaExecutable = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").startsWith("Windows") ? "java.exe" : "java").toString();
        Process child = new ProcessBuilder(javaExecutable, "-cp",
                System.getProperty("surefire.test.class.path", System.getProperty("java.class.path")),
                NativeSqliteProcess.class.getName(), mode, directory.toString(),
                artifact == null ? "" : artifact.toString()).redirectErrorStream(true).redirectOutput(output.toFile()).start();
        try {
            assertTrue(child.waitFor(60, TimeUnit.SECONDS), "native-loading child must exit within its bound");
            assertEquals(0, child.exitValue(), () -> {
                try { return Files.readString(output); }
                catch (java.io.IOException failure) { return "native-loading child failed"; }
            });
        } finally {
            if (child.isAlive()) {
                child.destroyForcibly();
                assertTrue(child.waitFor(10, TimeUnit.SECONDS), "native-loading child must terminate");
            }
        }
        // Exercise deletion only after process exit; a locked DLL must fail this check.
        try (var paths = Files.walk(directory)) {
            for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) Files.delete(path);
        }
        assertFalse(Files.exists(directory), "all child files, including loaded natives, must be deletable");
    }

    public static void main(String[] args) throws Exception {
        String mode = args[0];
        Path directory = Path.of(args[1]);
        if (mode.equals("runtime")) packagedRuntime(directory, Path.of(args[2]));
        else if (mode.equals("packaged-name")) packagedName(directory, Path.of(args[2]));
        else override(directory, mode);
    }

    private static void restoreProperty(String name, String value) {
        if (value == null) System.clearProperty(name);
        else System.setProperty(name, value);
    }

    private static void override(Path directory, String mode) throws Exception {
        Path caseDirectory = Files.createDirectory(directory.resolve(mode));
        Path customDirectory = Files.createDirectory(caseDirectory.resolve("custom-native"));
        String standardName = System.mapLibraryName("sqlitejdbc");
        String nativeName = mode.equals("path-only") || mode.equals("standard-name") ? standardName
                : System.mapLibraryName("sqlitejdbc-test-" + mode);
        String resource = "org/sqlite/native/"
                + org.sqlite.util.OSInfo.getNativeLibFolderPathForCurrentOS() + "/" + standardName;
        try (InputStream bytes = SqliteNativeLibrary.class.getClassLoader().getResourceAsStream(resource)) {
            assertNotNull(bytes);
            Files.copy(bytes, customDirectory.resolve(nativeName));
        }
        Path badTemp = caseDirectory.resolve("not-a-directory");
        Files.writeString(badTemp, "synthetic unusable temp directory");
        java.util.Map<String, String> previous = new java.util.HashMap<>();
        for (String key : java.util.List.of("org.sqlite.lib.path", "org.sqlite.lib.name",
                "java.library.path", "org.sqlite.tmpdir", "java.io.tmpdir")) {
            previous.put(key, System.getProperty(key));
        }
        URL code = SqliteNativeLibrary.class.getProtectionDomain().getCodeSource().getLocation();
        URL driver = org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation();
        URL logging = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        try {
            System.clearProperty("org.sqlite.lib.path");
            System.clearProperty("org.sqlite.lib.name");
            System.setProperty("java.io.tmpdir", badTemp.toString());
            System.setProperty("org.sqlite.tmpdir", badTemp.toString());
            System.setProperty("java.library.path", mode.equals("name-only")
                    ? customDirectory.toString() : badTemp.toString());
            if (mode.equals("path-only") || mode.equals("both")) {
                System.setProperty("org.sqlite.lib.path", customDirectory.toString());
            }
            if (!mode.equals("path-only")) System.setProperty("org.sqlite.lib.name", nativeName);
            Path libraries = caseDirectory.resolve("plugin-libraries");
            try (URLClassLoader isolated = new URLClassLoader(new URL[] { code, driver, logging },
                    ClassLoader.getPlatformClassLoader())) {
                Class<?> preparation = Class.forName(SqliteNativeLibrary.class.getName(), true, isolated);
                preparation.getMethod("ensureAvailable", Path.class).invoke(null, libraries);
                java.sql.Driver jdbc = (java.sql.Driver) Class.forName("org.sqlite.JDBC", true, isolated)
                        .getConstructor().newInstance();
                try (var connection = jdbc.connect("jdbc:sqlite::memory:", new java.util.Properties());
                        var statement = connection.createStatement();
                        var rows = statement.executeQuery("SELECT 42")) {
                    assertTrue(rows.next());
                    org.junit.jupiter.api.Assertions.assertEquals(42, rows.getInt(1));
                }
            }
            if (mode.equals("standard-name")) assertTrue(Files.isDirectory(libraries.resolve("sqlite-native")));
            else assertTrue(Files.notExists(libraries), "explicit selection must not be replaced by embedded native");
            if (!mode.equals("path-only")) {
                org.junit.jupiter.api.Assertions.assertEquals(nativeName, System.getProperty("org.sqlite.lib.name"));
            }
        } finally {
            previous.forEach(NativeSqliteProcess::restoreProperty);
        }
    }

    private static void packagedRuntime(Path directory, Path artifact) throws Exception {

        URL jar = artifact.toUri().toURL();
        URL platformSlf4j = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        Path unusableTemp = directory.resolve("not-a-directory");
        Files.writeString(unusableTemp, "synthetic unusable temp directory");
        String oldJavaTemp = System.getProperty("java.io.tmpdir");
        String oldSqliteTemp = System.getProperty("org.sqlite.tmpdir");
        try {
            System.setProperty("java.io.tmpdir", unusableTemp.toString());
            System.setProperty("org.sqlite.tmpdir", unusableTemp.toString());
            try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, platformSlf4j },
                    ClassLoader.getPlatformClassLoader())) {
                Class<?> runtime = Class.forName("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", true, loader);
                try (AutoCloseable instance = (AutoCloseable) runtime.getMethod("start", Path.class).invoke(null, directory)) {
                    assertTrue(Files.isRegularFile(directory.resolve("VotingPlugin.db")));
                    assertTrue(Files.isDirectory(directory.resolve("libraries/sqlite-native")));
                }
            }
        } finally {
            if (oldJavaTemp == null) System.clearProperty("java.io.tmpdir");
            else System.setProperty("java.io.tmpdir", oldJavaTemp);
            if (oldSqliteTemp == null) System.clearProperty("org.sqlite.tmpdir");
            else System.setProperty("org.sqlite.tmpdir", oldSqliteTemp);
        }

    }

    private static void packagedName(Path directory, Path artifact) throws Exception {

        URL jar = artifact.toUri().toURL();
        URL logging = org.slf4j.Logger.class.getProtectionDomain().getCodeSource().getLocation();
        String nativeName = System.mapLibraryName("sqlitejdbc-custom");
        String resource = "org/sqlite/native/" + org.sqlite.util.OSInfo.getNativeLibFolderPathForCurrentOS()
                + "/" + System.mapLibraryName("sqlitejdbc");
        try (JarFile packaged = new JarFile(artifact.toFile());
                var input = packaged.getInputStream(packaged.getEntry(resource))) {
            Files.copy(input, directory.resolve(nativeName));
        }
        java.util.Map<String, String> previous = new java.util.HashMap<>();
        for (String key : java.util.List.of("org.sqlite.lib.path", "org.sqlite.lib.name", "java.library.path")) {
            previous.put(key, System.getProperty(key));
        }
        try {
            System.clearProperty("org.sqlite.lib.path");
            System.setProperty("org.sqlite.lib.name", nativeName);
            System.setProperty("java.library.path", directory.toString());
            Path libraries = directory.resolve("plugin-libraries");
            try (URLClassLoader loader = new URLClassLoader(new URL[] { jar, logging },
                    ClassLoader.getPlatformClassLoader())) {
                Class.forName("com.bencodez.votingplugin.util.SqliteNativeLibrary", true, loader)
                        .getMethod("ensureAvailable", Path.class).invoke(null, libraries);
                java.sql.Driver jdbc = (java.sql.Driver) Class.forName("org.sqlite.JDBC", true, loader)
                        .getConstructor().newInstance();
                try (var connection = jdbc.connect("jdbc:sqlite::memory:", new java.util.Properties());
                        var statement = connection.createStatement();
                        var rows = statement.executeQuery("SELECT 42")) {
                    assertTrue(rows.next());
                    assertEquals(42, rows.getInt(1));
                }
            }
            assertFalse(Files.exists(libraries), "custom name must select the existing library");
            assertEquals(nativeName, System.getProperty("org.sqlite.lib.name"));
            assertNull(System.getProperty("org.sqlite.lib.path"));
        } finally {
            previous.forEach((key, value) -> {
                if (value == null) System.clearProperty(key);
                else System.setProperty(key, value);
            });
        }

    }
}
