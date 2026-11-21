package com.bencodez.votingplugin.tests.util;

import com.bencodez.votingplugin.util.SqliteNativeLibrary;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import java.util.stream.Stream;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SqliteNativeLibraryTest {
    @TempDir Path directory;

    @Test
    void usesEmbeddedCommonNativeWithoutFetching() throws Exception {
        Path prepared = SqliteNativeLibraryAccess.prepareNative(directory, "Linux/x86_64",
                SqliteNativeLibrary.class.getClassLoader(), (source, target) -> {
                    throw new AssertionError("embedded native must not download the driver");
                });
        assertTrue(Files.isRegularFile(prepared));
    }

    @TestFactory
    Stream<DynamicTest> everySupportedNativeWorksOfflineWithOnlyThePreviousDriverCached() throws Exception {
        Path driver = Path.of(org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        Files.writeString(directory.resolve("sqlite-jdbc-3.53.4.0.jar"), "previous installation cache");
        try (JarFile jar = new JarFile(driver.toFile())) {
            return jar.stream().filter(entry -> !entry.isDirectory()
                    && entry.getName().startsWith("org/sqlite/native/")).map(entry -> {
                String resource = entry.getName();
                String folder = resource.substring("org/sqlite/native/".length(), resource.lastIndexOf('/'));
                return DynamicTest.dynamicTest(folder, () -> {
                    Path prepared = SqliteNativeLibraryAccess.prepareNative(directory, folder,
                            SqliteNativeLibrary.class.getClassLoader(), (source, target) -> {
                                throw new AssertionError("offline upgrade must not fetch a new driver");
                            });
                    assertTrue(prepared.startsWith(directory.resolve("sqlite-native")));
                    try (InputStream expected = SqliteNativeLibrary.class.getClassLoader().getResourceAsStream(resource)) {
                        assertArrayEquals(expected.readAllBytes(), Files.readAllBytes(prepared));
                    }
                    assertTrue(Files.notExists(directory.resolve(SqliteNativeLibraryAccess.DRIVER_FILE)));
                    assertTrue(Files.exists(directory.resolve("sqlite-jdbc-3.53.4.0.jar")));
                });
            }).toList().stream();
        }
    }

    @Test
    void embeddedNativePreparationDoesNotDependOnTheJvmTempDirectory() throws Exception {
        Path unusableTemp = directory.resolve("not-a-directory");
        Files.writeString(unusableTemp, "synthetic unusable temp directory");
        String oldJavaTemp = System.getProperty("java.io.tmpdir");
        String oldSqliteTemp = System.getProperty("org.sqlite.tmpdir");
        try {
            System.setProperty("java.io.tmpdir", unusableTemp.toString());
            System.setProperty("org.sqlite.tmpdir", unusableTemp.toString());
            Path prepared = SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64",
                    SqliteNativeLibrary.class.getClassLoader(), (source, target) -> {
                        throw new AssertionError("offline native must not download");
                    });
            assertTrue(prepared.startsWith(directory.resolve("sqlite-native")));
            assertTrue(Files.isRegularFile(prepared));
        } finally {
            restoreProperty("java.io.tmpdir", oldJavaTemp);
            restoreProperty("org.sqlite.tmpdir", oldSqliteTemp);
        }
    }

    private static void restoreProperty(String name, String value) {
        if (value == null) System.clearProperty(name);
        else System.setProperty(name, value);
    }

    @TestFactory
    Stream<DynamicTest> preservesExplicitNativeSelectionAcrossOverrideModes() {
        return Stream.of("path-only", "name-only", "both", "standard-name").map(mode ->
                DynamicTest.dynamicTest(mode, () -> {
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
                        previous.forEach(SqliteNativeLibraryTest::restoreProperty);
                    }
                }));
    }

    @TestFactory
    Stream<DynamicTest> rejectsCorruptEmptyAndOversizedEmbeddedNativesWithoutFetching() {
        return Stream.of(new byte[0], new byte[] { 1, 2, 3 }, new byte[2 * 1024 * 1024 + 1])
                .map(bytes -> DynamicTest.dynamicTest("invalid native length " + bytes.length, () -> {
                    ClassLoader corrupt = new ClassLoader(ClassLoader.getPlatformClassLoader()) {
                        @Override public InputStream getResourceAsStream(String name) {
                            return new ByteArrayInputStream(bytes);
                        }
                    };
                    IOException error = assertThrows(IOException.class,
                            () -> SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64", corrupt,
                                    (source, target) -> { throw new AssertionError("must reject corrupt embedded native"); }));
                    assertTrue(error.getMessage().contains("SHA-256"));
                    assertTrue(Files.notExists(directory.resolve("sqlite-native")));
                }));
    }

    @Test
    void missingNativeAndUnverifiedCacheCannotSucceedOffline() throws Exception {
        Files.writeString(directory.resolve(SqliteNativeLibraryAccess.DRIVER_FILE), "unverified cache");
        try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
            assertThrows(IOException.class, () -> SqliteNativeLibraryAccess.prepareNative(directory,
                    "Mac/x86_64", empty, (source, target) -> { throw new IOException("offline"); }));
            assertTrue(Files.notExists(directory.resolve("sqlite-native")));
        }
    }

    @Test
    void rejectsUnverifiedDownloadedDriver() throws Exception {
        try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
            assertThrows(IOException.class, () -> SqliteNativeLibraryAccess.prepareNative(directory,
                    "Mac/x86_64", empty, (source, target) -> Files.writeString(target, "unverified driver")));
            assertTrue(Files.notExists(directory.resolve(SqliteNativeLibraryAccess.DRIVER_FILE)));
            assertTrue(Files.notExists(directory.resolve("sqlite-native")));
        }
    }

    @Test
    void verifiesDriverAndExtractsUncommonNative() throws Exception {
        Path driver = Path.of(org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        assertTrue(Files.isRegularFile(driver));
        try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
            Path prepared = SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64", empty,
                    (source, target) -> Files.copy(driver, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING));
            assertNotNull(prepared);
            assertTrue(Files.isRegularFile(prepared));
            try (JarFile jar = new JarFile(driver.toFile())) {
                byte[] expected = jar.getInputStream(jar.getJarEntry(
                        "org/sqlite/native/Mac/x86_64/libsqlitejdbc.dylib")).readAllBytes();
                assertArrayEquals(expected, Files.readAllBytes(prepared));
            }
        }
    }

	@Test
	void usesPreprovisionedVerifiedDriverWithoutFetching() throws Exception {
		Path driver = Path.of(org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		Files.createDirectories(directory);
		Files.copy(driver, directory.resolve(SqliteNativeLibraryAccess.DRIVER_FILE));
		try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
			Path prepared = SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64", empty,
					(source, target) -> { throw new AssertionError("verified pre-provisioned driver must be reused"); });
			assertTrue(Files.isRegularFile(prepared));
		}
	}

	@Test
	void extractsEachReloadToAUniqueNativePath() throws Exception {
		Path driver = Path.of(org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
			SqliteNativeLibraryAccess.ArtifactFetcher fetcher = (source, target) -> Files.copy(driver, target,
					java.nio.file.StandardCopyOption.REPLACE_EXISTING);
			Path first = SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64", empty, fetcher);
			Path second = SqliteNativeLibraryAccess.prepareNative(directory, "Mac/x86_64", empty, fetcher);
			assertNotEquals(first, second);
			assertTrue(Files.isRegularFile(second));
		}
	}
}
