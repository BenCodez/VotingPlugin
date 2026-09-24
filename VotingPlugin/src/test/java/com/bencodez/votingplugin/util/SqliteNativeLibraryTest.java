package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SqliteNativeLibraryTest {
    @TempDir Path directory;

    @Test
    void usesEmbeddedCommonNativeWithoutFetching() throws Exception {
        Path prepared = SqliteNativeLibrary.prepareNative(directory, "Linux/x86_64",
                SqliteNativeLibrary.class.getClassLoader(), (source, target) -> {
                    throw new AssertionError("embedded native must not download the driver");
                });
        assertNull(prepared);
    }

    @Test
    void verifiesDriverAndExtractsUncommonNative() throws Exception {
        Path driver = Path.of(org.sqlite.JDBC.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        assertTrue(Files.isRegularFile(driver));
        try (URLClassLoader empty = new URLClassLoader(new URL[0], ClassLoader.getPlatformClassLoader())) {
            Path prepared = SqliteNativeLibrary.prepareNative(directory, "Mac/x86_64", empty,
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
}
