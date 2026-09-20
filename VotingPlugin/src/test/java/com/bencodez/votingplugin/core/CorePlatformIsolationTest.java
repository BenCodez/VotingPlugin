package com.bencodez.votingplugin.core;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

class CorePlatformIsolationTest {
    private static final Pattern PLATFORM_IMPORT = Pattern.compile(
            "^import\\s+(?:static\\s+)?(?:org\\.bukkit|com\\.destroystokyo\\.paper|io\\.papermc\\.paper|"
                    + "net\\.fabricmc|net\\.minecraftforge|net\\.neoforged|cpw\\.mods)(?:\\.|;)");

    @Test
    void coreAndSubpackagesDoNotImportLoaderApis() throws IOException {
        Path core = Path.of(System.getProperty("basedir"), "src/main/java/com/bencodez/votingplugin/core");
        try (Stream<Path> sources = Files.walk(core)) {
            List<Path> javaFiles = sources.filter(path -> path.toString().endsWith(".java")).toList();
            assertFalse(javaFiles.isEmpty(), () -> "Core source directory has no Java files: " + core);
            List<String> violations = javaFiles.stream().flatMap(path -> importsFrom(path, core).stream()).toList();
            assertTrue(violations.isEmpty(),
                    () -> "Platform API imports in core: " + violations);
        }
    }

    private static List<String> importsFrom(Path source, Path core) {
        try {
            return Files.readAllLines(source).stream().map(String::strip)
                    .filter(line -> PLATFORM_IMPORT.matcher(line).find())
                    .map(line -> core.relativize(source) + " -> " + line).toList();
        } catch (IOException failure) {
            throw new IllegalStateException("Could not inspect " + source, failure);
        }
    }
}
