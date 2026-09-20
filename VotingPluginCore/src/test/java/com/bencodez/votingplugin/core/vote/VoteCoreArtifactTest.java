package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.List;
import java.util.jar.JarFile;

import org.junit.jupiter.api.Test;

class VoteCoreArtifactTest {
    @Test
    void packagedVoteCoreContainsOnlyStandaloneSharedClasses() throws Exception {
        Path path = Path.of(System.getProperty("votingplugin.voteCoreJar"));
        try (JarFile jar = new JarFile(path.toFile());
                URLClassLoader loader = new URLClassLoader(new java.net.URL[] { path.toUri().toURL() }, null)) {
            List<String> entries = jar.stream().filter(entry -> !entry.isDirectory())
                    .map(entry -> entry.getName()).toList();
            assertTrue(entries.contains("com/bencodez/votingplugin/core/vote/SharedVoteProcessor.class"));
            assertTrue(entries.contains("com/bencodez/votingplugin/core/vote/SharedVotePolicy.class"));
            assertFalse(entries.contains("plugin.yml"));
            for (String entry : entries) {
                if (entry.startsWith("META-INF/")) continue;
                assertTrue(entry.startsWith("com/bencodez/votingplugin/core/vote/") && entry.endsWith(".class"),
                        () -> "Unexpected vote core JAR entry: " + entry);
                Class.forName(entry.substring(0, entry.length() - 6).replace('/', '.'), false, loader);
            }
        }
    }
}
