package com.bencodez.votingplugin.tests.util;

import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.InvocationTargetException;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class ServerDataPublicationFenceTest {
    @TempDir Path directory;

    @Test void activeGenerationAndRetiringLeasesBlockReplacementOnlyInTheirOwnScope() {
        Path file = directory.resolve("ServerData.yml");
        var owner = ServerDataPublicationFence.beginGeneration(file);
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(file));
        var other = ServerDataPublicationFence.beginGeneration(directory.resolve("Other.yml"));
        var lease = owner.acquire();
        owner.retire();
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(file));
        lease.close(); lease.close();
        var successor = ServerDataPublicationFence.beginGeneration(file);
        assertThrows(IllegalStateException.class, owner::acquire);
        owner.retire();
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(file));
        successor.retire(); other.retire();
    }

    @Test void retiredOwnerCanRetryUntilASuccessorActuallyOwnsThePath() {
        Path file = directory.resolve("ServerData.yml");
        var owner = ServerDataPublicationFence.beginGeneration(file);
        owner.retire();
        try (var retry = owner.acquire()) {
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(file));
        }
        var successor = ServerDataPublicationFence.beginGeneration(file);
        successor.retire();
        assertThrows(IllegalStateException.class, owner::acquire,
                "even a stopped successor must fence an older writer");
    }

    @Test void participatingSeparateClassLoadersShareTheActualOwnershipBoundary() throws Exception {
        Path file = directory.resolve("ServerData.yml");
        var owner = ServerDataPublicationFence.beginGeneration(file);
        var lease = owner.acquire();
        owner.retire();
        try (URLClassLoader loader = new URLClassLoader(new java.net.URL[] {
                ServerDataPublicationFence.class.getProtectionDomain().getCodeSource().getLocation()
        }, ClassLoader.getPlatformClassLoader())) {
            Class<?> isolated = Class.forName(ServerDataPublicationFence.class.getName(), true, loader);
            assertNotSame(ServerDataPublicationFence.class, isolated);
            var begin = isolated.getMethod("beginGeneration", Path.class);
            InvocationTargetException blocked = assertThrows(InvocationTargetException.class,
                    () -> begin.invoke(null, file));
            assertInstanceOf(IllegalStateException.class, blocked.getCause());
            lease.close();
            Object successor = begin.invoke(null, file);
            try (AutoCloseable held = (AutoCloseable) isolated.getMethod("acquire").invoke(successor)) {
                assertThrows(IllegalStateException.class, owner::acquire);
            }
            isolated.getMethod("retire").invoke(successor);
        } finally { lease.close(); }
    }

    @Test void completeRegistryWrappersRejectMalformedOwnerDataWithoutTouchingAdjacentContent() throws Exception {
        Path file = directory.resolve("Événement 🎉").resolve("ServerData.yml");
        var owner = ServerDataPublicationFence.beginGeneration(file);
        var field = ServerDataPublicationFence.class.getDeclaredField("key");
        field.setAccessible(true);
        String key = (String) field.get(owner);
        var registry = System.getProperties();
        String original = registry.getProperty(key);
        String token = original.substring(0, original.indexOf(':'));
        String adjacent = "synthetic.fence.adjacent." + token;
        registry.setProperty(adjacent, "strings/data\nUnicode 🎉\r\nowner:0:false");
        try {
            for (Object malformed : List.of(token + ":-1:false", token + ":1025:false", token + ":00:false",
                    token + ":+0:false", token + ":٠:false",
                    token + ":0:False", token + ":0:false:extra", token + ":0:false\n",
                    "\uFEFF" + original, "not-a-uuid:0:false", new java.util.Properties())) {
                registry.put(key, malformed);
                assertThrows(IllegalStateException.class, owner::acquire);
                assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(file));
                assertSame(malformed, registry.get(key));
                assertEquals("strings/data\nUnicode 🎉\r\nowner:0:false", registry.getProperty(adjacent));
            }
        } finally { registry.setProperty(key, original); registry.remove(adjacent); owner.retire(); }
    }

    @Test void failedReleaseCanRetryAfterOwnershipStateIsRepaired() throws Exception {
        var owner = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        var lease = owner.acquire();
        owner.retire();
        var field = ServerDataPublicationFence.class.getDeclaredField("key");
        field.setAccessible(true);
        String key = (String) field.get(owner);
        var registry = System.getProperties();
        String original = registry.getProperty(key);
        registry.put(key, "synthetic malformed record");
        assertThrows(IllegalStateException.class, lease::close);
        registry.setProperty(key, original);
        lease.close();
        var successor = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        successor.retire();
    }
}
