package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.bstats.velocity.Metrics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.slf4j.Logger;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.velocitypowered.api.proxy.ProxyServer;
import com.velocitypowered.api.scheduler.ScheduledTask;

class VelocityConfigReloadTest {
    @TempDir Path directory;

    @Test void malformedNormalReloadPreservesMqttAndAdministratorSettings() throws Exception {
        Path file = validFile();
        VelocityConfig config = new VelocityConfig(file.toFile());
        var active = config.getData();
        String malformed = "BungeeMethod: [broken\n";
        Files.writeString(file, malformed);
        assertThrows(java.io.UncheckedIOException.class, config::reload);
        assertSame(active, config.getData());
        assertEquals("MQTT", config.getBungeeMethod());
        assertEquals("administrator", config.getNode("Custom", "Label").getString());
        assertEquals(malformed, Files.readString(file));
        Files.writeString(file, "BungeeMethod: REDIS\nCustom:\n  Label: repaired\n");
        config.reload();
        assertEquals("REDIS", config.getBungeeMethod());
        assertEquals("repaired", config.getNode("Custom", "Label").getString());
    }

    @Test void strictControlReloadStillRejectsWithoutReplacingSnapshot() throws Exception {
        Path file = validFile();
        VelocityConfig config = new VelocityConfig(file.toFile());
        var active = config.getData();
        Files.writeString(file, "BungeeMethod: [broken\n");
        assertThrows(IOException.class, config::loadControlConfiguration);
        assertSame(active, config.getData());
        assertEquals("MQTT", config.getBungeeMethod());
    }

    @Test void missingReloadPreservesSnapshotButMissingStartupUsesDefaults() throws Exception {
        Path file = validFile();
        VelocityConfig config = new VelocityConfig(file.toFile());
        var active = config.getData();
        Files.delete(file);
        assertThrows(java.io.UncheckedIOException.class, config::reload);
        assertSame(active, config.getData());
        assertFalse(Files.exists(file));
        VelocityConfig firstStartup = new VelocityConfig(file.toFile());
        assertEquals("PLUGINMESSAGING", firstStartup.getBungeeMethod());
        assertTrue(Files.exists(file));
    }

    @Test void malformedRuntimeReloadAbortsBeforeTasksChannelsOrTransportChange() throws Exception {
        for (boolean full : new boolean[] {false, true}) {
            Path file = validFile();
            VelocityConfig config = new VelocityConfig(file.toFile());
            var active = config.getData();
            ProxyServer server = mock(ProxyServer.class);
            Logger logger = mock(Logger.class);
            VotingPluginVelocity plugin = new VotingPluginVelocity(server, logger,
                    mock(Metrics.Factory.class), directory);
            VotingPluginProxy runtime = mock(VotingPluginProxy.class);
            ScheduledTask voteCheck = mock(ScheduledTask.class);
            ScheduledTask cacheSave = mock(ScheduledTask.class);
            setField(plugin, "config", config);
            setField(plugin, "votingPluginProxy", runtime);
            setField(plugin, "runtimeOperational", true);
            setField(plugin, "voteCheckTask", voteCheck);
            setField(plugin, "cacheSaveTask", cacheSave);
            String malformed = "BungeeMethod: [broken\n";
            Files.writeString(file, malformed);
            try {
                plugin.reloadAllInternal(full);
                assertSame(active, config.getData());
                assertEquals("MQTT", config.getBungeeMethod());
                assertTrue(plugin.isRuntimeOperational());
                assertFalse(plugin.isReloading());
                assertSame(runtime, plugin.getVotingPluginProxy());
                verifyNoInteractions(runtime, voteCheck, cacheSave, server);
                verify(logger).error("Failed to reload bungeeconfig.yml; the previous configuration and runtime remain active.");
                assertEquals(malformed, Files.readString(file));
            } finally { plugin.getTimer().shutdownNow(); }
        }
    }

    private Path validFile() throws IOException {
        Path file = directory.resolve("bungeeconfig.yml");
        Files.writeString(file, "BungeeMethod: MQTT\nCustom:\n  Label: administrator\n");
        return file;
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        var field = VotingPluginVelocity.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
    }
}
