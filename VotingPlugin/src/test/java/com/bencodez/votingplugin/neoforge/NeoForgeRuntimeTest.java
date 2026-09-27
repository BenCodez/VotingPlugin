package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import net.neoforged.fml.common.Mod;

class NeoForgeRuntimeTest {
    @TempDir Path directory;

    @Test
    void startsWithExistingConfigurationAndStorageThenClosesCleanly() throws IOException {
        NeoForgeRuntime runtime = NeoForgeRuntime.start(directory);
        try {
            assertTrue(Files.isRegularFile(directory.resolve("Config.yml")));
            assertTrue(Files.isRegularFile(directory.resolve("VoteSites.yml")));
            assertEquals("SQLITE", runtime.config().node("DataStorage").getString());
            assertFalse(runtime.voteSites().empty());
            assertTrue(runtime.storage().isOpen());
            assertTrue(Files.isRegularFile(directory.resolve("VotingPlugin.db")));

            runtime.close();
            runtime.close();
            assertFalse(runtime.storage().isOpen());
            assertThrows(RejectedExecutionException.class, () -> runtime.scheduler().execute(() -> {}));

            try (NeoForgeRuntime restarted = NeoForgeRuntime.start(directory)) {
                assertTrue(restarted.storage().isOpen());
            }
        } finally {
            runtime.close();
        }
    }

    @Test
    void rejectsUnsupportedStorageWithoutStartingSqlite() throws IOException {
        Files.writeString(directory.resolve("Config.yml"), "DataStorage: MYSQL\n");
        IOException failure = assertThrows(IOException.class, () -> NeoForgeRuntime.start(directory));
        assertTrue(failure.getMessage().contains("only SQLITE"));
        assertFalse(Files.exists(directory.resolve("VotingPlugin.db")));
    }

    @Test
    void serverTickSchedulerRunsQueuedWorkAndDropsItAtShutdown() {
        NeoForgeServerScheduler scheduler = new NeoForgeServerScheduler();
        AtomicInteger calls = new AtomicInteger();
        scheduler.execute(() -> {
            calls.incrementAndGet();
            scheduler.execute(calls::incrementAndGet);
        });
        scheduler.onServerTick();
        assertEquals(1, calls.get());
        scheduler.onServerTick();
        assertEquals(2, calls.get());
        scheduler.execute(calls::incrementAndGet);
        scheduler.close();
        scheduler.onServerTick();
        assertEquals(2, calls.get());
        assertThrows(RejectedExecutionException.class, () -> scheduler.execute(calls::incrementAndGet));
    }

    @Test
    void identityAdapterTracksLoginAndLogout() {
        NeoForgePlayerDirectory players = new NeoForgePlayerDirectory();
        UUID uuid = UUID.randomUUID();
        FakePlayer player = new FakePlayer(uuid, "Ben");
        players.joined(player);
        assertEquals("Ben", players.online(uuid).orElseThrow().playerName());
        assertTrue(players.online(uuid).orElseThrow().online());
        assertEquals(player, NeoForgePlayerDirectory.playerFromEvent(new FakeEvent(player)));
        players.left(player);
        assertTrue(players.online(uuid).isEmpty());
    }

    @Test
    void packagedEntryPointHasMatchingModId() {
        assertEquals("votingplugin", NeoForgeVotingPlugin.class.getAnnotation(Mod.class).value());
        assertTrue(NeoForgeRuntimeTest.class.getClassLoader().getResource("META-INF/neoforge.mods.toml") != null);
    }

    @Test
    void shutdownContinuesAfterAnEarlierResourceFails() {
        AtomicInteger closed = new AtomicInteger();
        IllegalStateException first = new IllegalStateException("socket close failed");
        IllegalArgumentException second = new IllegalArgumentException("later close failed");

        RuntimeException thrown = NeoForgeRuntime.closeAll(
                () -> { throw first; },
                closed::incrementAndGet,
                () -> { throw second; },
                closed::incrementAndGet);

        assertEquals(first, thrown);
        assertEquals(2, closed.get());
        assertEquals(1, thrown.getSuppressed().length);
        assertEquals(second, thrown.getSuppressed()[0]);
    }

    @Test
    void startupCleanupPreservesOriginalFailureAndClosesEveryResource() {
        AtomicInteger closed = new AtomicInteger();
        IllegalStateException startup = new IllegalStateException("replay start failed");
        IllegalArgumentException firstCleanup = new IllegalArgumentException("proxy close failed");
        IllegalStateException secondCleanup = new IllegalStateException("replay close failed");

        RuntimeException cleanup = NeoForgeRuntime.closeAll(
                () -> { throw firstCleanup; },
                closed::incrementAndGet,
                () -> { throw secondCleanup; },
                closed::incrementAndGet);
        if (cleanup != null) startup.addSuppressed(cleanup);

        assertEquals(2, closed.get());
        assertEquals(1, startup.getSuppressed().length);
        assertEquals(firstCleanup, startup.getSuppressed()[0]);
        assertEquals(1, firstCleanup.getSuppressed().length);
        assertEquals(secondCleanup, firstCleanup.getSuppressed()[0]);
    }

    public record FakePlayer(UUID uuid, String name) {
        public UUID getUUID() { return uuid; }
        public FakeName getName() { return new FakeName(name); }
    }
    public record FakeName(String value) {
        public String getString() { return value; }
    }
    public record FakeEvent(FakePlayer entity) {
        public FakePlayer getEntity() { return entity; }
    }
}
