package com.bencodez.votingplugin.npc;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class VotingNPCStoreTest {
    @TempDir Path directory;

    private VotingNPCDefinition definition(int id) {
        return new VotingNPCDefinition("npc-" + id, new UUID(0, id), new UUID(1, 0),
                -0.5, 64, 16.5, 90, 0, "Vote");
    }

    @Test void roundTripPreservesStableIdentitiesAndPositionAcrossRestart() throws Exception {
        Path file = directory.resolve("npcs.json");
        var store = new VotingNPCStore(file);
        assertEquals(List.of(), store.load());
        store.save(List.of(definition(1), definition(2)));
        assertEquals(List.of(definition(1), definition(2)), new VotingNPCStore(file).load());
        assertEquals(-1, definition(1).chunkX());
        assertEquals(1, definition(1).chunkZ());
    }

    @Test void missingInitialReadDoesNotCreateFilesAndUninitializedStoreCannotOverwrite() throws Exception {
        Path file = directory.resolve("npcs.json");
        var store = new VotingNPCStore(file);
        assertThrows(IOException.class, () -> store.save(List.of(definition(1))));
        assertFalse(Files.exists(file));
        assertTrue(store.load().isEmpty());
        assertFalse(Files.exists(file));
    }

    @Test void invalidLoadKeepsOriginalBytesAndBlocksMutations() throws Exception {
        Path file = directory.resolve("npcs.json");
        byte[] original = "{ damaged definitions".getBytes(StandardCharsets.UTF_8);
        Files.write(file, original);
        var store = new VotingNPCStore(file);
        assertThrows(IOException.class, store::load);
        assertThrows(IOException.class, () -> store.save(List.of()));
        assertArrayEquals(original, Files.readAllBytes(file));
    }

    @Test void duplicateIdentitiesAndCapacityRejectionDoNotChangeDurableDefinitions() throws Exception {
        Path file = directory.resolve("npcs.json");
        var store = new VotingNPCStore(file); store.load(); store.save(List.of(definition(1)));
        byte[] before = Files.readAllBytes(file);
        assertThrows(IOException.class, () -> store.save(List.of(definition(1), definition(1))));
        var sameIdentity = new VotingNPCDefinition("another", definition(1).identity(), definition(1).world(),
                0, 64, 0, 0, 0, "Vote");
        assertThrows(IOException.class, () -> store.save(List.of(definition(1), sameIdentity)));
        assertThrows(IOException.class, () -> store.save(IntStream.rangeClosed(1, 65).mapToObj(this::definition).toList()));
        assertArrayEquals(before, Files.readAllBytes(file));
    }

    @Test void publicationFailureFencesOldStateUntilCheckedReload() throws Exception {
        Path file = directory.resolve("npcs.json");
        AtomicBoolean fail = new AtomicBoolean();
        var store = new VotingNPCStore(file) {
            @Override void forceDirectory(Path parent) throws IOException {
                if (fail.get()) throw new IOException("Synthetic directory sync failure");
                super.forceDirectory(parent);
            }
        };
        store.load(); store.save(List.of(definition(1)));
        fail.set(true);
        assertThrows(IOException.class, () -> store.save(List.of(definition(1), definition(2))));
        byte[] uncertain = Files.readAllBytes(file);
        fail.set(false);
        assertThrows(IOException.class, () -> store.save(List.of(definition(1))));
        assertArrayEquals(uncertain, Files.readAllBytes(file));
        assertEquals(List.of(definition(1), definition(2)), store.load());
        store.save(List.of(definition(2)));
        assertEquals(List.of(definition(2)), new VotingNPCStore(file).load());
    }

    @Test void missingUncertainFileDoesNotBecomeAnEmptySuccessfulRecovery() throws Exception {
        Path file = directory.resolve("npcs.json");
        var store = new VotingNPCStore(file) {
            @Override void forceDirectory(Path ignored) throws IOException { throw new IOException("Synthetic failure"); }
        };
        store.load(); assertThrows(IOException.class, () -> store.save(List.of(definition(1))));
        Files.delete(file);
        assertThrows(IOException.class, store::load);
        assertThrows(IOException.class, () -> store.save(List.of()));
        assertFalse(Files.exists(file));
    }

    @Test void strictFieldsAndTypesRejectMalformedDocumentsIndividually() throws Exception {
        Path file = directory.resolve("npcs.json");
        var store = new VotingNPCStore(file); store.load(); store.save(List.of(definition(1)));
        String healthy = Files.readString(file);
        for (String damaged : List.of(
                healthy.replace("\"version\": 1", "\"version\": 1, \"version\": 1"),
                healthy.replace("\"version\": 1", "\"version\": \"1\""),
                healthy.replace("\"version\": 1", "\"version\": 1.0"),
                healthy.replace("\"id\": \"npc-1\"", "\"id\": \"npc-1\", \"id\": \"other\""),
                healthy.replace("\"x\": -0.5", "\"x\": \"-0.5\""),
                healthy.replace("\"name\": \"Vote\"", "\"unknown\": \"Vote\""),
                healthy.replace("\"name\": \"Vote\"", "\"name\": null"),
                healthy.replace("\"yaw\": 90.0", "\"yaw\": 1e100"),
                healthy + "{}")) {
            Files.writeString(file, damaged);
            assertThrows(IOException.class, () -> new VotingNPCStore(file).load(), damaged);
            assertEquals(damaged, Files.readString(file));
        }
    }

    @Test void invalidUtf8OversizeAndSymlinkCannotBeLoadedOrOverwritten() throws Exception {
        Path file = directory.resolve("npcs.json");
        Files.write(file, new byte[] {(byte) 0xc3, (byte) 0x28});
        assertThrows(IOException.class, () -> new VotingNPCStore(file).load());
        Files.write(file, new byte[128 * 1024 + 1]);
        assertThrows(IOException.class, () -> new VotingNPCStore(file).load());
        Files.delete(file);
        Path target = directory.resolve("unrelated.json");
        Files.writeString(target, "unrelated");
        Files.createSymbolicLink(file, target);
        assertThrows(IOException.class, () -> new VotingNPCStore(file).load());
        assertEquals("unrelated", Files.readString(target));
    }

    @Test void linkedDirectoriesAllowDurableDefinitionsButLeafLinksStayRejected() throws Exception {
        Path real = Files.createDirectory(directory.resolve("real"));
        Path linked = Files.createSymbolicLink(directory.resolve("linked"), real);
        Path nested = Files.createDirectory(real.resolve("data"));
        for (Path parent : List.of(linked, linked.resolve("data"))) {
            Path file = parent.resolve("npcs.json");
            var store = new VotingNPCStore(file);
            assertTrue(store.load().isEmpty());
            store.save(List.of(definition(1)));
            assertEquals(List.of(definition(1)), new VotingNPCStore(file).load());
        }
        Path leaf = linked.resolve("alias.json");
        Files.createSymbolicLink(leaf, nested.resolve("npcs.json"));
        assertThrows(IOException.class, () -> new VotingNPCStore(leaf).load());
        assertEquals(List.of(definition(1)), new VotingNPCStore(nested.resolve("npcs.json")).load());
    }

    @Test void invalidDefinitionsAreRejectedBeforePublication() {
        assertThrows(IllegalArgumentException.class, () -> new VotingNPCDefinition("bad id", new UUID(0, 1),
                new UUID(1, 0), 0, 64, 0, 0, 0, "Vote"));
        assertThrows(IllegalArgumentException.class, () -> new VotingNPCDefinition("valid", new UUID(0, 1),
                new UUID(1, 0), Double.NaN, 64, 0, 0, 0, "Vote"));
        assertThrows(IllegalArgumentException.class, () -> new VotingNPCDefinition("valid", new UUID(0, 1),
                new UUID(1, 0), 0, 64, 0, 0, 0, "Vote\nNew line"));
    }
}
