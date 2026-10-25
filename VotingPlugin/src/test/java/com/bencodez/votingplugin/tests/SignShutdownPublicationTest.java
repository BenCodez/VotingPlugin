package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.signs.SignHandler;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class SignShutdownPublicationTest {
    @TempDir Path directory;

    private static final class Admission extends Signs {
        final List<Runnable> workers = new ArrayList<>();
        boolean reject;
        Admission(VotingPluginMain plugin) { super(plugin); }
        @Override protected void startShutdownWorker(Runnable work) {
            if (reject) throw new RejectedExecutionException("Synthetic rejection");
            workers.add(work);
        }
    }

    private VotingPluginMain plugin(ServerDataPublicationFence fence) {
        var plugin = mock(VotingPluginMain.class);
        when(plugin.getDataFolder()).thenReturn(directory.toFile());
        when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
        when(plugin.getServerDataPublicationFence()).thenReturn(fence);
        return plugin;
    }

    private SignHandler sign(VotingPluginMain plugin, String id, Location location, Location skull) throws Exception {
        var sign = mock(SignHandler.class, CALLS_REAL_METHODS);
        for (var entry : java.util.Map.<String, Object>of("plugin", plugin, "sign", id,
                "data", "SyntheticSign" + id, "position", 2).entrySet()) {
            var field = SignHandler.class.getDeclaredField(entry.getKey());
            field.setAccessible(true);
            field.set(sign, entry.getValue());
        }
        sign.setLocation(location);
        sign.setSkullLocation(skull);
        return sign;
    }

    @Test void rejectedAdmissionRetainsTheOriginalBatchAndPublishesItOnceOnRetry() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var plugin = plugin(fence);
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            backing.getData().set("VotingPlugin.Unrelated", "preserved");
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            var signs = new Admission(plugin);
            var owner = Thread.currentThread();
            var world = mock(World.class);
            when(world.getName()).thenAnswer(call -> {
                assertSame(owner, Thread.currentThread(), "Platform state must be captured before handoff");
                return "SyntheticWorld";
            });
            var location = new Location(world, -1.5, 64, 2.5);
            var first = sign(plugin, "0", location, new Location(world, 3, 65, 4));
            signs.getSigns().add(first);
            signs.getSigns().add(sign(plugin, "1", new Location(world, 5, 66, 6), null));
            signs.reject = true;
            assertThrows(RejectedExecutionException.class, signs::storeSignsForShutdown);
            fence.retire();
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            // Retry must use the accepted detached snapshot, not newer platform state.
            location.setX(90);
            first.setData("LaterUnacceptedState");
            signs.getSigns().clear();
            signs.reject = false;
            var completion = signs.storeSignsForShutdown();
            signs.storeSignsForShutdown();
            assertEquals(1, signs.workers.size());
            var worker = java.util.concurrent.Executors.newSingleThreadExecutor();
            try {
                worker.submit(signs.workers.getFirst()).get(5, TimeUnit.SECONDS);
                completion.toCompletableFuture().get(5, TimeUnit.SECONDS);
            } finally {
                worker.shutdownNow();
                assertTrue(worker.awaitTermination(5, TimeUnit.SECONDS));
            }
            var disk = YamlConfiguration.loadConfiguration(target.toFile());
            assertEquals(-1, disk.getInt("VotingPlugin.Signs.0.X"), "Preserve legacy coordinate truncation");
            assertEquals(3, disk.getInt("VotingPlugin.Signs.0.Skull.X"));
            assertEquals("SyntheticSign0", disk.getString("VotingPlugin.Signs.0.Data"));
            assertEquals(2, disk.getInt("VotingPlugin.Signs.1.Position"));
            assertEquals("preserved", disk.getString("VotingPlugin.Unrelated"));
            signs.storeSignsForShutdown();
            assertEquals(1, signs.workers.size(), "Successful publication is idempotent");
            ServerDataPublicationFence.beginGeneration(target).retire();
        }
    }

    @Test void cancellationAndForgedObservationCannotReleaseTheRealPublicationOwner() throws Exception {
        var target = directory.resolve("ServerData.yml");
        var fence = ServerDataPublicationFence.beginGeneration(target);
        var plugin = plugin(fence);
        var data = mock(ServerData.class);
        var durable = new CompletableFuture<Void>();
        when(data.publishShutdownSigns(anyList())).thenReturn(durable);
        when(plugin.getServerData()).thenReturn(data);
        var signs = new Admission(plugin);
        var world = mock(World.class);
        when(world.getName()).thenReturn("SyntheticWorld");
        signs.getSigns().add(sign(plugin, "0", new Location(world, 1, 2, 3), null));
        var pending = signs.storeSignsForShutdown();
        pending.toCompletableFuture().cancel(true);
        pending.toCompletableFuture().complete(null);
        signs.storeSignsForShutdown();
        assertEquals(1, signs.workers.size(), "Accepted but unexecuted work retains one owner");
        fence.retire();
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
        signs.workers.getFirst().run();
        assertFalse(pending.toCompletableFuture().isDone(), "Submission is not durability");
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
        durable.complete(null);
        pending.toCompletableFuture().get(5, TimeUnit.SECONDS);
        ServerDataPublicationFence.beginGeneration(target).retire();
    }

    @Test void failedPublicationRetainsPayloadAndRequiresAnExplicitSuccessfulRetry() throws Exception {
        var target = directory.resolve("ServerData.yml");
        var fence = ServerDataPublicationFence.beginGeneration(target);
        var plugin = plugin(fence);
        var data = mock(ServerData.class);
        var attempts = new ArrayList<List<ServerData.SignSnapshot>>();
        when(data.publishShutdownSigns(anyList())).thenAnswer(call -> {
            attempts.add(call.getArgument(0));
            return attempts.size() == 1 ? CompletableFuture.failedFuture(new IllegalStateException("Synthetic I/O"))
                    : CompletableFuture.completedFuture(null);
        });
        when(plugin.getServerData()).thenReturn(data);
        var signs = new Admission(plugin);
        var world = mock(World.class);
        when(world.getName()).thenReturn("SyntheticWorld");
        signs.getSigns().add(sign(plugin, "0", new Location(world, 1, 2, 3), null));
        var first = signs.storeSignsForShutdown();
        signs.workers.getFirst().run();
        assertThrows(java.util.concurrent.CompletionException.class, () -> first.toCompletableFuture().join());
        fence.retire();
        assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
        signs.getSigns().clear();
        var retry = signs.storeSignsForShutdown();
        signs.workers.get(1).run();
        retry.toCompletableFuture().get(5, TimeUnit.SECONDS);
        assertSame(attempts.get(0), attempts.get(1), "Failure must not discard or recapture the accepted batch");
        ServerDataPublicationFence.beginGeneration(target).retire();
    }

    @Test void emptyShutdownDoesNotAdmitAWorkerOrTouchServerData() {
        var plugin = mock(VotingPluginMain.class);
        var signs = new Admission(plugin);
        assertDoesNotThrow(() -> signs.storeSignsForShutdown().toCompletableFuture().join());
        assertEquals(0, signs.workers.size());
        verifyNoInteractions(plugin);
    }

    @Test void actualSdkIoFailureKeepsTheGenerationBlocked() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var plugin = plugin(fence);
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin) {
                @Override protected void moveAtomically(Path source, Path destination) throws java.io.IOException {
                    throw new java.io.IOException("Synthetic failed atomic publication");
                }
            };
            backing.setData(new YamlConfiguration());
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            var signs = new Admission(plugin);
            var world = mock(World.class);
            when(world.getName()).thenReturn("SyntheticWorld");
            signs.getSigns().add(sign(plugin, "0", new Location(world, 1, 2, 3), null));
            var result = signs.storeSignsForShutdown();
            signs.workers.getFirst().run();
            assertThrows(java.util.concurrent.CompletionException.class, () -> result.toCompletableFuture().join());
            assertFalse(java.nio.file.Files.exists(target));
            fence.retire();
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target),
                    "Neither the SDK guard nor sign handoff may call failed I/O durable");
        }
    }

    @Test void runtimeSignSavingRemainsSynchronous() throws Exception {
        var plugin = mock(VotingPluginMain.class);
        var data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        var signs = new Admission(plugin);
        var location = new Location(mock(World.class), 1, 2, 3);
        signs.getSigns().add(sign(plugin, "0", location, null));
        var caller = Thread.currentThread();
        doAnswer(call -> { assertSame(caller, Thread.currentThread()); return null; })
                .when(data).setSign("0", location, null, "SyntheticSign0", 2);
        signs.storeSigns();
        verify(data).setSign("0", location, null, "SyntheticSign0", 2);
        assertEquals(0, signs.workers.size());
    }
}
