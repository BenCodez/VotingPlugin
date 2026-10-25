package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.lifecycle.AdvancedCoreLifecycle;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class GenerationFencedShutdownStageTest {
    @TempDir Path directory;

    private VotingPluginMain plugin(ServerDataPublicationFence fence) throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        doReturn(mock(Logger.class)).when(plugin).getLogger();
        // Mockito bypasses constructors; initialize the real SDK lifecycle lock.
        var lock = AdvancedCorePlugin.class.getDeclaredField("lifecycleLock");
        lock.setAccessible(true);
        lock.set(plugin, new Object());
        var owner = VotingPluginMain.class.getDeclaredField("serverDataPublicationFence");
        owner.setAccessible(true);
        owner.set(plugin, fence);
        return plugin;
    }

    @Test void disableReservesBeforeTheSDKAdmitsItsFinalStageAndRepeatedDisableDoesNotReleaseIt() throws Exception {
        var target = directory.resolve("ServerData.yml");
        var fence = ServerDataPublicationFence.beginGeneration(target);
        var plugin = plugin(fence);
        var finalFiles = new CompletableFuture<Void>();
        // This fixture controls only SDK hook admission; it does not claim a
        // full runtime cleanup. Adapter tests exercise the real file publisher.
        try (var cleanup = mockConstruction(AdvancedCoreLifecycle.class)) {
            plugin.onDisable();
            fence.retainUntilTermination(plugin.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            plugin.onShutdownComplete(finalFiles);
            plugin.onDisable();
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            finalFiles.complete(null);
            var successor = ServerDataPublicationFence.beginGeneration(target);
            successor.retire();
            plugin.onDisable();
            assertThrows(IllegalStateException.class, fence::acquire);
        }
    }

    @Test void failedFinalStageRetainsTheGenerationAfterAllCapturedExecutorsTerminate() throws Exception {
        checkFailure(false);
    }

    @Test void cancelledFinalStageRetainsTheGenerationAfterAllCapturedExecutorsTerminate() throws Exception {
        checkFailure(true);
    }

    private void checkFailure(boolean cancelled) throws Exception {
        var target = directory.resolve("ServerData.yml");
        var fence = ServerDataPublicationFence.beginGeneration(target);
        var plugin = plugin(fence);
        var finalFiles = new CompletableFuture<Void>();
        try (var cleanup = mockConstruction(AdvancedCoreLifecycle.class)) {
            plugin.onDisable();
            fence.retainUntilTermination(plugin.getServerDataWriterExecutors())
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
            plugin.onShutdownComplete(finalFiles);
            if (cancelled) finalFiles.cancel(false);
            else finalFiles.completeExceptionally(new java.io.IOException("synthetic final-stage failure"));
            plugin.onDisable();
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            // A later duplicate hook cannot replace the failed outcome.
            plugin.onShutdownComplete(CompletableFuture.completedFuture(null));
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
        }
    }
}
