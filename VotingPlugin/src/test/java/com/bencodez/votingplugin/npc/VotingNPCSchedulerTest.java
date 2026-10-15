package com.bencodez.votingplugin.npc;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;

class VotingNPCSchedulerTest {
    @Test void alreadyOwnedCleanupRunsInlineEvenDuringDisable() {
        var plugin = mock(VotingPluginMain.class);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
            var scheduler = new VotingNPCScheduler(plugin); var ran = new AtomicInteger();
            scheduler.entity(mock(Entity.class), ran::incrementAndGet, () -> fail("retired"), error -> fail(error));
            assertEquals(1, ran.get()); verify(plugin, never()).getBukkitScheduler();
        }
    }

    @Test void deferredRegionActionUsesTheLocationSchedulerAndReportsFailure() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        try (var bukkit = mockStatic(Bukkit.class)) {
            var scheduler = new VotingNPCScheduler(plugin); Location anchor = mock(Location.class);
            var completion = new CompletableFuture<Void>();
            var callback = new AtomicReference<Runnable>();
            when(plugin.getBukkitScheduler().getFoliaLib().getImpl().runAtLocation(eq(anchor), any()))
                    .thenAnswer(inv -> { Consumer<?> action = inv.getArgument(1); callback.set(() -> action.accept(null)); return completion; });
            var ran = new AtomicInteger(); List<Throwable> failures = new ArrayList<>();
            scheduler.region(anchor, ran::incrementAndGet, failures::add);
            assertEquals(0, ran.get()); callback.get().run(); assertEquals(1, ran.get());
            completion.completeExceptionally(new IllegalStateException("Synthetic scheduler failure"));
            assertEquals(1, failures.size());
        }
    }

    @Test @SuppressWarnings({"unchecked", "rawtypes"})
    void schedulerRetirementDoesNotPretendThatAnEntityWasRemoved() throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        try (var bukkit = mockStatic(Bukkit.class)) {
            var scheduler = new VotingNPCScheduler(plugin); Entity entity = mock(Entity.class);
            CompletableFuture completion = new CompletableFuture();
            when(plugin.getBukkitScheduler().getFoliaLib().getImpl().runAtEntityWithFallback(eq(entity), any(), any()))
                    .thenReturn(completion);
            var ran = new AtomicInteger(); var retired = new AtomicInteger(); List<Throwable> failures = new ArrayList<>();
            scheduler.entity(entity, ran::incrementAndGet, retired::incrementAndGet, failures::add);
            completion.complete(Enum.valueOf((Class) Class.forName("com.bencodez.simpleapi.folialib.enums.EntityTaskResult"), "SCHEDULER_RETIRED"));
            assertEquals(0, ran.get()); assertEquals(0, retired.get()); assertEquals(1, failures.size());
        }
    }

    @Test @SuppressWarnings({"unchecked", "rawtypes"})
    void entityRetirementFallbackAndFutureCompletionAreHandledOnce() throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        try (var bukkit = mockStatic(Bukkit.class)) {
            var scheduler = new VotingNPCScheduler(plugin); Entity entity = mock(Entity.class);
            CompletableFuture completion = new CompletableFuture(); var fallback = new AtomicReference<Runnable>();
            when(plugin.getBukkitScheduler().getFoliaLib().getImpl().runAtEntityWithFallback(eq(entity), any(), any()))
                    .thenAnswer(inv -> { fallback.set(inv.getArgument(2)); return completion; });
            var retired = new AtomicInteger();
            scheduler.entity(entity, () -> fail("Entity already retired"), retired::incrementAndGet, error -> fail(error));
            fallback.get().run();
            completion.complete(Enum.valueOf((Class) Class.forName("com.bencodez.simpleapi.folialib.enums.EntityTaskResult"), "ENTITY_RETIRED"));
            assertEquals(1, retired.get());
        }
    }
}
