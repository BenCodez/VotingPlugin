package com.bencodez.votingplugin.npc;

import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.Entity;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** World creation uses a region; subsequent cleanup follows the actual entity. */
class VotingNPCScheduler {
    private final VotingPluginMain plugin;
    private final Method locationOwner;
    private final Method entityOwner;

    VotingNPCScheduler(VotingPluginMain plugin) {
        this.plugin = plugin;
        Method location = null, entity = null;
        try {
            Class.forName("io.papermc.paper.threadedregions.RegionizedServer");
            location = Bukkit.class.getMethod("isOwnedByCurrentRegion", Location.class);
            entity = Bukkit.class.getMethod("isOwnedByCurrentRegion", Entity.class);
        } catch (ClassNotFoundException ordinaryServer) {
            // Spigot/Paper have a single world-owner thread.
        } catch (ReflectiveOperationException unavailable) {
            throw new IllegalStateException("Voting NPC region ownership API is unavailable", unavailable);
        }
        locationOwner = location;
        entityOwner = entity;
    }

    boolean owns(Location location) { return owns(locationOwner, location); }
    boolean owns(Entity entity) { return owns(entityOwner, entity); }

    private boolean owns(Method method, Object value) {
        if (method == null) return Bukkit.isPrimaryThread();
        try { return Boolean.TRUE.equals(method.invoke(null, value)); }
        catch (ReflectiveOperationException failure) { throw new IllegalStateException("Cannot verify voting NPC ownership", failure); }
    }

    void global(Runnable action, Consumer<Throwable> failure) {
        try {
            plugin.getBukkitScheduler().getFoliaLib().getImpl().runNextTick(ignored -> action.run())
                    .whenComplete((ignored, failed) -> { if (failed != null) failure.accept(failed); });
        } catch (RuntimeException | LinkageError rejected) { failure.accept(rejected); }
    }

    void region(Location anchor, Runnable action, Consumer<Throwable> failure) {
        try {
            if (owns(anchor)) action.run();
            else plugin.getBukkitScheduler().getFoliaLib().getImpl().runAtLocation(anchor, ignored -> action.run())
                    .whenComplete((ignored, failed) -> { if (failed != null) failure.accept(failed); });
        } catch (RuntimeException | LinkageError rejected) { failure.accept(rejected); }
    }

    void entity(Entity entity, Runnable action, Runnable retired, Consumer<Throwable> failure) {
        AtomicBoolean executed = new AtomicBoolean();
        Runnable run = () -> { if (executed.compareAndSet(false, true)) action.run(); };
        Runnable gone = () -> { if (executed.compareAndSet(false, true)) retired.run(); };
        try {
            if (owns(entity)) { run.run(); return; }
            plugin.getBukkitScheduler().getFoliaLib().getImpl().runAtEntityWithFallback(entity,
                    ignored -> run.run(), gone).whenComplete((status, failed) -> {
                if (failed != null) failure.accept(failed);
                else if (status instanceof Enum<?> result && "ENTITY_RETIRED".equals(result.name())) gone.run();
                else if (!BukkitCompletionScheduler.isSuccessfulEntityTaskResult(status)) {
                    // Scheduler retirement does not prove the entity was removed.
                    failure.accept(new IllegalStateException("Voting NPC entity scheduler is unavailable"));
                }
            });
        } catch (RuntimeException | LinkageError rejected) { failure.accept(rejected); }
    }
}
