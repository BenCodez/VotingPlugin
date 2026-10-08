package com.bencodez.votingplugin.hologram;

import java.lang.reflect.Method;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Uses entity ownership for players and fixed region ownership for stationary menu entities. */
class HologramMenuScheduler {
    private final VotingPluginMain plugin;
    private final Method regionExecute;
    private final Object regionScheduler;
    private final Method ownsLocation;

    HologramMenuScheduler(VotingPluginMain plugin) {
        this.plugin = plugin;
        Method execute = null;
        Method owns = null;
        Object scheduler = null;
        try {
            Class.forName("io.papermc.paper.threadedregions.RegionizedServer");
            scheduler = Bukkit.class.getMethod("getRegionScheduler").invoke(null);
            execute = Class.forName("io.papermc.paper.threadedregions.scheduler.RegionScheduler")
                    .getMethod("execute", Plugin.class, org.bukkit.World.class, int.class, int.class, Runnable.class);
            owns = Bukkit.class.getMethod("isOwnedByCurrentRegion", Location.class);
        } catch (ClassNotFoundException notFolia) {
            // Spigot/Paper have one world-owner thread. No Folia API is linked on these servers.
        } catch (ReflectiveOperationException incompatibleFolia) {
            throw new IllegalStateException("Folia region scheduler unavailable", incompatibleFolia);
        }
        regionExecute = execute;
        regionScheduler = scheduler;
        ownsLocation = owns;
    }

    void player(Player player, Runnable action, Runnable retired) {
        BukkitCompletionScheduler.run(plugin, player, action, retired, retired);
    }

    void region(Location anchor, Runnable action) {
        if (regionExecute == null) {
            if (Bukkit.isPrimaryThread()) action.run();
            else plugin.getBukkitScheduler().runTask(plugin, action, anchor);
            return;
        }
        try {
            // Raw execute survives subsequent task cancellation during PluginDisableEvent.
            // Every managed entity is stationary and removed from this same fixed region.
            regionExecute.invoke(regionScheduler, plugin, anchor.getWorld(), anchor.getBlockX() >> 4,
                    anchor.getBlockZ() >> 4, action);
        } catch (ReflectiveOperationException failed) {
            throw new IllegalStateException("Cannot schedule hologram region cleanup", failed);
        }
    }

    boolean owns(Location location) {
        if (ownsLocation == null) return Bukkit.isPrimaryThread();
        try {
            return Boolean.TRUE.equals(ownsLocation.invoke(null, location));
        } catch (ReflectiveOperationException failure) {
            throw new IllegalStateException("Cannot verify hologram region ownership", failure);
        }
    }

    Runnable watchPlayer(Player player, Runnable action, Runnable retired) {
        var task = plugin.getBukkitScheduler().getFoliaLib().getImpl()
                .runAtEntityTimer(player, action, retired, 20L, 20L);
        return task::cancel;
    }
}
