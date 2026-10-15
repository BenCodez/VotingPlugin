package com.bencodez.votingplugin.npc;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.entity.Villager;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityCombustEvent;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.entity.EntityInteractEvent;
import org.bukkit.event.entity.EntityTargetEvent;
import org.bukkit.event.entity.EntityTeleportEvent;
import org.bukkit.event.entity.EntityTransformEvent;
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.event.vehicle.VehicleEnterEvent;
import org.bukkit.event.world.ChunkLoadEvent;
import org.bukkit.event.world.ChunkUnloadEvent;
import org.bukkit.event.world.WorldLoadEvent;
import org.bukkit.event.world.WorldUnloadEvent;
import org.bukkit.inventory.EquipmentSlot;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Persistent definitions, transient public villagers, and no vote/reward accounting. */
public final class VotingNPCManager implements Listener, AutoCloseable {
    private final VotingPluginMain plugin;
    private final VotingNPCScheduler scheduler;
    private final Consumer<Player> open;
    private final Map<String, RuntimeNPC> resources = new ConcurrentHashMap<>();
    private final Map<UUID, RuntimeNPC> entities = new ConcurrentHashMap<>();
    private final Set<ChunkKey> unloadingChunks = ConcurrentHashMap.newKeySet();
    private final Set<UUID> unloadingWorlds = ConcurrentHashMap.newKeySet();
    private final Map<UUID, Long> recentClicks = new LinkedHashMap<>();
    private volatile VotingNPCRegistry registry;
    private volatile boolean enabled;
    private volatile boolean closed;
    private volatile long configuration;
    private final AtomicBoolean warned = new AtomicBoolean();

    private record ChunkKey(UUID world, int x, int z) { }
    private static final class RuntimeNPC {
        final VotingNPCDefinition definition;
        final Location anchor;
        final long configuration;
        boolean closed, cleanupQueued, reconstructRequested;
        Villager entity;
        UUID entityId;
        RuntimeNPC(VotingNPCDefinition definition, World world, long configuration) {
            this.definition = definition;
            this.configuration = configuration;
            anchor = new Location(world, definition.x(), definition.y(), definition.z(), definition.yaw(), definition.pitch());
        }
    }

    public VotingNPCManager(VotingPluginMain plugin) {
        this(plugin, new VotingNPCScheduler(plugin), null, player -> openVotingSites(plugin, player));
    }

    static void openVotingSites(VotingPluginMain plugin, Player player) {
        // Reuse the normal command's permissions, asynchronous user loading,
        // configured GUI, cooldown display, and URL handling; no root alias.
        for (var handler : plugin.getVoteCommand()) {
            if (handler.runCommand(player, new String[] { "URL" })) return;
        }
        player.sendMessage("Voting-site selection is unavailable.");
    }

    VotingNPCManager(VotingPluginMain plugin, VotingNPCScheduler scheduler, VotingNPCRegistry registry, Consumer<Player> open) {
        this.plugin = plugin;
        this.scheduler = scheduler;
        this.registry = registry;
        this.open = open;
    }

    public static boolean supported() {
        try {
            Entity.class.getMethod("setPersistent", boolean.class);
            Entity.class.getMethod("setGravity", boolean.class);
            Entity.class.getMethod("setInvulnerable", boolean.class);
            LivingEntity.class.getMethod("setAI", boolean.class);
            LivingEntity.class.getMethod("setCollidable", boolean.class);
            World.class.getMethod("spawn", Location.class, Class.class, Consumer.class);
            return true;
        } catch (ReflectiveOperationException | LinkageError unavailable) { return false; }
    }

    /** Configuration callers do no file I/O and do not wait for storage. */
    public void configure(boolean requested) {
        VotingNPCRegistry owner;
        CompletionStage<List<VotingNPCDefinition>> loading;
        long generation;
        boolean activate;
        synchronized (this) {
            if (closed) return;
            generation = ++configuration;
            activate = requested && supported();
            enabled = activate;
            if (!activate) { owner = null; loading = null; }
            else {
                if (registry == null) {
                    registry = new VotingNPCRegistry(plugin.getDataFolder().toPath().resolve("VotingNPCs.json"));
                    loading = registry.initialLoad();
                } else loading = registry.reload();
                owner = registry;
            }
        }
        if (!activate) {
            for (RuntimeNPC npc : resources.values()) retire(npc);
            if (requested) report(new IllegalStateException("Vanilla voting NPC API is unavailable"));
            return;
        }
        loading.whenComplete((definitions, failure) -> {
            synchronized (VotingNPCManager.this) {
                if (closed || !enabled || generation != configuration || registry != owner) return;
            }
            if (failure != null) {
                for (RuntimeNPC npc : resources.values()) retire(npc);
                report(failure);
            } else {
                warned.set(false);
                reconcile();
            }
        });
    }

    public boolean available() {
        VotingNPCRegistry owner = registry;
        return !closed && enabled && owner != null && owner.available();
    }

    public List<VotingNPCDefinition> definitions() {
        VotingNPCRegistry owner = registry;
        return owner == null ? List.of() : owner.snapshot();
    }

    /** Returns durable definition completion; runtime creation can still await a loaded chunk. */
    public CompletionStage<VotingNPCDefinition> create(VotingNPCDefinition definition) {
        VotingNPCRegistry owner = registry;
        if (!available()) return CompletableFuture.failedFuture(new IllegalStateException("Enable VotingNPCs and load valid storage first"));
        CompletionStage<VotingNPCDefinition> completion = owner.create(definition);
        completion.whenComplete((value, failure) -> mutationFinished(owner, failure));
        return completion;
    }

    public CompletionStage<Boolean> remove(String id) {
        VotingNPCRegistry owner = registry;
        if (!available()) return CompletableFuture.failedFuture(new IllegalStateException("Enable VotingNPCs and load valid storage first"));
        CompletionStage<Boolean> completion = owner.remove(id);
        completion.whenComplete((value, failure) -> mutationFinished(owner, failure));
        return completion;
    }

    private void mutationFinished(VotingNPCRegistry owner, Throwable failure) {
        if (owner != registry || closed) return;
        if (failure == null) reconcile();
        else if (!owner.available()) {
            for (RuntimeNPC npc : resources.values()) retire(npc);
            report(failure);
        }
    }

    private boolean current(RuntimeNPC npc) {
        return available() && definitions().contains(npc.definition);
    }

    private void reconcile() {
        List<VotingNPCDefinition> snapshot = definitions();
        Set<ChunkKey> chunks = new HashSet<>();
        Set<UUID> worlds = new HashSet<>();
        for (var value : snapshot) { chunks.add(key(value)); worlds.add(value.world()); }
        unloadingChunks.retainAll(chunks);
        unloadingWorlds.retainAll(worlds);
        for (RuntimeNPC npc : resources.values()) if (!current(npc)) retire(npc);
        if (!available()) return;
        scheduler.global(() -> {
            if (!available()) return;
            for (var value : definitions()) ensure(value);
        }, this::report);
    }

    private static ChunkKey key(VotingNPCDefinition value) { return new ChunkKey(value.world(), value.chunkX(), value.chunkZ()); }

    private void ensure(VotingNPCDefinition value) {
        if (!available() || !definitions().contains(value) || unloadingWorlds.contains(value.world()) || unloadingChunks.contains(key(value))) return;
        World world = Bukkit.getWorld(value.world());
        if (world == null) return;
        RuntimeNPC npc = new RuntimeNPC(value, world, configuration);
        RuntimeNPC previous;
        synchronized (resources) {
            previous = resources.get(value.id());
            if (previous == null) {
                if (resources.size() >= VotingNPCStore.MAX_NPCS) return;
                resources.put(value.id(), npc);
            }
        }
        if (previous != null) {
            synchronized (previous) { if (previous.closed) previous.reconstructRequested = true; }
            return;
        }
        scheduler.region(npc.anchor, () -> spawn(npc), failure -> { retire(npc); report(failure); });
    }

    private boolean loadedArea(RuntimeNPC npc) {
        World world = npc.anchor.getWorld();
        if (Bukkit.getWorld(npc.definition.world()) != world || unloadingWorlds.contains(npc.definition.world())
                || unloadingChunks.contains(key(npc.definition))) return false;
        for (double dx : new double[] { -.5, .5 }) for (double dz : new double[] { -.5, .5 }) {
            Location edge = npc.anchor.clone().add(dx, 0, dz);
            if (!scheduler.owns(edge) || !world.isChunkLoaded(edge.getBlockX() >> 4, edge.getBlockZ() >> 4)) return false;
        }
        return true;
    }

    private void spawn(RuntimeNPC npc) {
        try {
            synchronized (npc) {
                if (npc.closed || !current(npc) || !loadedArea(npc)) { forget(npc); return; }
                if (npc.entity != null) return;
                Villager spawned = npc.anchor.getWorld().spawn(npc.anchor, Villager.class, villager -> {
                    npc.entity = villager;
                    npc.entityId = villager.getUniqueId();
                    entities.put(npc.entityId, npc);
                    villager.setPersistent(false);
                    villager.setAI(false);
                    villager.setGravity(false);
                    villager.setInvulnerable(true);
                    villager.setSilent(true);
                    villager.setCollidable(false);
                    villager.setCanPickupItems(false);
                    villager.setRemoveWhenFarAway(false);
                    villager.setCustomName(ChatColor.translateAlternateColorCodes('&', npc.definition.name()));
                    villager.setCustomNameVisible(true);
                });
                if (spawned != npc.entity || !spawned.isValid() || !current(npc) || npc.closed) {
                    throw new IllegalStateException("Voting NPC spawn was cancelled or superseded");
                }
            }
        } catch (RuntimeException | LinkageError failure) { retire(npc); report(failure); }
    }

    private void retire(RuntimeNPC npc) {
        Villager entity;
        synchronized (npc) {
            npc.closed = true;
            entity = npc.entity;
            if (entity == null) { forget(npc); return; }
            if (npc.cleanupQueued) return;
            npc.cleanupQueued = true;
        }
        scheduler.entity(entity, () -> {
            try { entity.remove(); forget(npc); }
            catch (RuntimeException | LinkageError failure) { cleanupFailed(npc, failure); }
        }, () -> forget(npc), failure -> cleanupFailed(npc, failure));
    }

    private void cleanupFailed(RuntimeNPC npc, Throwable failure) {
        synchronized (npc) { npc.cleanupQueued = false; }
        // Keep the exact identity and capacity reservation. Rejected scheduling
        // is not successful removal and must not permit duplicate reconstruction.
        report(failure);
    }

    private void forget(RuntimeNPC npc) {
        boolean reconstruct;
        synchronized (npc) {
            reconstruct = npc.entity != null && (npc.reconstructRequested || npc.configuration != configuration
                    || definitions().stream().anyMatch(value -> value.id().equals(npc.definition.id()) && !value.equals(npc.definition)));
            if (npc.entityId != null) entities.remove(npc.entityId, npc);
            npc.entity = null;
            resources.remove(npc.definition.id(), npc);
        }
        // A successor sharing this configured ID must wait for exact predecessor
        // cleanup. Missing/unloaded spawn attempts do not recursively retry.
        if (reconstruct && available()) scheduler.global(() -> {
            for (var value : definitions()) if (value.id().equals(npc.definition.id())) ensure(value);
        }, this::report);
    }

    private void report(Throwable failure) {
        if (warned.compareAndSet(false, true)) {
            try {
                plugin.getLogger().warning("Voting NPCs could not complete an operation; definitions are retained. Check API support and VotingNPCs.json, then reload. Cause: "
                        + failure.getClass().getSimpleName());
            } catch (RuntimeException ignored) { /* Reporting must not prevent independent cleanup. */ }
        }
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interact(PlayerInteractEntityEvent event) {
        RuntimeNPC npc = entities.get(event.getRightClicked().getUniqueId());
        if (npc == null) return;
        boolean cancelled = event.isCancelled();
        event.setCancelled(true); // Only this exact owned entity; never trading.
        if (cancelled || event.getHand() != EquipmentSlot.HAND || !current(npc)) return;
        synchronized (npc) { if (npc.closed) { retire(npc); return; } }
        Player player = event.getPlayer();
        long now = System.nanoTime();
        synchronized (recentClicks) {
            recentClicks.entrySet().removeIf(entry -> now - entry.getValue() >= 300_000_000L);
            if (recentClicks.containsKey(player.getUniqueId()) || recentClicks.size() >= 256) return;
            recentClicks.put(player.getUniqueId(), now);
        }
        BukkitCompletionScheduler.run(plugin, player, () -> {
            if (!player.isOnline() || !current(npc) || !player.getWorld().getUID().equals(npc.definition.world())
                    || player.getLocation().distanceSquared(npc.anchor) > 36) return;
            synchronized (npc) { if (npc.closed) return; }
            open.accept(player);
        }, () -> { }, () -> { });
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interactAt(PlayerInteractAtEntityEvent event) { interact(event); }
    private boolean owned(Entity entity) { return entity != null && entities.containsKey(entity.getUniqueId()); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void damage(EntityDamageEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void combust(EntityCombustEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void teleport(EntityTeleportEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void transform(EntityTransformEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void target(EntityTargetEvent event) { if (owned(event.getEntity()) || owned(event.getTarget())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void blockInteract(EntityInteractEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void vehicle(VehicleEnterEvent event) { if (owned(event.getEntered())) event.setCancelled(true); }
    @EventHandler public void quit(PlayerQuitEvent event) { synchronized (recentClicks) { recentClicks.remove(event.getPlayer().getUniqueId()); } }

    @EventHandler(priority = EventPriority.MONITOR)
    public void chunkLoad(ChunkLoadEvent event) {
        UUID world = event.getWorld().getUID();
        unloadingChunks.remove(new ChunkKey(world, event.getChunk().getX(), event.getChunk().getZ()));
        for (var value : definitions()) if (value.world().equals(world)
                && Math.abs(value.chunkX() - event.getChunk().getX()) <= 1
                && Math.abs(value.chunkZ() - event.getChunk().getZ()) <= 1) ensure(value);
    }

    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void chunkUnload(ChunkUnloadEvent event) {
        ChunkKey chunk = new ChunkKey(event.getWorld().getUID(), event.getChunk().getX(), event.getChunk().getZ());
        if (definitions().stream().noneMatch(value -> key(value).equals(chunk))) return;
        unloadingChunks.add(chunk);
        for (RuntimeNPC npc : resources.values()) if (key(npc.definition).equals(chunk)) retire(npc);
    }

    @EventHandler(priority = EventPriority.MONITOR)
    public void worldLoad(WorldLoadEvent event) {
        UUID world = event.getWorld().getUID();
        if (definitions().stream().noneMatch(value -> value.world().equals(world))) return;
        unloadingWorlds.remove(world);
        reconcile();
    }

    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void worldUnload(WorldUnloadEvent event) {
        UUID world = event.getWorld().getUID();
        if (definitions().stream().noneMatch(value -> value.world().equals(world))) return;
        unloadingWorlds.add(world);
        for (RuntimeNPC npc : resources.values()) if (npc.definition.world().equals(world)) retire(npc);
    }

    public int runtimeCount() { return resources.size(); }

    public String runtimeState(String id) {
        RuntimeNPC npc = resources.get(id);
        if (npc == null) return "waiting for a loaded, available area";
        synchronized (npc) {
            if (npc.closed) return "cleanup pending";
            return npc.entity == null ? "creation scheduled" : "villager created";
        }
    }

    @Override public void close() {
        synchronized (this) { if (closed) return; closed = true; enabled = false; ++configuration; }
        VotingNPCRegistry owner = registry;
        if (owner != null) owner.close();
        for (RuntimeNPC npc : new ArrayList<>(resources.values())) retire(npc);
        synchronized (recentClicks) { recentClicks.clear(); }
    }
}
