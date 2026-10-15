package com.bencodez.votingplugin.experimental;

import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.logging.Level;

import org.bukkit.Bukkit;
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
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.vehicle.VehicleEnterEvent;
import org.bukkit.event.world.WorldUnloadEvent;
import org.bukkit.inventory.EquipmentSlot;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;

/** One temporary native Villager per session; no Citizens, fake players, storage or world scans. */
final class ExperimentalNPC implements Listener {
    private static final int MAX_NPCS = 64;
    private final VotingPluginMain plugin;
    private final HologramMenuScheduler scheduler;
    private final ExperimentalSessions sessions;
    private final Map<UUID, NPC> resources = new ConcurrentHashMap<>();
    private final Map<UUID, NPC> entities = new ConcurrentHashMap<>();

    private static final class NPC {
        final ExperimentalSessions.Session session;
        final Player player;
        final Location anchor;
        final String name;
        final Runnable selection;
        final AtomicBoolean selected = new AtomicBoolean();
        // Mutated under this record's monitor. Entity access itself belongs to the anchor region.
        boolean closed;
        boolean cleanupQueued;
        Villager entity;
        UUID entityId;
        boolean privateVisibility;

        NPC(ExperimentalSessions.Session session, Player player, Location anchor, String name, Runnable selection) {
            this.session = session;
            this.player = player;
            this.anchor = anchor.clone();
            this.name = name;
            this.selection = selection;
        }
    }

    ExperimentalNPC(VotingPluginMain plugin, HologramMenuScheduler scheduler, ExperimentalSessions sessions) {
        this.plugin = Objects.requireNonNull(plugin);
        this.scheduler = Objects.requireNonNull(scheduler);
        this.sessions = Objects.requireNonNull(sessions);
    }

    static boolean supported() {
        try {
            Entity.class.getMethod("setPersistent", boolean.class);
            Entity.class.getMethod("setGravity", boolean.class);
            Entity.class.getMethod("setInvulnerable", boolean.class);
            LivingEntity.class.getMethod("setAI", boolean.class);
            LivingEntity.class.getMethod("setCollidable", boolean.class);
            World.class.getMethod("spawn", Location.class, Class.class, Consumer.class);
            return true;
        } catch (ReflectiveOperationException | LinkageError unavailable) {
            return false;
        }
    }

    void open(ExperimentalSessions.Session session, Player player, Location anchor, String name, Runnable selection) {
        Objects.requireNonNull(session);
        Objects.requireNonNull(player);
        Objects.requireNonNull(selection);
        if (!sessions.current(session)) return;
        if (anchor == null || anchor.getWorld() == null || !Double.isFinite(anchor.getX())
                || !Double.isFinite(anchor.getY()) || !Double.isFinite(anchor.getZ())
                || !Float.isFinite(anchor.getYaw()) || !Float.isFinite(anchor.getPitch())
                || name == null || name.length() > 128 || !supported()) {
            sessions.close(session);
            return;
        }
        NPC npc = new NPC(session, player, anchor, name, selection);
        synchronized (resources) {
            // Repeated snapshots/refreshes do not create another NPC for the same session.
            if (resources.containsKey(session.id)) return;
            if (resources.size() >= MAX_NPCS) { sessions.close(session); return; }
            resources.put(session.id, npc);
        }
        try {
            // Own even a not-yet-spawned resource before either scheduler may accept work.
            session.own(() -> retire(npc), this::report);
            scheduler.player(player, () -> {
                try {
                    if (!validPlayer(npc, player)) { sessions.close(session); return; }
                    scheduler.region(npc.anchor, () -> spawn(npc));
                } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
            }, () -> sessions.close(session));
        } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
    }

    int entityCount() { return entities.size(); }

    /** Cleanup commands/reload can retry rejected removals after the session owner has retired. */
    void retryCleanup() {
        for (NPC npc : resources.values()) {
            boolean closed;
            synchronized (npc) { closed = npc.closed; }
            if (closed) retire(npc);
        }
    }

    private boolean current(NPC npc) {
        return !npc.closed && plugin.isEnabled() && sessions.current(npc.session);
    }

    // This method is called only on the player's owner scheduler, never from the spawn region.
    private boolean validPlayer(NPC npc, Player player) {
        synchronized (npc) {
            return current(npc) && npc.session.player.equals(player.getUniqueId()) && player.isOnline()
                    && (player.hasPermission("VotingPlugin.Admin")
                        || player.hasPermission("VotingPlugin.Commands.AdminVote." + npc.session.type.command()))
                    && player.getWorld().equals(npc.anchor.getWorld())
                    && player.getEyeLocation().distanceSquared(npc.anchor) <= 64;
        }
    }

    private boolean ownsArea(NPC npc) {
        if (!scheduler.owns(npc.anchor)) return false;
        // The Villager's bounds must not overlap a neighbouring region owned by another tick thread.
        for (double dx : new double[] { -.5, .5 }) for (double dz : new double[] { -.5, .5 })
            if (!scheduler.owns(npc.anchor.clone().add(dx, 0, dz))) return false;
        return true;
    }

    private boolean loaded(NPC npc) {
        World world = npc.anchor.getWorld();
        if (Bukkit.getWorld(world.getUID()) != world) return false;
        for (double dx : new double[] { -.5, .5 }) for (double dz : new double[] { -.5, .5 }) {
            Location edge = npc.anchor.clone().add(dx, 0, dz);
            if (!world.isChunkLoaded(edge.getBlockX() >> 4, edge.getBlockZ() >> 4)) return false;
        }
        return true;
    }

    private void spawn(NPC npc) {
        try {
            synchronized (npc) {
                if (!current(npc)) { sessions.close(npc.session); return; }
                if (npc.entity != null) return;
                if (!ownsArea(npc) || !loaded(npc)) {
                    throw new IllegalStateException("NPC area is unavailable or belongs to another region");
                }
                Villager spawned = npc.anchor.getWorld().spawn(npc.anchor, Villager.class, villager -> {
                    npc.entity = villager;
                    npc.entityId = villager.getUniqueId();
                    entities.put(npc.entityId, npc);
                    // No fallback to a persistent entity on old implementations.
                    villager.setPersistent(false);
                    if (!current(npc)) throw new IllegalStateException("NPC session retired during spawn");
                    villager.setAI(false);
                    villager.setGravity(false);
                    villager.setInvulnerable(true);
                    villager.setSilent(true);
                    villager.setCollidable(false);
                    villager.setCanPickupItems(false);
                    villager.setCustomName(npc.name);
                    villager.setCustomNameVisible(true);
                    villager.setRemoveWhenFarAway(false);
                    try {
                        Entity.class.getMethod("setVisibleByDefault", boolean.class);
                        Player.class.getMethod("showEntity", org.bukkit.plugin.Plugin.class, Entity.class);
                        villager.setVisibleByDefault(false);
                        npc.privateVisibility = true;
                    } catch (NoSuchMethodException olderVisibilityAPI) {
                        // Older Bukkit versions display a public Villager, still owner-interaction-only.
                    } catch (NoSuchMethodError | AbstractMethodError olderImplementation) {
                        // Implementations predating the visibility API retain public visibility.
                    }
                });
                if (spawned != npc.entity || !spawned.isValid() || !current(npc)) {
                    throw new IllegalStateException("NPC spawn was cancelled or its session retired");
                }
            }
            scheduler.player(npc.player, () -> {
                try {
                    synchronized (npc) {
                        if (!validPlayer(npc, npc.player)) { sessions.close(npc.session); return; }
                        if (npc.privateVisibility) {
                            // Showing touches tracking: the player's thread must own the stationary NPC too.
                            if (!ownsArea(npc)) { sessions.close(npc.session); return; }
                            npc.player.showEntity(plugin, npc.entity);
                        }
                        npc.session.activate();
                    }
                } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
            }, () -> sessions.close(npc.session));
        } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
    }

    private void retire(NPC npc) {
        synchronized (npc) {
            npc.closed = true;
            if (npc.entity == null) { resources.remove(npc.session.id, npc); return; }
            if (npc.cleanupQueued) return;
            npc.cleanupQueued = true;
        }
        try {
            // Inline removal, when already owned, also covers a partial-spawn exception before publication.
            if (ownsArea(npc)) removeOwned(npc);
            else scheduler.region(npc.anchor, () -> removeOwned(npc));
        } catch (RuntimeException | LinkageError failure) {
            // Final Folia shutdown can reject region work. Nonpersistent entities are discarded at unload.
            // Retain exact identity and the capacity slot until removal/unload; live rejection must not
            // permit unbounded orphan spawns. A later owned event may retry the cleanup safely.
            synchronized (npc) { npc.cleanupQueued = false; }
            report(failure);
        }
    }

    private void removeOwned(NPC npc) {
        try {
            synchronized (npc) {
                if (npc.entity == null) return;
                if (!ownsArea(npc)) throw new IllegalStateException("NPC removal requires its fixed region");
                World world = npc.anchor.getWorld();
                // An unloaded world already discarded the nonpersistent NPC; never reload it for cleanup.
                if (Bukkit.getWorld(world.getUID()) == world) npc.entity.remove();
            }
            forget(npc);
        } catch (RuntimeException | LinkageError failure) {
            synchronized (npc) { npc.cleanupQueued = false; }
            report(failure);
        }
    }

    private void forget(NPC npc) {
        synchronized (npc) {
            if (npc.entityId != null) entities.remove(npc.entityId, npc);
            npc.entity = null;
            resources.remove(npc.session.id, npc);
        }
    }

    private void fail(NPC npc, Throwable failure) {
        sessions.close(npc.session);
        report(failure);
    }

    private void report(Throwable failure) {
        try { plugin.getLogger().log(Level.WARNING, "Experimental NPC failed", failure); }
        catch (RuntimeException ignored) { /* Reporting must never prevent another session's cleanup. */ }
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interact(PlayerInteractEntityEvent event) {
        NPC npc = entities.get(event.getRightClicked().getUniqueId());
        if (npc == null) return;
        boolean previouslyCancelled = event.isCancelled();
        event.setCancelled(true); // Blocks trading for everyone, including unrelated viewers of old-API NPCs.
        synchronized (npc) {
            if (npc.closed) { retire(npc); return; }
        }
        Player player = event.getPlayer();
        if (previouslyCancelled || event.getHand() != EquipmentSlot.HAND
                || !npc.session.player.equals(player.getUniqueId()) || !npc.selected.compareAndSet(false, true)) return;
        try {
            scheduler.player(player, () -> {
                try {
                    if (!validPlayer(npc, player)) { sessions.close(npc.session); return; }
                    npc.selection.run();
                } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
            }, () -> sessions.close(npc.session));
        } catch (RuntimeException | LinkageError failure) { fail(npc, failure); }
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interactAt(PlayerInteractAtEntityEvent event) { interact(event); }

    private boolean owned(Entity entity) {
        NPC npc = entities.get(entity.getUniqueId());
        if (npc == null) return false;
        synchronized (npc) {
            if (npc.closed) retire(npc);
        }
        return true;
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void damage(EntityDamageEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void teleport(EntityTeleportEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void target(EntityTargetEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interactBlock(EntityInteractEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void combust(EntityCombustEvent event) { if (owned(event.getEntity())) event.setCancelled(true); }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void vehicle(VehicleEnterEvent event) { if (owned(event.getEntered())) event.setCancelled(true); }

    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void unload(WorldUnloadEvent event) {
        for (NPC npc : resources.values()) if (npc.anchor.getWorld() == event.getWorld()) {
            sessions.close(npc.session);
            // An accepted world unload discards nonpersistent NPCs even if its region has stopped ticking.
            forget(npc);
        }
    }
}
