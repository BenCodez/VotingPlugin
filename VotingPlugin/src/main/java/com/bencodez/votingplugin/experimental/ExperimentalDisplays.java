package com.bencodez.votingplugin.experimental;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.world.WorldUnloadEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.bukkit.util.Vector;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;

/** Bounded, reflectively loaded display/interaction renderer for experimental menus. */
public final class ExperimentalDisplays implements Listener {
    private static final int MAX_TARGETS = 10;
    private static final int MAX_ENTITIES = 32;
    private static final int MAX_FRAMES = 64;
    public enum Action { SITE, PREVIOUS, NEXT, CLOSE, INFO, PREVIOUS_REWARD, NEXT_REWARD, SITE_PAGE, TERMINAL_STYLE }

    public record Target(String id, String text, ItemStack icon, double x, double y, Action action) {
        public Target {
            if (id == null || id.isBlank() || id.length() > 128) throw new IllegalArgumentException("Invalid display target id");
            if (text == null || text.length() > 256) throw new IllegalArgumentException("Invalid display target text");
            if (!Double.isFinite(x) || !Double.isFinite(y) || Math.abs(x) > 8 || Math.abs(y) > 8)
                throw new IllegalArgumentException("Invalid display target position");
            Objects.requireNonNull(action, "action");
            if (icon != null) icon = icon.clone();
        }
    }

    private static final class Frame {
        final ExperimentalSessions.Session session; final Player player; final Location anchor;
        final Consumer<Target> selection; final long generation; final Signature signature;
        final List<Location> footprint;
        final List<Entity> owned = new ArrayList<>(); final Map<UUID, Target> hitTargets = new HashMap<>();
        final AtomicBoolean closed = new AtomicBoolean(); boolean cleanupQueued;
        Frame(ExperimentalSessions.Session session, Player player, Location anchor, Consumer<Target> selection,
                long generation, Signature signature, List<Location> footprint) { this.session=session; this.player=player; this.anchor=anchor.clone();
            this.selection=selection; this.generation=generation; this.signature=signature; this.footprint=footprint; }
    }

    private final VotingPluginMain plugin; private final HologramMenuScheduler scheduler;
    private final ExperimentalSessions sessions; private final Map<UUID, Frame> currentFrames = new ConcurrentHashMap<>();
    private final Map<UUID, Frame> pendingFrames = new ConcurrentHashMap<>();
    private final Map<UUID, Frame> entities = new ConcurrentHashMap<>(); private final AtomicLong generations = new AtomicLong();
    private final Map<UUID, Boolean> registeredSessions = new ConcurrentHashMap<>();

    public ExperimentalDisplays(VotingPluginMain plugin, HologramMenuScheduler scheduler, ExperimentalSessions sessions) {
        this.plugin=Objects.requireNonNull(plugin); this.scheduler=Objects.requireNonNull(scheduler); this.sessions=Objects.requireNonNull(sessions);
    }

    public static boolean supported() {
        try { Class<?> text=Class.forName("org.bukkit.entity.TextDisplay"); Class<?> interaction=Class.forName("org.bukkit.entity.Interaction");
            Entity.class.getMethod("setPersistent", boolean.class); World.class.getMethod("spawn", Location.class, Class.class, Consumer.class);
            text.getMethod("setText", String.class); interaction.getMethod("setInteractionWidth", float.class);
            interaction.getMethod("setInteractionHeight", float.class);
            interaction.getMethod("setResponsive", boolean.class);
            Class<?> matrix = Class.forName("org.joml.Matrix4f");
            text.getMethod("setTransformationMatrix", matrix);
            Class.forName("org.bukkit.entity.ItemDisplay").getMethod("setItemStack", ItemStack.class);
            return true;
        } catch (ReflectiveOperationException | LinkageError unavailable) { return false; }
    }
    public int entityCount() { return entities.size(); }
    public void retryCleanup() { for (Frame frame: pendingFrames.values()) if (frame.closed.get()) retire(frame); }

    public void render(ExperimentalSessions.Session session, Player player, Location anchor, List<Target> requested,
            Consumer<Target> selection) {
        Objects.requireNonNull(session); Objects.requireNonNull(player); Objects.requireNonNull(anchor); Objects.requireNonNull(selection);
        List<Target> targets=requested == null ? List.of() : List.copyOf(requested);
        if (!supported() || !sessions.current(session) || targets.size()>MAX_TARGETS
                || targets.stream().mapToInt(target -> target.icon() == null ? 2 : 3).sum() > MAX_ENTITIES || !validAnchor(anchor)) { sessions.close(session); return; }
        Signature signature=signature(targets,anchor); Frame existing=currentFrames.get(session.id);
        synchronized (currentFrames) {
            existing=currentFrames.get(session.id);
            if (existing != null && !existing.closed.get() && existing.signature.equals(signature)) return;
            if (existing != null) { currentFrames.remove(session.id, existing); closeFrame(existing); }
            if (currentFrames.size() + pendingFrames.size() >= MAX_FRAMES) { sessions.close(session); return; }
            Frame frame=new Frame(session,player,anchor,selection,generations.incrementAndGet(),signature,footprint(anchor,targets)); currentFrames.put(session.id,frame);
            try { if (registeredSessions.putIfAbsent(session.id, Boolean.TRUE) == null)
                    session.own(() -> closeSession(session), ignored -> { }); schedulePlayerValidation(frame,targets); }
            catch (RuntimeException failure) { registeredSessions.remove(session.id); currentFrames.remove(session.id,frame); sessions.close(session); }
        }
    }

    private void schedulePlayerValidation(Frame frame,List<Target> targets) {
        try { scheduler.player(frame.player, () -> { if (!validPlayer(frame)) { closeSession(frame.session); return; }
                scheduler.region(frame.anchor, () -> spawn(frame,targets)); }, () -> closeSession(frame.session)); }
        catch (RuntimeException | LinkageError failure) { closeSession(frame.session); }
    }
    private boolean validPlayer(Frame f) { return !f.closed.get() && plugin.isEnabled() && sessions.current(f.session)
        && currentFrames.get(f.session.id)==f && f.player.isOnline() && f.player.getUniqueId().equals(f.session.player)
        && (f.player.hasPermission("VotingPlugin.Admin") || f.player.hasPermission("VotingPlugin.Commands.AdminVote."+f.session.type.command()))
        && f.player.getWorld().equals(f.anchor.getWorld()) && f.player.getEyeLocation().distanceSquared(f.anchor)<=64; }
    private boolean current(Frame f) { return !f.closed.get() && plugin.isEnabled() && sessions.current(f.session)
        && currentFrames.get(f.session.id)==f; }

    private void spawn(Frame frame,List<Target> targets) {
        try { if (!current(frame) || !preflight(frame.footprint)) { closeSession(frame.session); return; }
            for (Target target:targets) { if (!current(frame)) throw new IllegalStateException("Display frame retired"); Location point=offset(frame.anchor,target.x(),target.y());
                spawnOwned(frame,point.clone().add(0, target.icon() == null ? 0 : .25, 0),"org.bukkit.entity.TextDisplay",e -> configureText(e,target.text()));
                Entity hit=spawnOwned(frame,point.clone().subtract(0,.08,0),"org.bukkit.entity.Interaction",this::configureHit);
                synchronized(frame) { frame.hitTargets.put(hit.getUniqueId(),target); }
                if (target.icon()!=null && classExists("org.bukkit.entity.ItemDisplay")) spawnOwned(frame,point.clone().subtract(0,.14,0),"org.bukkit.entity.ItemDisplay",e -> configureItem(e,target.icon()));
            }
            scheduler.player(frame.player, () -> { if (validPlayer(frame)) frame.session.activate(); else closeSession(frame.session); }, () -> closeSession(frame.session));
        } catch (RuntimeException | LinkageError | ReflectiveOperationException failure) { closeSession(frame.session); }
    }
    private boolean ownsFootprint(List<Location> footprint) { for (Location location : footprint) if (!scheduler.owns(location)) return false; return true; }
    private boolean preflight(List<Location> footprint) { for (Location location : footprint)
        if (!scheduler.owns(location) || !loaded(location)) return false; return true; }
    private static List<Location> footprint(Location anchor,List<Target> targets) { List<Location> result=new ArrayList<>(); result.add(anchor.clone());
        for(Target t:targets) { Location p=offset(anchor,t.x(),t.y()); result.add(p); for(double dx:new double[]{-.7,.7}) for(double dz:new double[]{-.7,.7}) result.add(p.clone().add(dx,0,dz)); } return List.copyOf(result); }
    private Entity spawnOnce(Location location,String className,Consumer<Entity> configure) throws ReflectiveOperationException {
        Class<?> type=Class.forName(className); Method spawn=World.class.getMethod("spawn",Location.class,Class.class,Consumer.class);
        try { return (Entity)spawn.invoke(location.getWorld(),location,type,(Consumer<Entity>)configure); }
        catch(InvocationTargetException failure) { Throwable cause=failure.getCause(); if(cause instanceof RuntimeException r) throw r; if(cause instanceof Error e) throw e; throw failure; }
    }
    private Entity spawnOwned(Frame frame,Location location,String className,Consumer<Entity> configure) throws ReflectiveOperationException {
        return spawnOnce(location,className,entity -> { synchronized(frame) { frame.owned.add(entity); } entities.put(entity.getUniqueId(),frame);
            invokeRequired(entity,"setPersistent",new Class<?>[]{boolean.class},false);
            if (!current(frame)) { closeFrame(frame); throw new IllegalStateException("Display frame retired during spawn"); } invokeRequired(entity,"setGravity",new Class<?>[]{boolean.class},false); invokeRequired(entity,"setInvulnerable",new Class<?>[]{boolean.class},true); configure.accept(entity); scheduleShow(frame,entity); });
    }
    private void scheduleShow(Frame frame, Entity entity) {
        try {
            Method visible = Entity.class.getMethod("setVisibleByDefault", boolean.class);
            Method show = Player.class.getMethod("showEntity", Plugin.class, Entity.class);
            visible.invoke(entity, false);
            scheduler.player(frame.player, () -> {
                if (!validPlayer(frame) || !ownsFootprint(frame.footprint)) {
                    closeSession(frame.session);
                    return;
                }
                try { show.invoke(frame.player, plugin, entity); }
                catch (ReflectiveOperationException failure) { closeSession(frame.session); }
            }, () -> closeSession(frame.session));
        } catch (NoSuchMethodException olderVisibilityAPI) {
            // Public visibility on old implementations; interactions remain owner-only.
        } catch (ReflectiveOperationException | LinkageError failure) {
            closeSession(frame.session);
            throw new IllegalStateException("Display visibility initialization failed", failure);
        }
    }
    private void configureText(Entity e,String text) { invokeRequired(e,"setText",new Class<?>[]{String.class},text); scale(e, .5f); try { Class<?> b=Class.forName("org.bukkit.entity.Display$Billboard"); invokeRequired(e,"setBillboard",new Class<?>[]{b},Enum.valueOf((Class)b,"FIXED")); } catch(ReflectiveOperationException|IllegalArgumentException x) { throw new IllegalStateException("Text billboard unavailable",x); } }
    private void configureHit(Entity e) { invokeRequired(e,"setInteractionWidth",new Class<?>[]{float.class},1.15f); invokeRequired(e,"setInteractionHeight",new Class<?>[]{float.class},.38f); invokeRequired(e,"setResponsive",new Class<?>[]{boolean.class},false); }
    private void configureItem(Entity e,ItemStack i) { invokeRequired(e,"setItemStack",new Class<?>[]{ItemStack.class},i.clone()); scale(e, .3f); }
    private static void scale(Entity entity, float amount) {
        try {
            Class<?> matrixType = Class.forName("org.joml.Matrix4f");
            Object matrix = matrixType.getConstructor().newInstance();
            matrixType.getMethod("scale", float.class).invoke(matrix, amount);
            invokeRequired(entity, "setTransformationMatrix", new Class<?>[]{matrixType}, matrix);
        } catch (ReflectiveOperationException | LinkageError unavailable) {
            throw new IllegalStateException("Display transformation unavailable", unavailable);
        }
    }
    private static void invokeRequired(Object t,String n,Class<?>[] types,Object v) { try { t.getClass().getMethod(n,types).invoke(t,v); } catch(ReflectiveOperationException|LinkageError x) { throw new IllegalStateException(n+" unavailable",x); } }
    private static boolean classExists(String n) { try { Class.forName(n); return true; } catch(ReflectiveOperationException|LinkageError x) { return false; } }
    private static Location offset(Location a,double side,double up) { double yaw=Math.toRadians(a.getYaw()); return a.clone().add(new Vector(Math.cos(yaw)*side,up,Math.sin(yaw)*side)); }
    private static boolean validAnchor(Location l) { return l.getWorld()!=null&&Double.isFinite(l.getX())&&Double.isFinite(l.getY())&&Double.isFinite(l.getZ())&&Float.isFinite(l.getYaw())&&Float.isFinite(l.getPitch()); }
    private static boolean loaded(Location l) { World w=l.getWorld(); return Bukkit.getWorld(w.getUID())==w&&w.isChunkLoaded(l.getBlockX()>>4,l.getBlockZ()>>4); }
    private record Signature(UUID world, double x, double y, double z, float yaw, float pitch, List<Target> targets) {}
    private static Signature signature(List<Target> targets, Location anchor) {
        return new Signature(anchor.getWorld().getUID(), anchor.getX(), anchor.getY(), anchor.getZ(),
                anchor.getYaw(), anchor.getPitch(), List.copyOf(targets));
    }

    /** One owner-region update per second; only the showcase preview model rotates. */
    public void rotateShowcase(ExperimentalSessions.Session session, long step) {
        Frame frame = currentFrames.get(session.id);
        if (frame == null || session.type != ExperimentalGUIType.REWARD_SHOWCASE || !current(frame)) return;
        try {
            scheduler.region(frame.anchor, () -> {
                if (!current(frame)) return;
                if (!preflight(frame.footprint)) { closeSession(session); return; }
                try {
                    Class<?> type = Class.forName("org.bukkit.entity.ItemDisplay");
                    synchronized (frame) {
                        // First item display is the reward preview; site icons stay stationary.
                        for (Entity entity : frame.owned) if (type.isInstance(entity)) {
                            entity.setRotation(frame.anchor.getYaw() + (float) ((step % 24) * 15), 0);
                            break;
                        }
                    }
                } catch (ReflectiveOperationException | RuntimeException | LinkageError failure) {
                    closeSession(session);
                }
            });
        } catch (RuntimeException | LinkageError failure) { closeSession(session); }
    }

    public void close(ExperimentalSessions.Session session) { closeSession(session); }
    private void closeSession(ExperimentalSessions.Session session) { sessions.close(session); registeredSessions.remove(session.id); Frame f=currentFrames.remove(session.id); if(f!=null)closeFrame(f); for(Frame p:List.copyOf(pendingFrames.values()))if(p.session==session)closeFrame(p); }
    private void closeFrame(Frame f) { f.closed.set(true); pendingFrames.put(frameKey(f),f); retire(f); }
    private UUID frameKey(Frame f) { return new UUID(f.session.id.getMostSignificantBits() ^ f.generation,f.session.id.getLeastSignificantBits() ^ Long.rotateLeft(f.generation,17)); }
    private void retire(Frame f) { synchronized(f) { if(f.owned.isEmpty()){pendingFrames.remove(frameKey(f),f);return;} if(f.cleanupQueued)return; f.cleanupQueued=true; }
        try { if(scheduler.owns(f.anchor))remove(f); else scheduler.region(f.anchor,()->remove(f)); } catch(RuntimeException|LinkageError x) { synchronized(f){f.cleanupQueued=false;} } }
    private void remove(Frame f) { try { if(!preflight(f.footprint))throw new IllegalStateException("Display cleanup outside owned footprint");
        synchronized(f){ for(Entity e:List.copyOf(f.owned)){ if(e.isValid())e.remove(); entities.remove(e.getUniqueId(),f); }
            f.owned.clear();f.hitTargets.clear();f.cleanupQueued=false;}pendingFrames.remove(frameKey(f),f); } catch(RuntimeException|LinkageError x){synchronized(f){f.cleanupQueued=false;}} }

    @EventHandler(priority=EventPriority.HIGHEST,ignoreCancelled=false) public void interact(PlayerInteractEntityEvent event) { Frame f=entities.get(event.getRightClicked().getUniqueId()); if(f==null)return; boolean was=event.isCancelled(); event.setCancelled(true);
        if(was||event.getHand()!=EquipmentSlot.HAND||!f.session.player.equals(event.getPlayer().getUniqueId())||!current(f))return; Target target; synchronized(f){target=f.hitTargets.get(event.getRightClicked().getUniqueId());} if(target==null)return; Target selected=target;
        scheduler.player(event.getPlayer(),()->{if(validPlayer(f))f.selection.accept(selected);},()->closeSession(f.session)); }
    @EventHandler(priority=EventPriority.MONITOR,ignoreCancelled=true) public void unload(WorldUnloadEvent event) { for(Frame f:currentFrames.values())if(f.anchor.getWorld()==event.getWorld())closeSession(f.session); for(Frame f:pendingFrames.values())if(f.anchor.getWorld()==event.getWorld()){synchronized(f){entities.entrySet().removeIf(entry -> entry.getValue() == f); f.owned.clear();f.hitTargets.clear();}pendingFrames.remove(frameKey(f),f);} }
}
