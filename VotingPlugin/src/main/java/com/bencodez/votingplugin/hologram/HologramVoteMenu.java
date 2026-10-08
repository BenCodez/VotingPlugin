package com.bencodez.votingplugin.hologram;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.LongSupplier;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.player.PlayerChangedWorldEvent;
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.event.server.PluginDisableEvent;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votesites.VoteSite;

import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.TextComponent;

/** Experimental, read-only menu. User storage is read solely on the existing persistence worker. */
public final class HologramVoteMenu implements Listener {
    static final int MAX_MENUS = 64;
    static final int MAX_SITES = 200;
    private final VotingPluginMain plugin;
    private final HologramMenuScheduler scheduler;
    private final LongSupplier clock;
    private final ConcurrentHashMap<UUID, Session> sessions = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<UUID, Button> buttons = new ConcurrentHashMap<>();
    private volatile boolean stopped;
    private final AtomicInteger pendingLoads = new AtomicInteger();

    enum Action { SITE, PREVIOUS, NEXT, CLOSE }
    record Button(Session session, long version, Action action, int siteIndex) { }

    static final class Session {
        final UUID owner;
        final Player player;
        final Location anchor;
        final HologramVoteSettings settings;
        final long expiresAt;
        final AtomicBoolean closed = new AtomicBoolean();
        final AtomicLong lastClick = new AtomicLong();
        // Entity list and page/version are accessed only in the stationary anchor region.
        final List<Entity> entities = new ArrayList<>();
        volatile List<HologramVoteModel.Site> sites = List.of();
        volatile Runnable cancelWatch = () -> { };
        int page;
        long version;
        Session(Player player, Location anchor, HologramVoteSettings settings) {
            this(player, anchor, settings, System.nanoTime());
        }
        Session(Player player, Location anchor, HologramVoteSettings settings, long now) {
            this.owner = player.getUniqueId();
            this.player = player;
            this.anchor = anchor.clone();
            this.settings = settings;
            this.expiresAt = now + settings.timeoutSeconds() * 1_000_000_000L;
        }
        boolean expired(long now) { return now - expiresAt >= 0; }
    }

    public HologramVoteMenu(VotingPluginMain plugin) {
        this(plugin, new HologramMenuScheduler(plugin));
    }
    HologramVoteMenu(VotingPluginMain plugin, HologramMenuScheduler scheduler) {
        this(plugin, scheduler, System::nanoTime);
    }
    HologramVoteMenu(VotingPluginMain plugin, HologramMenuScheduler scheduler, LongSupplier clock) {
        this.plugin = plugin;
        this.scheduler = scheduler;
        this.clock = clock;
    }

    public static boolean supported() {
        try {
            Class.forName("org.bukkit.entity.TextDisplay");
            Class.forName("org.bukkit.entity.Interaction");
            Entity.class.getMethod("setVisibleByDefault", boolean.class);
            return true;
        } catch (ReflectiveOperationException unavailable) {
            return false;
        }
    }

    public void open(Player player) {
        scheduler.player(player, () -> openOwned(player), () -> close(player.getUniqueId()));
    }

    private void openOwned(Player player) {
        if (stopped || !plugin.isEnabled() || !player.isOnline()) return;
        if (!player.hasPermission("VotingPlugin.Admin")
                && !player.hasPermission("VotingPlugin.Commands.AdminVote.TestHologram")) {
            player.sendMessage("\u00a7cYou do not have permission to use this experimental menu.");
            return;
        }
        HologramVoteSettings settings;
        try {
            settings = HologramVoteSettings.read(plugin.getConfigFile().getData());
        } catch (IllegalArgumentException invalid) {
            player.sendMessage("\u00a7cInvalid Experimental.HologramVoteGUI settings: " + invalid.getMessage());
            return;
        }
        if (!settings.enabled()) {
            player.sendMessage("\u00a7eEnable Experimental.HologramVoteGUI.Enabled to test this menu.");
            return;
        }
        if (!supported()) {
            player.sendMessage("\u00a7cThis experimental menu requires Minecraft 1.19.4+ entity/visibility APIs.");
            return;
        }
        close(player.getUniqueId());
        Location eyes = player.getEyeLocation();
        Location anchor = eyes.clone().add(eyes.getDirection().multiply(settings.distance()));
        anchor.setYaw(eyes.getYaw() + 180);
        anchor.setPitch(0);
        Session session = new Session(player, anchor, settings, clock.getAsLong());
        synchronized (sessions) {
            if (stopped) return;
            if (sessions.size() >= MAX_MENUS) {
                player.sendMessage("\u00a7eToo many experimental menus are open. Please retry shortly.");
                return;
            }
            sessions.put(session.owner, session);
        }
        List<VoteSite> visible = plugin.getVoteSiteManager().getVoteSitesEnabled().stream()
                .filter(site -> !site.isHidden() && (site.getPermissionToView().isEmpty()
                        || player.hasPermission(site.getPermissionToView())))
                .limit(MAX_SITES).toList();
        if (pendingLoads.incrementAndGet() > MAX_MENUS) {
            pendingLoads.decrementAndGet();
            close(session);
            player.sendMessage("\u00a7eVoting data is busy; please retry shortly.");
            return;
        }
        boolean submitted = false;
        try {
            session.cancelWatch = scheduler.watchPlayer(player, () -> watch(session), () -> close(session));
            if (session.closed.get()) session.cancelWatch.run();
            plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                try {
                    var user = plugin.getUser(session.owner);
                    List<HologramVoteModel.Site> sites = new ArrayList<>();
                    for (VoteSite site : visible) {
                        if (!current(session)) return;
                        sites.add(new HologramVoteModel.Site(site.getKey(), site.getDisplayName(),
                                site.getVoteURL(false), user.canVoteSite(site), user.voteNextDurationTime(site)));
                    }
                    if (!current(session)) return;
                    session.sites = List.copyOf(sites);
                    scheduler.region(session.anchor, () -> render(session));
                } catch (Exception failure) {
                    fail(session, "Voting-site data could not be loaded; check the server console.", failure);
                } finally {
                    pendingLoads.decrementAndGet();
                }
            });
            submitted = true;
        } catch (RuntimeException rejected) {
            if (!submitted) pendingLoads.decrementAndGet();
            fail(session, "The experimental menu could not be scheduled.", rejected);
        }
    }

    private boolean current(Session session) {
        return !stopped && !session.closed.get() && !session.expired(clock.getAsLong())
                && sessions.get(session.owner) == session;
    }

    private void watch(Session session) {
        if (!current(session) || !session.player.isOnline()
                || !session.anchor.getWorld().equals(session.player.getWorld())
                || session.anchor.distanceSquared(session.player.getEyeLocation()) > 64) close(session);
    }

    private void render(Session session) {
        if (!current(session)) { close(session); return; }
        removeEntities(session);
        long version = ++session.version;
        try {
            NativeHologramRenderer.render(session, scheduler, (entity, action, index) -> {
                session.entities.add(entity);
                if (action != null) buttons.put(entity.getUniqueId(), new Button(session, version, action, index));
            });
            if (!current(session)) { removeEntities(session); return; }
            List<Entity> entities = List.copyOf(session.entities);
            scheduler.player(session.player, () -> {
                if (!current(session)) return;
                // Showing an entity can touch its tracker. Verify the fixed menu area is also
                // owned by the player's current region before calling the player visibility API.
                for (int dx : new int[] { -3, 3 }) for (int dz : new int[] { -3, 3 }) {
                    if (!scheduler.owns(session.anchor.clone().add(dx, 0, dz))) { close(session); return; }
                }
                if (!session.player.getWorld().equals(session.anchor.getWorld())
                        || session.player.getEyeLocation().distanceSquared(session.anchor) > 64) { close(session); return; }
                entities.forEach(entity -> session.player.showEntity(plugin, entity));
            }, () -> close(session));
        } catch (RuntimeException failure) {
            removeEntities(session);
            fail(session, "The hologram could not be created here. Move to an open area and retry.", failure);
        }
    }

    private void fail(Session session, String message, Exception failure) {
        plugin.getLogger().warning("Experimental hologram failed: " + failure.getClass().getSimpleName());
        close(session);
        if (!stopped && plugin.isEnabled()) scheduler.player(session.player,
                () -> session.player.sendMessage("\u00a7c" + message), () -> { });
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interact(PlayerInteractEntityEvent event) {
        Button button = buttons.get(event.getRightClicked().getUniqueId());
        if (button == null) return;
        event.setCancelled(true);
        click(event.getPlayer(), button);
    }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void interactAt(PlayerInteractAtEntityEvent event) { interact(event); }

    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void damage(EntityDamageEvent event) {
        if (buttons.containsKey(event.getEntity().getUniqueId())) event.setCancelled(true);
    }

    private void click(Player player, Button button) {
        Session session = button.session();
        if (!session.owner.equals(player.getUniqueId()) || !current(session)) return;
        scheduler.player(player, () -> {
            if (!current(session) || !player.hasPermission("VotingPlugin.Admin")
                    && !player.hasPermission("VotingPlugin.Commands.AdminVote.TestHologram")) return;
            if (!player.getWorld().equals(session.anchor.getWorld())
                    || player.getEyeLocation().distanceSquared(session.anchor) > 64) { close(session); return; }
            long now = clock.getAsLong();
            long previous = session.lastClick.get();
            if (now - previous < 200_000_000L || !session.lastClick.compareAndSet(previous, now)) return;
            scheduler.region(session.anchor, () -> {
                if (!current(session) || session.version != button.version()) return;
                if (button.action() == Action.CLOSE) { close(session); return; }
                if (button.action() == Action.NEXT) session.page++;
                if (button.action() == Action.PREVIOUS) session.page--;
                if (button.action() == Action.SITE) {
                    HologramVoteModel.Site site = session.sites.get(button.siteIndex());
                    var url = HologramVoteModel.votingUrl(site.url());
                    scheduler.player(player, () -> {
                        if (!current(session)) return;
                        if (url.isEmpty()) {
                            player.sendMessage("\u00a7cThis voting site has no valid HTTP/HTTPS VoteURL.");
                            return;
                        }
                        TextComponent link = new TextComponent("\u00a7aOpen voting page: " + site.title());
                        link.setClickEvent(new ClickEvent(ClickEvent.Action.OPEN_URL, url.get()));
                        player.spigot().sendMessage(link);
                    }, () -> close(session));
                } else {
                    session.page = Math.max(0, Math.min(session.page,
                            HologramVoteModel.pages(session.sites.size(), session.settings.sitesPerPage()) - 1));
                    render(session);
                }
            });
        }, () -> close(session));
    }

    public void close(UUID owner) {
        Session session = sessions.get(owner);
        if (session != null) close(session);
    }

    private void close(Session session) {
        if (!session.closed.compareAndSet(false, true)) return;
        sessions.remove(session.owner, session);
        buttons.entrySet().removeIf(entry -> entry.getValue().session() == session);
        session.cancelWatch.run();
        try {
            scheduler.region(session.anchor, () -> removeEntities(session));
        } catch (RuntimeException rejected) {
            // During final Folia shutdown region ticks may already be stopped. These entities are
            // non-persistent; world teardown discards them rather than serializing orphan menus.
            plugin.getLogger().warning("Hologram cleanup could not be scheduled during shutdown.");
        }
    }

    private void removeEntities(Session session) {
        for (Entity entity : session.entities) {
            buttons.remove(entity.getUniqueId());
            entity.remove();
        }
        session.entities.clear();
    }

    public void clear() { List.copyOf(sessions.values()).forEach(this::close); }
    public void shutdown() { synchronized (sessions) { stopped = true; } clear(); }

    @EventHandler public void quit(PlayerQuitEvent event) { close(event.getPlayer().getUniqueId()); }
    @EventHandler public void changedWorld(PlayerChangedWorldEvent event) { close(event.getPlayer().getUniqueId()); }
    @EventHandler(priority = EventPriority.LOWEST)
    public void disabling(PluginDisableEvent event) { if (event.getPlugin() == plugin) shutdown(); }
}
