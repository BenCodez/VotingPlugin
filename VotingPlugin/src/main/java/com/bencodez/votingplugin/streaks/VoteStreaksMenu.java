package com.bencodez.votingplugin.streaks;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.BiFunction;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.PlayerDeathEvent;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.event.player.PlayerChangedWorldEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** One isolated native inventory view; no registrations in existing production GUI managers. */
public final class VoteStreaksMenu implements Listener, AutoCloseable {
    public static final String PERMISSION = "VotingPlugin.Commands.Vote.Streaks";
    private static final int MAX_SESSIONS = 128, MAX_LOADS = 16;
    private final VotingPluginMain plugin;
    private final Consumer<Runnable> worker;
    private final BiFunction<UUID, String, VoteStreaksSnapshot> snapshot;
    private final Map<UUID, Session> sessions = new ConcurrentHashMap<>();
    private final AtomicInteger loading = new AtomicInteger();
    private final AtomicInteger reloadDepth = new AtomicInteger();
    private volatile boolean closed;

    public VoteStreaksMenu(VotingPluginMain plugin) {
        this(plugin, task -> plugin.getUserManager().getDataManager().getTimer().execute(task), (uuid, name) ->
                plugin.getVoteStreakHandler().snapshotForDisplay(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, name)));
    }
    VoteStreaksMenu(VotingPluginMain plugin, Consumer<Runnable> worker, BiFunction<UUID, String, VoteStreaksSnapshot> snapshot) {
        this.plugin = plugin; this.worker = worker; this.snapshot = snapshot;
    }
    private static final class Session {
        final Player player;
        final UUID owner;
        final String name;
        final Inventory previous;
        final boolean ownsPrevious;
        final long created = System.nanoTime();
        volatile Holder view;
        Session(Player player, VoteStreaksMenu manager) {
            this.player = player; owner = player.getUniqueId(); name = player.getName();
            previous = player.getOpenInventory().getTopInventory();
            Holder holder = manager.holder(previous);
            ownsPrevious = holder != null && holder.session.owner.equals(owner);
        }
    }
    static final class Holder implements InventoryHolder {
        final VoteStreaksMenu manager;
        final Session session;
        final VoteStreaksSnapshot snapshot;
        final int page;
        Inventory inventory;
        Holder(VoteStreaksMenu manager, Session session, VoteStreaksSnapshot snapshot, int page) {
            this.manager = manager; this.session = session; this.snapshot = snapshot; this.page = snapshot.clampPage(page);
        }
        @Override public Inventory getInventory() { return inventory; }
    }
    private static boolean permitted(Player player) {
        return player.isOnline() && (player.hasPermission(PERMISSION) || player.hasPermission("VotingPlugin.Player"));
    }
    private void onPlayer(Player player, Runnable work, Runnable rejected) {
        BukkitCompletionScheduler.run(plugin, player, work, rejected, rejected);
    }
    public void open(Player player) {
        onPlayer(player, () -> begin(player), () -> { });
    }
    private void begin(Player player) {
        if (closed || reloadDepth.get() != 0 || !permitted(player)) return;
        if (loading.incrementAndGet() > MAX_LOADS) {
            loading.decrementAndGet(); player.sendMessage("Vote streaks are busy. Please try again shortly."); return;
        }
        Session session;
        synchronized (sessions) {
            if (closed || reloadDepth.get() != 0 || (sessions.size() >= MAX_SESSIONS && !sessions.containsKey(player.getUniqueId()))) {
                loading.decrementAndGet(); player.sendMessage("Vote streaks are unavailable. Please try again shortly."); return;
            }
            session = new Session(player, this); sessions.put(session.owner, session);
        }
        try {
            worker.accept(() -> {
                try {
                    if (!current(session)) return;
                    VoteStreaksSnapshot data = snapshot.apply(session.owner, session.name);
                    onPlayer(player, () -> {
                        if (!current(session)) return;
                        if (!permitted(player) || System.nanoTime() - session.created > 30_000_000_000L
                                || player.getOpenInventory().getTopInventory() != session.previous) {
                            retire(session); return;
                        }
                        renderSafely(session, data, 0);
                    }, () -> retire(session));
                } catch (RuntimeException failure) {
                    onPlayer(player, () -> {
                        if (retire(session) && player.isOnline())
                            player.sendMessage("Unable to read vote streaks. Please try again later.");
                    }, () -> retire(session));
                } finally { loading.decrementAndGet(); }
            });
        } catch (RuntimeException rejected) {
            loading.decrementAndGet(); retire(session);
            player.sendMessage("Vote streaks are unavailable. Please try again later.");
        }
    }
    private boolean current(Session session) { return !closed && sessions.get(session.owner) == session; }
    private Holder holder(Inventory inventory) {
        return inventory != null && inventory.getHolder() instanceof Holder holder && holder.manager == this ? holder : null;
    }
    private void renderSafely(Session session, VoteStreaksSnapshot data, int page) {
        try { render(session, data, page); }
        catch (RuntimeException failure) {
            if (sessions.remove(session.owner, session)) {
                closeView(session);
                if (session.player.isOnline()) session.player.sendMessage("Unable to open vote streaks. Please try again later.");
            }
        }
    }
    private void navigate(Holder holder, int page) {
        onPlayer(holder.session.player, () -> {
            if (current(holder.session) && holder.session.view == holder
                    && holder.session.player.getOpenInventory().getTopInventory() == holder.inventory)
                renderSafely(holder.session, holder.snapshot, page);
        }, () -> sessions.remove(holder.session.owner, holder.session));
    }
    private void render(Session session, VoteStreaksSnapshot data, int page) {
        if (!current(session) || !permitted(session.player)) return;
        Holder holder = new Holder(this, session, data, page);
        String title = plugin.getGui().getChestVoteStreaksName();
        holder.inventory = Bukkit.createInventory(holder, 54, MessageAPI.colorize(title));
        for (int i = 0; i < data.periods().size(); i++) {
            var period = data.periods().get(i); String unit = VoteStreaksSnapshot.unit(period.type());
            holder.inventory.setItem(11 + i * 2, item(Material.CLOCK, "&6" + period.type() + " streak",
                    List.of("&7Current: &e" + period.current() + " " + unit, "&7Best: &e" + period.best() + " " + unit)));
        }
        List<VoteStreaksSnapshot.Milestone> milestones = data.page(holder.page);
        for (int i = 0; i < milestones.size(); i++) {
            var milestone = milestones.get(i); String unit = VoteStreaksSnapshot.unit(milestone.type());
            var lore = new ArrayList<String>();
            lore.add("&7Current: &e" + milestone.current() + " " + unit);
            lore.add("&7Best: &e" + milestone.best() + " " + unit);
            lore.add("&7Milestone: &e" + milestone.threshold() + " " + unit);
            lore.add("&7Votes per period: &e" + milestone.votesRequired());
            lore.add(milestone.recurring() ? "&7Repeats every " + milestone.threshold() + " " + unit + "; next: " + milestone.nextThreshold()
                    : milestone.current() >= milestone.threshold() ? "&aCurrent streak meets this milestone" : "&7Upcoming milestone");
            lore.add("&7Progress only; this does not claim rewards.");
            holder.inventory.setItem(20 + i, item(Material.PAPER, "&e" + milestone.id(), lore));
        }
        if (milestones.isEmpty()) holder.inventory.setItem(22, item(Material.PAPER, "&7No enabled streak milestones",
                List.of("&7Configure VoteStreaks in SpecialRewards.yml.")));
        holder.inventory.setItem(45, item(Material.ARROW, "&ePrevious", List.of()));
        holder.inventory.setItem(49, item(Material.BARRIER, "&cClose", List.of()));
        holder.inventory.setItem(50, item(Material.CLOCK, "&eRefresh", List.of("&7Read current streaks again.")));
        holder.inventory.setItem(53, item(Material.ARROW, "&eNext", List.of("&7Page " + (holder.page + 1) + "/" + data.pages(),
                data.truncated() ? "&7Showing the first 512 enabled milestones." : "&7Read-only streak progression.")));
        session.view = holder;
        if (!current(session) || !permitted(session.player)) return;
        session.player.openInventory(holder.inventory);
        if (!current(session) && session.player.getOpenInventory().getTopInventory() == holder.inventory)
            session.player.closeInventory();
        if (session.player.getOpenInventory().getTopInventory() != holder.inventory) sessions.remove(session.owner, session);
    }
    private static ItemStack item(Material material, String name, List<String> lore) {
        ItemStack item = new ItemStack(material); ItemMeta meta = item.getItemMeta();
        if (meta != null) {
            meta.setDisplayName(MessageAPI.colorize(name));
            meta.setLore(lore.stream().map(MessageAPI::colorize).toList()); item.setItemMeta(meta);
        }
        return item;
    }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void inventoryOpen(InventoryOpenEvent event) {
        if (!(event.getPlayer() instanceof Player player)) return;
        Holder holder = holder(event.getInventory());
        if (holder == null) return;
        if (holder.session.player != player || !current(holder.session) || holder.session.view != holder || !permitted(player)) {
            event.setCancelled(true); return;
        }
        if (event.isCancelled()) sessions.remove(holder.session.owner, holder.session);
    }
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void unrelatedInventoryOpen(InventoryOpenEvent event) {
        if (!event.isCancelled() && holder(event.getInventory()) == null && event.getPlayer() instanceof Player player)
            sessions.remove(player.getUniqueId());
    }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void click(InventoryClickEvent event) {
        Holder holder = holder(event.getView().getTopInventory());
        if (holder == null) return;
        boolean cancelled = event.isCancelled(); event.setCancelled(true);
        if (cancelled || event.getWhoClicked() != holder.session.player || !current(holder.session)
                || holder.session.view != holder || !permitted(holder.session.player)) return;
        // Only ordinary top-inventory clicks navigate; shift/number/drop actions cannot move previews.
        if (event.getRawSlot() < 0 || event.getRawSlot() >= 54 || !event.isLeftClick() || event.isShiftClick()) return;
        switch (event.getRawSlot()) {
        case 45 -> navigate(holder, holder.page - 1);
        case 53 -> navigate(holder, holder.page + 1);
        case 49 -> { sessions.remove(holder.session.owner, holder.session); closeView(holder.session); }
        case 50 -> begin(holder.session.player);
        default -> { }
        }
    }
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = false)
    public void drag(InventoryDragEvent event) {
        if (holder(event.getView().getTopInventory()) != null) event.setCancelled(true);
    }
    @EventHandler public void inventoryClose(InventoryCloseEvent event) {
        Holder holder = holder(event.getInventory());
        if (holder != null && holder.session.view == holder) sessions.remove(holder.session.owner, holder.session);
        else if (event.getPlayer() instanceof Player player) {
            Session pending = sessions.get(player.getUniqueId());
            if (pending != null && pending.view == null && pending.previous == event.getInventory()) sessions.remove(pending.owner, pending);
        }
    }
    @EventHandler public void quit(PlayerQuitEvent event) { sessions.remove(event.getPlayer().getUniqueId()); }
    @EventHandler public void worldChange(PlayerChangedWorldEvent event) { end(event.getPlayer()); }
    @EventHandler public void death(PlayerDeathEvent event) { end(event.getEntity()); }
    private void end(Player player) { Session session = sessions.remove(player.getUniqueId()); if (session != null) closeView(session); }
    private boolean retire(Session session) {
        if (!sessions.remove(session.owner, session)) return false;
        closeView(session); return true;
    }
    private void closeView(Session session) {
        if (session.view == null && !session.ownsPrevious) return;
        Runnable cleanup = () -> {
            // A queued cleanup must never close an inventory adopted by a newer request.
            Session replacement = sessions.get(session.owner);
            if (replacement != null && replacement != session) return;
            Inventory top = session.player.getOpenInventory().getTopInventory();
            if ((session.view != null && top == session.view.inventory)
                    || (session.ownsPrevious && top == session.previous)) session.player.closeInventory();
        };
        // Bukkit rejects scheduling after disable. Its primary thread already owns this interaction.
        if (closed && ownsForShutdown(Bukkit.class, session.player)) cleanup.run();
        else onPlayer(session.player, cleanup, () -> { });
    }
    /** Public Folia ownership also recognizes its stopped-region shutdown owner. */
    static boolean ownsForShutdown(Class<?> platform, Player player) {
        try {
            return Boolean.TRUE.equals(platform.getMethod("isOwnedByCurrentRegion", org.bukkit.entity.Entity.class).invoke(null, player));
        } catch (NoSuchMethodException legacy) {
            try {
                Class.forName("io.papermc.paper.threadedregions.RegionizedServer", false, VoteStreaksMenu.class.getClassLoader());
                return false; // A regional server without its ownership API must fail closed.
            } catch (ClassNotFoundException ordinaryServer) { return Bukkit.isPrimaryThread(); }
            catch (LinkageError unavailable) { return false; }
        } catch (ReflectiveOperationException | LinkageError unavailable) { return false; }
    }
    public void suspend() { reloadDepth.incrementAndGet(); reset(); }
    public void resume() { reloadDepth.updateAndGet(value -> Math.max(0, value - 1)); }
    public void reset() {
        List<Session> old;
        synchronized (sessions) { old = List.copyOf(sessions.values()); sessions.clear(); }
        old.forEach(this::closeView);
    }
    @Override public void close() { synchronized (sessions) { closed = true; } reset(); }
    int sessionCount() { return sessions.size(); }
}
