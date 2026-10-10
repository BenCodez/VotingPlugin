package com.bencodez.votingplugin.experimental;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BooleanSupplier;
import java.util.logging.Level;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.PlayerDeathEvent;
import org.bukkit.event.player.PlayerChangedWorldEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.event.player.PlayerTeleportEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;
import com.bencodez.votingplugin.hologram.HologramVoteMenu;
import com.bencodez.votingplugin.hologram.HologramVoteModel;
import com.bencodez.votingplugin.hologram.HologramVoteSettings;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition;
import com.bencodez.advancedcore.api.item.ItemBuilder;

import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.TextComponent;

/** Opt-in presentation only: no reward actions, votes, totals or production GUI registrations. */
public final class ExperimentalGUIManager implements Listener {
    private final VotingPluginMain plugin;
    private final HologramMenuScheduler scheduler;
    private final ExperimentalSessions sessions;
    private final Map<UUID, Menu> menus = new ConcurrentHashMap<>();
    private final AtomicInteger loading = new AtomicInteger();
    private final ExperimentalInventoryListener inventoryListener;
    private final ExperimentalNPC npc;
    private final ExperimentalDialogs dialogs;
    private final ExperimentalDisplays displays;

    // Mutable view state below is accessed only by the player's scheduler.
    private static final class Menu {
        final ExperimentalSessions.Session session;
        final Player player;
        final Location anchor;
        final ExperimentalGUISettings settings;
        final Map<String, ItemStack> icons;
        Inventory inventory;
        List<HologramVoteModel.Site> sites = List.of();
        int page;
        int dailyStreak;
        int weeklyStreak;
        int monthlyStreak;
        int total;
        boolean pulse;
        long ticks;
        final AtomicBoolean loading = new AtomicBoolean();
        volatile List<HologramVoteModel.Site> displayed = List.of();
        List<VoteStreakDefinition> definitions = List.of();
        List<Milestone> milestones = List.of();
        final Map<String, ItemStack> rewardIcons = new HashMap<>();
        final Map<String, List<String>> rewardDescriptions = new HashMap<>();
        int rewardPage;
        Menu(ExperimentalSessions.Session session, Player player, Location anchor,
                ExperimentalGUISettings settings, Map<String, ItemStack> icons) {
            this.session = session;
            this.player = player;
            this.anchor = anchor;
            this.settings = settings;
            this.icons = icons;
        }
    }

    private record Milestone(String id, String type, int threshold, int progress, int best, boolean recorded, boolean recurring) {}
    private record Snapshot(List<HologramVoteModel.Site> sites, int daily, int weekly, int monthly, int total,
            List<Milestone> milestones) {}

    public ExperimentalGUIManager(VotingPluginMain plugin) {
        this(plugin, new HologramMenuScheduler(plugin));
    }

    ExperimentalGUIManager(VotingPluginMain plugin, HologramMenuScheduler scheduler) {
        this.plugin = plugin;
        this.scheduler = scheduler;
        this.sessions = new ExperimentalSessions(System::nanoTime, failure ->
                plugin.getLogger().log(Level.WARNING, "Experimental GUI cleanup failed", failure));
        this.inventoryListener = new ExperimentalInventoryListener(sessions, this::click);
        this.npc = new ExperimentalNPC(plugin, scheduler, sessions);
        this.dialogs = new ExperimentalDialogs(plugin, scheduler, sessions);
        this.displays = new ExperimentalDisplays(plugin, scheduler, sessions);
    }

    public Listener inventoryListener() { return inventoryListener; }
    public Listener npcListener() { return npc; }
    public Listener displaysListener() { return displays; }

    public void open(Player player, ExperimentalGUIType type) {
        scheduler.player(player, () -> openOwned(player, type), () -> sessions.close(player.getUniqueId()));
    }

    private void openOwned(Player player, ExperimentalGUIType type) {
        if (!plugin.isEnabled() || !player.isOnline()) return;
        if (!permitted(player, type)) {
            player.sendMessage("\u00a7cYou do not have permission to test this GUI.");
            return;
        }
        ExperimentalGUISettings settings;
        HologramVoteSettings hologram;
        try {
            var config = plugin.getConfigFile().getData();
            settings = ExperimentalGUISettings.read(config);
            hologram = type == ExperimentalGUIType.HOLOGRAM ? settings.hologram(config) : null;
        }
        catch (RuntimeException invalid) {
            player.sendMessage("\u00a7cInvalid experimental GUI settings: " + invalid.getMessage());
            return;
        }
        if (!settings.enabled().contains(type)) {
            player.sendMessage("\u00a7eThis experimental style is disabled. Enable Experimental.VoteGUIs and its style setting.");
            return;
        }
        String unsupported = unsupported(type);
        if (unsupported != null) {
            player.sendMessage("\u00a7c" + unsupported);
            return;
        }
        var session = sessions.open(player.getUniqueId(), type, settings.maxSessions(),
                type == ExperimentalGUIType.HOLOGRAM ? hologram.timeoutSeconds() : settings.timeoutSeconds());
        if (session == null) {
            player.sendMessage("\u00a7eExperimental menus are at capacity or shutting down.");
            return;
        }
        if (scheduler.regionized()) {
            try {
                // Folia's async teleport path currently does not publish PlayerTeleportEvent.
                // Keep private test menus stationary: translating the player closes the session.
                Location openingPosition = player.getLocation().clone();
                session.own(scheduler.watchPlayer(player, () -> {
                    if (!sessions.current(session)) return;
                    Location position = player.getLocation();
                    if (!player.isOnline() || position.getWorld() != openingPosition.getWorld()
                            || position.distanceSquared(openingPosition) > 0.000001) {
                        sessions.close(session);
                    }
                }, () -> sessions.close(session)), this::cleanupError);
            } catch (RuntimeException failure) { fail(session, player, failure); return; }
        }
        if (type == ExperimentalGUIType.HOLOGRAM) {
            try {
                BooleanSupplier guard = () -> sessions.current(session);
                session.own(() -> plugin.getHologramVoteMenu().close(session.player, guard), this::cleanupError);
                plugin.getHologramVoteMenu().open(player, hologram,
                        guard, () -> sessions.close(session));
                session.activate();
            } catch (RuntimeException failure) { fail(session, player, failure); }
            return;
        }
        try {
            Location eyes = player.getEyeLocation();
            Location anchor = eyes.clone().add(eyes.getDirection().multiply(settings.distance()));
            anchor.setYaw(eyes.getYaw() + 180);
            anchor.setPitch(0);
            List<VoteSite> visible = visible(player);
            Map<String, ItemStack> icons = new HashMap<>();
            for (VoteSite site : visible) {
                try { icons.put(site.getKey(), site.getItem().toItemStack().clone()); }
                catch (RuntimeException invalidItem) { icons.put(site.getKey(), new ItemStack(Material.PAPER)); }
            }
            Menu menu = new Menu(session, player, anchor, settings, icons);
            menus.put(session.id, menu);
            session.own(() -> menus.remove(session.id, menu), this::cleanupError);
            session.own(() -> scheduler.player(player, () -> {
                Inventory top = player.getOpenInventory().getTopInventory();
                if (menu.inventory != null && top == menu.inventory) player.closeInventory();
            }, () -> {}), this::cleanupError);
            session.own(scheduler.watchPlayer(player, () -> watch(menu), () -> sessions.close(session)), this::cleanupError);
            captureRewards(menu);
            load(menu, visible);
        } catch (RuntimeException failure) { fail(session, player, failure); }
    }

    private List<VoteSite> visible(Player player) {
        return plugin.getVoteSiteManager().getVoteSitesEnabled().stream()
                .filter(site -> !site.isHidden() && (site.getPermissionToView().isEmpty()
                        || player.hasPermission(site.getPermissionToView())))
                .limit(200).toList();
    }

    private void captureRewards(Menu menu) {
        menu.definitions = plugin.getVoteStreakHandler().getDefinitions().stream()
                .filter(VoteStreakDefinition::isEnabled)
                .sorted(java.util.Comparator.comparing((VoteStreakDefinition definition) -> definition.getType().name())
                        .thenComparingInt(VoteStreakDefinition::getRequiredAmount).thenComparing(VoteStreakDefinition::getId))
                .limit(100).toList();
        var config = plugin.getSpecialRewardsConfig().getData();
        for (var definition : menu.definitions) {
            ItemStack preview = new ItemStack(Material.CHEST);
            List<String> description = new ArrayList<>();
            var items = config.getConfigurationSection(definition.getRewardPath() + ".Items");
            if (items != null) {
                for (String key : items.getKeys(false).stream().limit(3).toList()) {
                    var configured = items.getConfigurationSection(key);
                    if (configured == null || !configured.getBoolean("Enabled", true)) continue;
                    description.add("\u00a77Configured item: " + key);
                    if (preview.getType() == Material.CHEST) {
                        try {
                            ItemStack parsed = new ItemBuilder(configured).toItemStack();
                            if (parsed != null && !parsed.getType().isAir()) preview = parsed;
                        }
                        catch (RuntimeException unsupportedItem) { /* Safe chest preview for unsupported material. */ }
                    }
                }
            }
            Object message = config.get(definition.getRewardPath() + ".Messages.Player");
            if (message instanceof String text) description.add("\u00a77Message: " + text.substring(0, Math.min(120, text.length())));
            description.add("\u00a77Configured reward preview; conditions/chance still apply.");
            menu.rewardIcons.put(definition.getId(), preview.clone());
            menu.rewardDescriptions.put(definition.getId(), List.copyOf(description));
        }
    }

    private void load(Menu menu, List<VoteSite> visible) {
        if (!sessions.current(menu.session) || !menu.loading.compareAndSet(false, true)) return;
        if (loading.incrementAndGet() > 64) {
            loading.decrementAndGet();
            menu.loading.set(false);
            fail(menu.session, menu.player, new IllegalStateException("Voting snapshot worker capacity reached"));
            return;
        }
        try {
            plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                try {
                    if (!sessions.current(menu.session)) return;
                    var user = plugin.getUser(menu.session.player);
                    List<HologramVoteModel.Site> sites = new ArrayList<>();
                    for (VoteSite site : visible) {
                        if (!sessions.current(menu.session)) return;
                        sites.add(new HologramVoteModel.Site(site.getKey(), site.getDisplayName(),
                                site.getVoteURL(false), user.canVoteSite(site), user.voteNextDurationTime(site)));
                    }
                    List<Milestone> milestones = new ArrayList<>();
                    for (var definition : menu.definitions) {
                        var progress = plugin.getVoteStreakHandler().getStoredPreview(user, definition);
                        milestones.add(new Milestone(definition.getId(), definition.getType().name(),
                                definition.getRequiredAmount(), progress.amount(), progress.bestAmount(),
                                progress.awardRecorded(), definition.isRecurring()));
                    }
                    Snapshot snapshot = new Snapshot(List.copyOf(sites), user.getDayVoteStreak(),
                            user.getWeekVoteStreak(), user.getMonthVoteStreak(), user.getTotal(TopVoter.AllTime),
                            List.copyOf(milestones));
                    scheduler.player(menu.player, () -> {
                        try {
                            if (!sessions.current(menu.session) || !permitted(menu.player, menu.session.type)) return;
                            menu.sites = snapshot.sites();
                            menu.dailyStreak = snapshot.daily();
                            menu.weeklyStreak = snapshot.weekly();
                            menu.monthlyStreak = snapshot.monthly();
                            menu.total = snapshot.total();
                            menu.milestones = snapshot.milestones();
                            render(menu);
                        } catch (RuntimeException failure) { fail(menu.session, menu.player, failure); }
                        finally { menu.loading.set(false); }
                    }, () -> { menu.loading.set(false); sessions.close(menu.session); });
                } catch (RuntimeException failure) { menu.loading.set(false); fail(menu.session, menu.player, failure); }
                finally { loading.decrementAndGet(); }
            });
        } catch (RuntimeException rejected) {
            loading.decrementAndGet();
            menu.loading.set(false);
            fail(menu.session, menu.player, rejected);
        }
    }

    private void watch(Menu menu) {
        try { watchOwned(menu); }
        catch (RuntimeException failure) { fail(menu.session, menu.player, failure); }
    }

    private void watchOwned(Menu menu) {
        if (!sessions.current(menu.session) || !menu.player.isOnline()
                || !permitted(menu.player, menu.session.type)
                || !menu.player.getWorld().equals(menu.anchor.getWorld())
                || menu.player.getEyeLocation().distanceSquared(menu.anchor) > 64) {
            sessions.close(menu.session);
            return;
        }
        if (menu.inventory != null && menu.player.getOpenInventory().getTopInventory() != menu.inventory) {
            sessions.close(menu.session);
            return;
        }
        // One bounded update per second, never a per-tick animation or storage query.
        menu.pulse = !menu.pulse;
        menu.ticks++;
        if (menu.inventory != null) renderInventory(menu);
        if (menu.session.type == ExperimentalGUIType.REWARD_SHOWCASE) displays.rotateShowcase(menu.session, menu.ticks);
    }

    private void renderInventory(Menu menu) {
        if (!sessions.current(menu.session)) return;
        if (menu.inventory == null) {
            ExperimentalInventory holder = new ExperimentalInventory(menu.session);
            menu.inventory = Bukkit.createInventory(holder, 54,
                    menu.session.type == ExperimentalGUIType.STREAK_TRACK ? "Experimental Streak Track" : "Experimental Voting");
            holder.bind(menu.inventory);
            menu.player.openInventory(menu.inventory);
            menu.session.activate();
        }
        Inventory inventory = menu.inventory;
        inventory.clear();
        for (int slot = 0; slot < 54; slot++) {
            if (slot < 9 || slot >= 45 || slot % 9 == 0 || slot % 9 == 8)
                inventory.setItem(slot, item(menu.pulse ? Material.BLUE_STAINED_GLASS_PANE : Material.GRAY_STAINED_GLASS_PANE,
                        " ", List.of()));
        }
        inventory.setItem(4, item(Material.NETHER_STAR, "\u00a76Voting rewards", List.of(
                "\u00a77Total votes: " + menu.total, "\u00a77Daily streak: " + menu.dailyStreak,
                "\u00a77Weekly streak: " + menu.weeklyStreak, "\u00a77Monthly streak: " + menu.monthlyStreak,
                "\u00a77Preview only; links never grant votes or rewards.")));
        List<HologramVoteModel.Site> page = HologramVoteModel.page(menu.sites, menu.page, menu.settings.sitesPerPage());
        // Capture exact visible targets so a delayed click cannot select a different page's URL.
        if (!menu.displayed.equals(page)) menu.displayed = page;
        for (int i = 0; i < page.size(); i++) {
            var site = page.get(i);
            ItemStack icon = menu.icons.getOrDefault(site.key(), new ItemStack(Material.PAPER)).clone();
            ItemMeta meta = icon.getItemMeta();
            if (meta != null) {
                List<String> lore = meta.hasLore() ? new ArrayList<>(meta.getLore()) : new ArrayList<>();
                lore.add(site.label());
                lore.add("\u00a77Click for the configured voting URL in chat.");
                meta.setDisplayName(site.title());
                meta.setLore(lore);
                if (site.canVote()) {
                    try { ItemMeta.class.getMethod("setEnchantmentGlintOverride", Boolean.class).invoke(meta, Boolean.TRUE); }
                    catch (ReflectiveOperationException olderAPI) { /* Preserve native icon on older Bukkit APIs. */ }
                }
                icon.setItemMeta(meta);
            }
            inventory.setItem((menu.session.type == ExperimentalGUIType.STREAK_TRACK ? 29 : 20) + i, icon);
        }
        if (page.isEmpty()) inventory.setItem(menu.session.type == ExperimentalGUIType.STREAK_TRACK ? 31 : 22,
                item(Material.PAPER, "No visible enabled voting sites", List.of()));
        if (menu.session.type == ExperimentalGUIType.STREAK_TRACK) {
            int start = Math.min(menu.rewardPage, Math.max(0, (menu.milestones.size() - 1) / 5)) * 5;
            for (int i = start; i < Math.min(start + 5, menu.milestones.size()); i++)
                inventory.setItem(20 + i - start, rewardItem(menu, menu.milestones.get(i)));
            if (menu.milestones.isEmpty()) inventory.setItem(22, item(Material.PAPER, "No enabled streak milestones",
                    List.of("Configure VoteStreaks in SpecialRewards.yml to show a reward track.")));
            inventory.setItem(36, item(Material.ARROW, "Previous voting sites", List.of()));
            inventory.setItem(44, item(Material.ARROW, "Next voting sites", List.of()));
        } else {
            menu.milestones.stream().filter(milestone -> milestone.progress() < milestone.threshold())
                    .findFirst().ifPresent(milestone -> inventory.setItem(31, rewardItem(menu, milestone)));
        }
        inventory.setItem(45, item(Material.ARROW, "Previous page", List.of()));
        inventory.setItem(49, item(Material.BARRIER, "Close", List.of()));
        inventory.setItem(50, item(Material.CLOCK, "Refresh status", List.of("Reads voting data asynchronously.")));
        inventory.setItem(53, item(Material.ARROW, "Next page", List.of("Page "
                + (menu.session.type == ExperimentalGUIType.STREAK_TRACK ? menu.rewardPage + 1 : menu.page + 1) + "/"
                + (menu.session.type == ExperimentalGUIType.STREAK_TRACK ? Math.max(1, (menu.milestones.size() + 4) / 5)
                        : HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage())))));
    }

    private void render(Menu menu) {
        if (!sessions.current(menu.session)) return;
        if (menu.session.type == ExperimentalGUIType.VOTING_TERMINAL) {
            renderTerminal(menu);
        } else if (menu.session.type == ExperimentalGUIType.REWARD_SHOWCASE) {
            renderShowcase(menu);
        } else if (menu.session.type == ExperimentalGUIType.RADIAL) {
            renderRadial(menu);
        } else if (menu.session.type == ExperimentalGUIType.NATIVE_DIALOG) {
            renderDialog(menu);
        } else if (menu.session.type == ExperimentalGUIType.NPC && menu.inventory == null) {
            String name = plugin.getConfigFile().getData().getString("Experimental.VoteGUIs.NPC.DisplayName", "Voting Guide");
            if (name == null || name.isBlank()) name = "Voting Guide";
            npc.open(menu.session, menu.player, menu.anchor, name.substring(0, Math.min(80, name.length())), () -> {
                if (sessions.current(menu.session) && permitted(menu.player, menu.session.type)) renderInventory(menu);
            });
            menu.session.activate();
        } else renderInventory(menu);
    }

    private void renderRadial(Menu menu) {
        List<ExperimentalDisplays.Target> targets = new ArrayList<>();
        var page = HologramVoteModel.page(menu.sites, menu.page, menu.settings.sitesPerPage());
        double radius = plugin.getConfigFile().getData().getDouble("Experimental.VoteGUIs.Radial.Radius", 1.25);
        if (!Double.isFinite(radius) || radius < 0.9 || radius > 1.8)
            throw new IllegalArgumentException("Radial.Radius must be 0.9..1.8");
        for (int i = 0; i < page.size(); i++) {
            var site = page.get(i);
            double angle = -Math.PI / 2 + i * Math.PI * 2 / page.size();
            targets.add(new ExperimentalDisplays.Target("site-" + i, site.label(), menu.icons.get(site.key()),
                    Math.cos(angle) * radius, Math.sin(angle) * radius, ExperimentalDisplays.Action.SITE, site.key()));
        }
        targets.add(new ExperimentalDisplays.Target("title", "Voting - Page " + (menu.page + 1) + "/"
                + HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage())
                + (page.isEmpty() ? "\nNo visible voting sites" : "\nAim and right-click a site"), null, 0, 0,
                ExperimentalDisplays.Action.INFO));
        targets.add(new ExperimentalDisplays.Target("previous", "[Previous]", null, -1, -radius - .55, ExperimentalDisplays.Action.PREVIOUS));
        targets.add(new ExperimentalDisplays.Target("next", "[Next]", null, 0, -radius - .55, ExperimentalDisplays.Action.NEXT));
        targets.add(new ExperimentalDisplays.Target("close", "[Close]", null, 1, -radius - .55, ExperimentalDisplays.Action.CLOSE));
        displays.render(menu.session, menu.player, menu.anchor, targets, target -> {
            try {
                if (!sessions.current(menu.session) || !permitted(menu.player, menu.session.type)) return;
                switch (target.action()) {
                    case CLOSE -> sessions.close(menu.session);
                    case PREVIOUS, NEXT -> {
                        menu.page = Math.min(Math.max(menu.page + (target.action() == ExperimentalDisplays.Action.PREVIOUS ? -1 : 1), 0),
                                HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage()) - 1);
                        renderRadial(menu);
                    }
                    case SITE -> sendSiteUrl(menu, target.siteKey());
                    default -> { }
                }
            } catch (RuntimeException failure) { fail(menu.session, menu.player, failure); }
        });
    }

    private void renderTerminal(Menu menu) {
        List<ExperimentalDisplays.Target> targets = new ArrayList<>();
        targets.add(new ExperimentalDisplays.Target("terminal", "Experimental Voting Terminal\n" + menu.session.id
                + "\nAim and right-click a style", null, 0, .65, ExperimentalDisplays.Action.INFO));
        int index = 0;
        for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
            if (type == ExperimentalGUIType.VOTING_TERMINAL) continue;
            String unavailable = unsupported(type);
            boolean enabled = menu.settings.enabled().contains(type);
            targets.add(new ExperimentalDisplays.Target(type.name(), type.configurationKey()
                    + (!enabled ? " [disabled]" : unavailable == null ? "" : " [unavailable]"), null,
                    (index % 2 == 0 ? -.8 : .8), -.05 - (index / 2) * .45, ExperimentalDisplays.Action.TERMINAL_STYLE));
            index++;
        }
        targets.add(new ExperimentalDisplays.Target("close", "[Remove temporary terminal]", null, 0, -2,
                ExperimentalDisplays.Action.CLOSE));
        displays.render(menu.session, menu.player, menu.anchor, targets, target -> {
            if (!sessions.current(menu.session) || !permitted(menu.player, menu.session.type)) return;
            if (target.action() == ExperimentalDisplays.Action.CLOSE) sessions.close(menu.session);
            else if (target.action() == ExperimentalDisplays.Action.TERMINAL_STYLE) {
                // Opening revalidates this style's own switch, capability and permission.
                // A successful replacement retires only this temporary terminal session.
                open(menu.player, ExperimentalGUIType.valueOf(target.id()));
            }
        });
    }

    public void terminalControl(CommandSender sender, String action) {
        Runnable operation = () -> {
            if (!(sender instanceof Player player)) {
                sender.sendMessage("Temporary terminal commands require a player.");
                return;
            }
            if (!permitted(player, ExperimentalGUIType.VOTING_TERMINAL)) {
                player.sendMessage("\u00a7cYou do not have permission to manage experimental terminals.");
                return;
            }
            if (action.equals("create")) { open(player, ExperimentalGUIType.VOTING_TERMINAL); return; }
            Menu terminal = menus.values().stream().filter(menu -> menu.session.player.equals(player.getUniqueId())
                    && menu.session.type == ExperimentalGUIType.VOTING_TERMINAL && sessions.current(menu.session)).findFirst().orElse(null);
            if (terminal == null) { player.sendMessage("No active temporary voting terminal owned by you."); return; }
            if (action.equals("remove")) {
                sessions.close(terminal.session);
                player.sendMessage("Temporary voting terminal removed: " + terminal.session.id);
            } else if (action.equals("list") || action.equals("inspect")) {
                player.sendMessage("Temporary terminal " + terminal.session.id + " | " + terminal.session.state()
                        + " | " + terminal.anchor.getWorld().getName() + " " + terminal.anchor.getBlockX() + ","
                        + terminal.anchor.getBlockY() + "," + terminal.anchor.getBlockZ()
                        + " | private, nonpersistent; timeout " + terminal.settings.timeoutSeconds() + "s");
            } else player.sendMessage("Use /av testterminalgui create|remove|list|inspect");
        };
        if (sender instanceof Player player) scheduler.player(player, operation, () -> {});
        else operation.run();
    }

    private void renderShowcase(Menu menu) {
        List<ExperimentalDisplays.Target> targets = new ArrayList<>();
        int count = menu.milestones.size();
        menu.rewardPage = Math.min(menu.rewardPage, Math.max(0, count - 1));
        if (count == 0) {
            targets.add(new ExperimentalDisplays.Target("reward", "Voting Rewards\nNo enabled streak rewards configured\nTotal votes: " + menu.total,
                    new ItemStack(Material.CHEST), 0, .85, ExperimentalDisplays.Action.INFO));
        } else {
            Milestone milestone = menu.milestones.get(menu.rewardPage);
            String text = milestone.id() + " (" + milestone.type() + ")\n" + milestone.progress() + "/" + milestone.threshold()
                    + " | Total votes: " + menu.total + "\n"
                    + (milestone.recorded() ? "Award recorded" : milestone.progress() >= milestone.threshold() ? "Threshold met" : "Upcoming reward")
                    + " | Preview " + (menu.rewardPage + 1) + "/" + count;
            List<String> description = menu.rewardDescriptions.getOrDefault(milestone.id(), List.of());
            if (!description.isEmpty()) text += "\n" + description.getFirst();
            text = text.substring(0, Math.min(256, text.length()));
            targets.add(new ExperimentalDisplays.Target("reward", text,
                    menu.rewardIcons.getOrDefault(milestone.id(), new ItemStack(Material.CHEST)), 0, .85, ExperimentalDisplays.Action.INFO));
        }
        var page = HologramVoteModel.page(menu.sites, menu.page, menu.settings.sitesPerPage());
        for (int i = 0; i < page.size(); i++) {
            var site = page.get(i);
            targets.add(new ExperimentalDisplays.Target("site-" + i, site.label(), menu.icons.get(site.key()),
                    0, -.05 - i * .42, ExperimentalDisplays.Action.SITE, site.key()));
        }
        targets.add(new ExperimentalDisplays.Target("previous-reward", "[Previous reward]", null, -1.25, .85, ExperimentalDisplays.Action.PREVIOUS_REWARD));
        targets.add(new ExperimentalDisplays.Target("next-reward", "[Next reward]", null, 1.25, .85, ExperimentalDisplays.Action.NEXT_REWARD));
        targets.add(new ExperimentalDisplays.Target("site-page", "[Voting sites " + (menu.page + 1) + "/"
                + HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage()) + "]", null, -.65, -2.35, ExperimentalDisplays.Action.SITE_PAGE));
        targets.add(new ExperimentalDisplays.Target("close", "[Close]", null, .65, -2.35, ExperimentalDisplays.Action.CLOSE));
        displays.render(menu.session, menu.player, menu.anchor, targets, target -> {
            try {
                if (!sessions.current(menu.session) || !permitted(menu.player, menu.session.type)) return;
                switch (target.action()) {
                    case CLOSE -> sessions.close(menu.session);
                    case SITE -> sendSiteUrl(menu, target.siteKey());
                    case PREVIOUS_REWARD, NEXT_REWARD -> {
                        menu.rewardPage = Math.min(Math.max(menu.rewardPage + (target.action() == ExperimentalDisplays.Action.PREVIOUS_REWARD ? -1 : 1), 0),
                                Math.max(0, menu.milestones.size() - 1));
                        renderShowcase(menu);
                    }
                    case SITE_PAGE -> {
                        menu.page = (menu.page + 1) % HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage());
                        renderShowcase(menu);
                    }
                    default -> { }
                }
            } catch (RuntimeException failure) { fail(menu.session, menu.player, failure); }
        });
    }

    private void sendSiteUrl(Menu menu, String key) {
        VoteSite current = visible(menu.player).stream().filter(candidate -> candidate.getKey().equals(key)).findFirst().orElse(null);
        if (current == null) return;
        HologramVoteModel.votingUrl(current.getVoteURL(false)).ifPresentOrElse(url -> {
            TextComponent link = new TextComponent("\u00a7aVote at " + current.getDisplayName() + " \u00a7b[Open link]");
            link.setClickEvent(new ClickEvent(ClickEvent.Action.OPEN_URL, url));
            menu.player.spigot().sendMessage(link);
        }, () -> menu.player.sendMessage("\u00a7cThis site has no valid HTTP/HTTPS voting URL."));
    }

    private void renderDialog(Menu menu) {
        List<ExperimentalDialogs.Button> buttons = new ArrayList<>();
        var page = HologramVoteModel.page(menu.sites, menu.page, menu.settings.sitesPerPage());
        for (var site : page) {
            VoteSite current = visible(menu.player).stream().filter(candidate -> candidate.getKey().equals(site.key())).findFirst().orElse(null);
            if (current == null) continue;
            var url = HologramVoteModel.votingUrl(current.getVoteURL(false));
            buttons.add(url.isPresent()
                    ? new ExperimentalDialogs.Button(site.title(), site.label(), url.get(), null)
                    : new ExperimentalDialogs.Button(site.title(), "No valid HTTP/HTTPS URL", null, "invalid:" + site.key()));
        }
        buttons.add(new ExperimentalDialogs.Button("Previous", "Previous voting sites", null, "previous"));
        buttons.add(new ExperimentalDialogs.Button("Next", "Next voting sites", null, "next"));
        buttons.add(new ExperimentalDialogs.Button("Close", "Close experimental menu", null, "close"));
        String body = "Total votes: " + menu.total + " | Daily streak: " + menu.dailyStreak
                + " | Weekly: " + menu.weeklyStreak + " | Monthly: " + menu.monthlyStreak + "\n"
                + (page.isEmpty() ? "No visible enabled voting sites.\n" : "")
                + "Page " + (menu.page + 1) + "/" + HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage())
                + "\nLinks are handled by your client; previews never grant rewards.";
        var nextReward = menu.milestones.stream().filter(m -> m.progress() < m.threshold()).findFirst();
        if (nextReward.isPresent()) {
            var reward = nextReward.get();
            body += "\nNext streak milestone: " + reward.id() + " " + reward.progress() + "/" + reward.threshold();
        }
        dialogs.render(menu.session, menu.player, "Experimental Voting", body, buttons, action -> {
            if (!sessions.current(menu.session)) return;
            if (action.equals("close")) { sessions.close(menu.session); return; }
            if (action.equals("previous") || action.equals("next")) {
                menu.page = Math.min(Math.max(menu.page + (action.equals("previous") ? -1 : 1), 0),
                        HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage()) - 1);
                renderDialog(menu);
            } else menu.player.sendMessage("\u00a7cThis site has no valid HTTP/HTTPS voting URL.");
        });
    }

    private static ItemStack rewardItem(Menu menu, Milestone milestone) {
        ItemStack item = menu.rewardIcons.getOrDefault(milestone.id(), new ItemStack(Material.CHEST)).clone();
        ItemMeta meta = item.getItemMeta();
        if (meta != null) {
            boolean reached = milestone.progress() >= milestone.threshold();
            meta.setDisplayName((reached ? "\u00a7a" : "\u00a7e") + milestone.id());
            List<String> lore = new ArrayList<>(menu.rewardDescriptions.getOrDefault(milestone.id(), List.of()));
            lore.add("\u00a77" + milestone.type() + " progress: " + milestone.progress() + "/" + milestone.threshold());
            int filled = (int) Math.min(10L, Math.max(0L, (long) milestone.progress() * 10 / Math.max(1, milestone.threshold())));
            lore.add("\u00a7a" + "|".repeat(filled) + "\u00a78" + ".".repeat(10 - filled));
            lore.add("\u00a77Best stored streak: " + milestone.best());
            lore.add(milestone.recorded() ? "\u00a7aAward recorded by the streak engine"
                    : reached ? "\u00a7aThreshold met (preview only)" : "\u00a7eUpcoming milestone");
            if (milestone.recurring()) lore.add("\u00a77Recurring milestone");
            lore.add("\u00a77No claiming or reward execution through this menu.");
            meta.setLore(lore);
            item.setItemMeta(meta);
        }
        return item;
    }

    private static ItemStack item(Material material, String title, List<String> lore) {
        ItemStack item = new ItemStack(material);
        ItemMeta meta = item.getItemMeta();
        if (meta != null) { meta.setDisplayName(title); meta.setLore(lore); item.setItemMeta(meta); }
        return item;
    }

    private void click(ExperimentalSessions.Session session, int slot) {
        Menu menu = menus.get(session.id);
        if (menu == null) return;
        List<HologramVoteModel.Site> displayed = menu.displayed;
        // Inventory callbacks are re-dispatched rather than assuming their execution context.
        scheduler.player(menu.player, () -> {
            try {
            if (!sessions.current(session) || !permitted(menu.player, session.type)
                    || menu.player.getOpenInventory().getTopInventory() != menu.inventory) return;
            if (slot == 49) { sessions.close(session); return; }
            if (slot == 45 || slot == 53) {
                if (session.type == ExperimentalGUIType.STREAK_TRACK) {
                    menu.rewardPage = Math.min(Math.max(menu.rewardPage + (slot == 45 ? -1 : 1), 0),
                            Math.max(0, (menu.milestones.size() - 1) / 5));
                    renderInventory(menu);
                    return;
                }
                menu.page = Math.min(Math.max(menu.page + (slot == 45 ? -1 : 1), 0),
                        HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage()) - 1);
                renderInventory(menu);
                return;
            }
            if (session.type == ExperimentalGUIType.STREAK_TRACK && (slot == 36 || slot == 44)) {
                menu.page = Math.min(Math.max(menu.page + (slot == 36 ? -1 : 1), 0),
                        HologramVoteModel.pages(menu.sites.size(), menu.settings.sitesPerPage()) - 1);
                renderInventory(menu);
                return;
            }
            if (slot == 50) { load(menu, visible(menu.player)); return; }
            int index = slot - (session.type == ExperimentalGUIType.STREAK_TRACK ? 29 : 20);
            if (displayed != menu.displayed || index < 0 || index >= displayed.size()) return;
            var site = displayed.get(index);
            // Configuration or permissions may have changed since the worker snapshot.
            VoteSite current = visible(menu.player).stream().filter(candidate -> candidate.getKey().equals(site.key())).findFirst().orElse(null);
            if (current == null) return;
            HologramVoteModel.votingUrl(current.getVoteURL(false)).ifPresentOrElse(url -> {
                TextComponent link = new TextComponent("\u00a7aVote at " + site.title() + " \u00a7b[Open link]");
                link.setClickEvent(new ClickEvent(ClickEvent.Action.OPEN_URL, url));
                menu.player.spigot().sendMessage(link);
            }, () -> menu.player.sendMessage("\u00a7cThis site has no valid HTTP/HTTPS voting URL."));
            } catch (RuntimeException failure) { fail(session, menu.player, failure); }
        }, () -> sessions.close(session));
    }

    private static boolean permitted(Player player, ExperimentalGUIType type) {
        return player.hasPermission("VotingPlugin.Admin")
                || player.hasPermission("VotingPlugin.Commands.AdminVote." + type.command());
    }

    private String unsupported(ExperimentalGUIType type) {
        if (scheduler.regionized() && (type == ExperimentalGUIType.ANIMATED_INVENTORY
                || type == ExperimentalGUIType.STREAK_TRACK || type == ExperimentalGUIType.NPC)) {
            return "This inventory-based experiment is unavailable on Folia: disabled-plugin player scheduling cannot guarantee safe inventory cleanup.";
        }
        return switch (type) {
            case HOLOGRAM -> HologramVoteMenu.supported() ? null : "Hologram requires native display and interaction APIs (1.19.4+).";
            case ANIMATED_INVENTORY, STREAK_TRACK -> null;
            case NPC -> ExperimentalNPC.supported() ? null : "NPC requires safe native entity spawning and nonpersistent entity APIs.";
            case RADIAL, REWARD_SHOWCASE, VOTING_TERMINAL -> ExperimentalDisplays.supported() ? null : "Floating menus require native display and interaction APIs (1.19.4+).";
            case NATIVE_DIALOG -> ExperimentalDialogs.supported() ? null : "Native dialog requires compatible Paper (1.21.7+) or Spigot dialog APIs and a compatible client.";
        };
    }

    private void cleanupError(RuntimeException failure) { plugin.getLogger().log(Level.WARNING, "Experimental cleanup failed", failure); }
    private void fail(ExperimentalSessions.Session session, Player player, RuntimeException failure) {
        sessions.close(session);
        plugin.getLogger().log(Level.WARNING, "Experimental " + session.type + " failed", failure);
        try { scheduler.player(player, () -> player.sendMessage("\u00a7cExperimental menu failed; see the server console."), () -> {}); }
        catch (RuntimeException rejected) { cleanupError(rejected); }
    }

    public void close(Player player) { sessions.close(player.getUniqueId()); }
    public void clear() { sessions.clear(false); npc.retryCleanup(); displays.retryCleanup(); }
    public void shutdown() { sessions.clear(true); npc.retryCleanup(); displays.retryCleanup(); }

    public void control(CommandSender sender, String action) {
        Runnable operation = () -> {
            if (!sender.hasPermission("VotingPlugin.Admin")
                    && !sender.hasPermission("VotingPlugin.Commands.AdminVote.TestGUI")) {
                sender.sendMessage("\u00a7cYou do not have permission to manage experimental GUIs.");
                return;
            }
            switch (action) {
                case "list" -> list(sender);
                case "status" -> status(sender);
                case "close" -> {
                    if (sender instanceof Player player) close(player);
                    else sender.sendMessage("Close requires a player; cleanup retires all temporary sessions.");
                }
                case "cleanup" -> { clear(); sender.sendMessage("Experimental temporary sessions retired."); }
                default -> sender.sendMessage("Use /av testgui list|close|status|cleanup");
            }
        };
        if (sender instanceof Player player) scheduler.player(player, operation, () -> {});
        else operation.run();
    }

    public void list(CommandSender sender) {
        ExperimentalGUISettings settings;
        try {
            settings = ExperimentalGUISettings.read(plugin.getConfigFile().getData());
        } catch (IllegalArgumentException invalid) {
            sender.sendMessage("\u00a7cInvalid experimental GUI settings: " + invalid.getMessage());
            return;
        }
        for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
            String reason = unsupported(type);
            sender.sendMessage(type.configurationKey() + ": " + (settings.enabled().contains(type) ? "enabled" : "disabled")
                    + (reason == null ? ", supported" : ", unavailable: " + reason));
        }
    }

    public void status(CommandSender sender) {
        sender.sendMessage("Experimental sessions: " + sessions.snapshot().size() + "; pending snapshots: " + loading.get()
                + "; temporary NPCs: " + npc.entityCount() + "; temporary displays: " + displays.entityCount());
        for (var session : sessions.snapshot()) sender.sendMessage(session.type + " " + session.player + " " + session.state());
    }

    @EventHandler public void quit(PlayerQuitEvent event) { sessions.close(event.getPlayer().getUniqueId()); }
    @EventHandler public void world(PlayerChangedWorldEvent event) { sessions.close(event.getPlayer().getUniqueId()); }
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void teleport(PlayerTeleportEvent event) { sessions.close(event.getPlayer().getUniqueId()); }
    @EventHandler public void death(PlayerDeathEvent event) { sessions.close(event.getEntity().getUniqueId()); }
}
