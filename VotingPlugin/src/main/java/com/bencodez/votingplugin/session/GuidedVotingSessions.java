package com.bencodez.votingplugin.session;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.session.GuidedVoteSession;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.votesites.VoteSite;
import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.TextComponent;

/** Chat navigation works on supported clients without an inventory/dialog requirement. */
public final class GuidedVotingSessions implements Listener {
    private final VotingPluginMain plugin;
    private final Map<UUID, GuidedVoteSession> sessions = new LinkedHashMap<>();
    private final Map<UUID, Long> requests = new LinkedHashMap<>();
    private record EarlyReceipt(UUID storageId, String site, UUID occurrence, long time) { }
    private final Map<UUID, UUID> storageIds = new LinkedHashMap<>();
    private final Map<UUID, String> playerNames = new LinkedHashMap<>();
    private final Map<UUID, Map<String, List<EarlyReceipt>>> early = new LinkedHashMap<>();
    private long lifecycle;
    private final java.util.Set<Long> outstanding = new java.util.HashSet<>();
    private final Map<UUID, Long> pending = new LinkedHashMap<>();
    private long generation;

    public GuidedVotingSessions(VotingPluginMain plugin) { this.plugin = plugin; }
    public synchronized void clear() { sessions.clear(); requests.clear(); storageIds.clear(); playerNames.clear(); early.clear(); pending.clear(); generation++; lifecycle++; }

    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public synchronized void credited(PlayerPostVoteEvent event) {
        if (event.getVoteSite() == null || !event.isRealVote() || event.isCancelled()
                || event.getVoteUUID() == null || event.getUuid() == null) return;
        String key = event.getVoteSite().getKey();
        for (var entry : sessions.entrySet()) {
            UUID owner = entry.getKey(); GuidedVoteSession session = entry.getValue();
            if (event.getUuid().equals(storageIds.get(owner))) {
                session.accepted(key, event.getVoteUUID(), event.getVoteTime(), true, false);
            } else if (!storageIds.containsKey(owner) && pending.containsKey(owner) && session.candidate(key)
                    && event.getVoteTime() > session.started() && event.getPlayerName() != null
                    && event.getPlayerName().equalsIgnoreCase(playerNames.get(owner))) {
                // Name only routes a bounded provisional receipt. Resolved storage UUID must
                // match before it can confirm progress; no player/entity access occurs here.
                var receipts = early.computeIfAbsent(owner, ignored -> new LinkedHashMap<>())
                        .computeIfAbsent(key, ignored -> new ArrayList<>());
                receipts.removeIf(receipt -> receipt.storageId().equals(event.getUuid()));
                if (receipts.size() >= 4) receipts.remove(0);
                receipts.add(new EarlyReceipt(event.getUuid(), key, event.getVoteUUID(), event.getVoteTime()));
            }
        }
        // Never reopen an interface or execute rewards from a notification.
    }

    public void command(Player player, String action) {
        long epoch;
        synchronized (this) { epoch = lifecycle; }
        BukkitCompletionScheduler.run(plugin, player, () -> {
            synchronized (this) { if (epoch == lifecycle) prepare(player, action); }
        }, () -> { }, () -> { });
    }
    private void prepare(Player player, String action) {
        if (!player.isOnline()) return;
        if (!plugin.getConfigFile().getData().getBoolean("GuidedVotingSession.Enabled", false)) {
            player.sendMessage("Guided voting sessions are disabled by this server."); return;
        }
        if (!player.hasPermission("VotingPlugin.Commands.Vote.Session") && !player.hasPermission("VotingPlugin.Player")) return;
        UUID uuid = player.getUniqueId();
        String name = player.getName();
        List<VoteSite> visible = new ArrayList<>();
        for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) {
            if (site.isEnabled() && !site.isHidden() && (site.getPermissionToView().isEmpty()
                    || player.hasPermission(site.getPermissionToView()))) visible.add(site);
            if (visible.size() >= 100) break;
        }
        GuidedVoteSession session;
        long request;
        synchronized (this) {
            if (pending.containsKey(uuid) || outstanding.size() >= 64) {
                player.sendMessage("Voting status is already being checked or busy. Please try again shortly."); return;
            }
            long now = System.currentTimeMillis();
            int minutes = Math.max(1, Math.min(120, plugin.getConfigFile().getData().getInt("GuidedVotingSession.TimeoutMinutes", 30)));
            sessions.entrySet().removeIf(e -> now - e.getValue().started() >= minutes * 60_000L);
            requests.keySet().retainAll(sessions.keySet());
            storageIds.keySet().retainAll(sessions.keySet());
            playerNames.keySet().retainAll(sessions.keySet());
            early.keySet().retainAll(sessions.keySet());
            if (!sessions.containsKey(uuid) && sessions.size() >= 2048) {
                player.sendMessage("Voting sessions are busy. Please try again later."); return;
            }
            if (action.equals("restart")) { sessions.remove(uuid); storageIds.remove(uuid); early.remove(uuid); }
            session = sessions.computeIfAbsent(uuid, ignored -> new GuidedVoteSession(now));
            session.bindCandidates(visible.stream().map(site -> new GuidedVoteSession.Site(site.getKey(),
                    site.getDisplayName(), site.getVoteURL(false), false, 0)).toList());
            playerNames.put(uuid, name);
            request = ++generation;
            requests.put(uuid, request);
            pending.put(uuid, request);
            outstanding.add(request);
        }
        try {
            plugin.getTimer().execute(() -> {
                try {
                    synchronized (this) { if (!current(uuid, session, request)) return; }
                    var currentSites = new java.util.HashMap<String, VoteSite>();
                    for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) currentSites.put(site.getKey(), site);
                    var sampledSites = new java.util.HashMap<String, VoteSite>();
                    var user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, name);
                    List<GuidedVoteSession.Site> snapshots = new ArrayList<>();
                    for (VoteSite original : visible) {
                        VoteSite site = currentSites.get(original.getKey());
                        if (site == null || !site.isEnabled() || site.isHidden()) continue;
                        sampledSites.put(site.getKey(), site);
                        snapshots.add(new GuidedVoteSession.Site(site.getKey(), site.getDisplayName(),
                                site.getVoteURL(false), user.canVoteSite(site), user.getTime(site)));
                    }
                    synchronized (this) {
                        if (!current(uuid, session, request)) return;
                        UUID storageId = user.getJavaUUID();
                        storageIds.put(uuid, storageId);
                        var provisional = early.remove(uuid);
                        if (provisional != null) for (var receipts : provisional.values()) for (var receipt : receipts) {
                            if (receipt.storageId().equals(storageId)) session.accepted(receipt.site(), receipt.occurrence(), receipt.time(), true, false);
                        }
                        session.refresh(snapshots);
                    }
                    var view = session.view(action);
                    BukkitCompletionScheduler.run(plugin, player, () -> {
                        synchronized (this) { if (!current(uuid, session, request)) return; }
                        if (player.isOnline() && plugin.getConfigFile().getData().getBoolean("GuidedVotingSession.Enabled", false)
                                && (player.hasPermission("VotingPlugin.Commands.Vote.Session") || player.hasPermission("VotingPlugin.Player"))) {
                            render(player, visibleView(player, view, sampledSites));
                        }
                    }, () -> { }, () -> { });
                } catch (RuntimeException failure) {
                    plugin.debug(failure);
                    BukkitCompletionScheduler.run(plugin, player, () -> {
                        synchronized (this) { if (!current(uuid, session, request)) return; }
                        if (player.isOnline() && plugin.getConfigFile().getData().getBoolean("GuidedVotingSession.Enabled", false)
                                && (player.hasPermission("VotingPlugin.Commands.Vote.Session") || player.hasPermission("VotingPlugin.Player"))) player.sendMessage("Could not check voting status. Please try /vote session check.");
                    }, () -> { }, () -> { });
                } finally {
                    synchronized (this) { pending.remove(uuid, request); outstanding.remove(request); }
                }
            });
        } catch (RejectedExecutionException unavailable) {
            synchronized (this) { pending.remove(uuid, request); outstanding.remove(request); }
            player.sendMessage("Voting status is temporarily unavailable. Please try again.");
        }
    }
    private synchronized boolean current(UUID uuid, GuidedVoteSession session, long request) {
        return sessions.get(uuid) == session && Long.valueOf(request).equals(requests.get(uuid));
    }
    /** Recheck visibility on the player context, after any asynchronous wait. */
    private GuidedVoteSession.View visibleView(Player player, GuidedVoteSession.View view, Map<String, VoteSite> sampledSites) {
        java.util.Set<String> allowed = new java.util.HashSet<>();
        for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) {
            if (sampledSites.get(site.getKey()) == site && site.isEnabled() && !site.isHidden() && (site.getPermissionToView().isEmpty()
                    || player.hasPermission(site.getPermissionToView()))) allowed.add(site.getKey());
        }
        List<GuidedVoteSession.Entry> entries = new ArrayList<>();
        for (var entry : view.entries()) entries.add(allowed.contains(entry.site().key()) ? entry
                : new GuidedVoteSession.Entry(new GuidedVoteSession.Site(entry.site().key(), "Unavailable site",
                        null, false, 0), GuidedVoteSession.Status.UNAVAILABLE));
        return new GuidedVoteSession.View(List.copyOf(entries), view.index(), view.finished());
    }
    private void render(Player player, GuidedVoteSession.View view) {
        player.sendMessage("Voting session: " + view.received() + "/" + view.entries().size() + " votes received.");
        if (view.entries().isEmpty()) {
            player.sendMessage("No eligible, visible voting sites. Try /vote session restart after your cooldowns end."); return;
        }
        if (view.finished() || view.complete()) {
            player.sendMessage(view.complete() ? "All session votes were received. Thank you!"
                    : "Session finished. Unconfirmed votes remain unconfirmed; normal vote rewards are unchanged.");
        }
        for (int i = 0; i < view.entries().size(); i++) {
            var entry = view.entries().get(i);
            player.sendMessage((i == view.index() ? "> " : "  ") + entry.site().name() + " — " + entry.status());
        }
        var entry = view.current();
        if (!view.finished() && (entry.status() == GuidedVoteSession.Status.AWAITING || entry.status() == GuidedVoteSession.Status.SKIPPED)) {
            String url = GuidedVoteSession.httpUrl(entry.site().url());
            if (url != null) button(player, "Open voting link: " + entry.site().name(), ClickEvent.Action.OPEN_URL, url);
            else player.sendMessage("This site has no valid HTTP/HTTPS link. Ask an administrator.");
        }
        for (String action : List.of("previous", "next", "skip", "check", "finish", "restart"))
            button(player, "[" + action + "]", ClickEvent.Action.RUN_COMMAND, "/vote session " + action);
        player.sendMessage("A link or click is not vote confirmation. Check again after the vote arrives.");
    }
    private static void button(Player player, String title, ClickEvent.Action action, String value) {
        TextComponent component = new TextComponent(title);
        component.setClickEvent(new ClickEvent(action, value));
        player.spigot().sendMessage(component);
    }
}
