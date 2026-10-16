package com.bencodez.votingplugin.specialrewards.datemilestones;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;
import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Owner-defined windows, independently accounted on one backend using original occurrence IDs. */
public final class DateVoteMilestones {
    private record Definition(DateVoteEvent event, YamlConfiguration rewards, java.util.Set<Integer> enabledThresholds,
            java.util.Map<Integer, DateVoteMilestoneConfig.Reward> milestones) { }
    private final VotingPluginMain plugin;
    private volatile List<Definition> definitions = List.of();
    private final java.util.Set<UUID> pendingProgress = new java.util.HashSet<>();
    private final java.util.concurrent.atomic.AtomicInteger deferredAccounting = new java.util.concurrent.atomic.AtomicInteger();
    private DateVoteLedger ledger;
    public DateVoteMilestones(VotingPluginMain plugin) { this.plugin = plugin; }
    private synchronized DateVoteLedger ledger() {
        if (ledger == null) ledger = new DateVoteLedger(plugin.getDataFolder().toPath().resolve("date-vote-milestones"));
        return ledger;
    }
    /** Configuration is already loaded; this method performs no disk access. */
    public void reload() {
        List<Definition> loaded = new ArrayList<>();
        YamlConfiguration rewards = new YamlConfiguration();
        var root = plugin.getSpecialRewardsConfig().getData().getConfigurationSection("DateVoteMilestones");
        if (root != null && !root.getKeys(false).isEmpty()) {
            try { rewards.loadFromString(plugin.getSpecialRewardsConfig().getData().saveToString()); }
            catch (org.bukkit.configuration.InvalidConfigurationException invalid) {
                definitions = List.of(); plugin.getLogger().warning("DateVoteMilestones configuration snapshot invalid"); return;
            }
        }
        int considered = 0;
        if (root != null) for (String id : root.getKeys(false)) {
            if (++considered > 64) { plugin.getLogger().warning("DateVoteMilestones supports at most 64 configured events"); break; }
            try {
                ConfigurationSection section = root.getConfigurationSection(id);
                if (section == null) throw new IllegalArgumentException("Expected an event section");
                registerRewardHandles(id, section, rewards);
                DateVoteEvent event = parse(id, section);
                if (plugin.getBungeeSettings().isUseBungeecoord() && event.accountingServer().isBlank())
                    throw new IllegalArgumentException("Proxy events require one explicit AccountingServer and all-server identified delivery");
                var configured = DateVoteMilestoneConfig.read(section);
                var enabledThresholds = configured.entrySet().stream().filter(entry -> entry.getValue().enabled())
                        .map(java.util.Map.Entry::getKey).collect(java.util.stream.Collectors.toUnmodifiableSet());
                loaded.add(new Definition(event, rewards, enabledThresholds, java.util.Map.copyOf(configured)));
            } catch (Exception invalid) {
                plugin.getLogger().warning("DateVoteMilestones " + id + " disabled: " + invalid.getMessage());
            }
        }
        definitions = List.copyOf(loaded);
    }
    /** Queued rewards must resolve even when an accounting-only setting is invalid. */
    private void registerRewardHandles(String id, ConfigurationSection section, YamlConfiguration rewards) {
        String fileId = DateVoteEvent.fileId(id);
        var milestones = section.getConfigurationSection("Milestones");
        if (milestones == null) return;
        java.util.Map<Integer, DateVoteMilestoneConfig.Reward> configuredRewards = new java.util.LinkedHashMap<>();
        java.util.Set<Integer> ambiguous = new java.util.HashSet<>();
        int count = 0;
        for (String key : milestones.getKeys(false)) {
            if (++count > 64) break;
            try {
                for (var entry : DateVoteMilestoneConfig.entry(key, milestones.getConfigurationSection(key)).entrySet()) {
                    if (configuredRewards.size() >= 64 && !configuredRewards.containsKey(entry.getKey())) continue;
                    if (configuredRewards.putIfAbsent(entry.getKey(), entry.getValue()) != null) ambiguous.add(entry.getKey());
                }
            } catch (IllegalArgumentException invalid) { /* Keep independent valid queued reward handles. */ }
        }
        for (var entry : configuredRewards.entrySet()) {
            int threshold = entry.getKey();
            if (ambiguous.contains(threshold)) continue;
            String source = "DateVoteMilestones." + id + ".Milestones." + entry.getValue().key() + ".Rewards";
            var configured = rewards.getConfigurationSection(source);
            if (configured == null) continue;
            String alias = "DateVoteMilestonesRuntime." + fileId + ".Milestones." + threshold + ".Rewards";
            rewards.createSection(alias, configured.getValues(true));
            plugin.addDirectlyDefinedRewards(new DirectlyDefinedReward(alias) {
                private String sourcePath(String requested) {
                    if (!requested.equals(alias) && !requested.startsWith(alias + "."))
                        throw new IllegalArgumentException("Unexpected date milestone reward path");
                    String current = currentRewardSource(plugin.getSpecialRewardsConfig().getData(), id, threshold);
                    if (current == null) throw new IllegalStateException("Date milestone reward definition is unavailable");
                    return current + requested.substring(alias.length());
                }
                @Override public ConfigurationSection getFileData() {
                    YamlConfiguration data = new YamlConfiguration();
                    var live = plugin.getSpecialRewardsConfig().getData();
                    String currentSource = currentRewardSource(live, id, threshold);
                    var current = currentSource == null ? null
                            : live.getConfigurationSection(currentSource);
                    if (current != null) data.createSection(alias, current.getValues(true));
                    return data;
                }
                @Override public void createSection(String path) { plugin.getSpecialRewardsConfig().createSection(sourcePath(path)); }
                @Override public void setData(String path, Object value) { plugin.getSpecialRewardsConfig().setValue(sourcePath(path), value); }
                @Override public void save() { plugin.getSpecialRewardsConfig().saveData(); }
            });
        }
    }
    /** Old registry handles can outlive YAML replacement during reload; resolve by stable threshold. */
    private static String currentRewardSource(ConfigurationSection live, String id, int threshold) {
        var milestones = live.getConfigurationSection("DateVoteMilestones." + id + ".Milestones");
        if (milestones == null) return null;
        String found = null;
        int count = 0;
        for (String key : milestones.getKeys(false)) {
            if (++count > 64) break;
            try {
                if (!DateVoteMilestoneConfig.entry(key, milestones.getConfigurationSection(key)).containsKey(threshold)) continue;
                if (found != null) return null;
                found = "DateVoteMilestones." + id + ".Milestones." + key + ".Rewards";
            } catch (IllegalArgumentException invalid) { /* An unrelated invalid entry does not hide queued rewards. */ }
        }
        return found;
    }
    public static DateVoteEvent parse(String id, ConfigurationSection section) {
        String zone = section.getString("Timezone");
        String start = section.getString("Start"), end = section.getString("End");
        var milestones = section.getConfigurationSection("Milestones");
        if (zone == null || start == null || end == null || milestones == null)
            throw new IllegalArgumentException("Start, End, Timezone and Milestones are required");
        List<Integer> thresholds = new ArrayList<>(DateVoteMilestoneConfig.read(section).keySet());
        Object rawSites = section.get("VoteSites");
        if (rawSites != null && (!(rawSites instanceof List<?> sites)
                || sites.stream().anyMatch(site -> !(site instanceof String))))
            throw new IllegalArgumentException("VoteSites must be a list of configured site keys");
        return new DateVoteEvent(id, section.getString("DisplayName", id), section.getBoolean("Enabled", false),
                DateVoteEvent.timestamp(start, zone), DateVoteEvent.timestamp(end, zone), zone, thresholds,
                new java.util.HashSet<>(section.getStringList("VoteSites")), section.getString("AccountingServer", ""));
    }
    static String path(DateVoteEvent event, int threshold) {
        return "DateVoteMilestonesRuntime." + event.fileId() + ".Milestones." + threshold + ".Rewards";
    }
    /** Owner-thread extension events use the existing ordered storage worker without waiting on gameplay. */
    public boolean deferAccepted(VotingPluginUser user, String site, UUID occurrence, long occurredAt,
            boolean real, boolean proxy, boolean canonicalProxy, boolean targeted, boolean forceProxyRouting) {
        if (plugin.getBungeeSettings().isUseBungeecoord() && !proxy) return true;
        if (!real || occurrence == null || occurredAt <= 0 || proxy && (!canonicalProxy || targeted)) return true;
        List<Definition> snapshot = definitions;
        if (snapshot.stream().noneMatch(definition -> definition.event().matches(occurredAt, site, real, proxy,
                plugin.getBungeeSettings().getServer()))) return true;
        int pending;
        do {
            pending = deferredAccounting.get();
            if (pending >= 64) {
                plugin.getLogger().warning("DateVoteMilestones deferred accounting is busy; occurrence remains unconfirmed.");
                return false;
            }
        } while (!deferredAccounting.compareAndSet(pending, pending + 1));
        try {
            plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                try {
                    if (!plugin.isEnabled() || snapshot != definitions) return;
                    accepted(user, site, occurrence, occurredAt, real, proxy, canonicalProxy, targeted, forceProxyRouting);
                } finally { deferredAccounting.decrementAndGet(); }
            });
        } catch (RejectedExecutionException rejected) {
            deferredAccounting.decrementAndGet();
            plugin.getLogger().warning("DateVoteMilestones deferred accounting was rejected; occurrence remains unconfirmed.");
        }
        return false;
    }
    /** Called only from the real asynchronous accepted-vote pipeline; never from navigation. */
    public boolean accepted(VotingPluginUser user, String site, UUID occurrence, long occurredAt,
            boolean real, boolean proxy, boolean canonicalProxy, boolean targeted, boolean forceProxyRouting) {
        if (plugin.getBungeeSettings().isUseBungeecoord() && !proxy) return true;
        if (!real || occurrence == null || occurredAt <= 0 || proxy && (!canonicalProxy || targeted)) return true;
        boolean recorded = true;
        for (Definition definition : definitions) {
            DateVoteEvent event = definition.event();
            if (!event.matches(occurredAt, site, real, proxy, plugin.getBungeeSettings().getServer())) continue;
            List<Integer> reached;
            try {
                reached = ledger().record(event, user.getJavaUUID(), occurrence, plugin.getOptions().isProcessRewards());
            } catch (IOException | RuntimeException failure) {
                recorded = false;
                plugin.getLogger().warning("DateVoteMilestones " + event.id() + " occurrence was not confirmed; proxy delivery must remain quarantined for accounting reconciliation: " + failure.getMessage());
                continue;
            }
            try {
                for (int threshold : reached) {
                    if (!plugin.getOptions().isProcessRewards()) break;
                    if (!definition.enabledThresholds().contains(threshold)) continue;
                    var placeholders = new HashMap<String, String>();
                    placeholders.put("DateVoteEvent", event.displayName());
                    placeholders.put("DateVoteThreshold", Integer.toString(threshold));
                    if (!ledger().reserve(event, user.getJavaUUID(), threshold)) continue;
                    plugin.getRewardHandler().giveReward(user, definition.rewards(), path(event, threshold),
                            new RewardOptions().setPrefix("DateVoteMilestones-" + event.id() + "-" + threshold)
                                    .setServer(forceProxyRouting).setPlaceholders(placeholders));
                    // Submission is not a transactional guarantee of arbitrary command execution.
                    ledger().submitted(event, user.getJavaUUID(), threshold);
                }
            } catch (IOException | RuntimeException failure) {
                plugin.getLogger().warning("DateVoteMilestones " + event.id() + " accounting/reward submission failed; progress retained for review: " + failure.getMessage());
            }
        }
        return recorded;
    }
    public void progress(Player player) {
        progress(player, (view, current) -> com.bencodez.votingplugin.commands.gui.player.DateVoteEventsMenu.open(plugin, player, view, current));
    }
    /** Display is an entity-owned effect; identity resolution and ledger reads stay on the storage worker. */
    void progress(Player player, java.util.function.BiConsumer<DateVoteProgressSnapshot, java.util.function.BooleanSupplier> display) {
        BukkitCompletionScheduler.run(plugin, player, () -> {
            UUID uuid = player.getUniqueId();
            String playerName = player.getName();
            if (!canView(player)) return;
            synchronized (pendingProgress) {
                if (pendingProgress.contains(uuid) || pendingProgress.size() >= 64) {
                    player.sendMessage("Date voting progress is already being checked or busy. Try again shortly."); return;
                }
                pendingProgress.add(uuid);
            }
            List<Definition> snapshot = definitions;
            try {
                plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                    try {
                    if (snapshot != definitions || !plugin.isEnabled()) return;
                    boolean proxyBackend = plugin.getBungeeSettings().isUseBungeecoord();
                    String server = plugin.getBungeeSettings().getServer();
                    boolean localProgress = snapshot.stream().anyMatch(d -> !proxyBackend || d.event().accountingServer().equals(server));
                    UUID storageId = !localProgress ? uuid
                            : plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, playerName).getJavaUUID();
                    List<DateVoteProgressSnapshot.Event> events = new ArrayList<>();
                    long now = System.currentTimeMillis();
                    for (Definition definition : snapshot) {
                        var event = definition.event();
                        var milestones = event.thresholds().stream().map(threshold -> {
                            var configured = definition.milestones().get(threshold);
                            var rewards = definition.rewards().getConfigurationSection(path(event, threshold));
                            var types = rewards == null ? List.<String>of() : rewards.getKeys(false).stream()
                                    .filter(key -> key.length() <= 64).limit(16).toList();
                            return new DateVoteProgressSnapshot.Milestone(threshold, configured.key(), configured.enabled(), types);
                        }).toList();
                        if (proxyBackend && !event.accountingServer().equals(server)) {
                            events.add(view(event, DateVoteProgressSnapshot.Availability.REMOTE, null, milestones)); continue;
                        }
                        try {
                            var progress = ledger().progress(event, storageId);
                            events.add(view(event, DateVoteProgressSnapshot.Availability.OWNED, progress, milestones));
                        } catch (IOException failure) {
                            events.add(view(event, DateVoteProgressSnapshot.Availability.UNAVAILABLE, null, milestones));
                        }
                    }
                    var result = new DateVoteProgressSnapshot(now, events);
                    java.util.function.BooleanSupplier current = () -> canView(player) && snapshot == definitions
                            && plugin.getDateVoteMilestones() == this;
                    BukkitCompletionScheduler.run(plugin, player, () -> {
                        if (current.getAsBoolean()) display.accept(result, current);
                    }, () -> { }, () -> { });
                    } catch (RuntimeException unavailable) {
                        BukkitCompletionScheduler.run(plugin, player, () -> {
                            if (canView(player) && snapshot == definitions)
                                player.sendMessage("Date voting progress is temporarily unavailable.");
                        }, () -> { }, () -> { });
                    } finally { synchronized (pendingProgress) { pendingProgress.remove(uuid); } }
                });
            } catch (RejectedExecutionException unavailable) {
                synchronized (pendingProgress) { pendingProgress.remove(uuid); }
                player.sendMessage("Date voting progress is temporarily unavailable.");
            }
        }, () -> { }, () -> { });
    }
    private boolean canView(Player player) {
        return plugin.isEnabled() && player.isOnline()
                && (player.hasPermission("VotingPlugin.Commands.Vote.DateEvents") || player.hasPermission("VotingPlugin.Player"));
    }
    private static DateVoteProgressSnapshot.Event view(DateVoteEvent event, DateVoteProgressSnapshot.Availability availability,
            DateVoteLedger.Progress progress, List<DateVoteProgressSnapshot.Milestone> milestones) {
        return new DateVoteProgressSnapshot.Event(event, availability, progress == null ? 0 : progress.votes(),
                progress == null ? java.util.Set.of() : progress.submittedAwards(),
                progress == null ? java.util.Set.of() : progress.deferredAwards(),
                progress == null ? java.util.Set.of() : progress.reservedAwards(), milestones);
    }
}
