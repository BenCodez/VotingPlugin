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
import org.bukkit.command.CommandSender;
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
            java.util.Map<Integer, DateVoteMilestoneConfig.Reward> milestones, boolean countFakeVotes) { }
    private final VotingPluginMain plugin;
    private volatile List<Definition> definitions = List.of();
    private final java.util.Set<UUID> pendingProgress = new java.util.HashSet<>();
    private final java.util.concurrent.atomic.AtomicInteger deferredAccounting = new java.util.concurrent.atomic.AtomicInteger();
    private final java.util.concurrent.atomic.AtomicInteger pendingAdministration = new java.util.concurrent.atomic.AtomicInteger();
    private record PlaceholderIdentity(UUID player, boolean resolveName) { }
    private final java.util.Map<PlaceholderIdentity, CachedProgress> placeholderCache = new java.util.LinkedHashMap<>(256, 0.75f, true);
    private static final class ProgressRequest {
        boolean invalidated;
        UUID resolvedStorageId;
    }
    private final java.util.Map<PlaceholderIdentity, ProgressRequest> placeholderRequests = new java.util.HashMap<>();
    private record CachedProgress(Object generation, long expires, UUID storageId, java.util.Map<String, DateVoteLedger.Progress> events) { }
    private volatile Object generation = new Object();
    private DateVoteLedger ledger;
    public DateVoteMilestones(VotingPluginMain plugin) { this.plugin = plugin; }
    private synchronized DateVoteLedger ledger() {
        if (ledger == null) ledger = new DateVoteLedger(plugin.getDataFolder().toPath().resolve("date-vote-milestones"));
        return ledger;
    }
    /** Configuration is already loaded; this method performs no disk access. */
    public void reload() {
        generation = null;
        synchronized (placeholderCache) {
            placeholderCache.clear();
            // Queued/running refreshes still own their admission until their existing finally/rejection path releases it.
            placeholderRequests.values().forEach(request -> request.invalidated = true);
        }
        List<Definition> loaded = new ArrayList<>();
        YamlConfiguration rewards = new YamlConfiguration();
        var root = plugin.getSpecialRewardsConfig().getData().getConfigurationSection("DateVoteMilestones");
        if (root != null && !root.getKeys(false).isEmpty()) {
            try { rewards.loadFromString(plugin.getSpecialRewardsConfig().getData().saveToString()); }
            catch (org.bukkit.configuration.InvalidConfigurationException invalid) {
                definitions = List.of(); generation = new Object();
                plugin.getLogger().warning("DateVoteMilestones configuration snapshot invalid"); return;
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
                loaded.add(new Definition(event, rewards, enabledThresholds, java.util.Map.copyOf(configured),
                        section.getBoolean("CountFakeVotes", true)));
            } catch (Exception invalid) {
                plugin.getLogger().warning("DateVoteMilestones " + id + " disabled: " + invalid.getMessage());
            }
        }
        definitions = List.copyOf(loaded);
        generation = new Object();
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
        if (section.contains("CountFakeVotes") && !section.isBoolean("CountFakeVotes"))
            throw new IllegalArgumentException("CountFakeVotes must be a boolean");
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
        if (occurrence == null || occurredAt <= 0 || proxy && (!canonicalProxy || targeted)) return true;
        List<Definition> snapshot = definitions;
        if (snapshot.stream().noneMatch(definition -> definition.event().matches(occurredAt, site,
                real || definition.countFakeVotes(), proxy,
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
    /** Called only from the asynchronous accepted-vote pipeline; never from navigation. */
    public boolean accepted(VotingPluginUser user, String site, UUID occurrence, long occurredAt,
            boolean real, boolean proxy, boolean canonicalProxy, boolean targeted, boolean forceProxyRouting) {
        if (plugin.getBungeeSettings().isUseBungeecoord() && !proxy) return true;
        if (occurrence == null || occurredAt <= 0 || proxy && (!canonicalProxy || targeted)) return true;
        boolean recorded = true;
        for (Definition definition : definitions) {
            DateVoteEvent event = definition.event();
            if (!event.matches(occurredAt, site, real || definition.countFakeVotes(), proxy,
                    plugin.getBungeeSettings().getServer())) continue;
            List<Integer> reached;
            try {
                reached = ledger().record(event, user.getJavaUUID(), occurrence, plugin.getOptions().isProcessRewards());
                invalidateProgress(user.getJavaUUID());
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
    public enum AdminAction { Status, Preview, SetVotes, Force, ForceNoLimits }

    /** Admission is not success: all identity/storage/reward work runs on the ordered worker. */
    public void administer(CommandSender sender, AdminAction action, String player, String eventId, String argument) {
        Object epoch = generation;
        dispatchAdmin(sender, action, () -> {
            if (!current(epoch) || !adminAllowed(sender, action)) return;
            if (!validPlayer(player) || eventId == null || !eventId.matches("[A-Za-z0-9_-]{1,64}")) {
                sender.sendMessage("Invalid exact player or event ID."); return;
            }
            int count;
            do { count = pendingAdministration.get();
                if (count >= 64) { sender.sendMessage("Date milestone administration is busy. Try again shortly."); return; }
            } while (!pendingAdministration.compareAndSet(count, count + 1));
            try {
                plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                    try {
                        if (!current(epoch)) return;
                        List<String> result = administerOnWorker(action, player, eventId, argument, epoch);
                        dispatchAdmin(sender, action, () -> {
                            if (current(epoch) && adminAllowed(sender, action)) result.forEach(sender::sendMessage);
                        });
                    } catch (IOException | RuntimeException failure) {
                        dispatchAdmin(sender, action, () -> {
                            if (current(epoch) && adminAllowed(sender, action))
                                sender.sendMessage("Date milestone operation failed; inspect retained progress before retrying rewards.");
                        });
                    } finally { pendingAdministration.decrementAndGet(); }
                });
            } catch (RejectedExecutionException unavailable) {
                pendingAdministration.decrementAndGet();
                sender.sendMessage("Date milestone storage is unavailable; nothing was submitted.");
            }
        });
    }
    private boolean current(Object epoch) {
        return epoch != null && epoch == generation && plugin.isEnabled() && plugin.getDateVoteMilestones() == this;
    }
    private static boolean validPlayer(String player) {
        return player != null && (player.matches("[A-Za-z0-9_]{1,16}")
                || player.matches("[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"));
    }
    private boolean adminAllowed(CommandSender sender, AdminAction action) {
        return (!(sender instanceof Player p) || p.isOnline()) && (sender.hasPermission("VotingPlugin.Admin")
                || sender.hasPermission("VotingPlugin.Commands.AdminVote.DateVoteMilestone." + action.name()));
    }
    private void dispatchAdmin(CommandSender sender, AdminAction action, Runnable effect) {
        Player player = sender instanceof Player p ? p : null;
        Runnable checked = () -> { if (plugin.isEnabled() && adminAllowed(sender, action)) effect.run(); };
        BukkitCompletionScheduler.run(plugin, player, checked, player == null ? checked : () -> { }, () -> { });
    }
    private List<String> administerOnWorker(AdminAction action, String player, String eventId, String argument,
            Object epoch) throws IOException {
        Definition definition = definitions.stream().filter(d -> d.event().id().equals(eventId)).findFirst().orElse(null);
        if (definition == null) return List.of("Unknown or invalid date event ID.");
        DateVoteEvent event = definition.event();
        boolean network = plugin.getBungeeSettings().isUseBungeecoord();
        if (network && !event.accountingServer().equals(plugin.getBungeeSettings().getServer()))
            return List.of("Progress is remote; run this command on the configured AccountingServer.");
        Integer value = null;
        List<Integer> selected = List.of();
        if (action == AdminAction.SetVotes) {
            if (argument == null || !argument.matches("0|[1-9][0-9]{0,3}") || Integer.parseInt(argument) > 4096)
                return List.of("Votes must be an integer from 0 to 4096.");
            value = Integer.parseInt(argument);
        } else if (action == AdminAction.Force || action == AdminAction.ForceNoLimits) {
            if (!event.enabled() || !plugin.getOptions().isProcessRewards())
                return List.of("Event or reward processing is disabled; no reward was submitted.");
            if (argument == null || !argument.matches("[A-Za-z0-9_-]{1,64}")) return List.of("Invalid milestone ID or threshold.");
            String selector = argument;
            // A named At/Every entry can expand into several independently fenced thresholds.
            selected = event.thresholds().stream().filter(n -> definition.milestones().get(n).key().equals(selector)).toList();
            if (selected.isEmpty()) selected = event.thresholds().stream().filter(n -> Integer.toString(n).equals(selector)).toList();
            if (selected.isEmpty()) return List.of("Unknown milestone ID or threshold.");
        }
        VotingPluginUser user = player.length() == 36
                ? plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(player), false)
                : plugin.getVotingPluginUserManager().getVotingPluginUser(player);
        UUID storageId = user.getJavaUUID();
        if (!current(epoch)) return List.of();
        if (action == AdminAction.SetVotes) {
            var result = ledger().setVotes(event, storageId, value);
            invalidateProgress(storageId);
            return List.of("Date event " + eventId + " votes set to " + result.votes()
                    + "; receipt IDs and reward fences retained. No rewards were submitted.");
        }
        if (action == AdminAction.Force || action == AdminAction.ForceNoLimits) {
            int submitted = 0;
            for (int threshold : selected) {
                if (!current(epoch) || !plugin.getOptions().isProcessRewards()) break;
                if (!definition.enabledThresholds().contains(threshold)) continue;
                if (!ledger().reserveForced(event, storageId, threshold, action == AdminAction.ForceNoLimits)) continue;
                invalidateProgress(storageId);
                var placeholders = new HashMap<String, String>();
                placeholders.put("DateVoteEvent", event.displayName());
                placeholders.put("DateVoteThreshold", Integer.toString(threshold));
                plugin.getRewardHandler().giveReward(user, definition.rewards(), path(event, threshold),
                        new RewardOptions().setPrefix("DateVoteMilestones-" + event.id() + "-" + threshold)
                                .setServer(network).setPlaceholders(placeholders));
                ledger().submitted(event, storageId, threshold);
                submitted++;
            }
            return List.of("Date event " + eventId + ": submitted " + submitted + " of " + selected.size()
                    + " selected thresholds; disabled or fenced thresholds skipped. Vote count unchanged.");
        }
        var progress = ledger().progress(event, storageId);
        List<String> lines = new ArrayList<>();
        lines.add("Date event " + eventId + ": votes=" + progress.votes() + ", CountFakeVotes=" + definition.countFakeVotes()
                + ", status=" + (!event.enabled() ? "disabled" : System.currentTimeMillis() < event.start() ? "upcoming"
                        : System.currentTimeMillis() >= event.end() ? "ended" : "active")
                + ". Stored in date-vote-milestones/<event-file-id>-<player-uuid>.properties.");
        for (int threshold : event.thresholds()) lines.add("Milestone " + definition.milestones().get(threshold).key()
                + " at " + threshold + ": " + (progress.reservedAwards().contains(threshold) ? "pending review"
                        : progress.submittedAwards().contains(threshold) ? "submitted"
                        : progress.deferredAwards().contains(threshold) ? "deferred" : "not reached")
                + (definition.enabledThresholds().contains(threshold) ? "" : " (disabled)"));
        return List.copyOf(lines);
    }
    private void invalidateProgress(UUID player) {
        synchronized (placeholderCache) {
            placeholderCache.entrySet().removeIf(entry -> player.equals(entry.getValue().storageId()));
            // Unresolved names have not read the ledger yet; their later read
            // will observe this mutation. Once resolved, fence matching reads.
            placeholderRequests.values().forEach(request -> {
                if (player.equals(request.resolvedStorageId)) request.invalidated = true;
            });
        }
    }

    /** Nonblocking projection using a previously captured storage identity. */
    public String placeholder(UUID player, String request) {
        return placeholder(player, null, request);
    }

    /** Named offline identity is resolved only on the persistence worker. */
    public String placeholder(UUID player, java.util.function.Supplier<String> offlineName, String request) {
        if (player == null || request == null || request.length() > 100 || !plugin.isEnabled()) return "unavailable";
        PlaceholderIdentity identity = new PlaceholderIdentity(player, offlineName != null);
        int split = request.indexOf('_');
        if (split < 1) return "invalid";
        String metric = request.substring(0, split).toLowerCase(java.util.Locale.ROOT);
        String id = request.substring(split + 1);
        int threshold = -1;
        if (metric.equals("award")) {
            split = id.indexOf('_');
            if (split < 1 || !id.substring(0, split).matches("[1-9][0-9]{0,3}")) return "invalid";
            threshold = Integer.parseInt(id.substring(0, split)); id = id.substring(split + 1);
        } else if (!List.of("votes", "next", "status").contains(metric)) return "invalid";
        String eventId = id;
        Definition definition = definitions.stream().filter(d -> d.event().id().equals(eventId)).findFirst().orElse(null);
        if (definition == null) return "invalid";
        DateVoteEvent event = definition.event();
        if (metric.equals("status")) return !event.enabled() ? "disabled" : System.currentTimeMillis() < event.start()
                ? "upcoming" : System.currentTimeMillis() >= event.end() ? "ended" : "active";
        if (metric.equals("award") && !event.thresholds().contains(threshold)) return "invalid";
        if (plugin.getBungeeSettings().isUseBungeecoord() && !event.accountingServer().equals(plugin.getBungeeSettings().getServer()))
            return "remote";
        Object epoch = generation;
        if (epoch == null) return "unavailable";
        CachedProgress cached;
        ProgressRequest launch = null;
        synchronized (placeholderCache) {
            cached = placeholderCache.get(identity);
            if (cached == null || cached.generation() != epoch || System.nanoTime() >= cached.expires()) {
                cached = null;
                if (!placeholderRequests.containsKey(identity)) {
                    if (placeholderRequests.size() >= 64) return "unavailable";
                    launch = new ProgressRequest();
                    if (!identity.resolveName()) launch.resolvedStorageId = player;
                    placeholderRequests.put(identity, launch);
                }
            }
        }
        if (launch != null && !refreshPlaceholder(identity, offlineName, epoch, definitions, launch)) return "unavailable";
        if (cached == null) return "loading";
        var progress = cached.events().get(eventId);
        if (progress == null) return "unavailable";
        if (metric.equals("votes")) return Integer.toString(progress.votes());
        if (metric.equals("next")) return event.thresholds().stream().filter(n -> n > progress.votes()
                && definition.enabledThresholds().contains(n)).findFirst().map(Object::toString).orElse("0");
        return progress.reservedAwards().contains(threshold) ? "pending_review" : progress.submittedAwards().contains(threshold)
                ? "submitted" : progress.deferredAwards().contains(threshold) ? "deferred" : "not_reached";
    }
    private boolean refreshPlaceholder(PlaceholderIdentity identity, java.util.function.Supplier<String> offlineName, Object epoch, List<Definition> snapshot, ProgressRequest request) {
        try {
            plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                try {
                    if (!current(epoch)) return;
                    UUID storageId;
                    try {
                        String name = offlineName == null ? null : offlineName.get();
                        storageId = name == null || name.isBlank() ? identity.player()
                                : plugin.getVotingPluginUserManager().getVotingPluginUser(identity.player(), name).getJavaUUID();
                        if (storageId == null) throw new IllegalStateException("Missing storage identity");
                        synchronized (placeholderCache) {
                            if (!current(epoch) || request.invalidated || placeholderRequests.get(identity) != request) return;
                            request.resolvedStorageId = storageId;
                        }
                    } catch (RuntimeException unavailable) {
                        synchronized (placeholderCache) {
                            if (current(epoch) && !request.invalidated && placeholderRequests.get(identity) == request) {
                                if (placeholderCache.size() >= 256) placeholderCache.remove(placeholderCache.keySet().iterator().next());
                                placeholderCache.put(identity, new CachedProgress(epoch, System.nanoTime() + 1_000_000_000L, null, java.util.Map.of()));
                            }
                        }
                        return;
                    }
                    java.util.Map<String, DateVoteLedger.Progress> results = new HashMap<>();
                    for (Definition definition : snapshot) {
                        var event = definition.event();
                        if (plugin.getBungeeSettings().isUseBungeecoord()
                                && !event.accountingServer().equals(plugin.getBungeeSettings().getServer())) continue;
                        try { results.put(event.id(), ledger().progress(event, storageId)); }
                        catch (IOException | RuntimeException unavailable) { /* Missing entry means unavailable, never zero. */ }
                    }
                    synchronized (placeholderCache) {
                        if (!current(epoch) || request.invalidated || placeholderRequests.get(identity) != request) return;
                        if (placeholderCache.size() >= 256) placeholderCache.remove(placeholderCache.keySet().iterator().next());
                        placeholderCache.put(identity, new CachedProgress(epoch, System.nanoTime() + 5_000_000_000L, storageId, java.util.Map.copyOf(results)));
                    }
                } finally { synchronized (placeholderCache) { placeholderRequests.remove(identity, request); } }
            });
            return true;
        } catch (RuntimeException unavailable) {
            synchronized (placeholderCache) {
                if (current(epoch) && placeholderRequests.get(identity) == request) {
                    if (placeholderCache.size() >= 256) placeholderCache.remove(placeholderCache.keySet().iterator().next());
                    placeholderCache.put(identity, new CachedProgress(epoch, System.nanoTime() + 1_000_000_000L, null, java.util.Map.of()));
                }
                placeholderRequests.remove(identity, request);
            }
            return false;
        }
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
