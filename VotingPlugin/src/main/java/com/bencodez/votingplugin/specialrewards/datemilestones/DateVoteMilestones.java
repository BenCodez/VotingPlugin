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
import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Owner-defined windows, independently accounted on one backend using original occurrence IDs. */
public final class DateVoteMilestones {
    private record Definition(DateVoteEvent event, YamlConfiguration rewards) { }
    private final VotingPluginMain plugin;
    private volatile List<Definition> definitions = List.of();
    private final java.util.Set<UUID> pendingProgress = new java.util.HashSet<>();
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
                loaded.add(new Definition(event, rewards));
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
        int count = 0;
        for (String key : milestones.getKeys(false)) {
            if (++count > 64) break;
            int threshold;
            try { threshold = Integer.parseInt(key); }
            catch (NumberFormatException invalid) { continue; }
            if (threshold <= 0 || threshold > 4096 || !key.equals(Integer.toString(threshold))) continue;
            String source = "DateVoteMilestones." + id + ".Milestones." + threshold + ".Rewards";
            var configured = rewards.getConfigurationSection(source);
            if (configured == null) continue;
            String alias = "DateVoteMilestonesRuntime." + fileId + ".Milestones." + threshold + ".Rewards";
            rewards.createSection(alias, configured.getValues(true));
            plugin.addDirectlyDefinedRewards(new DirectlyDefinedReward(alias) {
                private String sourcePath(String requested) {
                    if (!requested.equals(alias) && !requested.startsWith(alias + "."))
                        throw new IllegalArgumentException("Unexpected date milestone reward path");
                    return source + requested.substring(alias.length());
                }
                @Override public ConfigurationSection getFileData() {
                    YamlConfiguration data = new YamlConfiguration();
                    var current = plugin.getSpecialRewardsConfig().getData().getConfigurationSection(source);
                    if (current != null) data.createSection(alias, current.getValues(true));
                    return data;
                }
                @Override public void createSection(String path) { plugin.getSpecialRewardsConfig().createSection(sourcePath(path)); }
                @Override public void setData(String path, Object value) { plugin.getSpecialRewardsConfig().setValue(sourcePath(path), value); }
                @Override public void save() { plugin.getSpecialRewardsConfig().saveData(); }
            });
        }
    }
    public static DateVoteEvent parse(String id, ConfigurationSection section) {
        String zone = section.getString("Timezone");
        String start = section.getString("Start"), end = section.getString("End");
        var milestones = section.getConfigurationSection("Milestones");
        if (zone == null || start == null || end == null || milestones == null)
            throw new IllegalArgumentException("Start, End, Timezone and Milestones are required");
        List<Integer> thresholds = new ArrayList<>();
        for (String key : milestones.getKeys(false)) {
            int threshold = Integer.parseInt(key);
            if (!key.equals(Integer.toString(threshold)) || milestones.getConfigurationSection(key + ".Rewards") == null)
                throw new IllegalArgumentException("Each numeric milestone must contain Rewards");
            thresholds.add(threshold);
        }
        return new DateVoteEvent(id, section.getString("DisplayName", id), section.getBoolean("Enabled", false),
                DateVoteEvent.timestamp(start, zone), DateVoteEvent.timestamp(end, zone), zone, thresholds,
                new java.util.HashSet<>(section.getStringList("VoteSites")), section.getString("AccountingServer", ""));
    }
    static String path(DateVoteEvent event, int threshold) {
        return "DateVoteMilestonesRuntime." + event.fileId() + ".Milestones." + threshold + ".Rewards";
    }
    private static String configPath(DateVoteEvent event, int threshold) {
        return "DateVoteMilestones." + event.id() + ".Milestones." + threshold + ".Rewards";
    }
    /** Called only from the real asynchronous accepted-vote pipeline; never from navigation. */
    public void accepted(VotingPluginUser user, String site, UUID occurrence, long occurredAt,
            boolean real, boolean proxy, boolean canonicalProxy, boolean targeted, boolean forceProxyRouting) {
        if (plugin.getBungeeSettings().isUseBungeecoord() && !proxy) return;
        if (!real || occurrence == null || occurredAt <= 0 || proxy && (!canonicalProxy || targeted)) return;
        for (Definition definition : definitions) {
            DateVoteEvent event = definition.event();
            if (!event.matches(occurredAt, site, real, proxy, plugin.getBungeeSettings().getServer())) continue;
            try {
                for (int threshold : ledger().record(event, user.getJavaUUID(), occurrence, plugin.getOptions().isProcessRewards())) {
                    if (!plugin.getOptions().isProcessRewards()) break;
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
    }
    public void progress(Player player) {
        BukkitCompletionScheduler.run(plugin, player, () -> {
            UUID uuid = player.getUniqueId();
            String playerName = player.getName();
            if (!player.hasPermission("VotingPlugin.Commands.Vote.DateEvents") && !player.hasPermission("VotingPlugin.Player")) return;
            synchronized (pendingProgress) {
                if (pendingProgress.contains(uuid) || pendingProgress.size() >= 64) {
                    player.sendMessage("Date voting progress is already being checked or busy. Try again shortly."); return;
                }
                pendingProgress.add(uuid);
            }
            List<Definition> snapshot = definitions;
            try {
                plugin.getTimer().execute(() -> {
                    try {
                    if (snapshot != definitions || !plugin.isEnabled()) return;
                    UUID storageId = snapshot.isEmpty() ? uuid
                            : plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, playerName).getJavaUUID();
                    List<String> messages = new ArrayList<>();
                    if (snapshot.stream().noneMatch(d -> d.event().enabled())) messages.add("No date voting events are enabled.");
                    for (Definition definition : snapshot) {
                        var event = definition.event();
                        if (!event.enabled()) continue;
                        if (plugin.getBungeeSettings().isUseBungeecoord() && !event.accountingServer().equals(plugin.getBungeeSettings().getServer())) {
                            messages.add(event.displayName() + ": progress is owned by backend " + event.accountingServer()); continue;
                        }
                        try {
                            var progress = ledger().progress(event, storageId);
                            long now = System.currentTimeMillis();
                            String state = now < event.start() ? "upcoming" : now >= event.end() ? "ended" : "active";
                            messages.add(event.displayName() + " (" + state + "): " + progress.votes() + " votes; milestones " + event.thresholds()
                                    + "; submitted " + progress.submittedAwards() + "; deferred " + progress.deferredAwards() + "; pending review " + progress.reservedAwards());
                        } catch (IOException failure) { messages.add(event.displayName() + ": progress unavailable; ask an administrator."); }
                    }
                    BukkitCompletionScheduler.run(plugin, player, () -> {
                        if (player.isOnline() && plugin.isEnabled() && snapshot == definitions
                                && (player.hasPermission("VotingPlugin.Commands.Vote.DateEvents") || player.hasPermission("VotingPlugin.Player")))
                            messages.forEach(player::sendMessage);
                    }, () -> { }, () -> { });
                    } catch (RuntimeException unavailable) {
                        BukkitCompletionScheduler.run(plugin, player, () -> {
                            if (player.isOnline() && plugin.isEnabled() && snapshot == definitions
                                    && (player.hasPermission("VotingPlugin.Commands.Vote.DateEvents") || player.hasPermission("VotingPlugin.Player")))
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
}
