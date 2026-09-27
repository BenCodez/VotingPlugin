package com.bencodez.votingplugin.neoforge;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;

import org.spongepowered.configurate.ConfigurationNode;

/** Parses the first NeoForge-safe subset of existing VotingPlugin reward YAML. */
final class NeoForgeRewardConfiguration {
    private static final Pattern UNRESOLVED_PLACEHOLDER = Pattern.compile("%[^%]+%");
    private static final Pattern LEGACY_COLOR = Pattern.compile("(?i)(?:&[0-9A-FK-ORX]|&#[0-9A-F]{6}|\\u00A7)");
    private final ConfigurationNode config;
    private final ConfigurationNode voteSites;
    private final ConfigurationNode specialRewards;
    private final boolean ignoreCase;

    NeoForgeRewardConfiguration(ConfigurationNode config, ConfigurationNode voteSites,
            ConfigurationNode specialRewards) {
        this.config = Objects.requireNonNull(config, "config");
        this.voteSites = Objects.requireNonNull(voteSites, "voteSites");
        this.specialRewards = Objects.requireNonNull(specialRewards, "specialRewards");
        ignoreCase = node(config, false, "CaseInsensitiveYMLFiles").getBoolean(false);
    }

    NeoForgeRewardPlan plan(NeoForgeDeferredVote vote, NeoForgeVoteSite site, boolean online) {
        String globalBlocker = unsupportedGlobalBehavior();
        if (globalBlocker != null) return blocked(globalBlocker);
        if (site.waitUntilVoteDelay()) {
            return blocked("WaitUntilVoteDelay completion is not supported by NeoForge replay yet");
        }
        if (!node(config, ignoreCase, "ProcessRewards").getBoolean(true)) {
            return blocked("ProcessRewards is disabled; retained vote remains pending");
        }

        ArrayList<NeoForgeRewardPlan.Action> actions = new ArrayList<>();
        String unsupported = appendReward(node(voteSites, ignoreCase, "EverySiteReward"), actions,
                "EverySiteReward");
        if (unsupported == null) {
            unsupported = appendReward(node(voteSites, ignoreCase, "VoteSites", site.key(), "Rewards"),
                    actions, "VoteSites." + site.key() + ".Rewards");
        }
        if (unsupported != null) return blocked(unsupported);
        actions.replaceAll(action -> new NeoForgeRewardPlan.Action(action.type(), action.value()
                .replace("%SiteName%", site.displayName()).replace("%sitename%", site.displayName())));
        boolean hasMessages = actions.stream().anyMatch(action ->
                action.type() == NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE);
        boolean hasCommands = actions.stream().anyMatch(action ->
                action.type() == NeoForgeRewardPlan.ActionType.CONSOLE_COMMAND);
        if (hasMessages && hasCommands) {
            return blocked("Mixed command and player-message rewards cannot yet be retried safely");
        }
        if (actions.size() > 1) {
            return blocked("Multiple external reward actions cannot yet be retried safely");
        }
        boolean needsPlayer = hasMessages;
        if (!online && (needsPlayer || (!actions.isEmpty() && !site.giveOfflineRewards()))) {
            return new NeoForgeRewardPlan(NeoForgeRewardPlan.Status.WAITING_FOR_PLAYER, actions, true,
                    "Player is offline; retained reward remains pending");
        }
        return new NeoForgeRewardPlan(NeoForgeRewardPlan.Status.READY, actions,
                needsPlayer || (!actions.isEmpty() && !site.giveOfflineRewards()),
                "Supported reward configuration is ready");
    }

    private String unsupportedGlobalBehavior() {
        String broadcast = node(config, ignoreCase, "VoteBroadcast", "Type").getString("EVERY_VOTE");
        if (!"NONE".equalsIgnoreCase(broadcast)) return "VoteBroadcast is not supported by NeoForge replay yet";
        if (node(config, ignoreCase, "PerSiteCoolDownEvents").getBoolean(false)) {
            return "PerSiteCoolDownEvents is not supported by NeoForge replay yet";
        }
        if (node(config, ignoreCase, "UseVoteStreaks").getBoolean(true)) {
            return "Vote streak processing is not supported by NeoForge replay yet";
        }
        if (node(config, ignoreCase, "LimitMonthlyVotes").getBoolean(false)) {
            return "LimitMonthlyVotes is not supported by NeoForge replay yet";
        }
        if (node(config, ignoreCase, "OfflineVotesLimit", "Enabled").getBoolean(false)) {
            return "OfflineVotesLimit is not supported by NeoForge replay yet";
        }
        if (node(specialRewards, ignoreCase, "VoteParty", "Enabled").getBoolean(false)) {
            return "VoteParty is not supported by NeoForge replay yet";
        }
        if (!node(specialRewards, ignoreCase, "AnySiteRewards").empty()) {
            return "AnySiteRewards are not supported by NeoForge replay yet";
        }
        if (!node(specialRewards, ignoreCase, "VoteCoolDownEndedReward").empty()) {
            return "Vote cooldown rewards are not supported by NeoForge replay yet";
        }
        if (hasEnabledChild(node(specialRewards, ignoreCase, "VoteMilestones"))) {
            return "VoteMilestones are not supported by NeoForge replay yet";
        }
        if (hasEnabledRewardDefinition(node(specialRewards, ignoreCase, "VoteStreaks"))
                || hasEnabledRewardDefinition(node(specialRewards, ignoreCase, "VoteStreak"))) {
            return "Vote streak rewards are not supported by NeoForge replay yet";
        }
        if (hasEnabledChild(node(specialRewards, ignoreCase, "Cumulative"))
                || hasEnabledChild(node(specialRewards, ignoreCase, "MileStones"))) {
            return "Legacy milestone rewards are not supported by NeoForge replay yet";
        }
        if (!node(specialRewards, ignoreCase, "FirstVote").empty()
                || !node(specialRewards, ignoreCase, "FirstVoteToday").empty()) {
            return "First-vote rewards are not supported by NeoForge replay yet";
        }
        if (!node(specialRewards, ignoreCase, "AllSites").empty()
                || !node(specialRewards, ignoreCase, "AlmostAllSites").empty()) {
            return "Legacy all-sites rewards are not supported by NeoForge replay yet";
        }
        return null;
    }

    private String appendReward(ConfigurationNode reward, List<NeoForgeRewardPlan.Action> actions, String path) {
        if (reward.empty()) return null;
        if (reward.isList()) {
            return reward.childrenList().isEmpty() ? null : path + " named reward lists are unsupported";
        }
        if (reward.childrenMap().isEmpty()) return path + " has an unsupported value";
        for (Map.Entry<Object, ? extends ConfigurationNode> entry : reward.childrenMap().entrySet()) {
            String key = String.valueOf(entry.getKey());
            if (!matchesKey(key, "Commands") && !matchesKey(key, "Messages"))
                return path + "." + key + " is unsupported";
        }
        ConfigurationNode messageNode = node(reward, ignoreCase, "Messages");
        if (!messageNode.empty()) {
            if (messageNode.childrenMap().isEmpty()) return path + ".Messages has an unsupported value";
            for (Map.Entry<Object, ? extends ConfigurationNode> message : messageNode.childrenMap().entrySet()) {
                if (!matchesKey(String.valueOf(message.getKey()), "Player"))
                    return path + ".Messages." + message.getKey() + " is unsupported";
                if (!appendActions(message.getValue(), actions, NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE))
                    return path + ".Messages.Player has an unsupported value";
            }
        }
        ConfigurationNode commands = node(reward, ignoreCase, "Commands");
        if (!commands.empty()
                && !appendActions(commands, actions, NeoForgeRewardPlan.ActionType.CONSOLE_COMMAND))
            return path + ".Commands has an unsupported value";
        return null;
    }

    private boolean matchesKey(String actual, String expected) {
        return ignoreCase ? actual.equalsIgnoreCase(expected) : actual.equals(expected);
    }

    private static boolean appendActions(ConfigurationNode node, List<NeoForgeRewardPlan.Action> output,
            NeoForgeRewardPlan.ActionType type) {
        if (node.isList()) {
            for (ConfigurationNode child : node.childrenList()) {
                String value = child.getString();
                if (value == null) return false;
                if (type == NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE && value.isEmpty()) continue;
                if (!supportedValue(value, type)) return false;
                output.add(new NeoForgeRewardPlan.Action(type, value));
            }
            return true;
        }
        String value = node.getString();
        if (value == null) return node.empty();
        if (type == NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE && value.isEmpty()) return true;
        if (!supportedValue(value, type)) return false;
        output.add(new NeoForgeRewardPlan.Action(type, value));
        return true;
    }

    private static boolean supportedValue(String value, NeoForgeRewardPlan.ActionType type) {
        String knownRemoved = value.replace("%player%", "").replace("%Player%", "")
                .replace("%ServiceSite%", "").replace("%servicesite%", "")
                .replace("%SiteName%", "").replace("%sitename%", "");
        if (UNRESOLVED_PLACEHOLDER.matcher(knownRemoved).find()
                || knownRemoved.toLowerCase(java.util.Locale.ROOT).contains("[javascript")) return false;
        return type != NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE
                || !LEGACY_COLOR.matcher(value).find();
    }

    private boolean hasEnabledChild(ConfigurationNode parent) {
        return parent.childrenMap().values().stream()
                .anyMatch(child -> node(child, ignoreCase, "Enabled").getBoolean(true));
    }

    private boolean hasEnabledRewardDefinition(ConfigurationNode parent) {
        if (!node(parent, ignoreCase, "Rewards").empty()
                && node(parent, ignoreCase, "Enabled").getBoolean(true)) return true;
        return parent.childrenMap().values().stream().anyMatch(this::hasEnabledRewardDefinition);
    }

    private static NeoForgeRewardPlan blocked(String detail) {
        return new NeoForgeRewardPlan(NeoForgeRewardPlan.Status.BLOCKED_UNSUPPORTED, List.of(), false, detail);
    }

    private static ConfigurationNode node(ConfigurationNode parent, boolean ignoreCase, String... path) {
        ConfigurationNode current = parent;
        for (String key : path) {
            if (ignoreCase) {
                ConfigurationNode match = current.childrenMap().entrySet().stream()
                        .filter(entry -> String.valueOf(entry.getKey()).equalsIgnoreCase(key))
                        .map(Map.Entry::getValue).findFirst().orElse(null);
                if (match != null) {
                    current = match;
                    continue;
                }
            }
            current = current.node(key);
        }
        return current;
    }
}
