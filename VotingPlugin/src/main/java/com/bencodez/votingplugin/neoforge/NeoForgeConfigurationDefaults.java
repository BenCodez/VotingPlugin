package com.bencodez.votingplugin.neoforge;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

/** Derives missing-file defaults from the shared Bukkit templates, not a second schema. */
final class NeoForgeConfigurationDefaults {
    private NeoForgeConfigurationDefaults() { }

    static byte[] fromShared(String name, byte[] template) throws IOException {
        if (!List.of("Config.yml", "VoteSites.yml", "SpecialRewards.yml").contains(name)) return template;
        var node = YamlConfigurationLoader.builder().source(() -> new BufferedReader(
                new StringReader(new String(template, StandardCharsets.UTF_8)))).build().load();
        switch (name) {
            case "Config.yml" -> {
                for (String key : List.of("CloseInventoryOnVote", "UseVoteStreaks", "PerSiteCoolDownEvents",
                        "LimitMonthlyVotes")) node.node(key).set(false);
                node.node("OfflineVotesLimit", "Enabled").set(false);
                node.node("VoteBroadcast", "Type").set("NONE");
            }
            case "SpecialRewards.yml" -> {
                node.node("VoteParty", "Enabled").set(false);
                for (String key : List.of("AnySiteRewards", "VoteCoolDownEndedReward", "VoteMilestones",
                        "VoteStreaks", "VoteStreak", "Cumulative", "MileStones", "FirstVote",
                        "FirstVoteToday", "AllSites", "AlmostAllSites")) node.node(key).set(java.util.Map.of());
            }
            case "VoteSites.yml" -> {
                // The shared examples combine unsupported actions. Keep their site settings,
                // but offer only one supported action, without opting into offline execution.
                for (var site : node.node("VoteSites").childrenMap().values()) {
                    site.node("ForceOffline").set(false);
                    site.node("GiveOffline").set(false);
                    site.node("Rewards").set(java.util.Map.of("Commands", List.of("say %player% voted")));
                }
            }
            default -> throw new IllegalStateException("Unexpected default template");
        }
        StringWriter rendered = new StringWriter();
        YamlConfigurationLoader.builder().sink(() -> new BufferedWriter(rendered)).build().save(node);
        return rendered.toString().getBytes(StandardCharsets.UTF_8);
    }
}
