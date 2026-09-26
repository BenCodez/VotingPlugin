package com.bencodez.votingplugin.neoforge;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.spongepowered.configurate.ConfigurationNode;

import com.bencodez.advancedcore.api.time.TimeCalculation;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

/**
 * Immutable NeoForge view of the small configuration subset needed by native
 * accepted-vote processing. It deliberately does not load rewards or Bukkit
 * configuration objects.
 */
public final class NeoForgeVoteConfiguration {
    private final boolean allowUnjoined;
    private final boolean allowUnjoinedCheckServer;
    private final boolean addTotals;
    private final boolean addTotalsOffline;
    private final boolean countFakeVotes;
    private final boolean processRewards;
    private final int pointsOnVote;
    private final int limitVotePoints;
    private final boolean offlineVoteLimitEnabled;
    private final int offlineVoteLimitAmount;
    private final int timeHourOffset;
    private final String timeZone;
    private final List<NeoForgeVoteSite> voteSites;

    private NeoForgeVoteConfiguration(ConfigurationNode config, boolean ignoreCase,
            List<NeoForgeVoteSite> voteSites) {
        allowUnjoined = node(config, ignoreCase, "AllowUnjoined").getBoolean(false);
        allowUnjoinedCheckServer = node(config, ignoreCase, "AllowUnJoinedCheckServer").getBoolean(true);
        addTotals = node(config, ignoreCase, "AddTotals").getBoolean(true);
        addTotalsOffline = node(config, ignoreCase, "AddTotalsOffline").getBoolean(true);
        countFakeVotes = node(config, ignoreCase, "CountFakeVotes").getBoolean(true);
        processRewards = node(config, ignoreCase, "ProcessRewards").getBoolean(true);
        pointsOnVote = node(config, ignoreCase, "PointsOnVote").getInt(1);
        limitVotePoints = node(config, ignoreCase, "LimitVotePoints").getInt(-1);
        offlineVoteLimitEnabled = node(config, ignoreCase, "OfflineVotesLimit", "Enabled").getBoolean(false);
        offlineVoteLimitAmount = node(config, ignoreCase, "OfflineVotesLimit", "Amount").getInt(5);
        timeHourOffset = node(config, ignoreCase, "TimeHourOffSet").getInt(0);
        timeZone = node(config, ignoreCase, "TimeZone").getString("");
        this.voteSites = List.copyOf(voteSites);
    }

    static NeoForgeVoteConfiguration load(ConfigurationNode config, ConfigurationNode voteSites) {
        Objects.requireNonNull(config, "config");
        Objects.requireNonNull(voteSites, "voteSites");
        boolean ignoreCase = config.node("CaseInsensitiveYMLFiles").getBoolean(false);
        List<NeoForgeVoteSite> parsedSites = new ArrayList<>();
        for (Map.Entry<Object, ? extends ConfigurationNode> entry
                : node(voteSites, ignoreCase, "VoteSites").childrenMap().entrySet()) {
            String key = String.valueOf(entry.getKey());
            if (key.equalsIgnoreCase("null")) continue;
            ConfigurationNode site = entry.getValue();
            String displayName = node(site, ignoreCase, "Name").getString(key);
            if (displayName == null || displayName.isEmpty()) displayName = key;
            String serviceSite = node(site, ignoreCase, "ServiceSite").getString();
            boolean enabled = node(site, ignoreCase, "Enabled").getBoolean(false);
            boolean loadable = enabled && serviceSite != null && !serviceSite.isEmpty();
            parsedSites.add(new NeoForgeVoteSite(key, displayName, serviceSite,
                    node(site, ignoreCase, "Priority").getInt(0),
                    loadable ? readVoteDelayMillis(site, ignoreCase) : 0,
                    enabled, node(site, ignoreCase, "WaitUntilVoteDelay").getBoolean(false),
                    node(site, ignoreCase, "VoteDelayDaily").getBoolean(false),
                    node(site, ignoreCase, "VoteDelayDailyHour").getInt(0),
                    node(site, ignoreCase, "ForceOffline").getBoolean(
                            node(site, ignoreCase, "GiveOffline").getBoolean(false))));
        }
        parsedSites.sort(Comparator.comparingInt(NeoForgeVoteSite::priority).reversed());
        return new NeoForgeVoteConfiguration(config, ignoreCase, parsedSites);
    }

    private static long readVoteDelayMillis(ConfigurationNode site, boolean ignoreCase) {
        Object value = node(site, ignoreCase, "VoteDelay").rawScalar();
        if (value instanceof String text) {
            return ParsedDuration.parse(text, TimeUnit.HOURS).getMillis();
        }
        double hours = node(site, ignoreCase, "VoteDelay").getDouble(0);
        double minutes = node(site, ignoreCase, "VoteDelayMin").getDouble(0);
        return (long) (hours * TimeUnit.HOURS.toMillis(1))
                + (long) (minutes * TimeUnit.MINUTES.toMillis(1));
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

    /** Resolves only configured sites that Bukkit would load for accepted votes. */
    public Optional<NeoForgeVoteSite> resolveEnabledSite(String identifier) {
        if (!ServiceSiteValidator.isValid(identifier)) return Optional.empty();
        return voteSites.stream().filter(NeoForgeVoteSite::canResolveIncomingVote)
                .filter(site -> site.matches(identifier)).findFirst();
    }

    boolean hasEnabledSiteKey(String key) {
        if (key == null) return false;
        return voteSites.stream().filter(NeoForgeVoteSite::canResolveIncomingVote)
                .anyMatch(site -> site.key().equalsIgnoreCase(key));
    }

    public SharedVotePolicy policyFor(NeoForgeVoteSite site) {
        Objects.requireNonNull(site, "site");
        return new SharedVotePolicy(countFakeVotes, addTotals, addTotalsOffline,
                processRewards, site.giveOfflineRewards());
    }

    public boolean allowUnjoined() { return allowUnjoined; }
    public boolean allowUnjoinedCheckServer() { return allowUnjoinedCheckServer; }
    public boolean addTotals() { return addTotals; }
    public boolean addTotalsOffline() { return addTotalsOffline; }
    public boolean countFakeVotes() { return countFakeVotes; }
    public boolean processRewards() { return processRewards; }
    public int pointsOnVote() { return pointsOnVote; }
    public int limitVotePoints() { return limitVotePoints; }
    public boolean offlineVoteLimitEnabled() { return offlineVoteLimitEnabled; }
    public int offlineVoteLimitAmount() { return offlineVoteLimitAmount; }
    public int timeHourOffset() { return timeHourOffset; }
    LocalDateTime currentTime(Clock clock) {
        try {
            return TimeCalculation.currentTime(clock, timeZone, timeHourOffset);
        } catch (RuntimeException invalidTimeZone) {
            return TimeCalculation.currentTime(clock, "", timeHourOffset);
        }
    }
    public List<NeoForgeVoteSite> voteSites() { return voteSites; }
}
