package com.bencodez.votingplugin.specialrewards.datemilestones;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Pattern;
import org.bukkit.configuration.ConfigurationSection;

/** Bounded owner configuration; persistent award identities remain the numeric thresholds. */
final class DateVoteMilestoneConfig {
    record Reward(String key, boolean enabled) { }
    private static final Pattern RANGE = Pattern.compile("(\\d+)\\s*\\.\\.\\s*(\\d+)(?:\\s+step\\s+(\\d+))?");
    private DateVoteMilestoneConfig() { }

    static Map<Integer, Reward> read(ConfigurationSection event) {
        var section = event.getConfigurationSection("Milestones");
        if (section == null || section.getKeys(false).isEmpty() || section.getKeys(false).size() > 64)
            throw new IllegalArgumentException("Milestones requires 1–64 entries");
        Map<Integer, Reward> result = new LinkedHashMap<>();
        for (String key : section.getKeys(false)) {
            var milestone = section.getConfigurationSection(key);
            for (var entry : entry(key, milestone).entrySet()) {
                if (result.putIfAbsent(entry.getKey(), entry.getValue()) != null)
                    throw new IllegalArgumentException("Milestone thresholds must not overlap between entries");
                if (result.size() > 64) throw new IllegalArgumentException("At most 64 thresholds per event");
            }
        }
        return result;
    }

    static Map<Integer, Reward> entry(String key, ConfigurationSection section) {
        if (!key.matches("[A-Za-z0-9_-]{1,64}") || section == null
                || section.getConfigurationSection("Rewards") == null)
            throw new IllegalArgumentException("Each milestone requires an ID and Rewards section");
        if (section.contains("Enabled") && !(section.get("Enabled") instanceof Boolean))
            throw new IllegalArgumentException("Milestone Enabled must be a boolean");
        for (String unsupported : List.of("Total", "Group", "Limit")) {
            if (section.contains(unsupported))
                throw new IllegalArgumentException(unsupported + " is not supported for date milestones");
        }
        TreeSet<Integer> thresholds = new TreeSet<>();
        if (!section.contains("At") && !section.contains("Every")) {
            int value = integer(key, false);
            if (!key.equals(Integer.toString(value))) throw new IllegalArgumentException("Legacy threshold must be canonical");
            thresholds.add(value);
        } else {
            if (section.contains("At")) {
                Object raw = section.get("At");
                List<?> values = raw instanceof List<?> list ? list : java.util.Collections.singletonList(raw);
                if (values.isEmpty() || values.size() > 64) throw new IllegalArgumentException("At requires 1–64 values");
                for (Object value : values) addAt(thresholds, value);
            }
            if (section.contains("Every")) {
                int every = integer(section.get("Every"), false);
                for (int value = every; value <= 4096; value += every) add(thresholds, value);
            }
        }
        Map<Integer, Reward> result = new LinkedHashMap<>();
        for (int value : thresholds) result.put(value, new Reward(key, section.getBoolean("Enabled", true)));
        return result;
    }

    private static void addAt(TreeSet<Integer> values, Object raw) {
        if (raw instanceof String text) {
            var range = RANGE.matcher(text.trim());
            if (range.matches()) {
                int start = integer(range.group(1), true), end = integer(range.group(2), true);
                int step = range.group(3) == null ? 1 : integer(range.group(3), false);
                int first = Math.min(start, end), last = Math.max(start, end);
                for (int value = first; value <= last; value += step) add(values, value == 0 ? 1 : value);
                return;
            }
        }
        int value = integer(raw, true);
        add(values, value == 0 ? 1 : value);
    }

    private static void add(TreeSet<Integer> values, int value) {
        values.add(value);
        if (values.size() > 64) throw new IllegalArgumentException("At most 64 thresholds per event");
    }

    private static int integer(Object raw, boolean zero) {
        // Avoid Number.intValue truncation and YAML's boolean/string-list coercions.
        if (!(raw instanceof Byte || raw instanceof Short || raw instanceof Integer || raw instanceof Long || raw instanceof String))
            throw new IllegalArgumentException("Threshold must be an integer");
        String text = raw.toString().trim();
        if (!text.matches("\\d{1,4}")) throw new IllegalArgumentException("Threshold must be an integer from 1 to 4096");
        int value = Integer.parseInt(text);
        if (value < (zero ? 0 : 1) || value > 4096) throw new IllegalArgumentException("Threshold out of range");
        return value;
    }
}
