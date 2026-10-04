package com.bencodez.votingplugin.control;

import org.bukkit.configuration.file.FileConfiguration;

import com.google.gson.JsonObject;

/** Small fixed allow-list of non-secret config facts for Network Doctor. */
final class NetworkHealthConfigFacts {
    private NetworkHealthConfigFacts() { }

    static void addBackend(JsonObject out, FileConfiguration config) {
        addBoolean(out, config, "onlineMode", "OnlineMode");
        addString(out, config, "bedrockPlayerPrefix", "BedrockPlayerPrefix", 80);
        addInt(out, config, "timeHourOffset", "TimeHourOffSet");
        addBoolean(out, config, "resetMilestonesMonthly", "ResetMilestonesMonthly");
        addBoolean(out, config, "monthDateTotals", "UseMonthDateTotalsAsPrimaryTotal");
        addBoolean(out, config, "extraAllSitesCheck", "ExtraAllSitesCheck");
        addBoolean(out, config, "allowUnjoined", "AllowUnjoined");
        addInt(out, config, "maxVotesPerDay", "MaxAmountOfVotesPerDay");
        addBoolean(out, config, "votePartyEnabled", "VoteParty.Enabled");
        addInt(out, config, "votePartyVotesRequired", "VoteParty.VotesRequired");
    }

    static void addGlobal(JsonObject out, FileConfiguration config) {
        addString(out, config, "globalDataPrefix", "GlobalData.Prefix", 80);
        addBoolean(out, config, "globalDataUseMainMysql", "GlobalData.UseMainMySQL");
    }

    static void addDelay(JsonObject out, Object rawDelay, Object rawMinutes, com.bencodez.simpleapi.time.ParsedDuration parsed) {
        boolean valid = rawDelay == null || rawDelay instanceof String || rawDelay instanceof Number;
        valid &= rawMinutes == null || rawMinutes instanceof Number;
        valid &= !(rawDelay instanceof Number n) || Double.isFinite(n.doubleValue()) && n.doubleValue() >= 0;
        valid &= !(rawMinutes instanceof Number n) || Double.isFinite(n.doubleValue()) && n.doubleValue() >= 0;
        if (rawDelay instanceof String text && (parsed == null || parsed.isEmpty()))
            valid &= text.trim().matches("(?i)0+(?:\\.0+)?(?:\\s*(?:ms|s|m|h|d|w))?");
        out.addProperty("delayValid", valid && parsed != null);
        if (valid && parsed != null) {
            long millis = parsed.getMillis();
            long hours = millis / 3_600_000L + (millis % 3_600_000L == 0 ? 0 : 1);
            if (hours >= 0 && hours <= 1_000_000L) out.addProperty("delayHours", hours);
        }
    }

    static void addObservedServices(JsonObject out, Iterable<String> observations) {
        java.util.Set<String> names = new java.util.TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        if (observations == null) return;
        for (String name : observations) {
            if (name == null || name.isBlank() || name.length() > 80 || name.codePoints().anyMatch(Character::isISOControl)) return;
            names.add(name);
            if (names.size() > 100) return;
        }
        com.google.gson.JsonArray values = new com.google.gson.JsonArray();
        names.forEach(values::add); out.add("detectedServices", values);
    }

    private static void addBoolean(JsonObject out, FileConfiguration c, String field, String path) {
        if (c.isBoolean(path)) out.addProperty(field, c.getBoolean(path));
        else if (c.contains(path)) invalid(out, path);
    }
    private static void addInt(JsonObject out, FileConfiguration c, String field, String path) {
        Object value = c.get(path);
        if (value instanceof Integer || value instanceof Long) {
            long count = ((Number)value).longValue();
            if (count >= -1_000_000_000L && count <= 1_000_000_000L
                    && ("timeHourOffset".equals(field) || count >= 0)) out.addProperty(field, count);
            else invalid(out, path);
        } else if (c.contains(path)) invalid(out, path);
    }
    static void addString(JsonObject out, FileConfiguration c, String field, String path, int max) {
        if (!c.isString(path)) { if (c.contains(path)) invalid(out, path); return; }
        String value = c.getString(path);
        if (value != null && value.length() <= max && value.chars().noneMatch(Character::isISOControl)) {
            out.addProperty(field, value);
        } else invalid(out, path);
    }
    static void invalid(JsonObject out, String path) {
        com.google.gson.JsonArray errors = out.has("invalidConfigurationFields") ? out.getAsJsonArray("invalidConfigurationFields") : new com.google.gson.JsonArray();
        if (errors.size() < 100 && errors.asList().stream().noneMatch(v -> v.getAsString().equals(path))) errors.add(path);
        out.add("invalidConfigurationFields", errors);
    }
}
