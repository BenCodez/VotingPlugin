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
    private static void invalid(JsonObject out, String path) {
        com.google.gson.JsonArray errors = out.has("invalidConfigurationFields") ? out.getAsJsonArray("invalidConfigurationFields") : new com.google.gson.JsonArray();
        if (errors.size() < 100 && errors.asList().stream().noneMatch(v -> v.getAsString().equals(path))) errors.add(path);
        out.add("invalidConfigurationFields", errors);
    }
}
