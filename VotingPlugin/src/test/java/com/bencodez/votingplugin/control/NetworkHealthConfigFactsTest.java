package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.*;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.google.gson.JsonObject;

class NetworkHealthConfigFactsTest {
    @Test void onlyTypedAllowlistedBackendFactsAreEmitted() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("OnlineMode", true);
        yaml.set("MaxAmountOfVotesPerDay", "not-an-int");
        yaml.set("Database.Password", "secret");
        yaml.set("BedrockPlayerPrefix", "x".repeat(81));
        JsonObject out = new JsonObject();
        NetworkHealthConfigFacts.addBackend(out, yaml);
        assertTrue(out.get("onlineMode").getAsBoolean());
        assertFalse(out.has("maxVotesPerDay"));
        assertFalse(out.has("Database.Password"));
        assertFalse(out.has("bedrockPlayerPrefix"));
    }

    @Test void globalFactsRejectWrongTypesAndLongValues() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("GlobalData.Prefix", "p".repeat(81));
        yaml.set("GlobalData.UseMainMySQL", "yes");
        JsonObject out = new JsonObject();
        NetworkHealthConfigFacts.addGlobal(out, yaml);
        assertFalse(out.has("globalDataPrefix"));
        assertFalse(out.has("globalDataUseMainMysql"));
    }

    @Test void integerFactsRemainBoundedToContractRange() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("TimeHourOffSet", Integer.MAX_VALUE);
        JsonObject out = new JsonObject();
        NetworkHealthConfigFacts.addBackend(out, yaml);
        assertFalse(out.has("timeHourOffset"));
        yaml.set("TimeHourOffSet", -3);
        NetworkHealthConfigFacts.addBackend(out, yaml);
        assertEquals(-3, out.get("timeHourOffset").getAsInt());
    }

    @Test void longMinimumDoesNotPassAbsoluteValueBound() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("TimeHourOffSet", Long.MIN_VALUE);
        JsonObject out = new JsonObject();
        NetworkHealthConfigFacts.addBackend(out, yaml);
        assertFalse(out.has("timeHourOffset"));
    }
    @Test void delayParserEmptyResultCannotProveMalformedTextValid() {
        for (String text : java.util.List.of("abc", "-5h", "")) {
            JsonObject out = new JsonObject();
            NetworkHealthConfigFacts.addDelay(out, text, null, com.bencodez.simpleapi.time.ParsedDuration.parse(text, java.util.concurrent.TimeUnit.HOURS));
            assertFalse(out.get("delayValid").getAsBoolean());
        }
        JsonObject halfHour = new JsonObject();
        NetworkHealthConfigFacts.addDelay(halfHour, "30m", null, com.bencodez.simpleapi.time.ParsedDuration.parse("30m"));
        assertTrue(halfHour.get("delayValid").getAsBoolean()); assertEquals(1, halfHour.get("delayHours").getAsInt());
        JsonObject zero = new JsonObject();
        NetworkHealthConfigFacts.addDelay(zero, "0", null, com.bencodez.simpleapi.time.ParsedDuration.empty());
        assertTrue(zero.get("delayValid").getAsBoolean()); assertEquals(0, zero.get("delayHours").getAsInt());
    }
    @Test void incompleteServiceObservationsAreOmittedInsteadOfEmpty() {
        JsonObject invalid = new JsonObject();
        NetworkHealthConfigFacts.addObservedServices(invalid, java.util.List.of("unmatched", "s".repeat(81)));
        assertFalse(invalid.has("detectedServices"));
        JsonObject large = new JsonObject();
        NetworkHealthConfigFacts.addObservedServices(large, java.util.stream.IntStream.range(0, 101).mapToObj(i -> "site" + i).toList());
        assertFalse(large.has("detectedServices"));
        JsonObject valid = new JsonObject();
        NetworkHealthConfigFacts.addObservedServices(valid, java.util.List.of("site", "SITE", "other"));
        assertEquals(2, valid.getAsJsonArray("detectedServices").size());
    }

}
