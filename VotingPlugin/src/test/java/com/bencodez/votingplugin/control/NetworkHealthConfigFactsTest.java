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
}
