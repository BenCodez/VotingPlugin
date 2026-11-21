package com.bencodez.votingplugin.tests.control;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.List;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

class VoteSiteInspectionScalarTest {
    private record Fixture(VotingPluginMain plugin, ConfigVoteSites sites, YamlConfiguration yaml) {
        JsonObject inspect(String kind) {
            return ControlInspectionServiceAccess.create(plugin).inspect(JsonParser.parseString(
                    "{\"kind\":\"" + kind + "\",\"filters\":{}}").getAsJsonObject()).getAsJsonObject("result");
        }
    }

    private Fixture fixture(boolean folded) {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        ConfigVoteSites sites = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
        YamlConfiguration yaml = new YamlConfiguration();
        doReturn(yaml).when(sites).getData();
        doReturn(folded).when(sites).isIgnoreCase();
        when(plugin.getConfigVoteSites()).thenReturn(sites);
        YamlConfiguration config = new YamlConfiguration();
        when(plugin.getConfigFile().getData()).thenReturn(config);
        when(plugin.getBungeeSettings().getData()).thenReturn(config);
        when(plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of());
        when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(List.of("route.example"));
        when(plugin.getVoteLogMysqlTable()).thenReturn(null);
        when(plugin.getServer().getPluginManager().getPlugins()).thenReturn(new org.bukkit.plugin.Plugin[0]);
        return new Fixture(plugin, sites, yaml);
    }

    @Test void healthUsesFoldedServiceForConfiguredAndDetectedMatchingOnlyWhenEnabled() {
        for (boolean folded : new boolean[] {false, true}) {
            Fixture f = fixture(folded);
            f.yaml.set("VoteSites.Example.servicesite", "route.example");
            String before = f.yaml.saveToString();
            JsonObject result = f.inspect("vote-site-health");
            var site = result.getAsJsonArray("sites").get(0).getAsJsonObject();
            assertEquals(folded ? "route.example" : "", site.get("serviceSite").getAsString());
            assertEquals(folded ? 0 : 1, result.getAsJsonArray("detectedUnconfiguredServices").size());
            assertEquals(before, f.yaml.saveToString());
            verify(f.sites, never()).saveData();
        }
    }

    @Test void networkPredicatesAndValuesResolveSameLeafAndKeepInvalidEvidence() {
        for (boolean folded : new boolean[] {false, true}) {
            Fixture f = fixture(folded);
            f.yaml.set("VoteSites.Example.servicesite", "route.example");
            f.yaml.set("VoteSites.Example.enabled", true);
            f.yaml.set("VoteSites.Example.autocreated", false);
            f.yaml.set("VoteSites.Example.votedelay", 2);
            f.yaml.set("VoteSites.Example.voteurl", "https://voting.example/vote");
            JsonObject site = f.inspect("network-health").getAsJsonArray("voteSites").get(0).getAsJsonObject();
            assertEquals(folded, site.get("enabled").getAsBoolean());
            assertEquals(folded ? "route.example" : "", site.get("serviceSite").getAsString());
            assertEquals(folded ? "EXAMPLE" : "BLANK", site.get("voteUrlState").getAsString());
            assertEquals(folded, site.has("autoCreated"));
            f.yaml.set("VoteSites.Example.enabled", "invalid boolean");
            JsonObject invalid = f.inspect("network-health");
            if (folded) {
                assertFalse(invalid.getAsJsonArray("voteSites").get(0).getAsJsonObject().has("enabled"));
                assertFalse(invalid.get("voteSitesComplete").getAsBoolean());
            }
        }
    }

    @Test void resolutionRetainsTrueDefaultDistinctFromConfigGetterFalse() {
        Fixture f = fixture(true);
        f.yaml.set("VoteSites.Example.name", "Folded label");
        f.yaml.set("VoteSites.Example.servicesite", "route.example");
        when(f.plugin.getVoteSiteManager().getVoteSiteName(anyBoolean(), anyString(), anyString())).thenReturn("Example");
        when(f.plugin.getVoteSiteManager().resolveVoteSite(anyString(), anyBoolean())).thenReturn(null);
        when(f.plugin.getVoteSiteManager().getResolver().getConfiguredVoteSiteName(anyString(), anyString()))
                .thenReturn("Example");
        JsonObject result = ControlInspectionServiceAccess.create(f.plugin).inspect(JsonParser.parseString(
                "{\"kind\":\"vote-site-resolution\",\"filters\":{\"serviceSite\":\"route.example\",\"includeDisabled\":\"true\"}}")
                .getAsJsonObject()).getAsJsonObject("result");
        assertEquals("Folded label", result.get("displayName").getAsString());
        assertEquals("route.example", result.get("configuredServiceSite").getAsString());
        assertTrue(result.get("enabled").getAsBoolean());
        assertFalse(f.sites.getVoteSiteEnabled("Example"));
        assertFalse(result.get("sideEffects").getAsBoolean());
    }

    @Test void networkKeepsNativeParentWhenRootFoldedLookupWouldSelectSibling() {
        for (boolean lowerFirst : new boolean[] {false, true}) {
            Fixture f = fixture(true);
            for (String name : lowerFirst ? new String[] {"site", "Site"} : new String[] {"Site", "site"}) {
                boolean upper = name.equals("Site");
                f.yaml.set("VoteSites." + name + ".servicesite", upper ? "upper.example" : "lower.example");
                f.yaml.set("VoteSites." + name + ".enabled", upper);
                f.yaml.set("VoteSites." + name + ".votedelay", upper ? 2 : 5);
            }
            ConfigVoteSites nativeRootConfig = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
            nativeRootConfig.setData(f.yaml);
            nativeRootConfig.setIgnoreCase(true);
            when(f.plugin.getConfigVoteSites()).thenReturn(nativeRootConfig);
            String before = f.yaml.saveToString();
            var rows = f.inspect("network-health").getAsJsonArray("voteSites");
            assertEquals(2, rows.size());
            for (var element : rows) {
                var row = element.getAsJsonObject();
                boolean upper = row.get("name").getAsString().equals("Site");
                assertEquals(upper, row.get("enabled").getAsBoolean());
                assertEquals(upper ? "upper.example" : "lower.example", row.get("serviceSite").getAsString());
                assertEquals(upper ? 2 : 5, row.get("delayHours").getAsInt());
            }
            assertEquals(before, f.yaml.saveToString());
        }
    }

    @Test void healthEnabledAndStatusAgreeWithCanonicalKey() {
        for (boolean enabled : new boolean[] {false, true}) {
            Fixture f = fixture(true);
            f.yaml.set("VoteSites.Example.ServiceSite", "route.example");
            f.yaml.set("VoteSites.Example.enabled", !enabled);
            f.yaml.set("VoteSites.Example.Enabled", enabled);
            JsonObject row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
            assertEquals(enabled, f.sites.getVoteSiteEnabled("Example"));
            assertEquals(enabled, row.get("enabled").getAsBoolean());
            assertEquals(enabled ? "VOTE_LOG_UNAVAILABLE" : "DISABLED", row.get("status").getAsString());
            f.yaml.set("VoteSites.Example.Enabled", null);
            f.yaml.set("VoteSites.Example.enabled", false);
            row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
            assertFalse(row.get("enabled").getAsBoolean());
            assertEquals("DISABLED", row.get("status").getAsString());
        }
    }

    @Test void networkCanonicalWrongTypeIsNotReplacedByValidFoldedSibling() {
        Fixture f = fixture(true);
        f.yaml.set("VoteSites.Example.enabled", true);
        f.yaml.set("VoteSites.Example.Enabled", "invalid boolean");
        f.yaml.set("VoteSites.Example.servicesite", "lower.example");
        f.yaml.set("VoteSites.Example.ServiceSite", "canonical.example");
        JsonObject result = f.inspect("network-health");
        JsonObject row = result.getAsJsonArray("voteSites").get(0).getAsJsonObject();
        assertFalse(row.has("enabled"));
        assertFalse(result.get("voteSitesComplete").getAsBoolean());
        assertEquals("canonical.example", row.get("serviceSite").getAsString());
        assertFalse(f.sites.getVoteSiteEnabled("Example"));
    }

    @Test void inspectionDefaultsDoNotHideConfiguredFoldedValuesWhenCopied() {
        for (boolean copyDefaults : new boolean[] {false, true}) {
            Fixture f = fixture(true);
            f.yaml.set("VoteSites.Example.servicesite", "configured.example");
            f.yaml.set("VoteSites.Example.enabled", false);
            f.yaml.addDefault("VoteSites.Example.ServiceSite", "default.example");
            f.yaml.addDefault("VoteSites.Example.Enabled", true);
            f.yaml.options().copyDefaults(copyDefaults);
            JsonObject row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
            assertEquals("configured.example", row.get("serviceSite").getAsString());
            assertFalse(row.get("enabled").getAsBoolean());
            assertEquals("DISABLED", row.get("status").getAsString());
            row = f.inspect("network-health").getAsJsonArray("voteSites").get(0).getAsJsonObject();
            assertEquals("configured.example", row.get("serviceSite").getAsString());
            assertFalse(row.get("enabled").getAsBoolean());
        }
    }

    @Test void healthScalarFieldsFollowFilePolicyAndKeepRawDelayFormat() {
        for (boolean folded : new boolean[] {false, true}) {
            for (boolean lower : new boolean[] {false, true}) {
                for (Object rawDelay : new Object[] {"2h", 2, true}) {
                    Fixture f = fixture(folded);
                    var site = f.yaml.createSection("VoteSites.Example");
                    site.set("ServiceSite", "route.example");
                    site.set(lower ? "name" : "Name", "Public label");
                    site.set(lower ? "hidden" : "Hidden", true);
                    site.set(lower ? "priority" : "Priority", 7);
                    site.set(lower ? "votedelay" : "VoteDelay", rawDelay);
                    String before = f.yaml.saveToString();
                    JsonObject row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
                    boolean found = folded || !lower;
                    assertEquals(found ? "Public label" : "Example", row.get("displayName").getAsString());
                    assertEquals(found, row.get("hidden").getAsBoolean());
                    assertEquals(found ? 7 : 5, row.get("priority").getAsInt());
                    assertEquals(found ? String.valueOf(rawDelay) : "24h", row.get("voteDelay").getAsString());
                    assertEquals(before, f.yaml.saveToString());
                }
            }
        }
    }

    @Test void healthInvalidEnabledIsDisabledButAbsentKeepsTrueFallback() {
        for (boolean folded : new boolean[] {false, true}) {
            Fixture f = fixture(folded);
            var site = f.yaml.createSection("VoteSites.Example");
            site.set("ServiceSite", "route.example");
            for (Object invalid : new Object[] {"invalid boolean", 0, List.of(true)}) {
                site.set(folded ? "enabled" : "Enabled", invalid);
                JsonObject row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
                assertFalse(f.sites.getVoteSiteEnabled("Example"));
                assertFalse(row.get("enabled").getAsBoolean());
                assertEquals("DISABLED", row.get("status").getAsString());
            }
            site.set("enabled", true);
            site.set("Enabled", "invalid boolean");
            JsonObject row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
            assertFalse(row.get("enabled").getAsBoolean(), "invalid canonical key must not select valid folded sibling");
            site.set("enabled", null);
            site.set("Enabled", null);
            row = f.inspect("vote-site-health").getAsJsonArray("sites").get(0).getAsJsonObject();
            assertTrue(row.get("enabled").getAsBoolean());
            assertEquals("VOTE_LOG_UNAVAILABLE", row.get("status").getAsString());
        }
    }
}
