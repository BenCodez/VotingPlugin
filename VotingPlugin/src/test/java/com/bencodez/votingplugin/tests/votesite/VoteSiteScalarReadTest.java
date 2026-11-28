package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.votesites.VoteSiteRegistry;
import com.bencodez.votingplugin.votesites.VoteSiteResolver;
import com.bencodez.votingplugin.votesites.VoteSiteValidator;

class VoteSiteScalarReadTest {
    private ConfigVoteSites config(YamlConfiguration yaml, boolean folded) {
        ConfigVoteSites config = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
        doReturn(yaml).when(config).getData();
        doReturn(folded).when(config).isIgnoreCase();
        return config;
    }

    @Test void foldedLeafReadsUseAlreadySelectedNativeParent() {
        for (boolean lowerFirst : new boolean[] {false, true}) {
            YamlConfiguration yaml = new YamlConfiguration();
            for (String parent : lowerFirst ? new String[] {"site", "Site"} : new String[] {"Site", "site"}) {
                yaml.set("VoteSites." + parent + ".name", parent + " label");
                yaml.set("VoteSites." + parent + ".servicesite", parent + ".example");
            }
            ConfigVoteSites config = config(yaml, true);
            assertEquals("Site label", config.getDisplayName("Site"));
            assertEquals("site label", config.getDisplayName("site"));
            assertSame(yaml.getConfigurationSection("VoteSites.Site"), config.getData("Site"));
        }
    }

    @Test void realResolverFindsFoldedServiceAndDisplayLeavesWithoutCreating() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("VoteSites.Example.servicesite", "voting.example");
        yaml.set("VoteSites.Example.name", "Example label");
        ConfigVoteSites config = config(yaml, true);
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        when(plugin.getConfigVoteSites()).thenReturn(config);
        VoteSiteResolver resolver = new VoteSiteResolver(plugin, new VoteSiteRegistry(), new VoteSiteValidator(plugin));
        assertEquals("Example", resolver.getConfiguredVoteSiteName("voting.example"));
        assertEquals("Example", resolver.getConfiguredVoteSiteName("Example label"));
        assertNull(resolver.getConfiguredVoteSiteName("unknown.example"));
        verify(config, never()).tryGenerateVoteSite(anyString());
        verify(config, never()).tryAutoGenerateVoteSite(anyString());
    }

    @Test void strictReadsAndNativeDefaultFallbackRemainUnchanged() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.set("VoteSites.Example.name", "folded label");
        yaml.addDefault("VoteSites.Example.Name", "native default");
        ConfigVoteSites config = config(yaml, false);
        assertEquals("native default", config.getDisplayName("Example"));
        assertFalse(config.getVoteSiteEnabled("Example"));
        assertEquals("", config.getVoteURL("Example"));
    }

    @Test void scalarDefaultsTypesAliasesAndLeafOrderMatchNativeGetters() {
        for (boolean folded : new boolean[] {false, true}) {
            YamlConfiguration yaml = new YamlConfiguration();
            var site = yaml.createSection("VoteSites.Example");
            yaml.addDefault("VoteSites.Example.Name", "default label");
            ConfigVoteSites config = config(yaml, folded);
            assertEquals("default label", config.getDisplayName("Example"));
            assertEquals("", config.getPermissionToView("Example"));
            assertEquals(0, config.getPriority("Example"));
            assertNull(config.getServiceSite("Example"));
            assertEquals(0, config.getVoteDelayDailyHour("Example"));
            assertFalse(config.getVoteSiteEnabled("Example"));
            assertFalse(config.getVoteSiteGiveOffline("Example"));
            assertFalse(config.getVoteSiteHidden("Example"));
            assertFalse(config.getVoteSiteIgnoreCanVote("Example"));
            assertFalse(config.getVoteSiteResetVoteDelayDaily("Example"));
            assertFalse(config.getWaitUntilVoteDelay("Example"));
            site.set("GiveOffline", true);
            site.set("ForceOffline", false);
            assertFalse(config.getVoteSiteGiveOffline("Example"));
            site.set("Priority", "not a number");
            assertEquals(site.getInt("Priority"), config.getPriority("Example"));
            site.set("Name", null);
            site.set("name", "first");
            site.set("Name", "second");
            assertEquals("second", config.getDisplayName("Example"));
        }
    }

    @Test void nativeIdentitiesExactWritesAndCapturedPolicySurviveReadsAndToggle() {
        YamlConfiguration yaml = new YamlConfiguration();
        var site = yaml.createSection("VoteSites.Example");
        var item = site.createSection("Item");
        var rewards = site.createSection("Rewards");
        var lowerRewards = site.createSection("rewards");
        site.set("name", "folded label");
        ConfigVoteSites config = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
        config.setData(yaml);
        config.setIgnoreCase(true);
        var root = config.getData();
        var created = root.createSection("Created");
        String before = yaml.saveToString();
        assertEquals("folded label", config.getDisplayName("Example"));
        assertSame(site, root.get("VoteSites.Example"));
        assertSame(site, config.getData("Example"));
        assertSame(site, config.getData("Example"));
        assertSame(yaml, root.getConfigurationSection(""));
        assertSame(yaml.getConfigurationSection("VoteSites"), site.getParent());
        assertSame(created, yaml.getConfigurationSection("Created"));
        assertSame(item, config.getItem("Example"));
        assertSame(rewards, site.getValues(false).get("Rewards"));
        assertSame(lowerRewards, site.getValues(false).get("rewards"));
        assertEquals(before, yaml.saveToString());
        rewards.set("Exact", 1);
        lowerRewards.set("Exact", 2);
        assertEquals(1, site.getInt("Rewards.Exact"));
        assertEquals(2, site.getInt("rewards.Exact"));
        config.setIgnoreCase(false);
        assertSame(yaml, config.getData());
        assertNull(config.getDisplayName("Example"));
        config.setIgnoreCase(true);
        assertEquals("folded label", config.getDisplayName("Example"));
    }

    @Test void separatorLeafAndMissingSectionKeepNativeBehavior() {
        YamlConfiguration yaml = new YamlConfiguration();
        yaml.options().pathSeparator('a');
        var site = yaml.createSection("VoteSites.Example");
        site.set("Name", "native path");
        ConfigVoteSites config = config(yaml, true);
        assertEquals(site.getString("Name"), config.getDisplayName("Example"));
        doReturn(null).when(config).getData("Missing");
        assertThrows(NullPointerException.class, () -> config.getDisplayName("Missing"));
    }

    @Test void allScalarGettersRetainCanonicalAndFoldedTypeContracts() {
        for (boolean folded : new boolean[] {false, true}) {
            for (boolean lower : new boolean[] {false, true}) {
                YamlConfiguration yaml = new YamlConfiguration();
                var site = yaml.createSection("VoteSites.Example");
                String[] keys = {"Name", "PermissionToView", "Priority", "ServiceSite", "VoteDelayDailyHour",
                        "Enabled", "GiveOffline", "Hidden", "IgnoreCanVote", "VoteDelayDaily", "VoteURL", "WaitUntilVoteDelay"};
                Object[] values = {"label", "vote.example", 7, "route.example", 3, true, true, true, true, true,
                        "https://voting.example/vote", true};
                for (int i = 0; i < keys.length; i++) site.set(lower ? keys[i].toLowerCase(java.util.Locale.ROOT) : keys[i], values[i]);
                ConfigVoteSites config = config(yaml, folded);
                boolean found = folded || !lower;
                assertEquals(found ? "label" : null, config.getDisplayName("Example"));
                assertEquals(found ? "vote.example" : "", config.getPermissionToView("Example"));
                assertEquals(found ? 7 : 0, config.getPriority("Example"));
                assertEquals(found ? "route.example" : null, config.getServiceSite("Example"));
                assertEquals(found ? 3 : 0, config.getVoteDelayDailyHour("Example"));
                assertEquals(found, config.getVoteSiteEnabled("Example"));
                assertEquals(found, config.getVoteSiteGiveOffline("Example"));
                assertEquals(found, config.getVoteSiteHidden("Example"));
                assertEquals(found, config.getVoteSiteIgnoreCanVote("Example"));
                assertEquals(found, config.getVoteSiteResetVoteDelayDaily("Example"));
                assertEquals(found, config.getWaitUntilVoteDelay("Example"));
                assertEquals(found ? "https://voting.example/vote" : "", config.getVoteURL("Example"));
                site.set(lower ? "forceoffline" : "ForceOffline", false);
                assertFalse(config.getVoteSiteGiveOffline("Example"));
            }
        }
    }

    @Test void durationStringAndNumericAliasUseSameSelectedLeafPolicy() {
        YamlConfiguration yaml = new YamlConfiguration();
        var site = yaml.createSection("VoteSites.Example");
        ConfigVoteSites config = config(yaml, true);
        site.set("votedelay", "2h");
        assertEquals(7_200_000L,
                config.getVoteDelay("Example").getMillis());
        site.set("votedelay", 1);
        site.set("votedelaymin", 30);
        assertEquals(5_400_000L,
                config.getVoteDelay("Example").getMillis());
        site.set("votedelay", "nonsense");
        assertTrue(assertDoesNotThrow(() -> config.getVoteDelay("Example")).isEmpty());
    }

    @Test void exactConfigWritesStillSelectNativeRewardsAndNeverFoldLeafTargets() {
        YamlConfiguration yaml = new YamlConfiguration();
        var site = yaml.createSection("VoteSites.Example");
        site.set("Rewards.Exact", 1);
        site.set("rewards.Exact", 2);
        ConfigVoteSites config = config(yaml, true);
        doNothing().when(config).saveData();
        config.set("Example", "Rewards.Exact", 3);
        assertEquals(3, site.getInt("Rewards.Exact"));
        assertEquals(2, site.getInt("rewards.Exact"));
        verify(config).saveData();
    }

    @Test void nativeReloadKeepsCapturedPolicyAndDoesNotRewriteFile(
            @org.junit.jupiter.api.io.TempDir java.nio.file.Path folder) throws Exception {
        var file = folder.resolve("VoteSites.yml");
        String bytes = "VoteSites:\n  Example:\n    name: before\n";
        java.nio.file.Files.writeString(file, bytes);
        ConfigVoteSites config = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
        var field = com.bencodez.simpleapi.file.YMLFile.class.getDeclaredField("dFile");
        field.setAccessible(true);
        field.set(config, file.toFile());
        config.setIgnoreCase(true);
        config.reloadData();
        assertEquals("before", config.getDisplayName("Example"));
        assertEquals(bytes, java.nio.file.Files.readString(file));
        bytes = bytes.replace("before", "after");
        java.nio.file.Files.writeString(file, bytes);
        config.reloadData();
        assertEquals("after", config.getDisplayName("Example"));
        assertTrue(config.isIgnoreCase());
        assertEquals(bytes, java.nio.file.Files.readString(file));
        config.setIgnoreCase(false);
        config.reloadData();
        assertNull(config.getDisplayName("Example"));
        assertEquals(bytes, java.nio.file.Files.readString(file));
    }

    @Test void scalarSetterReadbackRemainsEffectiveWithExistingLowercaseLeaf() {
        for (boolean folded : new boolean[] {false, true}) {
            for (boolean lower : new boolean[] {false, true}) {
                YamlConfiguration yaml = new YamlConfiguration();
                yaml.set("VoteSites.Example." + (lower ? "enabled" : "Enabled"), true);
                ConfigVoteSites config = config(yaml, folded);
                doNothing().when(config).saveData();
                config.setEnabled("Example", false);
                assertEquals(Boolean.FALSE, yaml.get("VoteSites.Example.Enabled", null), "native exact setter wrote false");
                assertFalse(config.getVoteSiteEnabled("Example"), "setter/readback must agree");
                if (lower) assertTrue(yaml.getBoolean("VoteSites.Example.enabled"), "exact writes retain sibling");
            }
        }
    }

    @Test void canonicalKeysWinWithoutRewritingEarlierFoldedSiblings() {
        YamlConfiguration yaml = new YamlConfiguration();
        var site = yaml.createSection("VoteSites.Example");
        site.set("name", "lower");
        site.set("Name", "canonical");
        site.set("priority", 7);
        site.set("Priority", "invalid number");
        ConfigVoteSites config = config(yaml, true);
        String before = yaml.saveToString();
        assertEquals("canonical", config.getDisplayName("Example"));
        assertEquals(0, config.getPriority("Example"), "wrong canonical type must not select a folded sibling");
        assertEquals(before, yaml.saveToString());
        site.set("Name", null);
        site.set("NAME", "later folded");
        assertEquals("lower", config.getDisplayName("Example"));
        site.set("name", null);
        assertEquals("later folded", config.getDisplayName("Example"));
    }

    @Test void nativeDefaultIsFallbackRatherThanAnExplicitCanonicalLeaf() {
        for (boolean copyDefaults : new boolean[] {false, true}) {
            YamlConfiguration yaml = new YamlConfiguration();
            var site = yaml.createSection("VoteSites.Example");
            yaml.addDefault("VoteSites.Example.Name", "native default");
            yaml.options().copyDefaults(copyDefaults);
            site.set("name", "configured lower");
            ConfigVoteSites config = config(yaml, true);
            assertNull(site.get("Name", null), "native explicit-value lookup excludes defaults");
            assertEquals("configured lower", config.getDisplayName("Example"));
            site.set("name", null);
            assertEquals("native default", config.getDisplayName("Example"));
        }
    }

    @Test void scalarSetterFamilyRetainsExactWritesAndCanonicalReadback() {
        for (boolean folded : new boolean[] {false, true}) {
            YamlConfiguration yaml = new YamlConfiguration();
            var site = yaml.createSection("VoteSites.Example");
            site.set("name", "previous");
            site.set("servicesite", "previous.example");
            site.set("priority", 9);
            site.set("forceoffline", true);
            site.set("votedelay", "9h");
            site.set("voteurl", "previous");
            site.set("votedelaydailyhour", 9);
            site.set("votedelaydaily", true);
            ConfigVoteSites config = config(yaml, folded);
            doNothing().when(config).saveData();
            config.setDisplayName("Example", "current");
            config.setServiceSite("Example", "current.example");
            config.setPriority("Example", 3);
            config.setForceOffline("Example", false);
            config.setVoteDelay("Example", "2h");
            config.setVoteURL("Example", "https://voting.example/vote");
            config.setVoteDelayDailyHour("Example", 2);
            config.setVoteDelayDaily("Example", false);
            assertEquals("current", config.getDisplayName("Example"));
            assertEquals("current.example", config.getServiceSite("Example"));
            assertEquals(3, config.getPriority("Example"));
            assertFalse(config.getVoteSiteGiveOffline("Example"));
            assertEquals(7_200_000L, config.getVoteDelay("Example").getMillis());
            assertEquals("https://voting.example/vote", config.getVoteURL("Example"));
            assertEquals(2, config.getVoteDelayDailyHour("Example"));
            assertFalse(config.getVoteSiteResetVoteDelayDaily("Example"));
            assertEquals("previous", site.getString("name"));
            assertEquals("previous.example", site.getString("servicesite"));
            assertEquals(9, site.getInt("priority"));
            assertTrue(site.getBoolean("forceoffline"));
            assertEquals("9h", site.getString("votedelay"));
            assertEquals("previous", site.getString("voteurl"));
            assertEquals(9, site.getInt("votedelaydailyhour"));
            assertTrue(site.getBoolean("votedelaydaily"));
        }
    }
}
