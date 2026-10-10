package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

class ExperimentalGUISettingsTest {
    @Test void missingSettingsNeverEnableNewStyles() {
        var settings = ExperimentalGUISettings.read(new YamlConfiguration());
        assertTrue(settings.enabled().isEmpty());
        assertFalse(settings.hologram(new YamlConfiguration()).enabled());
        assertEquals(32, settings.maxSessions());
    }

    @Test void legacyOptInEnablesOnlyHologramAndPreservesItsSettings() {
        var config = new YamlConfiguration();
        config.set("Experimental.HologramVoteGUI.Enabled", true);
        config.set("Experimental.HologramVoteGUI.Distance", 2.9);
        config.set("Experimental.HologramVoteGUI.TimeoutSeconds", 25);
        var settings = ExperimentalGUISettings.read(config);
        assertEquals(java.util.Set.of(ExperimentalGUIType.HOLOGRAM), settings.enabled());
        assertEquals(2.9, settings.hologram(config).distance());
        assertEquals(25, settings.hologram(config).timeoutSeconds());
        config.set("Experimental.VoteGUIs.Hologram.Enabled", false);
        assertTrue(ExperimentalGUISettings.read(config).enabled().isEmpty());
    }

    @Test void individualDisablingNeverDisablesAnotherStyle() {
        for (var type : ExperimentalGUIType.values()) {
            var config = new YamlConfiguration();
            config.set("Experimental.VoteGUIs.Enabled", true);
            config.set("Experimental.VoteGUIs." + type.configurationKey() + ".Enabled", false);
            var settings = ExperimentalGUISettings.read(config);
            assertFalse(settings.enabled().contains(type));
            assertEquals(7, settings.enabled().size());
        }
    }

    @Test void invalidPerformanceSettingsFailClosed() {
        for (String setting : new String[] {"MaxActiveSessions", "TimeoutSeconds", "SitesPerPage", "Distance"}) {
            var config = new YamlConfiguration();
            config.set("Experimental.VoteGUIs." + setting, 999);
            assertThrows(IllegalArgumentException.class, () -> ExperimentalGUISettings.read(config));
        }
    }

    @Test void malformedHologramSettingsDoNotDisableUnrelatedStyles() {
        var config = new YamlConfiguration();
        config.set("Experimental.VoteGUIs.Enabled", true);
        config.set("Experimental.HologramVoteGUI.Distance", 999);
        var settings = ExperimentalGUISettings.read(config);
        assertTrue(settings.enabled().contains(ExperimentalGUIType.ANIMATED_INVENTORY));
        assertTrue(settings.enabled().contains(ExperimentalGUIType.STREAK_TRACK));
        assertThrows(IllegalArgumentException.class, () -> settings.hologram(config));
    }
}
