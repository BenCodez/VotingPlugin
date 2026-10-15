package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;

import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

class HologramVoteSettingsTest {
    @Test void missingConfigurationUsesSafeDisabledDefaults() {
        HologramVoteSettings settings = HologramVoteSettings.read(new MemoryConfiguration());
        assertFalse(settings.enabled());
        assertEquals(2.5, settings.distance());
        assertEquals(60, settings.timeoutSeconds());
        assertEquals(5, settings.sitesPerPage());
    }

    @Test void configuredValuesAreReadAndBoundsAreEnforced() {
        MemoryConfiguration config = new MemoryConfiguration();
        config.set("Experimental.HologramVoteGUI.Enabled", true);
        config.set("Experimental.HologramVoteGUI.Distance", 1.5);
        config.set("Experimental.HologramVoteGUI.TimeoutSeconds", 1);
        config.set("Experimental.HologramVoteGUI.SitesPerPage", 1);
        assertEquals(new HologramVoteSettings(true, 1.5, 1, 1), HologramVoteSettings.read(config));
        assertThrows(IllegalArgumentException.class, () -> new HologramVoteSettings(true, 1.4, 10, 1));
        assertThrows(IllegalArgumentException.class, () -> new HologramVoteSettings(true, 2, 61, 1));
        assertThrows(IllegalArgumentException.class, () -> new HologramVoteSettings(true, 2, 10, 6));
    }
}
