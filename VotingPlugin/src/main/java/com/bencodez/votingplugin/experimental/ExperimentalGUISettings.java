package com.bencodez.votingplugin.experimental;

import java.util.EnumSet;
import java.util.Set;
import org.bukkit.configuration.ConfigurationSection;
import com.bencodez.votingplugin.hologram.HologramVoteSettings;

record ExperimentalGUISettings(Set<ExperimentalGUIType> enabled, int maxSessions, int timeoutSeconds,
        int sitesPerPage, double distance) {
    ExperimentalGUISettings {
        enabled = Set.copyOf(enabled);
        if (maxSessions < 1 || maxSessions > 64 || timeoutSeconds < 1 || timeoutSeconds > 60
                || sitesPerPage < 1 || sitesPerPage > 5 || !Double.isFinite(distance)
                || distance < 1.5 || distance > 3)
            throw new IllegalArgumentException("MaxSessions 1..64, TimeoutSeconds 1..60, SitesPerPage 1..5, Distance 1.5..3");
    }

    static ExperimentalGUISettings read(ConfigurationSection config) {
        String path = "Experimental.VoteGUIs.";
        boolean global = config.getBoolean(path + "Enabled", false);
        // Preserve the earlier explicit hologram opt-in, without enabling any other style.
        boolean legacy = config.getBoolean("Experimental.HologramVoteGUI.Enabled", false);
        Set<ExperimentalGUIType> enabled = EnumSet.noneOf(ExperimentalGUIType.class);
        for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
            if ((global || type == ExperimentalGUIType.HOLOGRAM && legacy)
                    && config.getBoolean(path + type.configurationKey() + ".Enabled", true)) enabled.add(type);
        }
        return new ExperimentalGUISettings(enabled, config.getInt(path + "MaxActiveSessions", 32),
                config.getInt(path + "TimeoutSeconds", 60), config.getInt(path + "SitesPerPage", 5),
                config.getDouble(path + "Distance", 2.5));
    }

    HologramVoteSettings hologram(ConfigurationSection config) {
        HologramVoteSettings original = HologramVoteSettings.read(config);
        return new HologramVoteSettings(enabled.contains(ExperimentalGUIType.HOLOGRAM),
                original.distance(), original.timeoutSeconds(), original.sitesPerPage());
    }
}
