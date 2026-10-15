package com.bencodez.votingplugin.hologram;

import org.bukkit.configuration.ConfigurationSection;

public record HologramVoteSettings(boolean enabled, double distance, int timeoutSeconds, int sitesPerPage) {
    public HologramVoteSettings {
        if (!Double.isFinite(distance) || distance < 1.5 || distance > 3
                || timeoutSeconds < 1 || timeoutSeconds > 60 || sitesPerPage < 1 || sitesPerPage > 5) {
            throw new IllegalArgumentException("Distance must be 1.5..3, TimeoutSeconds 1..60, SitesPerPage 1..5");
        }
    }
    public static HologramVoteSettings read(ConfigurationSection config) {
        String path = "Experimental.HologramVoteGUI.";
        return new HologramVoteSettings(config.getBoolean(path + "Enabled", false),
                config.getDouble(path + "Distance", 2.5), config.getInt(path + "TimeoutSeconds", 60),
                config.getInt(path + "SitesPerPage", 5));
    }
}
