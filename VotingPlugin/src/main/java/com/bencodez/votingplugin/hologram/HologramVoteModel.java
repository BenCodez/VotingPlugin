package com.bencodez.votingplugin.hologram;

import java.net.URI;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

/** Immutable presentation data; never executes votes, commands or rewards. */
public final class HologramVoteModel {
    private HologramVoteModel() { }

    public record Site(String key, String name, String url, boolean canVote, long remainingSeconds) {
        public String title() {
            String title = name == null || name.isBlank() ? key : name;
            title = title.replaceAll("[\\r\\n\\p{Cntrl}]", " ");
            if (title.length() > 22) title = title.substring(0, 21) + "\u2026";
            return title;
        }
        public String label() {
            String status = canVote ? "Vote Now" : remainingSeconds > 0
                    ? duration(remainingSeconds) + " Remaining" : "Cooldown active";
            return (canVote ? "\u00a7a" : "\u00a7c") + "\u25cf " + title() + " \u00a77- " + status;
        }
    }

    public static List<Site> page(List<Site> sites, int page, int perPage) {
        if (perPage < 1 || perPage > 5) throw new IllegalArgumentException("SitesPerPage must be 1..5");
        int pages = pages(sites.size(), perPage);
        int start = Math.min(Math.max(page, 0), pages - 1) * perPage;
        return List.copyOf(sites.subList(start, Math.min(start + perPage, sites.size())));
    }

    public static int pages(int sites, int perPage) {
        if (perPage < 1 || perPage > 5) throw new IllegalArgumentException("SitesPerPage must be 1..5");
        return Math.max(1, (sites + perPage - 1) / perPage);
    }

    public static Optional<String> votingUrl(String value) {
        if (value == null || value.isBlank() || value.length() > 2048) return Optional.empty();
        try {
            URI uri = new URI(value.trim());
            String scheme = uri.getScheme();
            if (scheme == null || !List.of("http", "https").contains(scheme.toLowerCase(Locale.ROOT))
                    || uri.getHost() == null || uri.getHost().isBlank() || uri.getRawUserInfo() != null
                    || uri.getPort() == 0 || uri.getPort() < -1 || uri.getPort() > 65535) return Optional.empty();
            return Optional.of(uri.toASCIIString());
        } catch (Exception invalid) {
            return Optional.empty();
        }
    }

    static String duration(long seconds) {
        long hours = seconds / 3600;
        long minutes = (seconds % 3600) / 60;
        return hours > 0 ? hours + "h " + minutes + "m" : minutes > 0 ? minutes + "m" : seconds + "s";
    }
}
