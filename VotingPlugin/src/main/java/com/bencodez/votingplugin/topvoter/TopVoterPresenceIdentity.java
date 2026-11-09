package com.bencodez.votingplugin.topvoter;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;

/** UUID-only evidence scopes; never resolves a profile or loads plugin/server storage. */
public final class TopVoterPresenceIdentity {
    private TopVoterPresenceIdentity() { }

    /** A null alias explicitly prevents uncertain migration evidence becoming negative. */
    public static List<UUID> identities(TopVoterPlayer row, boolean onlineMode, Function<String, String> cachedUuid) {
        var identities = new ArrayList<UUID>();
        if (row == null || row.getUuid() == null) { identities.add(null); return identities; }
        identities.add(row.getUuid());
        String name = row.getPlayerName();
        if (name == null || name.isBlank() || name.length() > 64 || !name.equals(name.trim())) {
            identities.add(null); return identities;
        }
        UUID offline = UUID.nameUUIDFromBytes(("OfflinePlayer:" + name).getBytes(StandardCharsets.UTF_8));
        if (!identities.contains(offline)) identities.add(offline);
        UUID current = null;
        try {
            String known = cachedUuid.apply(name);
            if (known != null && !known.isBlank()) {
                current = UUID.fromString(known);
                if (!current.toString().equalsIgnoreCase(known)) { identities.add(null); return identities; }
            }
        } catch (RuntimeException unavailable) { identities.add(null); return identities; }
        if (current != null && !identities.contains(current)) identities.add(current);
        // This validates known aliases; online historical completeness is checked
        // separately against retained server history by the asynchronous provider.
        boolean resolvable = onlineMode ? row.getUuid().version() == 4 && row.getUuid().variant() == 2
                && (current == null || current.equals(row.getUuid()))
                : offline.equals(row.getUuid()) || offline.equals(current);
        if (!resolvable) identities.add(null);
        return identities;
    }
}
