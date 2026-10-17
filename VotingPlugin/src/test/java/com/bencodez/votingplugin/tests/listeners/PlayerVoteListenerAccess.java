package com.bencodez.votingplugin.tests.listeners;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.listeners.*;
import com.bencodez.votingplugin.listeners.PlayerVoteListener.*;
import java.util.UUID;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;

/** Typed test-only access to package-private PlayerVoteListener seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class PlayerVoteListenerAccess {
    private PlayerVoteListenerAccess() { }
    public static void copySessionObservation(PlayerPostVoteEvent post, PlayerVoteEvent input) {
        ReflectiveAccess.call("com.bencodez.votingplugin.listeners.PlayerVoteListener", null,
                "copySessionObservation", new String[]{"PlayerPostVoteEvent", "PlayerVoteEvent"}, new Object[]{post, input});
    }
    public static UUID resolveProxyVoteId(PlayerVoteEvent event) { return (UUID) ReflectiveAccess.call("com.bencodez.votingplugin.listeners.PlayerVoteListener", null, "resolveProxyVoteId", new String[]{"PlayerVoteEvent"}, new Object[]{event}); }
}
