package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import java.util.Objects;
import java.util.UUID;

/** Typed test-only access to package-private NeoForgeDeferredVote seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeDeferredVoteAccess {
    private NeoForgeDeferredVoteAccess() { }
    public static NeoForgeDeferredVote withReplayContext(NeoForgeDeferredVote target, String currentName, String configuredServiceSite) { return (NeoForgeDeferredVote) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVote", target, "withReplayContext", new String[]{"String", "String"}, new Object[]{currentName, configuredServiceSite}); }
}
