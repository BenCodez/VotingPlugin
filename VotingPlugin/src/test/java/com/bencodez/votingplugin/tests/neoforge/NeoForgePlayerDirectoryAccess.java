package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory.*;
import java.util.Optional;
import java.util.UUID;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Typed test-only access to package-private NeoForgePlayerDirectory seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgePlayerDirectoryAccess {
    private NeoForgePlayerDirectoryAccess() { }
    public static void joined(NeoForgePlayerDirectory target, Object player) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory", target, "joined", new String[]{"Object"}, new Object[]{player}); }
    public static SharedVoteIdentity joinedIdentity(NeoForgePlayerDirectory target, Object player) { return (SharedVoteIdentity) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory", target, "joinedIdentity", new String[]{"Object"}, new Object[]{player}); }
    public static void joined(NeoForgePlayerDirectory target, SharedVoteIdentity identity) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory", target, "joined", new String[]{"SharedVoteIdentity"}, new Object[]{identity}); }
    public static Optional<String> latestName(NeoForgePlayerDirectory target, UUID uuid) { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory", target, "latestName", new String[]{"UUID"}, new Object[]{uuid}); return rawResult == null ? null : ((java.util.Optional<?>) rawResult).map(value -> (String) value); }
    public static Object playerFromEvent(Object event) { return (Object) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgePlayerDirectory", null, "playerFromEvent", new String[]{"Object"}, new Object[]{event}); }
}
