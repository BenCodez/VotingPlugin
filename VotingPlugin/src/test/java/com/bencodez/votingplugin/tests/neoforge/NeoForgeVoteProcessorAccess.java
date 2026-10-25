package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeVoteProcessor.*;
import java.time.Clock;

/** Typed test-only access to package-private NeoForgeVoteProcessor seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeVoteProcessorAccess {
    private NeoForgeVoteProcessorAccess() { }
    public static NeoForgeVoteProcessor create(NeoForgeVoteConfiguration configuration, NeoForgeVoteAccountingStore accounting, NeoForgeDeferredVoteStore deferredVotes, NeoForgePlayerDirectory players, Clock clock) { return (NeoForgeVoteProcessor) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeVoteProcessor", new String[]{"NeoForgeVoteConfiguration", "NeoForgeVoteAccountingStore", "NeoForgeDeferredVoteStore", "NeoForgePlayerDirectory", "Clock"}, new Object[]{configuration, accounting, deferredVotes, players, clock}); }


}
