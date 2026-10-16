package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeVoteAccountingDecision.*;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;

/** Typed test-only access to package-private NeoForgeVoteAccountingDecision seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeVoteAccountingDecisionAccess {
    private NeoForgeVoteAccountingDecisionAccess() { }
    public static NeoForgeVoteAccountingDecision capture(SharedVoteInput input, SharedVotePolicy policy, boolean currentlyOnline, int pointsOnVote, int pointLimit) { return (NeoForgeVoteAccountingDecision) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeVoteAccountingDecision", null, "capture", new String[]{"SharedVoteInput", "SharedVotePolicy", "boolean", "int", "int"}, new Object[]{input, policy, currentlyOnline, pointsOnVote, pointLimit}); }
}
