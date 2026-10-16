package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService.*;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Typed test-only access to package-private NeoForgeRewardReplayService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeRewardReplayServiceAccess {
    public static void stopAdmission(NeoForgeRewardReplayService target) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService", target, "stopAdmission", new String[]{}, new Object[]{}); }
    public static final int MAX_PER_RUN = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService", "MAX_PER_RUN");
    private NeoForgeRewardReplayServiceAccess() { }
    public static NeoForgeRewardReplayService create(NeoForgeVoteConfiguration configuration, NeoForgeRewardConfigurationAccess rewards, NeoForgeVoteAccountingStore accounting, NeoForgeDeferredVoteStore deferred, NeoForgePlayerDirectory players, NeoForgeRewardActionsAccess actions) { return (NeoForgeRewardReplayService) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService", new String[]{"NeoForgeVoteConfiguration", "NeoForgeRewardConfiguration", "NeoForgeVoteAccountingStore", "NeoForgeDeferredVoteStore", "NeoForgePlayerDirectory", "NeoForgeRewardActions"}, new Object[]{configuration, rewards, accounting, deferred, players, actions}); }


    public static void rememberIdentity(NeoForgeRewardReplayService target, SharedVoteIdentity identity) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService", target, "rememberIdentity", new String[]{"SharedVoteIdentity"}, new Object[]{identity}); }
    public static boolean deadlineReached(Long deadline, long now) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRewardReplayService", null, "deadlineReached", new String[]{"Long", "long"}, new Object[]{deadline, now}); }

}
