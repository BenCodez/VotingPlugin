package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import org.mockito.Mockito;

/** Narrow test-only access to the real startup ingress lifecycle. */
public final class DateVoteIngressAccess {
    private DateVoteIngressAccess() { }
    public static void initializeDateVoteIngress(VotingPluginMain target, Runnable opening) {
        ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", target,
                "initializeDateVoteIngress", new String[]{"Runnable"}, new Object[]{opening});
    }
    public static void registerEarlyVotifierIngress(VotingPluginMain target) {
        ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", target,
                "registerEarlyVotifierIngress", new String[]{}, new Object[]{});
    }
    public static void callRealInitialization(VotingPluginMain target) {
        initializeDateVoteIngress(Mockito.doCallRealMethod().when(target), Mockito.any());
    }
    public static void callRealRegistration(VotingPluginMain target) {
        registerEarlyVotifierIngress(Mockito.doCallRealMethod().when(target));
    }
}
