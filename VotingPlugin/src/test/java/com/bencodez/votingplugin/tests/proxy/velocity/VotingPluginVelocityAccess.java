package com.bencodez.votingplugin.tests.proxy.velocity;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.velocity.*;
import com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity.*;
import java.util.Collection;

/** Typed test-only access to package-private VotingPluginVelocity seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginVelocityAccess {
    public static void publishRetainedRuntimeOperational(VotingPluginVelocity target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "publishRetainedRuntimeOperational", new String[]{}, new Object[]{}); }
    public static VoteEventVelocity createVotifierListener(VotingPluginVelocity target) { return (VoteEventVelocity) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "createVotifierListener", new String[]{}, new Object[]{}); }
    public static void requireVotifierEventClass(VotingPluginVelocity target) throws ClassNotFoundException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "requireVotifierEventClass", new String[]{}, new Object[]{}); }
    public static boolean initVotifierListenerIfNeeded(VotingPluginVelocity target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "initVotifierListenerIfNeeded", new String[]{}, new Object[]{}); }
    public static void initializeFirstRuntime(VotingPluginVelocity target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "initializeFirstRuntime", new String[]{}, new Object[]{}); }
    public static boolean prepareDatabaseDriver(VotingPluginVelocity target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", target, "prepareDatabaseDriver", new String[]{}, new Object[]{}); }
    private VotingPluginVelocityAccess() { }
    public static Collection<String> installedPluginNames(Collection<com.velocitypowered.api.plugin.PluginContainer> plugins) { return (Collection<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", null, "installedPluginNames", new String[]{"Collection"}, new Object[]{plugins}); }
    public static Collection<Object> diagnosticProviders(Collection<com.velocitypowered.api.plugin.PluginContainer> plugins) { return (Collection<Object>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity", null, "diagnosticProviders", new String[]{"Collection"}, new Object[]{plugins}); }






}
