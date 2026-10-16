package com.bencodez.votingplugin.tests.proxy.bungee;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.bungee.*;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee.*;
import java.util.Collection;
import net.md_5.bungee.api.plugin.Plugin;

/** Typed test-only access to package-private VotingPluginBungee seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginBungeeAccess {
    public static void requireVotifierEventClass(VotingPluginBungee target) throws ClassNotFoundException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", target, "requireVotifierEventClass", new String[]{}, new Object[]{}); }
    private VotingPluginBungeeAccess() { }
    public static Collection<String> installedPluginNames(Collection<Plugin> plugins) { return (Collection<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", null, "installedPluginNames", new String[]{"Collection"}, new Object[]{plugins}); }
    public static void initializeFirstRuntime(VotingPluginBungee target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", target, "initializeFirstRuntime", new String[]{}, new Object[]{}); }
    public static void publishRetainedRuntimeOperational(VotingPluginBungee target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", target, "publishRetainedRuntimeOperational", new String[]{}, new Object[]{}); }
    public static boolean initVotifierListenerIfNeeded(VotingPluginBungee target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", target, "initVotifierListenerIfNeeded", new String[]{}, new Object[]{}); }

    public static VoteEventBungee createVotifierListener(VotingPluginBungee target) { return (VoteEventBungee) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee", target, "createVotifierListener", new String[]{}, new Object[]{}); }
}
