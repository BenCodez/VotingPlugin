package com.bencodez.votingplugin.tests.backgroundtask;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backgroundtask.*;
import com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask.*;
import com.bencodez.votingplugin.VotingPluginMain;

/** Typed test-only access to package-private VotingPluginBackgroundTask seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginBackgroundTaskAccess {
    private VotingPluginBackgroundTaskAccess() { }
    public static VotingPluginBackgroundTask create(VotingPluginMain plugin) { return (VotingPluginBackgroundTask) ReflectiveAccess.construct("com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static VotingPluginBackgroundTask create(VotingPluginMain plugin, long snapshotWaitMillis) { return (VotingPluginBackgroundTask) ReflectiveAccess.construct("com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask", new String[]{"VotingPluginMain", "long"}, new Object[]{plugin, snapshotWaitMillis}); }
}
