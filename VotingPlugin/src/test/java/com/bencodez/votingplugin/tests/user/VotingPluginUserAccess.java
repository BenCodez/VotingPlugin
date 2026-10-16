package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import com.bencodez.votingplugin.user.VotingPluginUser.*;

/** Typed test-only access to package-private VotingPluginUser seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginUserAccess {
    private VotingPluginUserAccess() { }
    public static void beginOfflineVoteBulkClear() { ReflectiveAccess.call("com.bencodez.votingplugin.user.VotingPluginUser", null, "beginOfflineVoteBulkClear", new String[]{}, new Object[]{}); }
    public static void endOfflineVoteBulkClear() { ReflectiveAccess.call("com.bencodez.votingplugin.user.VotingPluginUser", null, "endOfflineVoteBulkClear", new String[]{}, new Object[]{}); }
    public static String bulkPointOperationId(String prefix, String batchOperationId, String userId) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.user.VotingPluginUser", null, "bulkPointOperationId", new String[]{"String", "String", "String"}, new Object[]{prefix, batchOperationId, userId}); }
    public static boolean completesLegacyTransfer(PointTransferResult result) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.VotingPluginUser", null, "completesLegacyTransfer", new String[]{"PointTransferResult"}, new Object[]{result}); }
}
