package com.bencodez.votingplugin.tests.voteshop;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.voteshop.*;
import com.bencodez.votingplugin.voteshop.VoteShopManager.*;
import com.bencodez.votingplugin.VotingPluginMain;

/** Typed test-only access to package-private VoteShopManager seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteShopManagerAccess {
    private VoteShopManagerAccess() { }
    public static void scheduleSharedPurchaseRecovery(VotingPluginMain plugin) { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.VoteShopManager", null, "scheduleSharedPurchaseRecovery", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
}
