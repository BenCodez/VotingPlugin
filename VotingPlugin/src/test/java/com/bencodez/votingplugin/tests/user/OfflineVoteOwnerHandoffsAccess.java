package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import com.bencodez.votingplugin.user.OfflineVoteOwnerHandoffs.*;

/** Typed test-only access to package-private OfflineVoteOwnerHandoffs seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class OfflineVoteOwnerHandoffsAccess {
    private OfflineVoteOwnerHandoffsAccess() { }
    public static int pendingCount(OfflineVoteOwnerHandoffs target) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.OfflineVoteOwnerHandoffs", target, "pendingCount", new String[]{}, new Object[]{}); }
}
