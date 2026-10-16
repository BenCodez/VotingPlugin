package com.bencodez.votingplugin.tests.specialrewards;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.specialrewards.*;
import com.bencodez.votingplugin.specialrewards.NameMCLikeCheckerTask.*;
import java.util.UUID;

/** Typed test-only access to package-private NameMCLikeCheckerTask seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NameMCLikeCheckerTaskAccess {
    private NameMCLikeCheckerTaskAccess() { }
    public static void processUuid(NameMCLikeCheckerTask target, UUID uuid) { ReflectiveAccess.call("com.bencodez.votingplugin.specialrewards.NameMCLikeCheckerTask", target, "processUuid", new String[]{"UUID"}, new Object[]{uuid}); }
}
