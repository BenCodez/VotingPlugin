package com.bencodez.votingplugin.tests.backendproxy.global;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.global.*;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync.*;
import java.util.concurrent.TimeUnit;

/** Typed test-only access to package-private BackendGlobalDataSync seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendGlobalDataSyncAccess {
    public static void updateLastOnline(BackendGlobalDataSync target) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync", target, "updateLastOnline", new String[]{}, new Object[]{}); }
    private BackendGlobalDataSyncAccess() { }
    public static void close(BackendGlobalDataSync target) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync", target, "close", new String[]{}, new Object[]{}); }
    public static void close(BackendGlobalDataSync target, long timeout, TimeUnit unit) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync", target, "close", new String[]{"long", "TimeUnit"}, new Object[]{timeout, unit}); }

}
