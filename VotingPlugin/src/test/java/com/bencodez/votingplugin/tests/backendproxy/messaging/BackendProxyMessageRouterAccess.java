package com.bencodez.votingplugin.tests.backendproxy.messaging;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.messaging.*;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.*;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Typed test-only access to package-private BackendProxyMessageRouter seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendProxyMessageRouterAccess {
    private BackendProxyMessageRouterAccess() { }
    public static void handleVoteUpdate(BackendProxyMessageRouter target, JsonEnvelope msg) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter", target, "handleVoteUpdate", new String[]{"JsonEnvelope"}, new Object[]{msg}); }
    public static void handleVoteUpdate(BackendProxyMessageRouter target, JsonEnvelope msg, Runnable completion) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter", target, "handleVoteUpdate", new String[]{"JsonEnvelope", "Runnable"}, new Object[]{msg, completion}); }
}
