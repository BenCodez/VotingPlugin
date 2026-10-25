package com.bencodez.votingplugin.tests.backendproxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.*;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler.*;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Typed test-only access to package-private BackendProxyHandler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendProxyHandlerAccess {
    public static void dispatchIncomingAfterPublication(BackendProxyHandler target, JsonEnvelope envelope, Runnable localDispatch) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendProxyHandler", target, "dispatchIncomingAfterPublication", new String[]{"JsonEnvelope", "Runnable"}, new Object[]{envelope, localDispatch}); }
    public static final int MAX_ORDERED_VOTE_QUEUE = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.BackendProxyHandler", "MAX_ORDERED_VOTE_QUEUE");
    private BackendProxyHandlerAccess() { }

}
