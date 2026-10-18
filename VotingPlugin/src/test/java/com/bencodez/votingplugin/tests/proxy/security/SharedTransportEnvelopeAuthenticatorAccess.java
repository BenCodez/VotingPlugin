package com.bencodez.votingplugin.tests.proxy.security;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.security.*;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.*;
import java.time.Clock;

/** Typed test-only access to package-private SharedTransportEnvelopeAuthenticator seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedTransportEnvelopeAuthenticatorAccess {
    public static final long MAX_CLOCK_SKEW_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator", "MAX_CLOCK_SKEW_MILLIS");
    private SharedTransportEnvelopeAuthenticatorAccess() { }
    public static SharedTransportEnvelopeAuthenticator forTesting(byte[] key, Mode mode, Clock clock) { return (SharedTransportEnvelopeAuthenticator) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator", null, "forTesting", new String[]{"byte[]", "Mode", "Clock"}, new Object[]{key, mode, clock}); }
    public static SharedTransportEnvelopeAuthenticator forTesting(byte[] key, Mode mode, Clock clock, int maxReplayEntries) { return (SharedTransportEnvelopeAuthenticator) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator", null, "forTesting", new String[]{"byte[]", "Mode", "Clock", "int"}, new Object[]{key, mode, clock, maxReplayEntries}); }
    public static int pruneExpired(SharedTransportEnvelopeAuthenticator target, long now) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator", target, "pruneExpired", new String[]{"long"}, new Object[]{now}); }
}
